#!/bin/sh

path="$(nix-instantiate --find-file nixpkgs)"

if [ -f "${path}/.version-suffix" ]; then
  version="$(< $path/.version-suffix)"
elif [ -f "${path}/.git" ]; then
  version="$(< $(< ${path}/.git/HEAD))"
fi

cache=".direnv/cache-${version:-unknown}"

if [[ $(find "$filename" -mtime +6 -print) ]]; then
  echo "Cache is older than 6 days -- rebuild"
  rm "$cache"
fi

update_drv=0
if [[ ! -e "$cache" ]] || \
  [[ "$HOME/.direnvrc" -nt "$cache" ]] || \
  [[ .envrc -nt "$cache" ]] || \
  [[ default.nix -nt "$cache" ]] || \
  [[ shell.nix -nt "$cache" ]];
then
  [ -d .direnv ] || mkdir .direnv
  nix-shell --show-trace --pure "$@" --run "\"$direnv\" dump bash" > "$cache"
  update_drv=1
else
  echo "using cached derivation"
fi
term_backup=$TERM path_backup=$PATH
if [ -n ${TMPDIR+x} ]; then
  tmp_backup=$TMPDIR
fi

eval "$(< $cache)"
export PATH=$PATH:$path_backup TERM=$term_backup TMPDIR=$tmp_backup
if [ -n ${tmp_backup+x} ]; then
  export TMPDIR=${tmp_backup}
else
  unset TMPDIR
fi

# `nix-shell --pure` sets invalid ssl certificate paths
if [ "${SSL_CERT_FILE:-}" = /no-cert-file.crt ]; then
  unset SSL_CERT_FILE
fi
if [ "${NIX_SSL_CERT_FILE:-}" = /no-cert-file.crt ]; then
  unset NIX_SSL_CERT_FILE
fi

# This part is based on https://discourse.nixos.org/t/what-is-the-best-dev-workflow-around-nix-shell/418/4
if [ "$out" ] && (( $update_drv )); then
  drv_link=".direnv/drv"
  drv="$(nix show-derivation $out | grep -E -o -m1 '/nix/store/.*.drv')"
  stripped_pwd=${PWD/\//}
  escaped_pwd=${stripped_pwd//-/--}
  escaped_pwd=${escaped_pwd//\//-}
  ln -fs "$drv" "$drv_link"
  ln -fs "$PWD/$drv_link" "/nix/var/nix/gcroots/per-user/$LOGNAME/$escaped_pwd"
  echo "renewed cache and derivation link"
fi
