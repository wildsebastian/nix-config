PACK = editors \
	   gitTools \
       languageTools \
       systemPackages \
       chatTools

ENVS = env-js \
       env-db \
       env-coq88 \
       env-ghc84 \
       env-ocaml \
       env-ghc84-prof \
       env-elm \
       env-ghcjs

update-all:
	nix-channel --update
	for i in $(PACK); do                 \
		echo Updating $$i;               \
		nix-env -f '<nixpkgs>' -j 4 -u --leq -Q -k $$i; \
	done
	for i in $(ENVS); do                 \
		echo Updating $$i;               \
		nix-env -f '<nixpkgs>' -j 4 -u --leq -Q -k $$i; \
	done

gc:
	nix-collect-garbage --delete-older-than 7d
