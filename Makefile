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
       env-elm

install:
	nix-channel --update
	for i in $(PACK); do                 \
		echo Updating $$i;               \
		nix-env -f '<nixpkgs>' -j 4 -i $$i; \
	done
	for i in $(ENVS); do                 \
		echo Updating $$i;               \
		nix-env -f '<nixpkgs>' -j 4 -i $$i; \
	done


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


update-pack:
	nix-channel --update
	for i in $(PACK); do                 \
		echo Updating $$i;               \
		nix-env -f '<nixpkgs>' -j 4 -u --leq -Q -k $$i; \
	done


update-env:
	nix-channel --update
	for i in $(ENVS); do                 \
		echo Updating $$i;               \
		nix-env -f '<nixpkgs>' -j 4 -u --leq -Q -k $$i; \
	done


gc-all:
	nix-env --delete-generations old
	nix-collect-garbage

gc:
	nix-collect-garbage --delete-older-than 7d

