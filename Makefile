PACK = editors \
	   gitTools \
       languageTools \
       systemPackages \
       chatTools

ENVS = env-js \
       env-db \
       env-coq88 \
       env-ocaml \
       env-elm

HASKELL = env-ghc82 \
		  env-ghc82-prof \
		  env-ghc84 \
		  env-ghc84-prof \
		  env-ghc86 \
		  env-ghc86-prof \

install:
	for i in $(PACK); do                 	\
		echo Updating $$i;               	\
		nix-env -f '<nixpkgs>' -j 2 -i $$i; \
	done
	for i in $(ENVS); do                 	\
		echo Updating $$i;               	\
		nix-env -f '<nixpkgs>' -j 2 -i $$i; \
	done


update-all:
	for i in $(PACK); do                 				\
		echo Updating $$i;               				\
		nix-env -f '<nixpkgs>' -j 2 -u --leq -Q -k $$i; \
	done
	for i in $(ENVS); do                 				\
		echo Updating $$i;               				\
		nix-env -f '<nixpkgs>' -j 2 -u --leq -Q -k $$i; \
	done


update-pack:
	for i in $(PACK); do                 				\
		echo Updating $$i;               				\
		nix-env -f '<nixpkgs>' -j 2 -u --leq -Q -k $$i; \
	done


update-env:
	for i in $(ENVS); do                 				\
		echo Updating $$i;               				\
		nix-env -f '<nixpkgs>' -j 2 -u --leq -Q -k $$i; \
	done


update-haskell:
	for i in $(HASKELL); do								\
		echo Updating $$i; 								\
		nix-env -f '<nixpkgs>' -j 2 -u --leq -Q -k $$i; \
	done


gc-all:
	nix-env --delete-generations old
	nix-collect-garbage

gc:
	nix-collect-garbage --delete-older-than 7d

