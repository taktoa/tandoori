.RECIPEPREFIX := >
.PHONY: all build clean configure haddock dependencies repl run tags

shell = '$$SHELL'

ECHO_CMD = echo

ECHO = ${ECHO_CMD}
ECHN = ${ECHO_CMD} -n
DONE = ${ECHO_CMD} "[DONE]"

all: install configure build haddock tags

repl:
> @${ECHO} "Starting REPL"
> cabal repl "lib:Tandoori"
> @${ECHO} "Done with REPL"

run:
> @${ECHO} "Running project"
> cabal run --jobs .
> @${ECHO} "Done running project"

nix-shell: nix-init
> @${ECHO} "Entering Nix shell"
> nix-shell --command 'make dependencies && ${shell}'
> @make clean
> @${ECHO} "Safely exited Nix shell"

build:
> @${ECHO} "build:"
> @${ECHN} " Building project ... "
> cabal build --jobs
> @${DONE}

configure:
> @${ECHO} "build:"
> @${ECHN} " Building project ... "
> cabal configure
> @${DONE}

haddock:
> @${ECHO} "haddock:"
> @${ECHN} " Generating documentation ... "
> cabal haddock --hyperlink-source
> @${DONE}

dependencies: sandbox
> @${ECHO} "dependencies:"
> @${ECHN} " Installing project dependencies ... "
> cabal install --jobs --only-dependencies --reorder-goals
> @${DONE}

sandbox:
> @${ECHO} "sandbox:"
> @${ECHN} " Creating a sandbox ... "
> cabal sandbox init
> @${DONE}

nix-init: clean
> @${ECHO} "nix-init:"
> @${ECHN} " Generating shell.nix ... "
> @cabal2nix --shell . > shell.nix;
> @${DONE}
> @${ECHN} " Generating default.nix ... "
> @cabal2nix . > default.nix;
> @${DONE}

tags:
> @${ECHO} "tags:"
> @${ECHN} " Generating etags ... "
> @hasktags -e .
> @${DONE}
> @${ECHN} " Generating ctags ... "
> @hasktags -c .
> @${DONE}

distclean: clean sandbox-clean

clean: cabal-clean nix-clean prof-clean

cabal-clean:
> @${ECHO} "cabal-clean:"
> @${ECHN} " Running cabal clean ... "
> @cabal clean
> @${DONE}

nix-clean:
> @${ECHO} "nix-clean:"
> @${ECHN} " Removing default.nix ... "
> @if test -e default.nix; then rm default.nix; fi
> @${DONE}
> @${ECHN} " Removing shell.nix ... "
> @if test -e shell.nix; then rm shell.nix; fi
> @${DONE}

prof-clean:
> @${ECHO} "prof-clean:"
> @${ECHN} " Removing profiling tix files ... "
> @rm -f *.tix
> @${DONE}
> @${ECHN} " Removing .hpc directory ... "
> @if test -d .hpc; then rm -r .hpc; fi
> @${DONE}

sandbox-clean:
> @${ECHO} "prof-clean:"
> @${ECHN} " Removing .cabal-sandbox directory ... "
> @if test -d .cabal-sandbox; then cabal sandbox delete; fi
> @${DONE}
