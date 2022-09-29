#!/usr/bin/env make

SRC	:= $(wildcard src/*.hs app/*.hs test/*.hs bench/*.hs)
YAMLS	:= $(wildcard *.yaml .*.yml .*/.*.yml)

.PHONY: default
default:check build test

.PHONY: check
check:	tags style lint

.PHONY: all
all:	check build test doc bench exec

.PHONY: tags
tags:	$(SRC)
	@echo tags ...
	@hasktags --ctags --extendedctag $(SRC)

.PHONY: style
style:	$(SRC)
	@echo style ...
	@stylish-haskell --config=.stylish-haskell.yaml --inplace $(SRC)

.PHONY: lint
lint:	$(SRC)
	@echo lint ...
	@cabal check
	@hlint --cross --color --show $(SRC)
	@yamllint --strict $(YAMLS)

.PHONY: build
build:	check
	@echo build ...
	@cabal build

.PHONY: test
test:
	@echo test ...
	@cabal test --test-show-details=always
# Show just test failures
#	@cabal new-test --test-show-details=failures

.PHONY: bench
bench:
	@echo bench ...
	@cabal bench --jobs=1

.PHONY: doc
doc:
	@echo doc ...
	@cabal haddock --haddock-quickjump --haddock-hyperlink-source

.PHONY: exec
exec:	build
	@echo Counter ...
	@cabal exec counter 4
	@echo FPComplete ...
	@cabal exec fpcomplete
	@echo PolyDivisors ...
	@cabal exec polydivs 123456789
	@echo Quine ...
	@cabal exec quine
	@echo
	@echo ReadFile Setup.hs ...
	@cabal exec readfile Setup.hs
	@echo Skips ...
	@cabal exec skips abcd
	@echo Threads ...
	@cabal exec threads
	@echo Vocab ...
	@cabal exec vocab LICENSE
	@echo While ...
	@echo "a\nb\nc\nq\n" | cabal exec while
	@echo WordCount ...
	@cabal exec wordcount
	@cabal exec wordcount -- LICENSE
	@echo

.PHONY: setup
setup:
	cabal --version
	cabal update --only-dependencies --enable-tests --enable-documentation --enable-benchmarks

.PHONY: clean
clean:
	@cabal clean
	-$(RM) $(addsuffix .hi, $(basename $(SRC)))
	-$(RM) $(addsuffix .o, $(basename $(SRC)))
	-$(RM) $(addsuffix .prof, $(basename $(SRC)))

.PHONY: cleanall
cleanall: clean
	-$(RM) -rf public .pytest_cache .stack-work/
	-$(RM) *.pyc *.sublime-workspace tags
