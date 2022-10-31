#!/usr/bin/env make

SRC	:= $(wildcard *.hs **/*.hs)
YAML	:= $(shell git ls-files | grep --perl \.y?ml)

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
	@stylish-haskell --verbose --config=.stylish-haskell.yaml --inplace $(SRC)

.PHONY: lint
lint:	$(SRC)
	@echo lint ...
	@cabal check --verbose=2
	@hlint --cross --color --show $(SRC)
	@yamllint --strict $(YAML)

.PHONY: build
build:	check
	@echo build ...
	@cabal v2-build

.PHONY: test
test:
	@echo test ...
	@cabal v2-test --test-show-details=direct
# Show just test failures
#	@cabal new-test --test-show-details=failures

.PHONY: bench
bench:
	@echo bench ...
	@cabal v2-bench --jobs=1

.PHONY: doc
doc:
	@echo doc ...
ifeq ($(shell cabal --numeric-version), 3.4.1.0)
	@cabal v2-haddock --haddock-hyperlink-source
else
	@cabal v2-haddock --haddock-hyperlink-source --haddock-quickjump
endif

.PHONY: exec
exec:	build
	@echo Counter ...
	@cabal v2-exec counter 4
	@echo FPComplete ...
	@cabal v2-exec fpcomplete
	@echo NumberLines ...
	@cabal v2-exec numberlines -- Setup.hs
	@echo PolyDivisors ...
	@cabal v2-exec polydivs 123456789
	@echo Quine ...
	@cabal v2-exec quine
	@echo
	@echo ReadFile Setup.hs ...
	@cabal v2-exec readfile Setup.hs
	@echo Skips ...
	@cabal v2-exec skips abcd
	@echo Threads ...
	@cabal v2-exec threads
	@echo Vocab ...
	@cabal v2-exec vocab LICENSE
	@echo While ...
	@echo "a\nb\nc\nq\n" | cabal v2-exec while
	@echo WordCount ...
	@cabal v2-exec wordcount
	@cabal v2-exec wordcount -- LICENSE
	@echo

.PHONY: setup
setup:
	cabal --version
	cabal v2-update --only-dependencies --disable-tests --disable-documentation --disable-benchmarks
	cabal info scrapbook

.PHONY: clean
clean:
	@cabal v2-clean
	-$(RM) $(addsuffix .hi, $(basename $(SRC)))
	-$(RM) $(addsuffix .o, $(basename $(SRC)))
	-$(RM) $(addsuffix .prof, $(basename $(SRC)))

.PHONY: cleanall
cleanall: clean
	-$(RM) -rf public .pytest_cache .stack-work/
	-$(RM) *.pyc *.sublime-workspace tags
