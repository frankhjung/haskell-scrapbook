#!/usr/bin/env make

SRC	:= $(wildcard *.hs **/*.hs)
TARGET	:= Scrapbook
YAML	:= $(shell git ls-files "*.y*ml")

.PHONY: default
default:	format check test

.PHONY: all
	format check build test doc bench exec

.PHONY: format
format:	$(SRC)
	@echo format ...
	@cabal-fmt --inplace $(TARGET).cabal
	@stylish-haskell --inplace $(SRC)

.PHONY: check
check:	tags lint

.PHONY: tags
tags:	$(SRC)
	@echo tags ...
	@hasktags --ctags --extendedctag $(SRC)

.PHONY: lint
lint:	$(SRC)
	@echo lint ...
	@cabal check
	@hlint --cross --color --show $(SRC)
	@yamllint --strict $(YAML)

.PHONY: build
build:
	@stack build --pedantic --fast --ghc-options='-Wwarn'

.PHONY: test
test:
	@stack test --fast

.PHONY: bench
bench:
	@echo bench ...
	@stack bench Scrapbook:bench:monTransBench --ba '-o .stack-work/benchmark-monTransBench.html'
	@stack bench Scrapbook:bench:myfilterBench --ba '-o .stack-work/benchmark-myfilter.html'
	@stack bench Scrapbook:bench:myreverseBench --ba '-o .stack-work/benchmark-myreverse.html'
	@stack bench Scrapbook:bench:polydivisorsBench --ba '-o .stack-work/benchmark-polydivisors.html'
	@stack bench Scrapbook:bench:recursionschemesBench --ba '-o .stack-work/benchmark-recursionschemes.html'
	@stack bench Scrapbook:bench:repmaxBench --ba '-o .stack-work/benchmark-repmax.html'
	@stack bench Scrapbook:bench:subseqsBench --ba '-o .stack-work/benchmark-subseqs.html'
	@stack bench Scrapbook:bench:zipfoldBench --ba '-o .stack-work/benchmark-zipfold.html'

.PHONY: doc
doc:
	@stack haddock

.PHONY: exec
exec:
	stack exec -- counter 4
	stack exec -- fpcomplete
	stack exec -- json
	stack exec -- numberlines LICENSE
	stack exec -- polydivs 123456789
	stack exec -- quine
	@echo
	stack exec -- readfile LICENSE
	stack exec -- skips abcd
	stack exec -- stategame abcaaacbbcabbab
	stack exec -- threads
	stack exec -- vocab LICENSE
	echo "a\nb\nc\nq\n" | stack exec -- while
	stack exec -- wordcount
	stack exec -- wordcount LICENSE
	stack exec -- wordcountarrow
	stack exec -- wordcountarrow LICENSE
	@echo

.PHONY: setup
setup:
	cabal check --verbose
	stack path
	stack query
	stack ls dependencies

.PHONY: clean
clean:
	@stack clean
	@cabal clean

.PHONY: distclean
distclean: clean
	@$(RM) tags
	@stack purge
