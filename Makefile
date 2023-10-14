#!/usr/bin/env make

SRC	:= $(wildcard *.hs **/*.hs)
YAML	:= $(shell git ls-files "*.y*ml")

.PHONY: default
default:	format check test

.PHONY: all
	format check build test doc bench exec

.PHONY: format
format:	$(SRC)
	@echo format ...
	@cabal-fmt --inplace scrapbook.cabal
	@stylish-haskell --verbose --config=.stylish-haskell.yaml --inplace $(SRC)

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
	@stack build --pedantic --fast

.PHONY: test
test:
	@stack test --fast

.PHONY: bench
bench:
	@echo bench ...
	@stack bench scrapbook:bench:monTransBench --ba '-o .stack-work/benchmark-monTransBench.html'
	@stack bench scrapbook:bench:myfilterBench --ba '-o .stack-work/benchmark-myfilter.html'
	@stack bench scrapbook:bench:myreverseBench --ba '-o .stack-work/benchmark-myreverse.html'
	@stack bench scrapbook:bench:polydivisorsBench --ba '-o .stack-work/benchmark-polydivisors.html'
	@stack bench scrapbook:bench:recursionschemesBench --ba '-o .stack-work/benchmark-recursionschemes.html'
	@stack bench scrapbook:bench:repmaxBench --ba '-o .stack-work/benchmark-repmax.html'
	@stack bench scrapbook:bench:subseqsBench --ba '-o .stack-work/benchmark-subseqs.html'
	@stack bench scrapbook:bench:zipfoldBench --ba '-o .stack-work/benchmark-zipfold.html'

.PHONY: doc
doc:
	@stack haddock scrapbook

.PHONY: exec
exec:
	stack exec -- counter 4
	stack exec -- fpcomplete
	stack exec -- json
	stack exec -- numberlines LICENSE
	stack exec -- polydivs 123456789
	stack exec -- quine
	stack exec -- readfile Setup.hs
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

.PHONY: cleanall
cleanall: clean
	@$(RM) tags
	@stack purge
