#!/usr/bin/env make

.SUFFIXES:
.SUFFIXES: .o .hs .lhs .html .pdf

%	: %.lhs
	-ghc -package stm -package mtl --make $<

%.html	: %.lhs
	-pandoc -r markdown+lhs -s $< -w html --css haskell.css -o $@

%.pdf	: %.lhs
	-pandoc -r markdown+lhs -s $< --css haskell.css -o $@

LHS	:= $(wildcard doc/*.lhs)
SRC	:= $(wildcard src/*.hs app/*.hs test/*.hs bench/*.hs)
ROOT	:= $(shell stack path --local-doc-root)

.PHONY: default
default:check test

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
	@hlint --cross --color --show $(SRC)

.PHONY: build
build:
	@echo build ...
	@stack build --no-test

.PHONY: test
test:
	@echo test ...
	@stack test

.PHONY: doc
doc:
	@echo doc ...
	@stack haddock --no-run-tests --haddock-deps

.PHONY: bench
bench:
	@echo bench ...
	@stack bench scrapbook:bench:myreverseBench --benchmark-arguments '-o $(ROOT)/benchmark-myreverse.html'
	@stack bench scrapbook:bench:polydivisorsBench --benchmark-arguments '-o $(ROOT)/benchmark-polydivisors.html'
	@stack bench scrapbook:bench:repmaxBench --benchmark-arguments '-o $(ROOT)/benchmark-repmax.html'
	@stack bench scrapbook:bench:subseqsBench --benchmark-arguments '-o $(ROOT)/benchmark-subseqs.html'
	@stack bench scrapbook:bench:zipfoldBench --benchmark-arguments '-o $(ROOT)/benchmark-zipfold.html'

.PHONY: exec
exec:	$(SRC)
	@echo FPComplete ...
	@stack exec fpcomplete
	@echo PolyDivisors ...
	@stack exec polydivs 123456789
	@echo Quine ...
	@stack exec quine
	@echo
	@echo ReadFile Setup.hs ...
	@stack exec readfile Setup.hs
	@echo Skips ...
	@stack exec skips abcd
	@echo Threads ...
	@stack exec threads
	@echo While ...
	@echo "a\nb\nc\nq\n" | stack exec while
	@echo WordCount ...
	@cat Setup.hs | stack exec wordcount
	@echo

.PHONY: setup
setup:
	@stack update
	@stack setup
	@stack build
	@stack query
	@stack ls dependencies
	#stack exec ghc-pkg -- list

.PHONY: clean
clean:
	@stack clean
	-$(RM) $(addsuffix .hi, $(basename $(LHS) $(SRC)))
	-$(RM) $(addsuffix .o, $(basename $(LHS) $(SRC)))
	-$(RM) $(addsuffix .prof, $(basename $(LHS) $(SRC)))

.PHONY: cleanall
cleanall: clean
	@stack purge
	-$(RM) -rf public .pytest_cache dist
	-$(RM) *.pyc *.sublime-workspace tags
	-$(RM) $(patsubst %.lhs, %, $(LHS))
	-$(RM) $(patsubst %.lhs, %.html, $(LHS))
	-$(RM) $(patsubst %.lhs, %.pdf, $(LHS))
