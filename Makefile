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
TGT 	:= scrapbook
ROOT	:= $(shell stack path --local-doc-root)

.PHONY: default
default:check build test

.PHONY: check
check:	tags style lint

.PHONY: all
all:	check build test bench doc exec

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
	@stack build --pedantic --no-test

.PHONY: test
test:
	@stack test

.PHONY: bench
bench:
	@stack bench --benchmark-arguments '-o $(ROOT)/benchmark.html'

.PHONY: doc
doc:
	@stack haddock --no-rerun-tests --no-reconfigure

.PHONY: exec
exec:	$(SRC)
	@echo Random ...
	@stack exec random
	@echo Skips ...
	@stack exec skips abcd
	@echo Threads ...
	@stack exec threads
	@echo While ...
	@echo "a\nb\nc\nq\n" | stack exec while
	@echo ReadFile ...
	@stack exec readfile
	@echo WordCount ...
	@cat Setup.hs | stack exec wordcount

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
	-$(RM) -rf public .pytest_cache dist
	-$(RM) *.pyc *.sublime-workspace tags
	-$(RM) $(TGT)
	-$(RM) $(patsubst %.lhs, %, $(LHS))
	-$(RM) $(patsubst %.lhs, %.html, $(LHS))
	-$(RM) $(patsubst %.lhs, %.pdf, $(LHS))
