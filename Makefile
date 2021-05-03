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

.PHONY: build
build:
	@echo build ...
	@cabal build

.PHONY: test
test:
	@echo test ...
	@cabal test --test-show-details=always

.PHONY: bench
bench:
	@echo bench ...
	@cabal bench

.PHONY: doc
doc:
	@echo doc ...
	@cabal haddock --haddock-quickjump --haddock-hyperlink-source

.PHONY: exec
exec:	$(SRC)
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
	@echo While ...
	@echo "a\nb\nc\nq\n" | cabal exec while
	@echo WordCount ...
	@cat LICENSE | cabal exec wordcount
	@echo

.PHONY: setup
setup:
	cabal --version
	cabal update --only-dependencies --enable-tests --enable-documentation --enable-benchmarks

.PHONY: clean
clean:
	@cabal clean
	-$(RM) $(addsuffix .hi, $(basename $(LHS) $(SRC)))
	-$(RM) $(addsuffix .o, $(basename $(LHS) $(SRC)))
	-$(RM) $(addsuffix .prof, $(basename $(LHS) $(SRC)))

.PHONY: cleanall
cleanall: clean
	-$(RM) -rf public .pytest_cache
	-$(RM) *.pyc *.sublime-workspace tags
	-$(RM) $(patsubst %.lhs, %, $(LHS))
	-$(RM) $(patsubst %.lhs, %.html, $(LHS))
	-$(RM) $(patsubst %.lhs, %.pdf, $(LHS))
