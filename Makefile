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
	@hlint --cross --color --show $(SRC)

.PHONY: build
build:
	@echo build ...
	@cabal new-build

.PHONY: test
test:
	@echo test ...
	@cabal new-test --test-show-details=always

.PHONY: bench
bench:
	@echo bench ...
	@cabal new-bench

.PHONY: doc
doc:
	@echo doc ...
	@cabal new-haddock --haddock-quickjump --haddock-hyperlink-source

.PHONY: exec
exec:	$(SRC)
	@echo FPComplete ...
	@cabal new-exec fpcomplete
	@echo PolyDivisors ...
	@cabal new-exec polydivs 123456789
	@echo Quine ...
	@cabal new-exec quine
	@echo
	@echo ReadFile Setup.hs ...
	@cabal new-exec readfile Setup.hs
	@echo Skips ...
	@cabal new-exec skips abcd
	@echo Threads ...
	@cabal new-exec threads
	@echo While ...
	@echo "a\nb\nc\nq\n" | cabal new-exec while
	@echo WordCount ...
	@cat LICENSE | cabal new-exec wordcount
	@echo

.PHONY: setup
setup:
	cabal new-update --only-dependencies
	cabal configure --package-db=clear --package-db=global --package-db=$(stack path --snapshot-pkg-db) --package-db=$(stack path --local-pkg-db)

.PHONY: clean
clean:
	@cabal new-clean
	-$(RM) $(addsuffix .hi, $(basename $(LHS) $(SRC)))
	-$(RM) $(addsuffix .o, $(basename $(LHS) $(SRC)))
	-$(RM) $(addsuffix .prof, $(basename $(LHS) $(SRC)))

.PHONY: cleanall
cleanall: clean
	-$(RM) -rf public .pytest_cache dist
	-$(RM) *.pyc *.sublime-workspace tags
	-$(RM) $(patsubst %.lhs, %, $(LHS))
	-$(RM) $(patsubst %.lhs, %.html, $(LHS))
	-$(RM) $(patsubst %.lhs, %.pdf, $(LHS))
