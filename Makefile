#!/usr/bin/env make

.SUFFIXES:
.SUFFIXES: .o .hs .lhs .html .pdf

%	: %.lhs
	-ghc -package stm -package mtl --make $<

%.html	: %.lhs
	-pandoc -r markdown+lhs -s $< -w html --css haskell.css -o $@

%.pdf	: %.lhs
	-pandoc -r markdown+lhs -s $< --css haskell.css -o $@

%	: %.hs
	-ghc -O2 -Wall -fplugin=Splint -Wno-type-defaults\
		-rtsopts -threaded -package mtl -package stm --make $<

LHSS	:= $(wildcard *.lhs)
SRCS	:= $(wildcard *.hs)
TGTS 	:= $(patsubst %.hs, %, $(SRCS))

.DEFAULT: check
check:	tags style lint

.PHONY: tags
tags:	$(SRCS)
	@echo tags ...
	@hasktags --ctags $(SRCS)

.PHONY: style
style:	$(SRCS)
	@echo style ...
	@stylish-haskell --config=.stylish-haskell.yaml --inplace $(SRCS)

.PHONY: lint
lint:	$(SRCS)
	@echo lint ...
	@hlint --cross --color --show $(SRCS)

.PHONY: hdevtools
hdevtools:$(SRCS)
	@echo hdevtools check ...
	$(foreach file, $(SRCS), hdevtools check $(file);)

.PHONY: all
all:	check build doc

.PHONY: build
build:	check $(TGTS)

.PHONY: doc
doc:
	-haddock --title="Haskell Scrapbook" --html\
		--hyperlinked-source --odir public $(SRCS)

.PHONY: clean
clean:
	-$(RM) .hdevtools.sock
	-$(RM) $(addsuffix .hi, $(basename $(LHSS) $(SRCS)))
	-$(RM) $(addsuffix .o, $(basename $(LHSS) $(SRCS)))
	-$(RM) $(addsuffix .prof, $(basename $(LHSS) $(SRCS)))

.PHONY: cleanall
cleanall: clean
	-$(RM) -rf public .pytest_cache dist
	-$(RM) *.pyc *.sublime-workspace tags
	-$(RM) $(TGTS)
	-$(RM) $(patsubst %.lhs, %, $(LHSS))
	-$(RM) $(patsubst %.lhs, %.html, $(LHSS))

