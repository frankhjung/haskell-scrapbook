#!/usr/bin/env make

.SUFFIXES:
.SUFFIXES: .o .hs .lhs .html

.DEFAULT: all

%	: %.lhs
	-ghc --make $<

%.html	: %.lhs
	-pandoc -r markdown+lhs -s $< -w html --css haskell.css -o $@

%	: %.hs
	-ghc -O2 -Wall -fplugin=Splint -Wno-type-defaults -rtsopts -threaded --make $<

LHSS	:= $(wildcard *.lhs)
SRCS	:= $(wildcard *.hs)
TGTS 	:= $(patsubst %.hs, %, $(SRCS))

.PHONY: all
all:	clean check build doc

.PHONY: check
check:	tags style lint

tags:	$(SRCS)
	-hasktags --ctags $(SRCS)

style:	$(SRCS)
	-stylish-haskell --config=.stylish-haskell.yaml --inplace $(SRCS)

lint:	$(SRCS)
	-hlint --cross --color --show $(SRCS)

.PHONY: build
build:	check $(TGTS)

.PHONY: doc
doc:
	-haddock --title="Haskell Scrapbook" --html --hyperlinked-source --odir public $(SRCS)

.PHONY: clean
clean:
	-$(RM) $(addsuffix .hi, $(basename $(LHSS) $(SRCS)))
	-$(RM) $(addsuffix .o, $(basename $(LHSS) $(SRCS)))
	-$(RM) $(addsuffix .prof, $(basename $(LHSS) $(SRCS)))

.PHONY: cleanall
cleanall: clean
	-$(RM) -rf public
	-$(RM) tags *.pyc
	-$(RM) $(TGTS)
	-$(RM) $(patsubst %.lhs, %, $(LHSS))
	-$(RM) $(patsubst %.lhs, %.html, $(LHSS))
