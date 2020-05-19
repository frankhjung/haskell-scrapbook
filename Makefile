#!/usr/bin/env make

.SUFFIXES:
.SUFFIXES: .o .hs

.DEFAULT: all

%:%.hs
	-ghc -O2 -Wall -Wno-type-defaults -rtsopts -threaded --make $<

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
	-hlint --color --show $(SRCS)

.PHONY: build
build:	check $(TGTS)

.PHONY: doc
doc:
	-haddock --title="Haskell Scrapbook" --html --hyperlinked-source --odir public $(SRCS)

.PHONY: clean
clean:
	-$(RM) $(patsubst %.hs, %.hi, $(SRCS))
	-$(RM) $(patsubst %.hs, %.o, $(SRCS))
	-$(RM) $(patsubst %.hs, %.prof, $(SRCS))

.PHONY: cleanall
cleanall: clean
	-$(RM) -rf public
	-$(RM) $(TGTS) tags *.pyc
