#!/usr/bin/env make

.SUFFIXES:
.SUFFIXES: .o .hs

.DEFAULT: all

%:%.hs
	-ghc -Wall -Wno-type-defaults -O2 -threaded -rtsopts -prof --make $<

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
	-haddock --show=all --html --title="Haskell Scrapbook" --odir public $(SRCS)

.PHONY: clean
clean:
	-$(RM) $(patsubst %.hs, %.hi, $(SRCS))
	-$(RM) $(patsubst %.hs, %.o, $(SRCS))
	-$(RM) $(patsubst %.hs, %.prof, $(SRCS))

.PHONY: cleanall
cleanall: clean
	-$(RM) public
	-$(RM) $(TGTS) tags *.pyc
