#!/usr/bin/env make

SRC	:= $(wildcard *.hs **/*.hs)
YAML	:= $(shell git ls-files | grep --perl \.y?ml)

.PHONY: default
default:	check build test

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
	@stylish-haskell --verbose --config=.stylish-haskell.yaml --inplace $(SRC)

.PHONY: lint
lint:	$(SRC)
	@echo lint ...
	@cabal check
	@hlint --cross --color --show $(SRC)
	@yamllint --strict $(YAML)

.PHONY: build
build:
	@echo build ...
	@stack build --pedantic --fast

.PHONY: test
test:
	@echo test ...
	@stack test --fast

.PHONY: bench
bench:
	@echo bench ...
	@stack bench scrapbook
	@#stack bench scrapbook:bench:monTransBench --ba '-o .stack-work/benchmark-monTransBench.html'
	@#stack bench scrapbook:bench:myfilterBench --ba '-o .stack-work/benchmark-myfilter.html'
	@#stack bench scrapbook:bench:myreverseBench --ba '-o .stack-work/benchmark-myreverse.html'
	@#stack bench scrapbook:bench:polydivisorsBench --ba '-o .stack-work/benchmark-polydivisors.html'
	@#stack bench scrapbook:bench:recursionschemesBench --ba '-o .stack-work/benchmark-recursionschemes.html'
	@#stack bench scrapbook:bench:repmaxBench --ba '-o .stack-work/benchmark-repmax.html'
	@#stack bench scrapbook:bench:subseqsBench --ba '-o .stack-work/benchmark-subseqs.html'
	@#stack bench scrapbook:bench:zipfoldBench --ba '-o .stack-work/benchmark-zipfold.html'

.PHONY: doc
doc:
	@echo doc ...
	@stack haddock scrapbook
	@#stack haddock scrapbook --haddock-arguments '--haddock-tests --haddock-benchmarks --haddock-executables'

.PHONY: exec
exec:
	@echo Counter ...
	@stack exec -- counter 4
	@echo FPComplete ...
	@stack exec -- fpcomplete
	@echo JSON ...
	@stack exec -- json
	@echo NumberLines ...
	@stack exec -- numberlines LICENSE
	@echo PolyDivisors ...
	@stack exec -- polydivs 123456789
	@echo Quine ...
	@stack exec -- quine
	@echo
	@echo ReadFile Setup.hs ...
	@stack exec -- readfile Setup.hs
	@echo Skips ...
	@stack exec -- skips abcd
	@echo StateGame ...
	@stack exec -- stategame abcaaacbbcabbab
	@echo Threads ...
	@stack exec -- threads
	@echo Vocab ...
	@stack exec -- vocab LICENSE
	@echo While ...
	@echo "a\nb\nc\nq\n" | stack exec -- while
	@echo WordCount ...
	@stack exec -- wordcount
	@stack exec -- wordcount LICENSE
	@echo WordCountArrow ...
	@stack exec -- wordcountarrow
	@stack exec -- wordcountarrow LICENSE
	@echo

.PHONY: setup
setup:
	stack path
	stack query
	stack ls dependencies

.PHONY: clean
clean:
	@stack clean
	@cabal clean
	@$(RM) tags
	@$(RM) $(wildcard *.hi **/*.hi)
	@$(RM) $(wildcard *.o **/*.o)
	@$(RM) $(wildcard *.prof **/*.prof)
	@$(RM) $(wildcard *.tix **/*.tix)

.PHONY: cleanall
cleanall: clean
	@stack purge
