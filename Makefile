#!/usr/bin/env make

SRC	:= $(wildcard *.hs **/*.hs)
TARGET	:= Scrapbook
YAML	:= $(shell git ls-files "*.y*ml")

.PHONY: help
help: ## Show this help message
	@echo Available targets:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "  %-15s %s\n", $$1, $$2}'

.PHONY: default
default:	format check test ## Run the default local checks

.PHONY: all
all:	format check build test doc bench exec ## Run the full local workflow

.PHONY: format
format:	$(SRC) ## Format Haskell sources and cabal file
	@echo format ...
	@cabal-fmt --inplace $(TARGET).cabal
	@stylish-haskell --inplace $(SRC)

.PHONY: check
check:	tags lint ## Run static checks

.PHONY: tags
tags:	$(SRC) ## Generate ctags for the source tree
	@echo tags ...
	@hasktags --ctags --extendedctag $(SRC)

.PHONY: lint
lint:	$(SRC) ## Run linters and metadata checks
	@echo lint ...
	@hlint --cross --color --show $(SRC)
	@cabal check
	@yamllint --strict $(YAML)

.PHONY: build
build: ## Build the project
	@stack build

.PHONY: test
test: ## Run the test suite
	@stack test --fast

.PHONY: bench
bench: ## Run benchmarks and write HTML reports
	@echo bench ...
	@stack bench Scrapbook:bench:myfilterBench --ba '-o .stack-work/benchmark-myfilter.html'
	@stack bench Scrapbook:bench:myreverseBench --ba '-o .stack-work/benchmark-myreverse.html'
	@stack bench Scrapbook:bench:mysumBench --ba '-o .stack-work/benchmark-mysum.html'
	@stack bench Scrapbook:bench:polydivisorsBench --ba '-o .stack-work/benchmark-polydivisors.html'
	@stack bench Scrapbook:bench:recursionschemesBench --ba '-o .stack-work/benchmark-recursionschemes.html'
	@stack bench Scrapbook:bench:repmaxBench --ba '-o .stack-work/benchmark-repmax.html'
	@stack bench Scrapbook:bench:subseqsBench --ba '-o .stack-work/benchmark-subseqs.html'
	@stack bench Scrapbook:bench:termFoldBench --ba '-o .stack-work/benchmark-termFoldBench.html'
	@stack bench Scrapbook:bench:zipfoldBench --ba '-o .stack-work/benchmark-zipfold.html'

.PHONY: doc
doc: ## Build Haddock documentation
	@stack haddock

.PHONY: exec
exec: ## Run the sample executables
	stack exec -- counter 4
	stack exec -- fpcomplete
	stack exec -- json
	stack exec -- numberlines LICENSE
	stack exec -- polydivs 123456789
	stack exec -- quine
	@echo
	stack exec -- readfile LICENSE
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
setup: ## Initialize cabal config and fetch dependencies
ifeq (,$(wildcard ${CABAL_CONFIG}))
	-cabal user-config init
else
	@echo Using user-config from ${CABAL_CONFIG} ...
endif
	-cabal update --only-dependencies

.PHONY: clean
clean: ## Remove build artifacts
	@stack clean
	@cabal clean

.PHONY: distclean
distclean: clean ## Remove build artifacts and generated tags
	@$(RM) tags
	@stack purge
