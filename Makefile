SUBDIRS := regress

all:
	stack build

install:
	stack install

doc:
	stack haddock --open

test: all
	make -C regress clean
	$(foreach i,$(SUBDIRS),$(MAKE) -C $i $@ &&) true
	stack test

clean:
	stack clean
	rm -f idiomaticca.cabal
	$(foreach i,$(SUBDIRS),$(MAKE) -C $i $@ &&) true

.PHONY: all install test clean
