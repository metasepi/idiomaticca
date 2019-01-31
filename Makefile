SUBDIRS := regress

all:
	stack build

install:
	stack install

test:
	$(foreach i,$(SUBDIRS),$(MAKE) -C $i $@ &&) true
	stack test

clean:
	stack clean
	rm -f idiomaticca.cabal
	$(foreach i,$(SUBDIRS),$(MAKE) -C $i $@ &&) true

.PHONY: all install test clean
