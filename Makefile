SUBDIRS := regress
HSSRC := `find app/ src/ test/ -name "*.hs"`

all:
	stack build

setup:
	stack --no-terminal --install-ghc test --only-dependencies

install:
	stack install

doc:
	stack haddock --open

hlint:
	hlint $(HSSRC)

test: all
	make -C regress clean
	$(foreach i,$(SUBDIRS),$(MAKE) -C $i $@ &&) true
#	stack test

clean:
	stack clean
	rm -f idiomaticca.cabal a.out main_dats.c
	$(foreach i,$(SUBDIRS),$(MAKE) -C $i $@ &&) true

.PHONY: all setup install doc hlint test clean
