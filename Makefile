PKG_NAME = $(shell oasis query name)
PKG_VERSION = $(shell oasis query version)
PKG_TARBALL = $(PKG_NAME)-$(PKG_VERSION).tar.gz

DISTFILES = $(shell git ls-files)

CONFIGUREFLAGS =

# OASIS_START
# DO NOT EDIT (digest: a3c674b4239234cbbe53afe090018954)

SETUP = ocaml setup.ml

build: setup.data
	$(SETUP) -build $(BUILDFLAGS)

doc: setup.data build
	$(SETUP) -doc $(DOCFLAGS)

test: setup.data build
	$(SETUP) -test $(TESTFLAGS)

all:
	$(SETUP) -all $(ALLFLAGS)

install: setup.data
	$(SETUP) -install $(INSTALLFLAGS)

uninstall: setup.data
	$(SETUP) -uninstall $(UNINSTALLFLAGS)

reinstall: setup.data
	$(SETUP) -reinstall $(REINSTALLFLAGS)

clean:
	$(SETUP) -clean $(CLEANFLAGS)

distclean:
	$(SETUP) -distclean $(DISTCLEANFLAGS)

setup.data:
	$(SETUP) -configure $(CONFIGUREFLAGS)

configure:
	$(SETUP) -configure $(CONFIGUREFLAGS)

.PHONY: build doc test all install uninstall reinstall clean distclean configure

# OASIS_STOP

setup.ml: _oasis
	oasis setup -setup-update dynamic

# Make a tarball
.PHONY: dist tar
dist tar: $(DISTFILES)
	mkdir $(PKG_NAME)-$(PKG_VERSION)
	cp --parents -r $(DISTFILES) $(PKG_NAME)-$(PKG_VERSION)/
#	setup.ml independent of oasis:
	cd $(PKG_NAME)-$(PKG_VERSION) && oasis setup
	tar -zcvf $(PKG_TARBALL) $(PKG_NAME)-$(PKG_VERSION)
	$(RM) -rf $(PKG_NAME)-$(PKG_VERSION)
