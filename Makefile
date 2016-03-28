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
# setup.ml independent of oasis:
	cd $(PKG_NAME)-$(PKG_VERSION) && oasis setup
	tar -zcvf $(PKG_TARBALL) $(PKG_NAME)-$(PKG_VERSION)
	$(RM) -rf $(PKG_NAME)-$(PKG_VERSION)

CMXA=decompress.cmxa

$(CMXA): all

BUILDDIR=_build
VPATH=$(BUILDDIR)
OCAMLDIR=$(shell ocamlopt -where)
$(shell mkdir -p $(BUILDDIR) $(BUILDDIR)/rstub $(BUILDDIR)/gen $(BUILDDIR)/out)
PACKAGES=ctypes.stubs,ctypes.foreign,logs.fmt

# The file used to build the stub generator.
GENERATOR_FILES=$(BUILDDIR)/rstub/bindings.cmx \
				$(BUILDDIR)/gen/generate.cmx

RSTUBS=$(BUILDDIR)/rstub/bindings.cmx \
	   $(BUILDDIR)/out/decompress_bindings.cmx \
	   $(BUILDDIR)/rstub/abindings.cmx \
	   $(BUILDDIR)/out/decompress.o

GENERATED=$(BUILDDIR)/out/decompress.h \
		  $(BUILDDIR)/out/decompress.c \
		  $(BUILDDIR)/out/decompress_bindings.ml

OSTYPE:=$(shell ocamlfind ocamlc -config | awk '/^os_type:/ {print $$2}')
EXTDLL:=$(shell ocamlfind ocamlc -config | awk '/^ext_dll:/ {print $$2}')
CC:=$(shell ocamlfind ocamlc -config | awk '/^bytecomp_c_compiler/ {for(i=2;i<=NF;i++) printf "%s " ,$$i}')

ifeq ($(OSTYPE), $(filter $(OSTYPE), Win32 Cygwin))
  EXTEXE=.exe
else
  EXTEXE=
endif

GENERATOR=$(BUILDDIR)/generate$(EXTEXE)

sharedlib: $(BUILDDIR)/libdecompress$(EXTDLL)

ifeq ($(OSTYPE),$(filter $(OSTYPE),Win32 Cygwin))
$(BUILDDIR)/libdecompress$(EXTDLL): $(RSTUBS)
	ocamlfind opt -g -o $@ -linkpkg -output-obj -verbose -package $(PACKAGES) $(BUILDDIR)/lib/$(CMXA) $^
else
$(BUILDDIR)/libdecompress$(EXTDLL): $(RSTUBS)
	ocamlfind opt -g -o $@ -linkpkg -output-obj -runtime-variant _pic -verbose -package $(PACKAGES) $(BUILDDIR)/lib/$(CMXA) $^
endif

stubs: $(GENERATED)

$(GENERATED): $(GENERATOR)
	$(GENERATOR) $(BUILDDIR)/out

$(BUILDDIR)/%.o: %.c
	$(CC) -g -c -o $@ -fPIC -I $(shell ocamlfind query ctypes) -I $(OCAMLDIR) -I $(OCAMLDIR)/../ctypes $<

$(BUILDDIR)/%.cmx: %.ml
	ocamlfind opt -g -c -o $@ -I $(BUILDDIR)/out -I $(BUILDDIR)/rstub -I $(BUILDDIR)/lib -package $(PACKAGES) $<

$(GENERATOR): $(GENERATOR_FILES)
	ocamlfind opt -g -o $@ -linkpkg -package $(PACKAGES) $(BUILDDIR)/lib/$(CMXA) $^

c: sharedlib
	$(MAKE) -C $@
