BUILDDIR=../_build/c
$(shell mkdir -p $(BUILDDIR))
OCAMLDIR=$(shell ocamlopt -where)

CC:=$(shell ocamlfind ocamlc -config | awk '/^bytecomp_c_compiler/ {for(i=2;i<=NF;i++) printf "%s " ,$$i}')
LD=$(CC)

NAME=dpipe

SRC=main.c

TESTS=tests/diff.t

OBJ=$(BUILDDIR)/main.o

all: $(BUILDDIR)/$(NAME)
	ln -f $(BUILDDIR)/$(NAME) $(NAME)

$(BUILDDIR)/$(NAME): $(OBJ)
	$(LD) -o $@ $^ -L$(BUILDDIR)/../lib/ -ldecompress

$(BUILDDIR)/%.o: %.c
	$(CC) -c -o $@ -I $(BUILDDIR)/../stub $<

run: $(NAME)
	LD_LIBRARY_PATH=$(BUILDDIR)/../lib/ ./$(NAME) $(ARGS)

test:
	BUILDDIR=$(shell pwd)/$(BUILDDIR)/../ DPIPE=$(shell pwd) cram $(TESTS)
