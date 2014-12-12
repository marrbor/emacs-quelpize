include ../Makefile.leaf

INSTALL:=$(SUDO) make install EMACS=$(EMACS)

all: skk

skk: $(SHAREDIR)/skk $(VERSITELISP)/skk

arc2dir=$(subst .tar.gz,,$(1))
dir2arg=$(addsuffix .tar.gz,$(1))

# skk
SKKARC:=$(shell $(FIND) . -name 'ddskk*.tar.gz')
SKKDIR:=$(call arc2dir,$(SKKARC))
$(SHAREDIR)/skk: $(SKKARC)
	tar -zxf $(SKKARC)
	cd $(SKKDIR) && $(INSTALL)

$(VERSITELISP)/skk: $(SKKARC)
	tar -zxf $(SKKARC)
	cd $(SKKDIR) && $(INSTALL)

test:
	@echo "PREFIX:$(PREFIX)"
	@echo "SKKDIR:$(SKKDIR)"
	@echo "APELDIR:$(APELDIR)"
	@echo "SHAREDIR:$(SHAREDIR)"
