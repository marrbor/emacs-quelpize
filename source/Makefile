include ../Makefile.leaf

INSTALL:=sudo make install EMACS=$(EMACS)

all: skk w3m

skk: $(SHAREDIR)/skk
w3m: apel $(LISPDIR)/w3m
apel: $(LISPDIR)/apel

arc2dir=$(subst .tar.gz,,$(1))
dir2arg=$(addsuffix .tar.gz,$(1))

# skk
SKKARC:=$(shell $(FIND) . -name 'ddskk*.tar.gz')
SKKDIR:=$(call arc2dir,$(SKKARC))
$(SHAREDIR)/skk: $(SKKARC)
	tar -zxf $(SKKARC)
	cd $(SKKDIR) && $(INSTALL)

# apel
APELARC:=$(shell $(FIND) . -name 'apel*.tar.gz')
APELDIR:=$(call arc2dir,$(APELARC))
$(LISPDIR)/apel: $(APELARC)
	tar -zxf $(APELARC)
	cd $(APELDIR) && $(INSTALL)


# w3m
W3MARC:=$(shell $(FIND) . -name 'emacs-w3m*.tar.gz')
W3MDIR:=$(call arc2dir,$(W3MARC))
$(LISPDIR)/w3m: $(W3MARC)
	tar -zxf $(W3MARC)
	cd $(W3MDIR) && ./configure --with-emacs=$(EMACS) && $(INSTALL)

test:
	@echo "PREFIX:$(PREFIX)"
	@echo "SKKDIR:$(SKKDIR)"
	@echo "APELDIR:$(APELDIR)"
	@echo "W3MDIR:$(W3MDIR)"
