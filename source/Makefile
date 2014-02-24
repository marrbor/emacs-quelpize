include ../Makefile.leaf

INSTALL:=$(SUDO) make install EMACS=$(EMACS)
INSTALL_W3M:=$(SUDO) make install-lisp EMACS=$(EMACS) lispdir=$(SITELISP)/w3m

all: skk w3m

skk: $(SITELISP)/skk
w3m: apel $(SITELISP)/w3m
apel: $(SITELISP)/apel

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
$(SITELISP)/apel: $(APELARC)
	tar -zxf $(APELARC)
	cd $(APELDIR) && $(INSTALL)


# w3m
W3MARC:=$(shell $(FIND) . -name 'emacs-w3m*.tar.gz')
W3MDIR:=$(call arc2dir,$(W3MARC))
$(SITELISP)/w3m: $(W3MARC)
	tar -zxf $(W3MARC)
	cd $(W3MDIR) && ./configure --with-emacs=$(EMACS) && $(INSTALL_W3M)

test:
	@echo "PREFIX:$(PREFIX)"
	@echo "SKKDIR:$(SKKDIR)"
	@echo "APELDIR:$(APELDIR)"
	@echo "W3MDIR:$(W3MDIR)"
