include Makefile.leaf

DIRS:=lisp source fonts libs/ditaa

all: $(DIRS)

.PHONY : $(DIRS)

$(DIRS):
ifeq ($(strip $(EMACS)),)
	$(error "cannot find emacs.")
else
ifeq ($(strip $(AUTOCONF)),)
	$(error "cannot find autoconf.")
endif
endif
	-cd $@ && make

lisp: source

test:
	@echo "EMACS:$(EMACS)"
	@echo "EMACSVER:$(EMACSVER)"
	@echo "DIRS:$(DIRS)"
	@echo "SHAREDIR:$(SHAREDIR)"
	@echo "SITELISP:$(SITELISP)"
	@echo "VERSITELISP:$(VERSITELISP)"
