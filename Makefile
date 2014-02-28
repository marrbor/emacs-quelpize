include Makefile.leaf

DIRS:=lisp source fonts

.PHONY: $(DIRS)
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

