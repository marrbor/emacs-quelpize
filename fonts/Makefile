include ../Makefile.leaf

#
# Font install for Linux
#

SRCFONTS:=$(shell find . -name '*.ttf')
FONTSDIR:=/usr/share/fonts/japanese/TrueType
FONTS:=$(subst ./,$(FONTSDIR)/,$(SRCFONTS))

ifneq ($(strip $(SYS)),CYGWIN_NT-6.1)
all: $(FONTS)
else
all: error
endif

$(FONTSDIR)/%.ttf: ./%.ttf $(FONTSDIR)
	sudo cp -f $< $@
	fc-cache -vf

$(FONTSDIR):
	sudo mkdir -p $@

test:
	@echo "FONTS:$(FONTS)"

error:
	$(warn "This program run on linux only.")
