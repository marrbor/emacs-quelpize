DIRS:=lisp source

.PHONY: $(DIRS)

$(DIRS):
	cd $@ && make

lisp: source

