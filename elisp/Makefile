EMACS       := emacs
FIND        := find

EMACSFLAGS  := --batch -q --no-site-file
EMACSCMD     = $(EMACS) $(EMACSFLAGS)


all: clean compile


.PHONY: clean
clean:
	$(FIND) $(PWD) -iname "*.elc" -delete


.PHONY: compile
compile:
	$(EMACSCMD) --eval "(byte-recompile-directory \"$(PWD)\" 0)"


.PHONY: install
install: compile
	$(EMACSCMD) \
		--eval "(require 'package)" \
		--eval "(package-install-file \"$(PWD)\")"
