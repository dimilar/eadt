EMACS=emacs

# Where local software is found
prefix=/usr/local

# Where local lisp files go.
lispdir   = $(prefix)/share/emacs/site-lisp

# Where info files go.
infodir = $(prefix)/share/info


# Using emacs in batch mode.

BATCH=$(EMACS) -batch -q -no-site-file -eval                             			\
  "(setq load-path (cons (expand-file-name \"./\") (cons \"$(lispdir)\" load-path)))"

# Specify the byte-compiler for compiling android-mode files
ELC= $(BATCH) -f batch-byte-compile

# The following variables need to be defined by the maintainer
LISPF      = 	android.el			\
		android-command.el		\
		android-debug.el		\
		android-gen.el		\
		android-init.el		\
		android-launch.el		\
		android-project.el		\
		android-target.el		\
		android-tools.el		\

.SUFFIXES: .el .elc .texi
SHELL = /bin/sh

# Additional distribution files
DISTFILES_extra=  Makefile request-assign-future.txt contrib

ELCFILES=$(LISPF:.el=.elc)

all:	$(ELCFILES)

default: $(ELCFILES)

up2:	update
	sudo ${MAKE} install

update:
	git pull
	${MAKE} clean
	${MAKE} all
clean:
	@rm -f *.elc

compile: $(ELCFILES)

install: install-lisp

autoloads: android-autolaod.el

android-autoload.el: $(LISPFILES0) Makefile
	$(BATCH) --eval "(require 'autoload)" \
		--eval '(find-file "android-autoload.el")'  \
		--eval '(erase-buffer)' \
		--eval '(mapc (lambda (x) (generate-file-autoloads (symbol-name x))) (quote ($(LISPF))))' \
		--eval '(insert "\n(provide (quote android-autoload))\n")' \
		--eval '(save-buffer)'

# Dependencies

android.elc:	android.el
android-tools.elc:	android.el
android-init.elc:	android.el
android-command.elc:	android.el
android-debug.elc:	android.el
android-launch.elc:	android.el
android-target.elc:	android.el
android-project.elc:	android.el
android-gen.elc:	android.el


.el.elc:
	$(ELC) $<
