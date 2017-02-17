#############################################################################
#
#  File originallly generated by ocp-autoconf.
#
#############################################################################

include autoconf/Makefile.config

# Always build ocp-annot, build ocp-genannot only if ocaml_cmt is not
# provided by the current OCaml distribution.

all:
	ocp-build ocp-annot
	if test -f $(bindir)/ocaml_cmt; then :; else \
	   ocp-build ocp-genannot; \
	fi

install:
	ocp-build install ocp-annot
	if test -f _obuild/ocp-genannot/ocp-genannot.asm; then \
	  cp _obuild/ocp-genannot/ocp-genannot.asm \
	    $(bindir)/ocp-genannot; \
	fi

clean: ocp-build-clean

distclean: clean ocp-distclean
	find . -name '*~' -exec rm -f {} \;

include autoconf/Makefile.rules

