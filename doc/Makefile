#
# Makefile:
# Build various bits of documentation
#
# Copyright (c) 2003 Chris Lightfoot. All rights reserved.
# Email: chris@ex-parrot.com; WWW: http://www.ex-parrot.com/~chris/
#
# $Id: Makefile,v 1.1 2003/07/16 18:50:14 chris Exp $
#

default: statsnotes.pdf statsnotes.ps

clean:
	rm -f *~ *.bak
	rm -f *.pstex *.pstex_t *.ps *.pdf *.dvi *.log *.lof *.lot *.toc *.aux


# LaTeX stuff. The tex->dvi rule stolen shamelessly from David Richerby.
%.dvi: %.tex
	latex $<
	@while ( grep "Rerun to get cross-references"           \
            $(subst .tex,.log,$<) >/dev/null ); do              \
            echo '** Re-running LaTeX **';                      \
            latex $<;                                           \
        done

# Postscript stuff.
%.ps: %.dvi
	dvips -f -ta4 < $< > $@

%.pdf: %.dvi
	dvips -o - -Ppdf -Pcmz -Pamz $< | ps2pdf /dev/stdin $@
