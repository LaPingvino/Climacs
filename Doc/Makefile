all: climacs-internals.pdf climacs-internals.ps climacs-user.pdf climacs-user.ps

%.eps: %.fig
	fig2dev -Leps -m 0.75  $< $@

climacs-internals.pdf: climacs-internals.ps
	ps2pdf climacs-internals.ps

climacs-internals.ps: climacs-internals.texi undo.eps
	texi2dvi climacs-internals.texi
	dvips -o climacs-internals.ps climacs-internals.dvi

climacs-user.pdf: climacs-user.texi
	texi2pdf climacs-user.texi

climacs-user.ps: climacs-user.texi
	texi2dvi climacs-user.texi
	dvips -o climacs-user.ps climacs-user.dvi

clean:
	rm -f *.aux *.cp *.fn *.fns *.ky *.log *.pdf *.pg *.toc *.tp *.tps *.vr *.dvi *.ps *.eps *~
