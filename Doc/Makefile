all: climacs-internals.pdf climacs-internals.ps

%.eps: %.fig
	fig2dev -Leps -m 0.75  $< $@

climacs-internals.pdf: climacs-internals.ps
	ps2pdf climacs-internals.ps

climacs-internals.ps: climacs-internals.texi undo.eps
	texi2dvi climacs-internals.texi
	dvips -o climacs-internals.ps climacs-internals.dvi

clean:
	rm -f *.aux *.cp *.fn *.fns *.ky *.log *.pdf *.pg *.toc *.tp *.tps *.vr *.dvi *.ps *.eps *~
