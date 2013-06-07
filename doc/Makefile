LATEX=latex
BIBTEX=bibtex
DVI2PDF=dvipdfm

doc: pgsolver.pdf

pgsolver.pdf: main.dvi
	$(DVI2PDF) -o pgsolver.pdf main.dvi

main.dvi: main.bbl
	$(LATEX) main
	$(LATEX) main

main.bbl: main.aux
	$(BIBTEX) main

main.aux:
	$(LATEX) main

clean:
	rm -f main.dvi main.aux pgsolver.pdf
