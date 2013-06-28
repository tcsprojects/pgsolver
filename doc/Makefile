LATEX=pdflatex
BIBTEX=bibtex
#DVI2PDF=dvipdfm
RENAME=mv

doc: pgsolver.pdf

pgsolver.pdf: main.pdf
#	$(DVI2PDF) -o pgsolver.pdf main.dvi
	$(RENAME) main.pdf pgsolver.pdf

#main.dvi: main.bbl
#	$(LATEX) main
#	$(LATEX) main

main.bbl: main.aux
	$(BIBTEX) main

main.aux:
	$(LATEX) main

main.pdf: main.bbl
	$(LATEX) main

clean:
	rm -f main.dvi main.aux pgsolver.pdf
