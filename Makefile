MAIN     := main
SUBFILES := $(shell sed -n 's_.*\\subfileinclude{\([^}]*\)}.*_\1_p' main.tex)
LATEX    := pdflatex -interaction=nonstopmode

.PHONY: all clean

report:
	lualatex -interaction=nonstopmode report.tex
	lualatex -interaction=nonstopmode report.tex | tail -n2 | grep 'Output written' | sed 's_.*(\([[:digit:]]*\) pages.*_\\setcounter{page}{\1}_' > nbpages.tex


main:
	$(LATEX) $(MAIN)
	$(foreach s,$(SUBFILES),bibtex $(s);)
	$(LATEX) $(MAIN)
	$(LATEX) $(MAIN)

total: report.pdf main.pdf merge.py
	python3 merge.py

tables:
	uv run xlsx2tex.py combined.xlsx -o tables

map:
	uv run maps/map_gisco.py --scale 03M --dpi 600 -o maps/map_gisco.png

clean:
	rm -f $(foreach ext,aux log out toc bbl blg sta,$(MAIN).$(ext))
	$(foreach s,$(SUBFILES),rm -f $(foreach ext,aux log out toc bbl blg sta,$(s).$(ext));)
