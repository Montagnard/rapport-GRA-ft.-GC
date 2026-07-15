MAIN     := main
SUBFILES := $(shell sed -n 's_.*\\subfileinclude{\([^}]*\)}.*_\1_p' main.tex)
LATEX    := pdflatex -interaction=nonstopmode

.PHONY: all clean

all:
	$(LATEX) $(MAIN)
	$(foreach s,$(SUBFILES),bibtex $(s);)
	$(LATEX) $(MAIN)
	$(LATEX) $(MAIN)

clean:
	rm -f $(foreach ext,aux log out toc bbl blg sta,$(MAIN).$(ext))
	$(foreach s,$(SUBFILES),rm -f $(foreach ext,aux log out toc bbl blg sta,$(s).$(ext));)
