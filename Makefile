RVANILLA := R --vanilla --quiet
RSCRIPTVANILLA := Rscript --vanilla --quiet
QUARTO := quarto

FIGURES :=
RDATA   :=
SUPPLEMENTS := mimiciv-data-analysis.pdf
MANUSCRIPT := medicalcoder-manuscript.docx

.PHONY: all

all: $(SUPPLEMENTS) #$(MANUSCRIPT)

medicalcoder-manuscript.docx: medicalcoder-manuscript.qmd template.docx references.bib $(RDATA) $(FIGURES)
	$(QUARTO) render $<

%.pdf: %.qmd
	$(QUARTO) render $<

clean:
	$(RM) -r *_cache
