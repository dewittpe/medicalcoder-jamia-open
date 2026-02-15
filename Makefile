RVANILLA := R --vanilla --quiet
RSCRIPTVANILLA := Rscript --vanilla --quiet
QUARTO := quarto

FIGURES :=
RDATA   := mimicivDT.rds objs.rds si.rds
SUPPLEMENTS := mimiciv-data-analysis.pdf
MANUSCRIPT := medicalcoder-manuscript.docx

.PHONY: all

all: $(SUPPLEMENTS) $(MANUSCRIPT)

medicalcoder-manuscript.docx: medicalcoder-manuscript.qmd template.docx references.bib $(RDATA) $(FIGURES)
	$(QUARTO) render $<

mimiciv-data-analysis.pdf $(RDATA) &: mimiciv-data-analysis.qmd
	$(QUARTO) render $<

clean:
	$(RM) -r mimiciv-data-analysis_cache
	$(RM) -r mimiciv-data-analysis_figures
	$(RM) $(RDATA)
	$(RM) $(SUPPLEMENTS)
	$(RM) $(MANUSCRIPT)
