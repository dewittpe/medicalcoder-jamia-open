RVANILLA := R --vanilla --quiet
RSCRIPTVANILLA := Rscript --vanilla --quiet
QUARTO := quarto

FIGURES := figure1.pdf figure2.svg
RDATA   := mimicivDT.rds objs.rds si.rds
SUPPLEMENTS := mimiciv-data-analysis.pdf
MANUSCRIPT := medicalcoder-manuscript.docx

.PHONY: all

all: $(SUPPLEMENTS) $(FIGURES) $(MANUSCRIPT)

medicalcoder-manuscript.docx: medicalcoder-manuscript.qmd template.docx references.bib $(RDATA)
	$(QUARTO) render $<

mimiciv-data-analysis.pdf $(RDATA) &: mimiciv-data-analysis.qmd
	$(QUARTO) render $<

figure1.pdf: figure1.qmd
	$(QUARTO) render $<

figure2.svg: figure2.R objs.rds
	$(RSCRIPTVANILLA) $<

clean:
	$(RM) -r mimiciv-data-analysis_cache
	$(RM) -r mimiciv-data-analysis_figures
	$(RM) $(RDATA)
	$(RM) $(SUPPLEMENTS)
	$(RM) $(FIGURES)
	$(RM) $(MANUSCRIPT)
