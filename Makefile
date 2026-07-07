RVANILLA := R --vanilla --quiet
RSCRIPTVANILLA := Rscript --vanilla --quiet
QUARTO := quarto

FIGURES := figure1.pdf figure2.pdf figure3.pdf
SUPPLEMENTS := mimiciv-data-analysis.pdf
MANUSCRIPT := medicalcoder-manuscript.docx

.PHONY: all clean purge

all: $(SUPPLEMENTS) $(FIGURES) $(MANUSCRIPT)

medicalcoder-manuscript.docx: medicalcoder-manuscript.qmd template.docx references.bib $(SUPPLEMENTS) $(FIGURES)
	$(QUARTO) render $<

mimiciv-data-analysis.pdf figure2.pdf figure3.pdf &: mimiciv-data-analysis.qmd mimiciv-data-analysis.R references.bib
	$(QUARTO) render $<

figure1.pdf: figure1.qmd
	$(QUARTO) render $<

clean:
	$(RM) $(SUPPLEMENTS)
	$(RM) $(FIGURES)
	$(RM) $(MANUSCRIPT)

purge: clean
	$(RM) -r mimiciv-data-analysis_cache
	$(RM) -r mimiciv-data-analysis_figures
