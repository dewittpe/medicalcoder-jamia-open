RVANILLA := R --vanilla --quiet
RSCRIPTVANILLA := Rscript --vanilla --quiet
QUARTO := quarto

FIGURES := figure1.pdf figure2.svg figure3.svg figure2data.rds figure3data.rds
RDATA   := mimicivDT.rds objs.rds si.rds benchmarks.rds
RDS     := $(RDATA) figure2data.rds figure3data.rds
SUPPLEMENTS := mimiciv-data-analysis.pdf
MANUSCRIPT := medicalcoder-manuscript.docx

.PHONY: all
.PRECIOUS: $(RDS)

all: $(SUPPLEMENTS) $(FIGURES) $(MANUSCRIPT)

medicalcoder-manuscript.docx: medicalcoder-manuscript.qmd template.docx references.bib mimicivDT.rds objs.rds si.rds figure2data.rds figure3data.rds
	$(QUARTO) render $<

mimiciv-data-analysis.pdf $(RDATA) &: mimiciv-data-analysis.qmd
	$(QUARTO) render $<

figure1.pdf: figure1.qmd
	$(QUARTO) render $<

figure2data.rds figure2.svg &: figure2.R objs.rds
	$(RSCRIPTVANILLA) $<

figure3data.rds figure3.svg &: figure3.R benchmarks.rds
	$(RSCRIPTVANILLA) $<

clean:
	$(RM) -r mimiciv-data-analysis_cache
	$(RM) -r mimiciv-data-analysis_figures
	$(RM) $(RDS)
	$(RM) $(SUPPLEMENTS)
	$(RM) $(FIGURES)
	$(RM) $(MANUSCRIPT)
