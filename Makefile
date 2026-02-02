RVANILLA := R --vanilla --quiet
RSCRIPTVANILLA := Rscript --vanilla --quiet
QUARTO := quarto

FIGURES := figure1.png figure1.svg figure2.png figure2.svg
RDATA   := figure1.Rdata medicalcoder-vs.Rdata figure2permutations.rds

.PHONY: all

all: medicalcoder-manuscript.docx

medicalcoder-manuscript.docx: medicalcoder-manuscript.qmd template.docx references.bib $(RDATA) $(FIGURES)
	$(QUARTO) render $<

medicalcoder-vs.Rdata: medicalcoder-vs-other-r-packages.qmd
	$(QUARTO) render $<

figure1.svg figure1.png figure1.Rdata &: figure1-flag-method-example.R
	$(RSCRIPTVANILLA) $<

figure2.svg figure2.png figure2permutations.rds &: figure2-pcccv3-cumulative-flagging.R
	$(RSCRIPTVANILLA) $<

clean:
	$(RM) -r medicalcoder-vs-other-r-packages_cache
	$(RM) -r medicalcoder-vs-other-r-packages_files
