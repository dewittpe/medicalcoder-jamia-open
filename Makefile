RVANILLA := R --vanilla --quiet
RSCRIPTVANILLA := Rscript --vanilla --quiet
QUARTO := quarto

FIGURES := figure1.png figure1.svg figure2.png figure2.svg
RDATA   := figure1.Rdata medicalcoder-vs.Rdata

.PHONY: all

all: medicalcoder-manuscript.docx supplement.pdf

medicalcoder-manuscript.docx: medicalcoder-manuscript.qmd template.docx references.bib $(RDATA) $(FIGURES)
	$(QUARTO) render $<

supplement.pdf: supplement.qmd
	$(QUARTO) render $<

medicalcoder-vs.Rdata: medicalcoder-vs-other-r-packages.R
	$(RSCRIPTVANILLA) $<

figure1.svg figure1.png figure1.Rdata &: figure1-flag-method-example.R
	$(RSCRIPTVANILLA) $<

figure2.svg figure2.png &: figure2-pcccv3-cumulative-flagging.R
	$(RSCRIPTVANILLA) $<
