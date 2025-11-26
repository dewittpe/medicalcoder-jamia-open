RVANILLA := R --vanilla --quiet
RSCRIPTVANILLA := Rscript --vanilla --quiet
QUARTO := quarto

FIGURES := figure1.png figure1.svg figure2.png figure2.svg

.PHONY: all

all: medicalcoder-manuscript.docx

medicalcoder-manuscript.docx: medicalcoder-manuscript.qmd template.docx references.bib medicalcoder-vs.Rdata $(FIGURES)
	$(QUARTO) render $<

medicalcoder-vs.Rdata: medicalcoder-vs-other-r-packages.R
	$(RSCRIPTVANILLA) $<

figure1.svg figure1.png &: figure1-flag-method-example.R
	$(RSCRIPTVANILLA) $<

figure2.svg figure2.png &: figure2-pcccv3-cumulative-flagging.R
	$(RSCRIPTVANILLA) $<
