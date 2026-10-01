# medicalcoder JAMIA Open Manuscript

This repo contains all the source code needed to reproduce the application note
published in JAMIA Open.

> Peter E DeWitt, Seth Russell, James A Feinstein, Margaret N Rebull, Tellen D
> Bennett, medicalcoder: a unified and longitudinally aware framework for
> International Classification of Diseases code-based comorbidity assessment in
> R, JAMIA Open, Volume 9, Issue 5, October 2026, ooag182,
> https://doi.org/10.1093/jamiaopen/ooag182

## JAMIA Open Submission

[Application Note](https://academic.oup.com/jamiaopen/pages/General_Instructions)
Descriptions of computer software or algorithm implementations, including web
accessible services and mobile applications. The structured abstract should
contain the headings: Objectives, Materials and Methods, Results, Discussion,
and Conclusion. The main text should, in addition to the sections corresponding
to these headings, include a section describing Background and Significance.
Manuscripts must include a link to a publicly accessible code repository (e.g.,
GitHub or BitBucket) and, as applicable, reference to a Jupyter notebook for
sharing functional code examples.

* Word count: up to 2000 words.
* Abstract: up to 150 words.
* Tables: up to 2.
* Figures: up to 3.
* References: unlimited.

### Submissions

* tag v1.0.2 submitted, decision - revise and resubmit.  resubmission due
  18-July-2026

## Development work

* System dependencies:
  * [R](https://cran.r-project.org/)
  * [GNU make](https://www.gnu.org/software/make/)
  * [pandoc](https://pandoc.org/)
  * [quarto](https://quarto.org/)

The application note and supplemental file can be built by calling

    make

Convert a .docx to markdown for a good, but not perfect, way to compare the .qmd
source to a modified .docx

    pandoc manuscript.docx -f docx -t markdown --columns=80 -o manuscript.md
