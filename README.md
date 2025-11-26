# medicalcoder JAMIA Open Manuscript

This repo contains all the source code needed to reproduce the application note
submitted to JAMIA Open.

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
