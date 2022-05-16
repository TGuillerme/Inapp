#!/bin/sh

## Compile the html
R -e 'bookdown::render_book("index.Rmd", "bookdown::gitbook")' # HTML version
## Compile the pdf
##   We do not presently compile a PDF as there are issues with the
##   layout and position of figures
## R -e 'bookdown::render_book("index.Rmd", "bookdown::pdf_book")' # PDF version
