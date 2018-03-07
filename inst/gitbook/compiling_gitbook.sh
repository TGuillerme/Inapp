#!/bin/sh

## Compile the html
R -e 'bookdown::render_book("index.Rmd", "bookdown::gitbook")' # HTML version
## Compile the pdf
R -e 'bookdown::render_book("index.Rmd", "bookdown::pdf_book")' # PDF version
