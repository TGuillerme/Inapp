## Packages
require(ape)

## Functions
sourceDir <- function(path, ...) {
    for (name_file in list.files(path, pattern = "[.][RrSsQq]$")) {
        source(file.path(path, name_file), ...)
    }
}

plotError <- function(text, col='#cc6644', font=1, cex=1.6, ...) {
  plot(-1, -1, xlim=c(0, 1), ylim=c(0,1), axes=F, xlab='', ylab='')
  text(0, 1, pos=4, paste("Cannot plot tree.", text), col=col, font=font, cex=cex, ...)
}

sourceDir("R/")
