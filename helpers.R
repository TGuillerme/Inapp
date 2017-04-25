## Packages
require(ape)

## Functions
sourceDir <- function(path, ...) {
    for (name_file in list.files(path, pattern = "[.][RrSsQq]$")) {
        source(file.path(path, name_file), ...)
    }
}

sourceDir("R/")