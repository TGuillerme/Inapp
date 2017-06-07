#' @title Read tree scores
#'
#' @description Reads a list of tree lengths
#'
#' @param chain The chain name
#' @param path The path to the file to be read
#' 
#' @examples
#'
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export

read.tree.score <- function(chain, path = ".") {

    chain_list <- list.files(path, pattern = chain)
    full_path <- paste(path, chain_list, sep = "")
    names <- strsplit(chain_list, split = paste(chain, ".", sep = ""))
    names <- unlist(lapply(names, `[[`, 2))
    names <- strsplit(names, split = ".count")
    names <- unlist(lapply(names, `[[`, 1))
    names <- strsplit(names, split = "morphy.")
    names <- unlist(lapply(names, `[[`, 2))


    data <- list()
    for(file in 1:length(chain_list)) {
        data[[file]] <- read.table(full_path[[file]], header = TRUE)[,2]
    }

    names(data) <- names

    return(data)
}

read.matrices <- function(name, path = ".") {
    ## Load the matrix
    matrix <- Claddis::ReadMorphNexus(paste(path, paste(name, "nex", sep = "."), sep = "/"))
    
    return(matrix$matrix)
}