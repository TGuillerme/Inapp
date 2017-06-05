#' @title Standardise scores
#'
#' @description Standardises parsimony scores
#'
#' @param data A list of parsimony scores
#' @param name The name of the matrix
#' @param path path to the original matrices
#' @param difference either \code{"substract"} or \code{"divide"} to be applied to the scores
#' 
#' @examples
#'
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export

standardise.score <- function(data) {

    ## Standardise the scores by dividing it by the base score
    data_out <- list()
    for(score in 1:length(data)) {
        data_out[[score]] <- data[[score]]/data$missing[[1]]-1
    }
    names(data_out) <- names(data)
    return(data_out)
}

na.standardise.score <- function(data, name, path = ".") {

    ## Open the matrix
    matrix <- Claddis::ReadMorphNexus(paste(path, paste(name, "nex", sep = "."), sep = "/"))
    ## Get number of NA
    n_nas <- length(apply(matrix$matrix, 2, function(X) any(is.na(X))))
    ## Get the score corrector
    ncol(matrix$matrix)-n_nas

    ## Standardise the scores by dividing by the number of NAs
    data_out <- list()
    for(score in 1:length(data)) {
        data_out[[score]] <- ( (data[[score]] - n_nas) / (data$missing[[1]] - n_nas) )-1
    }
    names(data_out) <- names(data)
    return(data_out)
}


get.inapp.proportion <- function(name, path = ".") {
    ## Open the matrix
    matrix <- Claddis::ReadMorphNexus(paste(path, paste(name, "nex", sep = "."), sep = "/"))
    ## Get the number of cells
    n_cells <- length(matrix$matrix)
    ## Get the number of NAs
    n_nas <- length(which(is.na(as.vector(matrix$matrix)) != FALSE))

    return(n_nas/n_cells)
}

get.score.div <- function(data, difference = "Inapp/NA") {
    if(difference == "Inapp/NA") {
        return(data$inapplicable / data$missing)
    } 
    if(difference == "NS/NA") {
        return(data$newstate / data$missing)
    }
    if(difference == "NS/Inapp") {
        return(data$newstate / data$inapplicable)
    }
    if(difference == "NA/Inapp") {
        return(data$missing / data$inapplicable)
    } 
    if(difference == "NA/NS") {
        return(data$missing / data$newstate)
    }
    if(difference == "Inapp/NS") {
        return(data$inapplicable / data$newstate)
    }
}

get.score.div.na <- function(data, nas, difference = "Inapp/NA") {
    if(difference == "Inapp/NA") {
        return(data$inapplicable / data$missing * nas)
    } 
    if(difference == "NS/NA") {
        return(data$newstate / data$missing * nas)
    }
    if(difference == "NS/Inapp") {
        return(data$newstate / data$inapplicable * nas)
    }
    if(difference == "NA/Inapp") {
        return(data$missing / data$inapplicable * nas)
    } 
    if(difference == "NA/NS") {
        return(data$missing / data$newstate * nas)
    }
    if(difference == "Inapp/NS") {
        return(data$inapplicable / data$newstate * nas)
    }
}