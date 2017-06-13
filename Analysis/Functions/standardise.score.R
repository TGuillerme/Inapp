#' @title Standardise scores
#'
#' @description Standardises parsimony scores
#'
#' @param data A list of parsimony scores
#' @param name The name of the matrix
#' @param path path to the original matrices
#' @param difference either \code{"substract"} or \code{"divide"} to be applied to the scores
#' @param matrix the character matrix
#' 
#' @examples
#'
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export

standardise.score <- function(data, matrix, std = c("MP", "ntax", "nchar", "nas")) {
    
    ## Standardise the scores by dividing it by the proportion of nas
    if(any(std == "nas")) {
        nnas <- length(which(is.na(as.vector(matrix))))/length(matrix)
        for(score in 1:length(data)) {
            data[[score]] <- data[[score]]*nnas
        }
    }

    ## Standardise the scores by dividing it by the number of taxa
    if(any(std == "ntax")) {
        ntax <- nrow(matrix)
        for(score in 1:length(data)) {
            data[[score]] <- data[[score]]/ntax
        }
    }    

    ## Standardise the scores by dividing it by the number of characters with NAs
    if(any(std == "nchar")) {
        nchar <- length(unlist(apply(matrix, 2, function(X) any(is.na(X)))))
        for(score in 1:length(data)) {
            data[[score]] <- data[[score]]/nchar
        }
    }

    ## Standardise the scores by dividing it by the most parsimonious score
    if(any(std == "MP")) {
        MP <- data$missing[1]
        for(score in 1:length(data)) {
            data[[score]] <- data[[score]]/MP-1
        }
    }

    return(data)
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


## Get the the proportion of none minimum tree
get.proportion <- function(data) {
    ## Getting the proportional score for one algorithm
    get.prop.lapply <- function(one_score) {
        min_val <- min(one_score)
        score_l <- length(one_score)
        return(length(which(one_score != min_val))/score_l)
    }

    ## Getting all the proportional scores
    return(lapply(data, get.prop.lapply))
}