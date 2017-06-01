#' @title Standardise scores
#'
#' @description Standardises parsimony scores
#'
#' @param data A list of parsimony scores
#' @param std The standardisation type
#' 
#' @examples
#'
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export

standardise.score <- function(data, std = "base") {

    if(std == "base") {
        ## Standardise the scores by dividing it by the base score
        data_out <- list()
        for(score in 1:length(data)) {
            data_out[[score]] <- data[[score]]/data$PAUPna-1
        }
        names(data_out) <- names(data)
        return(data_out)
    }
}