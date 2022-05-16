#' @title Test difference
#'
#' @description Test differences in tree lists
#'
#' @param data A list of parsimony scores
#' @param test A test function
#' @param correction logical, whether to correct the p-values
#' @param what which types to compare (1=inapplicable, 2=missing, 3=newstate)
#' 
#' @examples
#'
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export

test.differences <- function(data, test, correction = FALSE, what = c(1,3)) {

    ## lapply tester
    tester <- function(one_data, test, what) {

        if(length(what) == 1) {
            return(test(one_data[[what[1]]]))
        } else {
            if(length(one_data[[what[1]]]) > 2 && length(one_data[[what[2]]]) > 2) {
                return(test(one_data[[what[1]]], one_data[[what[2]]]))
            } else {
                return(NULL)
            }
        }
    }

    ## Returning a table of numeric values
    output.numeric.results <- function(results) {
        ## Transforming list to table
        table_out <- do.call(rbind.data.frame, results)
        ## Getting col names
        colnames(table_out)[1] <- "Probability of overlap"
        return(table_out)
    }

    ## Returning a table for htests
    output.htest.results <- function(results) {

        ## Get elements of interest
        get.elements <- function(one_result) {
            if(!is.null(one_result)) {
                return(c(one_result$statistic[[1]], one_result$parameter[[1]], one_result$p.value))
            } else {
                return(c(NA, NA, NA))
            }
        }

        ## Transforming the results into a table
        table_out <- do.call(rbind.data.frame, lapply(results, get.elements))

        ## Getting col names
        if(ncol(table_out) == 3) {
            colnames(table_out) <- c("statistic", "df", "p.value")    
        } else {
            colnames(table_out) <- c("statistic", "p.value")
        }
        
        return(table_out)
    }

    ## Run the tests
    results <- lapply(data, tester, test = test, what = what)

    ## Getting the output class
    out_class <- unique(unlist(lapply(results, class)))[1]

    ## Numeric output
    if(out_class == "numeric") {
        return(output.numeric.results(results))
    }

    ## htest output
    if(out_class == "htest") {
        data_out <- output.htest.results(results)

        ## Correcting p-values
        if(correction) {
            data_out$p.value <- p.adjust(data_out$p.value, method = "bonferroni")
        }
        return(data_out)
    }
}