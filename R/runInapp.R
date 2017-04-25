#' @title Run Inapp
#'
#' @description Running the Inapp GUI via shiny
#'
#' @param ... Any optional option to be passed to shiny::runApp
#' 
#' @examples
#' 
#' @author Thomas Guillerme
#' @export


## Inspire from squid::squidApp
runInapp <- function(...) {
    
    ## Get the directory
    path <- system.file("", package = "Inapp")
        
    ## Check path
    if(path == "") {
        stop("Inapp shiny application not found. Try re-installing `Inapp` package.")
    }
    
    shiny::runApp(appDir = path, ...)
}