#' @title Run Inapp
#'
#' @description Running the Inapp GUI via shiny
#'
#' @param ... Any optional option to be passed to shiny::runApp
#' 
#' @examples
#' \dontrun{
#' ## Running the GUI on your web browser
#' runInapp()
#' }
#' 
#' @seealso \code{\link{apply.reconstruction}}, \code{\link{plot.states.matrix}}
#' 
#' @author Thomas Guillerme
#' @export

runInapp <- function(...) {
    shiny::runGitHub("Inapp", "TGuillerme", ...)
}