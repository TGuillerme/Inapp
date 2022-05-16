#' @title Run Inapp
#'
#' @description Running the Inapp GUI via shiny
#'
#' @param ... Any optional option to be passed to shiny::runApp
#' @param remote Whether to run the app from a remote server (\code{TRUE}, default - \code{shiny::runGitHub("Inapp", "TGuillerme", ...)}) or from the local server (\code{FALSE} - \code{shiny::runApp(".", ...)}).
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
#' @importFrom TreeTools ReadCharacters
#' @export

runInapp <- function(..., remote = TRUE) {
    if(remote) {
        shiny::runGitHub("Inapp", "TGuillerme", ...)
    } else {
        shiny::runApp(".", ...)
    }
}
