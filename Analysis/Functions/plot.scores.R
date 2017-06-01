#' @title Plot scores
#'
#' @description Plots the density of steps under the different algorithms
#'
#' @param scores the list of scores
#' @param ... graphical parameters to be passed to ...
#' 
#' @examples
#'
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export

plot.scores <- function(scores, ...) {
    ## Estimate densities
    density_paup <- density(scores$PAUPns)
    density_morphy <- density(scores$morphy)

    ## Get the graphic parameters
    xlim <- c(scores$PAUPna[1], max(c(scores$PAUPns, scores$morphy)))
    xlab <- ifelse(scores$PAUPna[1] == 0, "Relative steps", "Number of steps")

    ## Plotting the first curve
    plot(density_paup, xlim = xlim, col = "blue", xlab = xlab, ...)
    lines(density_morphy, col = "orange")
    legend("topleft", legend = c("- = New state", "- = morphy"), col = c("blue", "orange"), lty = 1, cex = 0.8)
}

#' @title Plot scores list
#'
#' @description Plots the density of steps under the different algorithms for a list
#'
#' @param scores_list a list of list of scores
#' @param relative.density logical, whether to plot the relative density (TRUE) or the absolute one (FALSE)
#' @param ... graphical parameters to be passed to ...
#' 
#' @examples
#'
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export

plot.scores.list <- function(scores_list, relative.density, ...) {

    ## Getting the densities (relative or not)
    get.density <- function(one_score, what, relative) { # what = 1 -> morphy; what = 3 -> PAUPns;
        ## Get the density
        density <- density(one_score[[what]])

        if(relative) {
            ## Scale the density
            density$y <- density$y/max(density$y)
        }

        return(density)
    }

    ## Getting the range of one density profile
    get.range <- function(one_density, what) { # what = 1 -> x; what = 2 -> y
        return(range(one_density[[what]]))
    }


    ## Estimate the densities
    densities_paup <- lapply(scores_list, get.density, what = 3, relative = relative.density)
    densities_morphy <- lapply(scores_list, get.density, what = 1, relative = relative.density)

    ## Graphical parameters
    ## Get the xlimits
    x_lim <- range(unlist(lapply(densities_paup, get.range, what = 1)), unlist(lapply(densities_morphy, get.range, what = 1)))

    ## Get the ylimits (if density is not relative)
    if(relative.density) {
        y_lim <- c(0,1)
    } else {
        y_lim <- range(unlist(lapply(densities_paup, get.range, what = 2)), unlist(lapply(densities_morphy, get.range, what = 2)))
    }

    ## Axis labels    
    xlab <- ifelse(scores_list[[1]]$PAUPna[1] == 0, "Relative steps", "Number of steps")
    ylab <- ifelse(relative.density == TRUE, "Relative density", "Density")

    ## Empty plot
    plot(0,0, xlim = x_lim, ylim = y_lim, col = "white", xlab = xlab, ylab = ylab, ...)
    
    ## Add the densities
    for(density in 1:length(densities_paup)) {
        lines(densities_paup[[density]], col = "blue")
        lines(densities_morphy[[density]], col = "orange")
    }

    ## Add the legend
    legend("topleft", legend = c("- = New state", "- = morphy"), col = c("blue", "orange"), lty = 1, cex = 0.5)
}