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
    density_paup <- density(scores$newstate)
    density_inapplicable <- density(scores$inapplicable)

    ## Get the graphic parameters
    xlim <- c(scores$missing[1], max(c(scores$newstate, scores$inapplicable)))
    xlab <- ifelse(scores$missing[1] == 0, "Relative steps", "Number of steps")

    ## Plotting the first curve
    plot(density_paup, xlim = xlim, col = "blue", xlab = xlab, ...)
    lines(density_inapplicable, col = "orange")
    legend("topleft", legend = c("- = New state", "- = inapplicable"), col = c("blue", "orange"), lty = 1, cex = 0.8)
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
    get.density <- function(one_score, what, relative) { # what = 1 -> inapplicable; what = 3 -> newstate;
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
    densities_inapplicable <- lapply(scores_list, get.density, what = 1, relative = relative.density)

    ## Graphical parameters
    ## Get the xlimits
    x_lim <- range(unlist(lapply(densities_paup, get.range, what = 1)), unlist(lapply(densities_inapplicable, get.range, what = 1)))

    ## Get the ylimits (if density is not relative)
    if(relative.density) {
        y_lim <- c(0,1)
    } else {
        y_lim <- range(unlist(lapply(densities_paup, get.range, what = 2)), unlist(lapply(densities_inapplicable, get.range, what = 2)))
    }

    ## Axis labels    
    xlab <- ifelse(scores_list[[1]]$missing[1] == 0, "Relative steps", "Number of steps")
    ylab <- ifelse(relative.density == TRUE, "Relative density", "Density")

    ## Empty plot
    plot(0,0, xlim = x_lim, ylim = y_lim, col = "white", xlab = xlab, ylab = ylab, ...)
    
    ## Add the densities
    for(density in 1:length(densities_paup)) {
        lines(densities_paup[[density]], col = "blue")
        lines(densities_inapplicable[[density]], col = "orange")
    }

    ## Add the legend
    legend("topleft", legend = c("- = New state", "- = inapplicable"), col = c("blue", "orange"), lty = 1, cex = 0.5)
}


sauronplot <- function(proportions_combined, CI = c(95, 50)) {

    CI.converter <- function(CI) {
        sort(c(50-CI/2, 50+CI/2)/100)
    }

    ## Plotting one tie
    one.plot <- function(position, data, CI) {
        ## Polygon coordinates
        ys <- c(data[,1], rev(data[,1]))
        xs <- c((position-data[,2]), rev(position+data[,3]))

        ## Plot the polygon
        polygon(xs, ys, col = "grey")

        ## Add the boxplot
        for(q in 1:length(CI)) {
            lines(x = rep(position, 2), y = quantile(data[,1], probs = CI.converter(CI[q])), lty = 1, lwd = q + (0.5 * q - 0.5) )
        }
        median <- median(data[,1])
        points(x = position, y = median, pch = 19, cex = 2)
    }

    ## Plot window
    plot(1,1, ylim = c(0,1), xlim = c(0,2), col = "white")

    ## Inapp data
    inapp_data <- proportions_combined[,1:3]
    inapp_data <- inapp_data[order(inapp_data[,1]),]

    ## New state data
    extra_data <- proportions_combined[,4:6]
    extra_data <- extra_data[order(extra_data[,1]),]
    
    ## Adding the ties
    one.plot(0.5, inapp_data, CI)
    one.plot(1.5, extra_data, CI)

    ## Add the legend
    lines(c(0,0.1), c(0.1, 0.1))
    text(0.1, 0.05, "Proportional\nextra length", cex = 0.8)

}