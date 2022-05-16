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


#' @title Sauron plots
#'
#' @description Plots the results with percentage of discarded trees on the y and range of extra length on the x
#'
#' @param proportions_combined the data with the proportion of rejected tree and the associated minimum and maximum extra length
#' @param CI the confidence intervals to be plotted
#' @param names the names of each plot
#' @param plot.range the range of the plot (how many outliers should be kept laterally)
#' @param ... graphical parameters to be passed to plot
#' 
#' @examples
#'
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export


sauronplot <- function(proportions_combined, CI = c(95, 50), names, plot.range = 0.9, scale.bar = 0.5, ...) {

    CI.converter <- function(CI) {
        sort(c(50-CI/2, 50+CI/2)/100)
    }

    ## Plotting one tie
    one.plot <- function(position, data, CI) {
        match.xs <- function(y_point, x_data, y_data, type) {
            ## Selecting the x values for y
            return(type(x_data[y_data %in% y_point]))
        }
        ## Polygon coordinates
        # ys <- c(data[,1], rev(data[,1]))
        # xs <- c((position-data[,2]), rev(position+data[,3]))

        y_points <- unique(data[,1])
        ys <- c(y_points, rev(y_points))

        x_points1 <- position-sapply(y_points, match.xs, data[,2], data[,1], min)
        x_points2 <- position+sapply(y_points, match.xs, data[,3], data[,1], max)
        xs <- c(x_points1, rev(x_points2))

        ## Plot the polygon
        polygon(xs, ys, col = "grey")

        ## Add the boxplot
        ltys <- c(2, rep(1, (length(CI)-1)))
        for(quant in 1:length(CI)) {
            lines(x = rep(position, 2), y = quantile(data[,1], probs = CI.converter(CI[quant])), lty = ltys[quant], lwd = quant + (0.5 * quant - 0.5) )
        }

        median <- median(data[,1])
        points(x = position, y = median, pch = 19, cex = 2)
    }

    ## Detecting plot range
    positions <- ncol(proportions_combined)/3
    ranges <- list()
    for(pos in 1:positions) {
        ## Get the second lowest/highest value (to avoid outliers)
        ranges[[pos]] <- range(c(
            -quantile(proportions_combined[,(pos*3-1)], prob = c(plot.range)),
            quantile(proportions_combined[,(pos*3)], prob = c(plot.range))))
    }

    ## Setting up the plot length
    xmax <- sum(ceiling(unlist(lapply(ranges, abs))))
    if(all(unlist(lapply(ranges, abs)) < 0.1)) {
        ## Scale the ranges if lower than 0.1.
        xmax <- xmax/10
        scale_0.1 <- TRUE
    } else {
        scale_0.1 <- FALSE
    }

    ## Setting up the positions
    xpos <- numeric()
    ## First position
    xpos[1] <- ceiling(abs(ranges[[1]])[1])
    for(pos in 2:positions) {
        ## Other positions
        xpos[pos] <- sum(ceiling(abs(ranges[[pos-1]])))+ceiling(abs(ranges[[pos]])[1])
    } 
    if(scale_0.1) {
        xpos/10
    }

    ## Plot window (1.05 for ylim is to allow to print the title)
    plot(1,1, ylim = c(0,1.05), xlim = c(0,xmax), col = "white", xaxt = "n", ...)
    # plot(1,1, ylim = c(0,1.05), xlim = c(0,xmax), col = "white", xaxt = "n") ; warning("DEBUG")

    ## Separating the data
    data_list <- list()
    for(data in 1:positions) {
        data_list[[data]] <- proportions_combined[,(data*3-2):(data*3)]
        data_list[[data]] <- data_list[[data]][order(data_list[[data]][,1]), ]
    }
    
    ## Plotting the data
    for(plot in 1:positions) {
        one.plot(xpos[plot], data_list[[plot]], CI)
    }

    ## Add the legend
    for(name in 1:pos) {
        text(xpos[name], y = 1.05, names[name], cex = 1)
    }

    ## X axis
    for(pos in 1:positions) {
        scale_labels <- paste(scale.bar, c("min", "max"), sep = "\n")
        axis(1, c((xpos[pos]-scale.bar), xpos[pos], (xpos[pos]+scale.bar)), tick = TRUE, labels = c(scale_labels[1], "0\n", scale_labels[2]), padj = 0.3)
    }

}