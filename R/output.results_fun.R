## Making an output matrix
make.output.data.frame <- function(states.matrix) {
    ## Create the output table
    output_matrix <- as.data.frame(matrix(NA, nrow = length(states.matrix$Char), ncol = 6))
    colnames(output_matrix) <- c(names(states.matrix)[2:5], "Changes", "Regions")
    if(is.null(states.matrix$tree$node.label)) {
        rownames(output_matrix) <- c(states.matrix$tree$tip.label, paste("n", (ape::Ntip(states.matrix$tree)+1):length(states.matrix$Char), sep = ""))
    } else {
        rownames(output_matrix) <- c(states.matrix$tree$tip.label, tree$node.label)
    }
    
    ## Selecting all states (for missing data)
    all_states <- sort((unique(unlist(states.matrix$Char))))

    ## Collapsing the data
    collapse.data <- function(data, all_states) {
        if(all(all_states %in% data)){
            return("?")
        } else {
            return(paste(gsub(-1, "-", data), collapse = ""))
        }
    }

    ## Collapse the character names
    output_list <- lapply(states.matrix[2:5], lapply, collapse.data, all_states = all_states)

    ## Fill up the matrix
    for(recon in 1:4) {
        output_matrix[,recon] <- unlist(output_list[[recon]])
    }

    ## Add the changes
    output_matrix[,5] <- FALSE
    if(length(states.matrix$changes) != 0) {
        output_matrix[states.matrix$changes, 5] <- TRUE
    }

    ## Add the applicable regions
    output_matrix[,6] <- TRUE
    if(!is.null(unlist(states.matrix$tracker[[4]]))) {
        output_matrix[, 6] <- unlist(states.matrix$tracker[[4]])
    }    

    ## If Fitch, remove the extra columns
    if(is.null(unlist(states.matrix$Dp2)) && is.null(unlist(states.matrix$Up2))) {
        output_matrix <- output_matrix[,-c(3,4,6)]
    }

    return(output_matrix)
}


## Asking for confirmation
read.key <- function(msg1, msg2, scan = TRUE) {
    message(msg1)
    if(scan == TRUE) {
        scan(n = 1, quiet = TRUE)
    }
    silent <- "yes"
    if(!missing(msg2)) {
        message(msg2)
    }
}