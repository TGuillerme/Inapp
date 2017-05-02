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

## Create a note for a node
create.note <- function(node, states_dataframe) {
    return(paste("[", paste(paste(colnames(states_dataframe), states_dataframe[node,], sep = "="), collapse = ","), "]", sep = ""))
}


## Write nexus function (ape)
## In write.nexus, replace write.tree by write.tree.annotated.

## In write.tree, replace .write.tree2 by .write.tree.annotated.


write.tree.annotated <- function(phy, file = "", append = FALSE, digits = 10, tree.names = FALSE, comments)
{
    if (!(inherits(phy, c("phylo", "multiPhylo"))))
        stop("object \"phy\" has no trees")

    if (inherits(phy, "phylo")) phy <- c(phy)
    N <- length(phy)
    res <- character(N)

    if (is.logical(tree.names)) {
        if (tree.names) {
            tree.names <-
                if (is.null(names(phy))) character(N)
                else names(phy)
        } else tree.names <- character(N)
    }

    for (i in 1:N)
        res[i] <- write.tree2.annotated(phy[[i]], digits = digits, tree.prefix = tree.names[i], comments = comments)

    if (file == "") return(res)
    else cat(res, file = file, append = append, sep = "\n")
}

comments <- as.list(paste("[", seq(1:19), "]", sep = ""))

write.tree.annotated(tree, comments = comments)

write.tree2.annotated <- function(phy, digits = 10, tree.prefix = "", comments)
{
    brl <- !is.null(phy$edge.length)
    nodelab <- !is.null(phy$node.label)
    phy$tip.label <- checkLabel(phy$tip.label)
    if (nodelab) phy$node.label <- checkLabel(phy$node.label)
    f.d <- paste("%.", digits, "g", sep = "")

    cp <- function(x){
        STRING[k] <<- x
        k <<- k + 1
    }
    
    cp.annotate.tip <- function(comments){
        STRING[k] <<- comments[[k.tip]]
        k <<- k + 1
        k.tip <<- k.tip + 1
    }
    cp.annotate.node <- function(comments){
        STRING[k] <<- comments[[k.node]]
        k <<- k + 1
        k.node <<- k.node - 1
    }

    add.internal <- function(i) {
        cp("(")
        desc <- kids[[i]]
        for (j in desc) {
            if (j > n) {add.internal(j) ; cp.annotate.node(comments)} ## add comment here
            else {add.terminal(ind[j]) ; cp.annotate.tip(comments)} ## add comment here
            if (j != desc[length(desc)]) cp(",")
        }
        cp(")")
        if (nodelab && i > n) cp(phy$node.label[i - n]) # fixed by Naim Matasci (2010-12-07)
        if (brl) {
            cp(":")
            cp(sprintf(f.d, phy$edge.length[ind[i]]))
        }
    }
    add.terminal <- function(i) {
        cp(phy$tip.label[phy$edge[i, 2]])
        if (brl) {
            cp(":")
            cp(sprintf(f.d, phy$edge.length[i]))
        }
    }

    n <- length(phy$tip.label)

    ## borrowed from phangorn:
    parent <- phy$edge[, 1]
    children <- phy$edge[, 2]
    kids <- vector("list", n + phy$Nnode)
    for (i in 1:length(parent))
        kids[[parent[i]]] <- c(kids[[parent[i]]], children[i])

    ind <- match(1:max(phy$edge), phy$edge[, 2])

    LS <- 4*n + 5 ; if (!missing(comments)) LS <- LS*2 ## add comment here
    if (brl) LS <- LS + 4*n
    if (nodelab)  LS <- LS + n
    STRING <- character(LS)
    k <- 1
    k.tip <- 1
    k.node <- n + n-1

    cp(tree.prefix)
    cp("(")
    getRoot <- function(phy)
        phy$edge[, 1][!match(phy$edge[, 1], phy$edge[, 2], 0)][1]
    root <- getRoot(phy) # replaced n+1 with root - root has not be n+1
    desc <- kids[[root]]
    for (j in desc) {
        if (j > n) {add.internal(j) ; cp.annotate.node(comments)} ## add comment here
        else {add.terminal(ind[j]) ; cp.annotate.tip(comments)} ## add comment here
        if (j != desc[length(desc)]) cp(",")

    }

    if (is.null(phy$root.edge)) {
        cp(")") ; cp.annotate.node(comments) ## add comment here
        if (nodelab) cp(phy$node.label[1])
        cp(";")
    }
    else {
        cp(")") ; cp.annotate.node(comments) ## add comment here
        if (nodelab) cp(phy$node.label[1])
        cp(":")
        cp(sprintf(f.d, phy$root.edge))
        cp(";")
    }
    paste(STRING, collapse = "")
}