## Making an output matrix
make.output.data.frame <- function(states.matrix) {
    ## Create the output table
    output_matrix <- as.data.frame(matrix(NA, nrow = length(states.matrix$Char), ncol = 7))
    colnames(output_matrix) <- c("label", names(states.matrix)[2:5], "Changes", "Regions")


    if(is.null(states.matrix$tree$node.label)) {
        output_matrix[,1] <- c(states.matrix$tree$tip.label, paste("n", (ape::Ntip(states.matrix$tree)+1):length(states.matrix$Char), sep = ""))
    } else {
        output_matrix[,1] <- c(states.matrix$tree$tip.label, states.matrix$tree$node.label)
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
    for(recon in 2:5) {
        output_matrix[,recon] <- unlist(output_list[[recon-1]])
    }

    ## Add the changes
    output_matrix[,6] <- FALSE
    if(length(states.matrix$changes) != 0) {
        output_matrix[states.matrix$changes, 6] <- TRUE
    }

    ## Add the applicable regions
    output_matrix[,7] <- TRUE
    if(!is.null(unlist(states.matrix$tracker[[4]]))) {
        output_matrix[, 7] <- unlist(states.matrix$tracker[[4]])
    }    

    ## If Fitch, remove the extra columns
    if(is.null(unlist(states.matrix$Dp2)) && is.null(unlist(states.matrix$Up2))) {
        output_matrix <- output_matrix[,-c(4,5,7)]
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
    return(paste("[", paste(paste(colnames(states_dataframe)[-1], states_dataframe[node,c(-1)], sep = "="), collapse = ","), "]", sep = ""))
}


## Modified version of ape::.write.tree
write.tree.commented <- function(phy, file = "", append = FALSE, digits = 10, tree.names = FALSE, comments)
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
        res[i] <- write.tree2.commented(phy[[i]], digits = digits, tree.prefix = tree.names[i], comments = comments)

    if (file == "") return(res)
    else cat(res, file = file, append = append, sep = "\n")
}

## Modified version of ape::.write.tree2
write.tree2.commented <- function(phy, digits = 10, tree.prefix = "", comments)
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
        STRING[k] <<- comments
        k <<- k + 1
    }

    add.internal <- function(node) {
        cp("(")
        desc <- kids[[node]]
        for (one_kid in desc) {
            if (one_kid > n) {add.internal(one_kid)}
            else {add.terminal(ind[one_kid]) ; cp.annotate.tip(comments)} ## add comment here
            if (one_kid != desc[length(desc)]) cp(",")
        }
        cp(")") ; cp.annotate.node(comments[[node]]) ## add comment here
        if (nodelab && node > n) cp(phy$node.label[node - n]) # fixed by Naim Matasci (2010-12-07)
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

    LS <- 4*n + 5 ; if (!missing(comments)) LS <- LS*2
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
        if (j > n) {add.internal(j)} ## add comment here
        else {add.terminal(ind[j]) ; cp.annotate.tip(comments)} ## add comment here
        if (j != desc[length(desc)]) cp(",")

    }

    if (is.null(phy$root.edge)) {
        cp(")") ; cp.annotate.node(comments[[n+1]]) ## add comment here
        if (nodelab) cp(phy$node.label[1])
        cp(";")
    }
    else {
        cp(")") ; cp.annotate.node(comments[[n+1]]) ## add comment here
        if (nodelab) cp(phy$node.label[1])
        cp(":")
        cp(sprintf(f.d, phy$root.edge))
        cp(";")
    }
    paste(STRING, collapse = "")
}

## Modified version of ape::.write.nexus
write.nexus.commented <- function (phy, file = "", translate = TRUE, comments = comments) {

    obj <- list(phy)
    class(obj) <- "multiPhylo"

    ntree <- length(obj)
    cat("#NEXUS\n", file = file)
    cat(paste("[Inapp ancestral states reconstructions, ", date(), "]\n", sep = ""), 
        file = file, append = TRUE)
    cat("[More for at https://github.com/TGuillerme/Inapp/]\n\n", file = file, append = TRUE)
    N <- length(obj[[1]]$tip.label)
    cat("BEGIN TAXA;\n", file = file, append = TRUE)
    cat(paste("\tDIMENSIONS NTAX = ", N, ";\n", sep = ""), file = file, 
        append = TRUE)
    cat("\tTAXLABELS\n", file = file, append = TRUE)
    cat(paste("\t\t", obj[[1]]$tip.label, sep = ""), sep = "\n", 
        file = file, append = TRUE)
    cat("\t;\n", file = file, append = TRUE)
    cat("END;\n", file = file, append = TRUE)
    cat("BEGIN TREES;\n", file = file, append = TRUE)
    if (translate) {
        cat("\tTRANSLATE\n", file = file, append = TRUE)
        obj <- .compressTipLabel(obj)
        X <- paste("\t\t", 1:N, "\t", attr(obj, "TipLabel"), 
            ",", sep = "")
        X[length(X)] <- gsub(",", "", X[length(X)])
        cat(X, file = file, append = TRUE, sep = "\n")
        cat("\t;\n", file = file, append = TRUE)
        class(obj) <- NULL
        for (i in 1:ntree) {obj[[i]]$tip.label <- as.character(1:N)}
    } else {
        if (is.null(attr(obj, "TipLabel"))) {
            for (i in 1:ntree) obj[[i]]$tip.label <- checkLabel(obj[[i]]$tip.label)
        }
        else {
            attr(obj, "TipLabel") <- checkLabel(attr(obj, "TipLabel"))
            obj <- .uncompressTipLabel(obj)
        }
    }
    title <- names(obj)
    if (is.null(title)) {
        title <- rep("UNTITLED", ntree)
    } else {
        if (any(s <- title == "")) {
            title[s] <- "UNTITLED"
        }
    }
    for (i in 1:ntree) {
        if (class(obj[[i]]) != "phylo") {
            next
        }
        root.tag <- ifelse(is.rooted(obj[[i]]), "= [&R] ", "= [&U] ")
        cat("\tTREE *", title[i], root.tag, file = file, append = TRUE)
        cat(write.tree.commented(obj[[i]], file = "", comments = comments), "\n", sep = "", file = file, append = TRUE)
    }
    cat("END;\n", file = file, append = TRUE)
}

## Converts a digit into it's binary equivalent
convert.binary.value <- function(vector, states_matrix) {
    ## Get all the states to order them
    all_states <- sort(unique(unlist(states_matrix$Char)))
    new_states <- seq(from = 0, to = (length(all_states)-1))

    ## Translating states
    translate.states <- function(state, all_states, new_states) {
        return(new_states[match(state, all_states)])
    }

    ## Translating the vector
    translated_vector <- lapply(vector, translate.states, all_states, new_states)

    ## Convert vector
    converted_vector <- lapply(translated_vector, function(x) sum(2^x))

    return(converted_vector)
}