source("Functions/read.tree.score.R")
source("Functions/standardise.score.R")
source("Functions/plot.scores.R")

library(Inapp)
library(Claddis)

## Read the tree scores
path <- "Data/Scores/"
chains <- list.files(path, pattern = ".morphy.inapplicable.count")
chains <- unlist(strsplit(chains, split = ".morphy.inapplicable.count"))

## Get the scores
scores <- sapply(chains, read.tree.score, path, simplify = FALSE)

## Standardise the scores
scores_std <- lapply(scores, standardise.score)

## Plot the normal score differences for a single distributions
plot.scores(scores_std[[10]])

## Plot all the densities
plot.scores.list(scores_std, relative.density = TRUE)

## Get the proportion of nas
nas_prop <- lapply(as.list(names(scores)), get.inapp.proportion, path = "Data/Matrices")


## Score differences
scores_NA_Inapp <- lapply(scores, get.score.div, difference = "Inapp/NA")
scores_NA_NS <- lapply(scores, get.score.div, difference = "NS/NA")
scores_NS_Inapp <- lapply(scores, get.score.div, difference = "NS/Inapp")

## Plot the differences densities
cols <- palette()[2:4]

ylim <- c(min(c(unlist(scores_NA_Inapp), unlist(scores_NA_NS), unlist(scores_NS_Inapp))), max(c(unlist(scores_NA_Inapp), unlist(scores_NA_NS), unlist(scores_NS_Inapp))))

boxplot(scores_NA_Inapp, las = 2, ylab = "relative extra steps", main = "Step differences between the three algorithms", col = cols[1], outline = FALSE, border = cols[1], ylim = ylim)
boxplot(scores_NA_NS, xlab = "", ylab = "", main = "", col = cols[2], outline = FALSE, border = cols[2], add = TRUE, xaxt = "n", yaxt = "n", ylim = ylim)
boxplot(scores_NS_Inapp, xlab = "", ylab = "", main = "", col = cols[3], outline = FALSE, border = cols[3], add = TRUE, xaxt = "n", yaxt = "n", ylim = ylim)
legend("topleft", legend = c("inapplicable/missing", "new state/missing", "inapplicable/new state"), col = cols, pch = 15, cex = 1)



## Score differences
scores_NA_Inapp <- lapply(scores, get.score.div, difference = "NA/Inapp")
scores_NA_NS <- lapply(scores, get.score.div, difference = "NA/NS")
scores_NS_Inapp <- lapply(scores, get.score.div, difference = "NS/Inapp")

## Plot the differences densities
cols <- palette()[2:4]

ylim <- c(min(c(unlist(scores_NA_Inapp), unlist(scores_NA_NS), unlist(scores_NS_Inapp))), max(c(unlist(scores_NA_Inapp), unlist(scores_NA_NS), unlist(scores_NS_Inapp))))

boxplot(scores_NA_Inapp, las = 2, ylab = "relative extra steps", main = "Step differences between the three algorithms", col = cols[1], outline = FALSE, border = cols[1], ylim = ylim)
boxplot(scores_NA_NS, xlab = "", ylab = "", main = "", col = cols[2], outline = FALSE, border = cols[2], add = TRUE, xaxt = "n", yaxt = "n", ylim = ylim)
boxplot(scores_NS_Inapp, xlab = "", ylab = "", main = "", col = cols[3], outline = FALSE, border = cols[3], add = TRUE, xaxt = "n", yaxt = "n", ylim = ylim)
legend("topleft", legend = c("missing/inapplicable", "missing/new state", "new state/inapplicable"), col = cols, pch = 15, cex = 1)









scores_NA_Inapp <- mapply(get.score.div.na, scores, nas_prop, difference = "Inapp/NA", SIMPLIFY = TRUE)
scores_NA_NS <- mapply(get.score.div.na, scores, nas_prop, difference = "NS/NA", SIMPLIFY = TRUE)
scores_NS_Inapp <- mapply(get.score.div.na, scores, nas_prop, difference = "NS/Inapp", SIMPLIFY = TRUE)

## Plot the differences densities
cols <- palette()[2:4]

ylim <- c(min(c(unlist(scores_NA_Inapp), unlist(scores_NA_NS), unlist(scores_NS_Inapp))), max(c(unlist(scores_NA_Inapp), unlist(scores_NA_NS), unlist(scores_NS_Inapp))))

boxplot(scores_NA_Inapp, las = 2, ylab = "relative extra steps * proportion of NAs", main = "Step differences between the three algorithms", col = cols[1], outline = FALSE, border = cols[1], ylim = ylim)
boxplot(scores_NA_NS, xlab = "", ylab = "", main = "", col = cols[2], outline = FALSE, border = cols[2], add = TRUE, xaxt = "n", yaxt = "n", ylim = ylim)
boxplot(scores_NS_Inapp, xlab = "", ylab = "", main = "", col = cols[3], outline = FALSE, border = cols[3], add = TRUE, xaxt = "n", yaxt = "n", ylim = ylim)
legend("topleft", legend = c("inapplicable/missing", "new state/missing", "inapplicable/new state"), col = cols, pch = 15, cex = 1)



## Score differences
scores_NA_Inapp <- mapply(get.score.div.na, scores, nas_prop, difference = "NA/Inapp", SIMPLIFY = TRUE)
scores_NA_NS <- mapply(get.score.div.na, scores, nas_prop, difference = "NA/NS", SIMPLIFY = TRUE)
scores_NS_Inapp <- mapply(get.score.div.na, scores, nas_prop, difference = "NS/Inapp", SIMPLIFY = TRUE)

## Plot the differences densities
cols <- palette()[2:4]

ylim <- c(min(c(unlist(scores_NA_Inapp), unlist(scores_NA_NS), unlist(scores_NS_Inapp))), max(c(unlist(scores_NA_Inapp), unlist(scores_NA_NS), unlist(scores_NS_Inapp))))

boxplot(scores_NA_Inapp, las = 2, ylab = "relative extra steps * proportion of NAs", main = "Step differences between the three algorithms", col = cols[1], outline = FALSE, border = cols[1], ylim = ylim)
boxplot(scores_NA_NS, xlab = "", ylab = "", main = "", col = cols[2], outline = FALSE, border = cols[2], add = TRUE, xaxt = "n", yaxt = "n", ylim = ylim)
boxplot(scores_NS_Inapp, xlab = "", ylab = "", main = "", col = cols[3], outline = FALSE, border = cols[3], add = TRUE, xaxt = "n", yaxt = "n", ylim = ylim)
legend("topleft", legend = c("missing/inapplicable", "missing/new state", "new state/inapplicable"), col = cols, pch = 15, cex = 1)





