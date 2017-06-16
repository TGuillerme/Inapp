source("Functions/read.tree.score.R")
source("Functions/standardise.score.R")
source("Functions/plot.scores.R")

library(Inapp)
library(Claddis)
library(MASS)

## Read the tree scores
path <- "Data/Scores/"
chains <- list.files(path, pattern = ".morphy.inapplicable.count")
chains <- unlist(strsplit(chains, split = ".morphy.inapplicable.count"))

## Get the scores
scores <- sapply(chains, read.tree.score, path, simplify = FALSE)

## Get the matrices
matrices <- lapply(chains, read.matrices, path = "Data/Matrices")

## Standardise the scores
scores_std <- mapply(standardise.score, scores, matrices, std = "MP", SIMPLIFY = FALSE)


## Plot the normal score differences for a single distributions
plot.scores(scores_std[[10]])

## Plot all the densities
plot.scores.list(scores_std, relative.density = TRUE)

## Get the proportion of nas
# nas_prop <- lapply(as.list(names(scores)), get.inapp.proportion, path = "Data/Matrices")

proportions <- lapply(scores, get.proportion)
inapp_proportions <- unlist(lapply(proportions, `[[`, 1))
extra_proportions <- unlist(lapply(proportions, `[[`, 3))
proportions_combined <- data.frame(inapp_proportions, extra_proportions)
colnames(proportions_combined) <- c("Inapplicable", "New state")


boxplot(proportions_combined, ylab = "Proportion of rejected (longer) trees")


max_scores <- lapply(scores_std, lapply, max)
min_scores <- lapply(scores_std, lapply, min)
inapp_max <- unlist(lapply(max_scores, `[[`, 1))
extra_max <- unlist(lapply(max_scores, `[[`, 3))
inapp_min <- unlist(lapply(min_scores, `[[`, 1))
extra_min <- unlist(lapply(min_scores, `[[`, 3))


proportions_combined <- data.frame(inapp_proportions, inapp_min, inapp_max, extra_proportions, extra_min, extra_max)

sauronplot(proportions_combined, names = c("Inapplicable", "New state"), scale.bar = 0.5, plot.range = 0.9)
