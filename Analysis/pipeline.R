source("Functions/read.tree.score.R")
source("Functions/standardise.score.R")
source("Functions/plot.scores.R")

library(Inapp)

## Read the tree scores
path <- "Data/Scores/"
chains <- list.files(path, pattern = ".morphy")
chains <- unlist(strsplit(chains, split = ".morphy.count"))

## Get the scores
scores <- sapply(chains, read.tree.score, path, simplify = FALSE)

## Standardise the scores
scores_std <- lapply(scores, standardise.score, "base")

## Plot the normal score differences for a single distributions
plot.scores(scores_std[[3]])

## Plot all the densities
plot.scores.list(scores, relative.density = FALSE)
