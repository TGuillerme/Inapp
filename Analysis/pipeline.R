sourceDir <- function(path, ...) {
    for (name_file in list.files(path, pattern = "[.][RrSsQq]$")) {
        source(file.path(path, name_file), ...)
    }
}

sourceDir("/Functions/")


## Read the tree scores
chain <- "Giles2015"
path <- "/Data/Scores/"
scores <- read.tree.score(chain, path)

## Get the score data
scores_std <- standardise.score(scores, "base")

## Plot the normal score differences distributions
boxplot(scores)

plot(density(scores$NAextr), xlim = c(scores$NAmiss[1], max(scores$NAextr)), col = "blue", xlab = "Number of steps", main = "Effect of different NA treatment")
abline(v = scores$NAmiss[1], col = "orange")
legend("center", legend = c("NA = ?", "NA = 9"), col = c("orange", "blue"), lty = 1)