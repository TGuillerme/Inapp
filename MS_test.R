library(shiny); library(devtools)
library(phangorn)
runGitHub("Inapp", "TGuillerme")
install_github("TGuillerme/Inapp")
library(Inapp)
library(inapplicable)
m_length <- function (mat) mat$regions + length(mat$changes)
Rprof()
for (i in 10:30) {
  tree <- rtree(i, br=NULL)
  char <- paste0(sample(c('0', '1', '-', '-'), length(tree$tip.label), replace=TRUE), collapse='')
  scores <- unique(vapply(tree$tip.label, function (root) m_length(apply.reconstruction(root.robust(tree, root), char, method='NA')), double(1)))
  if (length(scores) != 1) stop(scores)
  cat('.')
}
Rprof(NULL)
summaryRprof()


for (i in 10:130) {
  tree <- rtree(i, br=NULL)
  char <- paste0(sample(c('0', '1', '-', '-'), length(tree$tip.label), replace=TRUE), collapse='')
  scores <- unique(vapply(tree$tip.label, function (root) m_length(apply.reconstruction(root.robust(tree, root), char, method='NA')), double(1)))
  if (length(scores) != 1) stop(scores)
  cat('.')
}


for (i in 10:130) {
  tree <- rtree(i, br=NULL)
  char <- paste0(sample(c('0', '1', '-', '-', '-'), length(tree$tip.label), replace=TRUE), collapse='')
  scores <- unique(vapply(tree$tip.label, function (root) m_length(apply.reconstruction(root.robust(tree, root), char, method='NA')), double(1)))
  if (length(scores) != 1) stop(scores)
  cat('.')
}


source('R/plot.algorithm.R')
tree <- read.tree(text='(((a,b),(c,d)), ((e, f), (g, h)));')
character  <- "1--12--2"
character  <- "1--125-2"
applicable <- "10011101"
par(mfrow=c(2,2), mar=rep(1.5, 4))




plot.states.matrix(tree, applicable, passes = as.vector(passes.to.plot), method = "Inapplicable", inapplicable = NULL, show.labels=1)
plot.inapplicable.algorithm(tree, character, passes = as.vector(passes.to.plot), method = "Inapplicable", inapplicable = NULL, show.tip.label = TRUE)
cat("expected length: 0\n")

tree_root_a <- root(tree, 'a', resolve.root=TRUE)
tree_root_h <- root(tree, 'h', resolve.root=TRUE)
plot.inapplicable.algorithm(tree_root_a, character, passes = as.vector(passes.to.plot), method = "Inapplicable", inapplicable = NULL, show.tip.label = TRUE)
plot.inapplicable.algorithm(tree_root_h, character, passes = as.vector(passes.to.plot), method = "Inapplicable", inapplicable = NULL, show.tip.label = TRUE)
          
# tree_root_h <- read.tree(text='(h, (g, ((f, e), ((c, d), (a, b)))));')
plot(tree)
plot.inapplicable.algorithm(tree_root_h, character, passes = as.vector(passes.to.plot), method = "Inapplicable", inapplicable = NULL, show.tip.label = TRUE)
## Isn't it interesting that a node that should unambiguously be "2" on pass 4 is instead reconstructed as "125"?

tree_root_h_problem <- read.tree(text='(h, (g, ((i, (f, e)), ((c, d), (a, b)))));')
character_plus_ina <- '2--521--1'
character_plus_amb <- '2-?521--1'
character_plus_one <- '2-1521--1'
character_plus_two <- '2-2521--1'
par(mfrow=c(2,2), mar=rep(1.5, 4))
plot.inapplicable.algorithm(tree_root_h_problem, character_plus_ina, passes = as.vector(passes.to.plot), method = "Inapplicable", inapplicable = NULL, show.tip.label = TRUE)
plot.inapplicable.algorithm(tree_root_h_problem, character_plus_amb, passes = as.vector(passes.to.plot), method = "Inapplicable", inapplicable = NULL, show.tip.label = TRUE)
plot.inapplicable.algorithm(tree_root_h_problem, character_plus_one, passes = as.vector(passes.to.plot), method = "Inapplicable", inapplicable = NULL, show.tip.label = TRUE)
plot.inapplicable.algorithm(tree_root_h_problem, character_plus_two, passes = as.vector(passes.to.plot), method = "Inapplicable", inapplicable = NULL, show.tip.label = TRUE)
#  Intriguing that, even given no additional information, the node is resolved.


tree <- read.tree(text='(((a,b),(c,d)), ((e, f), (g, h)));')
character  <- "1--125-2"
par(mfrow=c(1,2), mar=rep(1.5, 4))
tree_root_a <- root(tree, 'a', resolve.root=TRUE) #<- read.tree(text='(a, (b, ((c, d), ((e, f), (g,h)))));')

plot.inapplicable.algorithm(tree_root_a, character, passes = c(1,2,3,4), method = "Inapplicable", inapplicable = NULL, show.tip.label = TRUE)
plot(tree_root_a, main='MRS suggestion'); nodelabels(c(1, 1, 1, 2, 2, 2, 1), col=c('black', 'red', rep('black', 4), 'red')); 
tiplabels(c('1', '-', '-', '1', 2, 5, '-', 2))

tree_root_h <- root(tree, 'h', resolve.root=TRUE)
plot.inapplicable.algorithm(tree_root_h, character, passes = as.vector(passes.to.plot), method = "Inapplicable", inapplicable = NULL, show.tip.label = TRUE)
plot(tree_root_h, main='MRS suggestion'); nodelabels(c(2, 1, 1, 1, 2, 2, 2), col=c('black', rep('red', 5), 'red')); 
tiplabels(c('1', '-', '-', '1', 2, 5, '-', 2))
          