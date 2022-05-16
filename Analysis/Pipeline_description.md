# Pipeline for testing the effect of inapplicable data

## 1 - Get the most parsimonious tree island in PAUP
Search for the most parsimonious tree island **treating NAs as missing data** and save all the topologies from the island and the score.

```
execute $MATRIX.nex
hsearch addseq=random nreps=1000
savetrees /file=$MATRIX.tre
```

Then combine both matrix and trees into $MATRIX.tre

## 2 - Calculate tree length of all the topologies in the island in PAUP and morphy

* First calculate the tree length **treating NAs as an extra state** in PAUP

```
execute $MATRIX.nex
Pscores /scorefile=$MATRIX.NAmiss.score
Pset GapMode=newstate
Pscores /scorefile=$MATRIX.NAextr.score
```

* Then calculate the tree length using the morphy algorithm