## Solving polytomies

## Read the CHAIN
CHAIN=$1

## Preparing the R script
echo "" > solve.poly.${CHAIN}.R
echo "library(ape)" >> solve.poly.${CHAIN}.R
echo "trees <- read.nexus(\"Trees/${CHAIN}.tre\")" >> solve.poly.${CHAIN}.R
echo "trees <- multi2di(trees)" >> solve.poly.${CHAIN}.R
echo "write.nexus(trees, file = \"Trees/${CHAIN}.solve.tre\")" >> solve.poly.${CHAIN}.R
echo "quit()" >> solve.poly.${CHAIN}.R

## Run the R script
R < solve.poly.${CHAIN}.R --no-save

## Clean up the script
rm solve.poly.${CHAIN}.R

## Combine the new trees to the nexus file
cp Matrices/${CHAIN}.nex Counting/${CHAIN}.solve.nex 
sed '1d' Trees/${CHAIN}.solve.tre >> Counting/${CHAIN}.solve.nex

## Remove all the tmp files
#mv Trees/${CHAIN}.solve.tre Trees/${CHAIN}.tre
