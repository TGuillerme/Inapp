## Script for counting tree length using Morphy's counting

## Get the chain name
chain=$1

## Run PAWM
./PAWM Counting/${chain}.NAmissing.nex > Scores/${chain}.morphy.scores

## Modify the file header
sed '2,4d' Scores/${chain}.morphy.count > Scores/tmp
sed 's/Hello, World!/Tree   Length/g' Scores/tmp > Scores/${chain}.morphy.count

## Get the number of trees
length=$(wc -l Scores/${chain}.morphy.count | sed 's/Scores\/'"${chain}"'.morphy.count//g' | sed 's/[[:space:]]//g')
let "length -= 1"

## Renaming the trees
for i in $(seq 1 ${length})
do 
j=${i}
let "j += 1"
    sed ''"${i}"'s/The length: /'"${i}"'    /' Scores/${chain}.morphy.count > tmp
    mv tmp  Scores/${chain}.morphy.count
    printf "."
done
