## Script for counting tree length using Morphy's counting

# for f in Counting/*.nex.tre ; do chain=$(echo $f | sed 's/.nex.tre//g' | sed 's/Counting\///g') ; sh count.morphy.sh ${chain} ; done

## Get the chain name
chain=$1

## Run PAWM
./PAWM Counting/${chain}.nex.tre missing > Scores/${chain}.morphy.missing.count
./PAWM Counting/${chain}.nex.tre newstate > Scores/${chain}.morphy.newstate.count
./PAWM Counting/${chain}.nex.tre inapplicable > Scores/${chain}.morphy.inapplicable.count

## Modify the file header
tail -1 Scores/${chain}.morphy.missing.count 
sed '2,6d' Scores/${chain}.morphy.missing.count | sed '$d' > Scores/tmp
sed 's/Hello, World!/Tree   Length/g' Scores/tmp > Scores/${chain}.morphy.missing.count

tail -1 Scores/${chain}.morphy.newstate.count 
sed '2,6d' Scores/${chain}.morphy.newstate.count | sed '$d' > Scores/tmp
sed 's/Hello, World!/Tree   Length/g' Scores/tmp > Scores/${chain}.morphy.newstate.count

tail -1 Scores/${chain}.morphy.inapplicable.count 
sed '2,6d' Scores/${chain}.morphy.inapplicable.count | sed '$d' > Scores/tmp
sed 's/Hello, World!/Tree   Length/g' Scores/tmp > Scores/${chain}.morphy.inapplicable.count
