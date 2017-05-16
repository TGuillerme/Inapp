##########################
# Script for setting up the paup command file for counting the tree lengths of the matrix
##########################
#SYNTAX: sh set.job.paup.sh -c <chain> -o <paup_options> -s <search_options>
#with:
#-c <chain> the name of the chain
#-o <paup_options> OPTIONAL path to a text file containing paup options (e.g. ordered characters, etc...)
#-s <search_options> OPTIONAL path to a text file containing the paup search options (if missing, is set to hsearch addseq=random nreps=1000)
##########################
#guillert(at)tcd.ie - 2017/05/16
##########################

## ~~~~~~
## INPUT
## ~~~~~~

## Input values
while [[ $# -gt 1 ]]
do
key="$1"

case $key in
    -c|--chain)
        CHAIN="$2"
        shift
        ;;
    -o|--paup_options)
        PAUP_OPTIONS="$2"
        shift
        ;;
    -s|--search_options)
        SEARCH_OPTIONS="$2"
        ;;
        *)
        ;;
esac
    shift
done

## Check chain name
if [ "$CHAIN" == "" ]
then
    echo "ERROR: --chain argument is missing!"
    exit
fi

cp ${CHAIN}.nex ${CHAIN}.infer.nex
cp ${CHAIN}.nex ${CHAIN}.NAmissing.nex
cp ${CHAIN}.nex ${CHAIN}.NAextra.nex
cp ${CHAIN}.nex ${CHAIN}.NAmorphy.nex

## ~~~~~~~~~~~~~~~~~~
## GETTING THE ISLAND
## ~~~~~~~~~~~~~~~~~~
echo "BEGIN PAUP;" >> ${CHAIN}.infer.nex 

## Add the PAUP options
if [ "$PAUP_OPTIONS" != "" ]
then
    cat $PAUP_OPTIONS >> ${CHAIN}.infer.nex
fi

## Add the PAUP search options
if [ "$SEARCH_OPTIONS" != "" ]
then
    cat $SEARCH_OPTIONS >> ${CHAIN}.infer.nex
else
    echo "hsearch addseq=random nreps=1000;" >> ${CHAIN}.infer.nex
fi

## Save the tree and close
echo "set maxtrees=500 increase=auto autoInc=500;"  >> ${CHAIN}.infer.nex
echo "savetrees /file=${CHAIN}.tre replace;"  >> ${CHAIN}.infer.nex
echo "q;"  >> ${CHAIN}.infer.nex
echo "\nEND;" >> ${CHAIN}.infer.nex

## Add the trees inference to the paupjob
echo "## Infer the trees" > ${CHAIN}.paupjob.sh
echo "paup ${CHAIN}.infer.nex" >> ${CHAIN}.paupjob.sh
echo "" >> ${CHAIN}.paupjob.sh

## ~~~~~~~~~~~~~~~~~~
## COUNTING THE STEPS
## ~~~~~~~~~~~~~~~~~~

## Replacing "-" by "9" in the NAextra matrix
sed 's/-/9/g' ${CHAIN}.NAextra.nex > ${CHAIN}.NAextra.nex.tmp
## Correcting the format line
symbols=$(grep -i 'symbols' ${CHAIN}.NAextra.nex.tmp | sed 's/.*[Ss][Yy][Mm][Bb][Oo][Ll][Ss]="//g' | sed 's/".*//g') ## ERROR HERE! DOESN'T SELECT SYMBOLS PROPERLY
sed 's/[Gg][Aa][Pp]=9/gap=-/g' ${CHAIN}.NAextra.nex.tmp | sed 's/[Ss][Yy][Mn][Bb][Oo][Ll][Ss]="[0-9].*";/symbols="'"${symbols}"'9";/g' > ${CHAIN}.NAextra.nex
rm ${CHAIN}.NAextra.nex.tmp

## Adding the tree island to the file
echo "## Add the trees to the counting matrices" >> ${CHAIN}.paupjob.sh
echo "sed '1d' ${CHAIN}.tre >> ${CHAIN}.NAmissing.nex" >> ${CHAIN}.paupjob.sh
echo "sed '1d' ${CHAIN}.tre >> ${CHAIN}.NAextra.nex" >> ${CHAIN}.paupjob.sh

## Add the PAUP options
if [ "$PAUP_OPTIONS" != "" ]
then
    echo "## Add the step counting commands" >> ${CHAIN}.paupjob.sh
    echo "echo \"BEGIN PAUP;\" >> ${CHAIN}.NAmissing.nex" >> ${CHAIN}.paupjob.sh
    echo "echo \"BEGIN PAUP;\" >> ${CHAIN}.NAextra.nex" >> ${CHAIN}.paupjob.sh
    echo "cat $PAUP_OPTIONS >> ${CHAIN}.NAmissing.nex" >> ${CHAIN}.paupjob.sh
    echo "cat $PAUP_OPTIONS >> ${CHAIN}.NAextra.nex" >> ${CHAIN}.paupjob.sh
    echo "echo \"\nEND;\" >> ${CHAIN}.NAmissing.nex" >> ${CHAIN}.paupjob.sh
    echo "echo \"\nEND;\" >> ${CHAIN}.NAextra.nex" >> ${CHAIN}.paupjob.sh    
fi
echo "" >> ${CHAIN}.paupjob.sh


echo "## Generate the counting scripts" >> ${CHAIN}.paupjob.sh
echo "echo \"#NEXUS\" > ${CHAIN}.NAmissing.count.nex " >> ${CHAIN}.paupjob.sh
echo "echo \"BEGIN PAUP;\" > ${CHAIN}.NAmissing.count.nex " >> ${CHAIN}.paupjob.sh
echo "echo \"set autoclose=yes warntree=no warnreset=no;\" >> ${CHAIN}.NAmissing.count.nex" >> ${CHAIN}.paupjob.sh
echo "echo \"set maxtrees=500 increase=auto autoInc=500;\" >> ${CHAIN}.NAmissing.count.nex" >> ${CHAIN}.paupjob.sh
echo "echo \"execute ${CHAIN}.NAmissing.nex;\" >> ${CHAIN}.NAmissing.count.nex" >> ${CHAIN}.paupjob.sh
echo "echo \"Pscores /scorefile=${CHAIN}.NAmissing.score replace;\" >> ${CHAIN}.NAmissing.count.nex" >> ${CHAIN}.paupjob.sh
echo "echo \"q;\" >> ${CHAIN}.NAmissing.count.nex" >> ${CHAIN}.paupjob.sh
echo "echo \"end;\" >> ${CHAIN}.NAmissing.count.nex" >> ${CHAIN}.paupjob.sh

echo "echo \"#NEXUS\" > ${CHAIN}.NAextra.count.nex " >> ${CHAIN}.paupjob.sh
echo "echo \"BEGIN PAUP;\" > ${CHAIN}.NAextra.count.nex " >> ${CHAIN}.paupjob.sh
echo "echo \"set autoclose=yes warntree=no warnreset=no;\" >> ${CHAIN}.NAextra.count.nex" >> ${CHAIN}.paupjob.sh
echo "echo \"set maxtrees=500 increase=auto autoInc=500;\" >> ${CHAIN}.NAextra.count.nex" >> ${CHAIN}.paupjob.sh
echo "echo \"execute ${CHAIN}.NAextra.nex;\" >> ${CHAIN}.NAextra.count.nex" >> ${CHAIN}.paupjob.sh
echo "echo \"Pscores /scorefile=${CHAIN}.NAextra.score replace;\" >> ${CHAIN}.NAextra.count.nex" >> ${CHAIN}.paupjob.sh
echo "echo \"q;\" >> ${CHAIN}.NAextra.count.nex" >> ${CHAIN}.paupjob.sh
echo "echo \"end;\" >> ${CHAIN}.NAextra.count.nex" >> ${CHAIN}.paupjob.sh
echo "" >> ${CHAIN}.paupjob.sh


## Add the step counting to the paupjob
echo "## Count the steps on each trees" >> ${CHAIN}.paupjob.sh
echo "paup ${CHAIN}.NAmissing.count.nex" >> ${CHAIN}.paupjob.sh
echo "paup ${CHAIN}.NAextra.count.nex" >> ${CHAIN}.paupjob.sh

#end;