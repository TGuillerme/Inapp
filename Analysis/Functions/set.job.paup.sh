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

## ~~~~~~~~~~~~~~~~~~
## GETTING THE ISLAND
## ~~~~~~~~~~~~~~~~~~
echo "BEGIN PAUP;" >> ${CHAIN}.infer.nex 

## Add the PAUP options
if [ "$PAUP_OPTIONS" != "" ]
then
    cat $PAUP_OPTIONS >> ${CHAIN}.infer.nex
fi

echo "set maxtrees=500 increase=auto autoInc=500;"  >> ${CHAIN}.infer.nex
echo "set outroot=monophyl;" >> ${CHAIN}.infer.nex


## Add the PAUP search options
if [ "$SEARCH_OPTIONS" != "" ]
then
    cat $SEARCH_OPTIONS >> ${CHAIN}.infer.nex
else
    echo "hsearch addseq=random nreps=100 rearrlimit=5000000 limitperrep=yes;" >> ${CHAIN}.infer.nex
fi

## Save the tree and close
echo "savetrees /file=${CHAIN}.tre replace;"  >> ${CHAIN}.infer.nex
echo "q;"  >> ${CHAIN}.infer.nex
echo "\nEND;" >> ${CHAIN}.infer.nex

## Add the trees inference to the paupjob
echo "## Infer the trees" > ${CHAIN}.paupjob.sh
echo "paup ${CHAIN}.infer.nex" >> ${CHAIN}.paupjob.sh
echo "" >> ${CHAIN}.paupjob.sh

#end;