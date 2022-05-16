## Moving the trees into the appropriate nexus files

for f in *.tre
do
    chain=$(basename $f .tre)
    cp ../Matrices/${chain}.nex ../Counting/${chain}.nex.tre
    sed '1d' ${chain}.tre > tmp
    cat tmp >> ../Counting/${chain}.nex.tre
    rm tmp
done
