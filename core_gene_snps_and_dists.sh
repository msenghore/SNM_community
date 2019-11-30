for i in *_*/
do
IFS='_' read -r -a f <<< "$i"
name=${f[0]}
#echo name is $name
cd $i
snp-sites -c -o $name.snps.fasta core_gene_alignment.aln
snp-dists -b -c -a $name.snps.fasta > ../dists/$name.dist.csv
cd ../
done
