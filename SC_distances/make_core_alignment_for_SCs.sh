for i in *.list
do
IFS='.' read -r -a f <<< "$i"
name=${f[0]}
#echo I wanna open file $i and create dir $name
mkdir $name
pf annotation -t file -i $i -f gff -l $name
bsub -n6 -R"span[hosts=1]" -M24000 -R "select[mem>24000] rusage[mem=24000]" -o o.%J -e e.%J roary -e --mafft -p 6 -f $name $name/*.gff
done
