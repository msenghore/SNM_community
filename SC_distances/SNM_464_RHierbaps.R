library(rhierbaps)
library(ggtree)
library(phytools)
library(ape)

set.seed(1234)

setwd("~/Projects/GPS_Gambia/SNM/New_clonbality_paper/subset464/")

#load data from multiple sequence alignment into a snp matrix
#SNM_464_snps.fasta <- system.file("extdata", "seqs.fa", package = "rhierbaps")
snp.matrix <- load_fasta("SNM_464_snps.fasta")

#Run HierBAPS with three levels and initially 100 clusters
hb2.results <- hierBAPS(snp.matrix, max.depth = 2, n.pops = 100, quiet = TRUE)
head(hb.results$partition.df)
write.csv(hb.results$partition.df, "SNM464_BAPS_partitions")
