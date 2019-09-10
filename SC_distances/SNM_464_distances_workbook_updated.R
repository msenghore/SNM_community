library(OneR)
library(ggplot2)
library(plyr)
library("dplyr")
library("geosphere")
source('~/Projects/GPS_Gambia/SNM/New_clonbality_paper/subset475/geo_distance.R')


setwd("/Users/mas2535/Projects/GPS_Gambia/SNM/New_clonbality_paper/subset464/")
dist <- read.csv("SNM_464_dist_metadata.csv", header = TRUE)


#load SNM villages distance matrix
Vmatrix <- read.csv("~/Projects/GPS_Gambia/SNM/Metadata/SNM_Village_matrix.csv")

#subset the comparisons under 100 snps difference
d1<- subset(dist,dist$Dist<101)

#calculate village distances
d1 <- mutate(d1, Vdistm = get_geo_distance(d1$X1, d1$Y1, d1$X2, d1$Y2, units = "km"))
is.na(dist$X1)

#Create bins for snp distance and physical distance
d1 <- mutate(d1, snpbin = bin(d1$Dist,nbins = 20, labels = c(5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100)) )
d1 <- mutate(d1, Distbin = bin(d1$Vdistm,nbins = 8, labels = c(5,10,15,20,25,30,35,40)) )


# Export table of snp bins and distance bins as csv for Croucher plot
croucher_table <- table(d1$snpbin,d1$Distbin)
  head(croucher_table)
write.csv(croucher_table, "SNM464_croucher_table.csv")


