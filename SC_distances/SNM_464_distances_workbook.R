library(OneR)
library(ggplot2)
library(plyr)
library("dplyr")
library("geosphere")

setwd("/Users/mas2535/Projects/GPS_Gambia/SNM/New_clonbality_paper/subset464/")
dist <- read.csv("SNM_464_snps_DISTS_metadata.csv", header = TRUE)
dist100$bin <- bin(dist100$Dist,nbins = 10, labels = c(10,20,30,40,50,60,70,80,90,100))
dist100<- subset(dist,dist$Dist<101)
dist150<- subset(dist,dist$Dist<151)


dist100$bin <- bin(dist100$Dist,nbins = 10, labels = c(10,20,30,40,50,60,70,80,90,100))
table(dist100$bin,dist100$Same_village)

dist100$bin5 <- bin(dist100$Dist,nbins = 20, labels = c(5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100))
dist100$distance <- Village_distance_matrix[dist100$Village1,dist100$Village2] 
bins5 <- table(dist100$bin5,dist100$Same_village)

filter(dist100, Dist < 5 & Same_subject == "TRUE")

dist0 <- subset(dist,dist$Dist<1)
table(dist0$Same_village)

#Calculate_Eucladian_distance
# distm (c(lon1, lat1), c(lon2, lat2), fun = distHaversine)
#dist100$euc_dist<- distm(c(dist100$X1,dist100$Y1), c(dist100$X2,dist100$Y2), fun = distHaversine)
source('~/Projects/GPS_Gambia/SNM/New_clonbality_paper/subset475/geo_distance.R')

write.csv(dist100, "dist100.csv")

get_geo_distance(dist100$X1, dist100$Y1, dist100$X2, dist100$Y2, units = "km")

source("~/Projects/R_functions/Geo_dist_matrix_function.R")

