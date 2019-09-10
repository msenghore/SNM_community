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
max.col(Vmatrix)
#subset the comparisons under 100 snps difference
d1<- subset(dist,dist$Dist<101)

#calculate village distances
d1 <- mutate(d1, Vdistm = get_geo_distance(d1$X1, d1$Y1, d1$X2, d1$Y2, units = "km"))
is.na(dist$X1)
#Create distance bins

for (i in 1:(nrow(d1))){
  if (d1$Vdistm[i] == 0) {d1$binkm[i] <- 0} 
  else if(0 < d1$Vdistm[i] & d1$Vdistm[i] <= 5){d1$binkm[i] <- 5}
  else if(5 < d1$Vdistm[i] & d1$Vdistm[i] <= 10){d1$binkm[i] <- 10}
  else if(10 < d1$Vdistm[i] & d1$Vdistm[i] <= 15){d1$binkm[i] <- 15}
  else if(15 < d1$Vdistm[i] & d1$Vdistm[i] <= 20){d1$binkm[i] <- 20}
  else if(20 < d1$Vdistm[i] & d1$Vdistm[i] <= 25){d1$binkm[i] <- 25}
  else if(25 < d1$Vdistm[i] & d1$Vdistm[i] <= 30){d1$binkm[i] <- 30}
  else if(30 < d1$Vdistm[i] & d1$Vdistm[i] <= 35){d1$binkm[i] <- 35}
  else if(35 < d1$Vdistm[i] & d1$Vdistm[i] <= 40){d1$binkm[i] <- 40}
  else if(40 < d1$Vdistm[i] & d1$Vdistm[i] <= 45){d1$binkm[i] <- 45}
}

for(i in 1: (nrow(d1))){
  if (d1$Dist[i] == 0) {d1$binsnp[i] <- 0} 
  else if(0 < d1$Dist[i] & d1$Dist[i] <= 5){d1$binsnp[i] <- 5}
  else if(5 < d1$Dist[i] & d1$Dist[i] <= 10){d1$binsnp[i] <- 10}
  else if(10 < d1$Dist[i] & d1$Dist[i] <= 15){d1$binsnp[i] <- 15}
  else if(15 < d1$Dist[i] & d1$Dist[i] <= 20){d1$binsnp[i] <- 20}
  else if(20 < d1$Dist[i] & d1$Dist[i] <= 25){d1$binsnp[i] <- 25}
  else if(25 < d1$Dist[i] & d1$Dist[i] <= 30){d1$binsnp[i] <- 30}
  else if(30 < d1$Dist[i] & d1$Dist[i] <= 35){d1$binsnp[i] <- 35}
  else if(35 < d1$Dist[i] & d1$Dist[i] <= 40){d1$binsnp[i] <- 40}
  else if(40 < d1$Dist[i] & d1$Dist[i] <= 45){d1$binsnp[i] <- 45}
  else if(45 < d1$Dist[i] & d1$Dist[i] <= 50){d1$binsnp[i] <- 50}
  else if(50 < d1$Dist[i] & d1$Dist[i] <= 55){d1$binsnp[i] <- 55}
  else if(55 < d1$Dist[i] & d1$Dist[i] <= 60){d1$binsnp[i] <- 60}
  else if(60 < d1$Dist[i] & d1$Dist[i] <= 65){d1$binsnp[i] <- 65}
  else if(65 < d1$Dist[i] & d1$Dist[i] <= 70){d1$binsnp[i] <- 70}
  else if(70 < d1$Dist[i] & d1$Dist[i] <= 75){d1$binsnp[i] <- 75}
  else if(75 < d1$Dist[i] & d1$Dist[i] <= 80){d1$binsnp[i] <- 80}
  else if(80 < d1$Dist[i] & d1$Dist[i] <= 85){d1$binsnp[i] <- 85}
  else if(85 < d1$Dist[i] & d1$Dist[i] <= 90){d1$binsnp[i] <- 90}
  else if(90 < d1$Dist[i] & d1$Dist[i] <= 95){d1$binsnp[i] <- 95}
  else if(95 < d1$Dist[i] & d1$Dist[i] <= 100){d1$binsnp[i] <- 100}
}


cr_raw <- table(d1$binsnp,d1$binkm) 
View(cr_raw)
write.csv(cr_raw, "SNM464_updated_croucher_table.csv")

# Create SNP bins and distance bins

table(d0i$Dist)


##### OLD COMMANDS #####

#Create bins for snp distance and physical distance
#d1_out <- filter(d1, Vill_same == "FALSE")
#d1 <- mutate(d1, snpbin = bin(d1$Dist,nbins = 20, labels = c(5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100)) )
#d1_out <- mutate(d1_out, snpbin = bin(d1_out$Dist,nbins = 20, labels = c(5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100)) )
#d1 <- mutate(d1, Distbin = bin(d1$Vdistm,nbins = 9, labels = c(5,10,15,20,25,30,35,40,45)) )


# Export table of snp bins and distance bins as csv for Croucher plot
croucher_table <- table(d1$snpbin,d1$Distbin)
  head(croucher_table)
write.csv(croucher_table, "SNM464_croucher_table.csv")


