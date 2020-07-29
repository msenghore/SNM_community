setwd("/Users/mas2535/Projects/MRCG/GPS_Gambia/SNM/New_clonbality_paper/Final_analysis/Sharing_index/")

library(dplyr)
library(tidyverse)
library(ggplot2)
library(aod)
library(vegan)

# for the R5 subset load two files
#1) The metadata file with data on whether strain is shared and sharing index
#2) Comparisons in R5 subset with 50 or less SNPs
R5_meta <- read.csv("Metadata_R5_subset.csv")
R5_dist <- read.csv("50_SNPS_R5subset.csv")

#Load the SNM full metadata to be able to extract travel data and count of barcodes
Full_meta <- read.csv("../Metadata/SNM_full_final_metadata.csv")

boxplot(Index~villagecode, data = R5_meta)

#In the R5_meta, add a column for how many times child travelled
for (i in 1:nrow(R5_meta)){
  R5_meta$travel_count[i] <- Full_meta %>% filter(Barcode == R5_meta$Barcode[i] & travelled == 1 ) %>% nrow()
}

#In R5_meta, add a column to show if strains are sustained or not
R5_meta$Sustained <- 0
for (j in 1:nrow(R5_meta)){
  if(R5_meta$Barcode_count[j] > 2){
    R5_meta$Sustained[j] <- 1
  }
  }
#Plot travel frequency with sharing index
plot(travel_count~Index, data = R5_meta)
R5_meta %>% ggplot(aes(x = Index)) + geom_density()
R5_meta %>% ggplot(aes(x = travel_count)) + geom_density()

Full_meta %>% select(Barcode == "MM74_14_ST915" & travelled == 1 ) %>% nrow()

#Add a column for vaccine type
R5_meta$PCV7 <- 0
PCVs <- c("4","14","18C","19F","23F","6B","9V")
for (j in 1:nrow(R5_meta)){
  if(any(PCVs == R5_meta$insilicoserotype[j])){
    R5_meta$PCV7[j] <- 1
  }
}

#Assign strains to groups based on village
for (j in 1:nrow(R5_meta)){
  if(R5_meta$villagecode[j] %in% c("DD","EE","FF","GG","HH","II")){
    R5_meta$Group[j] <- 1
  }
  if(R5_meta$villagecode[j] %in% c("JJ","LL","MM","OO","PP","RR","SS")){
    R5_meta$Group[j] <- 2
  }
  if(R5_meta$villagecode[j] %in% c("B","C","D","E","M","N","P","T")){
    R5_meta$Group[j] <- 3
  }
  
}


#Perform generalized linear model for regression with outcone shared

R5logit <- glm(Shared ~ villagecode, data = R5_meta, family = "binomial")
summary(R5logit)
wald.test(b = coef(R5logit), Sigma = vcov(R5logit), Terms = 1:21)
confint(R5logit)

R5logit2 <- glm(Shared ~ GPSCCluster, data = R5_meta, family = "binomial")
summary(R5logit2)

R5logitGPSC <- glm(Shared ~ GPSCCluster + PCV7, data = R5_meta, family = "binomial")
summary(R5logitGPSC)

R5logitI <- glm(Shared ~ Index, data = R5_meta, family = "binomial")
summary(R5logitI)

R5logitPCV <- glm(Shared ~ PCV7, data = R5_meta, family = "binomial")
summary(R5logitPCV)

R5logitV <- glm(Shared ~ villagecode + PCV7, data = R5_meta, family = "binomial")
summary(R5logitV)
wald.test(b = coef(R5logitV), Sigma = vcov(R5logitV), Terms = 1:21)

R5logitVPI <- glm(Shared ~ villagecode + PCV7 + Sustained, data = R5_meta, family = "binomial")
summary(R5logitVPI)

R5logitGPI <- glm(Shared ~ as.factor(Group) + PCV7 + Sustained, data = R5_meta, family = "binomial")
summary(R5logitGPI)

R5logit_final <- glm(Shared ~ villagecode + PCV7 + Barcode_count, data = R5_meta, family = "binomial")
summary(R5logit_final)
confint(R5logit_final)
wald.test(b = coef(R5logit_final), Sigma = vcov(R5logit_final), Terms = 1:21)


#Create a variable sero_ST the for each village calculate number of strains and the Shannon diversity
R5_meta$sero_ST <- paste(R5_meta$insilicoserotype,R5_meta$insilicost,sep="_") 

Village_strains <- table(R5_meta$villagecode,R5_meta$sero_ST)
Shan <- diversity(Village_strains, index = "shannon")
head(Shan)

sst <- R5_meta %>% group_by(villagecode, sero_ST) %>% summarize(count=n())
table(sst$villagecode)


Vill_indices <- read.csv("Village_diversity_indices.csv")

ggplot(Vill_indices, aes(x=Strains, y=Shannon)) + 
  geom_point(size = Vill_indices$Subjects, aes(fill = Village, color = Villaage)) + 
  geom_smooth()
