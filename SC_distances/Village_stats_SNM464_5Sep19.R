library(ggplot2)
library(plyr)
library("dplyr")
library(vegan)

setwd("/Users/mas2535/Projects/GPS_Gambia/SNM/New_clonbality_paper/subset464/")
data <- read.csv("SNM_464_metadata_Rbook.csv", header = TRUE)
g <- ggplot(data, aes(data$villagecode)) + geom_bar()
g
length(unique(data$subjectid))

#Number of children in each village
d <- data %>% group_by(data$villagecode, data$subjectid) %>% summarize(count=n())
View(d)
dp <- ggplot(d, aes(d$`data$villagecode`)) + geom_bar()
dp

#Number of serotypes in each village
s <- data %>% group_by(data$villagecode, data$InSilicoSerotype) %>% summarize(count=n())
View(s)
ds <- ggplot(s, aes(s$`data$villagecode`)) + geom_bar()
ds

sst <- data %>% group_by(data$villagecode, data$Sero_ST) %>% summarize(count=n())
View(sst)
dsst <- ggplot(s, aes(s$`data$villagecode`)) + geom_bar()
dsst

subjectst <- data %>% group_by(data$subjectid, data$Sero_ST) %>% summarize(count=n())
sbust <-

#Create new dataframw with Village descriptions for number of subjects, serotypes and strains.
bene <- table(s$`data$villagecode`) %>% data.frame()
names(bene) <- c("Village", "serotype_count")

Pat <- table(d$`data$villagecode`) %>% data.frame()
names(Pat) <- c("Village", "Subjects")
head(Pat)

SeroST <- table(sst$`data$villagecode`) %>% data.frame()
names(SeroST) <- c("Village", "Strains")
head(SeroST)
#MErge all descriptors into one dataframe "stats"
Village_stats <- merge(bene, Pat, by="Village")
Village_stats <- merge(Village_stats, SeroST, by="Village")
head(Village_stats)


# Diversity calculation
#Tabulate data by strain counts (serotype_ST) in each village
table(data$villagecode,data$Sero_ST)
Village_strains <- table(data$villagecode,data$Sero_ST)
#Calculate indices from this summary table
Simp <- diversity(Village_strains, index = "simpson")
Village_simp <- {View(Simp)}
head(Simp)
Shan <- diversity(Village_strains, index = "shannon")
View(Shan)
#Add indices to the Village stats file
Village_stats$Shannon=Shan
Village_stats$simpson = Simp

ggplot(Village_stats, aes(x=Village_stats$Subjects, y=Village_stats$Strains)) + geom_point(size=Village_stats$Shannon) + geom_text(aes(label=Village_stats$Village),hjust=0, vjust=0)
ggplot(Village_stats, aes(x=Village_stats$Subjects, y=Village_stats$Strains)) + geom_point(size=Village_stats$Shannon) + geom_text(aes(label=Village_stats$Village),hjust=0, vjust=0) + xlab("Number Of Subjects") + ylab("Number of Strains") + ggtitle("Comparing numner of strains with strain diversity in Villages")

ggplot(Village_stats, aes(x=Village_stats$Subjects, y=Village_stats$simpson)) + geom_point() + geom_smooth() + geom_text(aes(label=Village_stats$Village),hjust=0, vjust=0) + xlab("Number Of Subjects") + ylab("Simpson Diversity") + ggtitle("Comparing number of strains with strain diversity across Villages")
ggplot(Village_stats, aes(x=Village_stats$Subjects, y=Village_stats$Shannon)) + geom_point() + geom_smooth() + geom_text(aes(label=Village_stats$Village),hjust=0, vjust=0) + xlab("Number Of Subjects") + ylab("Shannon Diversity") + ggtitle("Comparing number of strains with strain diversity across Villages")


