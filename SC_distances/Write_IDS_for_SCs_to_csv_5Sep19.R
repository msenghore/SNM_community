library(ggplot2)
library(plyr)
library("dplyr")
library("tidyverse")

setwd("/Users/mas2535/Projects/GPS_Gambia/SNM/New_clonbality_paper/subset464/")

#Load in the results from BAPS
BAPS <- read.csv("SNM464_BAPS_partisions.csv")
head(BAPS)
BAPS <- select(BAPS, Isolate, level.1, level.2)

#Rename columns and remove BAPS SC1 which is the bin
colnames(BAPS) <- c("ID","BAPS1","BAPS2")
Mono_BAPS <- filter(BAPS, BAPS2 != 1)
head(Mono_BAPS)

#Create a loop to write all the IDs linked to each SC into a variable and then export csv
#Write SCs to list
BAPS_SC_list <- unique(Mono_BAPS$BAPS2)
#Create loop to parse the list and extract values
for (i in 1:(length(BAPS_SC_list))){
  SC <- BAPS_SC_list[i]
  handle <- paste("SC",SC,".list", sep = "")
  #cat(SC, handle, "\n")
  tmp <- filter(Mono_BAPS, BAPS2 == SC) %>% select(ID)
  write.table(tmp,handle,col.names=FALSE, row.names = FALSE,quote = FALSE)
}


#Use PoPUNK to resolve bin 1
BAPS_bin <- filter(BAPS, BAPS2 == 1)

#Load in The GPSC clusters and merge with BAPS_bin
S475 <- read.csv("../Metadata/SNM_474_metadata_GPSC.csv")
head(S475)
GPSC <- select(S475, LaneId, GPSC)
head(GPSC)
colnames(GPSC) <- c("ID","GPSC")

SCs <- merge(BAPS_bin, GPSC, by="ID")
head(SCs)
GPSC_counts <- table(SCs$GPSC) %>% as.data.frame()
colnames(GPSC_counts) <- c("GPSC","Counts")
head(GPSC_counts)

#Filter out GPSCs that has fewer than three isolates
GPSC_counts_filter <- filter(GPSC_counts, Counts > 2)
head(GPSC_counts_filter)
GPSC_list <- as.vector(select(GPSC_counts_filter, GPSC))
list <- GPSC_list$GPSC
#Loop through list and print IDs
for (j in 1:length(list)){
  GSC <- list[j]
  fhandle <- paste("GPSC",GSC,".list", sep = "")
  #cat(GSC, fhandle, "\n")
  tmp2 <- filter(SCs, GPSC == GSC) %>% select(ID)
  write.table(tmp2,fhandle,col.names=FALSE, row.names = FALSE, quote = FALSE)
}




#Old code, may be useful later.

# Find isolates that was missed in merge
#missing <- which(!(BAPSID %in% GPSCID))
#BAPS$ID[missing]
#SCs <- rbind(SCs, "16399_5#1" = c(1,9,))

#BAPSID <- BAPS$ID
#GPSCID <- GPSC$ID

#which(!(BAPSID %in% GPSCID))
#any(BAPSID)ta
