library(OneR)
library(ggplot2)
library(plyr)
library("dplyr")
library("geosphere")
source('~/Projects/MRCG/GPS_Gambia/SNM/New_clonbality_paper/subset475/geo_distance.R')
library("vcd")
library("lubridate")
library(cowplot)
library(rlang)
library(reshape2)
library(ggsignif)


setwd("/Users/mas2535/Projects/MRCG/GPS_Gambia/SNM/New_clonbality_paper/Final_analysis/Metadata/")
long_dist <- read.csv("Full_dataset_distances_metadata.csv")

#Remove comparisons between the same strain from same patient.
long_dist <- long_dist %>% filter(Barcode_match == "FALSE")
within_host <- read.csv("Full_dataset_distances_metadata.csv") %>% filter(Barcode_match == "TRUE")
within_host %>% filter(V1 %in% c("JJ","HH","RR","D","E","II")) %>% ggplot(aes(y = Dist, x= V1, fill = V1)) + geom_boxplot() + ylim(0,250)
ld500 %>% filter(V1==V2) %>% filter(V1 %in% c("JJ","HH","RR","D","E","II")) %>% ggplot(aes(y = Dist, x= V1, fill = V1)) + geom_boxplot()

#Subset comparisons that are 500 SNPS or less for croucher plot
ld500 <- long_dist %>% filter(Dist <= 500)

ld50 <- long_dist %>% filter(Dist < 51)
write.csv(ld50,"Distances_0_50_SNPs_multiple_strain_per_child.csv")



#Calculate the physical distances between Villages
ld500 <- mutate(ld500, Vdistm = get_geo_distance(ld500$X1, ld500$Y1, ld500$X2, ld500$Y2, units = "km"))
for (i in 1:(nrow(ld500))){
  if (ld500$Vdistm[i] <= 5) {ld500$trange[i] <- "1.Low"} 
  else if(5 < ld500$Vdistm[i] & ld500$Vdistm[i] <= 15){ld500$trange[i] <- "2.Med"}
  else{ld500$trange[i] <- "3.High"}
}


#Convert the dates to decimal and calculate time between collection
ld500$dDate1 <- decimal_date(dmy(ld500$Date1))
ld500$dDate2 <- decimal_date(dmy(ld500$Date2))
ld500 <- mutate(ld500, TDiff = abs(dDate1 - dDate2)*365)

#Create bins for SNP distances and plot box plot of with x-axis as SNP distances
for(i in 1: (nrow(ld500))){
  if (ld500$Dist[i] == 0) {ld500$snpbin[i] <- 0} 
  else if(0 < ld500$Dist[i] & ld500$Dist[i] <= 5){ld500$snpbin[i] <- 5}
  else if(5 < ld500$Dist[i] & ld500$Dist[i] <= 10){ld500$snpbin[i] <- 10}
  else if(10 < ld500$Dist[i] & ld500$Dist[i] <= 15){ld500$snpbin[i] <- 15}
  else if(15 < ld500$Dist[i] & ld500$Dist[i] <= 20){ld500$snpbin[i] <- 20}
  else if(20 < ld500$Dist[i] & ld500$Dist[i] <= 25){ld500$snpbin[i] <- 25}
  else if(25 < ld500$Dist[i] & ld500$Dist[i] <= 30){ld500$snpbin[i] <- 30}
  else if(30 < ld500$Dist[i] & ld500$Dist[i] <= 35){ld500$snpbin[i] <- 35}
  else if(35 < ld500$Dist[i] & ld500$Dist[i] <= 40){ld500$snpbin[i] <- 40}
  else if(40 < ld500$Dist[i] & ld500$Dist[i] <= 45){ld500$snpbin[i] <- 45}
  else if(45 < ld500$Dist[i] & ld500$Dist[i] <= 50){ld500$snpbin[i] <- 50}
  else if(50 < ld500$Dist[i] & ld500$Dist[i] <= 55){ld500$snpbin[i] <- 55}
  else if(55 < ld500$Dist[i] & ld500$Dist[i] <= 60){ld500$snpbin[i] <- 60}
  else if(60 < ld500$Dist[i] & ld500$Dist[i] <= 65){ld500$snpbin[i] <- 65}
  else if(65 < ld500$Dist[i] & ld500$Dist[i] <= 70){ld500$snpbin[i] <- 70}
  else if(70 < ld500$Dist[i] & ld500$Dist[i] <= 75){ld500$snpbin[i] <- 75}
  else if(75 < ld500$Dist[i] & ld500$Dist[i] <= 80){ld500$snpbin[i] <- 80}
  else if(80 < ld500$Dist[i] & ld500$Dist[i] <= 85){ld500$snpbin[i] <- 85}
  else if(85 < ld500$Dist[i] & ld500$Dist[i] <= 90){ld500$snpbin[i] <- 90}
  else if(90 < ld500$Dist[i] & ld500$Dist[i] <= 95){ld500$snpbin[i] <- 95}
  else if(95 < ld500$Dist[i] & ld500$Dist[i] <= 100){ld500$snpbin[i] <- 100}
  else if(100 < ld500$Dist[i] & ld500$Dist[i] <= 110){ld500$snpbin[i] <- 110}
  else if(110 < ld500$Dist[i] & ld500$Dist[i] <= 120){ld500$snpbin[i] <- 120}
  else if(120 < ld500$Dist[i] & ld500$Dist[i] <= 130){ld500$snpbin[i] <- 130}
  else if(130 < ld500$Dist[i] & ld500$Dist[i] <= 140){ld500$snpbin[i] <- 140}
  else if(140 < ld500$Dist[i] & ld500$Dist[i] <= 150){ld500$snpbin[i] <- 150}
  else if(150 < ld500$Dist[i] & ld500$Dist[i] <= 160){ld500$snpbin[i] <- 160}
  else if(160 < ld500$Dist[i] & ld500$Dist[i] <= 170){ld500$snpbin[i] <- 170}
  else if(170 < ld500$Dist[i] & ld500$Dist[i] <= 180){ld500$snpbin[i] <- 180}
  else if(180 < ld500$Dist[i] & ld500$Dist[i] <= 190){ld500$snpbin[i] <- 190}
  else if(190 < ld500$Dist[i] & ld500$Dist[i] <= 200){ld500$snpbin[i] <- 200}
  else if(200 < ld500$Dist[i] & ld500$Dist[i] <= 210){ld500$snpbin[i] <- 210}
  else if(210 < ld500$Dist[i] & ld500$Dist[i] <= 220){ld500$snpbin[i] <- 220}
  else if(220 < ld500$Dist[i] & ld500$Dist[i] <= 230){ld500$snpbin[i] <- 230}
  else if(230 < ld500$Dist[i] & ld500$Dist[i] <= 240){ld500$snpbin[i] <- 240}
  else if(240 < ld500$Dist[i] & ld500$Dist[i] <= 250){ld500$snpbin[i] <- 250}
  else if(250 < ld500$Dist[i] & ld500$Dist[i] <= 300){ld500$snpbin[i] <- 300}
  else if(300 < ld500$Dist[i] & ld500$Dist[i] <= 350){ld500$snpbin[i] <- 350}
  else if(350 < ld500$Dist[i] & ld500$Dist[i] <= 400){ld500$snpbin[i] <- 400}
  else if(400 < ld500$Dist[i] & ld500$Dist[i] <= 450){ld500$snpbin[i] <- 450}
  else if(450 < ld500$Dist[i] & ld500$Dist[i] <= 500){ld500$snpbin[i] <- 500}
}


#Plot the Croucher plot

#tabulate SNP bins by distance bins
tmp <- table(ld500$snpbin, ld500$trange)
write.csv(tmp,"test_trange_500.csv")
#Export table as csv and import to get it in a dataframe
tmp <- read.csv("test_trange_500.csv")
colnames(tmp) <- c("SNPs", "Low", "Med", "High")
#Tabulate the total number of observations in each snp bin as total and merge with counts
Snp_df500 <- as.data.frame(table(ld500$snpbin))
colnames(Snp_df500) <- c("SNPs","Total")
c500 <- merge(tmp, Snp_df500, by = "SNPs")

#Calculate the cumulative frequencies for the Croucher plot
for (j in 1:(nrow(c500))){
  i <- j-1
  c500$HighC[j] <- sum(c500$High[1:j])
  c500$LowC[j] <- sum(c500$Low[1:j])
  c500$MedC[j] <- sum(c500$Med[1:j])
  c500$TotalC[j] <- sum(c500$Total[1:j])
}

#Calculate the proportion based on cumulative frequencies
c500$LowP <- c500$LowC/c500$TotalC
c500$MedP <- c500$MedC/c500$TotalC
c500$HighP <- c500$HighC/c500$TotalC

#Normalize the proportions define total for (N ki/N i)/(N k/N)
Nlow <- sum(c500$Low)
NMed <- sum(c500$Med)
NHigh <- sum(c500$High)
Ntot <- sum(c500$Total)

c500$LowPN <- c500$LowP * Ntot / Nlow
c500$MedPN <- c500$MedP * Ntot / NMed
c500$HighPN <- c500$HighP * Ntot / NHigh



 PP1 <- c500 %>% select(SNPs,LowP, MedP,HighP) %>% rename("5 km"=LowP, "5-15 km"=MedP , "15+ km"=HighP) %>% melt(id.vars = "SNPs") %>% rename(Distance=variable) %>% 
   ggplot(aes(color=Distance)) + xlab('SNP Threshold')+ ylab('Proportion of pairs') +
   geom_smooth(aes(SNPs,value, fill = Distance, alpha =0.5), se = FALSE) + 
   scale_color_manual(values = c("#1b9e77","#d95f02","#7570b3")) +
   theme_bw(base_size = 12, base_family = "Arial") + theme_half_open()
 PP2 <- c500 %>% select(SNPs,LowPN, MedPN,HighPN) %>% rename("5 km"=LowPN, "5-15 km"=MedPN , "15+ km"=HighPN) %>% melt(id.vars = "SNPs") %>% rename(Distance=variable) %>% 
   ggplot(aes(color=Distance)) + xlab('SNP Threshold')+ ylab('Log normalized proportion') +
   geom_smooth(aes(SNPs,value, fill = Distance), se = FALSE) + 
   scale_color_manual(values = c("#1b9e77","#d95f02","#7570b3")) +
   theme_bw(base_size = 12, base_family = "Arial") + theme_half_open()


#Plot SNP density
 SDP <- ld500 %>% select(Dist,trange) %>% 
  ggplot(aes(x = Dist,fill = trange)) + 
   xlab('SNP Distance')+ ylab('Density') +
   geom_density(bw = 10, alpha = 0.5) + 
   scale_fill_manual(values = c("#1b9e77","#d95f02","#7570b3"), name="Distance",labels =c("0-5 km","5-15 km","15+ km"))+
   theme_bw(base_size = 12, base_family = "Arial") + facet_grid(~trange) + theme_half_open() + 
   theme(strip.background = element_blank(), strip.text.x = element_blank()) +
   scale_x_continuous(breaks=c(250,500))
 SDPR <- ld500R5 %>% select(Dist,trange) %>% 
   ggplot(aes(x = Dist,fill = trange)) + 
   xlab('SNPs distance')+ ylab('Density') +
   geom_density(bw = 10, alpha = 0.5) + 
   scale_fill_manual(values = c("#1b9e77","#d95f02","#7570b3"), name="Distance",labels =c("0-5 km","5-15 km","15+ km"))+
   theme_bw(base_size = 12, base_family = "Arial") + facet_grid(~trange) + theme_half_open() + 
   theme(strip.background = element_blank(), strip.text.x = element_blank())

 bxp <- ld500 %>% ggplot(aes(y = Dist, x= trange, fill = trange)) + geom_boxplot(alpha = 0.5) + xlab('')+ ylab('SNP Distance')+
   scale_fill_manual(values = c("#1b9e77","#d95f02","#7570b3")) +
   theme_bw(base_size = 12, base_family = "Arial") + theme_half_open() + ylim(0,600) + theme(legend.position="none") + theme(strip.background = element_blank(), strip.text.x = element_blank(), axis.text.x = element_blank())
 
 bxp <- bxp + geom_signif(comparisons=list(c("1.Low", "3.High")), annotations="****",y_position = 590, tip_length = 0, vjust=0.4) + 
   geom_signif(comparisons=list(c("1.Low", "2.Med")), annotations="****",y_position = 550, tip_length = 0, vjust=0.4) 
   
   #Create a panel figure using CowPlot
lg <- plot_grid(PP1 + theme(legend.position="none"),PP2 + theme(legend.position="none"), labels = c("A","B") )
legend <- get_legend(SDP + theme(legend.box.margin = margin(0, 0, 0, 12)))
SDRG <- plot_grid(SDP + theme(legend.position="none") + axisTicks(),SDPR + theme(legend.position="none"), labels = c("B","C"), ncol = 1, rel_heights = c(1,1))
densp <- plot_grid(bxp, SDP, ncol = 2, rel_widths = c(1,3), labels = c("B","C"))
plot_grid( bxp, legend,rel_widths = c(1, .6))
merged_plot <- plot_grid(PP2 ,densp, ncol = 1, rel_heights = c(1.2,0.7), labels = c("A",""))

save_plot("Croucher_plot.pdf", merged_plot, ncol = 3)
merged_plot
dev.off()

#lg2 <- plot_grid(lg,SDP + theme(legend.position="none"), labels = c("A","C"), ncol = 1, rel_heights = c(2,1))
#lg2 <- plot_grid(SDP,SDPR, labels = c("C","D"), ncol = 1, rel_heights = c(3,1))
#lg2

#load the randomized list of laneIDs
R1 <- read.csv("Random_laneIDs1", header = FALSE)
R2 <- read.csv("Random_laneIDs2", header = FALSE)
R3 <- read.csv("Random_laneIDs3", header = FALSE)
R4 <- read.csv("Random_laneIDs4", header = FALSE)
R5 <- read.csv("Random_laneIDs5", header = FALSE)

#Convert the list of IDs to a vector
R1 <- R1$V1
R2 <- R2$V1
R3 <- R3$V1
R4 <- R4$V1
R5 <- R5$V1

#Subet the distance table for random IDs only
ld500R1 <- ld500 %>% filter(ID1 %in%  R1 & ID2 %in% R1)
ld500R2 <- ld500 %>% filter(ID1 %in%  R2 & ID2 %in% R2)
ld500R3 <- ld500 %>% filter(ID1 %in%  R3 & ID2 %in% R3)
ld500R4 <- ld500 %>% filter(ID1 %in%  R4 & ID2 %in% R4)
ld500R5 <- ld500 %>% filter(ID1 %in%  R5 & ID2 %in% R5)

#Plot Croucher Plot using the function below
r1p <- Plot_Croucher(ld500R1)
r2p <- Plot_Croucher(ld500R2)
r3p <- Plot_Croucher(ld500R3)
r4p <- Plot_Croucher(ld500R4)
r5p <- Plot_Croucher(ld500R5)

#Take randomized set 5 and establish how many times each barcode is present
Full_data <- read.csv("SNM_full_final_metadata.csv")
Full_dataset_barcodes <- Full_data$Barcode
d50 <- filter(ld500R5, Dist <= 50 )
write.csv(d50,"Distances_0_50_SNPs_single_strain_per_child.csv")
#Count occurence for each Barcode
for (i in 1:nrow(d50)){d50$Count1[i] <- length(grep(d50$Barcode1[i],Full_dataset_barcodes))}
for (i in 1:nrow(d50)){d50$Count2[i] <- length(grep(d50$Barcode2[i],Full_dataset_barcodes))}

#Check pairs with more than one strain in each
d50 %>% filter(Count1 > 2 | Count2 > 2) %>% filter(! Count1 > 2 & Count2 > 2) %>% select(trange) %>% table() %>% sort()

## transient vs sustained transmission
#1) Both sustained
sustained <- d50 %>% filter(Count1 > 2 & Count2 > 2)

#Take a pair and calculate the mean pairwise distance in sustained pairs
#Create a loop 
for (i in 1:nrow(sustained)){
  tmpvec <- ""
  tmpvec <- ld500 %>% filter(ST1 == sustained$ST1[i] & Sero1 == sustained$Sero1[i] & S1 %in% c(sustained$S1[i],sustained$S2[i]) & S2 %in% c(sustained$S1[i],sustained$S2[i])) %>% select(Dist)
  sustained$meanpwsnp[i] <- mean(tmpvec$Dist,trim=0,na.rm = FALSE)
}

for (i in 1:nrow(sustained)){
  tmpvec2 <- ""
  tmpvec2 <- ld500 %>% filter(ST1 == sustained$ST1[i] & Sero1 == sustained$Sero1[i] & S1 %in% c(sustained$S1[i],sustained$S2[i]) & S2 %in% c(sustained$S1[i],sustained$S2[i])) %>% select(TDiff)
  sustained$meantdiff[i] <- mean(tmpvec2$TDiff,trim=0,na.rm = FALSE)
}

#2) Mixed: 1 transient and 1 sustained
mixed <- d50 %>% filter(Count1 <= 2 | Count2 <= 2) %>% filter(!(Count1 > 2 & Count2 > 2)) %>% filter(!(Count1 <= 2 & Count2 <= 2))
for (i in 1:nrow(mixed)){
  tmpvec <- ""
  tmpvec <- ld500 %>% filter(ST1 == mixed$ST1[i] & Sero1 == mixed$Sero1[i] & S1 %in% c(mixed$S1[i],mixed$S2[i]) & S2 %in% c(mixed$S1[i],mixed$S2[i])) %>% select(Dist)
  mixed$meanpwsnp[i] <- mean(tmpvec$Dist,trim=0,na.rm = FALSE)
}

#3) Transient: both transient
transient <- d50 %>% filter(Count1 <= 2 & Count2 <= 2)


### In the d50 dataframe, class each comparison as either transient, mixed or sustained.
for (i in 1:nrow(d50)){
  if (d50$Count1[i] > 2 & d50$Count2[i] > 2) {
    d50$Category[i] <- "Sustained"
  } else if (d50$Count1[i] <= 2 & d50$Count2[i] <= 2) {
    d50$Category[i] <- "transient"
  }else{ 
    d50$Category[i] <- "Mixed"
    }
    
}

#Calculate the mean pairwise SNP difference
for (i in 1:nrow(d50)){
  tmpvec2 <- ""
  tmpvec2 <- ld500 %>% filter(ST1 == d50$ST1[i] & Sero1 == d50$Sero1[i] & S1 %in% c(d50$S1[i],d50$S2[i]) & S2 %in% c(d50$S1[i],d50$S2[i])) %>% select(Dist)
  d50$meansnpdiff[i] <- mean(tmpvec2$Dist,trim=0,na.rm = FALSE)
}


# Create a function for running the Croucher plot.
Plot_Croucher <- function(a){
  tmp <- table(a$snpbin, a$trange)
  write.csv(tmp,"test_trange_500.csv")
  #Export table as csv and import to get it in a dataframe
  tmp <- read.csv("test_trange_500.csv")
  colnames(tmp) <- c("SNPs", "Low", "Med", "High")
  #Tabulate the total number of observations in each snp bin as total and merge with counts
  Snp_df500 <- as.data.frame(table(a$snpbin))
  colnames(Snp_df500) <- c("SNPs","Total")
  cum500 <- merge(tmp, Snp_df500, by = "SNPs")
  
  #Calculate the cumulative frequencies for the Croucher plot
  for (j in 1:(nrow(cum500))){
    i <- j-1
    cum500$HighC[j] <- sum(cum500$High[1:j])
    cum500$LowC[j] <- sum(cum500$Low[1:j])
    cum500$MedC[j] <- sum(cum500$Med[1:j])
    cum500$TotalC[j] <- sum(cum500$Total[1:j])
  }
  
  #Calculate the proportion based on cumulative frequencies
  cum500$LowP <- cum500$LowC/cum500$TotalC
  cum500$MedP <- cum500$MedC/cum500$TotalC
  cum500$HighP <- cum500$HighC/cum500$TotalC
  
  #Normalize the proportions define total for (N ki/N i)/(N k/N)
  Nlow <- sum(cum500$Low)
  NMed <- sum(cum500$Med)
  NHigh <- sum(cum500$High)
  Ntot <- sum(cum500$Total)
  
  cum500$LowPN <- cum500$LowP * Ntot / Nlow
  cum500$MedPN <- cum500$MedP * Ntot / NMed
  cum500$HighPN <- cum500$HighP * Ntot / NHigh
  
  
p1 <-  cum500 %>% select(SNPs,LowP, MedP,HighP) %>% rename("5 km"=LowP, "5-15 km"=MedP , "15+ km"=HighP) %>% melt(id.vars = "SNPs") %>% rename(Distance=variable) %>% 
    ggplot(aes(color=Distance)) + xlab('SNP Threshold')+ ylab('Proportion') +
    ggtitle("Distribution of pairs within a SNP threshold by village distance") + 
    geom_smooth(aes(SNPs,value, fill = Distance), se = FALSE) + 
    scale_color_manual(values = c("#1b9e77","#d95f02","#7570b3")) +
    theme_bw(base_size = 12, base_family = "Arial")
p2 <- cum500 %>% select(SNPs,LowPN, MedPN,HighPN) %>% rename("5 km"=LowPN, "5-15 km"=MedPN , "15+ km"=HighPN) %>% melt(id.vars = "SNPs") %>% rename(Distance=variable) %>% 
    ggplot(aes(color=Distance)) + xlab('SNP Threshold')+ ylab('Log normalized proportion') +
    ggtitle("Distribution of pairs within a SNP threshold by village distance") + 
    geom_smooth(aes(SNPs,log(value), fill = Distance), se = FALSE) + 
    scale_color_manual(values = c("#1b9e77","#d95f02","#7570b3")) +
    theme_bw(base_size = 12, base_family = "Arial")

plot_grid(p1, p2, labels = c('A', 'B'))
  
}

