#!/usr/bin/R
args = commandArgs(trailingOnly=TRUE)

if (length(args)!=3) {
  stop("Usage: R melt_matrix.R <input file> <output_file> <Working directory>", call.=FALSE)
}

library(reshape2)
library("tidyverse")
setwd(args[3])
#m <- read.csv(args[1], header = TRUE)
#melt <- data.frame(col = rep(colnames(m), each = nrow(m)), row = rep(rownames(m), ncol(m)), value = as.vector(m))
#melt <- melt(m)

m <- read.csv(args[1], row.names = 1, header = TRUE, check.names = FALSE) %>% as.matrix() %>% melt()
#m
write.csv(m, args[2], row.names = FALSE, quote = FALSE)
