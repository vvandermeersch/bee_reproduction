#######################################
### Prepare solitary bees (SB) data ###
#######################################

#Version Victor

#Setup----



#Data reading ----

source("scripts/prepare_parcels.R")

Sampling_SB <- read.table(file = "data/raw_data/PassageAbeilles_DD.txt",
                          header = TRUE, sep = "\t", dec = "." , encoding = "UTF-8")

#Data preparation ----

names(Sampling_SB)[1] <- 'ID_SGBD_parcelle'

Sampling_SB$SR <- rowSums(Sampling_SB[,10:16]>0) #species richness
