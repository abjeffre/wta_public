############################################################################
################### BUILD BASE DATA ########################################

library("gridBase")
library("rethinking")
library(matrixStats)


# setwd("/home/jeffrey_andrews")
# setwd("C:/Users/jeffr/OneDrive/Documents/") 
# setwd("C:/Users/jeffrey_andrews/OneDrive/Documents/") 
source("WTA/paths.R")
source(path$load)

###SOURCING####
source("functions/plotting.R")
source("functions/data_manipulation.R")
source("functions/utility.R")
source("functions/graphics.R")
source("functions/measurement.R")
forest<- read.csv("WTA/data/AmyDataSummary.csv")
# source("functions/load.R")

protest_old = FALSE
run = FALSE
# build WTA data
wta <- prefernces$wta_final
wta <- log10(exp(wta)/2500)
wta[wta < 0] <- 0
# identify protest bids
if(protest_old == F){
  protest <- exp(prefernces$wta_final)/2500 >= 4000
  protest = ifelse(prefernces$priced_out==1 & is.na(prefernces$wta_final), 1, protest)
}else{
  protest <- prefernces$wta_final > log1p(df_income$total_inc)
  protest = ifelse(prefernces$priced_out==1, 1, protest)
}



# Construct Social pressure data
SN = log10(ifelse(as.numeric(prefernces$corr_wta_neighbor) > 0, as.numeric(prefernces$corr_wta_neighbor), prefernces$wta_neighbor)/2500)
# Construct Environmental Attitudes
EA=as.integer(conservation$defor_prob_shehia+conservation$defor_prob_pemba)
# Construct Risk 
RL = prefernces$risk_losses
RG = prefernces$risk_gains
# Construct Forest Income
FI = standardize(log10(df_income$forestinc/2500+1))
# Get Error
U=log10((exp(prefernces$wta_final+prefernces$wta_sds)-exp(prefernces$wta_final))/2500+1) +0.0001
# Get controls 
shehia<-c("tondooni","msuka magharibi","tumbe magharibi","shumba mjini","kifundi","mgogoni","gando","mjini wingwi",
          "mtambwe kaskazini","mtambwe kusini","fundo","mgelema", "kisiwa panza", "kangani", "changaweni", "kambini", "chumbageni",
          "shungi", "ziwani", "piki", "ukunjwi", "mjimbini", "michenzani", "kilindi")


controls<-c("shungi", "ziwani", "piki","ukunjwi", "chumbageni", "kilindi")
contbin<-ifelse(shehia %in% controls, 1, 0)


df <- data.frame(wta = wta)
df$EA = EA
df$FI = FI
df$RL = RL+1
df$RG = RG+1
df$SN = SN
df$P = ifelse(protest==TRUE, 2, 1)
df$M = demographics$controls+1
df$U = U
df$G = as.numeric(demographics$pres_sex)+1
df$A = standardize(as.numeric(demographics$pres_age))
df$ED = demographics$pres_edu
# df$K = conservation$anajuaredd+1
wealth$total_wealth[wealth$total_wealth ==0] <- NA
# df$SS = standardize(as.numeric(shocks$community_help))
df$W = standardize(log10(wealth$total_wealth/2500+1))
df$WF = standardize(log10(wealth$total_wealth/2500+1))*standardize(log10(df_income$forestinc/2500+1))
df$E = demographics$enum_id
df$S = demographics$ward_id
# df$O = conservation$defor_neighbors
# df$KR = conservation$anajuaredd+1

# DEFORESTATION RATES


db <- as.data.frame(matrix(NA, nrow = 0, ncol = 3))

for(i in unique(forest$Shehia)){
  tot = mean((forest$Forest + forest$NonForest)[forest$Shehia==i])
  loss20022011=(forest$Forest[forest$Shehia==i & forest$Year==2002]-forest$Forest[forest$Shehia==i & forest$Year==2012])/forest$Forest[forest$Shehia==i & forest$Year==2002]/10
  loss20122017=(forest$Forest[forest$Shehia==i & forest$Year==2012]-forest$Forest[forest$Shehia==i & forest$Year==2017])/forest$Forest[forest$Shehia==i & forest$Year==2012]/5
  diff = loss20022011-loss20122017
  d2012 = forest$Forest[forest$Shehia==i & forest$Year==2012]
  db=rbind(db, c(i, diff, d2012))
  
}

dr <- rep(0, 829)
df2012 <- rep(0, 829)
for(i in 1:nrow(demographics)){
  ind=which(tolower(db[,1])== demographics$ward[i])
  
  dr[i] = ifelse(length(ind) >0, db[ind,2], NA)
  df2012[i] = ifelse(length(ind) >0, db[ind,3], NA)
}


df$DR <- standardize(as.numeric(dr))
df$FS <-standardize(as.numeric(df2012))
