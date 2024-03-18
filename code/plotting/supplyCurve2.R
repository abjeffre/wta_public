#######################################################################
################## SET UP FILE DIRECTORY #############################


set_project_wd <- function(dir=""){
  user=Sys.info()[[6]]
  if(user=="jeffrey_andrews") setwd(paste0("C:/Users/jeffrey_andrews/OneDrive/Documents/", dir))
  else if(user=="Jeff") setwd(paste0("C:/Users/Jeff/OneDrive/Documents/", dir))
  else if(user == 'jeffr') setwd(paste0("C:/Users/jeffr/OneDrive/Documents/", dir))
}

set_project_wd()

######################################################################
########## FIRST IDENTIFY WHAT MODEL IS BEING USED  ##################
# use 1 for without wealth
# use 2 for with wealth

file_names_wta <- c("mocci.RDS", "wta_with_wealth.RDS")
file_names_protest <- c("mpb.RDS", "protest_with_wealth.RDS")

model_selected <- 2

source("WTA/code/script/getBaseData.R")

df<-df[complete.cases(df),]
#Calculate mean group intercepts for causal inference. 
df$p_mu <- NA

for(i in 1:2){
  ind = which(df$M==i)
  df$p_mu[ind]<-logit(mean(df$P[ind]-1))
}

data = as.list(df)
data$N = length(df$wta)
data$alpha = rep(2, max(data$EA)-1)

m.pb=readRDS(paste0("WTA/data/", file_names_protest[model_selected]))
post<-extract.samples(m.pb)


sup <- function(x, xseq){
  out <- rbinom(1000, 1, inv_logit(post$a + post$b_mu*mean(data$p_mu[data$M==x]) + rowMedians(post$bS)*post$sigma_S + rowMedians(post$bE)*post$sigma_E + post$bM[,x]*post$sigma_M + (post$bF[, x]* post$sigma_F + post$bF_mu)*xseq + (post$bSN[,x]* post$sigma_SN + post$bSN_mu)*median(data$SN) + (post$bEA[,x]*post$sigma_EA+post$bEA_mu)*rowSums(post$delta[,x,1:8])+ rowMeans(post$bRL[,x,]) + rowMeans(post$bRG[,x,])))
  
  #out<-(10^out)
  return(out)
}

xseq = seq(0, 4, length.out =2)
redd<-replicate(100, sapply(xseq, function(x) sup(1, x)))
cont<-replicate(100, sapply(xseq, function(x) sup(2, x)))

reddnprotestlow<-colSums(redd[,1,])
reddnprotesthigh<-colSums(redd[,2,])
contnprotestlow<-colSums(cont[,1,])
contnprotesthigh<-colSums(cont[,2,])


#######################################################
################### SUPPLY CURVE ######################


source("WTA/code/script/getBaseData.R")
df<-df[complete.cases(df),]
#Remove Protest Bids
df <- df[df$P==1,]
data = as.list(df)
data$N = length(df$wta)
data$alpha = rep(2, max(data$EA)-1)
#Calculate mean group intercepts for causal inference. 
df <- df[complete.cases(df),]
df$mu_mu <- NA


for(i in 1:2){
  ind = which(df$M==i)
  df$mu_mu[ind]<-mean(df$wta[ind][df$wta[ind]>0])
}
data$mu_mu = df$mu_mu


# Plot oppotunity costs
m.occi=readRDS(paste0("WTA/data/", file_names_wta[model_selected]))
post<- extract.samples(m.occi)

sup <- function(n, x, xseq){
  mu<-rnorm(n,                post$b_mu*mean(data$mu_mu[data$M==x]) + post$a[,1] + rowMedians(post$bS[,1,]*post$sigma_S[,1]) + rowMedians(post$bE[,1,]*post$sigma_E[,1]) + post$bM[,1,x]*post$sigma_M[,1] + (post$bF[, 1,x]* post$sigma_F[,1] + post$bF_mu[,1])*xseq + (post$bSN[, 1,x]* post$sigma_SN[,1] + post$bSN_mu[,1])*median(df$SN) +  (post$bEA[,1,x]*post$sigma_EA[,1] + post$bEA_mu[,1])*rowSums(post$delta[,1,x,1:8]) + rowMeans(post$bRG[,1,x,]) + rowMeans(post$bRL[,1,x,]), post$sigma)
  p <- rbinom(n, 1, inv_logit(post$a[,2] +                                         rowMedians(post$bS[,2,]*post$sigma_S[,2]) + rowMedians(post$bE[,2,]*post$sigma_E[,2]) + post$bM[,2,x]*post$sigma_M[,2] + (post$bF[, 2,x]* post$sigma_F[,2] + post$bF_mu[,2])*xseq + (post$bSN[, 2,x]* post$sigma_SN[,2] + post$bSN_mu[,2])*median(df$SN) +  (post$bEA[,2,x]*post$sigma_EA[,2] + post$bEA_mu[,2])*rowSums(post$delta[,2,x,1:8]) + rowMeans(post$bRG[,2,x,]) + rowMeans(post$bRL[,2,x,])))
  out<-mu*p
  #out<-(10^out)
  return(out)
}

x.seq = seq(0, 4, length.out=2)
i = 1


oc_suppreddlow = matrix(NA, nrow = 1000, ncol = 100)
oc_suppreddhigh = matrix(NA, nrow = 1000, ncol = 100)
oc_suppcontlow = matrix(NA, nrow = 1000, ncol = 100)
oc_suppconthigh = matrix(NA, nrow = 1000, ncol = 100)

for(i in 1:100){
    oc_suppreddlow[,i] <- sort(c(sup((1000-reddnprotestlow[i]), 1, x.seq[1]), rep(log10(4500), reddnprotestlow[i])))  
    oc_suppreddhigh[,i] <- sort(c(sup((1000-reddnprotesthigh[i]), 1, x.seq[2]), rep(log10(4500), reddnprotesthigh[i])))
    oc_suppcontlow[,i] <- sort(c(sup((1000-contnprotestlow[i]), 2, x.seq[1]), rep(log10(4500), contnprotestlow[i])))  
    oc_suppconthigh[,i] <- sort(c(sup((1000-contnprotesthigh[i]), 2, x.seq[2]), rep(log10(4500), contnprotesthigh[i])))
}

#USE THESE FOR CALLING A GRAPH. 

par(mfrow=c(2,1))

supCurve(rowMeans(10^(oc_suppreddlow)), ylim = c(0, 4000), middle = T)
for(i in 1:100){
  supCurve(10^oc_suppreddlow[,i], add = T, col = col.alpha("black", .1), ylim = c(0, 4000))
}

supCurve(rowMeans(10^oc_suppreddhigh), ylim = c(0, 4000), add = T, middle = T)
for(i in 2:100){
  supCurve(10^oc_suppreddhigh[,i], add = T, col = col.alpha("goldenrod", .1), ylim = c(0, 4000))
}




supCurve(rowMeans(10^(oc_suppcontlow)), ylim = c(0, 4000), middle = T)
for(i in 1:100){
  supCurve(10^oc_suppcontlow[,i], add = T, col = col.alpha("black", .1), ylim = c(0, 4000))
}

supCurve(rowMeans(10^oc_suppconthigh), ylim = c(0, 4000), add = T, middle = T)
for(i in 2:100){
  supCurve(10^oc_suppconthigh[,i], add = T, col = col.alpha("purple", .1), ylim = c(0, 4000))
}




##########################################################################################################
#################### SOCIAL PERCEPTION ###################################################################





source("WTA/code/script/getBaseData.R")

df<-df[complete.cases(df),]
#Calculate mean group intercepts for causal inference. 
df$p_mu <- NA

for(i in 1:2){
  ind = which(df$M==i)
  df$p_mu[ind]<-logit(mean(df$P[ind]-1))
}

data = as.list(df)
data$N = length(df$wta)
data$alpha = rep(2, max(data$EA)-1)

m.pb=readRDS(paste0("WTA/data/", file_names_protest[model_selected]))
post<-extract.samples(m.pb)



sup <- function(x, xseq){
  out <- rbinom(1000, 1, inv_logit(post$a +  post$b_mu*mean(data$p_mu[data$M==x]) + rowMedians(post$bS)*post$sigma_S + rowMedians(post$bE)*post$sigma_E + post$bM[,x]*post$sigma_M + (post$bF[, x]* post$sigma_F + post$bF_mu)*median(data$FI) + (post$bSN[,x]* post$sigma_SN + post$bSN_mu)*xseq + (post$bEA[,x]*post$sigma_EA+post$bEA_mu)*rowSums(post$delta[,x,1:8])+ rowMeans(post$bRL[,x,]) + rowMeans(post$bRG[,x,])))
  
  #out<-(10^out)
  return(out)
}


xseq = seq(2, 3, length.out =2)
redd<-replicate(100, sapply(xseq, function(x) sup(1, x)))
cont<-replicate(100, sapply(xseq, function(x) sup(2, x)))

reddnprotestlow<-colSums(redd[,1,])
reddnprotesthigh<-colSums(redd[,2,])
contnprotestlow<-colSums(cont[,1,])
contnprotesthigh<-colSums(cont[,2,])


#######################################################
################### SUPPLY CURVE ######################


source("WTA/code/script/getBaseData.R")
df<-df[complete.cases(df),]
#Remove Protest Bids
df <- df[df$P==1,]
data = as.list(df)
data$N = length(df$wta)
data$alpha = rep(2, max(data$EA)-1)
#Calculate mean group intercepts for causal inference. 
df <- df[complete.cases(df),]
df$mu_mu <- NA


for(i in 1:2){
  ind = which(df$M==i)
  df$mu_mu[ind]<-mean(df$wta[ind][df$wta[ind]>0])
}
data$mu_mu = df$mu_mu

m.occi=readRDS(paste0("WTA/data/", file_names_wta[model_selected]))
post<- extract.samples(m.occi)


# SOCIAL NORMS
sup <- function(n,x, xseq){
  mu<-rnorm(n,                post$b_mu*mean(data$mu_mu[data$M==x]) + post$a[,1] +     rowMedians(post$bS[,1,]*post$sigma_S[,1]) + rowMedians(post$bE[,1,]*post$sigma_E[,1]) + post$bM[,1,x]*post$sigma_M[,1] + (post$bF[, 1,x]* post$sigma_F[,1] + post$bF_mu[,1])*median(data$FI) + (post$bSN[, 1,x]* post$sigma_SN[,1] + post$bSN_mu[,1])*xseq + (post$bEA[,1,x]*post$sigma_EA[,1] + post$bEA_mu[,1])*rowSums(post$delta[,1,x,1:8]) + rowMeans(post$bRG[,1,x,]) + rowMeans(post$bRL[,1,x,]), post$sigma)
  p <- rbinom(n, 1, inv_logit(post$a[,2] +                                             rowMedians(post$bS[,2,]*post$sigma_S[,2]) + rowMedians(post$bE[,2,]*post$sigma_E[,2]) + post$bM[,2,x]*post$sigma_M[,2] + (post$bF[, 2,x]* post$sigma_F[,2] + post$bF_mu[,2])*median(data$FI) + (post$bSN[, 2,x]* post$sigma_SN[,2] + post$bSN_mu[,2])*xseq + (post$bEA[,2,x]*post$sigma_EA[,2] + post$bEA_mu[,2])*rowSums(post$delta[,2,x,1:8])+ rowMeans(post$bRG[,2,x,]) + rowMeans(post$bRL[,2,x,])))
  out<-mu*p
  #out<-(10^out)
  return(out)
}



x.seq = seq(2, 3, length.out=2)
i = 1


sp_suppreddlow = matrix(NA, nrow = 1000, ncol = 100)
sp_suppreddhigh = matrix(NA, nrow = 1000, ncol = 100)
sp_suppcontlow = matrix(NA, nrow = 1000, ncol = 100)
sp_suppconthigh = matrix(NA, nrow = 1000, ncol = 100)

for(i in 1:100){
  sp_suppreddlow[,i] <- sort(c(sup((1000-reddnprotestlow[i]), 1, x.seq[1]), rep(log10(4500), reddnprotestlow[i])))  
  sp_suppreddhigh[,i] <- sort(c(sup((1000-reddnprotesthigh[i]), 1, x.seq[2]), rep(log10(4500), reddnprotesthigh[i])))
  sp_suppcontlow[,i] <- sort(c(sup((1000-contnprotestlow[i]), 2, x.seq[1]), rep(log10(4500), contnprotestlow[i])))  
  sp_suppconthigh[,i] <- sort(c(sup((1000-contnprotesthigh[i]), 2, x.seq[2]), rep(log10(4500), contnprotesthigh[i])))
}



######################################################################################
#################### Environmental  ATTITUDES ########################################




source("WTA/code/script/getBaseData.R")

df<-df[complete.cases(df),]
#Calculate mean group intercepts for causal inference. 
df$p_mu <- NA

for(i in 1:2){
  ind = which(df$M==i)
  df$p_mu[ind]<-logit(mean(df$P[ind]-1))
}

data = as.list(df)
data$N = length(df$wta)
data$alpha = rep(2, max(data$EA)-1)

m.pb=readRDS(paste0("WTA/data/", file_names_protest[model_selected]))
post<-extract.samples(m.pb)




sup <- function(x, xseq){
  out <- rbinom(1000, 1, inv_logit(post$a +  post$b_mu*mean(data$p_mu[data$M==x]) + rowMedians(post$bS)*post$sigma_S + rowMedians(post$bE)*post$sigma_E + post$bM[,x]*post$sigma_M + (post$bF[, x]* post$sigma_F + post$bF_mu)*median(data$FI) + (post$bSN[,x]* post$sigma_SN + post$bSN_mu)*median(data$SN) + ifelse(xseq ==0, 0, ifelse(xseq ==1, (post$bEA[,x]*post$sigma_EA + post$bEA_mu)*(post$delta[,x,1]), (post$bEA[,x]*post$sigma_EA + post$bEA_mu)*rowSums(post$delta[,x,1:xseq])))+ rowMeans(post$bRL[,x,]) + rowMeans(post$bRG[,x,])))
  
  #out<-(10^out)
  return(out)
}

xseq = seq(0, 9, length.out =2)
redd<-replicate(100, sapply(xseq, function(x) sup(1, x)))
cont<-replicate(100, sapply(xseq, function(x) sup(2, x)))

reddnprotestlow<-colSums(redd[,1,])
reddnprotesthigh<-colSums(redd[,2,])
contnprotestlow<-colSums(cont[,1,])
contnprotesthigh<-colSums(cont[,2,])


#######################################################
################### SUPPLY CURVE ######################






source("WTA/code/script/getBaseData.R")

df<-df[complete.cases(df),]
#Calculate mean group intercepts for causal inference. 
df$p_mu <- NA

for(i in 1:2){
  ind = which(df$M==i)
  df$p_mu[ind]<-logit(mean(df$P[ind]-1))
}

data = as.list(df)
data$N = length(df$wta)
data$alpha = rep(2, max(data$EA)-1)

m.pb=readRDS(paste0("WTA/data/", file_names_protest[model_selected]))
post<-extract.samples(m.pb)




sup <- function(x, xseq){
  if(xseq == 0) fix = 0
  if(xseq == 1 )fix = (post$bEA[,x]*post$sigma_EA + post$bEA_mu)*(post$delta[,x,1])
  if(xseq > 1)  fix = (post$bEA[,x]*post$sigma_EA + post$bEA_mu)*rowSums(post$delta[,x,1:xseq])
  out <- rbinom(1000, 1, inv_logit(post$a +  post$b_mu*mean(data$p_mu[data$M==x]) + rowMedians(post$bS)*post$sigma_S + rowMedians(post$bE)*post$sigma_E + post$bM[,x]*post$sigma_M + (post$bF[, x]* post$sigma_F + post$bF_mu)*median(data$FI) + (post$bSN[,x]* post$sigma_SN + post$bSN_mu)*median(data$SN) + fix + rowMeans(post$bRL[,x,]) + rowMeans(post$bRG[,x,])))
  
  ifelse(xseq ==0, 0, ifelse(xseq ==1, (post$bEA[,x]*post$sigma_EA + post$bEA_mu)*(post$delta[,x,1]), (post$bEA[,x]) ))
  
  
  #out<-(10^out)
  return(out)
}

xseq = seq(0, 9, length.out =2)
redd<-replicate(100, sapply(xseq, function(x) sup(1, x)))
cont<-replicate(100, sapply(xseq, function(x) sup(2, x)))

reddnprotestlow<-colSums(redd[,1,])
reddnprotesthigh<-colSums(redd[,2,])
contnprotestlow<-colSums(cont[,1,])
contnprotesthigh<-colSums(cont[,2,])


#######################################################
################### SUPPLY CURVE ######################



source("WTA/code/script/getBaseData.R")
df<-df[complete.cases(df),]
#Remove Protest Bids
df <- df[df$P==1,]
data = as.list(df)
data$N = length(df$wta)
data$alpha = rep(2, max(data$EA)-1)
#Calculate mean group intercepts for causal inference. 
df <- df[complete.cases(df),]
df$mu_mu <- NA


for(i in 1:2){
  ind = which(df$M==i)
  df$mu_mu[ind]<-mean(df$wta[ind][df$wta[ind]>0])
}
data$mu_mu = df$mu_mu

m.occi=readRDS(paste0("WTA/data/", file_names_wta[model_selected]))
post<- extract.samples(m.occi)

sup <- function(n, x, xseq){
  if(xseq == 0) fix1 = 0
  if(xseq == 1 )fix1 = (post$bEA[,1,x]*post$sigma_EA[,1] + post$bEA_mu[,1])*(post$delta[,x,1,1])
  if(xseq > 1)  fix1 = (post$bEA[,1,x]*post$sigma_EA[,1] + post$bEA_mu[,1])*rowSums(post$delta[,x,1,1:xseq])
  if(xseq == 0) fix2 = 0
  if(xseq == 1 )fix2 = (post$bEA[,2,x]*post$sigma_EA[,2] + post$bEA_mu[,2])*(post$delta[,x,2,1])
  if(xseq > 1)  fix2 = (post$bEA[,2,x]*post$sigma_EA[,2] + post$bEA_mu[,2])*rowSums(post$delta[,x,2,1:xseq])
  
  mu<-rnorm(n,               post$b_mu*mean(data$mu_mu[data$M==x]) + post$a[,1] + rowMedians(post$bS[,1,]*post$sigma_S[,1]) + rowMedians(post$bE[,1,]*post$sigma_E[,1]) + post$bM[,1,x]*post$sigma_M[,1] + (post$bF[, 1,x]* post$sigma_F[,1] + post$bF_mu[,1])*median(data$FI) + (post$bSN[, 1,x]* post$sigma_SN[,1] + post$bSN_mu[,1])*median(data$SN) + fix1 +   rowMeans(post$bRG[,1,x,]) + rowMeans(post$bRL[,1,x,]), post$sigma)
  p <- rbinom(n, 1, inv_logit(post$a[,2] +                                        rowMedians(post$bS[,2,]*post$sigma_S[,2]) + rowMedians(post$bE[,2,]*post$sigma_E[,2]) + post$bM[,2,x]*post$sigma_M[,2] + (post$bF[, 2,x]* post$sigma_F[,2] + post$bF_mu[,2])*median(data$FI) + (post$bSN[, 2,x]* post$sigma_SN[,2] + post$bSN_mu[,2])*median(data$SN) + fix2 +   rowMeans(post$bRG[,2,x,]) + rowMeans(post$bRL[,2,x,])))
  out<-mu*p
  #out<-(10^out)
  return(out)
}
x.seq = seq(0, 9, length.out=2)


ea_suppreddlow = matrix(NA, nrow = 1000, ncol = 100)
ea_suppreddhigh = matrix(NA, nrow = 1000, ncol = 100)
ea_suppcontlow = matrix(NA, nrow = 1000, ncol = 100)
ea_suppconthigh = matrix(NA, nrow = 1000, ncol = 100)

for(i in 1:100){
  ea_suppreddlow[,i] <-  sort(c(sup((1000-reddnprotestlow[i]),  1, x.seq[1]), rep(log10(4500), reddnprotestlow[i])))  
  ea_suppreddhigh[,i] <- sort(c(sup((1000-reddnprotesthigh[i]), 1, x.seq[2]), rep(log10(4500), reddnprotesthigh[i])))
  ea_suppcontlow[,i] <-  sort(c(sup((1000-contnprotestlow[i]),  2, x.seq[1]), rep(log10(4500), contnprotestlow[i])))  
  ea_suppconthigh[,i] <- sort(c(sup((1000-contnprotesthigh[i]), 2, x.seq[2]), rep(log10(4500), contnprotesthigh[i])))
}





#########################################################################################################################
#################################### RISK ###############################################################################






source("WTA/code/script/getBaseData.R")

df<-df[complete.cases(df),]
#Calculate mean group intercepts for causal inference. 
df$p_mu <- NA

for(i in 1:2){
  ind = which(df$M==i)
  df$p_mu[ind]<-logit(mean(df$P[ind]-1))
}

data = as.list(df)
data$N = length(df$wta)
data$alpha = rep(2, max(data$EA)-1)

m.pb=readRDS(paste0("WTA/data/", file_names_protest[model_selected]))
post<-extract.samples(m.pb)



sup <- function(x, xseq){
  out <- rbinom(1000, 1, inv_logit(post$a +  post$b_mu*mean(data$p_mu[data$M==x]) + rowMedians(post$bS)*post$sigma_S + rowMedians(post$bE)*post$sigma_E + post$bM[,x]*post$sigma_M + (post$bF[, x]* post$sigma_F + post$bF_mu)*median(data$FI) + (post$bSN[,x]* post$sigma_SN + post$bSN_mu)*median(data$SN) + (post$bEA[,x]*post$sigma_EA+post$bEA_mu)*rowSums(post$delta[,x,1:8])+ post$bRL[,x,xseq] + rowMeans(post$bRG[,x,])))
  
  #out<-(10^out)
  return(out)
}



xseq = seq(1, 2, length.out =2)
redd<-replicate(100, sapply(xseq, function(x) sup(1, x)))
cont<-replicate(100, sapply(xseq, function(x) sup(2, x)))

reddnprotestlow<-colSums(redd[,1,])
reddnprotesthigh<-colSums(redd[,2,])
contnprotestlow<-colSums(cont[,1,])
contnprotesthigh<-colSums(cont[,2,])


#######################################################
################### SUPPLY CURVE ######################


source("WTA/code/script/getBaseData.R")
df<-df[complete.cases(df),]
#Remove Protest Bids
df <- df[df$P==1,]
data = as.list(df)
data$N = length(df$wta)
data$alpha = rep(2, max(data$EA)-1)
#Calculate mean group intercepts for causal inference. 
df <- df[complete.cases(df),]
df$mu_mu <- NA


for(i in 1:2){
  ind = which(df$M==i)
  df$mu_mu[ind]<-mean(df$wta[ind][df$wta[ind]>0])
}
data$mu_mu = df$mu_mu


m.occi=readRDS(paste0("WTA/data/", file_names_wta[model_selected]))
post<- extract.samples(m.occi)

# Loss Averse
sup <- function(n, x, xseq){
  mu<-rnorm(n,              post$b_mu*mean(data$mu_mu[data$M==x]) +    post$a[,1] + rowMedians(post$bS[,1,]*post$sigma_S[,1]) + rowMedians(post$bE[,1,]*post$sigma_E[,1]) + post$bM[,1,x]*post$sigma_M[,1] + (post$bF[, 1,x]* post$sigma_F[,1] + post$bF_mu[,1])*median(data$FI) + (post$bSN[, 1,x]* post$sigma_SN[,1] + post$bSN_mu[,1])*median(data$SN) + (post$bEA[,1,x]*post$sigma_EA[,1] + post$bEA_mu[,1])*rowSums(post$delta[,1,x,1:8]) + rowMeans(post$bRG[,1,x,])  + post$bRL[,1,x,xseq], post$sigma)
  p <- rbinom(n, 1, inv_logit(                                         post$a[,2] + rowMedians(post$bS[,2,]*post$sigma_S[,2]) + rowMedians(post$bE[,2,]*post$sigma_E[,2]) + post$bM[,2,x]*post$sigma_M[,2] + (post$bF[, 2,x]* post$sigma_F[,2] + post$bF_mu[,2])*median(data$FI) + (post$bSN[, 2,x]* post$sigma_SN[,2] + post$bSN_mu[,2])*median(data$SN) + (post$bEA[,2,x]*post$sigma_EA[,1] + post$bEA_mu[,2])*rowSums(post$delta[,2,x,1:8]) + rowMeans(post$bRG[,2,x,]) +  post$bRL[,2,x,xseq]))
  out<-mu*p
  #out<-(10^out)
  return(out)
}
x.seq = seq(1, 2, length.out=2)


rp_suppreddlow = matrix(NA, nrow = 1000, ncol = 100)
rp_suppreddhigh = matrix(NA, nrow = 1000, ncol = 100)
rp_suppcontlow = matrix(NA, nrow = 1000, ncol = 100)
rp_suppconthigh = matrix(NA, nrow = 1000, ncol = 100)

for(i in 1:100){
  rp_suppreddlow[,i] <- sort(c(sup((1000-reddnprotestlow[i]), 1, x.seq[1]), rep(log10(4500), reddnprotestlow[i])))  
  rp_suppreddhigh[,i] <- sort(c(sup((1000-reddnprotesthigh[i]), 1, x.seq[2]), rep(log10(4500), reddnprotesthigh[i])))
  rp_suppcontlow[,i] <- sort(c(sup((1000-contnprotestlow[i]), 2, x.seq[1]), rep(log10(4500), contnprotestlow[i])))  
  rp_suppconthigh[,i] <- sort(c(sup((1000-contnprotesthigh[i]), 2, x.seq[2]), rep(log10(4500), contnprotesthigh[i])))
}




#########################################################
################  PLOTTING ##############################
pdf("WTA/figures/interactions.pdf", width = 9, height = 4.8)
par(mfrow =c(2, 4), mar = c(4,1.2,1,1), oma =c(0, 4,1,1))


# OC


supCurve(rowMeans(10^(oc_suppreddlow)), ylim = c(0, 4000), middle = T, xlab = "")
for(i in 1:100){
  supCurve(10^oc_suppreddlow[,i], add = T, col = col.alpha("black", .1), ylim = c(0, 4000))
}

supCurve(rowMeans(10^oc_suppreddhigh), ylim = c(0, 4000), add = T, middle = T)
for(i in 2:100){
  supCurve(10^oc_suppreddhigh[,i], add = T, col = col.alpha("goldenrod3", .1), ylim = c(0, 4000))
}

# SP

supCurve(rowMeans(10^(sp_suppreddlow)), ylim = c(0, 4000), middle = T, xlab = "" )
for(i in 1:100){
  supCurve(10^sp_suppreddlow[,i], add = T, col = col.alpha("black", .1), ylim = c(0, 4000))
}

supCurve(rowMeans(10^sp_suppreddhigh), ylim = c(0, 4000), add = T, middle = T)
for(i in 2:100){
  supCurve(10^sp_suppreddhigh[,i], add = T, col = col.alpha("goldenrod3", .1), ylim = c(0, 4000))
}


# ea


supCurve(rowMeans(10^(ea_suppreddlow)), ylim = c(0, 4000), middle = T, xlab = "")
for(i in 1:100){
  supCurve(10^ea_suppreddlow[,i], add = T, col = col.alpha("black", .1), ylim = c(0, 4000))
}

supCurve(rowMeans(10^ea_suppreddhigh), ylim = c(0, 4000), add = T, middle = T)
for(i in 2:100){
  supCurve(10^ea_suppreddhigh[,i], add = T, col = col.alpha("goldenrod3", .1), ylim = c(0, 4000))
}

# rp

supCurve(rowMeans(10^(rp_suppreddlow)), ylim = c(0, 4000), middle = T, xlab = "")
for(i in 1:100){
  supCurve(10^rp_suppreddlow[,i], add = T, col = col.alpha("black", .1), ylim = c(0, 4000))
}

supCurve(rowMeans(10^rp_suppreddhigh), ylim = c(0, 4000), add = T, middle = T)
for(i in 2:100){
  supCurve(10^rp_suppreddhigh[,i], add = T, col = col.alpha("goldenrod3", .1), ylim = c(0, 4000))
}


#################### CONTROLS


supCurve(rowMeans(10^(oc_suppcontlow)), ylim = c(0, 4000), middle = T)
for(i in 1:100){
  supCurve(10^oc_suppcontlow[,i], add = T, col = col.alpha("black", .1), ylim = c(0, 4000))
}

supCurve(rowMeans(10^oc_suppconthigh), ylim = c(0, 4000), add = T, middle = T)
for(i in 2:100){
  supCurve(10^oc_suppconthigh[,i], add = T, col = col.alpha("goldenrod", .1), ylim = c(0, 4000))
}

# SP

supCurve(rowMeans(10^(sp_suppcontlow)), ylim = c(0, 4000), middle = T)
for(i in 1:100){
  supCurve(10^sp_suppcontlow[,i], add = T, col = col.alpha("black", .1), ylim = c(0, 4000))
}

supCurve(rowMeans(10^sp_suppconthigh), ylim = c(0, 4000), add = T, middle = T)
for(i in 2:100){
  supCurve(10^sp_suppconthigh[,i], add = T, col = col.alpha("goldenrod", .1), ylim = c(0, 4000))
}

# ea

supCurve(rowMeans(10^(ea_suppcontlow)), ylim = c(0, 4000), middle = T)
for(i in 1:100){
  supCurve(10^ea_suppcontlow[,i], add = T, col = col.alpha("black", .1), ylim = c(0, 4000))
}

supCurve(rowMeans(10^ea_suppconthigh), ylim = c(0, 4000), add = T, middle = T)
for(i in 2:100){
  supCurve(10^ea_suppconthigh[,i], add = T, col = col.alpha("goldenrod", .1), ylim = c(0, 4000))
}


# RP

supCurve(rowMeans(10^(rp_suppcontlow)), ylim = c(0, 4000), middle = T)
for(i in 1:100){
  supCurve(10^rp_suppcontlow[,i], add = T, col = col.alpha("black", .1), ylim = c(0, 4000))
}

supCurve(rowMeans(10^rp_suppconthigh), ylim = c(0, 4000), add = T, middle = T)
for(i in 2:100){
  supCurve(10^rp_suppconthigh[,i], add = T, col = col.alpha("goldenrod", .1), ylim = c(0, 4000))
}

mtext("Use-value", outer = T, side = 3, adj = .10, cex = .7)
mtext("Social Pressure", outer = T, side = 3, adj = .36, cex = .7)
mtext("Environmental Concern", outer = T, side = 3, adj = .63, cex = .7)
mtext("Risk", outer = T, side = 3, adj = .90, cex = .7)
mtext("Price", outer = T, side  = 2, cex = .7, adj = .30, line = 1)
mtext("Price", outer = T, side  = 2, cex = .7, adj = .80, line = 1)
mtext("Control", outer = T, side  = 2, cex = 1, adj = .30, line = 2)
mtext("REDD+", outer = T, side  = 2, cex = 1, adj = .80, line = 2)

dev.off()
