######################################################################
########## FIRST IDENTIFY WHAT MODEL IS BEING USED  ##################
# use 1 for without wealth
# use 2 for with wealth
# Do you wanna save?
save = FALSE


file_names_wta <- c("mocci.RDS", "wta_with_wealth_FI_interaction.RDS")
file_names_protest <- c("mpb.RDS", "protest_with_wealth_FI_interaction.RDS")
model_selected <- 2


#
if(save == TRUE) pdf("WTA/figures/margin.pdf", width = 10, height = 3.5)
# MARGINAL 

par(mfrow = c(1,3))
m.oc=readRDS(paste0("WTA/data/", file_names_wta[model_selected]))
post<-extract.samples(m.oc)

source("WTA/code/script/getBaseData_interaction.R")
df<-df[complete.cases(df),]
#Remove Protest Bids
data = as.list(df)
data$N = length(df$wta)
data$alpha = rep(2, max(data$EA)-1)
#Calculate mean group intercepts for causal inference. 
df$mu_mu <- NA
df$p_mu <- NA
for(i in 1:2){
  ind = which(df$M==i)
  df$mu_mu[ind]<-mean(df$wta[ind][df$wta[ind]>0])
  df$p_mu[ind]<-logit(mean(df$P[ind]-1))
}
data$mu_mu = df$mu_mu
data$p_mu = df$p_mu

n = 1000
sup <- function(x){
  mu<-rnorm(n,                post$a[,1] + post$b_mu*median(data$mu_mu[data$M==x]) + rowMedians(post$bS[,1,]*post$sigma_S[,1]) + rowMedians(post$bE[,1,]*post$sigma_E[,1])  + post$bM[,1,x]*post$sigma_M[,1] + (post$bF[, 1,x]* post$sigma_F[,1] + post$bF_mu[,1])*median(data$FI) + (post$bSN[, 1,x]* post$sigma_SN[,1] + post$bSN_mu[,1])*median(data$SN) + (post$bEA[,1,x]*post$sigma_EA[,1] + post$bEA_mu[,1])*rowSums(post$delta[,1,x,1:9]) + post$bRG[,1,x,1] +  post$bRL[,1,x,1], post$sigma)
  p <- rbinom(n, 1, inv_logit(post$a[,2]  +                                          rowMedians(post$bS[,2,]*post$sigma_S[,2]) + rowMedians(post$bE[,2,]*post$sigma_E[,2])  + post$bM[,2,x]*post$sigma_M[,2] + (post$bF[, 2,x]* post$sigma_F[,2] + post$bF_mu[,2])*median(data$FI) + (post$bSN[, 2,x]* post$sigma_SN[,2] + post$bSN_mu[,2])*median(data$SN) + (post$bEA[,2,x]*post$sigma_EA[,2] + post$bEA_mu[,2])*rowSums(post$delta[,2,x,1:9]) + post$bRG[,2,x,1] +  post$bRL[,2,x,1]))
  out<-mu*p
  #out<-(10^out)
  return(out)
}

a<-replicate(500,sort(10^sup(1)))
supCurve(rowMedians(a), ylim = c(0, 4000), middle = T, col = "firebrick")

for(i in 1:500){
  supCurve(a[,i], add = T, col = col.alpha("firebrick", .05))
}

a<-replicate(500,sort(10^sup(2)))
supCurve(rowMedians(a), add = T, col = "dodgerblue", middle = T)
for(i in 1:500){
  supCurve(a[,i], add = T, col = col.alpha("dodgerblue", .05))
}


m.pb=readRDS(paste0("WTA/data/", file_names_protest[model_selected]))
post<-extract.samples(m.pb)

source("WTA/code/script/getBaseData_interaction.R")
df<-df[complete.cases(df),]
#Remove Protest Bids
data = as.list(df)
data$N = length(df$wta)
data$alpha = rep(2, max(data$EA)-1)
#Calculate mean group intercepts for causal inference. 
df$mu_mu <- NA
df$p_mu <- NA
for(i in 1:2){
  ind = which(df$M==i)
  df$mu_mu[ind]<-mean(df$wta[ind][df$wta[ind]>0])
  df$p_mu[ind]<-logit(mean(df$P[ind]-1))
}
data$mu_mu = df$mu_mu
data$p_mu = df$p_mu

sup <- function(x){
  out <- rbinom(n, 1, inv_logit(post$a + rowMedians(post$bS)*post$sigma_S + rowMedians(post$bE)*post$sigma_E +
                                  post$bM[,x]*post$sigma_M + (post$bF[,x]* post$sigma_F + post$bF_mu)*median(data$FI)+
                                  (post$bSN[,x]* post$sigma_SN + post$bSN_mu)*median(data$SN) +
                                  rowMedians(post$bRL[,x,]) + rowMedians(post$bRG[,x,]) +
                                  (post$bEA[,x]*post$sigma_EA+post$bEA_mu)*rowSums(post$delta[,x,1:9]) + post$b_mu*mean(data$p_mu[data$M==x])))
  
  #out<-(10^out)
  return(out)
}



redd<-replicate(500, sup(1))
cont<-replicate(500, sup(2))

Y<-colSums(redd[,])
Y2<-colSums(cont[,])

Y=c(Y, Y2)
G = c(rep("REDD+", 500), rep("Control", 500))

df <- data.frame(Y=Y,
                 G=G)
vioplot(df$Y ~ df$G, col = c(col.alpha("dodgerblue", .2), col.alpha("firebrick", .2)), ylab = "Protest bids", xlab = "")
stripchart(df$Y ~ df$G, vertical = TRUE, method = "jitter", col = c(col.alpha("dodgerblue", .2), col.alpha("firebrick", .2)), pch= 19, add =T)



######################################
########## GET FULL COST ESTIMATES ###



qseq = seq(.1, .8, length.out = 16)
n= 13461

l = list()
for(x in 1:2){
  m = matrix(NA, 500, length(qseq))
  cnt = 1
  for(perc in qseq){
    source("WTA/code/script/getBaseData_interaction.R")
    df<-df[complete.cases(df),]
    #Remove Protest Bids
    data = as.list(df)
    data$N = length(df$wta)
    data$alpha = rep(2, max(data$EA)-1)
    #Calculate mean group intercepts for causal inference. 
    df$mu_mu <- NA
    df$p_mu <- NA
    for(i in 1:2){
      ind = which(df$M==i)
      df$mu_mu[ind]<-mean(df$wta[ind][df$wta[ind]>0])
      df$p_mu[ind]<-logit(mean(df$P[ind]-1))
    }
    data$mu_mu = df$mu_mu
    data$p_mu = df$p_mu
  
    #read in data
    m.pb=readRDS(paste0("WTA/data/", file_names_protest[model_selected]))
    post<-extract.samples(m.pb)
    sup <- function(x){
      out <- rbinom(n, 1, inv_logit(post$a + rowMedians(post$bS)*post$sigma_S + rowMedians(post$bE)*post$sigma_E +
                                      post$bM[,x]*post$sigma_M + (post$bF[,x]* post$sigma_F + post$bF_mu)*median(data$FI)+
                                      (post$bSN[,x]* post$sigma_SN + post$bSN_mu)*median(data$SN) +
                                      rowMedians(post$bRL[,x,]) + rowMedians(post$bRG[,x,]) +
                                      (post$bEA[,x]*post$sigma_EA+post$bEA_mu)*rowSums(post$delta[,x,1:9]) + post$b_mu*mean(data$p_mu[data$M==x])))
      
      #out<-(10^out)
      return(out)
    }
    redd<-replicate(500, sup(x))
    n2=n-mean(colSums(redd)) # get the total number of in market households
    print("got here")
    
    source("WTA/code/script/getBaseData_interaction.R")
    df<-df[complete.cases(df),]
    #Remove Protest Bids
    df = df[df$P==1, ]
    data = as.list(df)
    data$N = length(df$wta)
    data$alpha = rep(2, max(data$EA)-1)
    #Calculate mean group intercepts for causal inference. 
    df$mu_mu <- NA
    
    for(i in 1:2){
      ind = which(df$M==i)
      df$mu_mu[ind]<-mean(df$wta[ind][df$wta[ind]>0])
    }
    data$mu_mu = df$mu_mu
    #Load data
    m.pb=readRDS(paste0("WTA/data/", file_names_wta[model_selected]))
    post<-extract.samples(m.pb)
    sup2 <- function(x, n){
      mu<-rnorm(n,                post$a[,1] + post$b_mu*median(data$mu_mu[data$M==x]) + rowMedians(post$bS[,1,]*post$sigma_S[,1]) + rowMedians(post$bE[,1,]*post$sigma_E[,1])  + post$bM[,1,x]*post$sigma_M[,1] + (post$bF[, 1,x]* post$sigma_F[,1] + post$bF_mu[,1])*median(data$FI) + (post$bSN[, 1,x]* post$sigma_SN[,1] + post$bSN_mu[,1])*median(data$SN) + (post$bEA[,1,x]*post$sigma_EA[,1] + post$bEA_mu[,1])*rowSums(post$delta[,1,x,1:9]) + post$bRG[,1,x,1] + + post$bRL[,1,x,1], post$sigma)
      p <- rbinom(n, 1, inv_logit(post$a[,2]  +                                         rowMedians(post$bS[,2,]*post$sigma_S[,2]) + rowMedians(post$bE[,2,]*post$sigma_E[,2]) + post$bM[,2,x]*post$sigma_M[,2] + (post$bF[, 2,x]* post$sigma_F[,2] + post$bF_mu[,2])*median(data$FI) + (post$bSN[, 2,x]* post$sigma_SN[,2] + post$bSN_mu[,2])*median(data$SN) + (post$bEA[,2,x]*post$sigma_EA[,2] + post$bEA_mu[,2])*rowSums(post$delta[,2,x,1:9]) + post$bRG[,2,x,1] + + post$bRL[,2,x,1]))
      out<-mu*p
      #out<-(10^out)
      return(out)
    }
    print("just before rep")
    print(perc)
    print(x)
    print(n2)
    m[,cnt] = replicate(500,sum(sort(10^sup2(x, round(n2)))[1:round(n*perc)]))
    cnt <- cnt+1
  }
l[[x]]=m
}




y=1-(l[[1]]/l[[2]])


plot(colMeans(y)*100 ~ qseq, ylab = "Expected percent savings due to REDD+", xlab = "Percent targeted enrollment", ylim = c(15, 25), type = "l")
PI = apply(y*100, 2, PI, .89)
shade(PI, qseq)


if(save == TRUE) dev.off()