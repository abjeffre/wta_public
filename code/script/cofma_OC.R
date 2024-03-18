########################################
######### Opportunity costs ###########
df<-readRDS("data/base_data.rds")

df<-df[complete.cases(df),]
#Remove Protest Bids
df <- df[df$P==1,]

#Calculate mean group intercepts for causal inference. 
df$mu_mu <- NA

for(i in 1:2){
  ind = which(df$M==i)
  df$mu_mu[ind]<-mean(df$wta[ind][df$wta[ind]>0])
}
  
data = as.list(df)
data$N = length(df$wta)
data$alpha = rep(2, max(data$EA)-1)

m.oc <- stan(file = paste0("code/models/", "wta_OC_CI.stan"), data = data, iter = 1000, cores = 4, chains = 4)

saveRDS(m.oc, "data/mocci.RDS")
# post<- extract.samples(m.oc)
# 
# 
# 
# # OPPORTUNITY COSTS
# sup <- function(x, xseq){
#   mu<-rnorm(1000,                post$a[,1] + rowMedians(post$bS[,1,]*post$sigma_S[,1]) + rowMedians(post$bE[,1,]*post$sigma_E[,1]) + post$bM[,1,x]*post$sigma_M[,1] + (post$bF[, 1,x]* post$sigma_F[,1] + post$bF_mu[,1])*xseq + (post$bSN[, 1,x]* post$sigma_SN[,1] + post$bSN_mu[,1])*median(df$SN) +  (post$bEA[,1,x]*post$sigma_EA[,1] + post$bEA_mu[,1])*rowSums(post$delta[,1,x,1:8]), post$sigma)
#   p <- rbinom(1000, 1, inv_logit(post$a[,2] + rowMedians(post$bS[,2,]*post$sigma_S[,2]) + rowMedians(post$bE[,2,]*post$sigma_E[,2]) + post$bM[,2,x]*post$sigma_M[,2] + (post$bF[, 2,x]* post$sigma_F[,2] + post$bF_mu[,2])*xseq + (post$bSN[, 2,x]* post$sigma_SN[,2] + post$bSN_mu[,2])*median(df$SN) +  (post$bEA[,2,x]*post$sigma_EA[,2] + post$bEA_mu[,2])*rowSums(post$delta[,2,x,1:8])))
#   out<-mu*p
#   #out<-(10^out)
#   return(out)
# }
# 
# x.seq = seq(0, 4, length.out=100)
# temp<-sapply(x.seq, function(x) sup(1, x))
# mu<-colMedians(temp)
# PI<-apply(temp, 2, PI, .89)
# plot(x.seq, mu, ylim= c(0, 4.1), type = "l", col = "orange")
# shade(PI, x.seq, col = col.alpha("orange",.2))
# 
# x.seq = seq(0, 4, length.out=100)
# temp<-sapply(x.seq, function(x) sup(2, x))
# mu<-colMedians(temp)
# PI<-apply(temp, 2, PI, .89)
# lines(x.seq, mu, ylim= c(0, 4), col = "blue")
# shade(PI, x.seq, col = col.alpha("blue",.2))
# 
# 
# par(mfrow = c(1,3))
# for(j in 1:3){
#   a<-replicate(500,sort(10^sup(1, j)))
#   supCurve(rowMedians(a), ylim = c(0, 4000), middle = T, col = "firebrick")
#   
#   for(i in 1:500){
#     supCurve(a[,i], add = T, col = col.alpha("firebrick", .05))
#   }
#   
#   a<-replicate(500,sort(10^sup(2, j)))
#   supCurve(rowMedians(a), add = T, col = "dodgerblue", middle = T)
#   for(i in 1:500){
#     supCurve(a[,i], add = T, col = col.alpha("dodgerblue", .05))
#   }
# }
# 
# 
# # SOCIAL NORMS
# sup <- function(x, xseq){
#   mu<-rnorm(1000,                post$a[,1] + rowMedians(post$bS[,1,]*post$sigma_S[,1]) + rowMedians(post$bE[,1,]*post$sigma_E[,1]) + post$bM[,1,x]*post$sigma_M[,1] + (post$bF[, 1,x]* post$sigma_F[,1] + post$bF_mu[,1])*2.6 + (post$bSN[, 1,x]* post$sigma_SN[,1] + post$bSN_mu[,1])*xseq + post$bEA[,1]*rowSums(post$delta[,1,1:8]), post$sigma)
#   p <- rbinom(1000, 1, inv_logit(post$a[,2] + rowMedians(post$bS[,2,]*post$sigma_S[,2]) + rowMedians(post$bE[,2,]*post$sigma_E[,2]) + post$bM[,2,x]*post$sigma_M[,2] + (post$bF[, 2,x]* post$sigma_F[,2] + post$bF_mu[,2])*2.6 + (post$bSN[, 2,x]* post$sigma_SN[,2] + post$bSN_mu[,2])*xseq + post$bEA[,2]*rowSums(post$delta[,2,1:8])))
#   out<-mu*p
#   #out<-(10^out)
#   return(out)
# }
# x.seq = seq(0, 4, length.out=100)
# temp<-sapply(x.seq, function(x) sup(1, x))
# mu<-colMedians(temp)
# PI<-apply(temp, 2, PI, .89)
# plot(x.seq, mu, ylim= c(0, 6), type = "l", col = "orange")
# shade(PI, x.seq, col = col.alpha("orange",.2))
# 
# x.seq = seq(0, 4, length.out=100)
# temp<-sapply(x.seq, function(x) sup(2, x))
# mu<-colMedians(temp)
# PI<-apply(temp, 2, PI, .89)
# lines(x.seq, mu, ylim= c(0, 4), col = "blue")
# shade(PI, x.seq, col = col.alpha("blue",.2))
# 
# 
# names = c("Forest Income = $10", "Forest Income = $100", "Forest Income = $1000")
# par(mfrow = c(1,3))
# for(j in 1:3){
#   supCurve(10^sup(1, j), ylim = c(0, 2500), middle = T, col = "firebrick", main = names[j])
#   
#   for(i in 1:100){
#     supCurve(10^sup(1, j), add = T, col = col.alpha("firebrick", .05), main = names[j])
#   }
#   supCurve(10^sup(2, j), add = T, col = "dodgerblue", middle = T)
#   for(i in 1:100){
#     supCurve(10^sup(2, j), add = T, col = col.alpha("dodgerblue", .05), main = names[j])
#   }
# }
# 
# # RISK
# sup <- function(x, xseq){
#   mu<-rnorm(1000,                post$a[,1] + rowMedians(post$bS[,1,]*post$sigma_S[,1]) + rowMedians(post$bE[,1,]*post$sigma_E[,1]) + post$bM[,1,x]*post$sigma_M[,1] + (post$bF[, 1,x]* post$sigma_F[,1] + post$bF_mu[,1])*2.6 + (post$bSN[, 1,x]* post$sigma_SN[,1] + post$bSN_mu[,1])*median(data$SN) + post$bEA[,1]*rowSums(post$delta[,1,1:8]) + post$bRG[,1,x,xseq] + + post$bRL[,1,x,xseq], post$sigma)
#   p <- rbinom(1000, 1, inv_logit(post$a[,2] + rowMedians(post$bS[,2,]*post$sigma_S[,2]) + rowMedians(post$bE[,2,]*post$sigma_E[,2]) + post$bM[,2,x]*post$sigma_M[,2] + (post$bF[, 2,x]* post$sigma_F[,2] + post$bF_mu[,2])*2.6 + (post$bSN[, 2,x]* post$sigma_SN[,2] + post$bSN_mu[,2])*median(data$SN) + post$bEA[,2]*rowSums(post$delta[,2,1:8]) + post$bRG[,2,x,xseq] + + post$bRL[,2,x,xseq]))
#   out<-mu*p
#   #out<-(10^out)
#   return(out)
# }
# x.seq = seq(1, 2, length.out=100)
# temp<-sapply(x.seq, function(x) sup(1, x))
# mu<-colMedians(temp)
# PI<-apply(temp, 2, PI, .89)
# plot(x.seq, mu, ylim= c(0, 6), type = "l", col = "orange")
# shade(PI, x.seq, col = col.alpha("orange",.2))
# 
# temp<-sapply(x.seq, function(x) sup(2, x))
# mu<-colMedians(temp)
# PI<-apply(temp, 2, PI, .89)
# lines(x.seq, mu, ylim= c(0, 4), col = "blue")
# shade(PI, x.seq, col = col.alpha("blue",.2))
# 
# 
# 
# 
# 
# # ENVIRONMENTAL ATTITUDES
# sup <- function(x, xseq){
#   mu<-rnorm(1000,                post$a[,1] + rowMedians(post$bS[,1,]*post$sigma_S[,1]) + rowMedians(post$bE[,1,]*post$sigma_E[,1]) + post$bM[,1,x]*post$sigma_M[,1] + (post$bF[, 1,x]* post$sigma_F[,1] + post$bF_mu[,1])*2.6 + (post$bSN[, 1,x]* post$sigma_SN[,1] + post$bSN_mu[,1])*median(data$SN) + post$bEA[,1]*rowSums(post$delta[,1,1:xseq]) + post$bRG[,1,x,1] + + post$bRL[,1,x,1], post$sigma)
#   p <- rbinom(1000, 1, inv_logit(post$a[,2] + rowMedians(post$bS[,2,]*post$sigma_S[,2]) + rowMedians(post$bE[,2,]*post$sigma_E[,2]) + post$bM[,2,x]*post$sigma_M[,2] + (post$bF[, 2,x]* post$sigma_F[,2] + post$bF_mu[,2])*2.6 + (post$bSN[, 2,x]* post$sigma_SN[,2] + post$bSN_mu[,2])*median(data$SN) + post$bEA[,2]*rowSums(post$delta[,2,1:xseq]) + post$bRG[,2,x,1] + + post$bRL[,2,x,1]))
#   out<-mu*p
#   #out<-(10^out)
#   return(out)
# }
# x.seq = 3:9
# temp<-sapply(x.seq, function(x) sup(1, x))
# mu<-colMedians(temp)
# PI<-apply(temp, 2, PI, .89)
# plot(x.seq, mu, ylim= c(0, 6), type = "l", col = "orange")
# shade(PI, x.seq, col = col.alpha("orange",.2))
# 
# temp<-sapply(x.seq, function(x) sup(2, x))
# mu<-colMedians(temp)
# PI<-apply(temp, 2, PI, .89)
# lines(x.seq, mu, ylim= c(0, 4), col = "blue")
# shade(PI, x.seq, col = col.alpha("blue",.2))
# 
# 
# 
# 
# par(mfrow = c(1,3))
# j= 9
# supCurve(10^sup(1, j), ylim = c(0, 2500))
# 
# for(i in 1:100){
#   supCurve(10^sup(1, j), add = T, col = "blue")
# }
# 
# for(i in 1:100){
#   supCurve(10^sup(2, j), add = T, col = "red")
# }
# 
# 
# 
# # MARGINAL 
# 
# 
# 
# pdf("margin.pdf", width = 10, height = 5.5)
# par(mfrow = c(1,2))
# m.oc=readRDS("moc.RDS")
# post<-extract.samples(m.oc)
# 
# 
# sup <- function(x){
#   mu<-rnorm(1000,                post$a[,1] + rowMedians(post$bS[,1,]*post$sigma_S[,1]) + rowMedians(post$bE[,1,]*post$sigma_E[,1]) + post$bM[,1,x]*post$sigma_M[,1] + (post$bF[, 1,x]* post$sigma_F[,1] + post$bF_mu[,1])*median(data$FI) + (post$bSN[, 1,x]* post$sigma_SN[,1] + post$bSN_mu[,1])*median(data$SN) + (post$bEA[,1,x]*post$sigma_EA[,1] + post$bEA_mu[,1])*rowSums(post$delta[,1,x,1:9]) + post$bRG[,1,x,1] + + post$bRL[,1,x,1], post$sigma)
#   p <- rbinom(1000, 1, inv_logit(post$a[,2] + rowMedians(post$bS[,2,]*post$sigma_S[,2]) + rowMedians(post$bE[,2,]*post$sigma_E[,2]) + post$bM[,2,x]*post$sigma_M[,2] + (post$bF[, 2,x]* post$sigma_F[,2] + post$bF_mu[,2])*median(data$FI) + (post$bSN[, 2,x]* post$sigma_SN[,2] + post$bSN_mu[,2])*median(data$SN) + (post$bEA[,2,x]*post$sigma_EA[,2] + post$bEA_mu[,2])*rowSums(post$delta[,2,x,1:9]) + post$bRG[,2,x,1] + + post$bRL[,2,x,1]))
#   out<-mu*p
#   #out<-(10^out)
#   return(out)
# }
# 
# for(j in 1:1){
#   a<-replicate(500,sort(10^sup(1)))
#   supCurve(rowMedians(a), ylim = c(0, 4000), middle = T, col = "firebrick")
#   
#   for(i in 1:500){
#     supCurve(a[,i], add = T, col = col.alpha("firebrick", .05))
#   }
#   
#   a<-replicate(500,sort(10^sup(2)))
#   supCurve(rowMedians(a), add = T, col = "dodgerblue", middle = T)
#   for(i in 1:500){
#     supCurve(a[,i], add = T, col = col.alpha("dodgerblue", .05))
#   }
# }
# 
# 
# m.pb=readRDS("mpb.RDS")
# post<-extract.samples(m.pb)
# 
# 
# sup <- function(x){
#   out <- rbinom(1000, 1, inv_logit(post$a + rowMedians(post$bS)*post$sigma_S + rowMedians(post$bE)*post$sigma_E + post$bM[,x]*post$sigma_M + (post$bF[,x]* post$sigma_F + post$bF_mu)*median(data$FI) + (post$bSN[,x]* post$sigma_SN + post$bSN_mu)*median(data$SN) + rowMedians(post$bRL[,x,]) + rowMedians(post$bRG[,x,]) + (post$bEA[,x]*post$sigma_EA+post$bEA_mu)*rowSums(post$delta[,x,1:8])))
#   
#   #out<-(10^out)
#   return(out)
# }
# 
# 
# 
# redd<-replicate(500, sup(1))
# cont<-replicate(500, sup(2))
# 
# Y<-colSums(redd[,])
# Y2<-colSums(cont[,])
# 
# Y=c(Y, Y2)
# G = c(rep("REDD+", 500), rep("Control", 500))
# 
# df <- data.frame(Y=Y,
#                  G=G)
# vioplot(df$Y ~ df$G, col = c(col.alpha("dodgerblue", .2), col.alpha("firebrick", .2)), ylab = "Protest bids", xlab = "")
# stripchart(df$Y ~ df$G, vertical = TRUE, method = "jitter", col = c(col.alpha("dodgerblue", .2), col.alpha("firebrick", .2)), pch= 19, add =T)
# 
# 
# 
# 
# dev.off()