########################################################################
############## PLOTTING PROJECT FEATURES ###############################

pdf("WTA/figures/payment_type.pdf", height = 3.5, width =8)
par(mfcol = c(1,2), mar =c(5., 4, 1,1))

library(vioplot)

source("WTA/code/script/getBaseData.R")
df<-df[complete.cases(df),]
#Remove Protest Bids
df <- df[df$P==1,]
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
# 
# ####################################################
# ############## Plot Consent #########################
# 
# mc=readRDS("mc.RDS")
# post<- extract.samples(mc)
# 
# sup=function(xseq){
#   mu =  rnorm(1000, post$a[,1] + post$bK[,1,xseq]*post$sigma_K[,1], post$sigma)
#   p =   rbern(1000, inv_logit(post$a[,2] + post$bK[,2,xseq]*post$sigma_K[,2]))
#   out = p*mu
# }
# 
# 
# x.seq = 1:2
# 
# redd<-replicate(500, sapply(x.seq, sup))
# 
# colMeans(redd[,1,])
# 
# 
# G = c(rep("No Prior", 500), rep("Prior", 500))
# 
# G = factor(G, levels = unique(G))
# 
# 
# df <- data.frame(Y=c(colMeans(redd[,1,]),colMeans(redd[,2,])),
#                  G=G)
# library(vioplot)
# vioplot(df$Y ~ df$G, yaxt ="n", col = c(col.alpha("orange2", .2), col.alpha("steelblue1", .2)), ylab = "", xlab = "", las = 2, ylim = c(log10(300), log10(500)))
# stripchart(df$Y ~ df$G, vertical = TRUE, method = "jitter", col = c(col.alpha("orange2", .2), col.alpha("steelblue1", .2)), pch= 19, add =T)
# 
# 
# tick = 1:2
# axis(side=1, at=tick, labels = FALSE)
# text(x=tick,  c(2.46, 2.46), 
#      labels = unique(G), pos=2, offset = 0, xpd = TRUE, srt = 90)
# 
# tick = c(log10(350), log10(400), log10(450))
# axis(side=2, at=tick, labels = FALSE)
# text(.2, tick,
#      labels = round(10^tick), offset = 0, xpd = TRUE, srt = 0)


#####################################
##### BENIFIT SHARING ###############


m=readRDS("WTA/data/mpt.RDS")
post<- extract.samples(m)


sup=function(xseq){
  mu =  rnorm(1000, post$a[,1]                +      rowMeans(post$bE[,1,])*post$sigma_E[,1] +
                rowMeans(post$bS[,1,])*post$sigma_S[,1]  + rowMeans(post$bPT[,1,, xseq ]) +
                (post$bAG[,1])*rowSums(post$delta[,1,1:9]), post$sigma)
  
  p =   rbern(1000, inv_logit(post$a[,2]      +      rowMeans(post$bE[,2,])*post$sigma_E[,2] +
                                rowMeans(post$bS[,2,])*post$sigma_S[,2]  + rowMeans(post$bPT[,2,, xseq ]) +
                                (post$bAG[,2])*rowSums(post$delta[,2,1:9])))
  out = p*mu
}



x.seq = 1:2
redd<-replicate(500, sapply(x.seq, sup))

supCurve(10^rowMeans(redd[,1,]), ylim = c(0, 4000), add = F, middle = T, col = col.alpha("white",.0001))
for(i in 1:500){
  supCurve(10^(redd[,1,i]), col = col.alpha("dodgerblue", .1), add = T)
}

supCurve(10^rowMeans(redd[,2,]), ylim = c(0, 4000), add = T, middle = T, col = col.alpha("white",.0001))
for(i in 1:500){
  supCurve(10^(redd[,2,i]), ylim = c(0,4000), col = col.alpha("firebrick", .1), add = T)
}

colMeans(redd[,1,])


# 
# 
# ############################################################################
# ############## GOVERANCE ###################################################
# 
# 
# 
# m=readRDS("mgov.RDS")
# post<- extract.samples(m)
# 
# 
# 
# sup=function(xseq){
#   mu =  rnorm(1000, post$a[,1]                +      rowMeans(post$bSC[,1,])*post$sigma_SC[,1] + rowMeans(post$bG[,1,])*post$sigma_G[,1] +  rowMeans(post$bE[,1,])*post$sigma_E[,1]+  rowMeans(post$bS[,1,])*post$sigma_S[,1]  +  ifelse(xseq ==0, 0, ifelse(xseq ==1, (post$bAG[,1])*(post$delta[,1,1]), (post$bAG[,1])*rowSums(post$delta[,1,1:xseq]))), post$sigma)
#   p =   rbern(1000, inv_logit(post$a[,2] +    +      rowMeans(post$bSC[,2,])*post$sigma_SC[,2] + rowMeans(post$bG[,2,])*post$sigma_G[,2] +  rowMeans(post$bE[,2,])*post$sigma_E[,2] + rowMeans(post$bS[,2,])*post$sigma_S[,2]  +  ifelse(xseq ==0, 0, ifelse(xseq ==1, (post$bAG[,2])*(post$delta[,2,1]), (post$bAG[,2])*rowSums(post$delta[,2,1:xseq])))))
#   out = p*mu
# }
# 
# 
# x.seq = 0:4
# temp<-sapply(x.seq, function(x) sup(x))
# mu<-colMedians(temp)
# PI<-apply(temp, 2, PI, .89)
# x..seq = x.seq+1
# plot(x.seq, mu, ylim= c(0, 5.2), type = "l", col = "black", yaxt = "n", xlab = "Trust in local conseravation organiztions", ylab = "")
# shade(PI, x.seq, col = col.alpha("grey",.2))
# 
# 
# 
# tick = 1:5
# axis(side=2, at=tick, labels = FALSE)
# text(-.4, tick,
#      labels = round(10^tick), xpd = TRUE, srt = 90)




####################################################################
############## CONSENT PROTEST #####################################

# 
# mc=readRDS("pc.RDS")
# post<- extract.samples(mc)
# 
# sup=function(xseq){
#   p =   rbern(1000, inv_logit(post$a + post$bK[,xseq]*post$sigma_K + rowMeans(post$bS)*post$sigma_S + rowMeans(post$bG)*post$sigma_G  + rowMeans(post$bE)*post$sigma_E))
# }
# 
# 
# x.seq = 1:2
# 
# redd<-replicate(500, sapply(x.seq, sup))
# 
# colMeans(redd[,1,])
# 
# 
# G = c(rep("No Prior", 500), rep("Prior", 500))
# 
# G = factor(G, levels = unique(G))
# 
# 
# df <- data.frame(Y=c(colSums(redd[,1,]),colSums(redd[,2,])),
#                  G=G)
# library(vioplot)
# vioplot(df$Y ~ df$G,  col = c(col.alpha("orange2", .2), col.alpha("steelblue1", .2)), ylab = "", xlab = "", las = 2, ylim = c(50, 250))
# stripchart(df$Y ~ df$G, vertical = TRUE, method = "jitter", col = c(col.alpha("orange2", .2), col.alpha("steelblue1", .2)), pch= 19, add =T)
# 


#############################################################
################# PAYMENT TYPE PROTEST ######################



m=readRDS("WTA/data/p.pt.RDS")
post<- extract.samples(m)

sup=function(xseq){
  p =   rbern(1000, inv_logit(post$a  +  rowMeans(post$bE)*post$sigma_E +
                                rowMeans(post$bS)*post$sigma_S  + post$bPT[,2, xseq ] +  (post$bAG)*rowSums(post$delta[,1:9])))
  
}



x.seq = 1:2
redd<-replicate(500, sapply(x.seq, sup))

colMeans(redd[,1,])


G = c(rep("No indv pay", 500), rep("Indv pay", 500))

G = factor(G, levels = unique(G))


df <- data.frame(Y=c(colSums(redd[,1,]),colSums(redd[,2,])),
                 G=G)
library(vioplot)
vioplot(df$Y ~ df$G,  col = c(col.alpha("dodgerblue", .2), col.alpha("firebrick", .2)), ylab = "Number of Protests (out of 1000)", xlab = "", las = 1, ylim = c(50, 250))
stripchart(df$Y ~ df$G, vertical = TRUE, method = "jitter", col = c(col.alpha("dodgerblue", .2), col.alpha("firebrick", .2)), pch= 19, add =T)

dev.off()

# 
# 
# #############################################
# ########## GOVERANCE PROTEST #################
# m=readRDS("pgov.RDS")
# post<- extract.samples(m)
# 
# 
# sup=function(xseq){
#   p =   rbern(1000, inv_logit(post$a  +      rowMeans(post$bSC[,])*post$sigma_SC + rowMeans(post$bPT) +  rowMeans(post$bG[,])*post$sigma_G +  rowMeans(post$bE)*post$sigma_E + rowMeans(post$bS)*post$sigma_S +  ifelse(xseq ==0, 0, ifelse(xseq ==1, (post$bAG)*(post$delta[,2]), (post$bAG)*rowSums(post$delta[,1:xseq])))))
# }
# 
# 
# 
# x.seq = 0:4
# redd<-replicate(500, sapply(x.seq, sup))
# 
# m = matrix(NA, 500, length(x.seq))
# cnt <- 1
# for(i in 1:length(x.seq)){
#   m[,i]=colSums(redd[,i,])
# }
# 
# plot(colMeans(m)~ x.seq, type = "l", col = "black", xlab = "Trust in local conseravation organiztions", ylab = "", ylim = c(50, 250))
# PI<-apply(m, 2, PI, .89)
# shade(PI, x.seq, col = col.alpha("grey",.2))
# 
# mtext("Predicted WTA", outer = T, side = 2,  adj= .80, cex = .8)
# mtext("Protest Bids", outer = T, side = 2, adj = .25, cex = .8)












