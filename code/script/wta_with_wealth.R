###################################################
######### Opportunity costs With Wealth ###########
df<-readRDS("data/base_data.rds")
df <- df[!is.na(df$wta), ]
df <- complete(mice(df, method = "cart"))



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



m.oc <- stan(file = paste0("code/models/", "wta_with_wealth.stan"), data = data, iter = 1000, cores = 4, chains = 4)
saveRDS(m.oc, "WTA/data/wta_with_wealth.RDS")
