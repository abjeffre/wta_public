########################################
######### PROTEST WEALTH ####################

df<-readRDS("data/base_data.rds")
forest<- read.csv("data/forest_data.csv")

# Build Data
df <- df[!is.na(df$wta), ]
df <- complete(mice(df, method = "cart"))

df<-df[complete.cases(df),]

#Calculate mean group intercepts for causal inference. 
df$p_mu <- NA

for(i in 1:2){
  ind = which(df$M==i)
  df$p_mu[ind]<-logit(mean(df$P[ind]-1))
}

# FINISH Data
data = as.list(df)
data$N = length(df$wta)
data$alpha = rep(2, max(data$EA)-1)
data$beta = rep(2, max(data$O)-1)
data$Y = df$P-1L

m.pb <- stan(file = paste0("code/models/", "protest_with_wealth.stan"), data = data, iter = 1000, cores = 4, chains = 4)
saveRDS(m.pb, "data/protest_with_wealth.RDS")