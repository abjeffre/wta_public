########################################
######### PROTEST WEALTH ####################
source("WTA/code/script/getBaseData_interaction.R")
forest<- read.csv("data/forest_data.csv")

# Build Data
df <- df[!is.na(df$wta), ]
df<-complete(mice(df, method = "cart"))
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
data$Y = df$P-1L


m.pd <- stan(file = paste0("code/models/", "protest_with_wealth_FI_interaction.stan"), data = data, iter = 1000, cores = 8, chains = 8)
saveRDS(m.pd, "data/protest_with_wealth_FI_interaction.RDS")