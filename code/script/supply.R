
########################################################
############# CONSTRUCT DATA ###########################
df <-readRDS("data/supply_data.RDS")

if(run == TRUE){
    intercept <- stan(file = paste0(path$models, "wta_base_new.stan"), data = dat_willing, iter = 1000, cores = 4, chains = 4)
}
saveRDS(intercept, file = paste0("WTA/data/", "wta_intercept.rds"))
#################################################
############ PLOTTING ###########################

# 
# intercept <- readRDS(paste0(path$data, "wta_intercept.rds"))

# #find means 


# intercept <- readRDS(paste0(path$data, "wta_intercept.rds"))


# par(mfrow = c(1,2), mar = c(5,4,1,1), oma = c(0,0,1,0))
# post <- extract.samples(intercept)
# sup <- function(){
#   mu<-rnorm(1000, post$a[,1] + rowMedians(post$bS[,1,]*post$sigma_S[,1]) + rowMedians(post$bE[,1,]*post$sigma_e[,1]), post$sigma)
# # mu <- rnorm(1000, mu, post$sigma)
#   p <- rbinom(1000, 1, inv_logit(post$a[,2] + rowMedians(post$bS[,2,]*post$sigma_S[,2]) + rowMedians(post$bE[,2,]*post$sigma_e[,2])))
#   out<-mu*p
#   out<-exp(out)/2500
#   return(out)
# }

# set.seed(2)
# Supply<-replicate(500,sup())

# #Get average
# out <- c()
# for(i in 1:500){
#   a <-sort(Supply[,i])
#   out<- cbind(out, a)
# }

# expected_supply <- rowMedians(out)
# supCurve(expected_supply, col = col.alpha("black", 0.5), prop = TRUE, ylim = c(0,median(apply(Supply, 2, max))))
# supCurve(exp(dat_willing$wta)/2500, col = col.alpha("black", 0.5), prop = TRUE, add = TRUE)

# for(i in 2:500){
#   supCurve(Supply[,i], col = col.alpha("dodgerblue", 0.05), add = TRUE, prop = TRUE)
# }
# supCurve(expected_supply, col = col.alpha("black", 0.5), prop = TRUE, add= TRUE)


# lines(c(0,1)+.83, c(0, max(expected_supply)))
# lines(c(-100,2), c(4000, 4000), col = "red", lty = 2)
# lines(c(.925,.925), c(0, 150000), col = "black", lty = 3)

# PI=matrix(NA, ncol = 2, nrow = 2)
# PI[1,] <- c(4000, 4000)
# PI[2,] <- c(50000, 50000)
# shade(PI, c(-.1, 1.1), col = col.alpha("red",.2))



# ###########################################################################################
# ############################### LOG PLOT ##################################################




# wta <- prefernces$wta_final
# wta <- log10(exp(wta)/2500)

# plot(log10(df_income$forestinc/2500), wta, xlim = c(0, 5), ylim =c(0, 5), col = sapply(c('firebrick','black'), col.alpha, .3)[demographics$controls+1], 
#      xlab = "Annual Forest Income (USD) ", ylab = "WTA (USD)" , xaxt = "n", yaxt = "n",
#      main = "")
# axis(1, at=0:5, labels=c(0, 10^(1:5)))
# axis(2, at=0:5, labels=c(0, 10^(1:5)))
# #rethinking::shade(PI, seq_x)
# lines(c(-100,210), c(log10(4010), log10(4010)), col = "red", lty = 2)
# seq_x <- -4:5
# seq_y <- -4:5
# lines(seq_x, seq_y, lty = 2)
# PI[1,] <- c(log10(4000), log10(4000))
# PI[2,] <- c(6, 6)
# shade(PI, c(-.3, 6), col = col.alpha("red",.2))


# legend("topleft", legend=c( "Control", "REDD+"),
#        fill=c("firebrick", "black", "black"), cex=0.6)



# ##########################################################################################
# #################### CREATE CATEGORIES ###################################################


# cat <- NA

# cat<-ifelse(protest, 1, cat)
# cat<- ifelse((log10(df_income$forestinc/2500) <= wta) & !protest, 2, cat) 
# cat<- ifelse(log10(df_income$forestinc/2500) > wta, 3, cat) 
# table(cat, demographics$controls)




# shehia<-c("tondooni","msuka magharibi","tumbe magharibi","shumba mjini","kifundi","mgogoni","gando","mjini wingwi",
#           "mtambwe kaskazini","mtambwe kusini","fundo","mgelema", "kisiwa panza", "kangani", "changaweni", "kambini", "chumbageni",
#           "shungi", "ziwani", "piki", "ukunjwi", "mjimbini", "michenzani", "kilindi")

# controls<-c("shungi", "ziwani", "piki","ukunjwi", "chumbageni", "kilindi")

# contbin<-ifelse(shehia %in% controls, 1, 0)