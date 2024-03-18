#####################################################################
############# PLOT POLICY PROTEST BIDS ##############################

intercept <- readRDS(paste0(path$data, "wta_intercept.rds"))
#######################################################################
################## SET UP FILE DIRECTORY #############################


set_project_wd <- function(dir=""){
  user=Sys.info()[[6]]
  if(user=="jeffrey_andrews") setwd(paste0("C:/Users/jeffrey_andrews/OneDrive/Documents/", dir))
  else if(user=="Jeff") setwd(paste0("C:/Users/Jeff/OneDrive/Documents/", dir))
  else if(user == 'jeffr') setwd(paste0("C:/Users/jeffr/OneDrive/Documents/", dir))
}

set_project_wd()


post <- extract.samples(intercept)
sup <- function(){
  mu<-rnorm(1000, post$a[,1] + rowMedians(post$bS[,1,]*post$sigma_S[,1]) + rowMedians(post$bE[,1,]*post$sigma_e[,1]), post$sigma)
  # mu <- rnorm(1000, mu, post$sigma)
  p <- rbinom(1000, 1, inv_logit(post$a[,2] + rowMedians(post$bS[,2,]*post$sigma_S[,2]) + rowMedians(post$bE[,2,]*post$sigma_e[,2])))
  out<-mu*p
  out<-exp(out)/2500
  return(out)
}

set.seed(2)
Supply<-replicate(500,sup())

#Get average
out <- c()
for(i in 1:500){
  a <-sort(Supply[,i])
  out<- cbind(out, a)
}


pdf("WTA/figures/protest.pdf", height = 4, width = 8)
par(mfrow = c(1, 2), mar = c(5, 4,3,1))

expected_supply <- rowMedians(out)
supCurve(expected_supply, col = col.alpha("black", 0.5), prop = TRUE, ylim = c(0,median(apply(Supply, 2, max))))
a<-exp(prefernces$wta_final)/2500
a<-a[!is.na(a)]
supCurve(a, col = col.alpha("black", 1), prop = TRUE, add= T,  lwd = 3)

for(i in 2:500){
  supCurve(Supply[,i], col = col.alpha("grey", 0.05), add = TRUE, prop = TRUE)
}



lines(c(0,1)+.83, c(0, max(expected_supply)), lw = 2, col = "red")
lines(c(-100,2), c(4000, 4000), col = "red", lty = 2)
lines(c(.927,.927), c(0, 150000), col = "black", lty = 3)

PI=matrix(NA, ncol = 2, nrow = 2)
PI[1,] <- c(4000, 4000)
PI[2,] <- c(50000, 50000)
#shade(PI, c(-.1, 1.1), col = col.alpha("red",.2))

a <-exp(prefernces$wta_final[demographics$controls==0])/2500
a<-a[!is.na(a)]
b <-exp(prefernces$wta_final[demographics$controls==1])/2500
b<-b[!is.na(b)]
supCurve(a, lw = 3, col = "firebrick", ylim = c(0,median(apply(Supply, 2, max))))
supCurve(b, add = TRUE, lw = 3, col = "dodgerblue")
lines(c(-100,2), c(4000, 4000), col = "red", lty = 2)

dev.off()