################################################################################
############# Measurement tools ################################################

### Gini Index

gini <- function(x) {
  
  gini_m <- matrix(ncol = length(x), nrow = length(x))
  
  for (i in 1:nrow(gini_m)){
    for (j in 1:nrow(gini_m)) {
      gini_m[i ,j] <- x[i] - x[j]
      if (i==j) gini_m[i, j] <- NA
    }
  }
  
  gini_m[lower.tri(gini_m)] <- NA
  
  n <- sum(!is.na(gini_m))
  
  gini <- sum(abs(gini_m), na.rm = T)/(2*n*mean(x, na.rm= T))
  
  return(gini)
}

theil <- function(x){ 
  if(any(is.na(x))) warning("NAs found, removing automatically")
  x <- unlist(x)
  sum(x/mean(x, na.rm =T)*log(x/mean(x, na.rm =T)), na.rm =T)/length(x)
}


### Theil decomp


theil_d <- function(x, y, warn = FALSE) {
  if(is.factor(y) != T) warning("y must be a factor. Try using lengthen().")
  if(any(x < 0)) warning("x cannot belower than zero.  All negatives converted to NA")
  x[x < 0] <- NA
  if(warn == TRUE) warning("All zeros must be NA if the boolean is zero. All zeros are converted to NA")
  x[x==0] <- NA
  name <- levels(y)
  y[is.na(x)] <- NA
  nu <-sum(!is.na(x))
  ma <- matrix(ncol = 2, nrow = nu) 
  ma[,1] <- x[!is.na(x)]
  ma[,2] <- y[!is.na(y)]
  
  x <- ma[,1]
  y <-ma[, 2]
  
  t <- sum(x/mean(x)*log(x/mean(x)), na.rm =T)/length(x)
  y <- as.numeric(y)
  n <- length(x)
  u <- mean(x)
  m <- sum(!is.na(unique(y)))
  xj <- rep(NA, m)
  nj <- rep(NA, m)
  for(k in 1:m) nj[k] <- sum(y==k)
  
  for(k in 1:m) xj[k] <- sum(x[y==k], na.rm =T)/nj[k]
  
  si <- rep(NA, m)
  for(k in 1:m) si[k] <- nj[k]/n * xj[k]/u  #This is the population weight
  
  
  sji <- rep(NA, length(x))
  for(i in 1:length(x)) sji[i] <- x[i]/xj[y[i]]
  
  Tjgroup <- rep(NA, m)
  Tjgroup_w <- rep(NA, m)
  l <-list()
  for(i in 1:m){
    a <- as.vector(x[y==i])
    l[[i]] <- a
    Tjgroup[i] <-  1/nj[i]*sum(a/mean(a)*log(a/mean(a)))
    Tjgroup_w[i] <-  Tjgroup[i]*(mean(a)/u)*length(a)/n
  }
  
  
  Tjbetween <- rep(NA, m)
  for(i in 1:m){
    a <- as.vector(x[y==i])
    a <- mean(a)
    Tjbetween[i] <- sum(si[i]*log(a/u))
  }
  
  #name things
  #name things
  Tjgroup<-t(data.frame(Tjgroup))
  Tjbetween<-t(data.frame(Tjbetween))
  Tjgroup_w<-t(data.frame(Tjgroup_w))
  colnames(Tjgroup_w) <- name
  colnames(Tjgroup) <- name
  colnames(Tjbetween) <- name
  output <- list(TheilBet = Tjbetween,
                 TheilGroup = Tjgroup,
                 TheilContribWith = Tjgroup_w,
                 Theil = t)
  return(output)
}


############ THEIL INDEX ELASTICITY

#Theil elasticity
theilElast  <- function(df, range = c(.5, 1.5)) {
  
  output <- matrix(ncol = ncol(df), nrow = 100) 
  seq <- seq(range[1], range[2], length.out = 100) 
  
  for(j in 1:ncol(df)){
    
    df <- df[,order(colnames(df))]
    
    for(i in 1:100){
      df_alt <- df
      
      df_alt[,j] = df_alt[,j]*seq[i] #set how much you will change it by
      
      q <-unlist(df, )
      w <- unlist(df/rowSums(df, na.rm =T))
      
      q1 <- unlist(df_alt, )
      w1 <- unlist(df_alt/rowSums(df_alt, na.rm =T))
      
      
      q[q<0]<-0
      q[q==0]<- NA
      f<- gsub('[[:digit:]]+', '', names(q))
      f[is.na(q)]<- NA
      f <- as.factor(f)
      w[is.na(q)]<- NA
      
      w <- w[complete.cases(w)]
      q <- q[complete.cases(q)]
      f <- f[complete.cases(f)]
      
      
      
      q1[q1<0]<-0
      q1[q1==0]<- NA
      f1<- gsub('[[:digit:]]+', '', names(q1))
      f1[is.na(q1)]<- NA
      f1 <- as.factor(f1)
      w1[is.na(q1)]<- NA
      
      w1 <- w1[complete.cases(w1)]
      q1 <- q1[complete.cases(q1)]
      f1 <- f1[complete.cases(f1)]
      
      g <- theil_d(q, f)
      g1 <- theil_d(q1, f1)
      
      output[i,j]<- (g1$Theil-g$Theil)/g$Theil
      
    }
  }
  colnames(output) <- names(df)
  return(output*100)
}

# Theil Plot

theilPlot <- function(x, add = FALSE, col = "black", id = "x", ylim = NULL, lwd = 1) {
  if (add == FALSE) {plot(seq(-50, 50, length.out = 100), x, type = "l", xlab = paste("% Change in",  id, "income"), lwd = lwd, ylab = "% Change Total Inequality", main =  id, ylim = ylim)
    abline(v = 0, lty = 2, col = "red")
    abline(h = 0, lty = 2, col = "red")
  }
  if(add == TRUE) lines(seq(-50, 50, length.out = 100), x, col = col)
  
}

########### Supply Curves #####################################
# This supply curve is defualted to the proportion of the population
# If you want it to be set to calculate the the raw supply for a vector of length n set prop to false
# If you want to set a price and see the proportion of population, set middle on AND price at the price you want
# otherwise middle defaults to the price needed to buy 50 percent of the population. 
supCurve<-function(x, add = FALSE, 
                   col = "black", 
                   prop = TRUE,
                   main = NULL, 
                   log = NULL,
                   middle = FALSE,
                   xlab = NULL,
                   price = NULL,
                   cex.main = 1,
                   cex = 1,
                   ylim = c(min(x, na.rm=T), max(x, na.rm = T)),
                   xlim = NULL,
                   lwd = 1){
  a <- (1:length(x))/length(x)
  b <- sort(x)
  
  pop <- c()  # saved for later  
  
  #Determine the median
  if(prop == TRUE){
    med <- which.min(abs(b-median(b)))
  }
  
  # Get the limits
  if(is.null(xlim)){
    if (prop == TRUE) {
      xlim <-  c(0, 1.01)
    }else{ 
      xlim  <- c(0, length(x))
    }
  }
  
  # SET UP YLAB
  if(is.null(xlab)) {
    if(prop==TRUE) xlab <- "Proportion of Population" else xlab <- "People"
  }
  if (add == FALSE & is.null(log)) {plot(a , b, xlab = xlab, ylab = "Price", type = 'l',
                                         col = col, ylim = ylim, 
                                         xlim = xlim, main = main,
                                         cex = cex,
                                         cex.main=cex.main,
                                         lwd = lwd) }
  if (add == FALSE & !is.null(log)) {plot(a , b, xlab = "Proportion of Population", ylab = "Price", type = 'l',
                                          col = col, xlim = xlim, ylim = ylim, main = main, log = log,
                                          cex = cex,
                                          cex.main=cex.main,
                                          lwd=lwd) }
  if (add == TRUE) lines(a, b, col = col, lwd=lwd)
  
  if(middle == TRUE){
    if(is.null(price)){
      if(prop == TRUE){
        lines(x=c(0.5, 0.5), y=c(-1000, median(b)), col = "red", lty = 2)
        lines(x =c(-.3, .5), y = c(median(b), median(b)), col = "red", lty = 2)
      }else{
        lines(x=c(mid, mid), y=c(-1000, median(b)), col = "red", lty = 2)
        lines(x =c(-100, mid), y = c(median(b), median(b)), col = "red", lty = 2)
      }
    }else{
      pop = b[which.min(abs(price-a))]
      lines(y = c(price, price), x=c(-1000, pop), col = "red", lty = 2)
      lines(y = c(price, -1000), x =c(pop,pop), col = "red", lty = 2)
    }
  }
  
  if(is.null(price)) out <-median(b) else out <-pop
  if(is.null(price)) names(out) <-"price" else names(out) <- "people"
  
  return(out)
}


#############################
########### ATE SIM #########

# This function takes in two vectors of diffrent length weights them gets them to be the same length and samples draws  
ateSim <- function(a, b){

  weight <-  function(a, b) {
    out <- c()
    out[1] <- (length(a)/(length(a)+length(b)))*2 
    out[2] <- (length(b)/(length(a)+length(b)))*2 
    return(out)
  }
  
  w <- weight(a, b)
  
  c <- sample(a*w[1], min(length(a), length(b)))
  d <- sample(b*w[2], min(length(a), length(b)))
  
  out <- list(a =c, b=d)
  return(out)
}


######### PCA ##############

# B <- apply(df, 2, scale) # or standardize

# prcomp