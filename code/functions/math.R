###################################
########### MATH FUNCTIONS ########


############################
#######Diffrentiation ######


#first write a function down in expression
f = expression(x^4 + 3*x)

# First order derivative with respect to x
D(f, "x")

# Second order derivative
D( D (f, "x"), "x")

# partial derivative
f = expression(4*x^2 + 2*y^3)
D(f, "x")




############################
###### MATRIX DISTANCE #####

#Feed it a matrix of the format: 
#It will give you a matrix of X,X back that contains the distance from one point to each other point
# x <- matrix(1:25, nrow = 5)

distance <- function(x){
  n <- length(x)
  distance <- matrix(ncol = n, nrow = n)
  for(i in 1:n){
    for(j in 1:n){
      cordi <- which(x == i, arr.ind = T)
      cordj <-  which(x == j, arr.ind = T)
      distance[i, j] <-  sqrt(abs(cordi[1]-cordj[1])^2 + abs(cordi[2]-cordj[2])^2)   
    }
  }
  return(distance)
}


############################
##### SIGMOIDAL CURVES #####

sigmoid01 <- function(x, b =.5){
  if(!b > 0) stop("b must be larger than zero")
  1/(1+(x/(1-x))^(-b))
}
###########################
##### LOGISTIC STUFF ######
  
  logit <- function (p) log(p/(1-p))
  inv_logit <- function (x) 1/(1+exp(-x))
  
  
###############################
######### Softmax #############

  softmax <- function(x) {exp(x)/sum(exp(x))}

  
  
###############################
######### Beta rescale #######
  
  ScaleBeta <- function(X){
    a<-0
    b<-1
    y<-X
    Samp<-50
    y2 <- (y-a)/(b-a)
    y3<-(y2*(Samp - 1) + 0.5)/Samp
    y3} 
  
################################
####### DOT PRODUCT ############
  
dot <- function(x, y, norm = TRUE){
  if(norm == TRUE) a <- sum(standardize(x)*standardize(y), na.rm =T)/length(x)
  if(norm == FALSE) a <- sum(standardize(x)*standardize(y), na.rm =T)
  return(a)
}

  
#######################################
###### Standardize nonzeros ###########

standardize2 <- function(x){    
temp <-  x  
temp[temp==0] <- NA
x <- rethinking::standardize(temp)
x[is.na(x)] <- 0
return(x)
}

  
############################################
######### Squared Expoential Kernal #######

#curve(exp(-(abs(x)^2)), .1, 5)
sek <- function(x,y) exp(-(abs(x,y)^2))

#############################################
########### EXPONENTS FOR WEIRD EXPOENT#######


exponent <- function(a, pow) (abs(a)^pow)*sign(a)
