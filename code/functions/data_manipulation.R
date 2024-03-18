########################################################## 
################## DATA MANIPULATION #####################


# The most basic wide to long transformation.
# Only takes df of x rows and makes it long 
# ideal for ridges


lengthen <- function(df){
  n <- length(as.matrix(df))
  m <- matrix(ncol = 2, nrow= n)
  m[,1] <- as.vector(as.matrix(df))
  m[,2] <- rep(1:ncol(df), each = nrow(df))
  
  m <- as.data.frame(m)
  colnames(m) <- c("value", "group")
  return(m)
  
}


##############################################################################
####################### SMOOOTHED ############################################

smooth_custom <- function(x, alpha){
  smoothed = rep(NA, length(x))
  for(i in 2:length(x)){
    smoothed[1] <- x[1]
    smoothed[i] <- x[i]*alpha + (1-alpha)*smoothed[i-1]
  }
  return(smoothed)
}


getts<-function(dataTimes, dataValues, maxtime=NULL){
  # Max a full of total values at date
  if(is.null(maxtime)){
    t <- max(dataTimes, na.rm = T)  
  }else{
    t <- maxtime
  }
  v <- as.data.frame(matrix(0,nrow = t, ncol=3))
  names(v) <- c("time", "value", "smoothed")
  v[,1]=1:t
  for(i in 1:t){
    v[i,"value"]=sum(dataValues[dataTimes==i], na.rm = T)
  }
  return(v)
}
