#####################################################################################
######################### Plotting ##################################################

t2c <- function(x) eval(parse(text=x))

#################################################################################
################### Ordered Logit Cummulative Sum Plot ##########################



cumSum_plot<- function(
  f,  #model
  x,   #main var vector
  seq = NA,
  depth = 1,
  data = post,
  title = "",
  xlab = "",
  ylab = "",
  col = collist,
  xaxt = NULL,
  cex = 1,
  cex.main = 1,
  cex.axis = 1,
  cex.lab = 1
){
  
  t2c <- function(x) eval(parse(text=x))
  if(is.na(seq)) seq <- seq(min(x, na.rm =T), max(x, na.rm =T), length.out = 100)
  r <- range(seq)  
  
  means <- list()
  PIs <- list()
  for (i in 1:ncol(data$cutpoints)){
    c <- paste0("data$cutpoints[, ", i,"]")
    m <- matrix(nrow = nrow(data[[1]]), ncol = length(seq))
    if (depth >1) m <- array(NA, c(nrow = nrow(data[[1]]), ncol = length(seq), 2))
    
    for (j in 1:length(seq)){
      x.seq <- seq[j]
      if(depth ==1 )m[,j] <- inv_logit(t2c(c) - t2c(f))
      if(depth>1)m[,j,] <- inv_logit(t2c(c) - t2c(f))
    }
    
    means[[i]] <- apply(m, 2, mean)
    PIs[[i]] <- apply(m, 2, PI)
  }
  
  plot(NULL, xlim=c(r[1],r[2]), ylim=c(0, 1),
       xlab = xlab,
       ylab = ylab,
       main = title,
       cex.main = cex.main,
       cex.lab = cex.lab,
       cex.axis = cex.axis,
       cex = cex,
       xaxt = xaxt)
  
  
  
  # NOTE I NEED TO MANUALLY ADD IN HIGHER ORDER CUT POINTS
  x.seq <- seq
  
  if(ncol(data$cutpoints)==2){
    # Bottom most plot
    cord.x <- c(x.seq, rev(x.seq))
    cord.y <- c(rep(0, length(seq)), rev(means[[1]]))
    polygon(cord.x,cord.y, col = col.alpha(col[[1]], .35))
    
    #Middle Plot
    cord.x <- c(x.seq, rev(x.seq))
    cord.y <- c(means[[2]], rev(means[[1]]))
    polygon(cord.x,cord.y, col=col.alpha(col[[2]], .35))
    
    # Top Most Plot
    cord.x <- c(x.seq, rev(x.seq))
    cord.y <- c(rep(1, length(seq)), rev(means[[2]]))
    polygon(cord.x,cord.y,col = col.alpha(col[[3]], .35))
    rethinking::shade(PIs[[1]], x.seq, col = col.alpha("black", .35))
    rethinking::shade(PIs[[2]], x.seq, col = col.alpha("black", .35))
    
  }
  
}


###################################################################
################## EXPECTATIONS FOR CUMSUM ########################

EcumSum<- function(
  f,  #model
  x,   #main var vector
  seq = NA,
  vars = NULL,
  data = post
){
  
  
  if(vars != "Only built for three variables - you gotta build it for more" )
    t2c <- function(x) eval(parse(text=x))
  if(is.na(seq)) seq <- seq(min(x, na.rm =T), max(x, na.rm =T), length.out = 100)
  r <- range(seq)  
  data <- data
  
  means <- list()
  temp <- list()
  for (i in 1:ncol(data$cutpoints)){
    c <- paste0("data$cutpoints[, ", i,"]")
    m <- matrix(nrow = nrow(data[[1]]), ncol = length(seq))
    
    for (j in 1:length(seq)){
      x.seq <- seq[j]
      m[,j] <- inv_logit(t2c(c) - t2c(f))
    }
    
    
    temp[[i]] <- m
  }
  means[[1]] <- temp[[1]]
  means[[2]] <- temp[[2]]-temp[[1]]
  means[[3]] <- 1-temp[[2]]
  
  return(means)
  
}


########################################################################################
######################## LINEAR REGRESSION WITH VARYING INTERCEPTS AND SLOPES ##########


mlm_plot <- function(
f,
y,
x,
cat = 4,
seq = NA,
ylab = "",
xlab = "",
title = "",
cols = collist,
standardize = FALSE,
cex = 1,
cex.main = 1,
cex.axis = 1,
cex.lab = 1,
ylim = NULL,
xaxt = FALSE,
yaxt = FALSE
){
warning("post$bX[,...], ... must be equal to j")
t2c <- function(x) eval(parse(text=x))
P = c(NA, NA)
if(standardize == T)P[1] = mean(y)
if(standardize == T)P[2] = sd(y)
if(is.na(seq)) seq = seq(min(x, na.rm = T), max(x, na.rm = T), length.out = 100)
xlim = c(min(seq), max(seq) )

if (is.null(ylim)) ylim = c(min(y), max(y))

if(isFALSE(xaxt) & isFALSE(yaxt)) { 
  plot(y ~ x, xlab = xlab, ylab = ylab, xlim =xlim, ylim = ylim, col = col.alpha(rangi2, .5), main = title, 
     cex.main = cex.main, 
     cex.lab = cex.lab, 
     cex.axis = cex.axis, 
     cex = cex)
}

if(isFALSE(xaxt) & yaxt == "n"){ 
  plot(y ~ x, xlab = xlab, ylab = ylab, xlim =xlim, ylim = ylim, col = col.alpha(rangi2, .5), main = title, 
     cex.main = cex.main, 
     cex.lab = cex.lab, 
     cex.axis = cex.axis, 
     cex = cex,
     yaxt = "n")
}

if(xaxt == "n" & isFALSE(yaxt)){ 
  plot(y ~ x, xlab = xlab, ylab = ylab, xlim =xlim, ylim = ylim, col = col.alpha(rangi2, .5), main = title, 
       cex.main = cex.main, 
       cex.lab = cex.lab, 
       cex.axis = cex.axis, 
       cex = cex,
       xaxt = "n")
}


if(xaxt == "n" & yaxt == "n"){ 
  plot(y ~ x, xlab = xlab, ylab = ylab, xlim =xlim, ylim = ylim, col = col.alpha(rangi2, .5), main = title, 
       cex.main = cex.main, 
       cex.lab = cex.lab, 
       cex.axis = cex.axis, 
       cex = cex,
       xaxt = "n",
       yaxt = "n")
}

  
for (j in 1:cat){
  link <- function(x.seq) t2c(f) #model 
  m <- matrix(ncol = length(seq), nrow = nrow(post[[1]]))
  for (i in 1:length(seq)){
    x.seq <- seq[i]
    m[,i] <- t2c(f)
    if(standardize == T) m[,i] <- m[,i]*P[2]+P[1]
  }
  means <- apply(m, 2, mean)
  PI <- apply(m, 2, PI, prob = .95)
  lines(seq, means, col = cols[j],lwd = 2)
  rethinking:: shade(PI, seq, col = col.alpha(cols[[j]], 0.10))
}

}

#########################################################################################
############### Multiple Density Parameters ############################################

# ENSURE THAT ONLY f[, 1] is recorded in the first 
mlm_par_plot <- function(
f,
par, #number of parameters
xlab = "",
ylab = "",
title = "",
cols = collist,
cex = 1,
cex.main = 1,
cex.axis = 1,
cex.lab = 1,
xlim = NULL,
legend = FALSE
){
  t2c <- function(x) eval(parse(text=x))

ymax<- rep(NA, par)
ymin<- rep(NA, par)
xmax<- rep(NA, par)
xmin<- rep(NA, par)
xmin <- xlim[1]
xmax <- xlim[2]



for (i in 1:par){
a <- density(t2c(f), adjust = 0.5)
ymax[i] <- max(a$y)
ymin[i] <- min(a$y)
if(is.null(xlim)){
xmax[i] <- max(a$x)
xmin[i] <- min(a$x)
}
}
i = 1
dens(t2c(f), xlim =c(min(xmin), max(xmax)), ylim = c(0, max(ymax)), adj =.5, main = title, xlab =xlab, ylab=ylab, 
     cex.main= cex.main,
     cex.lab = cex.lab, 
     cex.axis = cex.axis, 
     cex = cex)

rethinking::shade(density(t2c(f), adj = 0.5), HPDI(t2c(f), prob = .95), col = col.alpha(cols[[1]], 0.5))

a <- density(t2c(f), adjust = 0.5)
b <- which.min(abs(a$x-median(a$x)))


segments(x0 = median(a$x), y0 = 0,  x1 = median(a$x), y1 = a$y[b], lty = 2, col = cols[[1]])
mtext(text = "",
      side = 1, #side 1 = bottom
      line = 2,
      cex = .625)


for(i in c(2:par)){
  dens(t2c(f),  adj =.5, add = T)
  rethinking::shade(density(t2c(f), adj = 0.5), HPDI(t2c(f), prob = .95), col = col.alpha(cols[[i]], 0.5))
  a <- density(t2c(f), adjust = 0.5)
  b <- which.min(abs(a$x-median(a$x)))
  segments(x0 = median(a$x), y0 = 0,  x1 = median(a$x), y1 = a$y[b], lty = 2, col = cols[[i]])
}

if(is.logical(legend)){
  if(legend == TRUE){
  legend("topright", inset=.02, title="Forest Type", 
                         c("High Forest","Mangrove","Coral Rag", "Scrub"), 
                         fill=c(col.alpha("green2", .5), col.alpha("turquoise2", .5), col.alpha("coral2", .5), col.alpha("goldenrod2", .5)),
                         horiz=F, cex=.8)
         
 }
}
}


######################### LEGEND ##############################
 # legend("topright", inset=.02, title="Forest Type", 
 #                c("High Forest","Mangrove","Coral Rag", "Scrub"), 
 #                fill=c(col.alpha("green2", .5), col.alpha("turquoise2", .5), col.alpha("coral2", .5), col.alpha("goldenrod2", .5)),
 #                horiz=F, cex=.8)




####### GENERATE ADJUSTMENT SET TABLES ###########


idenVar <- function(dag, x){
  
  vars <- unique(c(ancestors(dag, x), spouses(dag, x)))
  outcomes <- matrix("", ncol =3, nrow = 2*length(setdiff( vars, x )))
  v1 <-  (setdiff(vars, x ))
  v2<- rep("", length(v1))
  
  r <- rbind(v1,matrix(v2,ncol=length(v2)))
  
  rownames(outcomes) <- c(r)
  colnames(outcomes) <- c("Total Effects", "Direct Effects", "Instrumental Variables")
  
  for( m in setdiff( ancestors( dag, x ), x ) ){
    temp <- setdiff( ancestors( dag, x ), x )
    i <- which(m == rownames(outcomes))+1
    a <- adjustmentSets( dag, outcome =x, exposure = m )
    if( length(a) > 0 ){
      outcomes[m, 1] <- "Yes" 
      if (paste0(a) == "list()"){
        outcomes[i,1] <-"{ }"
      }else{
        if(length(a[[1]])>1){
          temp <- rep("", length(a[[1]]))
          temp <- substring(gsub( "[^,_a-zA-Z\\s]" , "" , as.character(a) , perl = TRUE ),2)
          temp <- strsplit(temp, ", ")
          val <- list()
          for(j in 1:length(temp[[1]]))val[j] <- temp[[1]][j]
          val <- sapply(val, strsplit, "_")
          val <- sapply(val,substring, 1, 1)
          for(j in 1:length(val)) val[[j]] <- paste(val[[j]],collapse = "")
          q <- rep("", length(val))
          for(j in 1:length(val)) q[j] <- val[[j]]  
          val <- paste(q, collapse = ", ")
          outcomes[i,1] <- paste0("{",val,"}")
        }else{
          val <- strsplit(as.character(a), "_")
          val <- sapply(val,substring, 1, 1)
          val <- paste(val, collapse ="")
          outcomes[i,1] <-  paste0("{",val,"}")
        }
      }
      
    }else{
      outcomes[m, 1] <- "No"
    }
    if(length(a) > 1) warning(paste("More than one possible adjustment set for total effects of", m, ": only first printed"))
  }
  
  
  
  
  
  for( m in setdiff( parents( dag, x ), x ) ){
    i <- which(m == rownames(outcomes))+1
    a <- adjustmentSets( dag, outcome =x, exposure = m, effect = "direct" )
    if( length(a) > 0 ){
      outcomes[m, 2] <- "Yes" 
      if (paste0(a) == "list()"){
        outcomes[i,2] <-"{ }"
      }else{
        if(length(a[[1]])>1){
          temp <- rep("", length(a[[1]]))
          temp <- substring(gsub( "[^,_a-zA-Z\\s]" , "" , as.character(a) , perl = TRUE ),2)
          temp <- strsplit(temp, ", ")
          val <- list()
          for(j in 1:length(temp[[1]]))val[j] <- temp[[1]][j]
          val <- sapply(val, strsplit, "_")
          val <- sapply(val,substring, 1, 1)
          for(j in 1:length(val)) val[[j]] <- paste(val[[j]],collapse = "")
          q <- rep("", length(val))
          for(j in 1:length(val)) q[j] <- val[[j]]  
          val <- paste(q, collapse = ", ")
          outcomes[i,2] <- paste0("{",val,"}")
        }else{
          val <- strsplit(as.character(a), "_")
          val <- sapply(val,substring, 1, 1)
          val <- paste(val, collapse ="")
          outcomes[i,2] <-  paste0("{",val,"}")
        }
      }
      
    }else{
      outcomes[m, 2] <- "No"
    }
    if(length(a) > 1) warning(paste("More than one possible adjustment set for total effects of", m, ": only first printed"))
  }
  
  
  
  for( m in setdiff( spouses( dag, x ), x ) ){
    i <- which(m == rownames(outcomes))+1
    a <- instrumentalVariables( dag, outcome =x, exposure = m)
    if( length(a) > 0 ){
      outcomes[m, 3] <- "Yes" 
      if (paste0(a) == "list()"){
        outcomes[i,3] <-"{ }"
      }else{
        if(length(a[[1]])>1){
          b <- gsub( "[^,_a-zA-Z\\s]" , "" , as.character(a[[1]]$I) , perl = TRUE )
          b <- strsplit(as.character(b), "_")
          b <- sapply(b,substring, 1, 1)
          b <- paste(b, collapse ="")
          
          temp <- rep("", length(a[[1]]$Z))
          temp <- gsub( "[^,_a-zA-Z\\s]" , "" , as.character(a[[1]]$Z) , perl = TRUE )
          temp <- strsplit(temp, ", ")
          val <- list()
          for(j in 1:length(temp))val[j] <- temp[[j]]
          val <- sapply(val, strsplit, "_")
          val <- sapply(val,substring, 1, 1)
          for(j in 1:length(val)) val[[j]] <- paste(val[[j]],collapse = "")
          q <- rep("", length(val))
          for(j in 1:length(val)) q[j] <- val[[j]]  
          c <- paste(q, collapse = ", ")
          
          c<- paste(c,collapse=", ")
          outcomes[i,3] <- paste0("{",b, "|", c, "}") 
        }else{
          outcomes[i,3] <-  paste0("{",as.character(a[[1]]), "}")
        }
      }
      
    }else{
      outcomes[m, 3] <- "No"
    }
    if(length(a) > 1) warning(paste("More than one possible IV for total effects of", m, ": only first printed"))
  }
  
  
  for( m in setdiff( parents( dag, x ), x ) ){
    i <- which(m == rownames(outcomes))+1
    a <- instrumentalVariables( dag, outcome =x, exposure = m)
    if( length(a) > 0 ){
      outcomes[m, 3] <- "Yes" 
      if (paste0(a) == "list()"){
        outcomes[i,3] <-"{ }"
      }else{
        if(length(a[[1]])>1){
          b <- gsub( "[^,_a-zA-Z\\s]" , "" , as.character(a[[1]]$I) , perl = TRUE )
          b <- strsplit(as.character(b), "_")
          b <- sapply(b,substring, 1, 1)
          b <- paste(b, collapse ="")
          
          temp <- rep("", length(a[[1]]$Z))
          temp <- gsub( "[^,_a-zA-Z\\s]" , "" , as.character(a[[1]]$Z) , perl = TRUE )
          temp <- strsplit(temp, ", ")
          val <- list()
          for(j in 1:length(temp))val[j] <- temp[[j]]
          val <- sapply(val, strsplit, "_")
          val <- sapply(val,substring, 1, 1)
          for(j in 1:length(val)) val[[j]] <- paste(val[[j]],collapse = "")
          q <- rep("", length(val))
          for(j in 1:length(val)) q[j] <- val[[j]]  
          c <- paste(q, collapse = ", ")
          
          c<- paste(c,collapse=", ")
          outcomes[i,3] <- paste0("{",b, "|", c, "}") 
        }else{
          outcomes[i,3] <-  paste0("{",as.character(a[[1]]), "}")
        }
      }
      
    }else{
      outcomes[m, 3] <- "No"
    }
    if(length(a) > 1) warning(paste("More than one possible IV for total effects of", m, ": only first printed"))
  }
  return(outcomes)
  
}



####################################################
####### LABELING PAR PLOTS #########################


put.fig.letter <- function(label, location="topleft", x=NULL, y=NULL, 
                           offset=c(0, 0), ...) {
  if(length(label) > 1) {
    warning("length(label) > 1, using label[1]")
  }
  if(is.null(x) | is.null(y)) {
    coords <- switch(location,
                     topleft = c(0.1,0.90),
                     topcenter = c(0.3025,0.90),
                     topright = c(0.5685, 0.90),
                     bottomleft = c(0.1, 0.10), 
                     bottomcenter = c(0.1, 0.10), 
                     bottomright = c(0.5655, 0.10),
                     c(0.10, 0.5655) )
  } else {
    coords <- c(x,y)
  }
  this.x <- grconvertX(coords[1] + offset[1], from="nfc", to="user")
  this.y <- grconvertY(coords[2] + offset[2], from="nfc", to="user")
  text(labels=label[1], x=this.x, y=this.y, xpd=T, ...)
}


my.locations <- c("topleft", "topcenter", "topright",
                  "bottomleft", "bottomcenter", "bottomright")




###########################################################################
####################### VIEW PORT SETTINGS ################################

library(gridBase)
library(grid)

vp_master <- list()
fourbytwo <- list()
threebythree <- list()
 
vp.BottomRight <- viewport(height=unit(.40, "npc"), width=unit(0.25, "npc"), 
                           just=c("left","top"), 
                           y=0.45, x=0.75)


vp.TopRight <- viewport(height=unit(.40, "npc"), width=unit(0.25, "npc"), 
                        just=c("left","top"), 
                        y=.90, x=0.75)

fourbytwo[[1]] <- vp.BottomRight 
fourbytwo[[2]] <- vp.TopRight 

vp_master[[1]] <- fourbytwo 


vp.BottomRight <- viewport(height=unit(.2666, "npc"), width=unit(0.33, "npc"), 
                           just=c("left","top"), 
                           y=0.3, x=0.66)



vp.CenterRight <- viewport(height=unit(.266, "npc"), width=unit(0.33, "npc"), 
                           just=c("left","top"), 
                           y=.6, x=0.66)


vp.TopRight <- viewport(height=unit(.266, "npc"), width=unit(0.33, "npc"), 
                        just=c("left","top"), 
                        y=.90, x=0.66)


threebythree[[1]] <- vp.BottomRight 
threebythree[[2]] <- vp.CenterRight 
threebythree[[3]] <- vp.TopRight 

vp_master[[2]] <- threebythree 


vp.1of2 <- viewport(height=unit(.9, "npc"), width=unit(0.45, "npc"), 
                    just=c("left","top"), 
                    y=.9, x=0.1)


vp.2of2 <- viewport(height=unit(.9, "npc"), width=unit(0.45, "npc"), 
                    just=c("left","top"), 
                    y=.9, x=0.5)


# Save ggplot_file and then push it through view ports
# plot.new()              
# print(ggplot_file ,vp = vp.TopRight)



############################################################################
######################## DRAW DAG ##########################################


drawdag2 <- function (x, col_arrow = "black", col_segment = "black", col_labels = "black", 
          cex = 1, lwd = 1.5, goodarrow = TRUE, xlim, ylim, main = "", ...) 
{
  require(dagitty)
  x <- as.dagitty(x)
  dagitty:::.supportsTypes(x, c("dag", "mag", "pdag"))
  coords <- coordinates(x)
  if (any(!is.finite(coords$x) | !is.finite(coords$y))) {
    stop("Please supply plot coordinates for graph! See ?coordinates and ?graphLayout.")
  }
  labels <- names(coords$x)
  par(mar = rep(0, 4))
  plot.new()
  par(new = TRUE)
  wx <- sapply(paste0("mm", labels), function(s) strwidth(s, 
                                                          units = "inches"))
  wy <- sapply(paste0("\n", labels), function(s) strheight(s, 
                                                           units = "inches"))
  ppi.x <- dev.size("in")[1]/(max(coords$x) - min(coords$x))
  ppi.y <- dev.size("in")[2]/(max(coords$y) - min(coords$y))
  wx <- wx/ppi.x
  wy <- wy/ppi.y
  if (missing(xlim)) 
    xlim <- c(min(coords$x - wx/2), max(coords$x + wx/2))
  if (missing(ylim)) 
    ylim <- c(-max(coords$y + wy/2), -min(coords$y - wy/2))
  par(mfrow =c(1,1), oma = c(0, 0, 2, 0))
  plot(NA, xlim = xlim, ylim = ylim, xlab = "", ylab = "", 
       bty = "n", xaxt = "n", yaxt = "n")
  mtext(main, outer = TRUE, cex = 1)
  
  wx <- sapply(labels, function(s) strwidth(paste0("xx", s)))
  wy <- sapply(labels, function(s) strheight(paste0("\n", s)))
  asp <- par("pin")[1]/diff(par("usr")[1:2])/(par("pin")[2]/diff(par("usr")[3:4]))
  ex <- edges(x)
  ax1 <- rep(0, nrow(ex))
  ax2 <- rep(0, nrow(ex))
  ay1 <- rep(0, nrow(ex))
  ay2 <- rep(0, nrow(ex))
  axc <- rep(0, nrow(ex))
  ayc <- rep(0, nrow(ex))
  acode <- rep(2, nrow(ex))
  has.control.point <- rep(FALSE, nrow(ex))
  for (i in seq_len(nrow(ex))) {
    if (ex[i, 3] == "<->") {
      acode[i] <- 3
      has.control.point[i] <- TRUE
    }
    if (ex[i, 3] == "--") {
      acode[i] <- 0
    }
    l1 <- as.character(ex[i, 1])
    l2 <- as.character(ex[i, 2])
    x1 <- coords$x[l1]
    y1 <- coords$y[l1]
    x2 <- coords$x[l2]
    y2 <- coords$y[l2]
    if (is.na(ex[i, 4]) || is.na(ex[i, 5])) {
      cp <- dagitty:::.autoControlPoint(x1, y1, x2, y2, 
                                        asp, 0.2 * as.integer(acode[i] == 3))
    }
    else {
      cp <- list(x = ex[i, 4], y = ex[i, 5])
      has.control.point[i] <- TRUE
    }
    bi1 <- dagitty:::.lineSegBoxIntersect(x1 - wx[l1]/2, 
                                          y1 - wy[l1]/2, x1 + wx[l1]/2, y1 + wy[l1]/2, x1, 
                                          y1, cp$x, cp$y)
    bi2 <- dagitty:::.lineSegBoxIntersect(x2 - wx[l2]/2, 
                                          y2 - wy[l2]/2, x2 + wx[l2]/2, y2 + wy[l2]/2, cp$x, 
                                          cp$y, x2, y2)
    if (length(bi1) == 2) {
      x1 <- bi1$x
      y1 <- bi1$y
    }
    if (length(bi2) == 2) {
      x2 <- bi2$x
      y2 <- bi2$y
    }
    ax1[i] <- x1
    ax2[i] <- x2
    ay1[i] <- y1
    ay2[i] <- y2
    axc[i] <- cp$x
    ayc[i] <- cp$y
  }
  directed <- acode == 2 & !has.control.point
  undirected <- acode == 0 & !has.control.point
  if (goodarrow == TRUE) {
    require(shape)
    shape::Arrows(ax1[directed], -ay1[directed], ax2[directed], 
                  -ay2[directed], arr.length = 0.2, arr.width = 0.15, 
                  col = col_arrow, lwd = lwd, arr.adj = 1, arr.type = "curved")
  }
  else arrows(ax1[directed], -ay1[directed], ax2[directed], 
              -ay2[directed], length = 0.1, col = col_arrow, lwd = lwd)
  segments(ax1[undirected], -ay1[undirected], ax2[undirected], 
           -ay2[undirected], col = col_segment, lwd = lwd)
  for (i in which(has.control.point)) {
    dagitty:::.arc(ax1[i], -ay1[i], ax2[i], -ay2[i], axc[i], 
                   -ayc[i], col = c(col_arrow, col_segment)[1 + (acode[i] == 
                                                                   0)], code = acode[i], length = 0.1, lwd = 1 + 
                     (acode[i] == 0))
  }
  text(coords$x, -coords$y[labels], labels, cex = cex, col = col_labels)
}













########################################################################################
##################### Hurdle Data Prep #################################################


hurdPrep <- function(f1, f2, par, data, set = 1, prep = TRUE) {
  i = 1
  t2c <- function(x) eval(parse(text=x))
  n1 <- length(t2c(f1))
  n2 <- length(t2c(f2))
  
  joy <- matrix(nrow= n1, ncol =par)
  joy2 <- matrix(nrow=n2, ncol =par)
  
  for(i in 1:par){
    joy[, i] <- t2c(f1) 
    joy2[, i] <- t2c(f2) 
  }
  
  joy <- lengthen(joy)
  joy2 <- lengthen(joy2)
  df <- rbind(joy, joy2)
  
  for(j in 1:nrow(df)) df$type[j] <- ifelse(j <= n1*par, 1, 2)
  
  df$group <- as.factor(df$group)
  df$type <- as.factor(df$type)
  df$set <- as.factor(set)
  
  if(prep == TRUE) return(df)
  
  if(prep == FALSE){
    # Draw plot:
    df %>% 
      ggplot2::ggplot(aes(y = group)) +
      ggridges::geom_density_ridges2(aes(x = value, 
                                         colour = "black", 
                                         fill = type),
                                     scale = .7,
                                     alpha = 0.6, 
                                     size = 0.25)
  }
}




####################################################################################
##################### Joy plotting #################################################



hurdJoy <- function(data, xlim = c(-1,1)) {
  df<- do.call("rbind", data)
  
  ridgeplot <- df %>%
    ggplot(aes(y = group)) +
    geom_density_ridges(
      aes(x = value, fill = paste(group, type)), 
      color = "white", from = xlim[1], to = xlim[2]
    ) +
    scale_fill_cyclical(
      values =collist_master[[4]]
    ) +
    theme_bw() + 
    facet_wrap(~set, nrow = 3, ncol = 1, strip.position = "left") +  
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) +
    theme(strip.text.x = element_text(size = 8), strip.background = element_rect( color="white", fill="white", size=.5, linetype="solid")) +
    theme(panel.spacing = unit(.01, "lines")) + geom_vline(xintercept = 0) + 
    theme(panel.spacing = unit(.01, "lines")) + geom_vline(xintercept = 0)
  
  return(ridgeplot)
}



# NORMAL JOINING MECHANISM LOOKS SOMETHINK LIKE THIS  
# joy <- list()
# count <- 1
# for (i in c(4, 5, 6)){ 
#   post <- pst[[i]]
#   sets <- c("Fuel", "Timber", "NTFP")
#   joy[[count]] <- hurdPrep(f1 ="post$bma[, i]*post$sigma_3+post$mu_ma", f2 = "post$bpa[,i]*post$sigma_2+post$mu_pa", par = 4,  data = post, 
#                            set = sets[count], prep = TRUE)  
#   count <- count + 1
# }

#hurdJoy(joy) 




#####################################################################
################### CONTRAST PLOTTING ############################## 

joyPrep <- function(flist, par, data, set = 1, plot = FALSE) {
  i = 1
  t2c <- function(x) eval(parse(text=x))
  l <- length(flist)
  n <- unlist(lapply(flist, function(x) length(t2c(x))))
  
  joy <- list()
  for (j in 1:l)joy[[j]] <- matrix(nrow= n[j], ncol =par)
  
  for (j in 1:l){
    for(i in 1:par){
      joy[[j]][, i] <- t2c(flist[[j]]) 
    }
  }
  
  temp<- lapply(joy, lengthen)
  for(j in 1:l) temp[[j]][,par+1] <- j
  df <- do.call("rbind", temp)
  
  df$group <- as.factor(df$group)
  df$subgroup <- as.factor(df$V3)
  df$set <- as.factor(set)
  df <- df[, c("group", "subgroup", "value", "set")]
  if(plot == FALSE) return(df)
  
  if(plot == TRUE){
    # Draw plot:
    df %>% 
      ggplot2::ggplot(aes(y = subgroup)) +
      ggridges::geom_density_ridges2(aes(x = value, 
                                         colour = "black", 
                                         fill = group),
                                     scale = .7,
                                     alpha = 0.6, 
                                     size = 0.25)
  }
}

#########################################################
############# CONTRAST PLOTTING #########################



contJoy <- function(data, xlim = c(-1,1), col = collist2, title = "") {
  df<- do.call("rbind", data)
  
  ridgeplot <- df %>%
    ggplot(aes(y = subgroup)) +
    geom_density_ridges(
      aes(x = value, fill = paste(group, subgroup)), 
      color = "white", from = xlim[1], to = xlim[2]
    ) +
    scale_fill_cyclical(
      values =col,
      name = "Legend", guide = "legend"
    ) +
    theme_bw() + 
    facet_wrap(~set, nrow = 3, ncol = 1, strip.position = "left") +  
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) +
    theme(strip.text.x = element_text(size = 8), strip.background = element_rect( color="white", fill="white", size=.5, linetype="solid")) +
    theme(panel.spacing = unit(.01, "lines")) + geom_vline(xintercept = 0) + 
    theme(panel.spacing = unit(.01, "lines")) + geom_vline(xintercept = 0)+
    ggtitle(title)
  
  return(ridgeplot)
}


#####################################################
############# COMPARE 2 with file names #############


##############################################################
# COmpare 2 with names #

compare2 <-function (..., n = 1000, sort = "WAIC", func = WAIC, WAIC = TRUE, 
                     refresh = 0, warn = TRUE, result_order = c(1, 5, 3, 6, 2, 4), mnames = NULL) 
{
  L <- list(...)
  if (is.list(L[[1]]) && length(L) == 1) 
    L <- L[[1]]
  if (length(L) == 1) 
    stop("Need more than one model to compare.")
  if(is.null(mnames)){
    mnames <- match.call()
    mnames <- as.character(mnames)[2:(length(L) + 1)]
  }
  mnames <- as.character(mnames)
  the_func <- func
  if (class(the_func) != "character") 
    the_func <- deparse(substitute(func))
  classes <- as.character(sapply(L, class))
  if (any(classes != classes[1]) & warn == TRUE) {
    warning("Not all model fits of same class.\nThis is usually a bad idea, because it implies they were fit by different algorithms.\nCheck yourself, before you wreck yourself.")
  }
  nobs_list <- try(sapply(L, nobs))
  if (any(nobs_list != nobs_list[1]) & warn == TRUE) {
    nobs_out <- paste(mnames, nobs_list, "\n")
    nobs_out <- concat(nobs_out)
    warning(concat("Different numbers of observations found for at least two models.\nModel comparison is valid only for models fit to exactly the same observations.\nNumber of observations for each model:\n", 
                   nobs_out))
  }
  dSE.matrix <- matrix(NA, nrow = length(L), ncol = length(L))
  if (WAIC == FALSE) 
    func <- DIC
  if (the_func == "DIC") {
    IC.list <- lapply(L, function(z) DIC(z, n = n))
    p.list <- sapply(IC.list, function(x) attr(x, "pD"))
  }
  if (the_func %in% c("WAIC", "PSIS", "LOO")) {
    IC.list.pw <- lapply(L, function(z) do.call(the_func, 
                                                list(z, n = n, refresh = refresh, pointwise = TRUE, 
                                                     warn = warn)))
    p.list <- sapply(IC.list.pw, function(x) sum(x$penalty))
    se.list <- sapply(IC.list.pw, function(x) x$std_err[1])
    IC.list <- sapply(IC.list.pw, function(x) sum(x[[1]]))
    colnames(dSE.matrix) <- mnames
    rownames(dSE.matrix) <- mnames
    for (i in 1:(length(L) - 1)) {
      for (j in (i + 1):length(L)) {
        ic_ptw1 <- IC.list.pw[[i]][[1]]
        ic_ptw2 <- IC.list.pw[[j]][[1]]
        dSE.matrix[i, j] <- as.numeric(sqrt(length(ic_ptw1) * 
                                              var(ic_ptw1 - ic_ptw2)))
        dSE.matrix[j, i] <- dSE.matrix[i, j]
      }
    }
  }
  if (!(the_func %in% c("DIC", "WAIC", "LOO", 
                        "PSIS"))) {
    IC.list <- lapply(L, function(z) func(z))
  }
  IC.list <- unlist(IC.list)
  dIC <- IC.list - min(IC.list)
  w.IC <- ICweights(IC.list)
  if (the_func == "DIC") 
    result <- data.frame(DIC = IC.list, pD = p.list, dDIC = dIC, 
                         weight = w.IC)
  if (the_func == "WAIC") {
    topm <- which(dIC == 0)
    dSEcol <- dSE.matrix[, topm]
    result <- data.frame(WAIC = IC.list, pWAIC = p.list, 
                         dWAIC = dIC, weight = w.IC, SE = se.list, dSE = dSEcol)
  }
  if (the_func == "LOO" | the_func == "PSIS") {
    topm <- which(dIC == 0)
    dSEcol <- dSE.matrix[, topm]
    result <- data.frame(PSIS = IC.list, pPSIS = p.list, 
                         dPSIS = dIC, weight = w.IC, SE = se.list, dSE = dSEcol)
  }
  if (!(the_func %in% c("DIC", "WAIC", "LOO", 
                        "PSIS"))) {
    result <- data.frame(IC = IC.list, dIC = dIC, weight = w.IC)
  }
  rownames(result) <- mnames
  if (!is.null(sort)) {
    if (sort != FALSE) {
      if (the_func == "LOO") 
        the_func <- "PSIS"
      if (sort == "WAIC") 
        sort <- the_func
      result <- result[order(result[[sort]]), ]
    }
  }
  result <- result[, result_order]
  new("compareIC", result, dSE = dSE.matrix)
}



