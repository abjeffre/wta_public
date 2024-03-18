#################################################
######### GRAPHICS FUNCTIONS ####################

# Generate a plot of color names which R knows about.
#++++++++++++++++++++++++++++++++++++++++++++
# cl : a vector of colors to plots
# bg: background of the plot
# rot: text rotation angle
#usage=showCols(bg="gray33")
showCols <- function(cl=colors(), bg = "grey",
                     cex = 0.75, rot = 30) {
  m <- ceiling(sqrt(n <-length(cl)))
  length(cl) <- m*m; cm <- matrix(cl, m)
  require("grid")
  grid.newpage(); vp <- viewport(w = .92, h = .92)
  grid.rect(gp=gpar(fill=bg))
  grid.text(cm, x = col(cm)/m, y = rev(row(cm))/m, rot = rot,
            vp=vp, gp=gpar(cex = cex, col = cm))
}

### Custom Color Palettes

collist_master <- list()
collist_master[[1]] <- c("Goldenrod2", "Coral2", "Turquoise2", "green2")
collist_master[[2]] <- c("red", "goldenrod", "black")
collist_master[[3]] <- c("deeppink4", "goldenrod4", "green4")
collist_master[[4]] <- c(col.alpha("goldenrod2", .8), col.alpha("goldenrod2", .4),
              col.alpha("coral2", .8), col.alpha("coral2", .4),
              col.alpha("turquoise2", .8), col.alpha("turquoise2", .4),
              col.alpha("green2", .8), col.alpha("green2", .4)) 
collist_master[[5]] <- c(col.alpha("dodgerblue2", .8), col.alpha("firebrick4", .8),
                         col.alpha("mediumseagreen", .8), col.alpha("dodgerblue2", .4),
                         col.alpha("firebrick4", .4), col.alpha("mediumseagreen", .4))


collist_master[[6]] <- c("deeppink2", "springgreen2", "orange2")

#show custom(color)
showCollist <- function(dl = collist_master, alpha = 1){
  errorCondition(!is.list(dl))

  nl <- length(dl)  
  
  par(mfrow =c(3,3))
  
  for (i in (1:nl)){
   t <- paste(i)
   n<- length(dl[[i]])
   cl<- unlist(collist_master[[i]])
   b <- rep(1,n)
  if (alpha != 1){
    acol <- col2rgb(cl)
    for (j in 1:ncol(acol)) cl[j] <- rgb(acol[1, j]/255, acol[2, j]/255,  acol[3, j]/255, alpha)
    }
   barplot(b, col = cl, main = t)
   }
}

#showCollist()
#showCollist(alpha = .35)


#Colalpha

col.alpha <- function (acol, alpha = 0.2) 
{
  acol <- col2rgb(acol)
  acol <- rgb(acol[1]/255, acol[2]/255, acol[3]/255, alpha)
  acol
}


dens2 <- function (x, adj = 0.5, norm.comp = FALSE, main = "", show.HPDI = FALSE, 
          show.zero = FALSE, rm.na = TRUE, add = FALSE, ...) 
{
  if (inherits(x, "data.frame")) {
    n <- ncol(x)
    cnames <- colnames(x)
    set_nice_margins()
    par(mfrow = make.grid(n))
    for (i in 1:n) {
      dens(x[, i], adj = adj, norm.comp = norm.comp, show.HPDI = show.HPDI, 
           show.zero = TRUE, xlab = cnames[i], ...)
    }
  }
  else {
    if (rm.na == TRUE) 
      x <- x[!is.na(x)]
    thed <- density(x, adjust = adj)
    if (add == FALSE) {
      set_nice_margins()
      plot(thed, main = main, ...)
    }
    else lines(thed$x, thed$y, ...)
    if (show.HPDI != FALSE) {
      hpd <- HPDI(x, prob = show.HPDI)
      shade(thed, hpd , col = col.alpha("green", .2))
    }
    if (norm.comp == TRUE) {
      mu <- mean(x)
      sigma <- sd(x)
      curve(dnorm(x, mu, sigma), col = "white", lwd = 2, 
            add = TRUE)
      curve(dnorm(x, mu, sigma), add = TRUE)
    }
    if (show.zero == TRUE) {
      lines(c(0, 0), c(0, max(thed$y) * 2), lty = 2)
    }
  }
}
