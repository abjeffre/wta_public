#######################################################################
##################### MAKE WTA ########################################

library("gridBase")
library("rethinking")
library(matrixStats)
library(mice)
library(vioplot)
library(readxl)

set_project_wd <- function(dir=""){
  user=Sys.info()[[6]]
  if(user=="jeffrey_andrews") setwd(paste0("C:/Users/jeffrey_andrews/OneDrive/Documents/", dir))
  else if(user=="Jeff") setwd(paste0("C:/Users/Jeff/OneDrive/Documents/", dir))
  else if(user == 'jeffr') setwd(paste0("C:/Users/jeffr/OneDrive/Documents/", dir))
}

set_project_wd()

source("WTA/paths.R")
source(path$load)

###SOURCING####
source("functions/plotting.R")
source("functions/data_manipulation.R")
source("functions/utility.R")
source("functions/graphics.R")
source("functions/measurement.R")
mp<-read_xlsx("forests/data/raw/motivational_payments.xlsx")
forest<- read.csv("WTA/data/AmyDataSummary.csv")
# source("functions/load.R")


#########################
###### MODELS ###########
run = TRUE

source("WTA/code/script/supply.R") 
source("/WTA/code/script/cofma_OC.R") 
source("/WTA/code/script/wta_with_wealth_FI_interaction.R")
source("/WTA/code/script/protest_with_wealth_FI_interaction.R")
source("WTA/code/script/wta_with_wealth.R") 
source("WTA/code/script/protest_with_wealth.R")  


#########################
########### PLOTTING ####

source("WTA/code/plotting/plot_base_protest.R") # This gets inflection point
source("WTA/code/plotting/plotting_cofma.R") # THIS GETS MAIN FIG.
source("WTA/code/plotting/supplyCurve2.R") # This gets interactions

