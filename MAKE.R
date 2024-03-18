#######################################################################
##################### MAKE WTA ########################################

library("gridBase")
library("rethinking")
library(matrixStats)
library(mice)
library(vioplot)


set_project_wd <- function(dir=""){
  user=Sys.info()[[6]]
  if(user=="jeffrey_andrews") setwd(paste0("C:/Users/jeffrey_andrews/OneDrive/Documents/", dir))
  else if(user=="Jeff") setwd(paste0("C:/Users/Jeff/OneDrive/Documents/", dir))
  else if(user == 'jeffr') setwd(paste0("C:/Users/jeffr/OneDrive/Documents/", dir))
}

set_project_wd()


# LOAD DATA
demographics <- readRDS("data/demographics.rds")
farming <- readRDS("data/farming.rds")
forestproducts <- readRDS("data/forestproducts.rds")
agroforestry <- readRDS("data/agroforestry.rds")
df_income <- readRDS("data/df_income.rds")
land <- readRDS("data/land.rds")
clearance <- readRDS("data/clearance.rds")
df_forestIncP <- readRDS("data/df_forestIncP.rds")
df_forestIncA <- readRDS("data/df_forestIncA.rds")
secondaryprod <- readRDS("data/secondaryprod.rds")
wealth <- readRDS("data/wealth.rds")
trees <- readRDS("data/trees.rds")
conservation <- readRDS("data/conservation.rds")
prefernces <- readRDS("data/prefernces.rds")
income <- readRDS("data/income.rds")
shocks <- readRDS("data/shocks.rds")
livestock <- readRDS("data/livestock.rds")
choices_long <- readRDS("data/choices_long.rds")
df_income <- readRDS("data/df_income.rds")




###SOURCING####
source("code/functions/plotting.R")
source("code/functions/data_manipulation.R")
source("code/functions/utility.R")
source("code/functions/graphics.R")
source("code/functions/measurement.R")
mp<-read_xlsx("data/raw/motivational_payments.xlsx")
forest<- read.csv("data/forest_data.csv")

#########################
###### MODELS ###########
run = TRUE

source("WTA/code/script/supply.R") # Get base CURVES FOR INFLECTION PLOTS AND CALIBRATION
source("/WTA/code/script/cofma_OC.R") # Get BASE ESTIMATION FOR FIG 5
source("/WTA/code/script/wta_with_wealth_FI_interaction.R")
source("/WTA/code/script/protest_with_wealth_FI_interaction.R")
source("WTA/code/script/wta_with_wealth.R") # GET PROETST FOR MAIN FIG 5. 
source("WTA/code/script/protest_with_wealth.R") # GET PROETST FOR MAIN FIG 5. 
source("WTA/code/script/payment_type.R") # GET PROETST FOR MAIN FIG 5. 


#########################
########### PLOTTING ####

source("WTA/code/plotting/plot_base_protest.R") # This gets inflection point
source("WTA/code/plotting/plotting_cofma.R") # THIS GETS MAIN FIG.
source("WTA/code/plotting/supplyCurve2.R") # This gets interactions
source("WTA/code/plotting/plot_payment_type.R") # This gets interactions

