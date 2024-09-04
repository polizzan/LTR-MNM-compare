########################################
#---Maternal near miss meta-analysis---# 
#-----for global comparison of the-----# 
#--lifetime risk of maternal near miss-#
#--------------(LTR-MNM)---------------#  
########################################

## --- FILE 100-main.R: INSTALL AND LOAD PACKAGES, RUN ALL OTHER FILES ---

## packages to be installed from cran
from.cran <- c("data.table", "gt", "gtsummary",  "here", "metafor",
               "readxl", "rmarkdown", "tidyverse")

for(i in c(from.cran)){
    
    ## check if installed, else install  
    if(system.file(package = i) == ""){install.packages(i)}
  
    ## load packages    
    library(i, character.only = TRUE)
  
  }

## set path
here::i_am("scripts/100-main.R")

## create output folder
if(!dir.exists(here::here("out"))){dir.create(here::here("out"))}

## create folder for temporary files
if(!dir.exists(here::here("tmp"))){dir.create(here::here("tmp"))}  

## run scripts
source(here::here("scripts", "101-preparation.R")) 
source(here::here("scripts", "102-meta-analysis.R"))
source(here::here("scripts", "199-output.R"))