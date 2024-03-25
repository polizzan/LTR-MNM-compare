#################################
#--Simulation & calculation of--# 
#-------the lifetime risk-------# 
#-----of maternal near miss-----#
#-----------(LTR-MNM)-----------#  
#################################

## --- FILE 200-main.R: INSTALL AND LOAD PACKAGES, RUN ALL OTHER FILES ---

## packages to be installed from cran
from.cran <- c("biscale", "cowplot", "data.table",  
               "ggsci", "gt", "gtsummary", "here", "paletteer",  
               "rmarkdown", "rnaturalearth", "scales", "sf", "sp", 
               "tidyverse", "viridis")

for(i in c(from.cran)){
  
    ## check if installed, else install
    if(system.file(package = i) == ""){install.packages(i)}

    ## load packages    
    library(i, character.only = TRUE)
  
  }

## set path
here::i_am("scripts/200-main.R")

## pull available UNWPP country codes
unwpp.codes <-
  read_csv(here::here("data", "WPP2022_Demographic_Indicators_Medium.zip")) %>% 
  pull(ISO3_code) %>% 
  unique()

## read MNM ratio information
mnm_data <-  
  read_csv(here::here("tmp", "meta-results_110324.csv")) %>% 
  filter(ISO %in% unwpp.codes)

## read MMR information 
mmr_data <- 
  read_csv(here::here("data", "who-mmr-estimates.csv")) %>% 
          filter(parameter == "mmr") %>% 
          select(mmr = `0.5`, 
                 ISO = iso_alpha_3_code, 
                 YR = year_mid)

## merge MNM and MMR info
data <- left_join(mnm_data, mmr_data, by = c("ISO", "YR"))

## define age groups (five-year intervals)
age <- c("15-19", 
         "20-24", 
         "25-29", 
         "30-34", 
         "35-39", 
         "40-44", 
         "45-49")

## load and combine unwpp data
WPP.mort <- 
  read_csv(here::here("data", "WPP2022_Life_Table_Abridged_Medium_1950-2021.zip")) %>% 
  filter(!is.na(ISO3_code), Sex == "Female") %>% 
  select(ISO3_code, Time, AgeGrp, Lx, lx)
  
WPP.fert <- 
  read_csv(here::here("data", "WPP2022_Fertility_by_Age5.zip")) %>% 
  filter(!is.na(ISO3_code)) %>% 
  select(ISO3_code, Time, AgeGrp, ASFR, Births)

srb <- 
  read_csv(here::here("data", "WPP2022_Demographic_Indicators_Medium.zip")) %>% 
  filter(!is.na(ISO3_code)) %>% 
  select(ISO3_code, Time, SRB)

nrr <- 
  read_csv(here::here("data", "WPP2022_Demographic_Indicators_Medium.zip")) %>% 
  filter(!is.na(ISO3_code)) %>% 
  select(ISO3_code, Time, NRR)

tfr <- 
  read_csv(here::here("data", "WPP2022_Demographic_Indicators_Medium.zip")) %>% 
  filter(!is.na(ISO3_code)) %>% 
  select(ISO3_code, Time, TFR)

WPP.combined <-
  WPP.mort %>% 
  left_join(WPP.fert) %>% 
  left_join(srb) %>% 
  left_join(nrr) %>% 
  left_join(tfr) %>% 
  filter(AgeGrp %in% age) %>% 
  filter(ISO3_code %in% unique(data$ISO))

## save in temporary folder
save(WPP.combined,  file = here::here("tmp", "WPP-data.RData"))

## turn input data into list
LTR.MNM.international <-
  unname(split(data, data$row_number))

## main and sensitivity analysis
walk(c("Pooled_Ratio_adjusted", "Pooled_Ratio_unadjusted"), function(y){
  ## loop over all countries
  walk(LTR.MNM.international, function(x){
    
    ## load unwpp input data
    load(here::here("tmp", "WPP-data.RData"))
    
    ## create indicator
    ind <- ifelse(grepl("unadjusted", y), "unadjusted", "adjusted")
    
    ## select country (ISO code) and year
    CNTRY <- x$ISO
    YR <- x$YR
    
    ## SDG region
    region <- x$region
    
    ## country 
    country <- x$country
    
    ## assumed total MNM ratio
    mnm.ratio <- x[[paste(y)]]
    
    ## upper and lower bounds of MNM ratio
    mnm.ratio.lower <- x$Ratio_lower
    mnm.ratio.upper <- x$Ratio_upper
    
    ## assumed total maternal mortality ratio 
    md.ratio <- x$mmr
    
    ## assumed number of stillbirths per 1,000
    stillbirths <- x$stillbirth
    
    ## generate country folder
    if(!dir.exists(here::here("tmp", paste0(CNTRY, "-", YR, "-", ind)))){dir.create(here::here("tmp", paste0(CNTRY, "-", YR, "-", ind)))}
    
    ## save environment
    save(CNTRY, country, YR, region, mnm.ratio, 
         mnm.ratio.lower, mnm.ratio.upper, md.ratio, stillbirths, ind, 
         file = here::here("tmp", paste0(CNTRY, "-", YR, "-", ind), paste0("Input-", CNTRY, "-", YR, ".RData")))
    
    ## run scripts
    source(here::here("scripts", "201-preparation.R"), local = TRUE) 
    
    source(here::here("scripts", "202-LTR-MNM_aggregate.R"), local = TRUE)
    
    ## clear environment
    rm(list = ls())
    
  })    
})
  
## get file names
files <- list.files(here::here("tmp"), pattern = "LTR-agg", recursive = TRUE, full.names = TRUE)

## add output from different countries to list
ltr.aux <- 
  map(files, function(x){
    
    load(x)
    
    ltr.agg
    
  })

## append elements of list
ltr.agg.combined <-
  rbindlist(ltr.aux)

## save as .csv file
write_csv(ltr.agg.combined, (here::here("tmp", "ltr-agg-combined.csv")))

## run output file
source(here::here("scripts", "299-output.R"))

## delete temporary folder
unlink(here::here("tmp"), recursive = TRUE)
