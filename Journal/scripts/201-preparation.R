#################################
#--------Calculation of---------# 
#-------the lifetime risk-------# 
#-----of maternal near miss-----#
#-----------(LTR-MNM)-----------#  
#################################

## --- FILE 201-preparation.R: LOAD AND COMBINE UNWPP DATA ---

## WPP life table information (abridged)
CNTRY.l15 <- 
  WPP.combined %>% 
  filter(ISO3_code == CNTRY) %>%
  filter(Time == YR) %>% 
  pull(lx)

## sex ratio at birth
CNTRY.srb <- 
  WPP.combined %>% 
  filter(ISO3_code == CNTRY) %>% 
  filter(Time == YR) %>% 
  mutate(SRB = 1 + SRB / 100) %>% 
  pull(SRB)

## net reproduction rate
CNTRY.nrr <- 
  WPP.combined %>% 
  filter(ISO3_code == CNTRY) %>% 
  filter(Time == YR) %>% 
  pull(NRR) 

## total fertility rate
CNTRY.tfr <- 
  WPP.combined %>% 
  filter(ISO3_code == CNTRY) %>% 
  filter(Time == YR) %>% 
  pull(TFR)

## create list object
CNTRY.WPP <- list(CNTRY.l15, CNTRY.srb, CNTRY.nrr, CNTRY.tfr)

## save environment
save(CNTRY.WPP, file = here::here("tmp", paste0(CNTRY, "-", YR, "-", ind), paste0("WPP-", CNTRY, "-", YR, ".RData")))
