#################################
#--Simulation & calculation of--# 
#-------the lifetime risk-------# 
#-----of maternal near miss-----#
#-----------(LTR-MNM)-----------#  
#################################

## --- FILE 201-preparation.R: LOAD AND COMBINE UNWPP DATA ---

## WPP life table information (abridged)
CNTRY.mort <- WPP.combined %>% 
              filter(ISO3_code == CNTRY) %>%
              filter(Time == YR) %>% 
              mutate(survivor = Lx / lx[1]) %>% ## survivorship ratio (Lx / l15)
              select(Lx, survivor)
  
## WPP age-specific fertility rates & births per 1,000 (abridged) 
CNTRY.fert <- WPP.combined %>% 
              filter(ISO3_code == CNTRY) %>%
              filter(Time == YR) %>% 
              mutate(fx = ASFR / 1000,
                     livebirths = Births * (1000 - stillbirths) / 1000) %>% ## adjust for number of stillbirths
              select(fx, livebirths)

## sex ratio at birth
CNTRY.srb <- WPP.combined %>% 
              filter(ISO3_code == CNTRY) %>% 
              filter(Time == YR) %>% 
              mutate(SRB = 1 + SRB / 100) %>% 
              slice_head(n = 1) %>%             
              pull(SRB)

## net reproduction rate
CNTRY.nrr <- WPP.combined %>% 
              filter(ISO3_code == CNTRY) %>% 
              filter(Time == YR) %>% 
              slice_head(n = 1) %>% 
              pull(NRR) 

## total fertility rate
CNTRY.tfr <- WPP.combined %>% 
              filter(ISO3_code == CNTRY) %>% 
              filter(Time == YR) %>% 
              slice_head(n = 1) %>% 
              pull(TFR)
    
## create list object
CNTRY.WPP <- list(data.frame(age, CNTRY.mort, CNTRY.fert), CNTRY.srb, CNTRY.nrr, CNTRY.tfr)

## save environment
save(CNTRY.WPP, file = here::here("tmp", paste0(CNTRY, "-", YR, "-", ind), paste0("WPP-", CNTRY, "-", YR, ".RData")))
