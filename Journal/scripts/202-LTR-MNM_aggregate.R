#################################
#--------Calculation of---------# 
#-------the lifetime risk-------# 
#-----of maternal near miss-----#
#-----------(LTR-MNM)-----------#  
#################################

## --- FILE 202-LTR-MNM_aggregate.R: CALCULATE LTR-MNM WITH AGGREGATE DATA ---

## life table radix
l0 <- 100000

## life table survivors at age 15
l15 <- CNTRY.WPP[[1]]

## calculate LTR-MNM using aggregate data
ltr.mnm <- mnm.ratio * CNTRY.WPP[[2]] * CNTRY.WPP[[3]] * (l0 / l15) / 1000

## calculate upper and lower LTR-MNM bounds using MNMRatio UI 
ltr.mnm.lower <- mnm.ratio.lower * CNTRY.WPP[[2]] * CNTRY.WPP[[3]] * (l0 / l15) / 1000
ltr.mnm.upper <- mnm.ratio.upper * CNTRY.WPP[[2]] * CNTRY.WPP[[3]] * (l0 / l15) / 1000

## calculate LTR-MD using aggregate data
ltr.md <- md.ratio * CNTRY.WPP[[2]] * CNTRY.WPP[[3]] * (l0 / l15) / 100000

ltr.smo <- ltr.mnm + ltr.md
prop.morbidity <- ltr.mnm / ltr.smo

## fertility 
tfr <- CNTRY.WPP[[4]]

## create data frame
ltr.agg <- data.frame("ISO" = CNTRY,
                      "country" = country,
                      "region" = region,
                      "year" = YR,
                      "indicator" = ind,
                      "tfr" = tfr,
                      "mnm.ratio" = mnm.ratio,
                      "mnm.ratio.lower" = mnm.ratio.lower,
                      "mnm.ratio.upper" = mnm.ratio.upper,
                      "md.ratio" = md.ratio,
                      "ltr.mnm" = ltr.mnm,
                      "ltr.mnm.1.in" = 1 / ltr.mnm, 
                      "ltr.mnm.lower" = 1 / ltr.mnm.lower,
                      "ltr.mnm.upper" = 1 / ltr.mnm.upper,
                      "ltr.md" = ltr.md,
                      "ltr.md.1.in" = 1 / ltr.md, 
                      "ltr.smo" = ltr.smo,
                      "ltr.smo.1.in" = 1 / ltr.smo, 
                      "prop.morbidity" = prop.morbidity * 100)

## save environment
save(ltr.agg, file = here::here("tmp", paste0(CNTRY, "-", YR, "-", ind), paste0("LTR-agg-", CNTRY, "-", YR, ".RData")))
