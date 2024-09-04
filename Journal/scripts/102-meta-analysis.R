########################################
#---Maternal near miss meta-analysis---# 
#-----for global comparison of the-----# 
#--lifetime risk of maternal near miss-#
#--------------(LTR-MNM)---------------#  
########################################

## --- FILE 102-meta-analysis.R: CONDUCT META-ANALYSIS TO ESTIMATE POOLED MNM RATIO ---

## load input information
load(file = here::here("tmp", "adjusted-input.RData"))

## only include
## - studies using (modified) WHO criteria 
## - observations from 2010 onwards
meta_data <- subset(data_adjust_pop, criteria_type == "who" & midpoint_ref >= 2010) 

## 1. main meta analysis  
## - separate country level models: to obtain one MNM estimate per country
## - random effects: due to assumption of substantial heterogeneity in effect size (different criteria, sampling, etc.) 
##   within a country
## - weights: proportional to sample size so that for a given sample size,   
##   low prevalence studies contribute an equal weight

conduct_random_effects_meta_analysis_1 <- function(meta_data){
  
  meta_object <- rma(measure = "PLN", 
                     xi = cases, 
                     ni = denominator_pop,  
                     weights = denominator_pop, 
                     method = "REML",
                     data = meta_data)
  
  summary_object <- summary(meta_object)
  
  weights_info <- as.data.frame(weights(meta_object))
  
  return(data.table(ISO = meta_data$ISO,
                    YR = meta_data$midpoint_ref,
                    country = meta_data$country,
                    region = meta_data$region,
                    stillbirth = meta_data$sbr,
                    cases = meta_data$cases,
                    denominator_pop = meta_data$denominator_pop,
                    Pooled_Ratio_adjusted = c(exp(meta_object$beta)) * 1000, 
                    Ratio_lower = exp(meta_object$ci.lb) * 1000, 
                    Ratio_upper = exp(meta_object$ci.ub) * 1000,
                    I2 = summary_object$I2,
                    Weights = weights_info$weights)
  )
  
}

## run country-level random effects models 
meta_results <- 
  meta_data %>%
  group_by(ISO) %>%
  do(conduct_random_effects_meta_analysis_1(.)) 

## keep one observation per country 
meta_country <- 
  meta_results %>% 
  group_by(country) %>% 
  mutate(studies = n(),
         YR = round(sum(YR) / studies, digits = 0)) %>% 
  slice_head(n = 1) %>% 
  ungroup()

## 2. heterogeneity: keep data for later
## keep only countries with more than one study (where we are calculating a pooled ratio)
pooled <- subset(meta_country, studies != 1)

## 3. sensitivity: denominators adjusted only for stillbirth if not live births
conduct_random_effects_meta_analysis_2 <- function(meta_data){
  
  meta_object <- rma(measure = "PLN", 
                     xi = cases, 
                     ni = denominator_lb,  
                     weights = denominator_lb, 
                     method = "REML",
                     data = meta_data)
  
  summary_object <- summary(meta_object)
  
  weights_info <- as.data.frame(weights(meta_object))
  
  return(
    data.table(
      ISO = meta_data$ISO, 
      cases = meta_data$cases,
      denominator_lb = meta_data$denominator_lb,
      Pooled_Ratio_unadjusted = c(exp(meta_object$beta)) * 1000
    )
  )
}

## run country-level random effects models 
meta_results2 <- 
  meta_data %>%
  group_by(ISO) %>%
  do(conduct_random_effects_meta_analysis_2(.))

## keep one observation per country 
meta_country2 <- 
  meta_results2 %>% 
  group_by(ISO) %>% 
  slice_head(n = 1) %>% 
  ungroup()

subset <- subset(meta_country2, select = c(ISO, Pooled_Ratio_unadjusted))

## 4. merge data sets to compare adjusted and unadjusted results 
meta_analysis <- left_join(meta_country, subset, by = "ISO")

meta_analysis <-
  meta_analysis %>% 
  select(region, ISO, country, YR, stillbirth, studies, 
         Pooled_Ratio_unadjusted, Pooled_Ratio_adjusted,
         Ratio_lower, Ratio_upper)

meta_analysis[, sapply(meta_analysis, is.numeric)] <- 
  lapply(meta_analysis[, sapply(meta_analysis, is.numeric)], round, digits = 2)

## create row number
meta_analysis$row_number <- seq_along(meta_analysis[[1]])

## save environment
write_csv(meta_analysis, file = here::here("tmp", "meta-results_150724.csv"))
save(pooled, file = here::here("tmp", "meta_multiple_studies_150724.RData"))

## clear environment
rm(list = ls())