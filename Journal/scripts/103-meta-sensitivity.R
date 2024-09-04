########################################
#---Maternal near miss meta-analysis---# 
#-----for global comparison of the-----# 
#--lifetime risk of maternal near miss-#
#--------------(LTR-MNM)---------------#  
########################################

## --- FILE 103-meta-sensitivity.R: WEIGHTS AND HETEROGENEITY ---

## load input information
load(file = here::here("tmp", "adjusted-input.RData"))
meta_data <- subset(data_adjust_pop, criteria_type == "who" & midpoint_ref >= 2010) 

## 1. sensitivity of meta-analysis to sample weights 
conduct_random_effects_meta_analysis <- function(meta_data, transformation){
  
  ## transform denominators based on input
  meta_data <-
    meta_data %>% 
    mutate(across(denominator_pop,
                  .fns = get(transformation), .names = "weights"))
  
  meta_object <- rma(measure = "PLN", 
                     xi = cases, 
                     ni = denominator_pop,  
                     weights = weights, 
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
                    Weights = weights_info$weights,
                    meta_object = list(meta_object))
  )
  
}

## 1a. weights proportional to sample size
meta_results <- 
  meta_data %>%
  group_by(ISO) %>%
  do(conduct_random_effects_meta_analysis(., transformation = "as.numeric")) 

## 1b. weights as square root of the sample size 
meta_results2 <- 
  meta_data %>%
  group_by(ISO) %>%
  do(conduct_random_effects_meta_analysis(., transformation = "sqrt")) 

## 1c. weights as log transformed sample size
meta_results3 <- 
  meta_data %>%
  group_by(ISO) %>%
  do(conduct_random_effects_meta_analysis(., transformation = "log"))

## keep one observation per country 
meta_country <- 
  meta_results %>% 
  group_by(country) %>% 
  mutate(studies = n(),
         YR = round(sum(YR) / studies, digits = 0)) %>% 
  slice_head(n = 1) %>% 
  ungroup()

meta_country2 <- 
  meta_results2 %>% 
  group_by(country) %>% 
  mutate(studies = n(),
         YR = round(sum(YR) / studies, digits = 0)) %>% 
  slice_head(n = 1) %>% 
  ungroup()

meta_country3 <- 
  meta_results3 %>% 
  group_by(country) %>% 
  mutate(studies = n(),
         YR = round(sum(YR) / studies, digits = 0)) %>% 
  slice_head(n = 1) %>% 
  ungroup()

## compare MNM ratio estimates with the three different weights 
meta_country$multiple <- ifelse(meta_country$studies == 1, 0, 1)
meta_country <- subset(meta_country, multiple == 1)

meta_country2$sqrt_weights <- meta_country2$Pooled_Ratio_adjusted
meta_country2 <- subset(meta_country2, select = c(country, sqrt_weights))

meta_country3$log_weights <- meta_country3$Pooled_Ratio_adjusted
meta_country3 <- subset(meta_country3, select = c(country, log_weights))

weights_compare <- left_join(meta_country, meta_country2)
weights_compare <- left_join(weights_compare, meta_country3)
weights_compare <- subset(weights_compare, select = c(country, multiple, denominator_pop, 
                                                      Pooled_Ratio_adjusted, sqrt_weights, log_weights))

## save environment 
write_csv(weights_compare, file = here::here("tmp", "meta-weights_150724.csv"))

## clear environment
rm(list = ls())

## 2. meta-regression to explain heterogeneity
## load input information
load(file = here::here("tmp", "adjusted-input.RData"))
meta_data <- subset(data_adjust_pop, criteria_type == "who" & midpoint_ref >= 2010) 

## create dummy variables for each year
meta_data <-
  meta_data %>%
  mutate(years = strsplit(reference_period, "-")) %>%
  mutate(years = map(years, ~ seq(head(.x, n = 1), tail(.x, n = 1)))) %>%  
  unnest(years) %>%
  arrange(years) %>% 
  mutate(aux = 1) %>% 
  pivot_wider(names_from = "years",
              values_from = "aux",
              names_prefix = "year_") %>% 
  mutate(across(starts_with("year_"), ~ ifelse(is.na(.), 0, .)))

## conduct univariable regressions
## turn categorical variables into factor variables
meta_data <- 
  meta_data %>%
  mutate(
    country = as.factor(country),
    criteria_clean = as.factor(criteria_clean),
    area = as.factor(area),
    pop_based = as.factor(pop_based)
  )

## list of years to include in the model together
years <- paste0("year_20", 10:20)

## list of other moderators to evaluate individually
other_moderators <- c("country", "criteria_clean", "pop_based", "area")

## create a combined formula for the years
years_formula <- as.formula(paste("~ 1 +", paste(years, collapse = " + ")))

## fit the model with all the years together
years_model <- rma(measure = "PLN", 
                   xi = cases, 
                   ni = denominator_pop,  
                   weights = denominator_pop, 
                   mods = years_formula,
                   method = "REML",
                   data = meta_data)

## extract the coefficients and p-values for the years model
coef_vector <- coef(summary(years_model))[, 1]
p_values <- coef(summary(years_model))[, 4]

## store results in the list
results.years <- 
  data.frame(
    moderator = "years",
    level = rownames(coef(summary(years_model))),
    coef = coef_vector,
    p_value = p_values,
    I2 = years_model$I2,
    R2 = years_model$R2,
    tau2 = years_model$tau2
  )

## loop over each moderator and fit the univariable model
results.mod <-
  map(other_moderators, function(mod){
  
  ## define the formula for the current moderator
  current_formula <- as.formula(paste("~ 1 +", mod))
  
  ## fit the univariable model
  current_model <- rma(measure = "PLN", 
                       xi = cases, 
                       ni = denominator_pop,  
                       weights = denominator_pop, 
                       mods = current_formula,
                       method = "REML",
                       data = meta_data)
  
  ## extract the coefficients and p-values
  coef_vector <- coef(summary(current_model))[, 1]
  p_values <- coef(summary(current_model))[, 4]
  
  ## store results in a named list
  results <- 
    data.frame(
      moderator = mod,
      level = rownames(coef(summary(current_model))),
      coef = coef_vector,
      p_value = p_values, 
      I2 = current_model$I2,
      R2 = current_model$R2,
      tau2 = current_model$tau2
    )
  
  results
  
})

## combine all results into a single data frame
combined_results <- 
  results.years %>% 
  add_row(
    bind_rows(results.mod)
  )

## save the results
save(combined_results, file = here::here("tmp", "meta_regression_univariable.RData"))

## 3. build multivariable model 
## control for country
model0 <- 
  rma(measure = "PLN", 
      xi = cases, 
      ni = denominator_pop, 
      weights = denominator_pop, 
      mods = ~ 1 + criteria_clean, 
      method = "REML",
      data = meta_data
  )

summary(model0)

model1 <- 
  rma(measure = "PLN", 
      xi = cases, 
      ni = denominator_pop, 
      weights = denominator_pop, 
      mods = ~ 1 + country + criteria_clean, 
      method = "REML",
      data = meta_data
  )

summary(model1)

model2 <- 
  rma(measure = "PLN", 
      xi = cases, 
      ni = denominator_pop, 
      weights = denominator_pop, 
      mods = ~ 1 + country + criteria_clean + area, 
      method = "REML",
      data = meta_data
  )

summary(model2)
