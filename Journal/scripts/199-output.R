########################################
#---Maternal near miss meta-analysis---# 
#-----for global comparison of the-----# 
#--lifetime risk of maternal near miss-#
#--------------(LTR-MNM)---------------#  
########################################

## --- FILE 199-output.R: EXPORT TABLES FOR MANUSCRIPT: INCLUDED MNM STUDIES & META-ANALYSIS RESULTS ---

## load extracted study results
data <- read_csv(file = here::here("data", "mnm-extraction_150724.csv"))
data <- subset(data, criteria_type == "who" & midpoint_ref >= 2010)

## table S2: included studies in MNM meta-analysis
gt_tblS2 <- 
  data %>%
  arrange(region, country, citation) %>% 
  select(region, country, citation, reference_period, area, location_detail,
         mnm_criteria, criteria_type, cases, denominator, denom_type, mnm_ratio) %>% 
  gt(groupname_col = "region") %>%
  tab_header(title = "Included prevalence studies in MNM meta-analysis") %>% 
  cols_label(region = md("Region"), 
             country = md("Country"), 
             citation = md("Reference"),
             reference_period = ("Year of observation"),
             area = md("Study design"), 
             location_detail = md("Location"),
             mnm_criteria = md("MNM criteria"), 
             criteria_type = md("Type of criteria"),
             cases = md("MNM cases"), 
             denominator = md("Denominator"), 
             denom_type = md("Type of denominator"),
             mnm_ratio = md("MNM ratio")) %>% 
  tab_options(
    row_group.as_column = TRUE, 
    table.font.size = 14
    ) %>% 
  fmt_number(
    columns = c(cases, denominator),
    decimals = 0
  ) %>% 
  fmt_number(
    columns = c(mnm_ratio),
    decimals = 1
  )

## save table
gtsave(gt_tblS2, here::here("out", "table-S2_mnm-included-studies.docx"))

## table S3: breakdown of regional and national data
summary_data <- 
  data %>% 
  group_by(country) %>%
  summarise(
    study_summary = case_when(
      all(area == 'National') ~ 'National only',
      all(area == 'Regional') ~ 'Subnational only',
      TRUE ~ 'Both'
      ), 
    region = paste(unique(region), collapse = ", "),
    observations = n()
    )

save(summary_data, file = here::here("tmp","summary_data.RData"))

gt_tblS3 <- 
  summary_data %>%
  arrange(region, country, observations, study_summary) %>% 
  gt(groupname_col = "region") %>%
  tab_header(title = "Type of maternal near miss data by country") %>% 
  cols_label(region = md("Region"), 
             country = md("Country"), 
             observations = ("No. of studies"),
             study_summary = md("Type of data")) %>% 
  tab_options(
    row_group.as_column = TRUE, 
    table.font.size = 14
    )

## save table
gtsave(gt_tblS3, here::here("out", "table-S3_national_subnational.docx"))

check <- subset(summary_data, study_summary == "Combination")

## table S5: MNM ratio estimates
meta_analysis <- read_csv(file = here::here("tmp", "meta-results_150724.csv"))

gt_tblS5 <- 
  meta_analysis %>%
  mutate(mnm_confidence = sprintf("%.1f (%.1f, %.1f)", Pooled_Ratio_adjusted, Ratio_lower, Ratio_upper),
         meta = ifelse(studies == 1, 0, 1)) %>% 
  select(c(region, country, YR, studies, meta, mnm_confidence)) %>% 
  arrange(region, country) %>% 
  gt(groupname_col = "region") %>%
  tab_header(title = "Population-level maternal near miss ratio estimates by country") %>% 
  cols_label(region = md("Region"), 
             studies = md("No. of studies"),
             YR = md("Year midpoint"),
             meta = md("Meta-analysis performed"),
             country = md("Country"), 
             mnm_confidence = md("Adjusted MNM ratio")) %>% 
  tab_options(
    row_group.as_column = TRUE, 
    table.font.size = 14
    ) 

## save table
gtsave(gt_tblS5, here::here("out", "table-S5_MNM_estimates.docx"))

## table S6: meta-analysis sensitivity to weights  
weights_compare <- read_csv(file = here::here("tmp", "meta-weights_150724.csv"))

gt_tblS6 <- 
  weights_compare %>%
  select(-c(multiple, denominator_pop)) %>% 
  arrange(country) %>% 
  gt() %>% 
  tab_header(title = "Senstivity of pooled MNM ratio estimates to weights") %>% 
  cols_label(country = md("Country"), 
             Pooled_Ratio_adjusted = md("Weights = N"), 
             sqrt_weights = md("Weights = sqrt(N)"), 
             log_weights = md("Weights = log(N)")) %>% 
  fmt_number(
    columns = c(Pooled_Ratio_adjusted, sqrt_weights, log_weights),
    decimals = 2
  )

## save table
gtsave(gt_tblS6, here::here("out", "table-S6_mnm-meta-weights.docx"))

## table S7: heterogeneity 
load(file = here::here("tmp", "meta_multiple_studies_150724.RData"))
load(file = here::here("tmp", "adjusted-input.RData"))

data_adjust_pop <- 
  data_adjust_pop %>% 
  filter(criteria_type == "who" & midpoint_ref >= 2010) %>% 
  mutate(mnm_ratio_adj = (cases / denominator_pop) * 1000)

pooled <- subset(pooled, select = c(country, Pooled_Ratio_adjusted, Ratio_lower, Ratio_upper, I2))
study_info <- subset(data_adjust_pop, select = c(country, citation, cases, denominator_pop, mnm_ratio_adj))

study_info <- 
  study_info %>% 
  group_by(country) %>% 
  filter(n() != 1) %>% 
  ungroup()

heterogeneity <- left_join(study_info, pooled, by = "country")

gt_tblS7 <- 
  heterogeneity %>%
  arrange(country, citation) %>% 
  mutate(mnm_confidence = sprintf("%.1f (%.1f, %.1f)", Pooled_Ratio_adjusted, Ratio_lower, Ratio_upper)) %>% 
  select(country, citation, cases, denominator_pop, mnm_ratio_adj, mnm_confidence, I2) %>% 
  gt() %>%
  tab_header(title = "Heterogeneity in random effects meta-analysis model") %>% 
  cols_label(country = md("Country"), 
             citation = md("Reference"),
             cases = md("MNM cases"), 
             denominator_pop = md("Adjusted denominator"),
             mnm_ratio_adj = md("Adjusted MNM ratio"), 
             mnm_confidence = md("Pooled MNM ratio"), 
             I2 = md("I squared")) %>% 
  fmt_number(
    columns = c("mnm_ratio_adj", "mnm_confidence", "I2"),
    decimals = 1
    ) %>% 
  fmt_number(
    columns = c("cases", "denominator_pop"),
    decimals = 0
    ) %>% 
  tab_options(
    row_group.as_column = TRUE, 
    table.font.size = 14
  ) 

## save table
gtsave(gt_tblS7, here::here("out", "table-S7_heterogeneity.docx"))

## table S8: meta-regression results 
load(file = here::here("tmp", "meta_regression_univariable.RData"))

gt_tblS8 <- 
  combined_results %>% 
  gt(groupname_col = "moderator") %>%
  cols_label(
    moderator = md("Moderator"),
    level = md("Level"),
    coef = md("Coefficient"),
    p_value = md("p value"), 
    I2 = md("I squared"),
    R2 = md("R squared"),
    tau2 = md("tau squared")) %>%
  fmt_number(
    columns = c("coef", "I2", "R2", "tau2"),
    decimals = 2
  ) %>% 
  fmt_number(
    columns = "p_value",
    decimals = 3
  )

## save table
gtsave(gt_tblS8, here::here("out", "table-S8-univariable_results.docx"))

## table S11: MNM ratio estimate sensitivity to denominator adjustment
gt_tblS11 <- 
  meta_analysis %>%
  select(-c(stillbirth, row_number, Ratio_lower, Ratio_upper)) %>% 
  arrange(region, country) %>% 
  gt(groupname_col = "region") %>%
  tab_header(title = "Country-level maternal near miss ratio estimates") %>% 
  cols_label(region = md("Region"), 
             YR = md("Year midpoint"),
             country = md("Country"), 
             studies = md("No. of studies"),
             Pooled_Ratio_unadjusted = md("Unadjusted MNM ratio"), 
             Pooled_Ratio_adjusted = md("Adjusted MNM ratio")) %>% 
  tab_options(
    row_group.as_column = TRUE, 
    table.font.size = 14
  ) %>% 
  tab_style(
    style = cell_text(align = "center"), 
    locations = cells_body(columns = c("studies", 
                                       "Pooled_Ratio_unadjusted",
                                       "Pooled_Ratio_adjusted"))
    )

## save table
gtsave(gt_tblS11, here::here("out", "table-S11_mnm-sensitivity.docx"))
