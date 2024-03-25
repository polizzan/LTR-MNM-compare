########################################
#---Maternal near miss meta-analysis---# 
#-----for global comparison of the-----# 
#--lifetime risk of maternal near miss-#
#--------------(LTR-MNM)---------------#  
########################################

## --- FILE 109-output.R: EXPORT TABLES FOR MANUSCRIPT: INCLUDED MNM STUDIES & META-ANALYSIS RESULTS ---

## load extracted study results
data <- read_csv(file = here::here("data", "mnm-extraction_220224.csv"))

## table 1: included studies in MNM meta-analysis
gt_tbl1 <- 
  data %>%
  filter(criteria_type == "who" & midpoint_ref >= 2010) %>% 
  arrange(region, country, citation) %>% 
  select(region, country, citation, reference_period, area, location_detail,
         mnm_criteria, criteria_type, cases, denominator, denom_type) %>% 
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
             denom_type = md("Type of denominator")) %>% 
  tab_options(
    row_group.as_column = TRUE, 
    table.font.size = 14
    ) %>% 
  fmt_number(
    columns = c(cases, denominator),
    decimals = 0
    )

gtsave(gt_tbl1, here::here("out", "table-1_mnm-included-studies.docx"))

## load meta-analysis results
meta_analysis <- read_csv(file = here::here("tmp", "meta-results_110324.csv"))

## table 2: meta-analysis results by country 
gt_tbl2 <- 
  meta_analysis %>%
  select(-c(stillbirth, row_number)) %>% 
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
                                       "Pooled_Ratio_adjusted"))) %>% 
  summary_rows(
    columns = c(Pooled_Ratio_unadjusted, Pooled_Ratio_adjusted),
    fns = list(Mean = ~ round(mean(.), 1)),
    summary_interleave_text = "Regional Mean:"
  )

gtsave(gt_tbl2, here::here("out", "table-2_mnm-meta-analysis-results.docx"))
