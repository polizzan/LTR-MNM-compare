########################################
#---Maternal near miss meta-analysis---# 
#-----for global comparison of the-----# 
#--lifetime risk of maternal near miss-#
#--------------(LTR-MNM)---------------#  
########################################

## --- FILE 101-preparation.R: PREPARE ALL INPUT DATA FOR META-ANALYSIS ---

## load cleaned input information
data <- read_csv(here::here("data", "mnm-extraction_150724.csv"))

## 1. adjust deliveries and admissions which are not live births by stillbirths and multiple births
## load stillbirth data and convert to long format
stillbirth <- read_excel(here::here("data", "stillbirth-rate-and-stillbirths_2023.xlsx"),
                         sheet = "SBR Country estimates") %>% 
  filter(`Uncertainty.Bounds*` == "Median") %>% 
  pivot_longer(cols = -c(ISO:`Uncertainty.Bounds*`),
               names_to = "midpoint_ref",
               values_to = "sbr") %>% 
  filter(!is.na(sbr)) %>% 
  mutate(midpoint_ref = as.numeric(midpoint_ref)) %>% 
  select(ISO, midpoint_ref, sbr)

## load multiple-birth data 
multiplebirth <- read_excel(here::here("data", "twin-rate_monden-2021.xlsx"))

## join data sets
data_adjust_lb <- left_join(data, stillbirth, by = c("ISO", "midpoint_ref"))
data_adjust_lb <- left_join(data_adjust_lb, multiplebirth, by = "ISO")

## adjust denominator, unless live births: 
## - upwards by multiple-birth rate 
## - downwards by stillbirth rate 
data_adjust_lb <- 
  data_adjust_lb %>% 
  mutate(mbr = (1 + (twin_rate / 1000)), 
         lbr = ((1000 - sbr) / 1000),
         denominator_lb = case_when(denom_type != "livebirths" ~ denominator * mbr * lbr,
                                    TRUE ~ denominator))

## 2. adjust denominators which are not population-based by institutional delivery rate:
## - surveillance and registry data considered to be population-based 
## - match institutional delivery rate based on year closest to study's reference period
## - if institutional delivery data unavailable, assume no need to adjust (high-income countries)

## load WHO institutional delivery data
inst_del <- read.csv(here::here("data", "who-institutional-delivery.csv")) %>% 
  select(ISO, midpoint, Value)

## merge adjusted denominators with institutional delivery rate and take closest matching year 
data_adjust_pop <- left_join(data_adjust_lb, inst_del, by = "ISO", relationship = "many-to-many") %>% 
  group_by(id) %>% 
  arrange(ISO, abs(midpoint_ref - midpoint), na.rm = TRUE) %>% 
  slice_head(n = 1) %>% 
  ungroup()

## adjust denominators by institutional delivery rate
## assume institutional delivery rate = 1 if missing
data_adjust_pop <- 
  data_adjust_pop %>% 
  mutate(inst_del = case_when(is.na(Value) ~ 1,
                              TRUE ~ Value / 100),
         denominator_pop = case_when(pop_based == 0 ~ (denominator_lb * (1 / inst_del)),
                                     TRUE ~ denominator_lb))

## save environment
save(data_adjust_pop, file = here::here("tmp", "adjusted-input.RData"))
