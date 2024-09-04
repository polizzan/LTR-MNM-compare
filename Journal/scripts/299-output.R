#################################
#--------Calculation of---------# 
#-------the lifetime risk-------# 
#-----of maternal near miss-----#
#-----------(LTR-MNM)-----------#  
#################################

## --- FILE 299-output.R: PLOT AND TABULATE LTR-MNM ---

## load data
ltr_data <-read_csv(here::here("tmp", "ltr-agg-combined.csv"))
mnm_data <- read_csv(here::here("tmp", "meta-results_150724.csv"))
load(file = here::here("tmp", "summary_data.RData"))

ltr_data <-  left_join(ltr_data, summary_data)

## determine color palette
my_palette <- paletteer_d("ggsci::default_locuszoom")

## table of LTR main results
gt_tbl1 <- 
  ltr_data %>%
  filter(indicator == "adjusted") %>% 
  arrange(region, country) %>% 
  select(region, country, year, study_summary, observations,
         tfr, mnm.ratio, md.ratio,
         ltr.mnm.1.in, ltr.md.1.in, ltr.smo.1.in, prop.morbidity) %>% 
  mutate(across(c(starts_with("ltr."), prop.morbidity, tfr), ~ round(., 1))) %>% 
  gt(groupname_col = "region") %>%
  tab_header(title = "Global estimates of the lifetime risk of maternal near miss, maternal death, and severe maternal outcome") %>% 
  cols_label(region = md("Region"), 
             country = md("Country"), 
             year = md("Year"),
             observations = md("No. of MNM studies"),
             study_summary = md("Data type"),
             tfr = md("Total Fertility Rate"),
             mnm.ratio = md("MNMRatio"),
             md.ratio = md("MMR"),
             ltr.mnm.1.in = md("LTR-MNM 1 in N"), 
             ltr.md.1.in = md("LTR-MD 1 in N"), 
             ltr.smo.1.in = md("LTR-SMO 1 in N"), 
             prop.morbidity = md("Contribution (%) of morbidity to LTR-SMO")) %>% 
  fmt_number(
    columns = c(starts_with("ltr.")),
    decimals = 0
  ) %>% 
  fmt_number(
    columns = c(starts_with("m")),
    decimals = 1
  ) %>% 
  tab_options(
    row_group.as_column = TRUE, 
    table.font.size = 14
  ) 

## save table
gtsave(gt_tbl1, here::here("out", "table-1_main-LTR-results.docx"))

## uncertainty analysis 
## table S10: uncertainty in LTR-MNM estimates
gt_tblS10 <- 
  ltr_data %>%
  filter(indicator == "adjusted") %>% 
  left_join(mnm_data, by = c("ISO", "country", "region")) %>% 
  arrange(region, country) %>% 
  mutate(mnm_confidence = sprintf("%.1f (%.1f, %.1f)", mnm.ratio, mnm.ratio.lower, mnm.ratio.upper), 
         ltr_confidence = sprintf("%.0f (%.0f, %.0f)", round(ltr.mnm.1.in), round(ltr.mnm.lower), round(ltr.mnm.upper))) %>%
  select(region, country, year, studies, mnm_confidence, ltr_confidence) %>% 
  gt(groupname_col = "region") %>%
  tab_header(title = "Uncertainty in the estimates of the lifetime risk of maternal near miss") %>% 
  cols_label(region = md("Region"), 
             year = md("Year"),
             country = md("Country"), 
             mnm_confidence = md("MNMRatio"),
             studies = md("No. of studies in MNMRatio meta-analysis"), 
             ltr_confidence = md("LTR-MNM 1 in N")) %>% 
  fmt_number(
    columns = c("mnm_confidence"),
    decimals = 1
  ) %>% 
  tab_options(
    row_group.as_column = TRUE, 
    table.font.size = 14
  ) 

## save table
gtsave(gt_tblS10, here::here("out", "table-S10_uncertainty.docx"))

## sensitivity analysis
## table S12: compare adjusted and unadjusted ltr results
ltr.compare <- 
  ltr_data %>% 
  select(ISO, country, region, year, indicator,
         ltr.mnm, ltr.mnm.1.in,
         ltr.smo, ltr.smo.1.in) %>% 
  pivot_wider(names_from = "indicator",
              values_from = ltr.mnm:ltr.smo.1.in) %>% 
  mutate(diff.ltr = (ltr.mnm.1.in_adjusted - ltr.mnm.1.in_unadjusted) / ltr.mnm.1.in_adjusted * 100) 

## create table 
gt_tblS12 <- 
  ltr.compare %>%
  arrange(region, country) %>% 
  select(region, country, year, ltr.mnm.1.in_adjusted, ltr.mnm.1.in_unadjusted, diff.ltr) %>% 
  mutate(across(contains("ltr"), ~ round(., 1))) %>% 
  gt(groupname_col = "region") %>%
  tab_header(
    title = "Sensitivity of LTR to adjustment of MNM denominator for facility-based estimates"
  ) %>% 
  cols_label(
    region = md("Region"),
    year = md("Year midpoint"),
    country = md("Country"),
    ltr.mnm.1.in_adjusted = md("LTR-MNM Denom. Adjusted"), 
    ltr.mnm.1.in_unadjusted = md("LTR-MNM Denom. Unadjusted"), 
    diff.ltr = md("Difference in LTR-MNM (%)")
  ) %>% 
  fmt_number(
    columns = c(starts_with("ltr.")),
    decimals = 0
  ) %>% 
  fmt_number(
    columns = c(starts_with("diff.")),
    decimals = 1
  ) %>% 
  tab_options(
    row_group.as_column = TRUE, 
    table.font.size = 14
  ) 

## save table
gtsave(gt_tblS12, here::here("out", "table-S12_sensitivity-denominator.docx"))

## figure 1: world map of LTR-MNM results
## assign values to bins
ltr_mnm_bi <- bi_class(ltr_data %>% filter(indicator == "adjusted"),
                       x = ltr.mnm,
                       y = tfr,
                       style = "quantile", 
                       dim = 3)

## get breaks
breaks <- bi_class_breaks(ltr_data %>% filter(indicator == "adjusted"),
                          x = ltr.mnm,
                          y = tfr,
                          style = "quantile", 
                          dim = 3)

## get world map data
world_map <- ne_countries(scale = "medium", returnclass = "sf")
world_map <- world_map[world_map$continent != "Antarctica", ]

## merge with LTR data
map_data <- left_join(ltr_mnm_bi, world_map, by = c("ISO" = "iso_a3"))

## plot map
ltr_map <- 
  ggplot() +
  ## add world layer
  geom_sf(data = world_map, fill = "lightgray") +  
  ## plot data
  geom_sf(data = map_data, aes(fill = bi_class, geometry = geometry)) +
  bi_scale_fill(pal = "DkViolet", dim = 3)+
  labs(fill = "Lifetime risk") +
  coord_sf(clip = "off") + 
  theme_minimal() + 
  theme(panel.grid = element_blank(), 
        plot.margin = margin(0, 0, 0, 0, "cm"),
        aspect.ratio = 0.60,
        axis.text.x = element_blank(),
        legend.position = "none")

## plot legend
legend <- 
  bi_legend(pal = "DkViolet",
            dim = 3,
            xlab = "Lifetime risk MNM",
            ylab = "TFR",
            size = 16) +
  theme(plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent"))

## combine plot with legend
finalPlot <- 
  ggdraw() +
  draw_plot(ltr_map, 0, 0, 1, 1) +
  draw_plot(legend, 0.1, .1, 0.2, 0.2)

finalPlot

## save plot
ggsave(here::here("out", "figure-1_mnm-map.png"), plot = finalPlot, width = 13, height = 9, dpi = 400)

## export editable graphic
doc <- read_pptx()
doc <- add_slide(doc, "Title and Content", "Office Theme")

doc <- ph_with(doc, dml(ggobj = finalPlot), location = ph_location_type(type = "body"))

print(doc, target = here::here("out", "Figure 1.pptx"))

## figure 2: world map of LTR-SMO results 
## merge with LTR data
map_data2 <- left_join(ltr_data %>% filter(indicator == "adjusted"), world_map, by = c("ISO" = "iso_a3"))

## plot map
finalPlotSMO <- 
  ggplot() +
  geom_sf(data = world_map, fill = "lightgray") +
  geom_sf(data = map_data2, aes(fill = ltr.smo.1.in, geometry = geometry)) +
  scale_fill_viridis_c(name = "LTR-SMO 1 in N", option = "inferno", trans = "log",
                       labels = function(x) round(x, -1)) +
  coord_sf(clip = "off") + 
  theme_minimal() + 
  theme(panel.grid = element_blank(), 
        plot.margin = margin(0, 0, 0, 0, "cm"),
        aspect.ratio = 0.60,
        axis.text.x = element_blank())

## save plot
ggsave(here::here("out", "figure-2_smo-map.png"), plot = finalPlotSMO, width = 13, height = 9, dpi = 400)

## export editable graphic 
doc <- ph_with(doc, dml(ggobj = finalPlotSMO), location = ph_location_type(type = "body"))
print(doc, target = here::here("out", "Figure 2.pptx"))

## figure 3: bar chart with contribution of LTR-MNM to LTR-SMO
ltr_data$region <- as.factor(ltr_data$region)

ltr_data <- 
  ltr_data %>% 
  mutate(transition = as.factor(case_when(md.ratio >= 500 ~ "1",
                                          md.ratio >= 300 & md.ratio < 500 ~"2", 
                                          md.ratio >= 100 & md.ratio < 300 ~"3", 
                                          md.ratio >= 20 & md.ratio < 100 ~ "4a", 
                                          md.ratio < 20 ~ "4b")))

## create bar chart
## add symbol to denote whether data are national/subnational 
ltr_data <- ltr_data %>% 
  mutate(symbol = case_when(study_summary == "National only" ~ "(N)", 
                            study_summary == "Subnational only" ~ "(S)", 
                            TRUE ~ "(B)"))

ltr_data <- ltr_data %>% 
  mutate(country_new = paste(country, symbol))

fig3 <- 
  ggplot(ltr_data %>% filter(indicator == "adjusted"),
         aes(y = reorder(country, prop.morbidity),
             x = prop.morbidity, fill = transition)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.7) +
  geom_text(aes(label = symbol, x = prop.morbidity + 0.6), 
            hjust = 0, vjust = 0.5, size = 2) + 
  labs(x = "Contribution of MNM to LTR-SMO (%)", y = "",
       fill = "Stage in Obstetric Transition (MMR per 100,000)") +
  scale_fill_manual(values = my_palette) +
  theme_classic() +
  theme(axis.text.x = element_text(vjust = 0.5, hjust = 1),
        axis.text.y = element_text(size = 11), 
        axis.title = element_text(size = 14), 
        panel.grid = element_blank(),
        legend.position = "bottom") + 
  scale_y_discrete(expand = expansion(mult = c(0.001, 0.001)))

## save plot
ggsave(here::here("out", "figure-3_obst_transition.png"), plot = fig3, width = 13, height = 12, dpi = 400)

## export editable graphic 
doc <- ph_with(doc, dml(ggobj = fig3), location = ph_location_type(type = "body"))
print(doc, target = here::here("out", "Figure 3.pptx"))

## figure S1: map for national/subnational data
## plot map
levels <- c("National only", "Subnational only", "Both")
map_data2$study_summary <- factor(map_data2$study_summary, levels = levels)

figS1 <- 
  ggplot() +
  geom_sf(data = world_map, fill = "lightgray") +
  geom_sf(data = map_data2, aes(fill = study_summary, geometry = geometry)) +
  scale_fill_manual(name = "Type of data", values = my_palette) +
  coord_sf(clip = "off") + 
  theme_minimal() + 
  theme(panel.grid = element_blank(), 
        plot.margin = margin(0, 0, 0, 0, "cm"),
        aspect.ratio = 0.60,
        axis.text.x = element_blank())

figS1

## save plot
ggsave(here::here("out", "figure-s1_data-map.png"), plot = figS1, width = 13, height = 9, dpi = 400)

## export editable graphic 
doc <- ph_with(doc, dml(ggobj = figS1), location = ph_location_type(type = "body"))
print(doc, target = here::here("out", "Figure S1.pptx"))

## figure S2: scatter plot between LTR-MNM and MMR
figS2 <- 
  ggplot(ltr_data %>% filter(indicator == "adjusted"),
         aes(x = (ltr.mnm.1.in),
             y = (ltr.md.1.in), colour = region)) +
  geom_point() +
  ## add labels for ISO codes
  geom_text(aes(label = ISO), vjust = 1.5, hjust = 0.5) +  
  labs(x = "LTR Maternal Near Miss (log scale)", 
       y = "LTR Maternal Death (log scale)",
       ## color according to SDG region
       colour = "SDG Region") +
  ## plot on log scale
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") +
  scale_colour_manual(values = my_palette) +
  theme_classic() +
  theme(panel.grid = element_blank(),
        legend.position = "bottom")

## save plot
ggsave(here::here("out", "figure-S2_scatter.png"), plot = figS2, width = 13, height = 9, dpi = 400)
