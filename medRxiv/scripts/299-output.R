#################################
#--Simulation & calculation of--# 
#-------the lifetime risk-------# 
#-----of maternal near miss-----#
#-----------(LTR-MNM)-----------#  
#################################

## --- FILE 299-output.R: PLOT AND TABULATE LTR-MNM ---

## load data
ltr_data <-read_csv(here::here("tmp", "ltr-agg-combined.csv"))
mnm_data <- read_csv(here::here("tmp", "meta-results_110324.csv"))

## determine color palette
my_palette <- paletteer_d("ggsci::default_locuszoom")

## table of LTR main results
gt_tbl3 <- 
  ltr_data %>%
  filter(indicator == "adjusted") %>% 
  arrange(region, country) %>% 
  select(region, country, year, tfr, mnm.ratio, md.ratio,
         ltr.mnm.1.in, ltr.md.1.in, ltr.smo.1.in, prop.morbidity) %>% 
  mutate(across(c(starts_with("ltr."), prop.morbidity, tfr), ~ round(., 1))) %>% 
  gt(groupname_col = "region") %>%
  tab_header(title = "Global estimates of the lifetime risk of maternal near miss, maternal death, and severe maternal outcome") %>% 
  cols_label(region = md("Region"), 
             country = md("Country"), 
             tfr = md("Total Fertility Rate"),
             mnm.ratio = md("MNMRatio"),
             md.ratio = md("MMR"),
             ltr.mnm.1.in = md("LTR-MNM 1 in N"), 
             ltr.md.1.in = md("LTR-MD 1 in N"), 
             ltr.smo.1.in = md("LTR-SMO 1 in N"), 
             prop.morbidity = md("Contribution (%) of morbidity to LTR-SMO")) %>% 
  fmt_number(
    columns = c(starts_with("ltr.")),
    decimals = 0) %>% 
  fmt_number(
    columns = c(starts_with("m")),
    decimals = 1) %>% 
  tab_options(
    row_group.as_column = TRUE, 
    table.font.size = 14) 

gtsave(gt_tbl3, here::here("out", "table-3_main-LTR-results.docx"))

## compare adjusted and unadjusted ltr results
ltr.compare <- 
  ltr_data %>% 
  select(ISO, country, region, year, indicator,
         ltr.mnm, ltr.mnm.1.in,
         ltr.smo, ltr.smo.1.in) %>% 
  pivot_wider(names_from = "indicator",
              values_from = ltr.mnm:ltr.smo.1.in) %>% 
  mutate(diff.ltr = (ltr.mnm.1.in_adjusted - ltr.mnm.1.in_unadjusted) / ltr.mnm.1.in_adjusted * 100) 

## create table 
gt_tbl4 <- 
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
  ) %>% 
  summary_rows(
    columns = c(diff.ltr),
    fns = list(Mean = ~ round(mean(.), 1)),
    summary_interleave_text = "Regional Mean:"
  )

## save table
gtsave(gt_tbl4, here::here("out", "table-4_sensitivity-denominator.docx"))

## create table
gt_tbls5 <- 
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
             mnm_confidence = md("MNM ratio"),
             studies = md("No. of studies in MNM ratio meta-analysis"), 
             ltr_confidence = md("LTR-MNM 1 in N")) %>% 
  fmt_number(
    columns = c("mnm_confidence"),
    decimals = 1) %>% 
  tab_options(
    row_group.as_column = TRUE, 
    table.font.size = 14
    ) 

## save table
gtsave(gt_tbls5, here::here("out", "table-S5_uncertainty.docx"))

## world map of LTR-MNM results
## assign values to bins
ltr_mnm_bi <- bi_class(ltr_data %>% filter(indicator == "adjusted"),
                       x = ltr.mnm,
                       y = tfr,
                       style ="quantile", 
                       dim = 3)

## get breaks
breaks <- bi_class_breaks(ltr_data %>% filter(indicator == "adjusted"),
                          x = ltr.mnm,
                          y = tfr,
                          style ="quantile", 
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

## save plot
ggsave(here::here("out", "figure-1_mnm-map.png"), plot = finalPlot, width = 13, height = 9, dpi = 400)

## world map of LTR-SMO results 
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

## scatterplot between LTR-MNM and MMR
fig2 <- 
  ggplot(ltr_data %>% filter(indicator == "adjusted"),
         aes(x = (ltr.mnm.1.in),
             y = (ltr.md.1.in), colour = region)) +
  geom_point() +
  ## add labels for ISO codes
  geom_text(aes(label = ISO), vjust = 1.5, hjust = 0.5) +  
  labs(x = "LTR Maternal Near Miss (log scale)", y = "LTR Maternal Death (log scale)",
       ## color according to SDG region
       colour = "SDG Region") +
  ## plot on log scale
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") +
  scale_colour_manual(values = my_palette) +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        panel.grid = element_blank(),
        legend.position = "bottom")

## save plot
ggsave(here::here("out", "figure-3_scatter.png"), plot = fig2, width = 13, height = 9, dpi = 400)

## barchart with contribution of LTR-MNM to LTR-SMO
ltr_data$region <- as.factor(ltr_data$region)

ltr_data <- 
  ltr_data %>% 
  mutate(transition = as.factor(case_when(md.ratio >= 500 ~ "1",
                                          md.ratio >= 300 & md.ratio < 500 ~"2", 
                                          md.ratio >= 100 & md.ratio < 300 ~"3", 
                                          md.ratio >= 20 & md.ratio < 100 ~ "4a", 
                                          md.ratio < 20 ~ "4b")))

## create barchart
fig3 <- 
  ggplot(ltr_data %>% filter(indicator == "adjusted"),
         aes(y = reorder(country, prop.morbidity),
             x = prop.morbidity, fill = transition)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(x = "Country", y = "Contribution of MNM to LTR-SMO (%)",
       fill = "Stage in Obstetric Transition (MMR per 100,000)") +
  scale_fill_manual(values = my_palette) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(size = 12), 
        axis.title = element_text(size = 16), 
        panel.grid = element_blank(),
        legend.position = "bottom")

## save plot
ggsave(here::here("out", "figure-4_bar.png"), plot = fig3, width = 13, height = 9, dpi = 400)