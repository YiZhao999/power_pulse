# ================================================================
#  Regime Classification: Mapping Countries to R00, R01, R10, R11
#
#  Fix: explicit dplyr:: prefixes on select(), filter(), mutate()
#  to prevent masking by rnaturalearth / sf packages.
# ================================================================

# ── 0. Packages ──────────────────────────────────────────────────
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
  tidyverse,
  scales,
  rnaturalearth,
  rnaturalearthdata,
  sf,
  ggrepel
)

# Explicitly re-attach dplyr AFTER spatial packages to ensure
# select/filter/mutate resolve to dplyr, not plyr or other masks
library(dplyr)


# ── 1. Load & prepare data ───────────────────────────────────────
df_raw <- read_csv("~/Desktop/SPRING2026/MA_paper/0329/final_merged_dataset.csv",
                   show_col_types = FALSE)

rescale_fav <- function(x) {
  1 - (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

df <- df_raw %>%
  dplyr::mutate(
    fav_us_r      = rescale_fav(fav_us),
    fav_china_r   = rescale_fav(fav_china),
    log_aid_us    = log(aid_us    + 1),
    log_aid_china = log(aid_china + 1),
    country_clean = dplyr::recode(country,
                                  "Brasil"          = "Brazil",
                                  "España"          = "Spain",
                                  "México"          = "Mexico",
                                  "Panamá"          = "Panama",
                                  "Perú"            = "Peru",
                                  "Rep. Dominicana" = "Dominican Republic"
    )
  )


# ── 2. Compute within-country correlations ───────────────────────
cor_df <- df %>%
  dplyr::group_by(country_clean) %>%
  dplyr::summarise(
    n_obs = sum(!is.na(fav_us_r) & !is.na(log_aid_us)),
    
    cor_us = if_else(
      sum(!is.na(fav_us_r) & !is.na(log_aid_us)) >= 3,
      cor(log_aid_us, fav_us_r, use = "complete.obs"),
      NA_real_
    ),
    
    # Suppress NaN warning when China aid has zero variance
    cor_china = {
      n_valid <- sum(!is.na(fav_china_r) & !is.na(log_aid_china))
      if (n_valid >= 3) {
        val <- suppressWarnings(
          cor(log_aid_china, fav_china_r, use = "complete.obs")
        )
        if (is.nan(val)) NA_real_ else val
      } else {
        NA_real_
      }
    },
    
    .groups = "drop"
  )

message("=== Within-country correlations ===")
print(cor_df, n = Inf)


# ── 3. Assign compliance and regimes ────────────────────────────
regime_df <- cor_df %>%
  dplyr::mutate(
    comply_us    = !is.na(cor_us)    & cor_us    > 0,
    comply_china = !is.na(cor_china) & cor_china > 0,
    has_data     = n_obs > 0,
    
    regime = dplyr::case_when(
      !has_data                  ~ NA_character_,
      comply_us &  comply_china ~ "R00",
      comply_us & !comply_china ~ "R10",
      !comply_us &  comply_china ~ "R01",
      !comply_us & !comply_china ~ "R11"
    ),
    
    regime_label = dplyr::recode(regime,
                                 "R00" = "R00: H&M active for both",
                                 "R10" = "R10: H&M active for US only",
                                 "R01" = "R01: H&M active for China only",
                                 "R11" = "R11: H&M inactive for both"
    )
  )


# ── 4. Print & export classification table ───────────────────────
message("\n=== Country Regime Classification ===\n")

print_tbl <- regime_df %>%
  dplyr::arrange(regime, country_clean) %>%
  dplyr::transmute(
    Country             = country_clean,
    "Cor(Aid_US, Fav_US)"  = round(cor_us,    3),
    "Cor(Aid_CN, Fav_CN)"  = round(cor_china, 3),
    "US Complies"          = if_else(comply_us,    "Yes", "No"),
    "China Complies"       = if_else(comply_china, "Yes", "No"),
    Regime                 = if_else(!is.na(regime), regime, "No data")
  )

print(print_tbl, n = Inf)
write_csv(print_tbl, "h2_regime_classification.csv")
message("\n✓ Saved: h2_regime_classification.csv")

message("\n=== Regime frequency ===")
regime_df %>%
  dplyr::mutate(regime = if_else(is.na(regime), "No data", regime)) %>%
  dplyr::count(regime) %>%
  dplyr::arrange(regime) %>%
  print()


# ── 5. Choropleth Map ────────────────────────────────────────────
world <- ne_countries(scale = "medium", returnclass = "sf")

latam_bbox <- st_bbox(
  c(xmin = -120, xmax = -32, ymin = -56, ymax = 33),
  crs = st_crs(4326)
)

# Join regime data BEFORE cropping to avoid sf/dplyr conflicts
world_joined <- world %>%
  dplyr::left_join(
    regime_df %>%
      dplyr::select(country_clean, regime, regime_label),
    by = c("name" = "country_clean")
  )

latam <- sf::st_crop(world_joined, latam_bbox) %>%
  dplyr::mutate(
    fill_cat = dplyr::case_when(
      !is.na(regime_label)               ~ regime_label,
      name %in% unique(df$country_clean) ~ "In sample (no data)",
      TRUE                               ~ "Not in sample"
    )
  )

regime_colours <- c(
  "R00: H&M active for both"       = "#1B5E20",
  "R10: H&M active for US only"    = "#1565C0",
  "R01: H&M active for China only" = "#C62828",
  "R11: H&M inactive for both"     = "#F57F17",
  "In sample (no data)"            = "#BDBDBD",
  "Not in sample"                  = "#EEEEEE"
)

legend_order <- names(regime_colours)

# Country label centroids — classified countries only
label_df <- latam %>%
  dplyr::filter(!is.na(regime)) %>%
  sf::st_centroid() %>%
  dplyr::mutate(
    lon = sf::st_coordinates(.)[, 1],
    lat = sf::st_coordinates(.)[, 2]
  ) %>%
  sf::st_drop_geometry() %>%
  dplyr::select(name, regime, lon, lat)

fig_map <- ggplot(latam) +
  geom_sf(aes(fill = fill_cat), colour = "white", linewidth = 0.3) +
  geom_label_repel(
    data          = label_df,
    aes(x = lon, y = lat, label = name),
    size          = 2.8,
    fontface      = "bold",
    box.padding   = 0.45,
    point.padding = 0.3,
    segment.colour = "grey50",
    segment.size  = 0.4,
    max.overlaps  = 25,
    fill          = alpha("white", 0.80),
    label.size    = 0.25
  ) +
  scale_fill_manual(
    values = regime_colours,
    breaks = legend_order,
    name   = "Compliance Regime"
  ) +
  labs(
    title    = "Compliance Regime Classification across Latin American Countries",
    subtitle = paste0(
      "Based on sign of within-country correlation between log aid and public favorability (2002\u20132018)\n",
      "Positive correlation = hearts-and-minds channel active for that donor"
    ),
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title      = element_text(face = "bold", size = 13),
    plot.subtitle   = element_text(size = 9,  colour = "grey40"),
    legend.position = "right",
    legend.title    = element_text(face = "bold", size = 10),
    legend.text     = element_text(size = 9),
    panel.grid      = element_line(colour = "grey92"),
    axis.text       = element_blank()
  )

ggsave("h2_fig_regime_map.png", fig_map, width = 11, height = 8, dpi = 200)
message("✓ Saved: h2_fig_regime_map.png")


# ── 6. Quadrant scatter plot ─────────────────────────────────────
cor_plot_df <- regime_df %>%
  dplyr::filter(has_data) %>%
  dplyr::mutate(
    regime_label = if_else(is.na(regime_label), "No data", regime_label)
  )

fig_cor <- ggplot(cor_plot_df,
                  aes(x = cor_us, y = cor_china, colour = regime_label)) +
  annotate("rect", xmin =  0, xmax =  Inf, ymin =  0, ymax =  Inf,
           fill = "#1B5E20", alpha = 0.07) +
  annotate("rect", xmin =  0, xmax =  Inf, ymin = -Inf, ymax =  0,
           fill = "#1565C0", alpha = 0.07) +
  annotate("rect", xmin = -Inf, xmax =  0, ymin =  0, ymax =  Inf,
           fill = "#C62828", alpha = 0.07) +
  annotate("rect", xmin = -Inf, xmax =  0, ymin = -Inf, ymax =  0,
           fill = "#F57F17", alpha = 0.07) +
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "grey55", linewidth = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed",
             colour = "grey55", linewidth = 0.7) +
  annotate("text", x =  0.75, y =  0.92, label = "R00", size = 4.5,
           colour = "#1B5E20", fontface = "bold") +
  annotate("text", x =  0.75, y = -0.92, label = "R10", size = 4.5,
           colour = "#1565C0", fontface = "bold") +
  annotate("text", x = -0.75, y =  0.92, label = "R01", size = 4.5,
           colour = "#C62828", fontface = "bold") +
  annotate("text", x = -0.75, y = -0.92, label = "R11", size = 4.5,
           colour = "#F57F17", fontface = "bold") +
  geom_point(size = 3.8, alpha = 0.9) +
  geom_label_repel(
    aes(label = country_clean),
    size          = 2.8,
    box.padding   = 0.4,
    point.padding = 0.25,
    segment.size  = 0.35,
    show.legend   = FALSE,
    fill          = alpha("white", 0.78),
    label.size    = 0.2,
    max.overlaps  = 20
  ) +
  scale_colour_manual(
    values = c(
      "R00: H&M active for both"       = "#1B5E20",
      "R10: H&M active for US only"    = "#1565C0",
      "R01: H&M active for China only" = "#C62828",
      "R11: H&M inactive for both"     = "#F57F17",
      "No data"                        = "#BDBDBD"
    ),
    name = "Regime"
  ) +
  scale_x_continuous(limits = c(-1, 1),
                     breaks = seq(-1, 1, 0.5),
                     labels = number_format(accuracy = 0.1)) +
  scale_y_continuous(limits = c(-1, 1),
                     breaks = seq(-1, 1, 0.5),
                     labels = number_format(accuracy = 0.1)) +
  labs(
    title    = "Within-Country Aid-Favorability Correlations by Donor",
    subtitle = paste0(
      "Each point = one country (Pearson r, 2002\u20132018) | ",
      "Quadrant position determines regime assignment\n",
      "NA = no China aid variation \u2192 treated as non-complying"
    ),
    x = "Cor(Log US Aid,  US Favorability)",
    y = "Cor(Log China Aid,  China Favorability)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(size = 9, colour = "grey40"),
    legend.position  = "right",
    panel.grid.minor = element_blank()
  )

ggsave("h2_fig_regime_scatter.png", fig_cor, width = 9, height = 7, dpi = 200)
message("✓ Saved: h2_fig_regime_scatter.png")

message("\n=== Regime classification complete ===")
message("Outputs:")
message("  h2_regime_classification.csv   — table for paper appendix")
message("  h2_fig_regime_map.png          — choropleth map")
message("  h2_fig_regime_scatter.png      — quadrant scatter plot")

