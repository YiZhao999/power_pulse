# =============================================================================
# Visualizing Pivotality Tercile Classification
# Four plots:
#   (1) World map colored by tercile
#   (2) Scatter plot: alignment volatility vs alignment differential
#   (3) Dot plot of all countries ranked by volatility
#   (4) Tercile boundary density plot
# =============================================================================

library(tidyverse)
library(ggplot2)
library(ggrepel)    # non-overlapping labels: install.packages("ggrepel")
library(maps)       # world map data:         install.packages("maps")
library(viridis)    # color scales

# Color palette consistent across all plots
tercile_colors <- c(
  "T1: Stable"   = "#2166ac",   # blue
  "T2: Pivotal"  = "#f4a582",   # orange
  "T3: Volatile" = "#d6604d"    # red
)

# =============================================================================
# PLOT 1: World Map colored by tercile
# =============================================================================

world_map <- map_data("world")

# Standardize some country name mismatches between your data and map_data
country_pivot_map <- country_pivot %>%
  mutate(Countryname = case_when(
    Countryname == "United States"       ~ "USA",
    Countryname == "United Kingdom"      ~ "UK",
    Countryname == "South Korea"         ~ "South Korea",
    Countryname == "North Korea"         ~ "North Korea",
    Countryname == "Central African Republic" ~ "Central African Republic",
    Countryname == "Dominican Republic"  ~ "Dominican Republic",
    Countryname == "Papua New Guinea"    ~ "Papua New Guinea",
    Countryname == "South Sudan"         ~ "South Sudan",
    Countryname == "Cape Verde"          ~ "Cabo Verde",
    Countryname == "North Macedonia"     ~ "North Macedonia",
    Countryname == "Timor-Leste"         ~ "Timor-Leste",
    TRUE ~ Countryname
  ))

map_df <- world_map %>%
  left_join(
    country_pivot_map %>%
      select(Countryname, tercile_label, mean_pivot_sd, mean_align_diff),
    by = c("region" = "Countryname")
  )

p1 <- ggplot(map_df, aes(x = long, y = lat, group = group, fill = tercile_label)) +
  geom_polygon(color = "white", linewidth = 0.15) +
  scale_fill_manual(
    values = tercile_colors,
    na.value = "grey85",
    name = "Pivotality Tercile",
    labels = c(
      "T1: Stable"   = "T1: Stable (firmly aligned)",
      "T2: Pivotal"  = "T2: Pivotal (moderately competitive)",
      "T3: Volatile" = "T3: Volatile (most competitive)"
    )
  ) +
  coord_fixed(1.3) +
  labs(
    title    = "Country Classification by Alignment Volatility",
    subtitle = "Terciles of 5-year rolling SD of (USAgree − ChinaAgree), 2000–2020",
    caption  = "Grey = not in sample. Volatility proxies pivotality g(ξ*) in the model."
  ) +
  theme_void(base_size = 12) +
  theme(
    legend.position  = "bottom",
    legend.title     = element_text(face = "bold"),
    plot.title       = element_text(face = "bold", size = 14),
    plot.subtitle    = element_text(color = "grey40", size = 10),
    plot.caption     = element_text(color = "grey50", size = 8),
    plot.margin      = margin(10, 10, 10, 10)
  )

ggsave("fig_tercile_map.pdf", p1, width = 12, height = 7)
print(p1)

# =============================================================================
# PLOT 2: Scatter — alignment volatility vs alignment differential
# Label key countries
# =============================================================================

# Countries to label (theoretically interesting)
label_countries <- c(
  "United States", "China", "Israel", "Ukraine", "Russia",
  "France", "United Kingdom", "Japan", "Nauru", "Palau",
  "North Korea", "Iran", "Cuba", "South Korea",
  "Central African Republic", "Haiti", "Nigeria", "Germany"
)

scatter_df <- country_pivot %>%
  mutate(label = ifelse(Countryname %in% label_countries, Countryname, ""))

p2 <- ggplot(scatter_df,
             aes(x = mean_align_diff, y = mean_pivot_sd,
                 color = tercile_label, label = label)) +
  # Tercile boundary lines
  geom_hline(
    yintercept = quantile(country_pivot$mean_pivot_sd,
                          probs = c(1/3, 2/3), na.rm = TRUE),
    linetype = "dashed", color = "grey60", linewidth = 0.5
  ) +
  geom_vline(xintercept = 0, linetype = "dotted",
             color = "grey40", linewidth = 0.5) +
  # Points
  geom_point(size = 2.5, alpha = 0.8) +
  # Labels
  geom_text_repel(
    size          = 3,
    max.overlaps  = 20,
    box.padding   = 0.4,
    segment.color = "grey60",
    segment.size  = 0.3,
    show.legend   = FALSE
  ) +
  scale_color_manual(values = tercile_colors, name = "Tercile") +
  scale_x_continuous(
    breaks = seq(-1, 1, by = 0.25),
    labels = scales::label_number(accuracy = 0.25)
  ) +
  labs(
    title    = "Alignment Volatility vs. Alignment Differential",
    subtitle = "Each point = one country, averaged over 2000–2020",
    x        = "Mean alignment differential (USAgree − ChinaAgree)\n← Pro-China                                    Pro-US →",
    y        = "Mean rolling SD of alignment differential\n(pivotality proxy)",
    caption  = "Dashed lines = tercile boundaries. Dotted line = indifference (diff = 0)."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    plot.title      = element_text(face = "bold", size = 13),
    plot.subtitle   = element_text(color = "grey40", size = 10),
    plot.caption    = element_text(color = "grey50", size = 8),
    panel.grid.minor = element_blank()
  )

ggsave("fig_tercile_scatter.pdf", p2, width = 10, height = 7)
print(p2)

# =============================================================================
# PLOT 3: Ranked dot plot — all countries by volatility, colored by tercile
# =============================================================================

dot_df <- country_pivot %>%
  arrange(mean_pivot_sd) %>%
  mutate(rank = row_number())

# Label every country (readable because dots are stacked vertically)
p3 <- ggplot(dot_df,
             aes(x = mean_pivot_sd, y = reorder(Countryname, mean_pivot_sd),
                 color = tercile_label)) +
  geom_vline(
    xintercept = quantile(country_pivot$mean_pivot_sd,
                          probs = c(1/3, 2/3), na.rm = TRUE),
    linetype = "dashed", color = "grey60", linewidth = 0.5
  ) +
  geom_point(size = 2, alpha = 0.85) +
  scale_color_manual(values = tercile_colors, name = "Tercile") +
  labs(
    title    = "Countries Ranked by Alignment Volatility",
    subtitle = "Pivotality proxy: mean 5-year rolling SD of (USAgree − ChinaAgree)",
    x        = "Mean rolling SD of alignment differential (pivotality)",
    y        = NULL,
    caption  = "Dashed lines = tercile boundaries."
  ) +
  theme_minimal(base_size = 8) +
  theme(
    legend.position  = "right",
    axis.text.y      = element_text(size = 6.5),
    plot.title       = element_text(face = "bold", size = 12),
    plot.subtitle    = element_text(color = "grey40", size = 9),
    plot.caption     = element_text(color = "grey50", size = 7),
    panel.grid.major.y = element_line(color = "grey92"),
    panel.grid.minor   = element_blank()
  )

ggsave("fig_tercile_dotplot.pdf", p3, width = 8, height = 14)
print(p3)

# =============================================================================
# PLOT 4: Density plot showing tercile boundaries
# =============================================================================

# Tercile cutoffs
t_cuts <- quantile(country_pivot$mean_pivot_sd,
                   probs = c(1/3, 2/3), na.rm = TRUE)

p4 <- ggplot(country_pivot, aes(x = mean_pivot_sd)) +
  # Shade tercile regions
  annotate("rect",
           xmin = -Inf,       xmax = t_cuts[1],
           ymin = -Inf,       ymax = Inf,
           fill = "#2166ac",  alpha = 0.08) +
  annotate("rect",
           xmin = t_cuts[1],  xmax = t_cuts[2],
           ymin = -Inf,       ymax = Inf,
           fill = "#f4a582",  alpha = 0.08) +
  annotate("rect",
           xmin = t_cuts[2],  xmax = Inf,
           ymin = -Inf,       ymax = Inf,
           fill = "#d6604d",  alpha = 0.08) +
  geom_density(fill = "grey30", alpha = 0.25, color = "grey30") +
  geom_rug(aes(color = tercile_label), alpha = 0.7, linewidth = 0.8) +
  geom_vline(xintercept = t_cuts,
             linetype = "dashed", color = "grey40", linewidth = 0.7) +
  # Tercile labels
  annotate("text", x = mean(c(-Inf, t_cuts[1])),
           y = Inf, label = "T1\nStable",
           vjust = 1.5, color = "#2166ac", fontface = "bold", size = 4) +
  annotate("text", x = mean(t_cuts),
           y = Inf, label = "T2\nPivotal",
           vjust = 1.5, color = "#d4813a", fontface = "bold", size = 4) +
  annotate("text", x = mean(c(t_cuts[2], max(country_pivot$mean_pivot_sd))),
           y = Inf, label = "T3\nVolatile",
           vjust = 1.5, color = "#d6604d", fontface = "bold", size = 4) +
  scale_color_manual(values = tercile_colors, guide = "none") +
  scale_x_continuous(breaks = seq(0, 0.35, by = 0.05)) +
  labs(
    title    = "Distribution of Alignment Volatility Across Countries",
    subtitle = "Rug marks show individual countries; shaded regions = tercile boundaries",
    x        = "Mean rolling SD of alignment differential (pivotality proxy)",
    y        = "Density",
    caption  = "Right-skewed distribution driven by small island states (Nauru, Tuvalu, Palau)."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "grey40", size = 10),
    plot.caption  = element_text(color = "grey50", size = 8),
    panel.grid.minor = element_blank()
  )

ggsave("fig_tercile_density.pdf", p4, width = 9, height = 5)
print(p4)
