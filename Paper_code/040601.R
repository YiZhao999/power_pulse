# =============================================================================
# Visualizing Pivotality Tercile Classification (Mean Absolute Alignment Gap)
# =============================================================================

library(tidyverse)
library(ggplot2)
library(ggrepel)
library(maps)
library(viridis)

tercile_colors <- c(
  "T1: Most Contested"       = "#2166ac",
  "T2: Moderately Contested" = "#f4a582",
  "T3: Stably Aligned"       = "#d6604d"
)

# =============================================================================
# PLOT 1: World Map
# =============================================================================

world_map <- map_data("world")

# Name crosswalk based on your actual country names in the output
country_pivot_map <- country_pivot %>%
  mutate(Countryname = case_when(
    # Your data → maps package name
    Countryname == "United States"            ~ "USA",
    Countryname == "United Kingdom"           ~ "UK",
    Countryname == "Cape Verde"               ~ "Cabo Verde",
    Countryname == "North Macedonia"          ~ "North Macedonia",
    Countryname == "Timor-Leste"              ~ "East Timor",
    Countryname == "South Sudan"              ~ "South Sudan",
    Countryname == "Central African Republic" ~ "Central African Republic",
    Countryname == "Dominican Republic"       ~ "Dominican Republic",
    Countryname == "Papua New Guinea"         ~ "Papua New Guinea",
    Countryname == "Solomon Islands"          ~ "Solomon Islands",
    Countryname == "Trinidad and Tobago"      ~ "Trinidad",
    Countryname == "Congo"                    ~ "Republic of Congo",
    Countryname == "Democratic Republic of the Congo" ~ "Democratic Republic of the Congo",
    Countryname == "United Arab Emirates"     ~ "United Arab Emirates",
    Countryname == "Guinea-Bissau"            ~ "Guinea-Bissau",
    Countryname == "Kyrgyzstan"               ~ "Kyrgyzstan",
    Countryname == "Burkina Faso"             ~ "Burkina Faso",
    TRUE ~ Countryname
  ))

map_df <- world_map %>%
  left_join(
    country_pivot_map %>%
      select(Countryname, tercile_label, mean_abs_gap, mean_align_diff),
    by = c("region" = "Countryname")
  )

p1 <- ggplot(map_df, aes(x = long, y = lat, group = group, fill = tercile_label)) +
  geom_polygon(color = "white", linewidth = 0.15) +
  scale_fill_manual(
    values   = tercile_colors,
    na.value = "grey85",
    name     = "Alignment",
    labels   = c(
      "T1: Most Contested"       = "T1: Most Contested",
      "T2: Moderately Contested" = "T2: Moderately Contested",
      "T3: Stably Aligned"       = "T3: Stably Aligned"
    )
  ) +
  coord_fixed(1.3) +
  labs(
    title    = "Country Classification by Mean Absolute Alignment Gap",
    subtitle = "Terciles of mean |USAgree − ChinaAgree|; T1 = most evenly contested",
    caption  = "Grey = not in sample. T1 includes Western democracies; T3 includes autocracies and US/China themselves."
  ) +
  theme_void(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.title    = element_text(face = "bold"),
    plot.title      = element_text(face = "bold", size = 14),
    plot.subtitle   = element_text(color = "grey40", size = 10),
    plot.caption    = element_text(color = "grey50", size = 8),
    plot.margin     = margin(10, 10, 10, 10)
  )

ggsave("fig_tercile_map.pdf", p1, width = 12, height = 7)
print(p1)

# =============================================================================
# PLOT 2: Scatter — mean absolute gap vs alignment direction
# =============================================================================

# Countries to label based on your actual output
label_countries <- c(
  "France", "Australia", "United Kingdom", "Germany", "Canada",
  "Japan", "South Korea", "Israel", "Turkey", "India",
  "Russia", "China", "United States", "North Korea",
  "Iran", "Cuba", "Syria", "Venezuela", "Zimbabwe",
  "Nauru", "Palau", "Tuvalu", "Vanuatu",
  "Nigeria", "South Africa", "Brazil"
)

scatter_df <- country_pivot %>%
  mutate(label = ifelse(Countryname %in% label_countries, Countryname, ""))

gap_cuts <- quantile(country_pivot$mean_abs_gap, probs = c(1/3, 2/3), na.rm = TRUE)

p2 <- ggplot(scatter_df,
             aes(x = mean_abs_gap, y = mean_align_diff,
                 color = tercile_label, label = label)) +
  geom_vline(
    xintercept = gap_cuts,
    linetype = "dashed", color = "grey60", linewidth = 0.5
  ) +
  geom_hline(yintercept = 0, linetype = "dotted",
             color = "grey40", linewidth = 0.5) +
  geom_point(size = 2.5, alpha = 0.8) +
  geom_text_repel(
    size          = 3,
    max.overlaps  = 25,
    box.padding   = 0.4,
    segment.color = "grey60",
    segment.size  = 0.3,
    show.legend   = FALSE
  ) +
  scale_color_manual(values = tercile_colors, name = "Tercile") +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
  scale_y_continuous(breaks = seq(-1, 1, by = 0.25)) +
  labs(
    title    = "Mean Absolute Alignment Gap vs. Alignment Direction",
    subtitle = "Each point = one country, averaged over sample period",
    x        = "Mean |USAgree − ChinaAgree|\n← Most Contested                              Most Stably Aligned →",
    y        = "Mean alignment differential (USAgree − ChinaAgree)\n← Pro-China                                          Pro-US →",
    caption  = "Vertical dashed lines = tercile boundaries. Note: France and Western democracies cluster in T1 (most contested)."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "right",
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(color = "grey40", size = 10),
    plot.caption     = element_text(color = "grey50", size = 8),
    panel.grid.minor = element_blank()
  )

ggsave("fig_tercile_scatter.pdf", p2, width = 10, height = 7)
print(p2)

# =============================================================================
# PLOT 3: Ranked dot plot
# =============================================================================

dot_df <- country_pivot %>%
  arrange(mean_abs_gap) %>%
  mutate(rank = row_number())

p3 <- ggplot(dot_df,
             aes(x = mean_abs_gap,
                 y = reorder(Countryname, mean_abs_gap),
                 color = tercile_label)) +
  geom_vline(
    xintercept = gap_cuts,
    linetype = "dashed", color = "grey60", linewidth = 0.5
  ) +
  geom_point(size = 2, alpha = 0.85) +
  scale_color_manual(values = tercile_colors, name = "Tercile") +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
  labs(
    title    = "Countries Ranked by Mean Absolute Alignment Gap",
    subtitle = "Left = most evenly contested between US and China; Right = most stably aligned",
    x        = "Mean |USAgree − ChinaAgree|",
    y        = NULL,
    caption  = "Dashed lines = tercile boundaries. France is most contested; China and US are most stably aligned."
  ) +
  theme_minimal(base_size = 8) +
  theme(
    legend.position    = "right",
    axis.text.y        = element_text(size = 6.5),
    plot.title         = element_text(face = "bold", size = 12),
    plot.subtitle      = element_text(color = "grey40", size = 9),
    plot.caption       = element_text(color = "grey50", size = 7),
    panel.grid.major.y = element_line(color = "grey92"),
    panel.grid.minor   = element_blank()
  )

ggsave("fig_tercile_dotplot.pdf", p3, width = 8, height = 14)
print(p3)

# =============================================================================
# PLOT 4: Density plot
# =============================================================================

p4 <- ggplot(country_pivot, aes(x = mean_abs_gap)) +
  annotate("rect",
           xmin = -Inf,        xmax = gap_cuts[1],
           ymin = -Inf,        ymax = Inf,
           fill = "#2166ac",   alpha = 0.08) +
  annotate("rect",
           xmin = gap_cuts[1], xmax = gap_cuts[2],
           ymin = -Inf,        ymax = Inf,
           fill = "#f4a582",   alpha = 0.08) +
  annotate("rect",
           xmin = gap_cuts[2], xmax = Inf,
           ymin = -Inf,        ymax = Inf,
           fill = "#d6604d",   alpha = 0.08) +
  geom_density(fill = "grey30", alpha = 0.25, color = "grey30") +
  geom_rug(aes(color = tercile_label), alpha = 0.7, linewidth = 0.8) +
  geom_vline(xintercept = gap_cuts,
             linetype = "dashed", color = "grey40", linewidth = 0.7) +
  # Annotate notable countries on rug
  geom_text(
    data = country_pivot %>%
      filter(Countryname %in% c("France", "Israel", "China",
                                "United States", "North Korea", "Turkey")),
    aes(x = mean_abs_gap, y = 0, label = Countryname),
    angle = 90, vjust = -0.3, hjust = -0.1,
    size = 2.8, color = "grey30", inherit.aes = FALSE
  ) +
  annotate("text",
           x = mean(c(0, gap_cuts[1])),
           y = Inf, label = "T1\nContested",
           vjust = 1.5, color = "#2166ac", fontface = "bold", size = 4) +
  annotate("text",
           x = mean(gap_cuts),
           y = Inf, label = "T2\nModerate",
           vjust = 1.5, color = "#d4813a", fontface = "bold", size = 4) +
  annotate("text",
           x = mean(c(gap_cuts[2], max(country_pivot$mean_abs_gap, na.rm = TRUE))),
           y = Inf, label = "T3\nAligned",
           vjust = 1.5, color = "#d6604d", fontface = "bold", size = 4) +
  scale_color_manual(values = tercile_colors, guide = "none") +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
  labs(
    title    = "Distribution of Mean Absolute Alignment Gap Across Countries",
    subtitle = "Rug marks = individual countries; shaded regions = tercile boundaries",
    x        = "Mean |USAgree − ChinaAgree|",
    y        = "Density",
    caption  = "Left tail: Western democracies (T1). Right tail: authoritarian states and US/China themselves (T3)."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(color = "grey40", size = 10),
    plot.caption     = element_text(color = "grey50", size = 8),
    panel.grid.minor = element_blank()
  )

ggsave("fig_tercile_density.pdf", p4, width = 9, height = 5)
print(p4)
