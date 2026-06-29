##################################################
# 1. Libraries
##################################################

library(tidyverse)
library(tseries)
library(lmtest)
library(sandwich)
library(patchwork)
##################################################
# 2. Load Data
##################################################

nig <- read.csv("~/Desktop/PhD/FALL2025/PS2702/final/1109/nigeria.csv")
align <- read.csv("~/PycharmProjects/MA_paper/20260107/merged.csv")

##################################################
# 3. Rescale Favorability (higher = more favorable)
##################################################

nig <- nig %>%
  mutate(
    fav_us_rescaled = 4 - fav_us,
    fav_china_rescaled = 4 - fav_china
  )

##################################################
# 4. Aggregate to Nigeria-Year Level
##################################################

fav_year <- nig %>%
  group_by(year) %>%
  summarise(
    fav_us = mean(fav_us_rescaled, na.rm = TRUE),
    fav_china = mean(fav_china_rescaled, na.rm = TRUE),
    .groups = "drop"
  )

##################################################
# 5. Keep Nigeria Only from Alignment Data
##################################################

align_nig <- align %>%
  filter(Countryname == "Nigeria")

##################################################
# 6. Merge (Now 1 Row Per Year)
##################################################

df <- fav_year %>%
  inner_join(align_nig, by = "year") %>%
  arrange(year)

##################################################
# 7. Check Structure (IMPORTANT)
##################################################

print(nrow(df))           # should equal number of survey years
print(df %>% count(year)) # every year should be 1

##################################################
# 8. Standardize for Visualization
##################################################

df <- df %>%
  mutate(
    fav_china_z = as.numeric(scale(fav_china)),
    ChinaAgree_z = as.numeric(scale(ChinaAgree)),
    fav_us_z = as.numeric(scale(fav_us)),
    USAgree_z = as.numeric(scale(USAgree))
  )

##################################################
# 9 & 10. Improved Visualization (replace original)
##################################################

library(patchwork)

# ── Shared regime annotation layers ──────────────────────────────────────

pdp_shade <- annotate("rect",
                      xmin = -Inf, xmax = 2015, ymin = -Inf, ymax = Inf,
                      fill = "grey92", alpha = 0.6)

apc_shade <- annotate("rect",
                      xmin = 2015, xmax = Inf, ymin = -Inf, ymax = Inf,
                      fill = "#e8f4f8", alpha = 0.5)

election_line <- geom_vline(
  xintercept = 2015, linetype = "dashed",
  linewidth = 0.7, color = "grey35", alpha = 0.8)

election_label <- annotate("text",
                           x = 2015.15, y = Inf,
                           label = "2015\nElection", hjust = 0, vjust = 1.4,
                           size = 2.7, color = "grey35", fontface = "bold", lineheight = 0.9)

regime_pdp <- annotate("text",
                       x = 2011, y = Inf,
                       label = "PDP", hjust = 0.5, vjust = 1.8,
                       size = 2.8, color = "grey50", fontface = "italic")

regime_apc <- annotate("text",
                       x = 2017.5, y = Inf,
                       label = "APC", hjust = 0.5, vjust = 1.8,
                       size = 2.8, color = "grey50", fontface = "italic")

# ── Shared theme ──────────────────────────────────────────────────────────

viz_theme <- theme_minimal(base_size = 11) +
  theme(
    plot.title        = element_text(size = 12, face = "bold", margin = margin(b = 3)),
    plot.subtitle     = element_text(size = 9, color = "grey45", margin = margin(b = 10)),
    axis.title        = element_text(size = 9, color = "grey45"),
    axis.text         = element_text(size = 8, color = "grey50"),
    panel.grid.minor  = element_blank(),
    panel.grid.major  = element_line(color = "grey90", linewidth = 0.4),
    legend.position   = "top",
    legend.justification = "left",
    legend.title      = element_blank(),
    legend.text       = element_text(size = 9),
    legend.key.width  = unit(1.8, "cm"),
    plot.background   = element_rect(fill = "white", color = NA),
    panel.background  = element_rect(fill = "white", color = NA)
  )

# ── China panel ───────────────────────────────────────────────────────────

# Reshape to long for cleaner ggplot mapping
df_china_long <- df %>%
  select(year, fav_china_z, ChinaAgree_z) %>%
  pivot_longer(
    cols      = c(fav_china_z, ChinaAgree_z),
    names_to  = "series",
    values_to = "value"
  ) %>%
  mutate(series = recode(series,
                         "fav_china_z"  = "Citizen Favorability",
                         "ChinaAgree_z" = "UNGA Voting Similarity"
  ))

p_china <- ggplot(df_china_long, aes(x = year, y = value,
                                     color    = series,
                                     linetype = series)) +
  pdp_shade + apc_shade +
  regime_pdp + regime_apc +
  election_line + election_label +
  geom_line(linewidth = 1.1) +
  geom_point(aes(fill = series), shape = 21,
             size = 3, stroke = 0.7, color = "white") +
  scale_color_manual(values = c(
    "Citizen Favorability"   = "#E8873A",
    "UNGA Voting Similarity" = "#2E9E5B"
  )) +
  scale_fill_manual(values = c(
    "Citizen Favorability"   = "#E8873A",
    "UNGA Voting Similarity" = "#2E9E5B"
  )) +
  scale_linetype_manual(values = c(
    "Citizen Favorability"   = "solid",
    "UNGA Voting Similarity" = "longdash"
  )) +
  scale_x_continuous(breaks = seq(2008, 2020, by = 2)) +
  labs(
    title    = "Alignment with China",
    subtitle = "Parallel trends under PDP; diverge after 2015 transition",
    x = NULL, y = "Standardized Value (z-score)"
  ) +
  viz_theme +
  guides(
    color    = guide_legend(override.aes = list(shape = NA, linewidth = 1.3)),
    linetype = guide_legend(override.aes = list(shape = NA, linewidth = 1.3)),
    fill     = "none"
  )

# ── US panel ──────────────────────────────────────────────────────────────

df_us_long <- df %>%
  select(year, fav_us_z, USAgree_z) %>%
  pivot_longer(
    cols      = c(fav_us_z, USAgree_z),
    names_to  = "series",
    values_to = "value"
  ) %>%
  mutate(series = recode(series,
                         "fav_us_z"  = "Citizen Favorability",
                         "USAgree_z" = "UNGA Voting Similarity"
  ))

p_us <- ggplot(df_us_long, aes(x = year, y = value,
                               color    = series,
                               linetype = series)) +
  pdp_shade + apc_shade +
  regime_pdp + regime_apc +
  election_line + election_label +
  geom_line(linewidth = 1.1) +
  geom_point(aes(fill = series), shape = 21,
             size = 3, stroke = 0.7, color = "white") +
  scale_color_manual(values = c(
    "Citizen Favorability"   = "#C0392B",
    "UNGA Voting Similarity" = "#2471A3"
  )) +
  scale_fill_manual(values = c(
    "Citizen Favorability"   = "#C0392B",
    "UNGA Voting Similarity" = "#2471A3"
  )) +
  scale_linetype_manual(values = c(
    "Citizen Favorability"   = "solid",
    "UNGA Voting Similarity" = "longdash"
  )) +
  scale_x_continuous(breaks = seq(2008, 2020, by = 2)) +
  labs(
    title    = "Alignment with United States",
    subtitle = "Parallel trends become salient under APC post-2015",
    x = NULL, y = "Standardized Value (z-score)"
  ) +
  viz_theme +
  guides(
    color    = guide_legend(override.aes = list(shape = NA, linewidth = 1.3)),
    linetype = guide_legend(override.aes = list(shape = NA, linewidth = 1.3)),
    fill     = "none"
  )

# ── Combine and save ──────────────────────────────────────────────────────

p_final <- p_china + p_us +
  plot_annotation(
    title   = "Nigeria: Citizen Favorability vs. UNGA Voting Similarity",
    caption = paste(
      "Grey shading = PDP era (pre-2015); blue shading = APC era (post-2015).",
      "Solid line = citizen favorability (z-scored); dashed line = UNGA voting similarity (z-scored).",
      sep = "\n"
    ),
    theme = theme(
      plot.title   = element_text(size = 14, face = "bold"),
      plot.caption = element_text(size = 7.5, color = "grey50",
                                  hjust = 0, margin = margin(t = 8))
    )
  )

print(p_final)

ggsave("nigeria_alignment.pdf", p_final,
       width = 11, height = 5, device = cairo_pdf)
ggsave("nigeria_alignment.png", p_final,
       width = 11, height = 5, dpi = 300)

##################################################
# 11. Stationarity Tests
##################################################

adf.test(df$fav_us)
adf.test(df$USAgree)

adf.test(df$fav_china)
adf.test(df$ChinaAgree)

##################################################
# 12. Create Lag Structure
##################################################

df <- df %>%
  mutate(
    USAgree_l1 = lag(USAgree),
    ChinaAgree_l1 = lag(ChinaAgree),
    fav_us_l1 = lag(fav_us),
    fav_china_l1 = lag(fav_china)
  )

##################################################
# 13. Dynamic Model (Preferred)
##################################################

m_us_dyn <- lm(USAgree ~ USAgree_l1 + fav_us_l1, data = df)
coeftest(m_us_dyn, vcov = NeweyWest(m_us_dyn))

m_china_dyn <- lm(ChinaAgree ~ ChinaAgree_l1 + fav_china_l1, data = df)
coeftest(m_china_dyn, vcov = NeweyWest(m_china_dyn))

##################################################
# 14. First Difference Model
##################################################

df <- df %>%
  mutate(
    d_USAgree = USAgree - lag(USAgree),
    d_ChinaAgree = ChinaAgree - lag(ChinaAgree),
    d_fav_us = fav_us - lag(fav_us),
    d_fav_china = fav_china - lag(fav_china)
  )

m_us_diff <- lm(d_USAgree ~ d_fav_us, data = df)
coeftest(m_us_diff, vcov = NeweyWest(m_us_diff))

m_china_diff <- lm(d_ChinaAgree ~ d_fav_china, data = df)
coeftest(m_china_diff, vcov = NeweyWest(m_china_diff))