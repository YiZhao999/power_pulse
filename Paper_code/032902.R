# ================================================================
#  Hypothesis 2 Analysis
#  "The effect of foreign aid on public favorability toward a donor
#   is more positive where aid is less likely to be diverted,
#   proxied by the Control of Corruption Index (World Bank WGI).
#   Higher values = better governance (less corrupt), range -2.5 to 2.5."
#
#  Dataset: final_merged_dataset.csv
#  N = 247 obs | 19 countries | 2002–2018
#  Key note: higher raw fav_us / fav_china = LESS favourable
#            → variables are REVERSED before analysis
#  H2 prediction: β(Aid × Corruption) > 0
#    (better governance amplifies the aid–favorability return)
# ================================================================

# ── 0. Packages ──────────────────────────────────────────────────
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
  tidyverse,
  fixest,
  modelsummary,
  patchwork,
  scales
)


# ── 1. Load data ─────────────────────────────────────────────────
df_raw <- read_csv("~/Desktop/SPRING2026/MA_paper/0329/final_merged_dataset.csv",
                   show_col_types = FALSE)


# ── 2. Rescale favorability (higher = MORE favourable) ───────────
rescale_fav <- function(x) {
  1 - (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

df <- df_raw %>%
  mutate(
    fav_us_r      = rescale_fav(fav_us),
    fav_china_r   = rescale_fav(fav_china),
    log_aid_us    = log(aid_us    + 1),
    log_aid_china = log(aid_china + 1),
    country       = as.factor(country),
    year          = as.factor(year)
  )

stopifnot(
  all(between(df$fav_us_r,    0, 1), na.rm = TRUE),
  all(between(df$fav_china_r, 0, 1), na.rm = TRUE)
)
message("✓ Favorability rescaling verified: 0 = least favourable, 1 = most favourable")


# ── 3. Descriptive Statistics ─────────────────────────────────────
desc_vars <- c("fav_us_r", "fav_china_r", "log_aid_us", "log_aid_china", "corruption")

desc_labels <- c(
  fav_us_r      = "Favorability: US (rescaled, 0-1)",
  fav_china_r   = "Favorability: China (rescaled, 0-1)",
  log_aid_us    = "Log US Aid",
  log_aid_china = "Log China Aid",
  corruption    = "Control of Corruption Index (-2.5 to 2.5)"
)

desc_tbl <- df %>%
  select(all_of(desc_vars)) %>%
  pivot_longer(everything(), names_to = "Variable") %>%
  group_by(Variable) %>%
  summarise(
    N      = sum(!is.na(value)),
    Mean   = round(mean(value,   na.rm = TRUE), 3),
    SD     = round(sd(value,     na.rm = TRUE), 3),
    Min    = round(min(value,    na.rm = TRUE), 3),
    Median = round(median(value, na.rm = TRUE), 3),
    Max    = round(max(value,    na.rm = TRUE), 3),
    .groups = "drop"
  ) %>%
  mutate(Variable = recode(Variable, !!!desc_labels))

message("\n=== Table 1: Descriptive Statistics ===")
print(desc_tbl, n = Inf)


# ── 4. Visualisations ────────────────────────────────────────────
# Each figure is saved as a separate file.
# Titles contain only the subtitle-level description (no figure numbers)
# so they are ready to be relabelled in your paper.

theme_paper <- theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(size = 10, colour = "grey40"),
    legend.position  = "bottom",
    panel.grid.minor = element_blank()
  )

pal_donor <- c("United States" = "#1565C0", "China" = "#C62828")


# ------------------------------------------------------------------
# Fig A – Distribution of rescaled favorability
# ------------------------------------------------------------------
fav_long <- df %>%
  select(fav_us_r, fav_china_r) %>%
  pivot_longer(everything(), names_to = "Donor", values_to = "Favorability") %>%
  mutate(Donor = recode(Donor,
                        fav_us_r    = "United States",
                        fav_china_r = "China"
  ))

fav_means <- fav_long %>%
  group_by(Donor) %>%
  summarise(m = mean(Favorability, na.rm = TRUE), .groups = "drop")

fig_fav_dist <- ggplot(fav_long, aes(Favorability, fill = Donor, colour = Donor)) +
  geom_density(alpha = 0.30, linewidth = 0.9) +
  geom_vline(
    data     = fav_means,
    aes(xintercept = m, colour = Donor),
    linetype = "dashed", linewidth = 0.9
  ) +
  scale_fill_manual(values   = pal_donor) +
  scale_colour_manual(values = pal_donor) +
  scale_x_continuous(limits = c(0, 1), labels = label_number(accuracy = 0.1)) +
  labs(
    title    = "Distribution of Public Favorability toward the US and China (Rescaled)",
    subtitle = "0 = Least favourable   |   1 = Most favourable   |   Dashed line = group mean",
    x        = "Favorability toward donor (rescaled, 0-1)",
    y        = "Density",
    fill     = "Donor country",
    colour   = "Donor country"
  ) +
  theme_paper

ggsave("h2_fig_fav_distribution.png", fig_fav_dist,
       width = 9, height = 5, dpi = 200)
message("✓ Saved: h2_fig_fav_distribution.png")


# ------------------------------------------------------------------
# Fig B – Distribution of Control of Corruption Index
# ------------------------------------------------------------------
fig_corr_dist <- df %>%
  distinct(country, year, corruption) %>%
  ggplot(aes(corruption)) +
  geom_histogram(bins = 28, fill = "#6A1B9A", colour = "white", alpha = 0.85) +
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "grey40", linewidth = 0.8) +
  labs(
    title    = "Distribution of the Control of Corruption Index",
    subtitle = "Higher values = better governance (less corrupt)   |   Dashed line = 0",
    x        = "Control of Corruption Index (-2.5 to 2.5)",
    y        = "Frequency"
  ) +
  theme_paper

ggsave("h2_fig_corruption_distribution.png", fig_corr_dist,
       width = 7, height = 5, dpi = 200)
message("✓ Saved: h2_fig_corruption_distribution.png")


# ------------------------------------------------------------------
# Fig C – US Aid vs. US Favorability (coloured by corruption)
# ------------------------------------------------------------------
corr_gradient <- scale_colour_gradient2(
  low      = "#B71C1C",   # red   = low governance (more corrupt)
  mid      = "#FFF176",   # yellow = middle
  high     = "#1B5E20",   # green  = high governance (less corrupt)
  midpoint = 0,
  name     = "Control of\nCorruption"
)

fig_us_scatter <- df %>%
  ggplot(aes(log_aid_us, fav_us_r)) +
  geom_point(aes(colour = corruption), alpha = 0.55, size = 2.2) +
  geom_smooth(method = "loess", se = TRUE,
              colour = "#1565C0", fill = "#1565C0",
              alpha = 0.15, linewidth = 1) +
  corr_gradient +
  labs(
    title    = "US Aid and Public Favorability toward the US",
    subtitle = "Each point = country-year observation   |   Colour = Control of Corruption level",
    x        = "Log US Aid (USD + 1)",
    y        = "US Favorability (rescaled, 0-1)"
  ) +
  theme_paper

ggsave("h2_fig_us_aid_fav_scatter.png", fig_us_scatter,
       width = 8, height = 5.5, dpi = 200)
message("✓ Saved: h2_fig_us_aid_fav_scatter.png")


# ------------------------------------------------------------------
# Fig D – China Aid vs. China Favorability (coloured by corruption)
# ------------------------------------------------------------------
fig_cn_scatter <- df %>%
  ggplot(aes(log_aid_china, fav_china_r)) +
  geom_point(aes(colour = corruption), alpha = 0.55, size = 2.2) +
  geom_smooth(method = "loess", se = TRUE,
              colour = "#C62828", fill = "#C62828",
              alpha = 0.15, linewidth = 1) +
  corr_gradient +
  labs(
    title    = "China Aid and Public Favorability toward China",
    subtitle = "Each point = country-year observation   |   Colour = Control of Corruption level",
    x        = "Log China Aid (USD + 1)",
    y        = "China Favorability (rescaled, 0-1)"
  ) +
  theme_paper

ggsave("h2_fig_cn_aid_fav_scatter.png", fig_cn_scatter,
       width = 8, height = 5.5, dpi = 200)
message("✓ Saved: h2_fig_cn_aid_fav_scatter.png")


# ------------------------------------------------------------------
# Fig E – US Aid–Favorability slopes by corruption quartile
# Corrected labels: Q1 = lowest governance score (most corrupt)
#                   Q4 = highest governance score (least corrupt)
# H2 prediction: steepest positive slope in Q4 (best governance)
# ------------------------------------------------------------------
fig_us_quartile <- df %>%
  filter(!is.na(corruption), !is.na(log_aid_us), !is.na(fav_us_r)) %>%
  mutate(
    corr_q = ntile(corruption, 4),
    corr_label = factor(corr_q, labels = c(
      "Q1 - Most Corrupt",
      "Q2",
      "Q3",
      "Q4 - Least Corrupt"
    ))
  ) %>%
  ggplot(aes(log_aid_us, fav_us_r, colour = corr_label)) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.2) +
  scale_colour_manual(
    values = c(
      "Q1 - Most Corrupt"  = "#B71C1C",
      "Q2"                 = "#EF9A9A",
      "Q3"                 = "#66BB6A",
      "Q4 - Least Corrupt" = "#1B5E20"
    ),
    name = "Governance Quartile\n(Control of Corruption)"
  ) +
  labs(
    title    = "US Aid and Favorability by Governance Quartile",
    subtitle = "H2 prediction: steeper positive slope in better-governed settings (Q4)",
    x        = "Log US Aid (USD + 1)",
    y        = "US Favorability (rescaled, 0-1)"
  ) +
  theme_paper

ggsave("h2_fig_us_quartile_slopes.png", fig_us_quartile,
       width = 8, height = 5.5, dpi = 200)
message("✓ Saved: h2_fig_us_quartile_slopes.png")


# ------------------------------------------------------------------
# Fig F – China Aid–Favorability slopes by corruption quartile
# ------------------------------------------------------------------
fig_cn_quartile <- df %>%
  filter(!is.na(corruption), !is.na(log_aid_china), !is.na(fav_china_r)) %>%
  mutate(
    corr_q = ntile(corruption, 4),
    corr_label = factor(corr_q, labels = c(
      "Q1 - Most Corrupt",
      "Q2",
      "Q3",
      "Q4 - Least Corrupt"
    ))
  ) %>%
  ggplot(aes(log_aid_china, fav_china_r, colour = corr_label)) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.2) +
  scale_colour_manual(
    values = c(
      "Q1 - Most Corrupt"  = "#B71C1C",
      "Q2"                 = "#EF9A9A",
      "Q3"                 = "#66BB6A",
      "Q4 - Least Corrupt" = "#1B5E20"
    ),
    name = "Governance Quartile\n(Control of Corruption)"
  ) +
  labs(
    title    = "China Aid and Favorability by Governance Quartile",
    subtitle = "H2 prediction: steeper positive slope in better-governed settings (Q4)",
    x        = "Log China Aid (USD + 1)",
    y        = "China Favorability (rescaled, 0-1)"
  ) +
  theme_paper

ggsave("h2_fig_cn_quartile_slopes.png", fig_cn_quartile,
       width = 8, height = 5.5, dpi = 200)
message("✓ Saved: h2_fig_cn_quartile_slopes.png")


# ── 5. Regression Models ─────────────────────────────────────────
# Two-way FE (country + year) | SEs clustered by country
#
# Interaction: Log Aid × Corruption (Control of Corruption Index)
# CORRECTED H2 prediction: β(Aid × Corruption) > 0
#   Better governance (higher score) amplifies the favorability
#   return on aid; more corrupt settings attenuate or reverse it.

m_us_base <- feols(
  fav_us_r ~ log_aid_us + corruption | country + year,
  data = df, cluster = ~country
)

m_us_int <- feols(
  fav_us_r ~ log_aid_us * corruption | country + year,
  data = df, cluster = ~country
)

m_cn_base <- feols(
  fav_china_r ~ log_aid_china + corruption | country + year,
  data = df, cluster = ~country
)

m_cn_int <- feols(
  fav_china_r ~ log_aid_china * corruption | country + year,
  data = df, cluster = ~country
)


# ── 6. Print & export regression results ─────────────────────────
message("\n=== Table 2: Regression Results ===")
etable(
  m_us_base, m_us_int,
  m_cn_base, m_cn_int,
  headers     = c("US Fav (Base)", "US Fav (Interaction)",
                  "CN Fav (Base)", "CN Fav (Interaction)"),
  coefstat    = "se",
  signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10)
)

modelsummary(
  list(
    "US: Base"        = m_us_base,
    "US: Interaction" = m_us_int,
    "CN: Base"        = m_cn_base,
    "CN: Interaction" = m_cn_int
  ),
  stars       = c("*" = .10, "**" = .05, "***" = .01),
  coef_rename = c(
    "log_aid_us"               = "Log US Aid",
    "log_aid_china"            = "Log China Aid",
    "corruption"               = "Control of Corruption",
    "log_aid_us:corruption"    = "Log US Aid x Control of Corruption",
    "log_aid_china:corruption" = "Log China Aid x Control of Corruption"
  ),
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  notes   = paste(
    "Two-way fixed effects (country + year).",
    "Standard errors clustered by country.",
    "Favorability rescaled: 0 = least favourable, 1 = most favourable.",
    "Control of Corruption: higher = better governance (less corrupt), range -2.5 to 2.5.",
    "H2 prediction: positive interaction coefficient."
  ),
  output = "h2_regression_table.txt"
)
message("✓ Regression table saved to h2_regression_table.txt")


# ── 7. Marginal Effects Plot ──────────────────────────────────────
# ME of log aid on favorability = β_aid + β_(aid×corruption) × corruption
# Traced across the full observed range of the corruption index.
# CORRECTED shading: green = high governance (right side), red = low governance (left)

corr_seq <- seq(
  min(df$corruption, na.rm = TRUE),
  max(df$corruption, na.rm = TRUE),
  length.out = 120
)

b_us <- coef(m_us_int)
b_cn <- coef(m_cn_int)

me_us <- b_us["log_aid_us"]    + b_us["log_aid_us:corruption"]    * corr_seq
me_cn <- b_cn["log_aid_china"] + b_cn["log_aid_china:corruption"] * corr_seq

me_df <- tibble(
  corruption      = rep(corr_seq, 2),
  marginal_effect = c(me_us, me_cn),
  Donor           = rep(c("United States", "China"), each = 120)
)

fig_me <- ggplot(me_df, aes(corruption, marginal_effect, colour = Donor)) +
  # Corrected shading: left (negative) = more corrupt, right (positive) = less corrupt
  annotate("rect",
           xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf,
           fill = "#FFEBEE", alpha = 0.35) +
  annotate("rect",
           xmin = 0, xmax = Inf, ymin = -Inf, ymax = Inf,
           fill = "#E8F5E9", alpha = 0.35) +
  geom_hline(yintercept = 0, linetype = "dashed",
             colour = "grey55", linewidth = 0.8) +
  geom_line(linewidth = 1.3) +
  scale_colour_manual(values = pal_donor) +
  annotate("text", x = -1.2, y = max(c(me_us, me_cn), na.rm = TRUE) * 0.92,
           label = "More corrupt", colour = "#B71C1C",
           size = 3.5, fontface = "italic") +
  annotate("text", x =  1.1, y = max(c(me_us, me_cn), na.rm = TRUE) * 0.92,
           label = "Less corrupt", colour = "#1B5E20",
           size = 3.5, fontface = "italic") +
  labs(
    title    = "Marginal Effect of Aid on Favorability across Governance Levels",
    subtitle = "Interaction model with country + year fixed effects | Clustered SEs by country\nH2 prediction: positive and rising slope (left to right)",
    x        = "Control of Corruption Index (higher = better governance)",
    y        = "Marginal Effect of Log Aid on Favorability",
    colour   = "Donor"
  ) +
  theme_paper

ggsave("h2_fig_marginal_effects.png", fig_me,
       width = 10, height = 5.5, dpi = 200)
message("✓ Saved: h2_fig_marginal_effects.png")

message("\n=== H2 Analysis complete ===")
message("Files saved:")
message("  Figures : h2_fig_fav_distribution.png")
message("            h2_fig_corruption_distribution.png")
message("            h2_fig_us_aid_fav_scatter.png")
message("            h2_fig_cn_aid_fav_scatter.png")
message("            h2_fig_us_quartile_slopes.png")
message("            h2_fig_cn_quartile_slopes.png")
message("            h2_fig_marginal_effects.png")
message("  Table   : h2_regression_table.txt")
