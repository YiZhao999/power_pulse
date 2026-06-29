# =============================================================================
# H2 REVISED ANALYSIS — final version
#
# Dependent variable: proportion favorable = (n_fav) / n_valid
#   Constructed from Latinobarometro micro-data (merged_opinion_mapped.csv)
#   Original survey question: "How do you view [country]?"
#     1 = Very good, 2 = Good, 3 = Bad, 4 = Very bad
#     -1 = Don't know, -2 = No answer (excluded from denominator)
#   "Favorable" = responses 1 or 2; aggregated to country-year level in Python.
#   Output: favorability_country_year.csv
#     Columns: country, year,
#              us_n_valid, us_n_fav, us_n_unfav, us_prop_fav, us_net_fav,
#              china_n_valid, china_n_fav, china_n_unfav, china_prop_fav, china_net_fav
#
# Aid decomposition (per advisor):
#   AnyAid  = 1 if aid > 0 (presence margin, full sample)
#   IntAid  = log(aid) if aid > 0, else NA (intensity margin, pos-aid only)
#   log(aid+1) retained as robustness check only
#
# Models estimated in stages (per advisor):
#   M1: AnyAid  × Corruption            — presence margin, full sample
#   M2: IntAid  × Corruption            — intensity margin, pos-aid subsample
#   M3: AnyAid + IntAid × Corruption    — combined, pos-aid subsample
#   M_rob: log(aid+1) × Corruption      — robustness / appendix only
#
# Corruption = Control of Corruption Index (WB WGI)
#   Higher values = better governance = lower diversion risk
#   Used as researcher-side proxy for diversion likelihood (not direct obs)
# H2 prediction: interaction coefficient > 0
#   (better governance amplifies the favorability return on aid)
#
# Sample: Latin America, Latinobarometro waves 2001–2018
#   Sample choice is a data-quality decision: Latinobarometro provides the
#   most consistent repeated cross-national favorability time series available.
# =============================================================================

# ── 0. Packages ───────────────────────────────────────────────────────────────
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(tidyverse, fixest, modelsummary, patchwork, scales)

# ── 1. Load data ──────────────────────────────────────────────────────────────

# Favorability data (from Python aggregation of Latinobarometro micro-data)
df_fav <- read_csv(
  "~/Desktop/SPRING2026/MA_paper/0329/favorability_country_year.csv",
  show_col_types = FALSE
)

# Main panel dataset (contains aid, corruption, and other controls)
df_panel <- read_csv(
  "~/Desktop/SPRING2026/MA_paper/0329/final_merged_dataset.csv",
  show_col_types = FALSE
)

# ── 2. Merge favorability into panel ─────────────────────────────────────────
# Drop old fav_us / fav_china columns from the panel (min-max rescaled versions)
# and replace with the proportion-favorable measures from the Python aggregation.

df_panel <- df_panel %>%
  select(-any_of(c("fav_us", "fav_china",
                   "fav_us_r", "fav_china_r")))   # drop old DV columns

df <- df_panel %>%
  left_join(
    df_fav %>% select(country, year,
                      us_prop_fav, us_net_fav,
                      us_n_valid, us_n_fav, us_n_unfav,
                      china_prop_fav, china_net_fav,
                      china_n_valid, china_n_fav, china_n_unfav),
    by = c("country", "year")
  )

# ── 3. Construct aid decomposition and controls ───────────────────────────────
df <- df %>%
  mutate(
    # --- Presence indicators (full sample) ---
    AnyAid_us    = as.integer(aid_us    > 0),
    AnyAid_china = as.integer(aid_china > 0),
    
    # --- Intensity: log(aid), defined only for positive-aid observations ---
    # NA for zero-aid obs; used only in positive-aid subsample models (M2, M3)
    IntAid_us    = if_else(aid_us    > 0, log(aid_us),    NA_real_),
    IntAid_china = if_else(aid_china > 0, log(aid_china), NA_real_),
    
    # --- Robustness only: old log(y+1) transformation ---
    log_aid_us    = log(aid_us    + 1),
    log_aid_china = log(aid_china + 1),
    
    country = as.factor(country),
    year    = as.factor(year)
  )

# Positive-aid subsamples for M2 and M3
df_pos_us    <- df %>% filter(aid_us    > 0)
df_pos_china <- df %>% filter(aid_china > 0)

# ── 4. Descriptive statistics ─────────────────────────────────────────────────
desc_vars <- c("us_prop_fav", "china_prop_fav",
               "us_net_fav",  "china_net_fav",
               "AnyAid_us",   "AnyAid_china",
               "log_aid_us",  "log_aid_china",
               "corruption")

desc_labels <- c(
  us_prop_fav    = "Proportion Favorable: US (0-1)",
  china_prop_fav = "Proportion Favorable: China (0-1)",
  us_net_fav     = "Net Favorability: US (prop_fav - prop_unfav)",
  china_net_fav  = "Net Favorability: China (prop_fav - prop_unfav)",
  AnyAid_us      = "Any US Aid (binary)",
  AnyAid_china   = "Any China Aid (binary)",
  log_aid_us     = "Log US Aid + 1 (robustness only)",
  log_aid_china  = "Log China Aid + 1 (robustness only)",
  corruption     = "Control of Corruption Index (-2.5 to 2.5)"
)

desc_tbl <- df %>%
  select(any_of(desc_vars)) %>%
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
  mutate(Variable = case_match(
    Variable,
    "us_prop_fav"    ~ "Proportion Favorable: US (0-1)",
    "china_prop_fav" ~ "Proportion Favorable: China (0-1)",
    "us_net_fav"     ~ "Net Favorability: US (prop_fav - prop_unfav)",
    "china_net_fav"  ~ "Net Favorability: China (prop_fav - prop_unfav)",
    "AnyAid_us"      ~ "Any US Aid (binary)",
    "AnyAid_china"   ~ "Any China Aid (binary)",
    "log_aid_us"     ~ "Log US Aid + 1 (robustness only)",
    "log_aid_china"  ~ "Log China Aid + 1 (robustness only)",
    "corruption"     ~ "Control of Corruption Index (-2.5 to 2.5)",
    .default = Variable   # leaves any unlisted variables unchanged
  ))

message("\n=== Descriptive Statistics ===")
print(desc_tbl, n = Inf)

# ── 5. Regression models — staged per advisor ─────────────────────────────────

# -------------------------------------------------------------------------
# US MODELS
# -------------------------------------------------------------------------

# M1-US: Presence margin
# Full sample. Does corruption moderate whether having any US aid
# translates into higher favorability?
m1_us <- feols(
  us_prop_fav ~ AnyAid_us * corruption | country + year,
  data    = df,
  cluster = ~country
)

# M2-US: Intensity margin (positive-aid subsample only)
# Conditional on the US giving aid, does corruption moderate whether
# larger volumes raise favorability?
m2_us <- feols(
  us_prop_fav ~ IntAid_us * corruption | country + year,
  data    = df_pos_us,
  cluster = ~country
)

# M3-US: Combined decomposition (positive-aid subsample)
# Both presence and intensity interacted with corruption.
# AnyAid_us is constant = 1 in df_pos_us, so its main effect is absorbed
# by the country FE; the interaction AnyAid_us:corruption still identifies
# the moderating effect of corruption on the presence margin within
# positive-aid obs. Interpret with caution — see advisor note on collinearity.
m3_us <- feols(
  us_prop_fav ~ AnyAid_us * corruption + IntAid_us * corruption | country + year,
  data    = df_pos_us,
  cluster = ~country
)

# M_rob-US: Robustness — log(aid+1), appendix only
m_rob_us <- feols(
  us_prop_fav ~ log_aid_us * corruption | country + year,
  data    = df,
  cluster = ~country
)

# -------------------------------------------------------------------------
# CHINA MODELS
# -------------------------------------------------------------------------

m1_china <- feols(
  china_prop_fav ~ AnyAid_china * corruption | country + year,
  data    = df,
  cluster = ~country
)

m2_china <- feols(
  china_prop_fav ~ IntAid_china * corruption | country + year,
  data    = df_pos_china,
  cluster = ~country
)

m3_china <- feols(
  china_prop_fav ~ AnyAid_china * corruption + IntAid_china * corruption | country + year,
  data    = df_pos_china,
  cluster = ~country
)

m_rob_china <- feols(
  china_prop_fav ~ log_aid_china * corruption | country + year,
  data    = df,
  cluster = ~country
)

# ── 6. Print regression tables ────────────────────────────────────────────────

message("\n=== US Favorability Models ===")
etable(m1_us, m2_us, m3_us,
       headers     = c("M1: Presence", "M2: Intensity (pos-aid)", "M3: Combined"),
       coefstat    = "se",
       signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10))

message("\n=== China Favorability Models ===")
etable(m1_china, m2_china, m3_china,
       headers     = c("M1: Presence", "M2: Intensity (pos-aid)", "M3: Combined"),
       coefstat    = "se",
       signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10))

# ── 7. Export regression tables ───────────────────────────────────────────────

shared_notes_us <- paste(
  "Two-way fixed effects (country + year). Standard errors clustered by country.",
  "Dependent variable: proportion of respondents rating the US as 'Good' or 'Very good'",
  "(Latinobarometro; -1/Don't know and -2/No answer excluded from denominator).",
  "Control of Corruption Index (WB WGI): higher = better governance = lower diversion risk.",
  "Used as researcher-side proxy for likelihood of aid diversion, not direct observation.",
  "M2 and M3 estimated on positive-aid subsample only.",
  "H2 prediction: positive interaction coefficient (aid effect stronger where governance is better)."
)

shared_notes_cn <- gsub("the US", "China", shared_notes_us)

modelsummary(
  list(
    "M1: Presence"         = m1_us,
    "M2: Intensity"        = m2_us,
    "M3: Combined"         = m3_us,
    "Robustness: log(y+1)" = m_rob_us
  ),
  stars = c("*" = .10, "**" = .05, "***" = .01),
  coef_rename = c(
    "AnyAid_us"             = "Any US Aid",
    "IntAid_us"             = "Log US Aid (pos. obs. only)",
    "corruption"            = "Control of Corruption",
    "AnyAid_us:corruption"  = "Any US Aid × Corruption",
    "IntAid_us:corruption"  = "Log US Aid × Corruption",
    "log_aid_us"            = "Log US Aid + 1",
    "log_aid_us:corruption" = "Log US Aid + 1 × Corruption"
  ),
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  notes   = shared_notes_us,
  output  = "h2_us_regression_table.txt"
)
message("✓ Saved: h2_us_regression_table.txt")

modelsummary(
  list(
    "M1: Presence"         = m1_china,
    "M2: Intensity"        = m2_china,
    "M3: Combined"         = m3_china,
    "Robustness: log(y+1)" = m_rob_china
  ),
  stars = c("*" = .10, "**" = .05, "***" = .01),
  coef_rename = c(
    "AnyAid_china"              = "Any China Aid",
    "IntAid_china"              = "Log China Aid (pos. obs. only)",
    "corruption"                = "Control of Corruption",
    "AnyAid_china:corruption"   = "Any China Aid × Corruption",
    "IntAid_china:corruption"   = "Log China Aid × Corruption",
    "log_aid_china"             = "Log China Aid + 1",
    "log_aid_china:corruption"  = "Log China Aid + 1 × Corruption"
  ),
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  notes   = shared_notes_cn,
  output  = "h2_china_regression_table.txt"
)
message("✓ Saved: h2_china_regression_table.txt")

# ── 8. Marginal effects plots ─────────────────────────────────────────────────

theme_paper <- theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(size = 10, colour = "grey40"),
    legend.position  = "bottom",
    panel.grid.minor = element_blank()
  )

pal_donor <- c("United States" = "#1565C0", "China" = "#C62828")

corr_seq <- seq(
  min(df$corruption, na.rm = TRUE),
  max(df$corruption, na.rm = TRUE),
  length.out = 120
)

# Helper: build ME data frame from a fitted model and variable name
make_me_df <- function(model_us, model_cn, var_us, var_cn) {
  b_us <- coef(model_us)
  b_cn <- coef(model_cn)
  tibble(
    corruption      = rep(corr_seq, 2),
    marginal_effect = c(
      b_us[var_us] + b_us[paste0(var_us, ":corruption")] * corr_seq,
      b_cn[var_cn] + b_cn[paste0(var_cn, ":corruption")] * corr_seq
    ),
    Donor = rep(c("United States", "China"), each = 120)
  )
}

# Shared background / annotation layer
me_background <- list(
  annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf,
           fill = "#FFEBEE", alpha = 0.35),
  annotate("rect", xmin = 0, xmax = Inf, ymin = -Inf, ymax = Inf,
           fill = "#E8F5E9", alpha = 0.35),
  geom_hline(yintercept = 0, linetype = "dashed",
             colour = "grey55", linewidth = 0.8),
  scale_colour_manual(values = pal_donor)
)

# --- Fig A: Presence margin (M1) ---
me_pres <- make_me_df(m1_us, m1_china, "AnyAid_us", "AnyAid_china")

y_ann_pres <- max(abs(me_pres$marginal_effect), na.rm = TRUE) * 0.85

fig_me_presence <- ggplot(me_pres,
                          aes(corruption, marginal_effect, colour = Donor)) +
  me_background +
  geom_line(linewidth = 1.3) +
  annotate("text", x = -1.2, y = y_ann_pres,
           label = "More corrupt", colour = "#B71C1C",
           size = 3.5, fontface = "italic") +
  annotate("text", x = 1.1, y = y_ann_pres,
           label = "Less corrupt", colour = "#1B5E20",
           size = 3.5, fontface = "italic") +
  labs(
    title    = "Marginal Effect of Donor Presence (Any Aid) on Proportion Favorable",
    subtitle = "M1: presence margin | country + year FEs | SEs clustered by country\nH2 prediction: positive slope rising left to right",
    x        = "Control of Corruption Index (higher = better governance)",
    y        = "ME of Any Aid on Proportion Favorable",
    colour   = "Donor"
  ) +
  theme_paper

ggsave("h2_fig_me_presence.png", fig_me_presence,
       width = 10, height = 5.5, dpi = 200)
message("✓ Saved: h2_fig_me_presence.png")

# --- Fig B: Intensity margin (M2) ---
me_int <- make_me_df(m2_us, m2_china, "IntAid_us", "IntAid_china")

y_ann_int <- max(abs(me_int$marginal_effect), na.rm = TRUE) * 0.85

fig_me_intensity <- ggplot(me_int,
                           aes(corruption, marginal_effect, colour = Donor)) +
  me_background +
  geom_line(linewidth = 1.3) +
  annotate("text", x = -1.2, y = y_ann_int,
           label = "More corrupt", colour = "#B71C1C",
           size = 3.5, fontface = "italic") +
  annotate("text", x = 1.1, y = y_ann_int,
           label = "Less corrupt", colour = "#1B5E20",
           size = 3.5, fontface = "italic") +
  labs(
    title    = "Marginal Effect of Aid Intensity on Proportion Favorable (Positive-Aid Subsample)",
    subtitle = "M2: intensity margin | country + year FEs | SEs clustered by country\nH2 prediction: positive slope rising left to right",
    x        = "Control of Corruption Index (higher = better governance)",
    y        = "ME of Log Aid on Proportion Favorable",
    colour   = "Donor"
  ) +
  theme_paper

ggsave("h2_fig_me_intensity.png", fig_me_intensity,
       width = 10, height = 5.5, dpi = 200)
message("✓ Saved: h2_fig_me_intensity.png")

# ── 9. Distribution and quartile plots ───────────────────────────────────────

# Fig C: Distribution of proportion favorable
fav_long <- df %>%
  select(us_prop_fav, china_prop_fav) %>%
  pivot_longer(everything(),
               names_to  = "Donor",
               values_to = "Favorability") %>%
  mutate(Donor = recode(Donor,
                        us_prop_fav    = "United States",
                        china_prop_fav = "China"))

fav_means <- fav_long %>%
  group_by(Donor) %>%
  summarise(m = mean(Favorability, na.rm = TRUE), .groups = "drop")

fig_fav_dist <- ggplot(fav_long,
                       aes(Favorability, fill = Donor, colour = Donor)) +
  geom_density(alpha = 0.30, linewidth = 0.9) +
  geom_vline(data = fav_means, aes(xintercept = m, colour = Donor),
             linetype = "dashed", linewidth = 0.9) +
  scale_fill_manual(values = pal_donor) +
  scale_colour_manual(values = pal_donor) +
  scale_x_continuous(limits = c(0, 1),
                     labels = label_number(accuracy = 0.1)) +
  labs(
    title    = "Distribution of Public Favorability toward the US and China",
    subtitle = "Proportion rating donor as 'Good' or 'Very good' (Latinobarometro) | Dashed = group mean",
    x        = "Proportion Favorable (0 = none, 1 = all valid respondents)",
    y        = "Density",
    fill     = "Donor",
    colour   = "Donor"
  ) +
  theme_paper

ggsave("h2_fig_fav_distribution.png", fig_fav_dist,
       width = 9, height = 5, dpi = 200)
message("✓ Saved: h2_fig_fav_distribution.png")

# Fig D: US — aid presence vs. proportion favorable, by governance quartile
fig_us_quartile <- df %>%
  filter(!is.na(corruption), !is.na(AnyAid_us), !is.na(us_prop_fav)) %>%
  mutate(
    corr_q     = ntile(corruption, 4),
    corr_label = factor(corr_q, labels = c(
      "Q1 - Most Corrupt", "Q2", "Q3", "Q4 - Least Corrupt"))
  ) %>%
  ggplot(aes(AnyAid_us, us_prop_fav, colour = corr_label)) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.2) +
  scale_colour_manual(
    values = c("Q1 - Most Corrupt"  = "#B71C1C",
               "Q2"                 = "#EF9A9A",
               "Q3"                 = "#66BB6A",
               "Q4 - Least Corrupt" = "#1B5E20"),
    name = "Governance Quartile\n(Control of Corruption)"
  ) +
  labs(
    title    = "US Aid Presence and Proportion Favorable by Governance Quartile",
    subtitle = "H2 prediction: steeper positive slope in Q4 (least corrupt)",
    x        = "Any US Aid (0 = no aid, 1 = positive aid)",
    y        = "Proportion Favorable toward US"
  ) +
  theme_paper

ggsave("h2_fig_us_quartile_slopes.png", fig_us_quartile,
       width = 8, height = 5.5, dpi = 200)
message("✓ Saved: h2_fig_us_quartile_slopes.png")

# Fig E: China — aid presence vs. proportion favorable, by governance quartile
fig_cn_quartile <- df %>%
  filter(!is.na(corruption), !is.na(AnyAid_china), !is.na(china_prop_fav)) %>%
  mutate(
    corr_q     = ntile(corruption, 4),
    corr_label = factor(corr_q, labels = c(
      "Q1 - Most Corrupt", "Q2", "Q3", "Q4 - Least Corrupt"))
  ) %>%
  ggplot(aes(AnyAid_china, china_prop_fav, colour = corr_label)) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.2) +
  scale_colour_manual(
    values = c("Q1 - Most Corrupt"  = "#B71C1C",
               "Q2"                 = "#EF9A9A",
               "Q3"                 = "#66BB6A",
               "Q4 - Least Corrupt" = "#1B5E20"),
    name = "Governance Quartile\n(Control of Corruption)"
  ) +
  labs(
    title    = "China Aid Presence and Proportion Favorable by Governance Quartile",
    subtitle = "H2 prediction: steeper positive slope in Q4 (least corrupt)",
    x        = "Any China Aid (0 = no aid, 1 = positive aid)",
    y        = "Proportion Favorable toward China"
  ) +
  theme_paper

ggsave("h2_fig_cn_quartile_slopes.png", fig_cn_quartile,
       width = 8, height = 5.5, dpi = 200)
message("✓ Saved: h2_fig_cn_quartile_slopes.png")

message("\n=== H2 Analysis Complete ===")
message("Outputs:")
message("  Tables : h2_us_regression_table.txt")
message("           h2_china_regression_table.txt")
message("  Figures: h2_fig_me_presence.png")
message("           h2_fig_me_intensity.png")
message("           h2_fig_fav_distribution.png")
message("           h2_fig_us_quartile_slopes.png")
message("           h2_fig_cn_quartile_slopes.png")