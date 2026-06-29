# =============================================================================
# H3 REVISED ANALYSIS — per advisor comments
#
# Changes from prior draft:
#   1. Framed as a proxy test of the mediated channel, not direct observation
#      of the full theoretical sequence.
#   2. Aid decomposed into AnyAid (presence) and IntAid (intensity, pos-aid only),
#      mirroring the H2 decomposition.
#   3. Heterogeneity in the direct vs. mediated channel incorporated via:
#        (a) interactions with corruption in the lagged regression
#        (b) split-sample mediation by corruption tercile (low/high)
#   4. Favorability constructed as proportion favorable (from H2 aggregation),
#      not min-max rescaled. Cross-referenced to H2 rather than restated.
#   5. Latin America sample justified by one-sentence cross-reference to H2.
#   6. Pooled mediation retained as baseline; moderated mediation added.
# =============================================================================

# ── 0. Packages ───────────────────────────────────────────────────────────────
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(tidyverse, fixest, modelsummary, patchwork, scales, mediation)

# =============================================================================
# STEP 1: Load data
#
# H3 uses the same Latin America panel as H2, for the same reason:
# Latinobarometro provides the most consistent repeated cross-national
# favorability time series available. See H2 methods section for full
# justification.
#
# Favorability DV: proportion favorable (prop_fav), constructed from
# Latinobarometro micro-data in Python. See H2 for variable construction.
# us_prop_fav / china_prop_fav = share of valid respondents rating the
# donor as "Good" or "Very good". Higher = more favorable.
# =============================================================================

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

# Drop old fav columns and merge in proportion-favorable measures
df_panel <- df_panel %>%
  dplyr::select(-any_of(c("fav_us", "fav_china",
                          "fav_us_r", "fav_china_r")))

df_raw <- df_panel %>%
  left_join(
    df_fav %>% dplyr::select(country, year,
                             us_prop_fav, us_net_fav,
                             us_n_valid, us_n_fav, us_n_unfav,
                             china_prop_fav, china_net_fav,
                             china_n_valid, china_n_fav, china_n_unfav),
    by = c("country", "year")
  )

# =============================================================================
# STEP 2: Construct variables
# =============================================================================

df <- df_raw %>%
  mutate(
    # --- Aid decomposition (mirroring H2) ---
    # Presence: 1 if donor committed any positive aid
    AnyAid_us    = as.integer(aid_us    > 0),
    AnyAid_china = as.integer(aid_china > 0),
    
    # Intensity: log(aid) defined only for positive-aid obs
    IntAid_us    = if_else(aid_us    > 0, log(aid_us),    NA_real_),
    IntAid_china = if_else(aid_china > 0, log(aid_china), NA_real_),
    
    # Robustness: old log(y+1) — kept for comparability
    log_aid_us    = log(aid_us    + 1),
    log_aid_china = log(aid_china + 1),
    
    # --- Corruption terciles for heterogeneity analysis ---
    # Low corruption (tercile 3, least corrupt) vs. high (tercile 1, most corrupt)
    corr_tercile = ntile(corruption, 3),
    corr_group   = case_when(
      corr_tercile == 1 ~ "High Corruption",
      corr_tercile == 2 ~ "Middle",
      corr_tercile == 3 ~ "Low Corruption"
    ),
    corr_hi = as.integer(corr_tercile == 1),   # 1 = most corrupt tercile
    corr_lo = as.integer(corr_tercile == 3),   # 1 = least corrupt tercile
    
    country  = as.factor(country),
    year_int = as.integer(year),
    year     = as.factor(year)
  ) %>%
  arrange(country, year_int)

# =============================================================================
# STEP 3: Create lagged favorability variables
#
# Lags computed within country with gap-year correction:
# only assign lag when the prior row is exactly 1 (or 2) calendar years
# earlier, to avoid bridging across missing survey waves.
# =============================================================================

df <- df %>%
  group_by(country) %>%
  mutate(
    # Proportion favorable lags — main mediator variable
    fav_us_lag1    = if_else(year_int - lag(year_int)    == 1,
                             lag(us_prop_fav,    1), NA_real_),
    fav_china_lag1 = if_else(year_int - lag(year_int)    == 1,
                             lag(china_prop_fav, 1), NA_real_),
    fav_us_lag2    = if_else(year_int - lag(year_int, 2) == 2,
                             lag(us_prop_fav,    2), NA_real_),
    fav_china_lag2 = if_else(year_int - lag(year_int, 2) == 2,
                             lag(china_prop_fav, 2), NA_real_),
    
    # Lagged aid presence (for presence-margin lagged regression)
    AnyAid_us_lag1    = if_else(year_int - lag(year_int) == 1,
                                lag(AnyAid_us,    1), NA_integer_),
    AnyAid_china_lag1 = if_else(year_int - lag(year_int) == 1,
                                lag(AnyAid_china, 1), NA_integer_),
    
    # Lagged aid intensity (for intensity-margin lagged regression)
    IntAid_us_lag1    = if_else(year_int - lag(year_int) == 1,
                                lag(IntAid_us,    1), NA_real_),
    IntAid_china_lag1 = if_else(year_int - lag(year_int) == 1,
                                lag(IntAid_china, 1), NA_real_)
  ) %>%
  ungroup()

# Effective N check
message("=== Effective N after lagging ===")
message("US  lag-1: ", sum(!is.na(df$fav_us_lag1)    & !is.na(df$vote_us)))
message("US  lag-2: ", sum(!is.na(df$fav_us_lag2)    & !is.na(df$vote_us)))
message("CN  lag-1: ", sum(!is.na(df$fav_china_lag1) & !is.na(df$vote_china)))
message("CN  lag-2: ", sum(!is.na(df$fav_china_lag2) & !is.na(df$vote_china)))

# =============================================================================
# STEP 4: Descriptive statistics
# =============================================================================

h3_vars <- c("vote_us", "vote_china",
             "us_prop_fav", "china_prop_fav",
             "fav_us_lag1", "fav_china_lag1",
             "AnyAid_us", "AnyAid_china",
             "log_aid_us", "log_aid_china",
             "corruption")
pacman::p_load(tidyverse, fixest, modelsummary, patchwork, scales, mediation)
select <- dplyr::select 

desc_tbl <- df %>%
  select(any_of(h3_vars)) %>%
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
    "vote_us"        ~ "UN Voting Alignment: US (DV)",
    "vote_china"     ~ "UN Voting Alignment: China (DV)",
    "us_prop_fav"    ~ "US Favorability — proportion favorable (0-1)",
    "china_prop_fav" ~ "China Favorability — proportion favorable (0-1)",
    "fav_us_lag1"    ~ "US Favorability, lag 1 year",
    "fav_china_lag1" ~ "China Favorability, lag 1 year",
    "AnyAid_us"      ~ "Any US Aid (binary)",
    "AnyAid_china"   ~ "Any China Aid (binary)",
    "log_aid_us"     ~ "Log US Aid + 1",
    "log_aid_china"  ~ "Log China Aid + 1",
    "corruption"     ~ "Control of Corruption Index (-2.5 to 2.5)",
    .default = Variable
  ))

message("\n=== Descriptive Statistics ===")
print(desc_tbl, n = Inf)

# =============================================================================
# STEP 5: Part A — Lagged regression (primary H3 test)
#
# Proxy test: if the mediated channel operates, lagged favorability should
# predict subsequent alignment even after controlling for aid and fixed effects.
# We do NOT claim to directly observe the full theoretical sequence
# (aid → project success → citizen updating → favorability → alignment).
# We observe only aid, favorability, and alignment, and test whether the
# temporal pattern is consistent with the mediated channel.
#
# Staged by aid decomposition:
#   Stage 1: presence margin (AnyAid) — baseline
#   Stage 2: intensity margin (IntAid, pos-aid only)
#   Stage 3: robustness with log(y+1)
#
# Heterogeneity: interact lagged favorability with corruption to test whether
# the favorability → alignment pathway is stronger in lower-diversion settings.
# =============================================================================

# ── Stage 1: Presence margin ──────────────────────────────────────────────────

# US — lag 1 and lag 2, presence
m_us_pres_lag1 <- feols(
  vote_us ~ fav_us_lag1 + AnyAid_us_lag1 | country + year,
  data = df, cluster = ~country
)
m_us_pres_lag2 <- feols(
  vote_us ~ fav_us_lag2 + AnyAid_us_lag1 | country + year,
  data = df, cluster = ~country
)

# China — lag 1 and lag 2, presence
m_cn_pres_lag1 <- feols(
  vote_china ~ fav_china_lag1 + AnyAid_china_lag1 | country + year,
  data = df, cluster = ~country
)
m_cn_pres_lag2 <- feols(
  vote_china ~ fav_china_lag2 + AnyAid_china_lag1 | country + year,
  data = df, cluster = ~country
)

message("\n=== Stage 1: Presence margin lagged regressions ===")
etable(m_us_pres_lag1, m_us_pres_lag2, m_cn_pres_lag1, m_cn_pres_lag2,
       headers     = c("US (Lag 1)", "US (Lag 2)", "CN (Lag 1)", "CN (Lag 2)"),
       coefstat    = "se",
       signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10))

# ── Stage 2: Intensity margin (positive-aid subsample) ───────────────────────

df_pos_us    <- df %>% filter(aid_us    > 0)
df_pos_china <- df %>% filter(aid_china > 0)

m_us_int_lag1 <- feols(
  vote_us ~ fav_us_lag1 + IntAid_us_lag1 | country + year,
  data = df_pos_us, cluster = ~country
)
m_cn_int_lag1 <- feols(
  vote_china ~ fav_china_lag1 + IntAid_china_lag1 | country + year,
  data = df_pos_china, cluster = ~country
)

message("\n=== Stage 2: Intensity margin lagged regressions (pos-aid subsample) ===")
etable(m_us_int_lag1, m_cn_int_lag1,
       headers     = c("US (Lag 1, pos-aid)", "CN (Lag 1, pos-aid)"),
       coefstat    = "se",
       signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10))

# ── Stage 3: Robustness with log(y+1) ────────────────────────────────────────

m_us_rob_lag1 <- feols(
  vote_us ~ fav_us_lag1 + log_aid_us | country + year,
  data = df, cluster = ~country
)
m_us_rob_lag2 <- feols(
  vote_us ~ fav_us_lag2 + log_aid_us | country + year,
  data = df, cluster = ~country
)
m_cn_rob_lag1 <- feols(
  vote_china ~ fav_china_lag1 + log_aid_china | country + year,
  data = df, cluster = ~country
)
m_cn_rob_lag2 <- feols(
  vote_china ~ fav_china_lag2 + log_aid_china | country + year,
  data = df, cluster = ~country
)

message("\n=== Stage 3: Robustness log(y+1) ===")
etable(m_us_rob_lag1, m_us_rob_lag2, m_cn_rob_lag1, m_cn_rob_lag2,
       headers     = c("US (Lag 1)", "US (Lag 2)", "CN (Lag 1)", "CN (Lag 2)"),
       coefstat    = "se",
       signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10))

# ── Heterogeneity: interact lagged favorability × corruption ──────────────────
# Key prediction: the fav → alignment path should be stronger (more positive)
# in lower-corruption settings, where aid-funded projects are more likely to
# have been implemented visibly, generating the citizen updating that makes
# favorability meaningful. In high-corruption settings, the direct channel
# may dominate because project success is less likely to be visible.

m_us_het <- feols(
  vote_us ~ fav_us_lag1 * corruption + AnyAid_us_lag1 | country + year,
  data = df, cluster = ~country
)
m_cn_het <- feols(
  vote_china ~ fav_china_lag1 * corruption + AnyAid_china_lag1 | country + year,
  data = df, cluster = ~country
)

message("\n=== Heterogeneity: Fav × Corruption interaction ===")
etable(m_us_het, m_cn_het,
       headers     = c("US Vote", "China Vote"),
       coefstat    = "se",
       signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10))

# Export all lagged regression tables
modelsummary(
  list(
    "US Presence L1"   = m_us_pres_lag1,
    "US Presence L2"   = m_us_pres_lag2,
    "CN Presence L1"   = m_cn_pres_lag1,
    "CN Presence L2"   = m_cn_pres_lag2,
    "US Intensity L1"  = m_us_int_lag1,
    "CN Intensity L1"  = m_cn_int_lag1,
    "US Het"           = m_us_het,
    "CN Het"           = m_cn_het
  ),
  stars   = c("*" = .10, "**" = .05, "***" = .01),
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  notes   = "Two-way FEs (country + year). SEs clustered by country. Lags gap-corrected.",
  output  = "h3_lagged_regression_table.txt"
)
message("✓ Saved: h3_lagged_regression_table.txt")

# =============================================================================
# STEP 6: Part B — Mediation analysis
#
# Proxy test of the mediated channel: aid → favorability → alignment.
# Estimated as a pooled baseline, then split by corruption group to assess
# whether the indirect path is stronger where diversion risk is lower.
#
# NOTE: mediation::mediate() requires lm() objects, not feols().
# Country and year dummies are added as controls to approximate the FE
# structure. This is standard practice for panel mediation.
#
# Temporal ordering: AnyAid at t-1 → fav_lag1 (measured at t-1) → vote at t.
# =============================================================================

# ── Helper: run Baron-Kenny steps and print ───────────────────────────────────
run_bk <- function(data, treat_var, mediator_var, outcome_var, label) {
  f1 <- as.formula(paste(outcome_var,  "~", treat_var, "+ country + year"))
  f2 <- as.formula(paste(mediator_var, "~", treat_var, "+ country + year"))
  f3 <- as.formula(paste(outcome_var,  "~", treat_var, "+", mediator_var,
                         "+ country + year"))
  
  m1 <- lm(f1, data = data)
  m2 <- lm(f2, data = data)
  m3 <- lm(f3, data = data)
  
  cat("\n--- Baron & Kenny:", label, "---\n")
  cat("Step 1 (total effect,", treat_var, "→", outcome_var, "):",
      "β =", round(coef(m1)[treat_var], 4),
      "| p =", round(summary(m1)$coef[treat_var, "Pr(>|t|)"], 4), "\n")
  cat("Step 2 (a path,", treat_var, "→", mediator_var, "):",
      "β =", round(coef(m2)[treat_var], 4),
      "| p =", round(summary(m2)$coef[treat_var, "Pr(>|t|)"], 4), "\n")
  cat("Step 3 (b path,", mediator_var, "→", outcome_var, "):",
      "β =", round(coef(m3)[mediator_var], 4),
      "| p =", round(summary(m3)$coef[mediator_var, "Pr(>|t|)"], 4), "\n")
  cat("Step 3 (direct,", treat_var, "→", outcome_var, "):",
      "β =", round(coef(m3)[treat_var], 4),
      "| p =", round(summary(m3)$coef[treat_var, "Pr(>|t|)"], 4), "\n")
  
  list(m_outcome_total = m1, m_mediator = m2, m_outcome_full = m3)
}

# ── Helper: run formal mediation and return summary ───────────────────────────
run_mediation <- function(models, treat_var, mediator_var, label, sims = 1000) {
  set.seed(42)
  cat("\n--- Formal mediation:", label, "---\n")
  med_obj <- mediate(
    model.m  = models$m_mediator,
    model.y  = models$m_outcome_full,
    treat    = treat_var,
    mediator = mediator_var,
    sims     = sims,
    boot     = FALSE
  )
  summary(med_obj)
  med_obj
}

# ── Pooled mediation — US ─────────────────────────────────────────────────────

med_df_us <- df %>%
  filter(!is.na(fav_us_lag1), !is.na(vote_us),
         !is.na(AnyAid_us_lag1)) %>%
  droplevels()

bk_us_pool <- run_bk(med_df_us,
                     treat_var    = "AnyAid_us_lag1",
                     mediator_var = "fav_us_lag1",
                     outcome_var  = "vote_us",
                     label        = "US (pooled, presence margin)")

med_us_pool <- run_mediation(bk_us_pool,
                             treat_var    = "AnyAid_us_lag1",
                             mediator_var = "fav_us_lag1",
                             label        = "US pooled")

# ── Pooled mediation — China ──────────────────────────────────────────────────

med_df_cn <- df %>%
  filter(!is.na(fav_china_lag1), !is.na(vote_china),
         !is.na(AnyAid_china_lag1)) %>%
  droplevels()

bk_cn_pool <- run_bk(med_df_cn,
                     treat_var    = "AnyAid_china_lag1",
                     mediator_var = "fav_china_lag1",
                     outcome_var  = "vote_china",
                     label        = "China (pooled, presence margin)")

med_cn_pool <- run_mediation(bk_cn_pool,
                             treat_var    = "AnyAid_china_lag1",
                             mediator_var = "fav_china_lag1",
                             label        = "China pooled")

# ── Split-sample mediation by corruption group ────────────────────────────────
# Key heterogeneity test: if the mediated channel operates primarily where
# diversion risk is low (i.e. aid more likely to be implemented visibly),
# the ACME should be larger and more significant in the low-corruption split.

run_split_mediation <- function(full_df, corr_level,
                                treat_var, mediator_var, outcome_var,
                                label) {
  sub <- full_df %>%
    filter(corr_group == corr_level,
           !is.na(.data[[mediator_var]]),
           !is.na(.data[[outcome_var]]),
           !is.na(.data[[treat_var]])) %>%
    droplevels()
  
  if (nrow(sub) < 30) {
    cat("\nInsufficient obs for", label, "(n =", nrow(sub), ") — skipping.\n")
    return(NULL)
  }
  
  models <- run_bk(sub, treat_var, mediator_var, outcome_var, label)
  run_mediation(models, treat_var, mediator_var, label)
}

message("\n=== Split-sample mediation: US ===")
med_us_lo <- run_split_mediation(
  df, "Low Corruption",
  "AnyAid_us_lag1", "fav_us_lag1", "vote_us",
  "US — Low Corruption (least diversion risk)")

med_us_hi <- run_split_mediation(
  df, "High Corruption",
  "AnyAid_us_lag1", "fav_us_lag1", "vote_us",
  "US — High Corruption (most diversion risk)")

message("\n=== Split-sample mediation: China ===")
med_cn_lo <- run_split_mediation(
  df, "Low Corruption",
  "AnyAid_china_lag1", "fav_china_lag1", "vote_china",
  "China — Low Corruption")

med_cn_hi <- run_split_mediation(
  df, "High Corruption",
  "AnyAid_china_lag1", "fav_china_lag1", "vote_china",
  "China — High Corruption")

# =============================================================================
# STEP 7: Visualisations
# =============================================================================

theme_paper <- theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(size = 10, colour = "grey40"),
    legend.position  = "bottom",
    panel.grid.minor = element_blank()
  )

pal_donor <- c("United States" = "#1565C0", "China" = "#C62828")

# Fig 1: Cross-lagged scatter (presence margin)
fig_us_lag <- df %>%
  filter(!is.na(fav_us_lag1), !is.na(vote_us)) %>%
  ggplot(aes(fav_us_lag1, vote_us)) +
  geom_point(alpha = 0.50, size = 2.2, colour = "#1565C0") +
  geom_smooth(method = "lm", se = TRUE,
              colour = "#1565C0", fill = "#1565C0", alpha = 0.15) +
  labs(title    = "US Favorability (t−1) → US Vote Alignment (t)",
       subtitle = "Proxy test of mediated channel | each point = country-year",
       x = "US Proportion Favorable, lag 1",
       y = "UN Voting Alignment with US") +
  theme_paper

fig_cn_lag <- df %>%
  filter(!is.na(fav_china_lag1), !is.na(vote_china)) %>%
  ggplot(aes(fav_china_lag1, vote_china)) +
  geom_point(alpha = 0.50, size = 2.2, colour = "#C62828") +
  geom_smooth(method = "lm", se = TRUE,
              colour = "#C62828", fill = "#C62828", alpha = 0.15) +
  labs(title    = "China Favorability (t−1) → China Vote Alignment (t)",
       subtitle = "Proxy test of mediated channel | each point = country-year",
       x = "China Proportion Favorable, lag 1",
       y = "UN Voting Alignment with China") +
  theme_paper

ggsave("h3_fig_crosslag.png", fig_us_lag | fig_cn_lag,
       width = 13, height = 5.5, dpi = 200)
message("✓ Saved: h3_fig_crosslag.png")

# Fig 2: Mediation results — pooled + split, with ACME and 95% CI
extract_med <- function(med_obj, donor_label, sample_label) {
  if (is.null(med_obj)) return(NULL)
  tibble(
    Donor   = donor_label,
    Sample  = sample_label,
    Effect  = c("ACME (Indirect)", "ADE (Direct)", "Total"),
    Estimate = c(med_obj$d.avg,       med_obj$z.avg,       med_obj$tau.coef),
    Lower    = c(med_obj$d.avg.ci[1], med_obj$z.avg.ci[1], med_obj$tau.ci[1]),
    Upper    = c(med_obj$d.avg.ci[2], med_obj$z.avg.ci[2], med_obj$tau.ci[2]),
    p_value  = c(med_obj$d.avg.p,     med_obj$z.avg.p,     med_obj$tau.p)
  )
}

med_results <- bind_rows(
  extract_med(med_us_pool, "United States", "Pooled"),
  extract_med(med_us_lo,   "United States", "Low Corruption"),
  extract_med(med_us_hi,   "United States", "High Corruption"),
  extract_med(med_cn_pool, "China",         "Pooled"),
  extract_med(med_cn_lo,   "China",         "Low Corruption"),
  extract_med(med_cn_hi,   "China",         "High Corruption")
) %>%
  mutate(
    Effect = factor(Effect, levels = c("Total", "ADE (Direct)", "ACME (Indirect)")),
    Sig    = case_when(
      p_value < 0.01 ~ "p < .01",
      p_value < 0.05 ~ "p < .05",
      p_value < 0.10 ~ "p < .10",
      TRUE           ~ "n.s."
    ),
    Sample = factor(Sample, levels = c("Pooled", "Low Corruption", "High Corruption"))
  )

fig_med <- ggplot(med_results,
                  aes(x = Estimate, y = Effect,
                      colour = Sample, shape = Sig)) +
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "grey55", linewidth = 0.8) +
  geom_errorbarh(aes(xmin = Lower, xmax = Upper),
                 height = 0.25, linewidth = 0.9,
                 position = position_dodgev(height = 0.6)) +
  geom_point(size = 3.5,
             position = position_dodgev(height = 0.6)) +
  scale_colour_manual(
    values = c("Pooled"           = "grey40",
               "Low Corruption"   = "#1B5E20",
               "High Corruption"  = "#B71C1C"),
    name = "Sample"
  ) +
  scale_shape_manual(
    values = c("p < .01" = 16, "p < .05" = 17,
               "p < .10" = 15, "n.s." = 1),
    name = "Significance"
  ) +
  facet_wrap(~Donor) +
  labs(
    title    = "Mediation Analysis: Aid Presence → Favorability → UN Vote Alignment",
    subtitle = "ACME = indirect effect (a×b path) | 95% CI from 1000 quasi-Bayesian simulations\nH3 prediction: ACME > 0, larger in low-corruption settings",
    x = "Effect Size",
    y = NULL
  ) +
  theme_paper +
  theme(legend.position = "right")

ggsave("h3_fig_mediation_results.png", fig_med,
       width = 12, height = 6, dpi = 200)
message("✓ Saved: h3_fig_mediation_results.png")

# Fig 3: Heterogeneity — fav × corruption interaction marginal effects
corr_seq <- seq(min(df$corruption, na.rm = TRUE),
                max(df$corruption, na.rm = TRUE),
                length.out = 120)

b_us <- coef(m_us_het)
b_cn <- coef(m_cn_het)

me_het <- tibble(
  corruption      = rep(corr_seq, 2),
  marginal_effect = c(
    b_us["fav_us_lag1"]    + b_us["fav_us_lag1:corruption"]    * corr_seq,
    b_cn["fav_china_lag1"] + b_cn["fav_china_lag1:corruption"] * corr_seq
  ),
  Donor = rep(c("United States", "China"), each = 120)
)

fig_het <- ggplot(me_het, aes(corruption, marginal_effect, colour = Donor)) +
  annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf,
           fill = "#FFEBEE", alpha = 0.35) +
  annotate("rect", xmin = 0, xmax = Inf, ymin = -Inf, ymax = Inf,
           fill = "#E8F5E9", alpha = 0.35) +
  geom_hline(yintercept = 0, linetype = "dashed",
             colour = "grey55", linewidth = 0.8) +
  geom_line(linewidth = 1.3) +
  scale_colour_manual(values = pal_donor) +
  annotate("text", x = -1.0, y = max(abs(me_het$marginal_effect), na.rm=TRUE)*0.85,
           label = "More corrupt", colour = "#B71C1C",
           size = 3.5, fontface = "italic") +
  annotate("text", x =  1.0, y = max(abs(me_het$marginal_effect), na.rm=TRUE)*0.85,
           label = "Less corrupt", colour = "#1B5E20",
           size = 3.5, fontface = "italic") +
  labs(
    title    = "Marginal Effect of Lagged Favorability on Vote Alignment across Governance Levels",
    subtitle = "H3 heterogeneity: mediated path predicted to be stronger where diversion risk is lower",
    x = "Control of Corruption Index (higher = better governance)",
    y = "ME of Lagged Favorability on Vote Alignment",
    colour = "Donor"
  ) +
  theme_paper

ggsave("h3_fig_het_corruption.png", fig_het,
       width = 10, height = 5.5, dpi = 200)
message("✓ Saved: h3_fig_het_corruption.png")

message("\n=== H3 Revised Analysis Complete ===")
message("Outputs:")
message("  Tables : h3_lagged_regression_table.txt")
message("  Figures: h3_fig_crosslag.png")
message("           h3_fig_mediation_results.png")
message("           h3_fig_het_corruption.png")

