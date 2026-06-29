# =============================================================================
# H1 REVISED ANALYSIS — per advisor comments
#
# Changes from prior draft:
#   1. Extensive-margin logit (any aid) added
#   2. PPML full-sample amount model (replaces log(y+1) as lead spec)
#   3. Conditional positive-aid OLS (intensive margin only)
#   4. Pivotality redefined as distance-from-zero in align_diff
#   5. Rolling SD retained only as robustness check, relabeled "alignment volatility"
#   6. Presentation order: logit → logit×pivot → PPML → pos-aid OLS → symmetry → robustness
# =============================================================================

# --- Packages ----------------------------------------------------------------
packages <- c("tidyverse", "fixest", "zoo", "car", "stargazer",
              "ggplot2", "marginaleffects", "xtable")
installed <- rownames(installed.packages())
to_install <- packages[!packages %in% installed]
if (length(to_install)) install.packages(to_install)

library(tidyverse)
library(fixest)
library(zoo)
library(car)
library(stargazer)
library(ggplot2)
library(marginaleffects)
library(xtable)

# =============================================================================
# STEP 1: Load and sort data
# =============================================================================

df <- read_csv("~/Desktop/SPRING2026/MA_paper/0107.csv")
df <- df %>% arrange(Countryname, year)

# =============================================================================
# STEP 2: Construct variables
# =============================================================================

df <- df %>%
  group_by(Countryname) %>%
  mutate(
    
    # ------------------------------------------------------------------
    # 2a. Alignment differential
    # ------------------------------------------------------------------
    align_diff     = USAgree - ChinaAgree,
    align_diff_lag = lag(align_diff, 1),
    
    # ------------------------------------------------------------------
    # 2b. MAIN pivotality measure: distance from zero in align_diff
    #     Smaller |align_diff| → closer to indifference threshold → more pivotal
    #     Two equivalent framings; use pivot_closeness as the main regressor
    #     (larger = more pivotal) so the sign is intuitive in interactions.
    # ------------------------------------------------------------------
    pivot_distance  = abs(align_diff),          # smaller = more pivotal
    pivot_closeness = -abs(align_diff),          # larger  = more pivotal (for interactions)
    
    pivot_distance_lag  = lag(pivot_distance,  1),
    pivot_closeness_lag = lag(pivot_closeness, 1),
    
    # ------------------------------------------------------------------
    # 2c. SECONDARY robustness measure: rolling-SD of align_diff
    #     Relabeled "alignment volatility" per advisor.
    #     A volatile country is NOT the same as a pivotal one.
    # ------------------------------------------------------------------
    align_volatility = rollapply(
      align_diff,
      width   = 5,
      FUN     = sd,
      fill    = NA,
      align   = "right",
      partial = TRUE
    ),
    align_volatility_lag = lag(align_volatility, 1),
    
    # ------------------------------------------------------------------
    # 2d. Extensive-margin binary indicators
    # ------------------------------------------------------------------
    US_any  = as.integer(USA_comm  > 0),   # 1 if USA gives any aid
    CHN_any = as.integer(CHN_comm  > 0),   # 1 if China gives any aid
    
    US_any_lag  = lag(US_any,  1),
    CHN_any_lag = lag(CHN_any, 1),
    
    # ------------------------------------------------------------------
    # 2e. Standard lagged aid variables (levels and logged)
    # ------------------------------------------------------------------
    CHN_comm_lag      = lag(CHN_comm, 1),
    USA_comm_lag      = lag(USA_comm, 1),
    
    log_CHN_comm_lag  = log(CHN_comm_lag  + 1),
    log_USA_comm_lag  = log(USA_comm_lag  + 1),
    log_USA_comm      = log(USA_comm + 1), 
    CHN_any_lag_cont  = log(CHN_comm_lag + 1),  
    USA_any_lag_cont = log(USA_comm_lag + 1)# continuous logged version of lagged CHN aid
    # already have CHN_any_lag (binary), this gives the logged amount equivalent# kept for robustness only
  ) %>%
  ungroup()

# =============================================================================
# STEP 3: Tercile classification — distance-from-zero (main)
# =============================================================================

country_pivot <- df %>%
  group_by(Countryname) %>%
  summarise(
    mean_pivot_distance  = mean(pivot_distance_lag, na.rm = TRUE),
    mean_align_diff      = mean(align_diff,          na.rm = TRUE),
    mean_align_vol       = mean(align_volatility_lag, na.rm = TRUE),
    n_obs                = sum(!is.na(pivot_distance_lag))
  ) %>%
  filter(n_obs > 0) %>%
  mutate(
    # Terciles of |align_diff|: T1 = closest to zero = MOST pivotal
    tercile_dist = ntile(mean_pivot_distance, 3),
    tercile_dist_label = case_when(
      tercile_dist == 1 ~ "T1: Pivotal (close to threshold)",
      tercile_dist == 2 ~ "T2: Intermediate",
      tercile_dist == 3 ~ "T3: Aligned (far from threshold)"
    ),
    # Secondary: terciles of rolling-SD volatility (robustness only)
    tercile_vol = ntile(mean_align_vol, 3),
    tercile_vol_label = case_when(
      tercile_vol == 1 ~ "T1: Stable",
      tercile_vol == 2 ~ "T2: Moderate",
      tercile_vol == 3 ~ "T3: Volatile"
    )
  ) %>%
  arrange(mean_pivot_distance)

# Merge terciles back to panel
df <- df %>%
  left_join(
    country_pivot %>% select(Countryname,
                             tercile_dist, tercile_dist_label,
                             tercile_vol,  tercile_vol_label),
    by = "Countryname"
  )

# Print country lists (main pivotality measure)
cat("=============================================================\n")
cat("T1: PIVOTAL — closest to alignment threshold (|align_diff| small)\n")
cat("=============================================================\n")
country_pivot %>%
  filter(tercile_dist == 1) %>%
  select(Countryname, mean_pivot_distance, mean_align_diff) %>%
  arrange(mean_pivot_distance) %>%
  print(n = Inf)

cat("\n=============================================================\n")
cat("T2: INTERMEDIATE\n")
cat("=============================================================\n")
country_pivot %>%
  filter(tercile_dist == 2) %>%
  select(Countryname, mean_pivot_distance, mean_align_diff) %>%
  print(n = Inf)

cat("\n=============================================================\n")
cat("T3: ALIGNED — far from threshold (|align_diff| large)\n")
cat("=============================================================\n")
country_pivot %>%
  filter(tercile_dist == 3) %>%
  select(Countryname, mean_pivot_distance, mean_align_diff) %>%
  arrange(desc(mean_pivot_distance)) %>%
  print(n = Inf)

# Summary table
cat("\n=============================================================\n")
cat("Summary statistics by pivotality tercile (main measure)\n")
cat("=============================================================\n")
country_pivot %>%
  group_by(tercile_dist_label) %>%
  summarise(
    n_countries         = n(),
    mean_pivot_distance = round(mean(mean_pivot_distance), 4),
    min_pivot_distance  = round(min(mean_pivot_distance),  4),
    max_pivot_distance  = round(max(mean_pivot_distance),  4),
    mean_align_diff     = round(mean(mean_align_diff),     3)
  ) %>%
  print()

# Country-years per tercile
df %>%
  count(tercile_dist_label) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  print()

# =============================================================================
# STEP 4: ESTIMATION — ordered per advisor
#   (1) Extensive-margin logit — any aid
#   (2) Extensive-margin logit × pivotality interaction
#   (3) PPML — full-sample amount model (LEAD SPECIFICATION)
#   (4) Positive-aid OLS — conditional intensive margin
#   (5) Symmetry check for China (optional)
#   (6) Robustness: old log(y+1) FE OLS; volatility instead of distance
# =============================================================================

# Restrict to rows with complete key variables
df_est <- df %>%
  filter(!is.na(log_CHN_comm_lag),
         !is.na(pivot_closeness_lag),
         !is.na(pivot_distance_lag),
         !is.na(US_any_lag),
         !is.na(CHN_any_lag))

# ─────────────────────────────────────────────────────────────────────────────
# MODEL 1: Extensive-margin logit — Pr(US gives any aid)
# ─────────────────────────────────────────────────────────────────────────────
# Pr(US_any_it = 1) = logit^{-1}(alpha_i + lambda_t
#                                + beta1 US_any_i,t-1
#                                + beta2 CHN_any_i,t-1
#                                + beta3 controls_i,t-1)
#
# fixest::feglm handles logit with country+year FEs via the Mundlak/Chamberlain
# approach (within-group demeaning for nonlinear models).

m1_logit <- feglm(
  US_any ~ US_any_lag + CHN_any_lag + log_CHN_comm_lag +
    align_diff_lag | Countryname + year,
  data   = df_est,
  family = "logit",
  cluster = ~Countryname
)
summary(m1_logit)

# ─────────────────────────────────────────────────────────────────────────────
# MODEL 2: Extensive-margin logit × pivotality interaction
#   Key question: does prior Chinese aid differentially raise/lower the
#   probability of U.S. entry depending on how contestable the country is?
# ─────────────────────────────────────────────────────────────────────────────

m2_logit_int <- feglm(
  US_any ~ US_any_lag + CHN_any_lag +
    log_CHN_comm_lag * pivot_closeness_lag +
    align_diff_lag | Countryname + year,
  data   = df_est,
  family = "logit",
  cluster = ~Countryname
)
summary(m2_logit_int)

# Marginal effects at representative values of pivot_closeness_lag
mfx_logit_int <- avg_slopes(m2_logit_int,
                            variables  = "log_CHN_comm_lag",
                            vcov = FALSE,
                            newdata    = datagrid(
                              pivot_closeness_lag = quantile(
                                df_est$pivot_closeness_lag, c(.1, .5, .9),
                                na.rm = TRUE)
                            ))
print(mfx_logit_int)

# ─────────────────────────────────────────────────────────────────────────────
# MODEL 3: PPML — full-sample amount model (LEAD SPECIFICATION)
#   E[USA_comm_it | X] = exp(alpha_i + lambda_t + X_it * beta)
#   DV in LEVELS (not logged). Zeros are kept in sample naturally.
#   Equivalent to fepois() in fixest.
# ─────────────────────────────────────────────────────────────────────────────

m3_ppml <- fepois(
  USA_comm ~ log_CHN_comm_lag + pivot_closeness_lag +
    log_CHN_comm_lag:pivot_closeness_lag +
    align_diff_lag | Countryname + year,
  data    = df_est,
  cluster = ~Countryname
)
summary(m3_ppml)

# ─────────────────────────────────────────────────────────────────────────────
# MODEL 4: Conditional intensive-margin OLS
#   Among obs where US gives positive aid only.
#   DV: log(USA_comm_it) — no +1 needed since comm > 0 in this subsample.
# ─────────────────────────────────────────────────────────────────────────────

df_pos <- df_est %>% filter(USA_comm > 0)

m4_ols_pos <- feols(
  log(USA_comm) ~ log_CHN_comm_lag * pivot_closeness_lag +
    align_diff_lag | Countryname + year,
  data    = df_pos,
  cluster = ~Countryname
)
summary(m4_ols_pos)

# ─────────────────────────────────────────────────────────────────────────────
# MODEL 5: Symmetry check — China's response to US aid (optional)
# ─────────────────────────────────────────────────────────────────────────────

m5_ppml_chn <- fepois(
  CHN_comm ~ log_USA_comm_lag + pivot_closeness_lag +
    log_USA_comm_lag:pivot_closeness_lag +
    align_diff_lag | Countryname + year,
  data    = df_est,
  cluster = ~Countryname
)
summary(m5_ppml_chn)

# ─────────────────────────────────────────────────────────────────────────────
# MODEL 6: ROBUSTNESS — old log(y+1) FE OLS (retained as appendix comparison)
# ─────────────────────────────────────────────────────────────────────────────

m6_ols_log1p <- feols(
  log_USA_comm ~ log_CHN_comm_lag * pivot_closeness_lag +
    align_diff_lag | Countryname + year,
  data    = df_est,
  cluster = ~Countryname
)
summary(m6_ols_log1p)

# Robustness with alignment VOLATILITY (rolling SD) instead of distance
m6b_ppml_vol <- fepois(
  USA_comm ~ log_CHN_comm_lag * align_volatility_lag +
    align_diff_lag | Countryname + year,
  data    = df_est %>% filter(!is.na(align_volatility_lag)),
  cluster = ~Countryname
)
summary(m6b_ppml_vol)

# =============================================================================
# STEP 5: Split-sample specifications by pivotality tercile (distance-based)
# =============================================================================

split_ppml <- lapply(1:3, function(t) {
  fepois(
    USA_comm ~ log_CHN_comm_lag + align_diff_lag | Countryname + year,
    data    = df_est %>% filter(tercile_dist == t),
    cluster = ~Countryname
  )
})
names(split_ppml) <- c("T1_Pivotal", "T2_Intermediate", "T3_Aligned")

lapply(names(split_ppml), function(n) {
  cat("\n--- Split-sample PPML:", n, "---\n")
  print(summary(split_ppml[[n]]))
})

# =============================================================================
# STEP 6: Regression tables
# =============================================================================

# --- Table 1: Extensive-margin logit ---
etable(m1_logit, m2_logit_int,
       title  = "Extensive Margin: Pr(U.S. Gives Any Aid)",
       se     = "cluster",
       tex    = TRUE,
       file   = "tab_logit_extensive.tex")

# --- Table 2: Amount models (PPML + positive-aid OLS) ---
etable(m3_ppml, m4_ols_pos,
       title  = "Intensive Margin: Amount of U.S. Aid",
       se     = "cluster",
       tex    = TRUE,
       file   = "tab_amount_models.tex")

# --- Table 3: Robustness ---
etable(m6_ols_log1p, m6b_ppml_vol,
       title  = "Robustness Checks",
       se     = "cluster",
       tex    = TRUE,
       file   = "tab_robustness.tex")

# =============================================================================
# STEP 7: Export tercile classification tables
# =============================================================================

# Main (distance-based) tercile table
tercile_main <- country_pivot %>%
  select(Countryname, tercile_dist_label, mean_pivot_distance, mean_align_diff) %>%
  arrange(tercile_dist_label, mean_pivot_distance) %>%   # arrange BEFORE rename
  rename(
    Country             = Countryname,
    Tercile             = tercile_dist_label,
    `Mean |align_diff|` = mean_pivot_distance,
    `Mean Align. Diff.` = mean_align_diff
  )

write_csv(tercile_main, "tercile_classification_distance.csv")

print(
  xtable(tercile_main,
         caption = "Country Classification by Distance from Alignment Threshold",
         label   = "tab:tercile_countries",
         digits  = 3),
  include.rownames  = FALSE,
  booktabs          = TRUE,
  caption.placement = "top"
)

# Secondary (volatility) tercile table — robustness appendix only
tercile_vol_tbl <- country_pivot %>%
  select(Countryname, tercile_vol_label, mean_align_vol, mean_align_diff) %>%
  arrange(tercile_vol_label, mean_align_vol) %>%         # arrange BEFORE rename
  rename(
    Country                  = Countryname,
    `Tercile (Volatility)`   = tercile_vol_label,
    `Mean Align. Volatility` = mean_align_vol,
    `Mean Align. Diff.`      = mean_align_diff
  )

write_csv(tercile_vol_tbl, "tercile_classification_volatility.csv")

# ── Pooled tercile-interaction models ─────────────────────────────────────────
# Use tercile_dist directly (already in df_est from Step 3 merge).
# T2 (Intermediate) is the reference category.
# i() syntax shows T1 and T3 interactions explicitly in the output table.

# =============================================================================
# STEP 4.5: Pooled tercile-interaction models
# T2 = reference category throughout
# =============================================================================

df_est <- df_est %>%
  mutate(
    tercile_f = factor(tercile_dist,
                       levels = c(2, 1, 3),
                       labels = c("T2_Intermediate", "T1_Pivotal", "T3_Aligned"))
  )

# (a) Extensive margin — CHN_any_lag × tercile
# Fix: add | Countryname + year inside fml, remove standalone CHN_any_lag
# to avoid the variable-relabelling bug
m_logit_tercile <- feglm(
  fml    = US_any ~ US_any_lag +
    i(tercile_f, CHN_comm_lag, ref = "T2_Intermediate") +
    i(tercile_f, ref = "T2_Intermediate"),
  data   = df_est,
  family = "logit",
  cluster = ~Countryname
)
summary(m_logit_tercile)

# (b) Intensive margin — log_CHN_comm_lag × tercile
m_ppml_tercile <- fepois(
  fml    = USA_comm ~
    i(tercile_f, log_CHN_comm_lag, ref = "T2_Intermediate") +
    i(tercile_f, ref = "T2_Intermediate") +
    pivot_closeness_lag +
    align_diff_lag | Countryname + year,
  data    = df_est,
  cluster = ~Countryname
)
summary(m_ppml_tercile)

message("\n=== Pooled Tercile-Interaction Models ===")
etable(m_logit_tercile, m_ppml_tercile,
       headers     = c("Extensive Margin (Logit)", "Intensive Margin (PPML)"),
       coefstat    = "se",
       signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10))

etable(m_logit_tercile, m_ppml_tercile,
       title  = "Pooled Tercile-Interaction Models: Extensive and Intensive Margins",
       se     = "cluster",
       tex    = TRUE,
       file   = "tab_tercile_interaction.tex")
message("✓ Saved: tab_tercile_interaction.tex")