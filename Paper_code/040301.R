# =============================================================================
# STEP 4.5: Pooled tercile-interaction models
#
# Rather than splitting the sample by tercile, we interact the rival-aid
# variable with tercile indicators within a single pooled specification.
# This allows the strategic response coefficient to differ across
# pivotality regimes while keeping all observations in one model.
#
# T1 (Pivotal) is the omitted reference category, so the interaction
# coefficients on T2 and T3 capture how the response to Chinese aid
# differs in intermediate and aligned countries relative to pivotal ones.
#
# Estimated for both margins per advisor:
#   (a) Extensive margin — Pr(US gives any aid)
#   (b) Intensive margin — amount of US aid (PPML, full sample)
# =============================================================================

# Create tercile dummy variables (T1 = reference/omitted)
df_est <- df_est %>%
  mutate(
    T2 = as.integer(tercile_dist == 2),   # Intermediate
    T3 = as.integer(tercile_dist == 3)    # Aligned (far from threshold)
  )

# ── (a) Extensive margin: logit with tercile interactions ─────────────────────
# Interacts log_CHN_comm_lag with T2 and T3 dummies.
# Reference group = T1 (Pivotal). Coefficients on interactions capture
# how the Chinese aid → US entry response differs in T2 and T3 vs T1.

m_logit_tercile <- feglm(
  US_any ~ US_any_lag + CHN_any_lag +
    log_CHN_comm_lag +
    log_CHN_comm_lag:T2 +
    log_CHN_comm_lag:T3 +
    T2 + T3 +
    align_diff_lag | Countryname + year,
  data    = df_est,
  family  = "logit",
  cluster = ~Countryname
)
summary(m_logit_tercile)

# ── (b) Intensive margin: PPML with tercile interactions ──────────────────────
# Same logic applied to the amount equation.
# Coefficient on log_CHN_comm_lag = effect in T1 (Pivotal).
# Coefficients on interactions = differential effect in T2 and T3.

m_ppml_tercile <- fepois(
  USA_comm ~ log_CHN_comm_lag +
    log_CHN_comm_lag:T2 +
    log_CHN_comm_lag:T3 +
    T2 + T3 +
    pivot_closeness_lag +
    align_diff_lag | Countryname + year,
  data    = df_est,
  cluster = ~Countryname
)
summary(m_ppml_tercile)

# Print both together for easy comparison
message("\n=== Pooled Tercile-Interaction Models ===")
etable(m_logit_tercile, m_ppml_tercile,
       headers     = c("Extensive Margin (Logit)", "Intensive Margin (PPML)"),
       coefstat    = "se",
       signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10))

# Export
etable(m_logit_tercile, m_ppml_tercile,
       title  = "Pooled Tercile-Interaction Models: Extensive and Intensive Margins",
       se     = "cluster",
       tex    = TRUE,
       file   = "tab_tercile_interaction.tex")
message("✓ Saved: tab_tercile_interaction.tex")