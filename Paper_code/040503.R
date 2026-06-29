library(dplyr)
library(sandwich)
library(lmtest)

# ============================================================
# H2 Script: Diversion risk as moderator of aid → favorability
#
# Hypothesis: Foreign aid has a more positive effect on public
# favorability toward the donor in settings where diversion risk
# is lower. Favorability should be less responsive to aid in
# country-years identified as more corrupt, because aid is less
# likely to be translated into visible project success.
#   - Separate US story and China story
#   - Tercile split on diversion risk (low / medium / high)
#   - Cluster-robust SE by country
#   - OLS on favorability (main model)
#   - Interaction term: aid x diversion risk group
# ============================================================


# -------------------------------------------------------
# 1. Load data
# -------------------------------------------------------
df <- read.csv("~/Desktop/SPRING2026/MA_paper/code/final_merged.csv",
               stringsAsFactors = FALSE)


# -------------------------------------------------------
# 2. Construct variables
# -------------------------------------------------------
df_h2 <- df %>%
  arrange(country, year) %>%
  group_by(country) %>%
  mutate(
    # Log-transform aid (full sample, zeros become 0)
    log_aid_us    = log(aid_us    + 1),
    log_aid_china = log(aid_china + 1),
    
    # Lags of aid and favorability
    log_aid_us_l1    = lag(log_aid_us),
    log_aid_china_l1 = lag(log_aid_china),
    us_prop_fav_l1   = lag(us_prop_fav),
    china_prop_fav_l1= lag(china_prop_fav),
    
    # Lag diversion risk (use contemporaneous too — both tested below)
    simple_div_l1  = lag(simple_diversion_index),
    latent_div_l1  = lag(latent_diversion_risk)
  ) %>%
  ungroup()


# -------------------------------------------------------
# 3. Build country-level tercile groups on diversion risk
#    (matches your advisor's approach of using country-level
#     mean to assign stable tercile membership)
#
#    For simple_diversion_index:
#      higher value → more diversion risk → more corrupt
#    For latent_diversion_risk:
#      direction depends on construction; we label generically
#      and let the sign of the coefficient tell the story
# -------------------------------------------------------

# --- Simple diversion index terciles ---
terciles_simple <- df_h2 %>%
  group_by(country) %>%
  summarise(mean_simple_div = mean(simple_diversion_index, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(
    tercile_simple_num = ntile(mean_simple_div, 3),
    tercile_simple = case_when(
      tercile_simple_num == 1 ~ "T1_low_div",    # least diversion risk
      tercile_simple_num == 2 ~ "T2_mid_div",
      tercile_simple_num == 3 ~ "T3_high_div"    # most diversion risk / most corrupt
    )
  )

# --- Latent diversion risk terciles ---
terciles_latent <- df_h2 %>%
  group_by(country) %>%
  summarise(mean_latent_div = mean(latent_diversion_risk, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(
    tercile_latent_num = ntile(mean_latent_div, 3),
    tercile_latent = case_when(
      tercile_latent_num == 1 ~ "T1_latent",
      tercile_latent_num == 2 ~ "T2_latent",
      tercile_latent_num == 3 ~ "T3_latent"
    )
  )

df_h2 <- df_h2 %>%
  left_join(terciles_simple %>% select(country, tercile_simple, tercile_simple_num),
            by = "country") %>%
  left_join(terciles_latent %>% select(country, tercile_latent, tercile_latent_num),
            by = "country") %>%
  mutate(
    # Factor with T1 (low diversion) as reference category — so coefficients
    # show how aid effect shrinks as corruption increases
    tercile_simple_f = factor(tercile_simple,
                              levels = c("T1_low_div", "T2_mid_div", "T3_high_div")),
    tercile_latent_f = factor(tercile_latent,
                              levels = c("T1_latent", "T2_latent", "T3_latent")),
    
    # Collapsed binary: T3 (high diversion) vs T1+T2
    high_div_simple = if_else(tercile_simple_num == 3, 1L, 0L),
    high_div_latent = if_else(tercile_latent_num == 3, 1L, 0L)
  )

# Quick check: which countries fall into which tercile
cat("\n=== Country tercile assignments (simple diversion index) ===\n")
terciles_simple %>%
  left_join(df_h2 %>% distinct(country, tercile_simple), by = "country") %>%
  arrange(tercile_simple_num) %>%
  select(country, mean_simple_div, tercile_simple) %>%
  print(n = Inf)


# -------------------------------------------------------
# 4. Helper: run H2 models for one donor
#
#    Three model types:
#      M1: OLS — continuous interaction with diversion index
#      M2: OLS — tercile group interactions (three-way split)
#      M3: OLS — binary high-diversion interaction (collapsed)
#
#    Diversion measure: "simple" or "latent" (passed as arg)
# -------------------------------------------------------
run_h2_models <- function(data,
                          donor      = c("US", "CHN"),
                          div_measure = c("simple", "latent")) {
  
  donor       <- match.arg(donor)
  div_measure <- match.arg(div_measure)
  
  # Set donor-specific variable names
  if (donor == "US") {
    fav_var      <- "us_prop_fav"
    fav_lag      <- "us_prop_fav_l1"
    aid_lag      <- "log_aid_us_l1"
    rival_aid_lag<- "log_aid_china_l1"
  } else {
    fav_var      <- "china_prop_fav"
    fav_lag      <- "china_prop_fav_l1"
    aid_lag      <- "log_aid_china_l1"
    rival_aid_lag<- "log_aid_us_l1"
  }
  
  # Set diversion-measure-specific variable names
  if (div_measure == "simple") {
    div_cont  <- "simple_diversion_index"   # contemporaneous continuous
    div_lag   <- "simple_div_l1"            # lagged continuous
    tercile_f <- "tercile_simple_f"         # three-level factor
    high_div  <- "high_div_simple"          # binary collapsed
  } else {
    div_cont  <- "latent_diversion_risk"
    div_lag   <- "latent_div_l1"
    tercile_f <- "tercile_latent_f"
    high_div  <- "high_div_latent"
  }
  
  # ---- Clean estimation samples ----
  
  # Base variables needed in all models
  base_vars <- c(fav_var, fav_lag, aid_lag, rival_aid_lag,
                 div_lag, div_cont, tercile_f, high_div)
  
  est_base <- data %>%
    filter(if_all(all_of(base_vars), ~ !is.na(.)))
  
  # -------------------------------------------------------
  # M1: Continuous interaction
  #   favorability ~ fav_l1 + rival_aid_l1
  #                + log_aid_l1 * diversion_index_lag
  # -------------------------------------------------------
  f_m1 <- as.formula(
    paste(fav_var, "~", fav_lag, "+", rival_aid_lag,
          "+", aid_lag, "*", div_lag)
  )
  m1 <- lm(f_m1, data = est_base)
  r_m1 <- coeftest(m1, vcov = vcovCL(m1, cluster = ~ country))
  
  # -------------------------------------------------------
  # M2: Three-way tercile interaction
  #   favorability ~ fav_l1 + rival_aid_l1
  #                + tercile_f * log_aid_l1
  #   (T1=low-div is baseline; interaction terms capture
  #    how the aid slope differs in T2 and T3)
  # -------------------------------------------------------
  f_m2 <- as.formula(
    paste(fav_var, "~", fav_lag, "+", rival_aid_lag,
          "+", tercile_f, "*", aid_lag)
  )
  m2 <- lm(f_m2, data = est_base)
  r_m2 <- coeftest(m2, vcov = vcovCL(m2, cluster = ~ country))
  
  # -------------------------------------------------------
  # M3: Binary high-diversion interaction (collapsed T3 vs T1+T2)
  #   mirrors your advisor's T3_group approach
  # -------------------------------------------------------
  f_m3 <- as.formula(
    paste(fav_var, "~", fav_lag, "+", rival_aid_lag,
          "+", high_div, "*", aid_lag)
  )
  m3 <- lm(f_m3, data = est_base)
  r_m3 <- coeftest(m3, vcov = vcovCL(m3, cluster = ~ country))
  
  # -------------------------------------------------------
  # Implied aid slopes by group (from M2 and M3)
  # -------------------------------------------------------
  b2 <- coef(m2)
  b3 <- coef(m3)
  
  # M2 slopes
  base_slope_m2 <- b2[aid_lag]
  t2_slope_m2   <- b2[aid_lag] + b2[paste0(tercile_f, "T2_", ifelse(div_measure=="simple","mid","T2"), "_div:", aid_lag)]
  t3_slope_m2   <- b2[aid_lag] + b2[paste0(tercile_f, "T3_", ifelse(div_measure=="simple","high","T3"), "_div:", aid_lag)]
  
  # Handle latent naming difference
  if (div_measure == "latent") {
    t2_slope_m2 <- b2[aid_lag] + b2[paste0(tercile_f, "T2_latent:", aid_lag)]
    t3_slope_m2 <- b2[aid_lag] + b2[paste0(tercile_f, "T3_latent:", aid_lag)]
  }
  
  # M3 slopes
  low_slope_m3  <- b3[aid_lag]
  high_slope_m3 <- b3[aid_lag] + b3[paste0(high_div, ":", aid_lag)]
  
  slope_table <- data.frame(
    donor       = donor,
    div_measure = div_measure,
    model       = c("M2: T1 low-div (baseline)",
                    "M2: T2 mid-div",
                    "M2: T3 high-div",
                    "M3: T1+T2 (low+mid)",
                    "M3: T3 (high-div)"),
    aid_slope   = c(base_slope_m2, t2_slope_m2, t3_slope_m2,
                    low_slope_m3, high_slope_m3)
  )
  
  list(
    donor       = donor,
    div_measure = div_measure,
    m1_continuous = r_m1,
    m2_terciles   = r_m2,
    m3_binary     = r_m3,
    slope_table   = slope_table,
    # Raw model objects for post-estimation (e.g., margins, plots)
    m1_obj = m1,
    m2_obj = m2,
    m3_obj = m3,
    n_obs  = nobs(m1)
  )
}


# -------------------------------------------------------
# 5. Run all four combinations
# -------------------------------------------------------

cat("\n\nFitting models...\n")

US_simple  <- run_h2_models(df_h2, donor = "US",  div_measure = "simple")
US_latent  <- run_h2_models(df_h2, donor = "US",  div_measure = "latent")
CHN_simple <- run_h2_models(df_h2, donor = "CHN", div_measure = "simple")
CHN_latent <- run_h2_models(df_h2, donor = "CHN", div_measure = "latent")


# -------------------------------------------------------
# 6. Print results
# -------------------------------------------------------
print_results <- function(res) {
  cat("\n", strrep("=", 60), "\n", sep = "")
  cat(sprintf("DONOR: %s  |  DIVERSION MEASURE: %s  |  N = %d\n",
              res$donor, res$div_measure, res$n_obs))
  cat(strrep("=", 60), "\n\n")
  
  cat("--- M1: Continuous interaction (aid x diversion_lag) ---\n")
  print(res$m1_continuous)
  
  cat("\n--- M2: Three-way tercile interaction ---\n")
  cat("(T1 low-diversion is baseline; T2 and T3 interactions show slope change)\n")
  print(res$m2_terciles)
  
  cat("\n--- M3: Binary high-diversion interaction (T3 vs T1+T2) ---\n")
  print(res$m3_binary)
  
  cat("\n--- Implied aid-on-favorability slopes by group ---\n")
  print(res$slope_table, digits = 4, row.names = FALSE)
  cat("\n")
}

print_results(US_simple)
print_results(US_latent)
print_results(CHN_simple)
print_results(CHN_latent)


# -------------------------------------------------------
# 7. Combined slope summary across all models
# -------------------------------------------------------
all_slopes <- bind_rows(
  US_simple$slope_table,
  US_latent$slope_table,
  CHN_simple$slope_table,
  CHN_latent$slope_table
)

cat("\n", strrep("=", 60), "\n", sep = "")
cat("COMBINED SLOPE SUMMARY\n")
cat(strrep("=", 60), "\n\n")
print(all_slopes, digits = 4, row.names = FALSE)


# -------------------------------------------------------
# 8. Robustness: contemporaneous diversion (not lagged)
#    Some argue diversion risk reflects slow-moving
#    institutional quality, so contemporaneous is fine.
# -------------------------------------------------------
cat("\n", strrep("=", 60), "\n", sep = "")
cat("ROBUSTNESS: Contemporaneous diversion index (not lagged)\n")
cat(strrep("=", 60), "\n\n")

for (donor_choice in c("US", "CHN")) {
  for (div_choice in c("simple", "latent")) {
    
    fav_var       <- if (donor_choice == "US") "us_prop_fav"    else "china_prop_fav"
    fav_lag       <- if (donor_choice == "US") "us_prop_fav_l1" else "china_prop_fav_l1"
    aid_lag       <- if (donor_choice == "US") "log_aid_us_l1"  else "log_aid_china_l1"
    rival_aid_lag <- if (donor_choice == "US") "log_aid_china_l1" else "log_aid_us_l1"
    div_cont      <- if (div_choice == "simple") "simple_diversion_index" else "latent_diversion_risk"
    
    f_rob <- as.formula(
      paste(fav_var, "~", fav_lag, "+", rival_aid_lag,
            "+", aid_lag, "*", div_cont)
    )
    
    est_rob <- df_h2 %>%
      filter(if_all(all_of(c(fav_var, fav_lag, aid_lag, rival_aid_lag, div_cont)),
                    ~ !is.na(.)))
    
    m_rob <- lm(f_rob, data = est_rob)
    r_rob <- coeftest(m_rob, vcov = vcovCL(m_rob, cluster = ~ country))
    
    cat(sprintf("\n%s | %s diversion (contemporaneous):\n", donor_choice, div_choice))
    print(r_rob)
  }
}

