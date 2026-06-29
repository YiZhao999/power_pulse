library(dplyr)
library(sandwich)
library(lmtest)

# ============================================================
# Combined H1 script: U.S. and China reaction functions
# Includes both:
#   (1) threshold-distance targeting: pivot_closeness = -abs(USAgree - ChinaAgree)
#   (2) donor-centered directional targeting:
#         U.S.  -> USAgree - ChinaAgree
#         China -> ChinaAgree - USAgree
# Collapsed groups: T1/T2 vs T3
# No fixed effects
# Cluster-robust SE by country
# ============================================================

# -----------------------------
# 1. Load data
# -----------------------------
df <- read.csv("~/Desktop/SPRING2026/MA_paper/0107.csv", stringsAsFactors = FALSE)

# -----------------------------
# 2. Construct variables
# -----------------------------
df_h1 <- df %>%
  arrange(Countryname, year) %>%
  group_by(Countryname) %>%
  mutate(
    # Directional alignment variables
    align_diff_US  = USAgree - ChinaAgree,
    align_diff_CHN = ChinaAgree - USAgree,
    
    # Threshold-distance variables
    pivot_distance  = abs(USAgree - ChinaAgree),
    pivot_closeness = -pivot_distance,
    
    # Extensive-margin outcomes
    US_any  = if_else(USA_comm > 0, 1L, 0L),
    CHN_any = if_else(CHN_comm > 0, 1L, 0L),
    
    # Logs for positive aid only
    log_USA_comm = if_else(USA_comm > 0, log(USA_comm), NA_real_),
    log_CHN_comm = if_else(CHN_comm > 0, log(CHN_comm), NA_real_),
    
    # Full-sample logged aid
    log_USA_comm_p1 = log(USA_comm + 1),
    log_CHN_comm_p1 = log(CHN_comm + 1),
    
    # Lags
    US_any_l1           = lag(US_any),
    CHN_any_l1          = lag(CHN_any),
    log_USA_comm_p1_l1  = lag(log_USA_comm_p1),
    log_CHN_comm_p1_l1  = lag(log_CHN_comm_p1),
    align_diff_US_l1    = lag(align_diff_US),
    align_diff_CHN_l1   = lag(align_diff_CHN),
    pivot_distance_l1   = lag(pivot_distance),
    pivot_closeness_l1  = lag(pivot_closeness)
  ) %>%
  ungroup()

# -----------------------------
# 3. Build country-level terciles
# -----------------------------
country_terciles <- df_h1 %>%
  group_by(Countryname) %>%
  summarise(
    mean_pivot_distance = mean(pivot_distance, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    tercile_num = ntile(mean_pivot_distance, 3),
    tercile = case_when(
      tercile_num == 1 ~ "T1_pivotal",
      tercile_num == 2 ~ "T2_intermediate",
      tercile_num == 3 ~ "T3_aligned"
    ),
    T3_group = if_else(tercile == "T3_aligned", 1L, 0L)
  )

df_h1 <- df_h1 %>%
  left_join(country_terciles %>% select(Countryname, tercile, T3_group),
            by = "Countryname")

# ============================================================
# 4. Helper function to run one donor reaction function
# ============================================================
run_reaction_models <- function(data, donor = c("US", "CHN"), targeting = c("pivot", "directional")) {
  donor <- match.arg(donor)
  targeting <- match.arg(targeting)
  
  if (donor == "US") {
    dep_any      <- "US_any"
    dep_any_l1   <- "US_any_l1"
    dep_comm     <- "USA_comm"
    dep_log_comm <- "log_USA_comm"
    
    rival_any_l1      <- "CHN_any_l1"
    rival_log_comm_l1 <- "log_CHN_comm_p1_l1"
    
    targeting_var <- if (targeting == "pivot") "pivot_closeness_l1" else "align_diff_US_l1"
  }
  
  if (donor == "CHN") {
    dep_any      <- "CHN_any"
    dep_any_l1   <- "CHN_any_l1"
    dep_comm     <- "CHN_comm"
    dep_log_comm <- "log_CHN_comm"
    
    rival_any_l1      <- "US_any_l1"
    rival_log_comm_l1 <- "log_USA_comm_p1_l1"
    
    targeting_var <- if (targeting == "pivot") "pivot_closeness_l1" else "align_diff_CHN_l1"
  }
  
  # Samples
  est_logit <- data %>%
    filter(
      !is.na(.data[[dep_any]]),
      !is.na(.data[[dep_any_l1]]),
      !is.na(.data[[rival_any_l1]]),
      !is.na(.data[[rival_log_comm_l1]]),
      !is.na(.data[[targeting_var]]),
      !is.na(T3_group)
    )
  
  est_ppml <- data %>%
    filter(
      !is.na(.data[[dep_comm]]),
      !is.na(.data[[dep_any_l1]]),
      !is.na(.data[[rival_any_l1]]),
      !is.na(.data[[rival_log_comm_l1]]),
      !is.na(.data[[targeting_var]]),
      !is.na(T3_group)
    )
  
  est_pos <- data %>%
    filter(
      .data[[dep_comm]] > 0,
      !is.na(.data[[dep_log_comm]]),
      !is.na(.data[[dep_any_l1]]),
      !is.na(.data[[rival_any_l1]]),
      !is.na(.data[[rival_log_comm_l1]]),
      !is.na(.data[[targeting_var]]),
      !is.na(T3_group)
    )
  
  # Formulas
  f_logit <- as.formula(
    paste(dep_any, "~", dep_any_l1, "+", rival_any_l1, "+", targeting_var,
          "+ T3_group *", rival_log_comm_l1)
  )
  
  f_logit_presence <- as.formula(
    paste(dep_any, "~", dep_any_l1, "+", targeting_var,
          "+ T3_group *", rival_any_l1)
  )
  
  f_ppml <- as.formula(
    paste(dep_comm, "~", dep_any_l1, "+", rival_any_l1, "+", targeting_var,
          "+ T3_group *", rival_log_comm_l1)
  )
  
  f_ols <- as.formula(
    paste(dep_log_comm, "~", dep_any_l1, "+", rival_any_l1, "+", targeting_var,
          "+ T3_group *", rival_log_comm_l1)
  )
  
  # Estimate models
  m_logit <- glm(f_logit, data = est_logit, family = binomial(link = "logit"))
  m_logit_presence <- glm(f_logit_presence, data = est_logit, family = binomial(link = "logit"))
  m_ppml <- glm(f_ppml, data = est_ppml, family = poisson(link = "log"))
  m_ols <- lm(f_ols, data = est_pos)
  
  # Clustered vcov results
  r_logit <- coeftest(m_logit, vcov = vcovCL(m_logit, cluster = ~ Countryname))
  r_logit_presence <- coeftest(m_logit_presence, vcov = vcovCL(m_logit_presence, cluster = ~ Countryname))
  r_ppml <- coeftest(m_ppml, vcov = vcovCL(m_ppml, cluster = ~ Countryname))
  r_ols <- coeftest(m_ols, vcov = vcovCL(m_ols, cluster = ~ Countryname))
  
  # Implied rival-aid slopes by group
  b_logit <- coef(m_logit)
  b_ppml  <- coef(m_ppml)
  b_ols   <- coef(m_ols)
  
  slope_logit_T1T2 <- b_logit[rival_log_comm_l1]
  slope_logit_T3   <- b_logit[rival_log_comm_l1] + b_logit[paste0("T3_group:", rival_log_comm_l1)]
  
  slope_ppml_T1T2 <- b_ppml[rival_log_comm_l1]
  slope_ppml_T3   <- b_ppml[rival_log_comm_l1] + b_ppml[paste0("T3_group:", rival_log_comm_l1)]
  
  slope_ols_T1T2 <- b_ols[rival_log_comm_l1]
  slope_ols_T3   <- b_ols[rival_log_comm_l1] + b_ols[paste0("T3_group:", rival_log_comm_l1)]
  
  slope_table <- data.frame(
    donor = donor,
    targeting = targeting,
    model = c("Extensive logit", "Full-sample amount (PPML)", "Positive-aid OLS"),
    T1T2_combined = c(slope_logit_T1T2, slope_ppml_T1T2, slope_ols_T1T2),
    T3_aligned    = c(slope_logit_T3,   slope_ppml_T3,   slope_ols_T3)
  )
  
  list(
    logit = r_logit,
    logit_presence = r_logit_presence,
    ppml = r_ppml,
    ols = r_ols,
    slope_table = slope_table
  )
}

# ============================================================
# 5. Run all four main variants
# ============================================================

# U.S. reaction function, threshold-distance targeting
US_pivot <- run_reaction_models(df_h1, donor = "US", targeting = "pivot")

# U.S. reaction function, donor-centered directional targeting
US_dir <- run_reaction_models(df_h1, donor = "US", targeting = "directional")

# China reaction function, threshold-distance targeting
CHN_pivot <- run_reaction_models(df_h1, donor = "CHN", targeting = "pivot")

# China reaction function, donor-centered directional targeting
CHN_dir <- run_reaction_models(df_h1, donor = "CHN", targeting = "directional")

# ============================================================
# 6. Print results
# ============================================================
cat("\n==============================\n")
cat("U.S. reaction function - pivot closeness\n")
cat("==============================\n")
print(US_pivot$logit)
print(US_pivot$logit_presence)
print(US_pivot$ppml)
print(US_pivot$ols)
print(US_pivot$slope_table)

cat("\n==============================\n")
cat("U.S. reaction function - directional distance\n")
cat("==============================\n")
print(US_dir$logit)
print(US_dir$logit_presence)
print(US_dir$ppml)
print(US_dir$ols)
print(US_dir$slope_table)

cat("\n==============================\n")
cat("China reaction function - pivot closeness\n")
cat("==============================\n")
print(CHN_pivot$logit)
print(CHN_pivot$logit_presence)
print(CHN_pivot$ppml)
print(CHN_pivot$ols)
print(CHN_pivot$slope_table)

cat("\n==============================\n")
cat("China reaction function - directional distance\n")
cat("==============================\n")
print(CHN_dir$logit)
print(CHN_dir$logit_presence)
print(CHN_dir$ppml)
print(CHN_dir$ols)
print(CHN_dir$slope_table)

# ============================================================
# 7. Optional combined slope summary
# ============================================================
all_slopes <- bind_rows(
  US_pivot$slope_table,
  US_dir$slope_table,
  CHN_pivot$slope_table,
  CHN_dir$slope_table
)

cat("\n==============================\n")
cat("Combined slope summary\n")
cat("==============================\n")
print(all_slopes)
