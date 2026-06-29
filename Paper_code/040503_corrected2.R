library(dplyr)
library(sandwich)
library(lmtest)

# ============================================================
# H2 Script: Preferred specifications using LATENT diversion risk
#
# Main specifications:
#   1. FE continuous interaction:
#        aid_lag * latent_div_l1
#   2. FE tercile interaction:
#        aid_lag * tercile_latent_f
#   3. FE binary interaction:
#        T1 (low diversion) vs T2+T3
#
# This version reports BOTH:
#   - regular OLS standard errors
#   - cluster-robust standard errors by country
# ============================================================


# -------------------------------------------------------
# 1. Load data
# -------------------------------------------------------
df <- read.csv("~/Desktop/SPRING2026/MA_paper/code/final_merged_corrected.csv",
               stringsAsFactors = FALSE)


# -------------------------------------------------------
# 2. Construct variables
# -------------------------------------------------------
df_h2 <- df %>%
  arrange(country, year) %>%
  group_by(country) %>%
  mutate(
    log_aid_us    = log(aid_us + 1),
    log_aid_china = log(aid_china + 1),
    
    log_aid_us_l1     = lag(log_aid_us),
    log_aid_china_l1  = lag(log_aid_china),
    us_prop_fav_l1    = lag(us_prop_fav),
    china_prop_fav_l1 = lag(china_prop_fav),
    
    latent_div_l1 = lag(latent_diversion_risk)
  ) %>%
  ungroup() %>%
  mutate(
    country = factor(country),
    year    = factor(year)
  )


# -------------------------------------------------------
# 3. Build country-level latent-diversion terciles
# -------------------------------------------------------
safe_country_mean <- function(x) {
  if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
}

terciles_latent <- df_h2 %>%
  group_by(country) %>%
  summarise(
    mean_latent_div = safe_country_mean(latent_diversion_risk),
    .groups = "drop"
  ) %>%
  filter(!is.na(mean_latent_div)) %>%
  mutate(
    tercile_latent_num = ntile(mean_latent_div, 3),
    tercile_latent = case_when(
      tercile_latent_num == 1 ~ "T1_latent",
      tercile_latent_num == 2 ~ "T2_latent",
      tercile_latent_num == 3 ~ "T3_latent"
    )
  )

df_h2 <- df_h2 %>%
  left_join(
    terciles_latent %>% select(country, tercile_latent, tercile_latent_num),
    by = "country"
  ) %>%
  mutate(
    tercile_latent_f = factor(
      tercile_latent,
      levels = c("T1_latent", "T2_latent", "T3_latent")
    ),
    # Preferred binary split: T1 vs (T2 + T3)
    low_div_latent = if_else(tercile_latent_num == 1, 1L, 0L, missing = NA_integer_)
  )

cat("\n=== Country tercile assignments (latent diversion risk) ===\n")
terciles_latent %>%
  arrange(tercile_latent_num, mean_latent_div) %>%
  select(country, mean_latent_div, tercile_latent) %>%
  print(n = Inf)


# -------------------------------------------------------
# 4. Helper functions
# -------------------------------------------------------
get_coef0 <- function(model, term) {
  b <- coef(model)
  if (term %in% names(b) && !is.na(b[[term]])) unname(b[[term]]) else 0
}

get_interaction_coef <- function(model, var1, var2) {
  b <- coef(model)
  term1 <- paste0(var1, ":", var2)
  term2 <- paste0(var2, ":", var1)
  
  if (term1 %in% names(b) && !is.na(b[[term1]])) {
    unname(b[[term1]])
  } else if (term2 %in% names(b) && !is.na(b[[term2]])) {
    unname(b[[term2]])
  } else {
    0
  }
}

# Regular and clustered SE side by side
get_both_se <- function(model) {
  list(
    regular   = coeftest(model),
    clustered = coeftest(model, vcov = vcovCL(model, cluster = ~ country))
  )
}


# -------------------------------------------------------
# 5. Main estimation helper
# -------------------------------------------------------
run_h2_latent_fe <- function(data, donor = c("US", "CHN")) {
  
  donor <- match.arg(donor)
  
  if (donor == "US") {
    fav_var       <- "us_prop_fav"
    fav_lag       <- "us_prop_fav_l1"
    aid_lag       <- "log_aid_us_l1"
    rival_aid_lag <- "log_aid_china_l1"
  } else {
    fav_var       <- "china_prop_fav"
    fav_lag       <- "china_prop_fav_l1"
    aid_lag       <- "log_aid_china_l1"
    rival_aid_lag <- "log_aid_us_l1"
  }
  
  est_cont <- data %>%
    filter(if_all(
      all_of(c("country", "year", fav_var, fav_lag, aid_lag, rival_aid_lag, "latent_div_l1")),
      ~ !is.na(.)
    ))
  
  est_terc <- data %>%
    filter(if_all(
      all_of(c("country", "year", fav_var, fav_lag, aid_lag, rival_aid_lag, "tercile_latent_f")),
      ~ !is.na(.)
    ))
  
  est_bin <- data %>%
    filter(if_all(
      all_of(c("country", "year", fav_var, fav_lag, aid_lag, rival_aid_lag, "low_div_latent")),
      ~ !is.na(.)
    ))
  
  # M1: continuous latent interaction
  f_cont <- as.formula(
    paste(
      fav_var, "~",
      fav_lag, "+", rival_aid_lag, "+",
      aid_lag, "* latent_div_l1 +",
      "country + year"
    )
  )
  m_cont <- lm(f_cont, data = est_cont)
  r_cont <- get_both_se(m_cont)
  
  # M2: latent terciles
  f_terc <- as.formula(
    paste(
      fav_var, "~",
      fav_lag, "+", rival_aid_lag, "+",
      aid_lag, "* tercile_latent_f +",
      "country + year"
    )
  )
  m_terc <- lm(f_terc, data = est_terc)
  r_terc <- get_both_se(m_terc)
  
  # M3: binary split T1 vs (T2+T3)
  f_bin <- as.formula(
    paste(
      fav_var, "~",
      fav_lag, "+", rival_aid_lag, "+",
      aid_lag, "* low_div_latent +",
      "country + year"
    )
  )
  m_bin <- lm(f_bin, data = est_bin)
  r_bin <- get_both_se(m_bin)
  
  # Implied slopes
  slope_t1 <- get_coef0(m_terc, aid_lag)
  slope_t2 <- slope_t1 + get_interaction_coef(m_terc, "tercile_latent_fT2_latent", aid_lag)
  slope_t3 <- slope_t1 + get_interaction_coef(m_terc, "tercile_latent_fT3_latent", aid_lag)
  
  # Binary: baseline is T2+T3 because low_div_latent = 1 for T1
  slope_t2t3 <- get_coef0(m_bin, aid_lag)
  slope_t1_bin <- slope_t2t3 + get_interaction_coef(m_bin, "low_div_latent", aid_lag)
  
  slope_table <- data.frame(
    donor = donor,
    model = c(
      "M2: T1 low diversion",
      "M2: T2 mid diversion",
      "M2: T3 high diversion",
      "M3: T2+T3 pooled",
      "M3: T1 low diversion"
    ),
    aid_slope = c(slope_t1, slope_t2, slope_t3, slope_t2t3, slope_t1_bin)
  )
  
  list(
    donor       = donor,
    continuous  = r_cont,
    terciles    = r_terc,
    binary      = r_bin,
    slope_table = slope_table,
    m_cont_obj  = m_cont,
    m_terc_obj  = m_terc,
    m_bin_obj   = m_bin,
    n_cont      = nobs(m_cont),
    n_terc      = nobs(m_terc),
    n_bin       = nobs(m_bin)
  )
}


# -------------------------------------------------------
# 6. Run models
# -------------------------------------------------------
cat("\n\nFitting latent FE models...\n")

US_latent_main  <- run_h2_latent_fe(df_h2, donor = "US")
CHN_latent_main <- run_h2_latent_fe(df_h2, donor = "CHN")


# -------------------------------------------------------
# 7. Print results
# -------------------------------------------------------
print_latent_results <- function(res) {
  cat("\n", strrep("=", 70), "\n", sep = "")
  cat(sprintf("DONOR: %s | Main H2 latent specifications | FE: country + year\n", res$donor))
  cat(sprintf("N(continuous) = %d | N(terciles) = %d | N(binary) = %d\n",
              res$n_cont, res$n_terc, res$n_bin))
  cat(strrep("=", 70), "\n\n")
  
  cat("--- M1: Continuous latent interaction | REGULAR SE ---\n")
  print(res$continuous$regular)
  cat("\n--- M1: Continuous latent interaction | CLUSTERED SE ---\n")
  print(res$continuous$clustered)
  
  cat("\n--- M2: Latent tercile interaction | REGULAR SE ---\n")
  print(res$terciles$regular)
  cat("\n--- M2: Latent tercile interaction | CLUSTERED SE ---\n")
  print(res$terciles$clustered)
  
  cat("\n--- M3: Binary interaction T1 vs (T2+T3) | REGULAR SE ---\n")
  print(res$binary$regular)
  cat("\n--- M3: Binary interaction T1 vs (T2+T3) | CLUSTERED SE ---\n")
  print(res$binary$clustered)
  
  cat("\n--- Implied aid-on-favorability slopes ---\n")
  print(res$slope_table, digits = 4, row.names = FALSE)
  cat("\n")
}

print_latent_results(US_latent_main)
print_latent_results(CHN_latent_main)


# -------------------------------------------------------
# 8. Combined slope table
# -------------------------------------------------------
all_latent_slopes <- bind_rows(
  US_latent_main$slope_table,
  CHN_latent_main$slope_table
)

cat("\n", strrep("=", 70), "\n", sep = "")
cat("COMBINED LATENT FE SLOPES\n")
cat(strrep("=", 70), "\n\n")
print(all_latent_slopes, digits = 4, row.names = FALSE)


# -------------------------------------------------------
# 9. Robustness: contemporaneous latent diversion, FE
#    again showing both regular and clustered SE
# -------------------------------------------------------
cat("\n", strrep("=", 70), "\n", sep = "")
cat("ROBUSTNESS: contemporaneous latent diversion, FE\n")
cat(strrep("=", 70), "\n\n")

for (donor_choice in c("US", "CHN")) {
  
  fav_var       <- if (donor_choice == "US") "us_prop_fav" else "china_prop_fav"
  fav_lag       <- if (donor_choice == "US") "us_prop_fav_l1" else "china_prop_fav_l1"
  aid_lag       <- if (donor_choice == "US") "log_aid_us_l1" else "log_aid_china_l1"
  rival_aid_lag <- if (donor_choice == "US") "log_aid_china_l1" else "log_aid_us_l1"
  
  f_rob <- as.formula(
    paste(
      fav_var, "~",
      fav_lag, "+", rival_aid_lag, "+",
      aid_lag, "* latent_diversion_risk +",
      "country + year"
    )
  )
  
  est_rob <- df_h2 %>%
    filter(if_all(
      all_of(c("country", "year", fav_var, fav_lag, aid_lag, rival_aid_lag, "latent_diversion_risk")),
      ~ !is.na(.)
    ))
  
  m_rob <- lm(f_rob, data = est_rob)
  
  cat(sprintf("\n%s | contemporaneous latent diversion, FE | REGULAR SE:\n", donor_choice))
  cat(sprintf("N = %d\n", nobs(m_rob)))
  print(coeftest(m_rob))
  
  cat(sprintf("\n%s | contemporaneous latent diversion, FE | CLUSTERED SE:\n", donor_choice))
  cat(sprintf("N = %d\n", nobs(m_rob)))
  print(coeftest(m_rob, vcov = vcovCL(m_rob, cluster = ~ country)))
}

