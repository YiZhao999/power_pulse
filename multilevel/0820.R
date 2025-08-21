library(lme4)
library(lmerTest)
library(tidyverse)
library(sjPlot)
library(broom.mixed)
library(dplyr)

data_path <- "~/Desktop/SUMMER2025/office hour/office hour 8/0814/0814_nigeria.csv"
raw <- read.csv(data_path)

dat <- raw %>%
  mutate(
    year            = as.factor(year),
    region          = as.factor(region),
    fav_us_weighted        = fav_us * weight,
    fav_china_weighted     = fav_china * weight,
    econ_weighted          = econ * weight,
    satisfaction_weighted  = satisfaction * weight,
    log_CHN = log1p(CHN_comm),
    log_US  = log1p(USA_comm)
  ) %>%
  group_by(region) %>%
  mutate(satisf_cwc = econ_weighted - mean(econ_weighted, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    is_apc     = as.numeric(is_apc),
    is_pdp     = as.numeric(is_pdp),
    post_2015  = as.numeric(post_2015)
  )

# Compute region-year aggregate party shares
agg_party_df <- dat %>%
  group_by(region, year) %>%
  summarise(
    share_apc = mean(is_apc, na.rm = TRUE),
    share_pdp = mean(is_pdp, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(agg_party = share_apc)  

# Join back to individual-level data
dat <- dat %>%
  left_join(agg_party_df %>% select(region, year, agg_party), by = c("region", "year"))

# Function to select variables for modeling
make_model_df <- function(dat, vars) {
  out <- dat %>% select(all_of(vars)) %>% drop_na()
  n_total <- nrow(dat)
  n_used  <- nrow(out)
  message(sprintf("Kept %d / %d rows (%.1f%%) after NA-drop for vars: %s",
                  n_used, n_total, 100 * n_used / max(1, n_total), paste(vars, collapse = ", ")))
  out
}

# --- competition variables at region-year ---
dat <- dat %>%
  mutate(
    aid_total_log = log1p(CHN_comm + USA_comm),
    aid_diff      = log1p(CHN_comm) - log1p(USA_comm),
    # optional: shareCHN = CHN_comm / pmax(CHN_comm + USA_comm, 1e-6)
  )

# Model: China favorability with donor competition
m_comp_china <- lmer(
  fav_china_weighted ~ 
    satisf_cwc + is_apc + is_pdp +
    aid_total_log + aid_diff +
    agg_party + post_2015 +
    aid_diff:agg_party + aid_diff:post_2015 +
    aid_diff:is_apc + aid_diff:is_pdp +
    (1 + is_apc + is_pdp | region) + (1 | year),
  data = dat %>% drop_na(fav_china_weighted, satisf_cwc, is_apc, is_pdp,
                         aid_total_log, aid_diff, agg_party, post_2015, region, year)
)

# Model: US favorability with donor competition
m_comp_us <- lmer(
  fav_us_weighted ~ 
    satisf_cwc + is_apc + is_pdp +
    aid_total_log + aid_diff +
    agg_party + post_2015 +
    aid_diff:agg_party + aid_diff:post_2015 +
    aid_diff:is_apc + aid_diff:is_pdp +
    (1 + is_apc + is_pdp | region) + (1 | year),
  data = dat %>% drop_na(fav_us_weighted, satisf_cwc, is_apc, is_pdp,
                         aid_total_log, aid_diff, agg_party, post_2015, region, year)
)

summary(m_comp_china)
summary(m_comp_us)
