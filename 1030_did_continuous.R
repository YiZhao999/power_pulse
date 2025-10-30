needed_packages <- c(
  "lme4", "lmerTest", "tidyverse", "sjPlot", "broom.mixed",
  "ggeffects", "ggplot2", "dplyr", "lubridate",
  "did", "modelsummary"
)
new_packages <- needed_packages[!(needed_packages %in% installed.packages()[, "Package"])]
if (length(new_packages)) install.packages(new_packages)
library(lme4)
library(lmerTest)
library(tidyverse)
library(sjPlot)
library(broom.mixed)
library(ggeffects)
library(ggplot2)
library(dplyr)
library(lubridate)
library(did)
library(modelsummary)

data_path <- "~/Desktop/nigeria.csv"
raw <- read.csv(data_path)
dat <- raw %>%
  mutate(
    year                = as.factor(year),
    region              = as.factor(region),
    fav_us_weighted     = fav_us * weight,
    fav_china_weighted  = fav_china * weight,
    econ_weighted       = econ * weight,
    satisfaction_weighted = satisfaction * weight,
    # Region-year aid (donor-specific)
    log_CHN = log1p(CHN_comm),
    log_US  = log1p(USA_comm)
  ) %>%
  group_by(region) %>%
  mutate(satisf_cwc = econ_weighted - mean(econ_weighted, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    party_affil_apc = as.numeric(is_apc),  # 1 = APC, 0 = non-APC
    is_pdp          = as.numeric(is_pdp),  
    post_2015       = as.numeric(post_2015)
  )

# =============================
# Aggregate Party Support (Region-Year)
# =============================
agg_party_df <- dat %>%
  group_by(region, year) %>%
  summarise(
    share_apc = mean(party_affil_apc, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(agg_party = share_apc)

dat <- dat %>%
  left_join(agg_party_df %>% select(region, year, agg_party), by = c("region", "year"))

# =============================
# Country-Level Aid (Country-Year)
# =============================
country_aid_df <- dat %>%
  group_by(year) %>%
  summarise(
    CHN_country = sum(CHN_comm, na.rm = TRUE),
    USA_country = sum(USA_comm, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    log_CHN_country = log1p(CHN_country),
    log_US_country  = log1p(USA_country)
  )

dat <- dat %>%
  left_join(country_aid_df %>% select(year, log_CHN_country, log_US_country),
            by = "year")

make_model_df <- function(dat, vars) {
  out <- dat %>% select(all_of(vars)) %>% drop_na()
  n_total <- nrow(dat); n_used <- nrow(out)
  message(sprintf("Kept %d / %d rows (%.1f%%) after NA-drop for vars: %s",
                  n_used, n_total, 100 * n_used / max(1, n_total), paste(vars, collapse = ", ")))
  out
}

did_ml <- dat %>%
  mutate(
    # Convert year to numeric first
    year_num = as.numeric(as.character(year)),
    
    # log of Chinese commitments, continuous treatment
    log_CHN = log1p(CHN_comm),
    
    # Post-treatment period (e.g., post-2015)
    post_2015 = as.numeric(year_num >= 2015),
    
    # Interaction term for DID
    logCHN_post = log_CHN * post_2015,
    
    # Centered individual-level controls
    econ_cwc   = satisf_cwc,
    party_apc  = as.numeric(party_affil_apc),
    
    # Factorize grouping variables for random effects
    region = as.factor(region),
    year   = as.factor(year)
  ) %>%
  drop_na(fav_china_weighted, log_CHN, econ_cwc, party_apc)

model_did_cont <- lmer(
  fav_china_weighted ~ 
    post_2015 * log_CHN + 
    econ_cwc + party_apc +
    (1 | region) + (1 | year),
  data = did_ml
)

summary(model_did_cont)

gge_did <- ggpredict(model_did_cont, terms = c("log_CHN [all]", "post_2015"))
plot(gge_did) +
  labs(title = "Marginal Effects of Chinese Aid on Favorability (Pre vs. Post 2015)",
       x = "Log(Chinese Aid Commitments)", y = "Predicted Favorability") +
  theme_minimal()

# =============================
# US MODEL (continuous DID specification)
# =============================


# --- Data prep ---
did_us <- dat %>%
  mutate(
    # Convert year to numeric (for post indicator)
    year_num = as.numeric(as.character(year)),
    
    # log of U.S. commitments, continuous treatment
    log_US = log1p(USA_comm),
    
    # Post-treatment period (e.g., post-2015)
    post_2015 = as.numeric(year_num >= 2015),
    
    # Interaction term for DID
    logUS_post = log_US * post_2015,
    
    # Centered individual-level controls
    econ_cwc   = satisf_cwc,
    party_apc  = as.numeric(party_affil_apc),
    
    # Factorize grouping variables for random effects
    region = as.factor(region),
    year   = as.factor(year)
  ) %>%
  drop_na(fav_us_weighted, log_US, econ_cwc, party_apc)

model_did_us <- lmer(
  fav_us_weighted ~ 
    post_2015 * log_US + 
    econ_cwc + party_apc +
    (1 | region) + (1 | year),
  data = did_us
)

summary(model_did_us)
gge_us <- ggpredict(model_did_us, terms = c("log_US [all]", "post_2015"))
plot(gge_us) +
  labs(title = "Marginal Effects of U.S. Aid on Favorability (Pre vs. Post 2015)",
       x = "Log(U.S. Aid Commitments)",
       y = "Predicted Favorability toward the U.S.") +
  theme_minimal()
