# =============================
# Libraries
# =============================
library(lme4)
library(lmerTest)
library(tidyverse)
library(sjPlot)
library(broom.mixed)
library(ggeffects)
library(ggplot2)
library(dplyr)

# =============================
# Load Data
# =============================
data_path <- "~/Desktop/SUMMER2025/office hour/office hour 8/0814/0814_nigeria.csv"
raw <- read.csv(data_path)

# =============================
# Data Preparation
# =============================
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

# =============================
# CHINA MODEL (Y = favorability toward China)
#   Matches:
#   Y_irt = δ000 + (δ001 CountryAid_t + δ002 Post2015_t + δ003 CountryAid_t×Post2015_t)
#            + (γ01 RegionAid_rt + γ02 AggParty_rt + γ03 RegionAid_rt×AggParty_rt)
#            + [γ10 + γ11 RegionAid_rt + γ12 AggParty_rt] × PartyAffil_irt
#            + β2 GovSat_irt
#            + u0r + u1r*PartyAffil_irt + v0t + e_irt
# =============================
vars_china <- c("fav_china_weighted", "satisf_cwc",
                "party_affil_apc", "agg_party", "post_2015",
                "log_CHN",          # RegionAid_rt (China)
                "log_CHN_country",  # CountryAid_t (China)
                "region", "year")

df_china <- dat %>%
  mutate(region = droplevels(region), year = droplevels(year)) %>%
  make_model_df(vars_china)

model_china <- lmer(
  fav_china_weighted ~ 
    # country-year effects
    log_CHN_country + post_2015 + log_CHN_country:post_2015 +
    # region-year effects
    log_CHN + agg_party + log_CHN:agg_party +
    # cross-level interactions with PartyAffil (APC)
    party_affil_apc + log_CHN:party_affil_apc + agg_party:party_affil_apc +
    # individual control
    satisf_cwc +
    # random effects
    (1 + party_affil_apc | region) + (1 | year),
  data = df_china
)

cat("\n--- CHINA MODEL ---\n")
print(summary(model_china))
cat("Singular fit? ", isSingular(model_china), "\n\n")

# =============================
# US MODEL (Y = favorability toward US)
#   Same structure, swap CHN with US everywhere
# =============================
vars_us <- c("fav_us_weighted", "satisf_cwc",
             "party_affil_apc", "agg_party", "post_2015",
             "log_US",          # RegionAid_rt (US)
             "log_US_country",  # CountryAid_t (US)
             "region", "year")

df_us <- dat %>%
  mutate(region = droplevels(region), year = droplevels(year)) %>%
  make_model_df(vars_us)

model_us <- lmer(
  fav_us_weighted ~ 
    # country-year effects
    log_US_country + post_2015 + log_US_country:post_2015 +
    # region-year effects
    log_US + agg_party + log_US:agg_party +
    # cross-level interactions with PartyAffil (APC)
    party_affil_apc + log_US:party_affil_apc + agg_party:party_affil_apc +
    # individual control
    satisf_cwc +
    # random effects
    (1 + party_affil_apc | region) + (1 | year),
  data = df_us
)

cat("\n--- US MODEL ---\n")
print(summary(model_us))
cat("Singular fit? ", isSingular(model_us), "\n\n")

# =============================
# Side-by-side table
# =============================
suppressWarnings(
  tab_model(
    model_china, model_us,
    show.ci = FALSE, show.re.var = TRUE, show.icc = TRUE,
    title = "Multilevel Models with Country-Year & Region-Year Aid (Math-Consistent Spec.)",
    dv.labels = c("Favorability toward China", "Favorability toward US")
  )
)

# =============================
# VISUALIZATION 1: CountryAid × Post2015
# (lines over CountryAid; linetype by Post2015)
# =============================
pred_china_country <- ggpredict(model_china, terms = c("log_CHN_country [all]", "post_2015"))
ggplot(pred_china_country, aes(x = x, y = predicted, linetype = group)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15, color = NA) +
  labs(x = "China Country Aid (log)", y = "Predicted Favorability toward China",
       title = "Country-Level China Aid × Post-2015", linetype = "Post-2015") +
  theme_minimal(base_size = 14)

pred_us_country <- ggpredict(model_us, terms = c("log_US_country [all]", "post_2015"))
ggplot(pred_us_country, aes(x = x, y = predicted, linetype = group)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15, color = NA) +
  labs(x = "US Country Aid (log)", y = "Predicted Favorability toward US",
       title = "Country-Level US Aid × Post-2015", linetype = "Post-2015") +
  theme_minimal(base_size = 14)

# =============================
# VISUALIZATION 2: RegionAid × PartyAffil at Low vs. High AggParty
# (lines over RegionAid; color by PartyAffil; facet by AggParty quantile)
# =============================
# compute low/high agg_party cutpoints from each model's data
low_agp_ch <- as.numeric(quantile(df_china$agg_party, 0.25, na.rm = TRUE))
high_agp_ch <- as.numeric(quantile(df_china$agg_party, 0.75, na.rm = TRUE))

pred_china_region <- ggpredict(
  model_china,
  terms = c(
    "log_CHN [all]",
    "party_affil_apc",
    sprintf("agg_party [%.3f,%.3f]", low_agp_ch, high_agp_ch)
  )
)

pred_china_region$agg_band <- factor(pred_china_region$facet,
                                     labels = c("Low APC Region (Q1)", "High APC Region (Q3)"))
pred_china_region$party <- ifelse(pred_china_region$group == "0", "Non-APC", "APC")

ggplot(pred_china_region, aes(x = x, y = predicted, color = party)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = party), alpha = 0.15, color = NA) +
  facet_wrap(~ agg_band) +
  labs(x = "China Region Aid (log)", y = "Predicted Favorability toward China",
       title = "Region-Level China Aid × Party Affiliation\n(at Low vs. High Regional APC Support)",
       color = "Party", fill = "Party") +
  theme_minimal(base_size = 14)

low_agp_us <- as.numeric(quantile(df_us$agg_party, 0.25, na.rm = TRUE))
high_agp_us <- as.numeric(quantile(df_us$agg_party, 0.75, na.rm = TRUE))

pred_us_region <- ggpredict(
  model_us,
  terms = c(
    "log_US [all]",
    "party_affil_apc",
    sprintf("agg_party [%.3f,%.3f]", low_agp_us, high_agp_us)
  )
)

pred_us_region$agg_band <- factor(pred_us_region$facet,
                                  labels = c("Low APC Region (Q1)", "High APC Region (Q3)"))
pred_us_region$party <- ifelse(pred_us_region$group == "0", "Non-APC", "APC")

ggplot(pred_us_region, aes(x = x, y = predicted, color = party)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = party), alpha = 0.15, color = NA) +
  facet_wrap(~ agg_band) +
  labs(x = "US Region Aid (log)", y = "Predicted Favorability toward US",
       title = "Region-Level US Aid × Party Affiliation\n(at Low vs. High Regional APC Support)",
       color = "Party", fill = "Party") +
  theme_minimal(base_size = 14)
