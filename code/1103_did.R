# =============================
# 0. Install and load packages
# =============================
needed_packages <- c(
  "lme4", "lmerTest", "tidyverse", "sjPlot", "broom.mixed",
  "ggeffects", "ggplot2", "dplyr", "lubridate",
  "did", "modelsummary"
)
new_packages <- needed_packages[!(needed_packages %in% installed.packages()[, "Package"])]
if (length(new_packages)) install.packages(new_packages)
invisible(lapply(needed_packages, library, character.only = TRUE))

# =============================
# 1. Load and prepare data
# =============================
data_path <- "~/Desktop/DID/nigeria.csv"
raw <- read.csv(data_path)

dat <- raw %>%
  mutate(
    year                = as.factor(year),
    region              = as.factor(region),
    fav_us_weighted     = fav_us * weight,
    fav_china_weighted  = fav_china * weight,
    econ_weighted       = econ * weight,
    satisfaction_weighted = satisfaction * weight,
    log_CHN             = log1p(CHN_comm),
    log_US              = log1p(USA_comm)
  ) %>%
  group_by(region) %>%
  mutate(satisf_cwc = econ_weighted - mean(econ_weighted, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    party_affil_apc = as.numeric(is_apc),
    is_pdp          = as.numeric(is_pdp),
    post_2015       = as.numeric(post_2015)
  )

# Aggregate region-year APC share
agg_party_df <- dat %>%
  group_by(region, year) %>%
  summarise(share_apc = mean(party_affil_apc, na.rm = TRUE), .groups = "drop")

dat <- dat %>%
  left_join(agg_party_df, by = c("region", "year")) %>%
  mutate(agg_party = share_apc)

# Country-level aid
country_aid_df <- dat %>%
  group_by(year) %>%
  summarise(CHN_country = sum(CHN_comm, na.rm = TRUE),
            USA_country = sum(USA_comm, na.rm = TRUE), .groups = "drop") %>%
  mutate(log_CHN_country = log1p(CHN_country),
         log_US_country  = log1p(USA_country))

dat <- dat %>%
  left_join(country_aid_df %>% select(year, log_CHN_country, log_US_country), by = "year")

# =============================
# 2. Pre-trend diagnostic (parallel trends)
# =============================

# Visual check: pre- vs post-2015 trends by aid intensity
dat %>%
  mutate(aid_group = ifelse(log_CHN > median(log_CHN, na.rm = TRUE), "High aid", "Low aid")) %>%
  group_by(aid_group, year) %>%
  summarise(mean_fav = mean(fav_china_weighted, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = as.numeric(as.character(year)), y = mean_fav, color = aid_group)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 2015, linetype = "dashed", color = "gray40") +
  theme_minimal() +
  labs(title = "Pre-trend Check: Favorability toward China by Aid Group",
       x = "Year", y = "Average Favorability")

# Regression-based pre-trend test (China)
# Restrict to pre-period only (<= 2015)
dat_pre <- dat %>% filter(as.numeric(as.character(year)) < 2015)
pretrend_test <- lmer(
  fav_china_weighted ~ log_CHN * as.numeric(as.character(year)) +
    econ_weighted + party_affil_apc + (1 | region),
  data = dat_pre
)
summary(pretrend_test)
# --> If log_CHN:year interaction terms are insignificant, parallel trends hold.

# =============================
# 3. DID: Continuous treatment (China)
# =============================

did_ml <- dat %>%
  mutate(
    year_num = as.numeric(as.character(year)),
    post_2015 = as.numeric(year_num >= 2015),
    logCHN_post = log_CHN * post_2015,
    econ_cwc = satisf_cwc,
    party_apc = as.numeric(party_affil_apc),
    region = as.factor(region),
    year = as.factor(year)
  ) %>%
  drop_na(fav_china_weighted, log_CHN, econ_cwc, party_apc)

model_did_cont <- lmer(
  fav_china_weighted ~ post_2015 * log_CHN + econ_cwc + party_apc +
    (1 | region) + (1 | year),
  data = did_ml
)
summary(model_did_cont)

# Visualization of interaction
gge_did <- ggpredict(model_did_cont, terms = c("log_CHN [all]", "post_2015"))
plot(gge_did) +
  labs(title = "Marginal Effects of Chinese Aid on Favorability (Pre vs. Post 2015)",
       x = "Log(Chinese Aid Commitments)", y = "Predicted Favorability") +
  theme_minimal()

# =============================
# 4. DID: Continuous treatment (U.S.)
# =============================
# Visual check: pre- vs post-2015 trends by aid intensity
dat %>%
  mutate(aid_group = ifelse(log_US > median(log_US, na.rm = TRUE), "High aid", "Low aid")) %>%
  group_by(aid_group, year) %>%
  summarise(mean_fav = mean(fav_us_weighted, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = as.numeric(as.character(year)), y = mean_fav, color = aid_group)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 2015, linetype = "dashed", color = "gray40") +
  theme_minimal() +
  labs(title = "Pre-trend Check: Favorability toward US by Aid Group",
       x = "Year", y = "Average Favorability")

# Regression-based pre-trend test (China)
# Restrict to pre-period only (<= 2015)
dat_pre <- dat %>% filter(as.numeric(as.character(year)) < 2015)
pretrend_test <- lmer(
  fav_us_weighted ~ log_US * as.numeric(as.character(year)) +
    econ_weighted + party_affil_apc + (1 | region),
  data = dat_pre
)
summary(pretrend_test)
# --> If log_CHN:year interaction terms are insignificant, parallel trends hold.


did_us <- dat %>%
  mutate(
    year_num = as.numeric(as.character(year)),
    post_2015 = as.numeric(year_num >= 2015),
    logUS_post = log_US * post_2015,
    econ_cwc = satisf_cwc,
    party_apc = as.numeric(party_affil_apc),
    region = as.factor(region),
    year = as.factor(year)
  ) %>%
  drop_na(fav_us_weighted, log_US, econ_cwc, party_apc)

model_did_us <- lmer(
  fav_us_weighted ~ post_2015 * log_US + econ_cwc + party_apc +
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

# =============================
# 5. Placebo Test (False Treatment Year)
# =============================

# Pretend the "treatment" happened in 2013 instead of 2015
dat_placebo_china <- dat %>%
  mutate(
    year_num = as.numeric(as.character(year)),
    placebo_post = as.numeric(year_num >= 2013),
    logCHN_placebo = log_CHN * placebo_post
  )

model_placebo_china <- lmer(
  fav_china_weighted ~ placebo_post * log_CHN + econ_weighted + party_affil_apc +
    (1 | region) + (1 | year),
  data = dat_placebo
)
summary(model_placebo_china)

# --> If placebo interaction (placebo_post:log_CHN) is insignificant,
# it supports the claim that the real DID effect is not driven by spurious pre-trends.

# Optional visualization for placebo
gge_placebo <- ggpredict(model_placebo, terms = c("log_CHN [all]", "placebo_post"))
plot(gge_placebo) +
  labs(title = "Placebo Test (Fake Treatment in 2013)",
       x = "Log(Chinese Aid Commitments)", y = "Predicted Favorability") +
  theme_minimal()

# Pretend the "treatment" happened in 2013 instead of 2015
dat_placebo_us <- dat %>%
  mutate(
    year_num = as.numeric(as.character(year)),
    placebo_post = as.numeric(year_num >= 2013),
    logUS_placebo = log_US * placebo_post
  )

model_placebo_us <- lmer(
  fav_us_weighted ~ placebo_post * log_US + econ_weighted + party_affil_apc +
    (1 | region) + (1 | year),
  data = dat_placebo
)
summary(model_placebo)

# --> If placebo interaction (placebo_post:log_CHN) is insignificant,
# it supports the claim that the real DID effect is not driven by spurious pre-trends.

# Optional visualization for placebo
gge_placebo <- ggpredict(model_placebo_us, terms = c("log_US [all]", "placebo_post"))
plot(gge_placebo) +
  labs(title = "Placebo Test (Fake Treatment in 2013)",
       x = "Log(US Aid Commitments)", y = "Predicted Favorability") +
  theme_minimal()
