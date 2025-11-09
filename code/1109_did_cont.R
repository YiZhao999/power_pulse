# =============================
# 0. Install and load packages
# =============================
needed_packages <- c(
  "lme4", "lmerTest", "tidyverse", "sjPlot", "broom.mixed",
  "ggeffects", "ggplot2", "dplyr", "lubridate",
  "did", "modelsummary", "fixest"
)
new_packages <- needed_packages[!(needed_packages %in% installed.packages()[, "Package"])]
if (length(new_packages)) install.packages(new_packages)
invisible(lapply(needed_packages, library, character.only = TRUE))
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

# --- 1. Define binary treatment timing ---
# --- Ensure year is numeric ---
dat <- dat %>%
  mutate(year = as.numeric(as.character(year)))  # convert safely to numeric

dat <- dat %>%
  group_by(region) %>%
  mutate(
    # average pre-2015 aid per region
    aid_mean_pre = mean(log_US[year < 2015], na.rm = TRUE),
    # region-year deviation from pre-2015 average
    delta_aid = log_US - aid_mean_pre
  ) %>%
  ungroup()

# Restrict to pre-treatment period
dat_pre <- dat %>% filter(year < 2015)

# Regress favorability on delta_aid × year FE, with region + year FE
pretrend_model <- feols(
  fav_us_weighted ~ delta_aid:i(year, ref = 2014) | region + year,
  data = dat_pre,
  cluster = "region"
)

summary(pretrend_model)

library(stringr)

pre_df <- broom::tidy(pretrend_model) %>%
  filter(str_detect(term, "delta_aid:year::")) %>%   # only keep interaction terms
  mutate(
    year = as.numeric(str_remove(term, "delta_aid:year::")),
    ci_low = estimate - 1.96 * std.error,
    ci_high = estimate + 1.96 * std.error
  )


# --- Visualization ---
ggplot(pre_df, aes(x = year, y = estimate)) +
  geom_line(color = "darkblue", size = 1) +
  geom_point(color = "darkblue", size = 2) +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high),
              fill = "blue", alpha = 0.2) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = "Parallel Trend Test (Continuous Treatment, Pre-2015)",
    x = "Year",
    y = "Marginal Effect of Aid Intensity on Favorability (vs. 2014)"
  ) +
  theme_minimal(base_size = 13)

# --- Continuous-treatment DID model ---
did_model_cont <- feols(
  fav_us_weighted ~ delta_aid * post_2015 | region + year,
  data = dat,
  cluster = "region"
)

summary(did_model_cont)
library(ggplot2)
library(broom)

# Extract coefficient info
did_df <- tidy(did_model_cont) %>%
  filter(term %in% c("delta_aid", "delta_aid:post_2015")) %>%
  mutate(term = recode(term,
                       "delta_aid" = "Aid Intensity (Pre-2015)",
                       "delta_aid:post_2015" = "Aid Intensity × Post-2015"))

# Plot coefficients
ggplot(did_df, aes(x = term, y = estimate)) +
  geom_point(size = 3, color = "darkblue") +
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error,
                    ymax = estimate + 1.96 * std.error),
                width = 0.15, color = "darkblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Continuous DID Estimate: Effect of Aid Intensity on Favorability",
       x = NULL,
       y = "Estimated Coefficient (±95% CI)") +
  theme_minimal(base_size = 13)



