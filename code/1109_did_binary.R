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

# --- Define treatment timing: first year region receives any aid ---
treat_timing <- dat %>%
  filter(log_US > 0) %>%
  group_by(region) %>%
  summarise(treat_year = min(year, na.rm = TRUE)) %>%
  ungroup()

# --- Merge timing info back ---
dat <- dat %>%
  left_join(treat_timing, by = "region") %>%
  mutate(
    treat   = ifelse(!is.na(treat_year), 1, 0),
    treated = ifelse(!is.na(treat_year) & year >= treat_year, 1, 0)
  )

# Quick check
table(dat$treat, useNA = "ifany")
dat %>% count(treat, treated)

# Restrict to pre-treatment (before 2015)
dat_pre <- dat %>% filter(year < 2015)

# Pretrend: interactions of treat × year
pretrend_model <- feols(
  fav_us_weighted ~ i(year, treat, ref = 2014) | region + year,
  data = dat_pre,
  cluster = "region"
)

summary(pretrend_model)

# Extract coefficients for plotting
pre_df <- broom::tidy(pretrend_model) %>%
  mutate(term = str_remove(term, "year::")) %>%
  rename(year = term, estimate = estimate, se = std.error) %>%
  mutate(
    year = as.numeric(str_remove(year, ":treat")),
    ci_low = estimate - 1.96 * se,
    ci_high = estimate + 1.96 * se
  )

# --- Plot pre-trend coefficients ---
ggplot(pre_df, aes(x = year, y = estimate)) +
  geom_line(color = "darkblue") +
  geom_point(color = "darkblue") +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.2, fill = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Parallel Trend Test (Pre-2015)",
    x = "Year",
    y = "Treatment × Year Coefficient (vs. 2014)"
  ) +
  theme_minimal(base_size = 13)

# --- 2. Estimate DID model ---

did_model <- feols(
  fav_us_weighted ~ i(post_2015, treat, ref = 0) | region + year,
  data = dat,
  cluster = "region"
)
summary(did_model)

# Keep only regions with treatment year info
dat_event <- dat %>% filter(!is.na(treat_year))

event_study <- feols(
  fav_us_weighted ~ sunab(treat_year, year) | region + year,
  data = dat_event,
  cluster = "region"
)

# Plot dynamic effects
iplot(event_study,
      ref.line = 0,
      xlab = "Years relative to treatment",
      ylab = "Estimated effect on favorability",
      main = "Event Study: Effect of Aid on Public Favorability")
summary(event_study)

