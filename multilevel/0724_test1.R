install.packages("broom.mixed")
library(broom.mixed)
library(dplyr)
library(readr)
library(lme4)

setwd("~/Desktop/0721_test")
test <- read_csv("0721_mapped_region.csv")

test <- test %>%
  mutate(
    fav_us_weighted = fav_us * weight,
    fav_china_weighted = fav_China * weight,
    econ_weighted = econ * weight,
    satisfaction_weighted = satisfaction * weight
  )

# Fit models
model_china_logaid <- lmer(
  fav_china_weighted ~ log1p(CHN_comm) + econ_weighted + satisfaction_weighted + 
    (1 | region_mapped) + (1 | year), data = test
)

model_us_logaid <- lmer(
  fav_us_weighted ~ log1p(WB_comm) + econ_weighted + satisfaction_weighted + 
    (1 | region_mapped) + (1 | year), data = test
)

model_china_dummyaid <- lmer(
  fav_china_weighted ~ log1p(CHN_dummy_comm) + econ_weighted + satisfaction_weighted + 
    (1 | region_mapped) + (1 | year), data = test
)

model_us_dummyaid <- lmer(
  fav_us_weighted ~ log1p(WB_dummy_comm) + econ_weighted + satisfaction_weighted + 
    (1 | region_mapped) + (1 | year), data = test
)

summary(model_china_logaid)
summary(model_us_logaid)
summary(model_china_dummyaid)
summary(model_us_dummyaid)

install.packages(c("ggplot2", "broom.mixed"))
library(broom.mixed)
library(ggplot2)
library(dplyr)

# Extract and label fixed effects
tidy_china_logaid <- tidy(model_china_logaid, effects = "fixed") %>%
  mutate(model = "China (Log Aid)")

tidy_us_logaid <- tidy(model_us_logaid, effects = "fixed") %>%
  mutate(model = "US (Log Aid)")

tidy_china_dummyaid <- tidy(model_china_dummyaid, effects = "fixed") %>%
  mutate(model = "China (Dummy Aid)")

tidy_us_dummyaid <- tidy(model_us_dummyaid, effects = "fixed") %>%
  mutate(model = "US (Dummy Aid)")

# Combine all results
model_results <- bind_rows(
  tidy_china_logaid,
  tidy_us_logaid,
  tidy_china_dummyaid,
  tidy_us_dummyaid
)

# Plot with 95% confidence intervals
ggplot(model_results, aes(x = estimate, y = term, color = model)) +
  geom_point(position = position_dodge(width = 0.5), size = 2.5) +
  geom_errorbarh(aes(xmin = estimate - 1.96*std.error,
                     xmax = estimate + 1.96*std.error),
                 height = 0.2,
                 position = position_dodge(width = 0.5)) +
  facet_wrap(~ model, scales = "free_y") +
  labs(
    title = "Multilevel Model Coefficient Estimates",
    x = "Estimate with 95% CI",
    y = "Predictor"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


