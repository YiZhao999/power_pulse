packages <- c("lme4", "lmerTest", "tidyverse", "sjPlot", "broom.mixed")
installed <- packages %in% rownames(installed.packages())
if (any(!installed)) install.packages(packages[!installed])

library(lme4)
library(lmerTest)
library(tidyverse)
library(sjPlot)
library(broom.mixed)

setwd("~/Desktop/0722_test")
test <- read.csv("0722_mapped_region.csv")

# Data transformation
test <- test %>%
  mutate(
    year = as.factor(year),
    region = as.factor(region_mapped),
    fav_us_weighted = fav_us * weight,
    fav_china_weighted = fav_China * weight,
    econ_weighted = econ * weight,
    satisfaction_weighted = satisfaction * weight,
    log_CHN = log1p(CHN_comm),
    log_US = log1p(WB_comm),
    log_CHN_nat = log1p(CHN_NGA),
    log_US_nat = log1p(WB_NGA),
    aid_interaction = log_CHN * log_US,
    nat_aid_interaction = log_CHN_nat * log_US_nat
  )

model_us <- lmer(
  fav_us_weighted ~ econ_weighted + satisfaction_weighted +
    log_CHN + log_US + aid_interaction +
    (1 | region) + (1 | year),
  data = test
)

model_us_national <- lmer(
  fav_us_weighted ~ econ_weighted + satisfaction_weighted +
    log_CHN + log_US + aid_interaction +
    log_CHN_nat + log_US_nat + nat_aid_interaction +
    (1 | region) + (1 | year),
  data = test
)

model_china <- lmer(
  fav_china_weighted ~ econ_weighted + satisfaction_weighted +
    log_CHN + log_US + aid_interaction +
    (1 | region) + (1 | year),
  data = test
)

model_china_national <- lmer(
  fav_china_weighted ~ econ_weighted + satisfaction_weighted +
    log_CHN + log_US + aid_interaction +
    log_CHN_nat + log_US_nat + nat_aid_interaction +
    (1 | region) + (1 | year),
  data = test
)

# Print model summaries
summary(model_us)
summary(model_us_national)
summary(model_china)
summary(model_china_national)

# Tabulated comparison
tab_model(model_us, model_us_national, model_china, model_china_national,
          title = "Model Comparison: Favorability Toward US and China",
          dv.labels = c("US (Regional)", "US (Regional + National)",
                        "China (Regional)", "China (Regional + National)"))

# Fixed effects
plot_model(model_us, type = "est", title = "US Favorability – Fixed Effects")
plot_model(model_us_national, type = "est", title = "US Favorability – National Aid Included")
plot_model(model_china, type = "est", title = "China Favorability – Fixed Effects")
plot_model(model_china_national, type = "est", title = "China Favorability – National Aid Included")

# Random effects
plot_model(model_us_national, type = "re", title = "US Favorability – Random Effects")
plot_model(model_china_national, type = "re", title = "China Favorability – Random Effects")

ggplot(test, aes(x = log_US_nat, y = fav_us_weighted, color = log_CHN_nat)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  labs(
    title = "Interaction: National-Level US & China Aid on US Favorability",
    x = "US Aid (National, log)", y = "Favorability Toward US", color = "China Aid (log)"
  ) +
  theme_minimal()
ggplot(test, aes(x = log_CHN_nat, y = fav_china_weighted, color = log_US_nat)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  labs(
    title = "Interaction: National-Level China & US Aid on China Favorability",
    x = "China Aid (National, log)", y = "Favorability Toward China", color = "US Aid (log)"
  ) +
  theme_minimal()
