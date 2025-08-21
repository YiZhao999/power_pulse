packages <- c("lme4", "lmerTest", "tidyverse", "sjPlot", "broom.mixed")
installed <- packages %in% rownames(installed.packages())
if (any(!installed)) install.packages(packages[!installed])

library(lme4)
library(lmerTest)
library(tidyverse)
library(sjPlot)
library(broom.mixed)

setwd("~/Desktop/0729_test")
test <- read.csv("0729.csv")

test <- test %>%
  mutate(
    year = as.factor(year),
    region = as.factor(region),
    fav_us_weighted = fav_us * weight,
    fav_china_weighted = fav_China * weight,
    econ_weighted = econ * weight,
    satisfaction_weighted = satisfaction * weight,
    log_CHN = log1p(CHN_comm),
    log_US = log1p(WB_comm),
    log_CHN_nat = log1p(CHN_KEN),
    log_US_nat = log1p(WB_KEN),
    aid_interaction = log_CHN * log_US,
    nat_aid_interaction = log_CHN_nat * log_US_nat
  )

test <- test %>%
  group_by(region) %>%
  mutate(satisf_cwc = satisfaction_weighted - mean(satisfaction_weighted)) %>%
  ungroup()

model_china_1 <- lmer(fav_china_weighted ~ satisf_cwc * log_CHN + (1 | region),
              data = test)
summary(model_china_1)
model_china_2 <- lmer(fav_china_weighted ~ satisf_cwc * log_CHN + (1 + satisf_cwc | region),
                data = test)
summary(model_china_2)
model_china_3 <- lmer(fav_china_weighted ~ satisf_cwc * log_US + (1 | region),
                      data = test)
summary(model_china_3)
model_china_4 <- lmer(fav_china_weighted ~ satisf_cwc * log_US + (1 + satisf_cwc | region),
                      data = test)
summary(model_china_4)

tab_model(model_china_1, model_china_2, model_china_3, model_china_4,
          show.ci = FALSE, 
          show.re.var = TRUE, 
          show.icc = TRUE,
          title = "China and WB Aid Interaction Effects",
          dv.labels = c("Model 1: China", "Model 2: China (Random Slope)", 
                        "Model 3: WB", "Model 4: WB (Random Slope)"))



