packages <- c("lme4", "lmerTest", "tidyverse", "sjPlot", "broom.mixed")
installed <- packages %in% rownames(installed.packages())
if (any(!installed)) install.packages(packages[!installed])

library(lme4)
library(lmerTest)
library(tidyverse)
library(sjPlot)
library(broom.mixed)

setwd("~/Desktop/0730_test")
test <- read.csv("0730_updated.csv")

test <- test %>%
  mutate(
    year = as.factor(year),
    region = as.factor(region_mapped),
    fav_us_weighted = fav_us * weight,
    fav_china_weighted = fav_china * weight,
    econ_weighted = econ * weight,
    satisfaction_weighted = satisfaction * weight,
    log_CHN = log1p(chn_comm),
    log_US = log1p(wb_comm),
    aid_interaction = log_CHN * log_US
  )

test <- test %>%
  group_by(region) %>%
  mutate(satisf_cwc = satisfaction_weighted - mean(satisfaction_weighted)) %>%
  ungroup()

model_china_1 <- lmer(fav_china_weighted ~ satisf_cwc * log_CHN + (1 | region_mapped),
                      data = test)
summary(model_china_1)
model_china_2 <- lmer(fav_china_weighted ~ satisf_cwc * log_CHN + (1 + satisf_cwc | region_mapped),
                      data = test)
summary(model_china_2)
model_china_3 <- lmer(fav_china_weighted ~ satisf_cwc * log_US + (1 | region_mapped),
                      data = test)
summary(model_china_3)
model_china_4 <- lmer(fav_china_weighted ~ satisf_cwc * log_US + (1 + satisf_cwc | region_mapped),
                      data = test)
summary(model_china_4)
tab_model(model_china_1, model_china_2, model_china_3, model_china_4,
          show.ci = FALSE, 
          show.re.var = TRUE, 
          show.icc = TRUE,
          title = "China and WB Aid Interaction Effects",
          dv.labels = c("Model 1: China", "Model 2: China (Random Slope)", 
                        "Model 3: WB", "Model 4: WB (Random Slope)"))
model_apc <- lmer(
  fav_china ~ satisf_cwc * is_apc * post_2015 * log_CHN +
    (1 | region_mapped) + (1 | year),
  data = test
)
model_pdp <- lmer(
  fav_china ~ satisf_cwc * is_pdp * post_2015 * log_CHN +
    (1 | region_mapped) + (1 | year),
  data = test
)
summary(model_apc)
summary(model_pdp)

tab_model(model_apc, model_pdp, 
          show.ci = FALSE, 
          show.re.var = TRUE, 
          show.icc = TRUE,
          title = "Interaction Effects by Party Affiliation",
          dv.labels = c("APC Supporters", "PDP Supporters"))
