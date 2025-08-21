setwd("~/Desktop/test")  # Change this to your actual path
library(readr)
test <- read_csv("test_nigeria.csv")
# Check the structure of the data
str(test)

library(lme4)

# Model 1: Favorability toward China ~ Chinese aid
model_china <- lmer(
  fav_China ~ 
    CHN_dummy_comm +                   # REGION-YEAR LEVEL predictor
    econ + satisfaction +         # INDIVIDUAL LEVEL controls
    (1 | region) +               # Random intercept by REGION
    (1 | year),                  # Random intercept by YEAR
  data = test
)

# Model 2: Favorability toward US ~ WB aid
model_us <- lmer(
  fav_us ~ 
    WB_dummy_comm +                     # REGION-YEAR LEVEL predictor
    econ + satisfaction +        # INDIVIDUAL LEVEL controls
    (1 | region) +              # Random intercept by REGION
    (1 | year),                 # Random intercept by YEAR
  data = test
)
summary(model_china)
summary(model_us)
install.packages("sjPlot")
install.packages("ggeffects")
install.packages("ggplot2")

library(sjPlot)
library(ggeffects)
library(ggplot2)
# Model 1: Favorability toward China
plot_model(model_china, 
           type = "est", 
           show.values = TRUE, 
           value.offset = 0.3,
           title = "Effects on Favorability Toward China")

# Model 2: Favorability toward US
plot_model(model_us, 
           type = "est", 
           show.values = TRUE, 
           value.offset = 0.3,
           title = "Effects on Favorability Toward the US")
# China aid dummy
pred_china <- ggeffect(model_china, terms = "CHN_dummy_comm")
# Package `effects` required for this function to work.
ggplot(pred_china, aes(x = x, y = predicted)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1) +
  labs(
    title = "Predicted Favorability Toward China by Chinese Aid Dummy",
    x = "Region Received Chinese Aid (0 = No, 1 = Yes)",
    y = "Predicted Favorability"
  ) +
  theme_minimal()

# US aid dummy
pred_us <- ggeffect(model_us, terms = "WB_dummy_comm")
ggplot(pred_us, aes(x = x, y = predicted)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1) +
  labs(
    title = "Predicted Favorability Toward US by WB Aid Dummy",
    x = "Region Received WB Aid (0 = No, 1 = Yes)",
    y = "Predicted Favorability"
  ) +
  theme_minimal()
