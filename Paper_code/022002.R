library(tidyverse)
library(tseries)
library(lmtest)
library(sandwich)

##################################################
# 2. Load Data
##################################################

nig <- read.csv("~/Desktop/PhD/FALL2025/PS2702/final/1109/nigeria.csv")
align <- read.csv("~/PycharmProjects/MA_paper/20260107/merged.csv")

##################################################
# 3. Rescale Favorability (higher = more favorable)
##################################################

nig <- nig %>%
  mutate(
    fav_us_rescaled = 4 - fav_us,
    fav_china_rescaled = 4 - fav_china
  )

##################################################
# 4. Aggregate to Nigeria-Year Level
##################################################

fav_year <- nig %>%
  group_by(year) %>%
  summarise(
    fav_us = mean(fav_us_rescaled, na.rm = TRUE),
    fav_china = mean(fav_china_rescaled, na.rm = TRUE),
    .groups = "drop"
  )

##################################################
# 5. Keep Nigeria Only from Alignment Data
##################################################

align_nig <- align %>%
  filter(Countryname == "Nigeria")

##################################################
# 6. Merge (Now 1 Row Per Year)
##################################################

df <- fav_year %>%
  inner_join(align_nig, by = "year") %>%
  arrange(year)
df <- df %>%
  mutate(
    post2015 = ifelse(year >= 2015, 1, 0)
  )
df <- df %>%
  mutate(
    ChinaAgree_l1 = lag(ChinaAgree),
    fav_china_l1 = lag(fav_china)
  )

m_china_break <- lm(
  ChinaAgree ~ ChinaAgree_l1 +
    fav_china_l1 +
    fav_china_l1:post2015,
  data = df
)

coeftest(m_china_break, vcov = NeweyWest(m_china_break))

