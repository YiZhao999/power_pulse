##################################################
# 1. Libraries
##################################################

library(tidyverse)
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
# 5. Filter Nigeria Alignment Only
##################################################

align_nig <- align %>%
  filter(Countryname == "Nigeria") %>%
  select(year, USAgree, ChinaAgree)

##################################################
# 6. Merge
##################################################

df <- fav_year %>%
  inner_join(align_nig, by = "year") %>%
  arrange(year)

##################################################
# 7. Structural Break Dummy
##################################################

df <- df %>%
  mutate(post2015 = ifelse(year >= 2015, 1, 0))

##################################################
# 8. Rename Alignment Variables
##################################################

df <- df %>%
  rename(
    Agree_us = USAgree,
    Agree_china = ChinaAgree
  )

##################################################
# 9. Convert to Long (Stack US + China)
##################################################

df_long <- df %>%
  pivot_longer(
    cols = c(fav_us, fav_china, Agree_us, Agree_china),
    names_to = c(".value", "target"),
    names_pattern = "(fav|Agree)_(us|china)"
  ) %>%
  mutate(
    target = recode(target,
                    us = "US",
                    china = "China")
  ) %>%
  arrange(target, year)

##################################################
# 10. Same-Year Combined Structural Break Model
##################################################

m_same_year <- lm(
  Agree ~ 0 +
    target +                     # target-specific intercept
    target:fav +                 # pre-2015 slope
    target:fav:post2015 +        # slope change after 2015
    target:post2015,             # level shift after 2015
  data = df_long
)

cat("\n=== Same-Year Structural Break Model ===\n")
print(coeftest(m_same_year, vcov = NeweyWest(m_same_year)))
# Prediction grid
grid <- df_long %>%
  group_by(target) %>%
  summarise(
    fav_min = min(fav, na.rm = TRUE),
    fav_max = max(fav, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  expand_grid(post2015 = c(0, 1)) %>%
  rowwise() %>%
  mutate(
    fav = list(seq(fav_min, fav_max, length.out = 30))
  ) %>%
  unnest(fav) %>%
  ungroup()

grid$pred <- predict(m_same_year, newdata = grid)

ggplot(df_long, aes(x = fav, y = Agree)) +
  geom_point(aes(shape = factor(post2015)), size = 3, alpha = 0.8) +
  geom_line(data = grid,
            aes(y = pred, linetype = factor(post2015)),
            linewidth = 1.2) +
  facet_wrap(~ target, scales = "free_x") +
  labs(
    title = "Nigeria: Same-Year Favorability and Alignment",
    x = "Citizen Favorability (Same Year)",
    y = "UNGA Voting Similarity",
    linetype = "Post 2015",
    shape = "Post 2015"
  ) +
  theme_minimal()

