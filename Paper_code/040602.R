install.packages(c("tidyverse", "fixest", "zoo", "car", "stargazer", "ggplot2", "marginaleffects"))
library(tidyverse)
library(fixest)
library(zoo)
library(car)
library(stargazer)
library(ggplot2)
library(marginaleffects)

# -----------------------------------------------------------------------------
# STEP 1: Load data
# -----------------------------------------------------------------------------

df <- read_csv("~/Desktop/SPRING2026/MA_paper/0107.csv")

df <- df %>% arrange(Countryname, year)

# -----------------------------------------------------------------------------
# STEP 2: Compute alignment gap at the country-year level
# -----------------------------------------------------------------------------
# Theoretical motivation: countries with a small |USAgree - ChinaAgree| are
# evenly contested between the two donors — these are the swing states where
# strategic aid is most likely to move votes (H1b).
# Countries with a large average absolute gap are stably aligned with one donor.

df <- df %>%
  group_by(Countryname) %>%
  mutate(
    align_diff     = USAgree - ChinaAgree,
    align_diff_lag = lag(align_diff, 1),
    abs_align_gap  = abs(align_diff)         # country-year absolute gap
  ) %>%
  ungroup()

# -----------------------------------------------------------------------------
# STEP 3: Compute country-level mean absolute alignment gap
# -----------------------------------------------------------------------------

country_pivot <- df %>%
  group_by(Countryname) %>%
  summarise(
    mean_abs_gap   = mean(abs_align_gap, na.rm = TRUE),   # key measure
    sd_abs_gap     = sd(abs_align_gap,   na.rm = TRUE),
    n_obs          = sum(!is.na(abs_align_gap)),
    mean_align_diff = mean(align_diff,   na.rm = TRUE)    # context: direction
  ) %>%
  filter(n_obs > 0) %>%
  arrange(mean_abs_gap)

# -----------------------------------------------------------------------------
# STEP 4: Assign terciles based on mean absolute alignment gap
# -----------------------------------------------------------------------------
# T1 (bottom): smallest gap  → most evenly contested ("swing states")
# T2 (middle): moderate gap  → moderately contested
# T3 (top):    largest gap   → most stably aligned with one donor

country_pivot <- country_pivot %>%
  mutate(
    tercile = ntile(mean_abs_gap, 3),
    tercile_label = case_when(
      tercile == 1 ~ "T1: Most Contested",
      tercile == 2 ~ "T2: Moderately Contested",
      tercile == 3 ~ "T3: Stably Aligned"
    ),
    # Binary indicator: T3 = stably aligned; T1+T2 = contested
    T3_group = as.integer(tercile == 3)
  )

# -----------------------------------------------------------------------------
# STEP 5: Print country lists by tercile
# -----------------------------------------------------------------------------

cat("=============================================================\n")
cat("T1: MOST CONTESTED (smallest mean |USAgree - ChinaAgree|)\n")
cat("=============================================================\n")
country_pivot %>%
  filter(tercile == 1) %>%
  select(Countryname, mean_abs_gap, mean_align_diff) %>%
  arrange(mean_abs_gap) %>%
  print(n = Inf)

cat("\n=============================================================\n")
cat("T2: MODERATELY CONTESTED (middle tercile)\n")
cat("=============================================================\n")
country_pivot %>%
  filter(tercile == 2) %>%
  select(Countryname, mean_abs_gap, mean_align_diff) %>%
  arrange(mean_abs_gap) %>%
  print(n = Inf)

cat("\n=============================================================\n")
cat("T3: STABLY ALIGNED (largest mean |USAgree - ChinaAgree|)\n")
cat("=============================================================\n")
country_pivot %>%
  filter(tercile == 3) %>%
  select(Countryname, mean_abs_gap, mean_align_diff) %>%
  arrange(desc(mean_abs_gap)) %>%
  print(n = Inf)

# -----------------------------------------------------------------------------
# STEP 6: Summary statistics by tercile
# -----------------------------------------------------------------------------

cat("\n=============================================================\n")
cat("Summary statistics by tercile\n")
cat("=============================================================\n")

country_pivot %>%
  group_by(tercile_label) %>%
  summarise(
    n_countries     = n(),
    mean_abs_gap    = round(mean(mean_abs_gap), 4),
    min_abs_gap     = round(min(mean_abs_gap),  4),
    max_abs_gap     = round(max(mean_abs_gap),  4),
    mean_align_diff = round(mean(mean_align_diff), 3)
  ) %>%
  print()

# -----------------------------------------------------------------------------
# STEP 7: Merge tercile classification back into main data
# -----------------------------------------------------------------------------

df <- df %>%
  left_join(
    country_pivot %>%
      select(Countryname, tercile, tercile_label, T3_group, mean_abs_gap),
    by = "Countryname"
  )

# Quick check: country-year obs per tercile
df %>%
  count(tercile_label) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  print()

# Verify T3_group coding
df %>%
  count(tercile_label, T3_group) %>%
  print()

# -----------------------------------------------------------------------------
# STEP 8: Export classification table for paper appendix
# -----------------------------------------------------------------------------

tercile_table <- country_pivot %>%
  select(Countryname, tercile_label, T3_group, mean_abs_gap, mean_align_diff) %>%
  rename(
    Country                  = Countryname,
    `Tercile`                = tercile_label,
    `T3 Group (=1 if T3)`    = T3_group,
    `Mean |Align. Gap|`      = mean_abs_gap,
    `Mean Align. Diff.`      = mean_align_diff
  ) %>%
  arrange(tercile, `Mean |Align. Gap|`)

write_csv(tercile_table, "tercile_classification.csv")

# Optional: LaTeX table
library(xtable)
print(
  xtable(
    tercile_table,
    caption = "Country Classification by Mean Absolute Alignment Gap",
    label   = "tab:tercile_countries",
    digits  = 3
  ),
  include.rownames  = FALSE,
  booktabs          = TRUE,
  caption.placement = "top"
)

