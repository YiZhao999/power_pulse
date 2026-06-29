install.packages(c("tidyverse", "fixest", "zoo", "car", "stargazer", "ggplot2", "marginaleffects"))
library(tidyverse)
library(fixest)
library(zoo)        # for rollapply (rolling SD)
library(car)        # for vif
library(stargazer)
library(ggplot2)
library(marginaleffects)

# -----------------------------------------------------------------------------
# STEP 1: Load data
# -----------------------------------------------------------------------------

df <- read_csv("~/Desktop/SPRING2026/MA_paper/0107.csv")  

df <- df %>% arrange(Countryname, year)

# -----------------------------------------------------------------------------
# STEP 2: Rebuild pivotality measure — vote volatility
# -----------------------------------------------------------------------------
# Theoretical motivation: g(xi*) is large when the government is near the
# alignment threshold. A country that switches alignment frequently is
# empirically near-indifferent — it is the one where aid can flip the vote.
# Rolling SD of (USAgree - ChinaAgree) over a 5-year window captures this.

df <- df %>%
  group_by(Countryname) %>%
  mutate(
    align_diff      = USAgree - ChinaAgree,
    align_diff_lag  = lag(align_diff, 1),
    
    # --- Pivotality v2: rolling SD of alignment differential ---
    # Large rolling SD = frequent switcher = near indifference threshold
    pivot_roll_sd   = rollapply(
      align_diff,
      width   = 5,
      FUN     = sd,
      fill    = NA,
      align   = "right",
      partial = TRUE      # use available obs at start of series
    ),
    pivot_roll_sd_lag = lag(pivot_roll_sd, 1)
  )


country_pivot <- df %>%
  group_by(Countryname) %>%
  summarise(
    mean_pivot_sd  = mean(pivot_roll_sd_lag, na.rm = TRUE),
    sd_pivot_sd    = sd(pivot_roll_sd_lag,   na.rm = TRUE),
    n_obs          = sum(!is.na(pivot_roll_sd_lag)),
    # Also store mean alignment differential for context
    mean_align_diff = mean(align_diff, na.rm = TRUE)
  ) %>%
  filter(n_obs > 0) %>%
  arrange(mean_pivot_sd)

# -----------------------------------------------------------------------------
# STEP 2: Assign country-level terciles based on mean rolling SD
# -----------------------------------------------------------------------------

country_pivot <- country_pivot %>%
  mutate(
    tercile = ntile(mean_pivot_sd, 3),
    tercile_label = case_when(
      tercile == 1 ~ "T1: Stable",
      tercile == 2 ~ "T2: Pivotal",
      tercile == 3 ~ "T3: Volatile"
    )
  )

# -----------------------------------------------------------------------------
# STEP 3: Print country lists by tercile
# -----------------------------------------------------------------------------

cat("=============================================================\n")
cat("T1: STABLE (firmly aligned, low alignment volatility)\n")
cat("=============================================================\n")
country_pivot %>%
  filter(tercile == 1) %>%
  select(Countryname, mean_pivot_sd, mean_align_diff) %>%
  arrange(mean_pivot_sd) %>%
  print(n = Inf)

cat("\n=============================================================\n")
cat("T2: PIVOTAL (moderately competitive, middle tercile)\n")
cat("=============================================================\n")
country_pivot %>%
  filter(tercile == 2) %>%
  select(Countryname, mean_pivot_sd, mean_align_diff) %>%
  arrange(mean_pivot_sd) %>%
  print(n = Inf)

cat("\n=============================================================\n")
cat("T3: VOLATILE (most competitive, high alignment volatility)\n")
cat("=============================================================\n")
country_pivot %>%
  filter(tercile == 3) %>%
  select(Countryname, mean_pivot_sd, mean_align_diff) %>%
  arrange(desc(mean_pivot_sd)) %>%
  print(n = Inf)

# -----------------------------------------------------------------------------
# STEP 4: Summary statistics by tercile
# -----------------------------------------------------------------------------

cat("\n=============================================================\n")
cat("Summary statistics by tercile\n")
cat("=============================================================\n")

country_pivot %>%
  group_by(tercile_label) %>%
  summarise(
    n_countries    = n(),
    mean_pivot_sd  = round(mean(mean_pivot_sd), 4),
    min_pivot_sd   = round(min(mean_pivot_sd),  4),
    max_pivot_sd   = round(max(mean_pivot_sd),  4),
    mean_align_diff = round(mean(mean_align_diff), 3)
  ) %>%
  print()

# -----------------------------------------------------------------------------
# STEP 5: Export a clean table for the paper appendix
# -----------------------------------------------------------------------------

tercile_table <- country_pivot %>%
  select(Countryname, tercile_label, mean_pivot_sd, mean_align_diff) %>%
  rename(
    Country              = Countryname,
    `Tercile`            = tercile_label,
    `Mean Rolling SD`    = mean_pivot_sd,
    `Mean Align. Diff.`  = mean_align_diff
  ) %>%
  arrange(tercile, `Mean Rolling SD`)

# Save as CSV for the appendix
write_csv(tercile_table, "tercile_classification.csv")

# Optional: LaTeX table output
library(xtable)
print(
  xtable(
    tercile_table,
    caption = "Country Classification by Alignment Volatility Tercile",
    label   = "tab:tercile_countries",
    digits  = 3
  ),
  include.rownames  = FALSE,
  booktabs          = TRUE,
  caption.placement = "top"
)

# -----------------------------------------------------------------------------
# STEP 6: Sanity check — how many country-years fall in each tercile
# -----------------------------------------------------------------------------

df %>%
  left_join(
    country_pivot %>% select(Countryname, tercile, tercile_label),
    by = "Countryname"
  ) %>%
  count(tercile_label) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  print()
