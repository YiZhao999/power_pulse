# =============================================================================
# Proposition 1 v2: Strategic Interaction in Aid
# Revisions:
#   (1) Rebuild pivotality using vote volatility (rolling SD)
#   (2) Tercile split instead of median
#   (3) Collinearity diagnostics on within-transformed data
#   (4) Demeaned interactions to fix VCOV warning
# =============================================================================
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
    pivot_roll_sd_lag = lag(pivot_roll_sd, 1),
    
    # --- Tercile split of pivot_roll_sd ---
    # Middle tercile = truly competitive; top = volatile/noisy; bottom = stable
    pivot_tercile   = ntile(pivot_roll_sd_lag, 3),
    # Pivotal = middle tercile (near-indifferent, not just noisy)
    pivot_mid       = as.integer(pivot_tercile == 2),
    # Alternative: top tercile (most volatile)
    pivot_top       = as.integer(pivot_tercile == 3),
    
    # --- Lagged aid variables ---
    CHN_comm_lag    = lag(CHN_comm, 1),
    USA_comm_lag    = lag(USA_comm, 1),
    log_USA_comm    = log(USA_comm + 1),
    log_CHN_lag     = log(CHN_comm_lag + 1),
    log_USA_lag     = log(USA_comm_lag + 1)
  ) %>%
  ungroup()

# --- Sanity check: distribution of pivotality measures ---
df %>%
  select(pivot_roll_sd_lag, pivot_tercile) %>%
  summary()

df %>%
  count(pivot_tercile) %>%
  mutate(pct = n / sum(n))

# -----------------------------------------------------------------------------
# STEP 3: Collinearity diagnostics on within-transformed data
# -----------------------------------------------------------------------------
# Demean by country and year to replicate what feols absorbs,
# then check correlations and VIF on the within-transformed variables.

df_demean <- df %>%
  filter(!is.na(log_USA_comm), !is.na(log_CHN_lag),
         !is.na(align_diff_lag), !is.na(pivot_roll_sd_lag)) %>%
  group_by(Countryname) %>%
  mutate(across(c(log_USA_comm, log_CHN_lag, align_diff_lag,
                  pivot_roll_sd_lag),
                ~ . - mean(., na.rm = TRUE),
                .names = "dm_{.col}")) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(across(starts_with("dm_"),
                ~ . - mean(., na.rm = TRUE),
                .names = "{.col}_yd")) %>%
  ungroup()

# Correlation matrix of within-transformed regressors
cor_matrix <- df_demean %>%
  select(dm_log_CHN_lag_yd, dm_align_diff_lag_yd,
         dm_pivot_roll_sd_lag_yd) %>%
  cor(use = "complete.obs")

print("Correlation matrix (within-transformed):")
print(round(cor_matrix, 3))

# Construct within-transformed interactions and check VIF
df_demean <- df_demean %>%
  mutate(
    CHN_x_align_dm  = dm_log_CHN_lag_yd * dm_align_diff_lag_yd,
    CHN_x_pivot_dm  = dm_log_CHN_lag_yd * dm_pivot_roll_sd_lag_yd
  )

# VIF via OLS on demeaned data (approximates VIF in FE model)
vif_model_align <- lm(
  dm_log_USA_comm_yd ~ dm_log_CHN_lag_yd + dm_align_diff_lag_yd + CHN_x_align_dm,
  data = df_demean
)
vif_model_pivot <- lm(
  dm_log_USA_comm_yd ~ dm_log_CHN_lag_yd + dm_align_diff_lag_yd + CHN_x_pivot_dm,
  data = df_demean
)

print("VIF — alignment interaction:")
print(vif(vif_model_align))

print("VIF — pivotality interaction:")
print(vif(vif_model_pivot))

# VIF > 10 signals severe collinearity; > 5 is concerning.
# If interaction VIFs are high, mean-center before interacting (Step 4).

# -----------------------------------------------------------------------------
# STEP 4: Mean-center variables before interacting
# -----------------------------------------------------------------------------
# Mean-centering reduces collinearity between interaction terms and main effects
# without changing the substance of the model.

df <- df %>%
  mutate(
    log_CHN_lag_c       = log_CHN_lag - mean(log_CHN_lag, na.rm = TRUE),
    align_diff_lag_c    = align_diff_lag - mean(align_diff_lag, na.rm = TRUE),
    pivot_roll_sd_lag_c = pivot_roll_sd_lag - mean(pivot_roll_sd_lag, na.rm = TRUE),
    
    # Centered interactions
    CHN_x_align_c  = log_CHN_lag_c * align_diff_lag_c,
    CHN_x_pivot_c  = log_CHN_lag_c * pivot_roll_sd_lag_c,
    CHN_x_pivot_md = log_CHN_lag_c * pivot_mid,
    CHN_x_pivot_td = log_CHN_lag_c * pivot_top
  )

# -----------------------------------------------------------------------------
# STEP 5: Main regression models — revised
# -----------------------------------------------------------------------------

# --- Model 1: Baseline ---
m1 <- feols(
  log_USA_comm ~ log_CHN_lag_c + align_diff_lag_c |
    Countryname + year,
  data    = df,
  cluster = ~Countryname
)

# --- Model 2: Interaction with alignment (centered) ---
m2 <- feols(
  log_USA_comm ~ log_CHN_lag_c + align_diff_lag_c + CHN_x_align_c |
    Countryname + year,
  data    = df,
  cluster = ~Countryname
)

# --- Model 3: Interaction with rolling-SD pivotality (centered, continuous) ---
m3 <- feols(
  log_USA_comm ~ log_CHN_lag_c + align_diff_lag_c +
    pivot_roll_sd_lag_c + CHN_x_pivot_c |
    Countryname + year,
  data    = df,
  cluster = ~Countryname
)

# --- Model 4: Interaction with middle-tercile pivotality dummy ---
m4 <- feols(
  log_USA_comm ~ log_CHN_lag_c + align_diff_lag_c +
    pivot_mid + CHN_x_pivot_md |
    Countryname + year,
  data    = df,
  cluster = ~Countryname
)

# --- Model 5: Interaction with top-tercile pivotality dummy ---
m5 <- feols(
  log_USA_comm ~ log_CHN_lag_c + align_diff_lag_c +
    pivot_top + CHN_x_pivot_td |
    Countryname + year,
  data    = df,
  cluster = ~Countryname
)

etable(m1, m2, m3, m4, m5,
       title   = "Proposition 1: US Aid Reaction Function (Revised)",
       headers = c("Baseline", "x Alignment", "x Pivot SD",
                   "x Pivot Mid-3", "x Pivot Top-3"))

# -----------------------------------------------------------------------------
# STEP 6: Split-sample by tercile — revised
# -----------------------------------------------------------------------------

df_t1 <- df %>% filter(pivot_tercile == 1)   # stable, firmly aligned
df_t2 <- df %>% filter(pivot_tercile == 2)   # middle — theoretically pivotal
df_t3 <- df %>% filter(pivot_tercile == 3)   # most volatile

m6 <- feols(log_USA_comm ~ log_CHN_lag_c + align_diff_lag_c |
              Countryname + year, data = df_t1, cluster = ~Countryname)
m7 <- feols(log_USA_comm ~ log_CHN_lag_c + align_diff_lag_c |
              Countryname + year, data = df_t2, cluster = ~Countryname)
m8 <- feols(log_USA_comm ~ log_CHN_lag_c + align_diff_lag_c |
              Countryname + year, data = df_t3, cluster = ~Countryname)

etable(m6, m7, m8,
       title   = "Proposition 1: Split Sample by Pivotality Tercile",
       headers = c("Stable (T1)", "Pivotal (T2)", "Volatile (T3)"))

# Theoretical prediction:
# log_CHN_lag_c positive and significant in T2 (pivotal),
# close to zero in T1 (stable), potentially negative in T3 (volatile/noisy)

# -----------------------------------------------------------------------------
# STEP 7: Symmetric check — China reaction function (revised)
# -----------------------------------------------------------------------------

df <- df %>%
  group_by(Countryname) %>%
  mutate(
    log_USA_lag_c = log(lag(USA_comm, 1) + 1) -
      mean(log(lag(USA_comm, 1) + 1), na.rm = TRUE)
  ) %>%
  ungroup()

m9 <- feols(
  log(CHN_comm + 1) ~ log_USA_lag_c + align_diff_lag_c |
    Countryname + year,
  data    = df,
  cluster = ~Countryname
)

etable(m9, title = "Proposition 1: Symmetric Check — China Reaction Function")

# -----------------------------------------------------------------------------
# STEP 8: Visualization — marginal effect across pivotality spectrum
# -----------------------------------------------------------------------------

# Plot marginal effect of log_CHN_lag across range of pivot_roll_sd_lag
# using continuous interaction model (m3)

me <- slopes(
  m3,
  variables = "log_CHN_lag_c",
  newdata   = datagrid(
    pivot_roll_sd_lag_c = seq(
      min(df$pivot_roll_sd_lag_c, na.rm = TRUE),
      max(df$pivot_roll_sd_lag_c, na.rm = TRUE),
      length.out = 50
    )
  )
)

# Add back the original scale for x-axis label
mean_sd <- mean(df$pivot_roll_sd_lag, na.rm = TRUE)
me <- me %>%
  mutate(pivot_original = pivot_roll_sd_lag_c + mean_sd)

ggplot(me, aes(x = pivot_original, y = estimate,
               ymin = conf.low, ymax = conf.high)) +
  geom_ribbon(alpha = 0.15, fill = "steelblue") +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  # Mark tercile boundaries
  geom_vline(
    xintercept = quantile(df$pivot_roll_sd_lag, probs = c(1/3, 2/3),
                          na.rm = TRUE),
    linetype = "dotted", color = "gray40"
  ) +
  annotate("text", x = quantile(df$pivot_roll_sd_lag, 1/6, na.rm=TRUE),
           y = Inf, label = "Stable\n(T1)", vjust = 1.5, size = 3.5) +
  annotate("text", x = quantile(df$pivot_roll_sd_lag, 1/2, na.rm=TRUE),
           y = Inf, label = "Pivotal\n(T2)", vjust = 1.5, size = 3.5) +
  annotate("text", x = quantile(df$pivot_roll_sd_lag, 5/6, na.rm=TRUE),
           y = Inf, label = "Volatile\n(T3)", vjust = 1.5, size = 3.5) +
  labs(
    title    = "Marginal Effect of Chinese Aid on US Aid",
    subtitle = "By alignment volatility (pivotality proxy)",
    x        = "Rolling SD of alignment differential (pivotality)",
    y        = "Marginal effect of log(Chinese aid) on log(US aid)",
    caption  = "Dotted lines = tercile boundaries. Shaded area = 95% CI."
  ) +
  theme_minimal(base_size = 13)

ggsave("prop1_marginal_effects_v2.pdf", width = 8, height = 5)

# -----------------------------------------------------------------------------
# STEP 9: Summary of diagnostics to check after running
# -----------------------------------------------------------------------------

# Things to look for:
# 1. Correlation matrix: no pair > |0.7| after within-transformation
# 2. VIF: all < 5 after mean-centering
# 3. VCOV warning: should disappear after mean-centering
# 4. Split sample (m6/m7/m8): log_CHN_lag_c significant in T2, not T1
# 5. Marginal effect plot: positive slope in pivotal region, flat/negative elsewhere
# 6. Symmetric check (m9): China also responds positively to US aid