# =============================================================================
# Proposition 1: Strategic Interaction in Aid
# Model: Aid^US_it = a_i + l_t + b1*Aid^China_i,t-1 + b2*Alignment_i,t-1
#                  + b3*(Aid^China_i,t-1 x Alignment_i,t-1) + X*gamma + e_it
# =============================================================================
install.packages(c("tidyverse", "plm", "lmtest", "sandwich", "stargazer", "fixest",
                   "ggplot2", "marginaleffects"))
library(tidyverse)
library(plm)        
library(lmtest)    
library(sandwich)   
library(stargazer)  
library(fixest)     

# -----------------------------------------------------------------------------
# STEP 1: Load and inspect data
# -----------------------------------------------------------------------------

df <- read_csv("~/Desktop/SPRING2026/MA_paper/0107.csv")  

glimpse(df)
df %>%
  select(year, CHN_comm, USA_comm, ChinaAgree, USAgree) %>%
  summary()

# -----------------------------------------------------------------------------
# STEP 2: Construct variables
# -----------------------------------------------------------------------------

df <- df %>%
  arrange(Countryname, year) %>%
  group_by(Countryname) %>%
  mutate(
    # --- Lagged aid (right-hand side variables) ---
    CHN_comm_lag   = lag(CHN_comm, 1),
    USA_comm_lag   = lag(USA_comm, 1),
    
    # --- Baseline alignment proxy for Delta_kappa0 ---
    # USAgree proxies alignment with the US donor (donor A in the model)
    # ChinaAgree proxies alignment with China (donor B)
    # The net alignment differential maps to xi* in the model
    align_diff_lag = lag(USAgree - ChinaAgree, 1),
    
    # --- Pivotality proxy g(xi*): countries near indifference ---
    # High pivotality = alignment differential close to zero
    # We use the absolute value of the lagged differential:
    # small |align_diff| => near-pivotal; large => firmly aligned
    pivotality     = 1 - abs(lag(USAgree - ChinaAgree, 1)),
    # Alternatively: competitive_regime dummy (|align_diff| < median)
    pivotality_d   = as.integer(
      abs(lag(USAgree - ChinaAgree, 1)) <
        median(abs(USAgree - ChinaAgree), na.rm = TRUE)
    ),
    
    # --- Aid indicator dummies (for binary treatment robustness check) ---
    CHN_any_lag    = as.integer(CHN_comm_lag > 0),
    USA_any        = as.integer(USA_comm > 0),
    
    # --- Log transformations (common in aid literature) ---
    log_USA_comm   = log(USA_comm + 1),
    log_CHN_lag    = log(CHN_comm_lag + 1),
    
    # --- Interaction: rival aid x baseline alignment (tests heterogeneity) ---
    CHN_x_align    = CHN_comm_lag * align_diff_lag,
    
    # --- Interaction: rival aid x pivotality (tests Prop 1 heterogeneity) ---
    CHN_x_pivot    = CHN_comm_lag * pivotality,
    CHN_x_pivot_d  = CHN_comm_lag * pivotality_d
  ) %>%
  ungroup()

# -----------------------------------------------------------------------------
# STEP 3: Baseline reaction function — does US aid respond to Chinese aid?
# -----------------------------------------------------------------------------
# Equation: Aid^US_it = a_i + l_t + b1*Aid^China_i,t-1
#                      + b2*Alignment_i,t-1 + b3*(Aid^China x Alignment) + e
#
# b1 > 0 => strategic complements
# b1 < 0 => strategic substitutes
# b3 tests heterogeneity: does the reaction depend on baseline alignment?

# --- Model 1: Baseline, no interaction ---
m1 <- feols(
  log_USA_comm ~ log_CHN_lag + align_diff_lag |
    Countryname + year,
  data    = df,
  cluster = ~Countryname   # cluster SE at country level
)

# --- Model 2: Add interaction with baseline alignment ---
# b3 < 0: complementarity weakens (or reverses) in pro-US countries
# b3 > 0: complementarity strengthens in pro-US countries
m2 <- feols(
  log_USA_comm ~ log_CHN_lag + align_diff_lag + CHN_x_align |
    Countryname + year,
  data    = df,
  cluster = ~Countryname
)

# --- Model 3: Continuous pivotality interaction ---
m3 <- feols(
  log_USA_comm ~ log_CHN_lag + align_diff_lag + CHN_x_pivot |
    Countryname + year,
  data    = df,
  cluster = ~Countryname
)

# --- Model 4: Binary pivotality interaction ---
m4 <- feols(
  log_USA_comm ~ log_CHN_lag + align_diff_lag + CHN_x_pivot_d |
    Countryname + year,
  data    = df,
  cluster = ~Countryname
)

# Print results
etable(m1, m2, m3, m4,
       title   = "Proposition 1: US Aid Reaction Function",
       headers = c("Baseline", "+ Alignment Het.",
                   "+ Pivot (cont.)", "+ Pivot (binary)"))

# -----------------------------------------------------------------------------
# STEP 4: Heterogeneity by pivotality — split-sample check
# -----------------------------------------------------------------------------
# Theory: complementarity strongest in pivotal (competitive) countries
# Split sample by pivotality dummy and re-run baseline reaction function

df_pivot     <- df %>% filter(pivotality_d == 1)   # near-indifferent countries
df_non_pivot <- df %>% filter(pivotality_d == 0)   # firmly aligned countries

m5 <- feols(
  log_USA_comm ~ log_CHN_lag + align_diff_lag |
    Countryname + year,
  data    = df_pivot,
  cluster = ~Countryname
)

m6 <- feols(
  log_USA_comm ~ log_CHN_lag + align_diff_lag |
    Countryname + year,
  data    = df_non_pivot,
  cluster = ~Countryname
)

etable(m5, m6,
       title   = "Proposition 1: Heterogeneity by Pivotality (Split Sample)",
       headers = c("Pivotal countries", "Non-pivotal countries"))

# Prediction: b1 (CHN_comm_lag) positive and significant in pivotal sample,
#             close to zero or negative in non-pivotal sample.

# -----------------------------------------------------------------------------
# STEP 5: Symmetric check — does Chinese aid also respond to US aid?
# -----------------------------------------------------------------------------
# The model is symmetric: both donors best-respond to each other.
# A symmetric reaction function strengthens the strategic interaction story.

m7 <- feols(
  log(CHN_comm + 1) ~ log(USA_comm_lag + 1) + align_diff_lag |
    Countryname + year,
  data    = df %>%
    group_by(Countryname) %>%
    mutate(USA_comm_lag = lag(USA_comm, 1)) %>%
    ungroup(),
  cluster = ~Countryname
)

etable(m7,
       title = "Proposition 1: Symmetric Check — China Reaction Function")

# -----------------------------------------------------------------------------
# STEP 6: Robustness — binary aid outcome (extensive margin)
# -----------------------------------------------------------------------------
# Replace log(aid+1) with indicator any aid > 0 to check extensive margin

m8 <- feols(
  USA_any ~ CHN_any_lag + align_diff_lag + I(CHN_any_lag * align_diff_lag) |
    Countryname + year,
  data    = df,
  cluster = ~Countryname
)

etable(m8, title = "Proposition 1: Robustness — Extensive Margin (Any Aid)")

# -----------------------------------------------------------------------------
# STEP 7: Visualization — marginal effect of Chinese aid across alignment values
# -----------------------------------------------------------------------------
install.packages("marginaleffects")
library(ggplot2)
library(marginaleffects)  # install if needed: install.packages("marginaleffects")

# Compute marginal effects of CHN_comm_lag across range of align_diff_lag
# using Model 2 (interaction with baseline alignment)

me <- slopes(
  m2,
  variables  = "log_CHN_lag",
  newdata    = datagrid(
    align_diff_lag = seq(-1, 1, by = 0.1),
    model          = m2
  )
)

# Plot: marginal effect of Chinese aid on US aid across alignment spectrum
ggplot(me, aes(x = align_diff_lag, y = estimate,
               ymin = conf.low, ymax = conf.high)) +
  geom_ribbon(alpha = 0.2, fill = "steelblue") +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title    = "Marginal Effect of Chinese Aid on US Aid",
    subtitle = "Across baseline alignment differential (US - China)",
    x        = "Baseline alignment differential (USAgree - ChinaAgree), lagged",
    y        = "Marginal effect of log(CHN aid) on log(US aid)",
    caption  = paste("Negative x-axis = pro-China; Positive = pro-US.",
                     "Shaded area = 95% CI.")
  ) +
  theme_minimal(base_size = 13)

ggsave("prop1_marginal_effects.pdf", width = 8, height = 5)

# Expected pattern from the model:
# - Positive and large effect at low alignment values (Pro-B / pivotal region)
# - Declining and potentially negative at high alignment values (Pro-A / captured)
# This non-monotonic pattern is the empirical signature of Proposition 1.