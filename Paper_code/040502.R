# ============================================================
# Testing H2: Conditional Hearts-and-Minds (Proposition 2)
# 
# H2: Foreign aid has a more positive effect on public favorability
# toward donor d in settings where the corrupt type is less likely
# to divert aid and where aid is more likely to generate visible
# project success.
# ============================================================

library(tidyverse)
library(fixest)      # fast panel FE estimation (feols)
library(modelsummary)
library(marginaleffects)

# ---- 0. Load data --------------------------------------------------

df <- read_csv("~/Desktop/SPRING2026/MA_paper/code/final_merged.csv")

# ---- 1. Reshape to long (donor-year-country) -----------------------
# Each row becomes one donor observation so we can stack US and China

df_long <- df %>%
  pivot_longer(
    cols      = c(us_prop_fav, china_prop_fav),
    names_to  = "donor",
    values_to = "favorability"
  ) %>%
  mutate(
    donor = recode(donor,
                   "us_prop_fav"    = "US",
                   "china_prop_fav" = "China"),
    aid = case_when(
      donor == "US"    ~ aid_us,
      donor == "China" ~ aid_china
    ),
    vote_share = case_when(
      donor == "US"    ~ vote_us,
      donor == "China" ~ vote_china
    )
  ) %>%
  select(year, country, donor,
         favorability, aid, vote_share,
         simple_diversion_index, latent_diversion_risk)

# ---- 2. Aid transformations ----------------------------------------

df_long <- df_long %>%
  mutate(
    # log aid (common in foreign aid literature; zeros stay zero via IHS)
    aid_ihs      = asinh(aid),              # inverse hyperbolic sine handles zeros
    aid_positive = if_else(aid > 0, aid, NA_real_),
    aid_ihs_pos  = asinh(aid_positive),     # for intensity-margin subsample
    
    # Presence dummy: does any aid flow?
    aid_any      = as.integer(aid > 0 & !is.na(aid)),
    
    # Interaction terms (center moderators for interpretability)
    sdi_c        = simple_diversion_index - mean(simple_diversion_index, na.rm = TRUE),
    ldr_c        = latent_diversion_risk  - mean(latent_diversion_risk,  na.rm = TRUE),
    
    # Donor dummy
    donor_china  = as.integer(donor == "China")
  )

# ---- 3. Panel ID ---------------------------------------------------
# Unit = country x donor; time = year

df_long <- df_long %>%
  mutate(unit_id = paste(country, donor, sep = "_"))

# ============================================================
# STAGE 1 — PRESENCE MARGIN
# Does having any aid vs. no aid raise favorability,
# and does this depend on diversion risk?
# ============================================================

cat("\n========== STAGE 1: PRESENCE MARGIN ==========\n")

# --- 3a. Using simple_diversion_index ---

m1a <- feols(
  favorability ~ aid_any * sdi_c | unit_id + year,
  data    = df_long,
  cluster = ~country
)

# --- 3b. Using latent_diversion_risk ---

m1b <- feols(
  favorability ~ aid_any * ldr_c | unit_id + year,
  data    = df_long,
  cluster = ~country
)

# --- 3c. Both moderators jointly ---

m1c <- feols(
  favorability ~ aid_any * sdi_c + aid_any * ldr_c | unit_id + year,
  data    = df_long,
  cluster = ~country
)

# Print results
print(summary(m1a))
print(summary(m1b))
print(summary(m1c))

# ============================================================
# STAGE 2 — INTENSITY MARGIN
# Among country-donor-years with positive aid flows,
# does more aid raise favorability, conditional on diversion risk?
# ============================================================

cat("\n========== STAGE 2: INTENSITY MARGIN (positive aid only) ==========\n")

df_pos <- df_long %>% filter(aid > 0 & !is.na(aid))

m2a <- feols(
  favorability ~ aid_ihs_pos * sdi_c | unit_id + year,
  data    = df_pos,
  cluster = ~country
)

m2b <- feols(
  favorability ~ aid_ihs_pos * ldr_c | unit_id + year,
  data    = df_pos,
  cluster = ~country
)

m2c <- feols(
  favorability ~ aid_ihs_pos * sdi_c + aid_ihs_pos * ldr_c | unit_id + year,
  data    = df_pos,
  cluster = ~country
)

print(summary(m2a))
print(summary(m2b))
print(summary(m2c))

# ============================================================
# STAGE 3 — COMBINED MODEL (IHS aid, full sample)
# ============================================================

cat("\n========== STAGE 3: COMBINED MODEL (full sample, IHS aid) ==========\n")

m3a <- feols(
  favorability ~ aid_ihs * sdi_c | unit_id + year,
  data    = df_long,
  cluster = ~country
)

m3b <- feols(
  favorability ~ aid_ihs * ldr_c | unit_id + year,
  data    = df_long,
  cluster = ~country
)

m3c <- feols(
  favorability ~ aid_ihs * sdi_c + aid_ihs * ldr_c | unit_id + year,
  data    = df_long,
  cluster = ~country
)

print(summary(m3a))
print(summary(m3b))
print(summary(m3c))

# ============================================================
# STAGE 4 — DONOR-SPECIFIC MODELS
# The theory says the hearts-and-minds channel is donor-specific.
# Estimate separately for US aid → US favorability
# and China aid → China favorability.
# ============================================================

cat("\n========== STAGE 4: DONOR-SPECIFIC MODELS ==========\n")

df_us    <- df_long %>% filter(donor == "US")
df_china <- df_long %>% filter(donor == "China")

# --- US ---
m_us_sdi <- feols(
  favorability ~ aid_ihs * sdi_c | country + year,
  data    = df_us,
  cluster = ~country
)

m_us_ldr <- feols(
  favorability ~ aid_ihs * ldr_c | country + year,
  data    = df_us,
  cluster = ~country
)

# --- China ---
m_cn_sdi <- feols(
  favorability ~ aid_ihs * sdi_c | country + year,
  data    = df_china,
  cluster = ~country
)

m_cn_ldr <- feols(
  favorability ~ aid_ihs * ldr_c | country + year,
  data    = df_china,
  cluster = ~country
)

print(summary(m_us_sdi))
print(summary(m_us_ldr))
print(summary(m_cn_sdi))
print(summary(m_cn_ldr))

# ============================================================
# OUTPUT TABLES  (modelsummary)
# ============================================================

cat("\n========== SUMMARY TABLES ==========\n")

# Stage 1 table
modelsummary(
  list(
    "Presence × SDI"  = m1a,
    "Presence × LDR"  = m1b,
    "Presence × Both" = m1c
  ),
  stars      = c("*" = .10, "**" = .05, "***" = .01),
  title      = "Stage 1 — Presence Margin",
  coef_rename = c(
    "aid_any"        = "Aid presence (0/1)",
    "sdi_c"          = "Diversion index (SDI)",
    "ldr_c"          = "Latent diversion risk (LDR)",
    "aid_any:sdi_c"  = "Aid presence × SDI",
    "aid_any:ldr_c"  = "Aid presence × LDR"
  ),
  output     = "stage1_presence_margin.txt"
)

# Stage 2 table
modelsummary(
  list(
    "Intensity × SDI"  = m2a,
    "Intensity × LDR"  = m2b,
    "Intensity × Both" = m2c
  ),
  stars      = c("*" = .10, "**" = .05, "***" = .01),
  title      = "Stage 2 — Intensity Margin (positive-aid subsample)",
  coef_rename = c(
    "aid_ihs_pos"          = "Aid (IHS)",
    "sdi_c"                = "Diversion index (SDI)",
    "ldr_c"                = "Latent diversion risk (LDR)",
    "aid_ihs_pos:sdi_c"    = "Aid (IHS) × SDI",
    "aid_ihs_pos:ldr_c"    = "Aid (IHS) × LDR"
  ),
  output     = "stage2_intensity_margin.txt"
)

# Stage 3 table
modelsummary(
  list(
    "Combined × SDI"  = m3a,
    "Combined × LDR"  = m3b,
    "Combined × Both" = m3c
  ),
  stars      = c("*" = .10, "**" = .05, "***" = .01),
  title      = "Stage 3 — Combined Model (full sample, IHS aid)",
  coef_rename = c(
    "aid_ihs"        = "Aid (IHS)",
    "sdi_c"          = "Diversion index (SDI)",
    "ldr_c"          = "Latent diversion risk (LDR)",
    "aid_ihs:sdi_c"  = "Aid (IHS) × SDI",
    "aid_ihs:ldr_c"  = "Aid (IHS) × LDR"
  ),
  output     = "stage3_combined.txt"
)

# Donor-specific table
modelsummary(
  list(
    "US × SDI"    = m_us_sdi,
    "US × LDR"    = m_us_ldr,
    "China × SDI" = m_cn_sdi,
    "China × LDR" = m_cn_ldr
  ),
  stars      = c("*" = .10, "**" = .05, "***" = .01),
  title      = "Stage 4 — Donor-Specific Models",
  coef_rename = c(
    "aid_ihs"        = "Aid (IHS)",
    "sdi_c"          = "Diversion index (SDI)",
    "ldr_c"          = "Latent diversion risk (LDR)",
    "aid_ihs:sdi_c"  = "Aid (IHS) × SDI",
    "aid_ihs:ldr_c"  = "Aid (IHS) × LDR"
  ),
  output     = "stage4_donor_specific.txt"
)

# ============================================================
# MARGINAL EFFECTS PLOTS
# Conditional effect of aid at low / medium / high diversion risk
# ============================================================

cat("\n========== MARGINAL EFFECTS ==========\n")

# Conditional marginal effect of aid (IHS) across SDI values — combined model
me_sdi <- slopes(
  m3a,
  variables  = "aid_ihs",
  newdata    = datagrid(sdi_c = quantile(df_long$sdi_c, probs = c(.10, .25, .50, .75, .90), na.rm = TRUE))
)
print(me_sdi)

# Conditional marginal effect of aid (IHS) across LDR values
me_ldr <- slopes(
  m3b,
  variables  = "aid_ihs",
  newdata    = datagrid(ldr_c = quantile(df_long$ldr_c, probs = c(.10, .25, .50, .75, .90), na.rm = TRUE))
)
print(me_ldr)

# Plot: conditional effect over SDI range
plot_sdi <- plot_slopes(
  m3a,
  variables = "aid_ihs",
  condition = "sdi_c"
) +
  labs(
    title    = "Conditional Effect of Aid on Favorability\nby Diversion Risk (SDI)",
    subtitle = "Higher SDI = lower diversion risk. H2 predicts slope positive for low-SDI settings.",
    x        = "Simple Diversion Index (centered)",
    y        = "Marginal effect of aid (IHS) on favorability"
  ) +
  theme_minimal()

ggsave("plot_conditional_effect_SDI.png", plot_sdi, width = 8, height = 5, dpi = 150)

# Plot: conditional effect over LDR range
plot_ldr <- plot_slopes(
  m3b,
  variables = "aid_ihs",
  condition = "ldr_c"
) +
  labs(
    title    = "Conditional Effect of Aid on Favorability\nby Latent Diversion Risk (LDR)",
    subtitle = "Lower LDR = lower diversion risk. H2 predicts slope positive for low-LDR settings.",
    x        = "Latent Diversion Risk (centered)",
    y        = "Marginal effect of aid (IHS) on favorability"
  ) +
  theme_minimal()

ggsave("plot_conditional_effect_LDR.png", plot_ldr, width = 8, height = 5, dpi = 150)

cat("\nDone. Output files written:\n")
cat("  stage1_presence_margin.txt\n")
cat("  stage2_intensity_margin.txt\n")
cat("  stage3_combined.txt\n")
cat("  stage4_donor_specific.txt\n")
cat("  plot_conditional_effect_SDI.png\n")
cat("  plot_conditional_effect_LDR.png\n")
