packages <- c("lme4", "lmerTest", "tidyverse", "sjPlot", "broom.mixed")
installed <- packages %in% rownames(installed.packages())
if (any(!installed)) install.packages(packages[!installed], dependencies = TRUE)

library(lme4)
library(lmerTest)
library(tidyverse)
library(sjPlot)
library(broom.mixed)

data_path <- "~/Desktop/SUMMER2025/office hour/office hour 8/0814/0814_nigeria.csv"
raw <- read.csv(data_path)

dat <- raw %>%
  mutate(
    year            = as.factor(year),
    region          = as.factor(region),
    fav_us_weighted        = fav_us * weight,
    fav_china_weighted     = fav_china * weight,
    econ_weighted          = econ * weight,
    satisfaction_weighted  = satisfaction * weight,
    log_CHN = log1p(CHN_comm),
    log_US  = log1p(USA_comm),
  )

dat <- dat %>%
  group_by(region) %>%
  mutate(satisf_cwc = econ_weighted - mean(econ_weighted, na.rm = TRUE)) %>%
  ungroup()

dat <- dat %>%
  mutate(
    is_apc     = as.numeric(is_apc),
    is_pdp     = as.numeric(is_pdp),
    post_2015  = as.numeric(post_2015)
  )


make_model_df <- function(df, vars) {
  out <- df %>% select(all_of(vars)) %>% drop_na()
  n_total <- nrow(df)
  n_used  <- nrow(out)
  message(sprintf("Kept %d / %d rows (%.1f%%) after NA-drop for vars: %s",
                  n_used, n_total, 100 * n_used / max(1, n_total), paste(vars, collapse = ", ")))
  out
}

## =========================
## Models 1–4 (favor toward CHN BY China/US × satisfaction; by region)
## =========================

## Model 1: China, random intercept
vars_m1 <- c("fav_china_weighted", "satisf_cwc", "log_CHN", "region")
m1_df <- dat %>% mutate(region = droplevels(region)) %>% make_model_df(vars_m1)

model_china_1 <- lmer(
  fav_china_weighted ~ satisf_cwc * log_CHN + (1 | region),
  data = m1_df
)
cat("\n--- Model 1: China (RI) ---\n")
print(summary(model_china_1))
cat("Singular fit? ", isSingular(model_china_1), "\n\n")

## Model 2: China, random intercept + random slope (satisfaction)
vars_m2 <- c("fav_china_weighted", "satisf_cwc", "log_CHN", "region")
m2_df <- dat %>% mutate(region = droplevels(region)) %>% make_model_df(vars_m2)

model_china_2 <- lmer(
  fav_china_weighted ~ satisf_cwc * log_CHN + (1 + satisf_cwc | region),
  data = m2_df
)
cat("\n--- Model 2: China (RI+RS) ---\n")
print(summary(model_china_2))
cat("Singular fit? ", isSingular(model_china_2), "\n\n")

## Model 3: US, random intercept
vars_m3 <- c("fav_china_weighted", "satisf_cwc", "log_US", "region")
m3_df <- dat %>% mutate(region = droplevels(region)) %>% make_model_df(vars_m3)

model_china_3 <- lmer(
  fav_china_weighted ~ satisf_cwc * log_US + (1 | region),
  data = m3_df
)
cat("\n--- Model 3: US (RI) ---\n")
print(summary(model_china_3))
cat("Singular fit? ", isSingular(model_china_3), "\n\n")

## Model 4: US, random intercept + random slope (satisfaction)
vars_m4 <- c("fav_china_weighted", "satisf_cwc", "log_US", "region")
m4_df <- dat %>% mutate(region = droplevels(region)) %>% make_model_df(vars_m4)

model_china_4 <- lmer(
  fav_china_weighted ~ satisf_cwc * log_US + (1 + satisf_cwc | region),
  data = m4_df
)
cat("\n--- Model 4: US (RI+RS) ---\n")
print(summary(model_china_4))
cat("Singular fit? ", isSingular(model_china_4), "\n\n")

## Comparison table for Models 1–4
suppressWarnings(
  tab_model(
    model_china_1, model_china_2, model_china_3, model_china_4,
    show.ci = FALSE,
    show.re.var = TRUE,
    show.icc = TRUE,
    title = "China and US Aid Interaction Effects",
    dv.labels = c("Model 1: China (RI)", "Model 2: China (RI+RS)",
                  "Model 3: US (RI)", "Model 4: US (RI+RS)")
  )
)
## =========================
## Models 5-8 (favor toward US by China/US × satisfaction; by region)
## =========================

## Model 5: US, random intercept
vars_m5 <- c("fav_us_weighted", "satisf_cwc", "log_CHN", "region")
m5_df <- dat %>% mutate(region = droplevels(region)) %>% make_model_df(vars_m5)

model_us_5 <- lmer(
  fav_us_weighted ~ satisf_cwc * log_CHN + (1 | region),
  data = m5_df
)
cat("\n--- Model 5: US (RI) ---\n")
print(summary(model_us_5))
cat("Singular fit? ", isSingular(model_us_5), "\n\n")

## Model 6: US, random intercept + random slope (satisfaction)
vars_m6 <- c("fav_us_weighted", "satisf_cwc", "log_CHN", "region")
m6_df <- dat %>% mutate(region = droplevels(region)) %>% make_model_df(vars_m6)

model_us_6 <- lmer(
  fav_us_weighted ~ satisf_cwc * log_CHN + (1 + satisf_cwc | region),
  data = m6_df
)
cat("\n--- Model 6: US (RI+RS) ---\n")
print(summary(model_us_6))
cat("Singular fit? ", isSingular(model_us_6), "\n\n")

## Model 7: US, random intercept
vars_m7 <- c("fav_us_weighted", "satisf_cwc", "log_US", "region")
m7_df <- dat %>% mutate(region = droplevels(region)) %>% make_model_df(vars_m7)

model_us_7 <- lmer(
  fav_us_weighted ~ satisf_cwc * log_US + (1 | region),
  data = m7_df
)
cat("\n--- Model 7: US (RI) ---\n")
print(summary(model_us_7))
cat("Singular fit? ", isSingular(model_us_7), "\n\n")

## Model 8: US, random intercept + random slope (satisfaction)
vars_m8 <- c("fav_us_weighted", "satisf_cwc", "log_US", "region")
m8_df <- dat %>% mutate(region = droplevels(region)) %>% make_model_df(vars_m8)

model_us_8 <- lmer(
  fav_us_weighted ~ satisf_cwc * log_US + (1 + satisf_cwc | region),
  data = m8_df
)
cat("\n--- Model 8: US (RI+RS) ---\n")
print(summary(model_us_8))
cat("Singular fit? ", isSingular(model_us_8), "\n\n")

## Comparison table for Models 5-8
suppressWarnings(
  tab_model(
    model_us_5, model_us_6, model_us_7, model_us_8,
    show.ci = FALSE,
    show.re.var = TRUE,
    show.icc = TRUE,
    title = "China and US Aid Interaction Effects",
    dv.labels = c("Model 5: China (RI)", "Model 6: China (RI+RS)",
                  "Model 7: US (RI)", "Model 8: US (RI+RS)")
  )
)
## =========================
## Party × Post-2015 × China aid (APC & PDP)
## =========================

## Model APC with China aid 
vars_apc <- c("fav_china_weighted", "satisf_cwc", "is_apc", "post_2015", "log_CHN", "region", "year")
apc_df <- dat %>%
  mutate(
    region = droplevels(region),
    year = droplevels(year)
  ) %>%
  make_model_df(vars_apc)

model_apc <- lmer(
  fav_china_weighted ~ satisf_cwc * is_apc * post_2015 * log_CHN +
    (1 | region) + (1 | year),
  data = apc_df
)
cat("\n--- Model APC ---\n")
print(summary(model_apc))
cat("Singular fit? ", isSingular(model_apc), "\n\n")

## Model PDP with China aid
vars_pdp <- c("fav_china_weighted", "satisf_cwc", "is_pdp", "post_2015", "log_CHN", "region", "year")
pdp_df <- dat %>%
  mutate(
    region = droplevels(region),
    year = droplevels(year)
  ) %>%
  make_model_df(vars_pdp)

model_pdp <- lmer(
  fav_china_weighted ~ satisf_cwc * is_pdp * post_2015 * log_CHN +
    (1 | region) + (1 | year),
  data = pdp_df
)
cat("\n--- Model PDP ---\n")
print(summary(model_pdp))
cat("Singular fit? ", isSingular(model_pdp), "\n\n")

## Comparison table for APC & PDP
suppressWarnings(
  tab_model(
    model_apc, model_pdp,
    show.ci = FALSE,
    show.re.var = TRUE,
    show.icc = TRUE,
    title = "Interaction Effects by Party Affiliation",
    dv.labels = c("APC Supporters", "PDP Supporters")
  )
)
## =========================
## Party × Post-2015 × US aid (APC & PDP)
## Note: response is *unweighted* fav_china, following your code.
## =========================

## Model APC with US aid 
vars_apc_us <- c("fav_us_weighted", "satisf_cwc", "is_apc", "post_2015", "log_US", "region", "year")
apc_df_us <- dat %>%
  mutate(
    region = droplevels(region),
    year = droplevels(year)
  ) %>%
  make_model_df(vars_apc_us)

model_apc_us <- lmer(
  fav_us_weighted ~ satisf_cwc * is_apc * post_2015 * log_US +
    (1 | region) + (1 | year),
  data = apc_df_us
)
cat("\n--- Model APC US ---\n")
print(summary(model_apc_us))
cat("Singular fit? ", isSingular(model_apc_us), "\n\n")

## Model PDP with US aid
vars_pdp_us <- c("fav_us_weighted", "satisf_cwc", "is_pdp", "post_2015", "log_US", "region", "year")
pdp_df_us <- dat %>%
  mutate(
    region = droplevels(region),
    year = droplevels(year)
  ) %>%
  make_model_df(vars_pdp_us)

model_pdp_us <- lmer(
  fav_us_weighted ~ satisf_cwc * is_pdp * post_2015 * log_US +
    (1 | region) + (1 | year),
  data = pdp_df_us
)
cat("\n--- Model PDP ---\n")
print(summary(model_pdp_us))
cat("Singular fit? ", isSingular(model_pdp_us), "\n\n")

## Comparison table for APC & PDP
suppressWarnings(
  tab_model(
    model_apc_us, model_pdp_us,
    show.ci = FALSE,
    show.re.var = TRUE,
    show.icc = TRUE,
    title = "Interaction Effects by Party Affiliation",
    dv.labels = c("APC Supporters", "PDP Supporters")
  )
)
