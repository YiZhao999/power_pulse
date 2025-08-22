library(lme4)
library(lmerTest)
library(tidyverse)
library(sjPlot)
library(broom.mixed)
library(dplyr)

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
    log_US  = log1p(USA_comm)
  ) %>%
  group_by(region) %>%
  mutate(satisf_cwc = econ_weighted - mean(econ_weighted, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    is_apc     = as.numeric(is_apc),
    is_pdp     = as.numeric(is_pdp),
    post_2015  = as.numeric(post_2015)
  )

# Compute region-year aggregate party shares
agg_party_df <- dat %>%
  group_by(region, year) %>%
  summarise(
    share_apc = mean(is_apc, na.rm = TRUE),
    share_pdp = mean(is_pdp, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(agg_party = share_apc)  

# Join back to individual-level data
dat <- dat %>%
  left_join(agg_party_df %>% select(region, year, agg_party), by = c("region", "year"))

# Function to select variables for modeling
make_model_df <- function(dat, vars) {
  out <- dat %>% select(all_of(vars)) %>% drop_na()
  n_total <- nrow(dat)
  n_used  <- nrow(out)
  message(sprintf("Kept %d / %d rows (%.1f%%) after NA-drop for vars: %s",
                  n_used, n_total, 100 * n_used / max(1, n_total), paste(vars, collapse = ", ")))
  out
}

## =========================
## Full multilevel specification for favorability toward China
## =========================

vars_full <- c("fav_china_weighted", 
               "satisf_cwc", 
               "is_apc", "is_pdp",
               "agg_party",
               "post_2015", 
               "log_CHN", "log_US",
               "region", "year")

full_df <- dat %>%
  mutate(
    region = droplevels(region),
    year   = droplevels(year)
  ) %>%
  make_model_df(vars_full)

model_full_china <- lmer(
  fav_china_weighted ~ 
    satisf_cwc + 
    is_apc + is_pdp + 
    log_CHN * agg_party + 
    post_2015 * (is_apc + is_pdp) * log_CHN + 
    log_US + 
    (1 + is_apc + is_pdp | region) + 
    (1 | year),
  data = full_df
)

cat("\n--- Full Model: Favorability toward China ---\n")
print(summary(model_full_china))
cat("Singular fit? ", isSingular(model_full_china), "\n\n")
## =========================
## Full multilevel specification for favorability toward US
## =========================

vars_full_us <- c("fav_us_weighted", 
                  "satisf_cwc", 
                  "is_apc", "is_pdp",
                  "agg_party",
                  "post_2015", 
                  "log_CHN", "log_US",
                  "region", "year")

full_df_us <- dat %>%
  mutate(
    region = droplevels(region),
    year   = droplevels(year)
  ) %>%
  make_model_df(vars_full_us)

model_full_us <- lmer(
  fav_us_weighted ~ 
    satisf_cwc + 
    is_apc + is_pdp + 
    log_US * agg_party +     
    post_2015 * (is_apc + is_pdp) * log_US +  
    log_CHN +               
    (1 + is_apc + is_pdp | region) + 
    (1 | year),
  data = full_df_us
)

cat("\n--- Full Model: Favorability toward US ---\n")
print(summary(model_full_us))
cat("Singular fit? ", isSingular(model_full_us), "\n\n")

## =========================
## Side-by-side comparison
## =========================
suppressWarnings(
  tab_model(
    model_full_china, model_full_us,
    show.ci = FALSE,
    show.re.var = TRUE,
    show.icc = TRUE,
    title = "Multilevel Effects of China vs. US Aid on Nigerian Public Opinion",
    dv.labels = c("Favorability toward China", "Favorability toward US")
  )
)
# Install packages if needed
packages <- c("ggeffects", "ggplot2", "dplyr")
installed <- packages %in% rownames(installed.packages())
if(any(!installed)) install.packages(packages[!installed])

library(ggeffects)
library(ggplot2)
library(dplyr)

# -------------------------------
# Visualization for China favorability
# -------------------------------

# Generate predicted values for log_CHN across range, by party and post_2015
pred_china <- ggpredict(model_full_china, 
                        terms = c("log_CHN [all]", "is_apc", "post_2015"))

# Adjust labels for clarity
pred_china$party <- ifelse(pred_china$group == "0", "Non-APC", "APC")
pred_china$post <- ifelse(pred_china$facet == "0", "Pre-2015", "Post-2015")

# Plot
ggplot(pred_china, aes(x = x, y = predicted, color = party, linetype = post)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = party), alpha = 0.2, color = NA) +
  labs(
    x = "China Aid (log)",
    y = "Predicted Favorability toward China",
    title = "Effect of China Aid on Nigerian Favorability by APC Support & Election Period",
    color = "Party Affiliation",
    fill = "Party Affiliation",
    linetype = "Election Period"
  ) +
  theme_minimal(base_size = 14)

# -------------------------------
# Visualization for US favorability
# -------------------------------

pred_us <- ggpredict(model_full_us, 
                     terms = c("log_US [all]", "is_apc", "post_2015"))

pred_us$party <- ifelse(pred_us$group == "0", "Non-APC", "APC")
pred_us$post <- ifelse(pred_us$facet == "0", "Pre-2015", "Post-2015")

ggplot(pred_us, aes(x = x, y = predicted, color = party, linetype = post)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = party), alpha = 0.2, color = NA) +
  labs(
    x = "US Aid (log)",
    y = "Predicted Favorability toward US",
    title = "Effect of US Aid on Nigerian Favorability by APC Support & Election Period",
    color = "Party Affiliation",
    fill = "Party Affiliation",
    linetype = "Election Period"
  ) +
  theme_minimal(base_size = 14)
