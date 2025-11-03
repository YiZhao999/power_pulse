needed_packages <- c(
  "lme4", "lmerTest", "tidyverse", "sjPlot", "broom.mixed",
  "ggeffects", "ggplot2", "dplyr", "lubridate",
  "did", "modelsummary"
)
new_packages <- needed_packages[!(needed_packages %in% installed.packages()[, "Package"])]
if (length(new_packages)) install.packages(new_packages)
library(lme4)
library(lmerTest)
library(tidyverse)
library(sjPlot)
library(broom.mixed)
library(ggeffects)
library(ggplot2)
library(dplyr)
library(lubridate)
library(did)
library(modelsummary)

data_path <- "~/Desktop/nigeria.csv"
raw <- read.csv(data_path)

dat <- raw %>%
  mutate(
    year                = as.factor(year),
    region              = as.factor(region),
    fav_us_weighted     = fav_us * weight,
    fav_china_weighted  = fav_china * weight,
    econ_weighted       = econ * weight,
    satisfaction_weighted = satisfaction * weight,
    # Region-year aid (donor-specific)
    log_CHN = log1p(CHN_comm),
    log_US  = log1p(USA_comm)
  ) %>%
  group_by(region) %>%
  mutate(satisf_cwc = econ_weighted - mean(econ_weighted, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    party_affil_apc = as.numeric(is_apc),  # 1 = APC, 0 = non-APC
    is_pdp          = as.numeric(is_pdp),  
    post_2015       = as.numeric(post_2015)
  )

# =============================
# Aggregate Party Support (Region-Year)
# =============================
agg_party_df <- dat %>%
  group_by(region, year) %>%
  summarise(
    share_apc = mean(party_affil_apc, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(agg_party = share_apc)

dat <- dat %>%
  left_join(agg_party_df %>% select(region, year, agg_party), by = c("region", "year"))

# =============================
# Country-Level Aid (Country-Year)
# =============================
country_aid_df <- dat %>%
  group_by(year) %>%
  summarise(
    CHN_country = sum(CHN_comm, na.rm = TRUE),
    USA_country = sum(USA_comm, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    log_CHN_country = log1p(CHN_country),
    log_US_country  = log1p(USA_country)
  )

dat <- dat %>%
  left_join(country_aid_df %>% select(year, log_CHN_country, log_US_country),
            by = "year")

make_model_df <- function(dat, vars) {
  out <- dat %>% select(all_of(vars)) %>% drop_na()
  n_total <- nrow(dat); n_used <- nrow(out)
  message(sprintf("Kept %d / %d rows (%.1f%%) after NA-drop for vars: %s",
                  n_used, n_total, 100 * n_used / max(1, n_total), paste(vars, collapse = ", ")))
  out
}


did_dat <- dat %>%
  mutate(
    # ensure numeric year
    year_num = as.integer(as.character(year)),
    region_id = as.character(region),
    
    # region-year treatment indicator
    treat_ry = as.integer(CHN_comm > 0),
    
    # first year of treatment by region (NA if never treated)
    first_treat = ave(
      ifelse(treat_ry == 1, year_num, NA_integer_),
      region_id,
      FUN = function(v) {
        v <- v[!is.na(v)]
        if (length(v) == 0) return(NA_integer_) else return(min(v))
      }
    ),
    
    # outcomes and controls
    y_china = fav_china_weighted,
    y_us    = fav_us_weighted,
    econ_cwc  = satisf_cwc,
    party_apc = party_affil_apc
  )

did_dat <- did_dat %>%
  mutate(
    # Ensure ID is numeric
    survey_id = as.numeric(as.factor(survey)),
    
    # Confirm year numeric
    year_num = as.integer(as.character(year)),
    
    # Double-check treatment and cohort
    treat_ry = as.integer(CHN_comm > 0)
  )

# Now rerun att_gt
att_cs <- att_gt(
  yname   = "y_china",
  tname   = "year_num",
  idname  = "survey_id",       # numeric id
  gname   = "first_treat",     # first treatment year
  data    = did_dat,
  panel   = FALSE,
  control_group = "notyettreated",   # or "never_treated" if many early-treated
  weightsname   = "weight",
  clustervars   = "region_id"
)

summary(att_cs)

att_cs2 <- att_gt(
  yname = "y_china",
  tname = "year_num",
  idname = "survey_id",
  gname = "first_treat",
  data = did_dat,
  panel = FALSE,
  control_group = "never_treated",
  weightsname = "weight",
  clustervars = "region_id"
)
summary(att_cs2)

agg_dynamic <- aggte(att_cs2, type = "dynamic")
summary(agg_dynamic)
plot(agg_dynamic)

did_dat %>%
  distinct(region_id, first_treat) %>%
  count(first_treat)


# === Export tidy tables ===
msummary(
  list("Overall ATT (CHN → Fav. China)" = agg_overall),
  output = "markdown"
)

did_dat <- did_dat %>% mutate(treat_ry_us = as.integer(USA_comm > 0),
                              first_treat_us = ifelse(
                                ave(treat_ry_us, region_id, FUN = function(x) any(x==1)) == 1,
                                ave(ifelse(treat_ry_us==1, year_num, NA_integer_), region_id, FUN = function(v) min(v, na.rm=TRUE)),
                                NA_integer_
                              ),
                              y_us = fav_us_weighted)

att_cs_us <- att_gt(
  yname   = "y_us",
  tname   = "year_num",
  idname  = "survey",
  gname   = "first_treat_us",
  xformla = ~ econ_cwc + party_apc,
  data    = did_dat,
  panel   = FALSE,
  control_group = "notyettreated",
  weightsname   = "weight",
  clustervars   = "region_id"
)
agg_dynamic_us <- aggte(att_cs_us, type = "dynamic")
plot(agg_dynamic_us)
