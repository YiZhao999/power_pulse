# ===============================
# 0. Packages
# ===============================
library(tidyverse)
library(countrycode)
library(fixest)

# ===============================
# 1. Load data
# ===============================
dat <- read_csv("~/Desktop/0107.csv")

# ===============================
# 2. Panel identifiers
# ===============================
dat <- dat %>%
  mutate(
    year = as.integer(year),
    id   = as.factor(Countryname)
  ) %>%
  arrange(id, year)

# ===============================
# 3. Harmonize country names
# ===============================
dat <- dat %>%
  mutate(
    country_std = countrycode(
      Countryname,
      origin      = "country.name",
      destination = "country.name",
      warn = TRUE
    )
  )

dat <- dat %>%
  mutate(
    country_std = case_when(
      Countryname %in% c("Congo, Dem. Rep.", "DR Congo", "Democratic Republic Congo")
      ~ "Democratic Republic of the Congo",
      Countryname %in% c("Congo, Rep.", "Republic of Congo")
      ~ "Republic of the Congo",
      Countryname %in% c("Cote d'Ivoire", "Côte d’Ivoire")
      ~ "Ivory Coast",
      Countryname == "Cape Verde" ~ "Cabo Verde",
      TRUE ~ country_std
    )
  )

# ===============================
# 4. Continent dummy (Africa)
# ===============================
dat <- dat %>%
  mutate(
    continent = countrycode(
      country_std,
      origin = "country.name",
      destination = "continent"
    ),
    africa = if_else(continent == "Africa", 1L, 0L)
  )

# ===============================
# 5. Africa-only sample
# ===============================
dat_africa <- dat %>%
  filter(africa == 1)

# ===============================
# 6. Construct dynamic outcome
# ===============================
dat_africa <- dat_africa %>%
  arrange(id, year) %>%
  group_by(id) %>%
  mutate(
    d_USAgree = USAgree - lag(USAgree, 1)
  ) %>%
  ungroup()

# ===============================
# 7. Lag aid variables
# ===============================
dat_africa <- dat_africa %>%
  arrange(id, year) %>%
  group_by(id) %>%
  mutate(
    CHN_comm_l1 = lag(CHN_comm, 1),
    USA_comm_l1 = lag(USA_comm, 1)
  ) %>%
  ungroup()

# Keep usable observations
dat_africa <- dat_africa %>%
  filter(
    !is.na(d_USAgree),
    !is.na(CHN_comm_l1),
    !is.na(USA_comm_l1)
  )

# ===============================
# 8. Residualize US aid on China aid
#    (within year)
# ===============================
dat_africa <- dat_africa %>%
  group_by(year) %>%
  mutate(
    CHN_comm_resid = resid(
      lm(CHN_comm_l1 ~ USA_comm_l1)
    )
  ) %>%
  ungroup()

# ===============================
# 9. Model 1: China aid → change in similarity
# ===============================
m1_dyn <- feols(
  d_USAgree ~ USA_comm_l1 | id + year,
  data = dat_africa,
  cluster = ~id
)

# ===============================
# 10. Model 2: Residualized interaction
# ===============================
m2_dyn_resid <- feols(
  d_USAgree ~ USA_comm_l1 * CHN_comm_resid | id + year,
  data = dat_africa,
  cluster = ~id
)

# ===============================
# 11. Output
# ===============================
summary(m1_dyn)
summary(m2_dyn_resid)

etable(
  m1_dyn, m2_dyn_resid,
  se = "cluster",
  cluster = "id",
  dict = c(
    CHN_comm_l1 = "China Aid (t-1)",
    USA_comm_resid = "US Aid (residualized)",
    "CHN_comm_l1:USA_comm_resid" = "China Aid × US Aid (resid.)"
  ),
  fitstat = ~n + r2 + r2_within
)
