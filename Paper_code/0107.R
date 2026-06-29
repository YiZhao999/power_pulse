# ===============================
# 0. Load packages
# ===============================
library(tidyverse)
library(fixest)

# ===============================
# 1. Load data
# ===============================
dat <- read_csv("~/Desktop/0107.csv")

# ===============================
# 2. Basic cleaning
# ===============================
dat <- dat %>%
  mutate(
    year = as.integer(year),
    id   = as.factor(Countryname)
  ) %>%
  arrange(id, year)

# ===============================
# 3. Create lagged aid variables
# ===============================
dat <- dat %>%
  group_by(id) %>%
  mutate(
    USA_comm_l1 = lag(USA_comm, 1),
    CHN_comm_l1 = lag(CHN_comm, 1)
  ) %>%
  ungroup()

# Optional: drop first year of each panel
dat <- dat %>%
  filter(!is.na(CHN_comm_l1))

# ===============================
# 4. Model 1:
# Vote_it = β1 Aid_it-1 + α_i + λ_t
# ===============================
m1 <- feols(
  ChinaAgree ~ CHN_comm_l1 | id + year,
  data = dat,
  cluster = ~id
)

# ===============================
# 5. Model 2:
# US Aid + China Aid + Interaction
# ===============================
m2 <- feols(
  ChinaAgree ~ CHN_comm_l1 * USA_comm_l1 | id + year,
  data = dat,
  cluster = ~id
)

# ===============================
# 6. Results
# ===============================
summary(m1)
summary(m2)

# ===============================
# 7. Regression table (optional)
# ===============================
etable(
  m1, m2,
  se = "cluster",
  cluster = "id",
  dict = c(
    USA_comm_l1 = "US Aid (t-1)",
    CHN_comm_l1 = "China Aid (t-1)",
    "USA_comm_l1:CHN_comm_l1" = "US Aid × China Aid"
  ),
  fitstat = ~n + r2 + r2_within
)

