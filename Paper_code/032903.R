# ================================================================
#  Hypothesis 3 Analysis
#  "Improvements in public favorability toward a donor should
#   increase subsequent political alignment with that donor."
#
#  Part A – Lagged regression (primary H3 test)
#    DV:      vote_us / vote_china (UN voting alignment at t)
#    IV:      fav_us_r / fav_china_r lagged 1 and 2 years
#    Control: log_aid (contemporaneous)
#    FEs:     country + year | SEs clustered by country
#
#  Part B – Mediation analysis (does aid → favorability → vote?)
#    Step 1 (Baron & Kenny): aid → vote (total effect)
#    Step 2 (B&K):           aid → favorability (a path)
#    Step 3 (B&K):           aid + favorability → vote (b & c' paths)
#    Step 4 (formal):        mediation::mediate() for ACME + 95% CI
#
#  H3 prediction:  β(lagged favorability) > 0
#  Mediation pred: ACME (indirect effect) > 0 and significant
# ================================================================

# ── 0. Packages ─────────────────────────────────────────────────
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
  tidyverse, fixest, modelsummary, patchwork, scales, mediation
)


# ── 1. Load & prepare data ───────────────────────────────────────
df_raw <- read_csv("~/Desktop/SPRING2026/MA_paper/0329/final_merged_dataset.csv",
                   show_col_types = FALSE)

rescale_fav <- function(x) {
  1 - (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

df <- df_raw %>%
  mutate(
    fav_us_r      = rescale_fav(fav_us),
    fav_china_r   = rescale_fav(fav_china),
    log_aid_us    = log(aid_us    + 1),
    log_aid_china = log(aid_china + 1),
    country       = as.factor(country),
    year          = as.integer(year)
  ) %>%
  arrange(country, year)


# ── 2. Create lagged favorability variables ──────────────────────
# Lags are computed within country.
# Gap-year check: only assign a lag when the prior row is exactly
# 1 (or 2) years earlier — avoids bridging across missing years.

df <- df %>%
  group_by(country) %>%
  mutate(
    fav_us_lag1    = if_else(year - lag(year)    == 1, lag(fav_us_r,    1), NA_real_),
    fav_china_lag1 = if_else(year - lag(year)    == 1, lag(fav_china_r, 1), NA_real_),
    fav_us_lag2    = if_else(year - lag(year, 2) == 2, lag(fav_us_r,    2), NA_real_),
    fav_china_lag2 = if_else(year - lag(year, 2) == 2, lag(fav_china_r, 2), NA_real_)
  ) %>%
  ungroup() %>%
  mutate(year = as.factor(year))


# ── 3. Descriptive Statistics ────────────────────────────────────
h3_vars <- c(
  "vote_us", "vote_china",
  "fav_us_r", "fav_china_r",
  "fav_us_lag1", "fav_china_lag1",
  "fav_us_lag2", "fav_china_lag2",
  "log_aid_us", "log_aid_china"
)

h3_labels <- c(
  vote_us        = "UN Voting Alignment: US (DV)",
  vote_china     = "UN Voting Alignment: China (DV)",
  fav_us_r       = "US Favorability (contemporaneous)",
  fav_china_r    = "China Favorability (contemporaneous)",
  fav_us_lag1    = "US Favorability (lag 1 year)",
  fav_china_lag1 = "China Favorability (lag 1 year)",
  fav_us_lag2    = "US Favorability (lag 2 years)",
  fav_china_lag2 = "China Favorability (lag 2 years)",
  log_aid_us     = "Log US Aid",
  log_aid_china  = "Log China Aid"
)

desc_tbl <- df %>%
  select(all_of(h3_vars)) %>%
  pivot_longer(everything(), names_to = "Variable") %>%
  group_by(Variable) %>%
  summarise(
    N      = sum(!is.na(value)),
    Mean   = round(mean(value,   na.rm = TRUE), 3),
    SD     = round(sd(value,     na.rm = TRUE), 3),
    Min    = round(min(value,    na.rm = TRUE), 3),
    Median = round(median(value, na.rm = TRUE), 3),
    Max    = round(max(value,    na.rm = TRUE), 3),
    .groups = "drop"
  ) %>%
  mutate(Variable = recode(Variable, !!!h3_labels))

message("=== Table 1: Descriptive Statistics ===")
print(desc_tbl, n = Inf)

message("\n=== Effective N after lagging ===")
message("US  lag-1: ", sum(!is.na(df$fav_us_lag1)    & !is.na(df$vote_us)))
message("US  lag-2: ", sum(!is.na(df$fav_us_lag2)    & !is.na(df$vote_us)))
message("CN  lag-1: ", sum(!is.na(df$fav_china_lag1) & !is.na(df$vote_china)))
message("CN  lag-2: ", sum(!is.na(df$fav_china_lag2) & !is.na(df$vote_china)))


# ── 4. Visualisations ────────────────────────────────────────────
theme_paper <- theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(size = 10, colour = "grey40"),
    legend.position  = "bottom",
    panel.grid.minor = element_blank()
  )

pal_donor <- c("United States" = "#1565C0", "China" = "#C62828")

# Fig 1 – Distribution of UN voting alignment (DV)
vote_long <- df %>%
  select(vote_us, vote_china) %>%
  pivot_longer(everything(), names_to = "Donor", values_to = "Alignment") %>%
  mutate(Donor = recode(Donor, vote_us = "United States", vote_china = "China"))

vote_means <- vote_long %>%
  group_by(Donor) %>%
  summarise(m = mean(Alignment, na.rm = TRUE), .groups = "drop")

fig1 <- ggplot(vote_long, aes(Alignment, fill = Donor, colour = Donor)) +
  geom_density(alpha = 0.30, linewidth = 0.9) +
  geom_vline(data = vote_means,
             aes(xintercept = m, colour = Donor),
             linetype = "dashed", linewidth = 0.9) +
  scale_fill_manual(values = pal_donor) +
  scale_colour_manual(values = pal_donor) +
  labs(
    title    = "Figure 1. Distribution of UN Voting Alignment (Outcome)",
    subtitle = "Higher = more agreement with donor | Dashed = group mean",
    x = "UN Voting Alignment (proportion)", y = "Density",
    fill = "Donor", colour = "Donor"
  ) +
  theme_paper

# Fig 2 – Cross-lagged scatter: fav(t-1) → vote(t)
fig2a <- df %>%
  filter(!is.na(fav_us_lag1), !is.na(vote_us)) %>%
  ggplot(aes(fav_us_lag1, vote_us)) +
  geom_point(alpha = 0.50, size = 2.2, colour = "#1565C0") +
  geom_smooth(method = "lm", se = TRUE,
              colour = "#1565C0", fill = "#1565C0", alpha = 0.15) +
  labs(title    = "Figure 2a. US Favorability (t−1) → US Vote Alignment (t)",
       subtitle = "Each point = country-year",
       x = "US Favorability, lag 1 (rescaled)", y = "UN Voting Alignment with US") +
  theme_paper

fig2b <- df %>%
  filter(!is.na(fav_china_lag1), !is.na(vote_china)) %>%
  ggplot(aes(fav_china_lag1, vote_china)) +
  geom_point(alpha = 0.50, size = 2.2, colour = "#C62828") +
  geom_smooth(method = "lm", se = TRUE,
              colour = "#C62828", fill = "#C62828", alpha = 0.15) +
  labs(title    = "Figure 2b. China Favorability (t−1) → China Vote Alignment (t)",
       subtitle = "Each point = country-year",
       x = "China Favorability, lag 1 (rescaled)", y = "UN Voting Alignment with China") +
  theme_paper

# Fig 3 – Year trends (co-movement inspection)
trend_df <- df %>%
  mutate(year_num = as.integer(as.character(year))) %>%
  group_by(year_num) %>%
  summarise(
    fav_us_mean     = mean(fav_us_r,    na.rm = TRUE),
    fav_china_mean  = mean(fav_china_r, na.rm = TRUE),
    vote_us_mean    = mean(vote_us,     na.rm = TRUE),
    vote_china_mean = mean(vote_china,  na.rm = TRUE),
    .groups = "drop"
  )

fig3a <- trend_df %>%
  ggplot(aes(x = year_num)) +
  geom_line(aes(y = fav_us_mean,  colour = "US Favorability"),  linewidth = 1) +
  geom_line(aes(y = vote_us_mean, colour = "US Vote Alignment"),
            linewidth = 1, linetype = "dashed") +
  geom_point(aes(y = fav_us_mean,  colour = "US Favorability"),  size = 2) +
  geom_point(aes(y = vote_us_mean, colour = "US Vote Alignment"), size = 2) +
  scale_colour_manual(
    values = c("US Favorability" = "#1565C0", "US Vote Alignment" = "#90CAF9"),
    name = NULL) +
  labs(title    = "Figure 3a. Trends: US Favorability & Vote Alignment",
       subtitle = "Sample means by year",
       x = "Year", y = "Score (0–1)") +
  theme_paper

fig3b <- trend_df %>%
  ggplot(aes(x = year_num)) +
  geom_line(aes(y = fav_china_mean,  colour = "China Favorability"),  linewidth = 1) +
  geom_line(aes(y = vote_china_mean, colour = "China Vote Alignment"),
            linewidth = 1, linetype = "dashed") +
  geom_point(aes(y = fav_china_mean,  colour = "China Favorability"),  size = 2) +
  geom_point(aes(y = vote_china_mean, colour = "China Vote Alignment"), size = 2) +
  scale_colour_manual(
    values = c("China Favorability" = "#C62828", "China Vote Alignment" = "#EF9A9A"),
    name = NULL) +
  labs(title    = "Figure 3b. Trends: China Favorability & Vote Alignment",
       subtitle = "Sample means by year",
       x = "Year", y = "Score (0–1)") +
  theme_paper

# Fig 4 – Mediation path diagram (conceptual, static ggplot)
# Draws the three paths: X(aid) → M(fav) → Y(vote), X → Y directly
med_path <- ggplot() +
  # Nodes
  annotate("label", x = 0,   y = 0,   label = "Aid\n(X)",
           size = 4.5, fontface = "bold",
           fill = "#E3F2FD", colour = "#1565C0", label.size = 1) +
  annotate("label", x = 0.5, y = 0.8, label = "Favorability\n(M)",
           size = 4.5, fontface = "bold",
           fill = "#FFF9C4", colour = "#F57F17", label.size = 1) +
  annotate("label", x = 1,   y = 0,   label = "UN Vote\nAlignment (Y)",
           size = 4.5, fontface = "bold",
           fill = "#E8F5E9", colour = "#2E7D32", label.size = 1) +
  # Arrows
  annotate("segment", x = 0.12, xend = 0.38, y = 0.12, yend = 0.68,
           arrow = arrow(length = unit(0.25,"cm"), type = "closed"),
           colour = "#F57F17", linewidth = 1.1) +
  annotate("segment", x = 0.62, xend = 0.88, y = 0.68, yend = 0.12,
           arrow = arrow(length = unit(0.25,"cm"), type = "closed"),
           colour = "#2E7D32", linewidth = 1.1) +
  annotate("segment", x = 0.12, xend = 0.88, y = 0, yend = 0,
           arrow = arrow(length = unit(0.25,"cm"), type = "closed"),
           colour = "#1565C0", linewidth = 1.1) +
  # Path labels
  annotate("text", x = 0.22, y = 0.47, label = "a path",
           size = 3.8, colour = "#F57F17", fontface = "italic") +
  annotate("text", x = 0.78, y = 0.47, label = "b path",
           size = 3.8, colour = "#2E7D32", fontface = "italic") +
  annotate("text", x = 0.50, y = -0.10, label = "c' path (direct)",
           size = 3.8, colour = "#1565C0", fontface = "italic") +
  coord_cartesian(xlim = c(-0.15, 1.15), ylim = c(-0.25, 1.0)) +
  labs(title    = "Figure 4. Mediation Path Diagram",
       subtitle = "ACME (indirect effect) = a × b | H3: ACME > 0") +
  theme_void(base_size = 12) +
  theme(plot.title    = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(size = 10, colour = "grey40"))

# Save figures
ggsave("h3_fig_dv_dist.png",       fig1,          width = 9,  height = 5,   dpi = 200)
ggsave("h3_fig_crosslag.png",      fig2a | fig2b, width = 13, height = 5.5, dpi = 200)
ggsave("h3_fig_trends.png",        fig3a | fig3b, width = 13, height = 5.5, dpi = 200)
ggsave("h3_fig_mediation_dag.png", med_path,      width = 8,  height = 5,   dpi = 200)
message("✓ Figures saved.")


# ════════════════════════════════════════════════════════════════
#  PART A: Lagged Regression (Primary H3 Test)
# ════════════════════════════════════════════════════════════════

m_us_lag1 <- feols(vote_us ~ fav_us_lag1    + log_aid_us    | country + year,
                   data = df, cluster = ~country)
m_us_lag2 <- feols(vote_us ~ fav_us_lag2    + log_aid_us    | country + year,
                   data = df, cluster = ~country)
m_cn_lag1 <- feols(vote_china ~ fav_china_lag1 + log_aid_china | country + year,
                   data = df, cluster = ~country)
m_cn_lag2 <- feols(vote_china ~ fav_china_lag2 + log_aid_china | country + year,
                   data = df, cluster = ~country)

message("\n=== Table 2: Lagged Regression Results (H3) ===")
etable(
  m_us_lag1, m_us_lag2, m_cn_lag1, m_cn_lag2,
  headers     = c("US Vote (Lag 1)", "US Vote (Lag 2)",
                  "CN Vote (Lag 1)", "CN Vote (Lag 2)"),
  coefstat    = "se",
  signif.code = c("***" = 0.01, "**" = 0.05, "*" = 0.10)
)

modelsummary(
  list("US (Lag 1)" = m_us_lag1, "US (Lag 2)" = m_us_lag2,
       "CN (Lag 1)" = m_cn_lag1, "CN (Lag 2)" = m_cn_lag2),
  stars       = c("*" = .10, "**" = .05, "***" = .01),
  coef_rename = c(
    "fav_us_lag1"    = "US Favorability (t−1)",
    "fav_us_lag2"    = "US Favorability (t−2)",
    "fav_china_lag1" = "China Favorability (t−1)",
    "fav_china_lag2" = "China Favorability (t−2)",
    "log_aid_us"     = "Log US Aid",
    "log_aid_china"  = "Log China Aid"
  ),
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  notes   = "Two-way FEs (country + year). SEs clustered by country. Lags gap-corrected.",
  output  = "h3_lagged_regression_table.txt"
)
message("✓ Lagged regression table saved.")


# ════════════════════════════════════════════════════════════════
#  PART B: Mediation Analysis
#  Does aid → favorability → UN voting alignment?
#
#  NOTE: The mediation package requires lm() objects (not feols).
#  We add country and year as dummy controls via lm() to approximate
#  the FE structure. This is standard practice for mediation with
#  panel controls.
#
#  We use lag-1 favorability as the mediator to maintain temporal
#  ordering: aid(t-1) → fav(t-1) → vote(t).
#  Analysis run separately for US and China.
# ════════════════════════════════════════════════════════════════

# Prepare complete-case datasets for mediation
# Temporal ordering: all variables measured contemporaneously at t-1,
# outcome vote at t. We use fav_lag1 as the mediator.

med_df_us <- df %>%
  filter(!is.na(fav_us_lag1), !is.na(vote_us),
         !is.na(log_aid_us)) %>%
  droplevels()

med_df_cn <- df %>%
  filter(!is.na(fav_china_lag1), !is.na(vote_china),
         !is.na(log_aid_china)) %>%
  droplevels()

# ------------------------------------------------------------------
# Baron & Kenny Steps — United States
# ------------------------------------------------------------------
message("\n=== Baron & Kenny Steps: United States ===")

# Step 1: Total effect — X (log_aid_us) → Y (vote_us)
bk_us_step1 <- lm(vote_us ~ log_aid_us + country + year, data = med_df_us)
cat("\nStep 1 (Total effect: aid → vote_us):\n")
cat("  β(log_aid_us) =",
    round(coef(bk_us_step1)["log_aid_us"], 4),
    " | p =",
    round(summary(bk_us_step1)$coefficients["log_aid_us", "Pr(>|t|)"], 4), "\n")

# Step 2: a path — X (log_aid_us) → M (fav_us_lag1)
bk_us_step2 <- lm(fav_us_lag1 ~ log_aid_us + country + year, data = med_df_us)
cat("\nStep 2 (a path: aid → favorability):\n")
cat("  β(log_aid_us) =",
    round(coef(bk_us_step2)["log_aid_us"], 4),
    " | p =",
    round(summary(bk_us_step2)$coefficients["log_aid_us", "Pr(>|t|)"], 4), "\n")

# Step 3: b path + direct effect — X + M → Y
bk_us_step3 <- lm(vote_us ~ log_aid_us + fav_us_lag1 + country + year,
                  data = med_df_us)
cat("\nStep 3 (b path + direct effect: aid + fav → vote_us):\n")
cat("  β(fav_us_lag1) [b path] =",
    round(coef(bk_us_step3)["fav_us_lag1"], 4),
    " | p =",
    round(summary(bk_us_step3)$coefficients["fav_us_lag1", "Pr(>|t|)"], 4), "\n")
cat("  β(log_aid_us)  [direct] =",
    round(coef(bk_us_step3)["log_aid_us"], 4),
    " | p =",
    round(summary(bk_us_step3)$coefficients["log_aid_us", "Pr(>|t|)"], 4), "\n")

# ------------------------------------------------------------------
# Baron & Kenny Steps — China
# ------------------------------------------------------------------
message("\n=== Baron & Kenny Steps: China ===")

bk_cn_step1 <- lm(vote_china ~ log_aid_china + country + year, data = med_df_cn)
cat("\nStep 1 (Total effect: aid → vote_china):\n")
cat("  β(log_aid_china) =",
    round(coef(bk_cn_step1)["log_aid_china"], 4),
    " | p =",
    round(summary(bk_cn_step1)$coefficients["log_aid_china", "Pr(>|t|)"], 4), "\n")

bk_cn_step2 <- lm(fav_china_lag1 ~ log_aid_china + country + year, data = med_df_cn)
cat("\nStep 2 (a path: aid → favorability):\n")
cat("  β(log_aid_china) =",
    round(coef(bk_cn_step2)["log_aid_china"], 4),
    " | p =",
    round(summary(bk_cn_step2)$coefficients["log_aid_china", "Pr(>|t|)"], 4), "\n")

bk_cn_step3 <- lm(vote_china ~ log_aid_china + fav_china_lag1 + country + year,
                  data = med_df_cn)
cat("\nStep 3 (b path + direct effect: aid + fav → vote_china):\n")
cat("  β(fav_china_lag1) [b path] =",
    round(coef(bk_cn_step3)["fav_china_lag1"], 4),
    " | p =",
    round(summary(bk_cn_step3)$coefficients["fav_china_lag1", "Pr(>|t|)"], 4), "\n")
cat("  β(log_aid_china)  [direct] =",
    round(coef(bk_cn_step3)["log_aid_china"], 4),
    " | p =",
    round(summary(bk_cn_step3)$coefficients["log_aid_china", "Pr(>|t|)"], 4), "\n")


# ------------------------------------------------------------------
# Formal Mediation Test — mediation::mediate()
# Quasi-Bayesian simulation, 1000 draws, 95% CI on ACME
# ------------------------------------------------------------------
set.seed(42)

message("\n=== Formal Mediation: United States (1000 simulations) ===")
med_us <- mediate(
  model.m  = bk_us_step2,   # mediator model (a path)
  model.y  = bk_us_step3,   # outcome model  (b + c' paths)
  treat    = "log_aid_us",
  mediator = "fav_us_lag1",
  sims     = 1000,
  boot     = FALSE           # quasi-Bayesian (faster, robust)
)
summary(med_us)

message("\n=== Formal Mediation: China (1000 simulations) ===")
med_cn <- mediate(
  model.m  = bk_cn_step2,
  model.y  = bk_cn_step3,
  treat    = "log_aid_china",
  mediator = "fav_china_lag1",
  sims     = 1000,
  boot     = FALSE
)
summary(med_cn)


# ------------------------------------------------------------------
# Fig 5 – Mediation results plot (ACME, ADE, Total Effect with CIs)
# ------------------------------------------------------------------
extract_med <- function(med_obj, donor_label) {
  tibble(
    Donor  = donor_label,
    Effect = c("ACME\n(Indirect)", "ADE\n(Direct)", "Total Effect"),
    Estimate = c(med_obj$d.avg,   med_obj$z.avg,   med_obj$tau.coef),
    Lower    = c(med_obj$d.avg.ci[1], med_obj$z.avg.ci[1], med_obj$tau.ci[1]),
    Upper    = c(med_obj$d.avg.ci[2], med_obj$z.avg.ci[2], med_obj$tau.ci[2]),
    p_value  = c(med_obj$d.avg.p, med_obj$z.avg.p, med_obj$tau.p)
  )
}

med_results <- bind_rows(
  extract_med(med_us, "United States"),
  extract_med(med_cn, "China")
) %>%
  mutate(
    Effect    = factor(Effect,
                       levels = c("Total Effect", "ADE\n(Direct)", "ACME\n(Indirect)")),
    Donor     = factor(Donor, levels = c("United States", "China")),
    Sig       = case_when(
      p_value < 0.01 ~ "p < .01",
      p_value < 0.05 ~ "p < .05",
      p_value < 0.10 ~ "p < .10",
      TRUE           ~ "n.s."
    )
  )

fig5 <- ggplot(med_results,
               aes(x = Estimate, y = Effect, colour = Donor, shape = Sig)) +
  geom_vline(xintercept = 0, linetype = "dashed",
             colour = "grey55", linewidth = 0.8) +
  geom_errorbarh(aes(xmin = Lower, xmax = Upper),
                 height = 0.25, linewidth = 0.9,
                 position = position_dodgev(height = 0.5)) +
  geom_point(size = 3.5,
             position = position_dodgev(height = 0.5)) +
  scale_colour_manual(values = pal_donor) +
  scale_shape_manual(
    values = c("p < .01" = 16, "p < .05" = 17,
               "p < .10" = 15, "n.s." = 1),
    name = "Significance"
  ) +
  facet_wrap(~Donor) +
  labs(
    title    = "Figure 5. Mediation Analysis: Aid → Favorability → UN Vote Alignment",
    subtitle = "ACME = average causal mediation effect (indirect path a×b)\n95% CI from 1000 quasi-Bayesian simulations",
    x        = "Effect Size",
    y        = NULL,
    colour   = "Donor"
  ) +
  theme_paper +
  theme(legend.position = "right")

ggsave("h3_fig_mediation_results.png", fig5, width = 11, height = 5.5, dpi = 200)
message("✓ Mediation results figure saved.")

message("\n=== H3 Analysis complete ===\n")

