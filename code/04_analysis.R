################################################################################
# File    : 04_analysis.R
# Purpose : R version of the analysis in 04_analysis.do.
#           Loads the household-year panel built by the Stata pipeline
#           (01-03 *.do) and runs the same DiD specifications, robustness
#           checks, and trend plots using fixest and ggplot2.
# Input   : analysis/final_data.dta
# Output  : figures/*_R.png, tables/*_R.tex
################################################################################

# ---- Packages ----------------------------------------------------------------
# install.packages(c("haven", "fixest", "modelsummary", "ggplot2", "dplyr"))

library(haven)         # read Stata .dta files
library(dplyr)         # data manipulation
library(fixest)        # high-performance fixed-effects regressions
library(modelsummary)  # regression tables
library(ggplot2)       # plots

# ---- Paths -------------------------------------------------------------------
# Set this to the repo root before running.
root     <- "/path/to/migration-spending-did"
analysis <- file.path(root, "analysis")
figures  <- file.path(root, "figures")
tables   <- file.path(root, "tables")

dir.create(figures, showWarnings = FALSE, recursive = TRUE)
dir.create(tables,  showWarnings = FALSE, recursive = TRUE)

# ---- Load data ---------------------------------------------------------------
df <- read_dta(file.path(analysis, "final_data.dta")) |>
  mutate(
    treated_hh = as.integer(treated_hh),
    post       = as.integer(post),
    hhid       = as.integer(hhid),
    year       = as.integer(year)
  )

cat("N observations:", nrow(df), "\n")
cat("N households  :", length(unique(df$hhid)), "\n")
cat("Treated share :", mean(df$treated_hh, na.rm = TRUE), "\n")

# ==============================================================================
# 4.1 Summary statistics (pre-period)
# ==============================================================================
df |>
  filter(year <= 2013) |>
  group_by(treated_hh) |>
  summarise(
    mean_share_educ  = mean(share_educ,  na.rm = TRUE),
    sd_share_educ    = sd(share_educ,    na.rm = TRUE),
    mean_educ_spend  = mean(educ_spend,  na.rm = TRUE),
    sd_educ_spend    = sd(educ_spend,    na.rm = TRUE),
    mean_total_spend = mean(total_spend, na.rm = TRUE),
    sd_total_spend   = sd(total_spend,   na.rm = TRUE),
    n                = n()
  ) |>
  print()

# ==============================================================================
# 4.2 Pre-treatment trends (share)
# ==============================================================================
pre_trends_share <- df |>
  filter(year <= 2013) |>
  group_by(year, treated_hh) |>
  summarise(share_educ = mean(share_educ, na.rm = TRUE), .groups = "drop") |>
  mutate(group = factor(treated_hh,
                        levels = c(0, 1),
                        labels = c("Control households", "Treated households")))

p_pre <- ggplot(pre_trends_share,
                aes(x = year, y = share_educ, linetype = group, color = group)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = c(2011, 2012, 2013)) +
  scale_color_manual(values = c("Control households" = "grey40",
                                "Treated households" = "black")) +
  labs(
    title = "Pre-treatment trends in education spending share",
    x = "Year", y = "Education spending share",
    color = NULL, linetype = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave(file.path(figures, "pretrends_share_educ_R.png"),
       p_pre, width = 6, height = 4, dpi = 150)

# ==============================================================================
# 4.3 Full trends (share)
# ==============================================================================
trends_share <- df |>
  group_by(year, treated_hh) |>
  summarise(share_educ = mean(share_educ, na.rm = TRUE), .groups = "drop") |>
  mutate(group = factor(treated_hh,
                        levels = c(0, 1),
                        labels = c("Control households", "Treated households")))

p_full <- ggplot(trends_share,
                 aes(x = year, y = share_educ, linetype = group, color = group)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = c(2011, 2012, 2013, 2016, 2019)) +
  scale_color_manual(values = c("Control households" = "grey40",
                                "Treated households" = "black")) +
  labs(
    title = "Education spending share over time",
    x = "Year", y = "Education spending share",
    color = NULL, linetype = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave(file.path(figures, "trends_share_educ_R.png"),
       p_full, width = 6, height = 4, dpi = 150)

# ==============================================================================
# 4.4 Full trends (levels)
# ==============================================================================
trends_levels <- df |>
  group_by(year, treated_hh) |>
  summarise(educ_spend = mean(educ_spend, na.rm = TRUE), .groups = "drop") |>
  mutate(group = factor(treated_hh,
                        levels = c(0, 1),
                        labels = c("Control households", "Treated households")))

p_lvl <- ggplot(trends_levels,
                aes(x = year, y = educ_spend, linetype = group, color = group)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = c(2011, 2012, 2013, 2016, 2019)) +
  scale_color_manual(values = c("Control households" = "grey40",
                                "Treated households" = "black")) +
  labs(
    title = "Education spending in levels over time",
    x = "Year", y = "Mean education spending",
    color = NULL, linetype = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave(file.path(figures, "trends_educ_spend_R.png"),
       p_lvl, width = 6, height = 4, dpi = 150)

# ==============================================================================
# 4.5 Main DiD regressions
#   Y_ht = alpha_h + gamma_t + beta (Treated_h x Post_t) + eps_ht
#   SEs clustered at the household level.
# ==============================================================================
m1 <- feols(share_educ      ~ i(treated_hh, post, ref = 0) | hhid + year,
            cluster = ~hhid, data = df)
m2 <- feols(educ_spend      ~ i(treated_hh, post, ref = 0) | hhid + year,
            cluster = ~hhid, data = df)
m3 <- feols(total_spend     ~ i(treated_hh, post, ref = 0) | hhid + year,
            cluster = ~hhid, data = df)
m4 <- feols(share_essential ~ i(treated_hh, post, ref = 0) | hhid + year,
            cluster = ~hhid, data = df)

etable(m1, m2, m3, m4,
       headers = c("share_educ", "educ_spend", "total_spend", "share_essential"),
       title   = "Difference-in-differences estimates",
       file    = file.path(tables, "did_main_R.tex"),
       replace = TRUE)

etable(m1, m2, m3, m4,
       headers = c("share_educ", "educ_spend", "total_spend", "share_essential"))

# ==============================================================================
# 4.6 Formal pre-trend test
#   Year x treated interactions in the pre-period only.
# ==============================================================================
pretrend <- feols(share_educ ~ i(year, treated_hh, ref = 2011) | hhid,
                  cluster = ~hhid,
                  data = df |> filter(year <= 2013))

etable(pretrend,
       title   = "Formal pre-trend test",
       file    = file.path(tables, "pretrend_test_R.tex"),
       replace = TRUE)

# ==============================================================================
# 4.7 Placebo test (2013 as fake post-period within pre-treatment sample)
# ==============================================================================
df_pre <- df |>
  filter(year <= 2013) |>
  mutate(placebo_post = as.integer(year >= 2013))

placebo <- feols(share_educ ~ i(treated_hh, placebo_post, ref = 0) | hhid + year,
                 cluster = ~hhid, data = df_pre)

etable(placebo,
       title   = "Placebo test (2013 as fake post-period)",
       file    = file.path(tables, "placebo_R.tex"),
       replace = TRUE)

# ==============================================================================
# 4.8 Robustness: treated-specific linear trend
# ==============================================================================
df <- df |> mutate(year_c = year - 2011)

t1 <- feols(share_educ      ~ i(treated_hh, post, ref = 0) + i(treated_hh, year_c, ref = 0) | hhid,
            cluster = ~hhid, data = df)
t2 <- feols(educ_spend      ~ i(treated_hh, post, ref = 0) + i(treated_hh, year_c, ref = 0) | hhid,
            cluster = ~hhid, data = df)
t3 <- feols(total_spend     ~ i(treated_hh, post, ref = 0) + i(treated_hh, year_c, ref = 0) | hhid,
            cluster = ~hhid, data = df)
t4 <- feols(share_essential ~ i(treated_hh, post, ref = 0) + i(treated_hh, year_c, ref = 0) | hhid,
            cluster = ~hhid, data = df)

etable(t1, t2, t3, t4,
       headers = c("share_educ", "educ_spend", "total_spend", "share_essential"),
       title   = "DiD estimates with treated-specific linear trend",
       file    = file.path(tables, "did_trend_R.tex"),
       replace = TRUE)

etable(t1, t2, t3, t4,
       headers = c("share_educ", "educ_spend", "total_spend", "share_essential"))

# ==============================================================================
# 4.9 Intensity specification
#   Continuous treatment: pre-period migrant count interacted with post.
# ==============================================================================
intensity <- feols(share_educ ~ pre_migrant : post | hhid + year,
                   cluster = ~hhid, data = df)

etable(intensity,
       title   = "Intensity specification: continuous pre-period exposure",
       file    = file.path(tables, "did_intensity_R.tex"),
       replace = TRUE)

cat("\nAnalysis complete. Tables in:", tables, "\nFigures in:", figures, "\n")
