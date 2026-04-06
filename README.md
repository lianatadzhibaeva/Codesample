# Migration Exposure and Household Spending Responses to the 2014 Russia Shock

Difference-in-differences analysis of how households with pre-existing migration
exposure in Kyrgyzstan adjusted their spending after the 2014 Russia shock, which
sharply depreciated the Russian ruble and reduced remittance income for households
with migrant workers in Russia.

This repository contains the Stata code for an applied econometrics term paper
(Humboldt University of Berlin). The main outcome is the share of education
spending in total non-food expenditure, motivated by the literature on household
responses to income shocks and the trade-off between consumption smoothing and
human capital investment (Jacoby and Skoufias, 1997).

## Data

Household-level panel data from the [Life in Kyrgyzstan Study](https://lifeinkyrgyzstan.org/),
waves 2011, 2012, 2013, 2016, and 2019 (the 2019 wave is distributed in the
`data2022` folder in the IDSC repository). Access requires registration; the raw
`.dta` files are not included here.

To replicate, download the LiK data and place the wave folders under
`raw_data/Life_in_Kyrgyzstan_Study/IDSC_repository/stata/`, then set the `root`
global in `code/00_master.do` to the local repo path.

The analysis uses two modules per wave:
- `hh6` (migration): indicator of household migrants
- `hh4b` (non-food expenditures): item-level spending with reporting period

## Methodology

Two-way fixed effects difference-in-differences:

$$Y_{ht} = \alpha_h + \gamma_t + \beta (\text{Migrant}_h \times \text{Post}_t) + \varepsilon_{ht}$$

Treatment is defined on pre-period exposure: a household is treated if it had at
least one migrant in 2011–2013. `Post` equals one for 2016 and 2019 (after the
2014 shock). Standard errors are clustered at the household level. Household and
year fixed effects are included throughout.

In addition to the baseline DiD, the analysis includes:
- pre-trend plots (levels and shares)
- a formal pre-trend test on the pre-period sample
- a placebo test using 2013 as a fake treatment date
- a robustness specification allowing treated households to follow a separate
  linear time trend
- an intensity specification using pre-period migrant count instead of a binary
  indicator

## Main findings

The baseline DiD shows a negative and significant effect on the share of
education spending (around −1 percentage point on a mean of ~2%), consistent
with households reallocating resources after the remittance shock.

**However, the identifying assumptions are not fully satisfied.** Pre-trends are
visibly non-parallel, the formal pre-trend test rejects, the 2013 placebo
yields a significant effect, and allowing treated households to follow their own
linear trend makes the main effect disappear. The results should therefore be
read as suggestive of differential spending dynamics, not as causal estimates of
the effect of the shock. This limitation is a key takeaway of the exercise and
is discussed in the paper.

## Repository structure

```
migration-spending-did/
├── README.md
├── code/
│   ├── 00_master.do          # runs everything end to end
│   ├── 01_build_migrants.do  # append migration modules across waves
│   ├── 02_build_spendings.do # append expenditure modules across waves
│   ├── 03_construct_panel.do # merge, define treatment, build outcomes
│   └── 04_analysis.do        # DiD, pre-trend test, placebo, robustness
├── raw_data/                 # LiK data (not included, see above)
├── analysis/                 # intermediate .dta files (generated)
├── figures/                  # trend plots (generated)
└── tables/                   # regression tables (generated)
```

## Requirements

- Stata 17 or later
- `reghdfe` (Correia) — `ssc install reghdfe`
- `ftools` (dependency of reghdfe) — `ssc install ftools`
- `estout` — `ssc install estout`

## How to run

1. Install the packages above.
2. Place the LiK data under `raw_data/` as described.
3. Open `code/00_master.do`, set `global root` to your local repo path, and run.

All intermediate datasets, figures, and tables are written to `analysis/`,
`figures/`, and `tables/`.

## References

- Jacoby, H. G., and Skoufias, E. (1997). Risk, financial markets, and human
  capital in a developing country. *Review of Economic Studies*, 64(3), 311–335.
- Stepanyan, A., Roitman, A., Minasyan, G., Ostojic, D., and Epstein, N. (2016).
  The spillover effects of Russia's economic slowdown on neighboring countries.
  IMF Departmental Paper.
- Brück, T., Esenaliev, D., Kroeger, A., Kudebayeva, A., Mirkasimov, B., and
  Steiner, S. (2014). Household survey data for research on well-being and
  behavior in Central Asia. *Journal of Comparative Economics*, 42(3), 819–835.
