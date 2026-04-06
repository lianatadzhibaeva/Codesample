********************************************************************************
* File    : 04_analysis.do
* Purpose : Descriptive statistics, trend plots, main DiD estimates, and
*           robustness / placebo checks.
* Input   : $analysis/final_data.dta
* Output  : PNG figures in $figures and TEX tables in $tables
********************************************************************************

use "$analysis/final_data.dta", clear

* Descriptive checks
tab year
tab treated_hh
sum share_educ share_essential educ_spend total_spend

*===============================================================================
* 4.1 Summary statistics: treated vs control in the pre-period
*===============================================================================
preserve
    keep if year <= 2013
    estpost tabstat share_educ educ_spend total_spend, ///
        by(treated_hh) stat(mean sd n)
    esttab using "$tables/summary_pre.tex", ///
        replace cells("mean(fmt(4)) sd(fmt(4)) count(fmt(0))") ///
        title("Summary statistics (pre-treatment period)") ///
        nonumber nomtitle label
restore

*===============================================================================
* 4.2 Pre-treatment trends (share)
*===============================================================================
preserve
    keep if year <= 2013
    collapse (mean) share_educ, by(year treated_hh)

    twoway ///
        (line share_educ year if treated_hh == 1, lwidth(medthick)) ///
        (line share_educ year if treated_hh == 0, lpattern(dash) lwidth(medthick)), ///
        xlabel(2011 2012 2013) ///
        legend(label(1 "Treated households") label(2 "Control households") pos(6) rows(1)) ///
        ytitle("Education spending share") ///
        xtitle("Year") ///
        title("Pre-treatment trends in education spending share") ///
        graphregion(color(white))

    graph export "$figures/pretrends_share_educ.png", replace
restore

*===============================================================================
* 4.3 Full trends (share)
*===============================================================================
preserve
    collapse (mean) share_educ, by(year treated_hh)

    twoway ///
        (line share_educ year if treated_hh == 1, lwidth(medthick)) ///
        (line share_educ year if treated_hh == 0, lpattern(dash) lwidth(medthick)), ///
        xlabel(2011 2012 2013 2016 2019) ///
        legend(label(1 "Treated households") label(2 "Control households") pos(6) rows(1)) ///
        ytitle("Education spending share") ///
        xtitle("Year") ///
        title("Education spending share over time") ///
        graphregion(color(white))

    graph export "$figures/trends_share_educ.png", replace
restore

*===============================================================================
* 4.4 Full trends (levels)
*===============================================================================
preserve
    collapse (mean) educ_spend, by(year treated_hh)

    twoway ///
        (line educ_spend year if treated_hh == 1, lwidth(medthick)) ///
        (line educ_spend year if treated_hh == 0, lpattern(dash) lwidth(medthick)), ///
        xlabel(2011 2012 2013 2016 2019) ///
        legend(label(1 "Treated households") label(2 "Control households") pos(6) rows(1)) ///
        ytitle("Mean education spending") ///
        xtitle("Year") ///
        title("Education spending in levels over time") ///
        graphregion(color(white))

    graph export "$figures/trends_educ_spend.png", replace
restore

*===============================================================================
* 4.5 Main DiD regressions
*   Y_ht = alpha_h + gamma_t + beta (Treated_h x Post_t) + eps_ht
*   SEs clustered at the household level.
*===============================================================================
eststo clear

eststo m1: reghdfe share_educ      i.treated_hh##i.post, absorb(hhid year) cluster(hhid)
eststo m2: reghdfe educ_spend      i.treated_hh##i.post, absorb(hhid year) cluster(hhid)
eststo m3: reghdfe total_spend     i.treated_hh##i.post, absorb(hhid year) cluster(hhid)
eststo m4: reghdfe share_essential i.treated_hh##i.post, absorb(hhid year) cluster(hhid)

esttab m1 m2 m3 m4 using "$tables/did_main.tex", ///
    replace se label ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    keep(1.treated_hh 1.treated_hh#1.post) ///
    stats(N r2, labels("Observations" "R-squared")) ///
    title("Difference-in-differences estimates")

esttab m1 m2 m3 m4, ///
    se label ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    keep(1.treated_hh 1.treated_hh#1.post) ///
    stats(N r2, labels("Observations" "R-squared")) ///
    title("Difference-in-differences estimates")

*===============================================================================
* 4.6 Formal pre-trend test
*   Regress the outcome on year x treated interactions in the pre-period only.
*   Significant interactions would indicate non-parallel trends.
*===============================================================================
eststo clear
eststo pretrend: reghdfe share_educ i.year##i.treated_hh if year <= 2013, ///
    absorb(hhid) cluster(hhid)

esttab pretrend using "$tables/pretrend_test.tex", ///
    replace se label ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    stats(N r2, labels("Observations" "R-squared")) ///
    title("Formal pre-trend test")

*===============================================================================
* 4.7 Placebo test
*   Use 2013 as a fake post-period, restricting to the pre-treatment sample.
*   A significant effect here would further cast doubt on parallel trends.
*===============================================================================
gen placebo_post = (year >= 2013) if year <= 2013

eststo clear
eststo placebo: reghdfe share_educ i.treated_hh##i.placebo_post if year <= 2013, ///
    absorb(hhid year) cluster(hhid)

esttab placebo using "$tables/placebo.tex", ///
    replace se label ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    keep(1.treated_hh 1.treated_hh#1.placebo_post) ///
    stats(N r2, labels("Observations" "R-squared")) ///
    title("Placebo test (2013 as fake post-period)")

*===============================================================================
* 4.8 Robustness: treated-specific linear trend
*   Allow treated households to follow their own linear time trend. If the
*   baseline effect is driven by differential trends rather than the shock,
*   the interaction coefficient should shrink or disappear.
*===============================================================================
gen year_c = year - 2011

eststo clear
eststo m1: reghdfe share_educ      i.treated_hh##i.post c.year_c#i.treated_hh, absorb(hhid) cluster(hhid)
eststo m2: reghdfe educ_spend      i.treated_hh##i.post c.year_c#i.treated_hh, absorb(hhid) cluster(hhid)
eststo m3: reghdfe total_spend     i.treated_hh##i.post c.year_c#i.treated_hh, absorb(hhid) cluster(hhid)
eststo m4: reghdfe share_essential i.treated_hh##i.post c.year_c#i.treated_hh, absorb(hhid) cluster(hhid)

esttab m1 m2 m3 m4 using "$tables/did_trend.tex", ///
    replace se label ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    keep(1.treated_hh 1.treated_hh#1.post 1.treated_hh#c.year_c) ///
    stats(N r2, labels("Observations" "R-squared")) ///
    title("DiD estimates with treated-specific linear trend")

esttab m1 m2 m3 m4, ///
    se label ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    keep(1.treated_hh 1.treated_hh#1.post 1.treated_hh#c.year_c) ///
    stats(N r2, labels("Observations" "R-squared")) ///
    title("DiD estimates with treated-specific linear trend")

*===============================================================================
* 4.9 Intensity specification
*   Continuous treatment: pre-period migrant count interacted with post.
*===============================================================================
eststo clear
eststo intensity: reghdfe share_educ c.pre_migrant##i.post, ///
    absorb(hhid year) cluster(hhid)

esttab intensity using "$tables/did_intensity.tex", ///
    replace se label ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    stats(N r2, labels("Observations" "R-squared")) ///
    title("Intensity specification: continuous pre-period exposure")
