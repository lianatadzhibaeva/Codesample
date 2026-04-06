********************************************************************************
* File    : 02_build_spendings.do
* Purpose : Build a long panel of household non-food expenditure items by
*           appending module hh4b across LiK survey waves.
* Input   : $data/Life_in_Kyrgyzstan_Study/IDSC_repository/stata/data<year>/
*           household/hh4b.dta
* Output  : $analysis/spendings.dta  (hhid, year, nfood, h403, h404)
*
* Notes on harmonization:
*  - The category id variable differs across waves: `nitem` (2011),
*    `n4b` (2012, 2013, 2016), `nfitem` (2019). All are renamed to `nfood`.
*  - The reporting period is stored differently: in 2011 and 2012 there is an
*    auxiliary flag `h404` with the time-period variable in `h405`, so these
*    waves are filtered to h404==1 and h405 is renamed to h404. From 2013
*    onwards `h404` already holds the period indicator.
*  - h403 = reported spending, h404 = reporting period (1 = monthly, 2 = annual)
********************************************************************************

tempfile spendings_all
local first = 1

*-------------------------------------------------------------------------------
* 2011 wave
*-------------------------------------------------------------------------------
use "$data/Life_in_Kyrgyzstan_Study/IDSC_repository/stata/data2011/household/hh4b.dta", clear
keep hhid nitem h403 h404 h405
keep if h404 == 1
drop h404
rename h405 h404
rename nitem nfood
gen year = 2011
save `"`spendings_all'"', replace
local first = 0

*-------------------------------------------------------------------------------
* 2012 wave
*-------------------------------------------------------------------------------
use "$data/Life_in_Kyrgyzstan_Study/IDSC_repository/stata/data2012/household/hh4b.dta", clear
keep hhid n4b h403 h404 h405
keep if h404 == 1
drop h404
rename h405 h404
rename n4b nfood
gen year = 2012
append using `"`spendings_all'"'
save `"`spendings_all'"', replace

*-------------------------------------------------------------------------------
* 2013 and 2016 waves (same structure)
*-------------------------------------------------------------------------------
foreach y in 2013 2016 {
    use "$data/Life_in_Kyrgyzstan_Study/IDSC_repository/stata/data`y'/household/hh4b.dta", clear
    keep hhid n4b h403 h404
    rename n4b nfood
    gen year = `y'
    append using `"`spendings_all'"'
    save `"`spendings_all'"', replace
}

*-------------------------------------------------------------------------------
* 2019 wave (released in data2022)
*-------------------------------------------------------------------------------
use "$data/Life_in_Kyrgyzstan_Study/IDSC_repository/stata/data2022/household/hh4b.dta", clear
keep hhid nfitem h403 h404
rename nfitem nfood
gen year = 2019
append using `"`spendings_all'"'
save `"`spendings_all'"', replace

*-------------------------------------------------------------------------------
* Save final spendings file
*-------------------------------------------------------------------------------
use `"`spendings_all'"', clear
order hhid year nfood h403 h404
sort  hhid year
save "$analysis/spendings.dta", replace
