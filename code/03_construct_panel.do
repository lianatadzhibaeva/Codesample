********************************************************************************
* File    : 03_construct_panel.do
* Purpose : Build the household-year analysis panel. Define treatment from
*           pre-period migrant exposure, aggregate item-level spending to
*           household-year outcomes, and save the final dataset.
* Input   : $analysis/migrants.dta
*           $analysis/spendings.dta
* Output  : $analysis/migrants_panel.dta
*           $analysis/data_merged.dta
*           $analysis/final_data.dta
********************************************************************************

*===============================================================================
* 1) Household-year migrant panel and treatment definition
*===============================================================================
use "$analysis/migrants.dta", clear

* Sanity check: h601 should be constant within household-year in the raw data
bys hhid year: egen _max = max(h601)
bys hhid year: egen _min = min(h601)
assert _max == _min | missing(_max) | missing(_min)
drop _max _min

* Collapse to one observation per household-year
collapse (max) h601, by(hhid year)
gen hasmigrant = (h601 > 0) & !missing(h601)

* Treatment = pre-period (2011-2013) migrant exposure. This is fixed at the
* household level so treated status is pre-determined relative to the 2014 shock.
gen pre_migrant = h601 if inrange(year, 2011, 2013)
bys hhid: egen pre_migrant_hh = max(pre_migrant)
drop pre_migrant
rename pre_migrant_hh pre_migrant

* IMPORTANT: in Stata, `missing > 0` returns 1 (missing is +infinity). Guard
* against that so households never observed in the pre-period are not mis-coded
* as treated.
gen treated_hh = (pre_migrant > 0) & !missing(pre_migrant)

isid hhid year
save "$analysis/migrants_panel.dta", replace

*===============================================================================
* 2) Merge spending data with migrant panel
*===============================================================================
use "$analysis/spendings.dta", clear

merge m:1 hhid year using "$analysis/migrants_panel.dta"
* Households that appear in spending but not in the migrant file are assumed
* non-migrant (control).
replace hasmigrant = 0 if _merge == 1
replace treated_hh = 0 if _merge == 1 | missing(treated_hh)
drop _merge
save "$analysis/data_merged.dta", replace

*===============================================================================
* 3) Construct outcomes at item level, THEN collapse to household-year
*
* NOTE: outcomes must be built while the data is still at the item level.
* Collapsing first (e.g., `bys hhid year: keep if _n == 1`) and only then
* defining category indicators would drop all items except the first per
* household-year and silently break any category-based aggregate.
*===============================================================================
use "$analysis/data_merged.dta", clear

* Annualize reported spending (h404 = 1 monthly, 2 annual)
gen h403_year = cond(h404 == 1, h403 * 12, cond(h404 == 2, h403, .))

* Category indicators (codes from the LiK non-food module)
gen educ      = (nfood == 21)
gen essential = inlist(nfood, 6, 7, 10, 11, 12, 13, 14, 15)

* Household-year aggregates
bys hhid year: egen total_spend     = total(h403_year)
bys hhid year: egen educ_spend      = total(h403_year * educ)
bys hhid year: egen essential_spend = total(h403_year * essential)

gen share_educ      = educ_spend      / total_spend
gen share_essential = essential_spend / total_spend

gen post = (year >= 2016)

* Collapse to one row per household-year, keeping only the variables we need
bys hhid year: keep if _n == 1
keep hhid year treated_hh pre_migrant hasmigrant post ///
     total_spend educ_spend essential_spend share_educ share_essential

isid hhid year
save "$analysis/final_data.dta", replace
