********************************************************************************
* Project : Migration Exposure and Household Spending Responses
*           to the 2014 Russia Shock — Evidence from Kyrgyzstan
* Author  : Liana Tadzhibaeva
* File    : 00_master.do
* Purpose : Run the full pipeline end to end.
********************************************************************************

version 17
clear all
set more off

*-------------------------------------------------------------------------------
* Paths — set `root` to your local repo path before running.
*-------------------------------------------------------------------------------
global root     "/path/to/migration-spending-did"

global code     "$root/code"
global data     "$root/raw_data"
global analysis "$root/analysis"
global figures  "$root/figures"
global tables   "$root/tables"

cap mkdir "$analysis"
cap mkdir "$figures"
cap mkdir "$tables"

*-------------------------------------------------------------------------------
* Required packages (install once, then comment out)
*-------------------------------------------------------------------------------
* ssc install reghdfe, replace
* ssc install ftools,  replace
* ssc install estout,  replace

*-------------------------------------------------------------------------------
* Pipeline
*-------------------------------------------------------------------------------
do "$code/01_build_migrants.do"
do "$code/02_build_spendings.do"
do "$code/03_construct_panel.do"
do "$code/04_analysis.do"

display as result "Pipeline complete."
