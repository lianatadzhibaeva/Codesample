********************************************************************************
* File    : 01_build_migrants.do
* Purpose : Build a long panel of household migration records by appending the
*           migration module (hh6 / hh6a / hh6b) across LiK survey waves.
* Input   : $data/Life_in_Kyrgyzstan_Study/IDSC_repository/stata/data<year>/
*           household/<file>.dta
* Output  : $analysis/migrants.dta  (hhid, year, h601)
*
* Note on file naming: the LiK distributes the migration module under different
* file names across waves, and the 2019 survey wave lives in the `data2022`
* folder in the IDSC repository. The mapping below is hard-coded to that
* convention.
********************************************************************************

*-------------------------------------------------------------------------------
* Wave -> (folder, file) mapping
*-------------------------------------------------------------------------------
local waves     2011 2012 2013 2016 2019
local folder_2011 "data2011"
local folder_2012 "data2012"
local folder_2013 "data2013"
local folder_2016 "data2016"
local folder_2019 "data2022"   // 2019 wave is released under data2022
local file_2011   "hh6"
local file_2012   "hh6"
local file_2013   "hh6a"
local file_2016   "hh6b"
local file_2019   "hh6a"

*-------------------------------------------------------------------------------
* Append all waves
*-------------------------------------------------------------------------------
tempfile migrants_all
local first = 1

foreach y of local waves {
    local folder "`folder_`y''"
    local file   "`file_`y''"

    use "$data/Life_in_Kyrgyzstan_Study/IDSC_repository/stata/`folder'/household/`file'.dta", clear
    keep hhid h601
    gen year = `y'

    if `first' == 1 {
        save `"`migrants_all'"', replace
        local first = 0
    }
    else {
        append using `"`migrants_all'"'
        save `"`migrants_all'"', replace
    }
}

*-------------------------------------------------------------------------------
* Save final migrants file
*-------------------------------------------------------------------------------
use `"`migrants_all'"', clear
order hhid year h601
sort  hhid year
save "$analysis/migrants.dta", replace
