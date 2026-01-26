clear all

cd E:\06博士论文\002出国交流\02cliamte_economy\output_ld_new
insheet using GDPpc.csv ,clear

*****************************************
*      panel regression for GDPpc       *
*****************************************

*Variable generation
drop if missing(gdppc, tem, pre,tem_era, pre_era, pop)

encode subcontinent, gen(subc)
encode countsub, gen(countsub1)
encode country, gen(country1)
xtset gid_nmbr year 

gen lngdp = log(gdppc)
gen dgdp = d.lngdp

gen lnpop = log(pop)
gen dpop = d.lnpop

gen dT = d.tem
gen dP = d.pre

gen T2 = tem*tem
gen P2 = pre*pre

gen dT2= d.T2
gen dP2 = d.P2

gen year2=year*year


egen gdppc_mean = mean(gdppc), by(gid_nmbr)
pctile pct = gdppc_mean, nq(100)
gen poor = 0 if gdppc_mean <= pct[50]
replace poor = 1 if gdppc_mean >pct[50] 
tempname memhold
postfile `memhold' str8 regname lag sumT2_0_est sumT2_0_se sumT_0_est sumT_0_se sumT2_1_est sumT2_1_se sumT_1_est sumT_1_se using results_temp, replace

local lags  10
local reg = 0

foreach lag of local lags {
    local reg = 1 + `reg'
    local T2var c.tem#c.tem
    local P2var c.pre#c.pre

    forvalue l=1/`lag' {
        local T2var `T2var' c.l`l'.tem#c.l`l'.tem
        local P2var `P2var' c.l`l'.pre#c.l`l'.pre
    }

    reghdfe dgdp poor#( c.l(0/`lag').tem `T2var' ///
            c.l(0/`lag').pre `P2var' c.dpop) [aweight=weight], ///
            absorb(i.gid_nmbr i.countsub1#i.year) vce(cluster country1)

    local sumT_0 0
    local sumT2_0 0
	local sumT_1 0
    local sumT2_1 0

    forvalues l = 0/`lag' {
        local sumT2_0 `sumT2_0' + _b[0.poor#cL`l'.tem#cL`l'.tem]
        local sumT_0  `sumT_0'  + _b[0.poor#cL`l'.tem]
    }
	
	forvalues l = 0/`lag' {
        local sumT2_1 `sumT2_1' + _b[1.poor#cL`l'.tem#cL`l'.tem]
        local sumT_1  `sumT_1'  + _b[1.poor#cL`l'.tem]
    }

    qui  lincom `sumT2_0'
    local sumT2_0_est = r(estimate)
    local sumT2_0_se  = r(se)

    qui lincom `sumT_0'
    local sumT_0_est = r(estimate)
    local sumT_0_se  = r(se)
	
	qui lincom `sumT2_1'
    local sumT2_1_est = r(estimate)
    local sumT2_1_se  = r(se)

    qui lincom `sumT_1'
    local sumT_1_est = r(estimate)
    local sumT_1_se  = r(se)
	
	lincom 	`sumT_0'+2*25*`sumT2_0'
    
	local regname = "reg`reg'"

        post `memhold' ("`regname'") (`lag') (`sumT2_0_est') (`sumT2_0_se') (`sumT_0_est') (`sumT_0_se') (`sumT2_1_est') (`sumT2_1_se') (`sumT_1_est') (`sumT_1_se')
  
}

postclose `memhold'

use results_temp, clear

export delimited using "02table\Table9.csv", replace
