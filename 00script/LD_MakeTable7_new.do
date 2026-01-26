clear all

cd E:\06博士论文\002出国交流\02cliamte_economy\output_ld_new
insheet using GDPpc.csv ,clear

*****************************************
*      panel regression for GDPpc       *
*****************************************

*Variable generation
drop if missing(tem, pre,tem_era, pre_era)

encode subcontinent, gen(subc)
encode countsub, gen(countsub1)
encode country, gen(country1)
xtset gid_nmbr year 

replace pre_era=30.5*pre_era

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

egen gdppc_mean = mean(gdppc), by(gid_nmbr)
pctile pct = gdppc_mean, nq(100)
gen poor = 0 if gdppc_mean <= pct[50]
replace poor = 1 if gdppc_mean >pct[50] 

xtabond2 dgdp c.l(1/3).dgdp dT dT2 c.tem##c.tem dP dP2 c.pre##c.pre poor#c.(dT dT2 c.tem##c.tem dP dP2 c.pre##c.pre) dpop i.year i.subc#c.year  [aweight=weight], iv(dT dT2 c.tem##c.tem dP dP2 c.pre##c.pre  poor#c.(dT dT2 c.tem##c.tem dP dP2 c.pre##c.pre) i.year i.subc#c.year  dpop ) gmm(c.L(1/3).dgdp, l(4 6)  ) nolevel cluster(country1)  artests(3) or
estimates store r1

matrix results = J(24, 8, .) 
local index =1
local vars c.dT c.dT2 c.tem c.tem#c.tem c.dP c.dP2 c.pre c.pre#c.pre 
foreach var of local vars{
    lincom `var' + 1.poor#`var'
	matrix results[`index', 1] = _b[`var']
	
	matrix results[`index', 2] =  r(estimate)
	local index = `index'+1
	
	matrix results[`index', 1] = round(_se[`var'],0.0001)
	
    matrix results[`index', 2] =  round(r(se), 0.0001)
	local index = `index'+1
	
}

preserve
replace tem = tem_era
replace pre = pre_era
 
replace dT = d.tem_era
replace dP = d.pre_era

replace T2 = tem_era*tem_era
replace P2 = pre_era*pre_era

replace dT2= d.T2
replace dP2 = d.P2

xtabond2 dgdp c.l(1/3).dgdp dT dT2 c.tem##c.tem dP dP2 c.pre##c.pre poor#c.(dT dT2 c.tem##c.tem dP dP2 c.pre##c.pre) dpop i.year i.subc#c.year  [aweight=weight], iv(dT dT2 c.tem##c.tem dP dP2 c.pre##c.pre  poor#c.(dT dT2 c.tem##c.tem dP dP2 c.pre##c.pre) i.year i.subc#c.year  dpop ) gmm(c.L(1/3).dgdp, l(4 6)  ) nolevel cluster(country1)  artests(3) or
estimates store r2
restore

local index =1
local vars c.dT c.dT2 c.tem c.tem#c.tem c.dP c.dP2 c.pre c.pre#c.pre 
foreach var of local vars{
    lincom `var' + 1.poor#`var'
	matrix results[`index', 3] = _b[`var']
	
	matrix results[`index', 4] =  r(estimate)
	local index = `index'+1
	
	matrix results[`index', 3] = round(_se[`var'],0.0001)
	
    matrix results[`index', 4] =  round(r(se), 0.0001)
	local index = `index'+1
	
}


preserve
local deltaT = 4
gen N_usd = gdppc

sum year
gen y_min = r(min)

gen period = floor((year - y_min)/`deltaT')

collapse(mean) gdppc tem pre tem_era pre_era pop ///
   (first) countsub1 country1 subc weight ///
   (count) N_usd  ///
   , by(gid_nmbr period)

*keep if N_usd >1

sum period
gen T_max = r(max)

gen year = period
xtset gid_nmbr year 
gen lngdp = log(gdppc)
gen dgdp = lngdp-l2.lngdp

gen lnpop = log(pop)
gen dpop = lnpop-l2.lnpop

gen dT = tem-l2.tem
gen dP = pre-l2.pre

gen T2 = tem*tem
gen P2 = pre*pre

gen dT2= T2-l2.T2
gen dP2 =P2-l2.P2

egen gdppc_mean = mean(gdppc), by(gid_nmbr)
pctile pct = gdppc_mean, nq(100)
gen poor = 0 if gdppc_mean <= pct[50]
replace poor = 1 if gdppc_mean >pct[50] 

xtabond2 dgdp l(1/1).dgdp c.dT c.dT2 c.tem##c.tem c.l.tem##c.l.tem c.dP c.dP2 c.pre##c.pre c.l.pre##c.l.pre poor#c.(c.dT c.dT2 c.tem##c.tem c.l.tem##c.l.tem c.dP c.dP2 c.pre##c.pre c.l.pre##c.l.pre) dpop i.year i.subc#c.year  [aweight=weight], iv(c.dT c.dT2 c.tem##c.tem  c.l.tem##c.l.tem c.dP c.dP2 c.pre##c.pre c.l.pre##c.l.pre poor#c.(c.dT c.dT2 c.tem##c.tem c.l.tem##c.l.tem c.dP c.dP2 c.pre##c.pre c.l.pre##c.l.pre) dpop i.year i.subc#c.year ) gmm(l(1/1).dgdp, l(4 .))  nolevel cluster(country1) artests(4) or
estimates store r3

local index =1
local vars c.dT c.dT2 c.tem c.tem#c.tem c.dP c.dP2 c.pre c.pre#c.pre c.l.tem c.l.tem#c.l.tem c.l.pre c.l.pre#c.l.pre
foreach var of local vars{
    lincom `var' + 1.poor#`var'
	matrix results[`index', 5] = _b[`var']
	
	matrix results[`index', 6] =  r(estimate)
	local index = `index'+1
	
	matrix results[`index', 5] = round(_se[`var'],0.0001)
	
    matrix results[`index', 6] =  round(r(se), 0.0001)
	local index = `index'+1
	
}

replace tem = tem_era
replace pre = pre_era

replace dT = tem_era-l2.tem_era
replace dP = pre_era-l2.pre_era

replace T2 = tem_era*tem_era
replace P2 = pre_era*pre_era

replace dT2= T2-l2.T2
replace dP2 =P2-l2.P2

xtabond2 dgdp l(1/1).dgdp c.dT c.dT2 c.tem##c.tem c.l.tem##c.l.tem c.dP c.dP2 c.pre##c.pre c.l.pre##c.l.pre poor#c.(c.dT c.dT2 c.tem##c.tem c.l.tem##c.l.tem c.dP c.dP2 c.pre##c.pre c.l.pre##c.l.pre) dpop i.year i.subc#c.year  [aweight=weight], iv(c.dT c.dT2 c.tem##c.tem  c.l.tem##c.l.tem c.dP c.dP2 c.pre##c.pre c.l.pre##c.l.pre poor#c.(c.dT c.dT2 c.tem##c.tem c.l.tem##c.l.tem c.dP c.dP2 c.pre##c.pre c.l.pre##c.l.pre) dpop i.year i.subc#c.year ) gmm(l(1/1).dgdp, l(4 .))  nolevel cluster(country1) artests(4) or
estimates store r4
restore

local index =1
local vars c.dT c.dT2 c.tem c.tem#c.tem c.dP c.dP2 c.pre c.pre#c.pre c.l.tem c.l.tem#c.l.tem c.l.pre c.l.pre#c.l.pre
foreach var of local vars{
    lincom `var' + 1.poor#`var'
	matrix results[`index', 7] = _b[`var']
	
	matrix results[`index', 8] =  r(estimate)
	local index = `index'+1
	
	matrix results[`index', 7] = round(_se[`var'],0.0001)
	
    matrix results[`index', 8] =  round(r(se), 0.0001)
	local index = `index'+1
	
}

local vars c.dT c.dT2 c.tem c.tem#c.tem c.dP c.dP2 c.pre c.pre#c.pre c.l.tem c.l.tem#c.l.tem c.l.pre c.l.pre#c.l.pre
local row_names ""
foreach var of local vars {
    local clean_name = subinstr("`var'", "c.", "", .)
    local row_names "`row_names' `clean_name' `clean_name'_se"
}
matrix rownames results = `row_names'


esttab matrix(results,fmt(%9.3g)) using "02table\Table7.rtf",  se(%5.4f)  replace

esttab r1 r2 r3 r4 using "02table\Table7.rtf", star(* .1 ** .05  *** .01) nogap nonumber replace se(%5.4f) r2 ar2 aic(%10.4f) bic(%10.4f)