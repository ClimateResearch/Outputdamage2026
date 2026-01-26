clear all

cd E:\06博士论文\002出国交流\02cliamte_economy\output_ld_new
insheet using GDPpc.csv ,clear

*****************************************
*      panel regression for GDPpc       *
*****************************************

*Variable generation
drop if missing(tem ,pre , pop)
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
gen year2 = year*year

gen T2 = tem*tem
gen P2 = pre*pre

gen dT2= d.T2
gen dP2 = d.P2

egen gdppc_mean = mean(gdppc), by(gid_nmbr)
pctile pct = gdppc_mean, nq(100)
gen poor = 0 if gdppc_mean <= pct[50]
replace poor = 1 if gdppc_mean >pct[50] 

matrix results = J(16, 8, .) 
reghdfe dgdp c.l(1/3).dgdp poor#c.(dT dT2 c.tem##c.tem dP dP2 c.pre##c.pre) dpop  [aweight=weight], absorb(i.year i.gid_nmbr i.subc#c.year) cluster(country1)
estimates store r1
lincom 0.poor#c.tem+2*20*0.poor#c.tem#c.tem
lincom 1.poor#c.tem+2*20*1.poor#c.tem#c.tem

local index =1
local vars dT dT2 tem c.tem#c.tem dP dP2 pre c.pre#c.pre 
foreach var of local vars{
	matrix results[`index', 1] = _b[0.poor#c.`var']
	
	matrix results[`index', 2] = _b[1.poor#c.`var']
	local index = `index'+1
	
	matrix results[`index', 1] = round(_se[0.poor#c.`var'],0.0001)
	
    matrix results[`index', 2] = round(_se[1.poor#c.`var'],0.0001)
	local index = `index'+1
	
}


xtabond2 dgdp c.l(1/3).dgdp dT dT2 c.tem##c.tem dP dP2 c.pre##c.pre poor#c.(dT dT2 c.tem##c.tem dP dP2 c.pre##c.pre) dpop i.year i.subc#c.year  [aweight=weight], iv(dT dT2 c.tem##c.tem dP dP2 c.pre##c.pre  poor#c.(dT dT2 c.tem##c.tem dP dP2 c.pre##c.pre) i.year i.subc#c.year  dpop ) gmm(c.L(1/3).dgdp, l(4 6)  ) nolevel cluster(country1)  artests(3) or
estimates store r2
lincom c.tem+2*20*c.tem#c.tem
lincom c.tem+1.poor#c.tem+2*20*(c.tem#c.tem+1.poor#c.tem#c.tem)

local index =1
local vars dT dT2 tem c.tem#c.tem dP dP2 pre c.pre#c.pre 
foreach var of local vars{
    lincom `var' + 1.poor#`var'
	matrix results[`index', 3] = _b[`var']
	
	matrix results[`index', 4] =  r(estimate)
	local index = `index'+1
	
	matrix results[`index', 3] = round(_se[`var'],0.0001)
	
    matrix results[`index', 4] =  round(r(se), 0.0001)
	local index = `index'+1
	
}

xtabond2 dgdp c.l(1/5).dgdp dT dT2 c.tem##c.tem dP dP2 c.pre##c.pre poor#c.(dT dT2 c.tem##c.tem dP dP2 c.pre##c.pre) dpop i.year i.subc#c.year  [aweight=weight], iv(dT dT2 c.tem##c.tem dP dP2 c.pre##c.pre  poor#c.(dT dT2 c.tem##c.tem dP dP2 c.pre##c.pre) i.year i.subc#c.year  dpop ) gmm(c.L(1/5).dgdp, l(6 9) ) nolevel cluster(country1)  artests(3) or
estimates store r3
lincom c.tem+2*20*c.tem#c.tem
lincom c.tem+1.poor#c.tem+2*20*(c.tem#c.tem+1.poor#c.tem#c.tem)

local index =1
local vars dT dT2 tem c.tem#c.tem dP dP2 pre c.pre#c.pre 
foreach var of local vars{
    lincom `var' + 1.poor#`var'
	matrix results[`index', 5] = _b[`var']
	
	matrix results[`index', 6] =  r(estimate)
	local index = `index'+1
	
	matrix results[`index', 5] = round(_se[`var'],0.0001)
	
    matrix results[`index', 6] = round(r(se), 0.0001)
	local index = `index'+1
	
}

xtabond2 dgdp c.l(1/8).dgdp dT dT2 c.tem##c.tem dP dP2 c.pre##c.pre poor#c.(dT dT2 c.tem##c.tem dP dP2 c.pre##c.pre) dpop i.year i.subc#c.year  [aweight=weight], iv(dT dT2 c.tem##c.tem dP dP2 c.pre##c.pre  poor#c.(dT dT2 c.tem##c.tem dP dP2 c.pre##c.pre) i.year i.subc#c.year  dpop ) gmm(c.L(1/8).dgdp, l(9 11) ) nolevel cluster(country1)  artests(3) or
estimates store r4
lincom c.tem+2*20*c.tem#c.tem
lincom c.tem+1.poor#c.tem+2*20*(c.tem#c.tem+1.poor#c.tem#c.tem)

local index =1
local vars dT dT2 tem c.tem#c.tem dP dP2 pre c.pre#c.pre 
foreach var of local vars{
    lincom `var' + 1.poor#`var'
	matrix results[`index', 7] = _b[`var']
	
	matrix results[`index', 8] =  r(estimate)
	local index = `index'+1
	
	matrix results[`index', 7] =  round(_se[`var'],0.0001)
	
    matrix results[`index', 8] =  round(r(se), 0.0001)
	local index = `index'+1
	
}

local vars dT dT2 tem c.tem#c.tem dP dP2 pre c.pre#c.pre 
local row_names ""
foreach var of local vars {
    local clean_name = subinstr("`var'", "c.", "", .)
    local row_names "`row_names' `clean_name' `clean_name'_se"
}
matrix rownames results = `row_names'


esttab matrix(results,fmt(%9.3g)) using "02table\Table3.rtf",  se(%5.4f)  replace


esttab r1 r2 r3 r4 using "02table\Table3.rtf", star(* .1 ** .05  *** .01) nogap nonumber replace se(%5.4f) r2 ar2 aic(%10.4f) bic(%10.4f)