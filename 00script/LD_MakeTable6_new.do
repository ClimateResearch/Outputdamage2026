clear all

cd "~"
insheet using 01data\gdppc_world_bank_merge.csv ,clear

*********Make Table 6*************

encode subcontinent, gen(subc)
encode country, gen(country1)
xtset country1 year 

gen lngdp = log(gdppc)
gen dgdp = d.lngdp

gen lngdp_wb = log(gdppc_wb)
gen dgdp_wb = d.lngdp_wb

gen lnpop = log(pop)
gen dpop = d.lnpop

gen dT = d.tem
gen dP = d.pre
gen year2 = year*year

gen T2 = tem*tem
gen P2 = pre*pre

gen dT2= d.T2
gen dP2 = d.P2

egen gdppc_mean = mean(gdppc), by(country) 
pctile pct = gdppc_mean, nq(100)
gen poor = 0 if gdppc_mean <= pct[50]
replace poor = 1 if gdppc_mean >pct[50] 


preserve
drop if missing(dgdp, dgdp_wb, tem ,pre,pop)

reghdfe  dgdp l(1/3).dgdp poor#c.(c.dT c.dT2 c.tem##c.tem  c.dP c.dP2 c.pre##c.pre ) dpop , absorb(i.poor#i.year i.country1 i.subc#c.year ) cluster(country1)
estimates store r1

matrix results = J(30, 8, .) 
local index =1
local vars c.dT c.dT2 c.tem c.tem#c.tem c.dP c.dP2 c.pre c.pre#c.pre 
foreach var of local vars{
	matrix results[`index', 1] = _b[0.poor#`var']
	
	matrix results[`index', 2] = _b[1.poor#`var']
	local index = `index'+1
	
	matrix results[`index', 1] = round(_se[0.poor#`var'],0.0001)
	
    matrix results[`index', 2] = round(_se[1.poor#`var'], 0.0001)
	local index = `index'+1
	
}

local index= `index'+8
forvalues i=1/3{
	matrix results[`index', 1] = _b[L`i'.dgdp]
local index = `index'+1
matrix results[`index', 1] = round(_se[L`i'.dgdp], 0.0001)
local index = `index'+1
}


reghdfe  dgdp_wb l(1/3).dgdp_wb poor#c.(c.dT c.dT2 c.tem##c.tem  c.dP c.dP2 c.pre##c.pre ) dpop, absorb(i.poor#i.year i.country1 i.subc#c.year ) cluster(country1)
estimates store r2
local index =1
local vars c.dT c.dT2 c.tem c.tem#c.tem c.dP c.dP2 c.pre c.pre#c.pre 
foreach var of local vars{
	matrix results[`index', 3] = _b[0.poor#`var']
	
	matrix results[`index', 4] = _b[1.poor#`var']
	local index = `index'+1
	
	matrix results[`index', 3] = round(_se[0.poor#`var'],0.0001)
	
    matrix results[`index', 4] = round(_se[1.poor#`var'], 0.0001)
	local index = `index'+1
	
}
local index= `index'+8
forvalues i=1/3{
	matrix results[`index', 3] = _b[L`i'.dgdp]
local index = `index'+1
matrix results[`index', 3] = round(_se[L`i'.dgdp], 0.0001)
local index = `index'+1
}
restore

preserve
local deltaT = 4
gen N_usd = gdppc

sum year
gen y_min = r(min)

gen period = floor((year - y_min)/`deltaT')

collapse(mean) gdppc gdppc_wb tem pre pop ///
   (first)  country1  subc ///
   (count) N_usd  ///
   , by(country period)

*keep if N_usd >1

sum period
gen T_max = r(max)

gen year = period
xtset country1 year 
gen lngdp = log(gdppc)
gen dgdp = lngdp-l2.lngdp

gen lngdp_wb = log(gdppc_wb)
gen dgdp_wb = lngdp_wb-l2.lngdp_wb

gen lnpop = log(pop)
gen dpop = lnpop-l2.lnpop

gen dT = tem-l2.tem
gen dP = pre-l2.pre
gen year2 = year*year

gen T2 = tem*tem
gen P2 = pre*pre

gen dT2= T2-l2.T2
gen dP2 =P2-l2.P2

drop if missing(dgdp, dgdp_wb, tem ,pre,pop)

egen gdppc_mean = mean(gdppc), by(country1)
pctile pct = gdppc_mean, nq(100)
gen poor = 0 if gdppc_mean <= pct[50]
replace poor = 1 if gdppc_mean >pct[50] 

xtabond2 dgdp l(1/1).dgdp c.dT c.dT2 c.tem##c.tem c.l.tem##c.l.tem c.dP c.dP2 c.pre##c.pre c.l.pre##c.l.pre poor#c.(c.dT c.dT2 c.tem##c.tem c.l.tem##c.l.tem c.dP c.dP2 c.pre##c.pre c.l.pre##c.l.pre) dpop i.poor#i.year i.subc#c.year , iv(c.dT c.dT2 c.tem##c.tem  c.l.tem##c.l.tem c.dP c.dP2 c.pre##c.pre c.l.pre##c.l.pre poor#c.(c.dT c.dT2 c.tem##c.tem c.l.tem##c.l.tem c.dP c.dP2 c.pre##c.pre c.l.pre##c.l.pre) dpop i.poor#i.year i.subc#c.year ) gmm(l(1/1).dgdp, l(3 3)) nolevel cluster(country1) artests(3)
estimates store r3
local index =1
local vars dT dT2 tem c.tem#c.tem  dP dP2 pre c.pre#c.pre cL.tem cL.tem#cL.tem cL.pre cL.pre#cL.pre
foreach var of local vars{
    lincom `var' + 1.poor#`var'
	matrix results[`index', 5] = _b[`var']
	
	matrix results[`index', 6] =  r(estimate)
	local index = `index'+1
	
	matrix results[`index', 5] = round(_se[`var'],0.0001)
	
    matrix results[`index', 6] =  round(r(se), 0.0001)
	local index = `index'+1
	
}
matrix results[`index', 5] = _b[L1.dgdp]
local index = `index'+1
matrix results[`index', 5] = round(_se[L1.dgdp], 0.0001)


xtabond2 dgdp_wb l(1/1).dgdp c.dT c.dT2 c.tem##c.tem c.l.tem##c.l.tem c.dP c.dP2 c.pre##c.pre c.l.pre##c.l.pre poor#c.(c.dT c.dT2 c.tem##c.tem c.l.tem##c.l.tem c.dP c.dP2 c.pre##c.pre c.l.pre##c.l.pre) dpop i.poor#i.year i.subc#c.year , iv(c.dT c.dT2 c.tem##c.tem  c.l.tem##c.l.tem c.dP c.dP2 c.pre##c.pre c.l.pre##c.l.pre poor#c.(c.dT c.dT2 c.tem##c.tem c.l.tem##c.l.tem c.dP c.dP2 c.pre##c.pre c.l.pre##c.l.pre) dpop i.poor#i.year i.subc#c.year ) gmm(l(1/1).dgdp, l(3 3)) nolevel cluster(country1) artests(3)
lincom 1.poor#c.tem+2*25*(1.poor#c.tem#c.tem)
estimates store r4
local index =1
local vars dT dT2 tem c.tem#c.tem  dP dP2 pre c.pre#c.pre cL.tem cL.tem#cL.tem cL.pre cL.pre#cL.pre
foreach var of local vars{
    lincom `var' + 1.poor#`var'
	matrix results[`index', 7] = _b[`var']
	
	matrix results[`index', 8] =  r(estimate)
	local index = `index'+1
	
	matrix results[`index', 7] = round(_se[`var'],0.0001)
	
    matrix results[`index', 8] =  round(r(se), 0.0001)
	local index = `index'+1
	
}
matrix results[`index', 7] = _b[L1.dgdp]
local index = `index'+1
matrix results[`index', 7] = round(_se[L1.dgdp], 0.0001)

restore


local vars dT dT2 tem c.tem#c.tem  dP dP2 pre c.pre#c.pre cL.tem cL.tem#cL.tem cL.pre cL.pre#cL.pre L1.dgdp L2.dgdp L3.dgdp
local row_names ""
foreach var of local vars {
    local clean_name = subinstr("`var'", "c.", "", .)
    local row_names "`row_names' `clean_name' `clean_name'_se"
}
matrix rownames results = `row_names'

esttab matrix(results,fmt(%9.3g)) using "02table\Table6.rtf",  se(%5.4f)  replace

esttab r1 r2 r3 r4 using "02table\Table6.rtf", star(* .1 ** .05  *** .01) nogap nonumber replace se(%5.4f) r2 ar2 aic(%10.4f) bic(%10.4f)