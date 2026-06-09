clear all

cd "E:\06博士论文\002出国交流\02cliamte_economy\output_ld_new"
insheet using GDPpc.csv ,clear

*********Make Table 4*************

drop if missing(tem ,pre,pop)
encode subcontinent, gen(subc)
encode countsub, gen(countsub1)
encode country, gen(country1)
local deltaT = 4
gen N_usd = gdppc

sum year
gen y_min = r(min)

gen period = floor((year - y_min)/`deltaT')

collapse(mean) gdppc tem pre pop ///
   (first) countsub1 country1 weight subc ///
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
gen year2 = year*year

gen T2 = tem*tem
gen P2 = pre*pre

gen dT2= T2-l2.T2
gen dP2 =P2-l2.P2

egen gdppc_mean = mean(gdppc), by(gid_nmbr) 
pctile pct = gdppc_mean, nq(100)
gen poor = 0 if gdppc_mean <= pct[50]
replace poor = 1 if gdppc_mean >pct[50] 

reghdfe  dgdp  poor#c.(c.dT c.dT2 c.tem##c.tem c.l.tem##c.l.tem c.dP c.dP2 c.pre##c.pre c.l.pre##c.l.pre) dpop [aweight=weight], absorb(i.poor#i.year i.gid_nmbr i.subc#c.year ) cluster(country1)
estimates store r1
lincom 0.poor#c.l.tem+2*25*0.poor#c.l.tem#c.l.tem
lincom 1.poor#c.l.tem+2*25*1.poor#c.l.tem#c.l.tem

lincom 0.poor#c.tem+2*25*0.poor#c.tem#c.tem
lincom 1.poor#c.tem+2*25*1.poor#c.tem#c.tem

matrix results = J(26, 6, .) 
local index =1
local vars c.dT c.dT2 c.tem c.tem#c.tem cL.tem cL.tem#cL.tem c.dP c.dP2 c.pre c.pre#c.pre cL.pre cL.pre#cL.pre 
foreach var of local vars{
	matrix results[`index', 1] = _b[0.poor#`var']
	
	matrix results[`index', 2] = _b[1.poor#`var']
	local index = `index'+1
	
	matrix results[`index', 1] = round(_se[0.poor#`var'],0.0001)
	
    matrix results[`index', 2] = round(_se[1.poor#`var'], 0.0001)
	local index = `index'+1
	
}

xtabond2 dgdp l(1/1).dgdp c.dT c.dT2 c.tem##c.tem c.l.tem##c.l.tem c.dP c.dP2 c.pre##c.pre c.l.pre##c.l.pre poor#c.(c.dT c.dT2 c.tem##c.tem c.l.tem##c.l.tem c.dP c.dP2 c.pre##c.pre c.l.pre##c.l.pre) dpop i.poor#i.year i.subc#c.year  [aweight=weight], iv(c.dT c.dT2 c.tem##c.tem  c.l.tem##c.l.tem c.dP c.dP2 c.pre##c.pre c.l.pre##c.l.pre poor#c.(c.dT c.dT2 c.tem##c.tem c.l.tem##c.l.tem c.dP c.dP2 c.pre##c.pre c.l.pre##c.l.pre) dpop i.poor#i.year i.subc#c.year ) gmm(l(1/1).dgdp, l(3 4)) nolevel cluster(country1) artests(3)
estimates store r2
lincom cL.tem +2*25*cL.tem#cL.tem
lincom c.l.tem+1.poor#c.l.tem+2*25*(c.l.tem#c.l.tem+1.poor#c.l.tem#c.l.tem)
lincom c.tem +2*25*c.tem#c.tem
lincom c.tem+1.poor#c.tem+2*25*(c.tem#c.tem+1.poor#c.tem#c.tem)
local index =1
local vars dT dT2 tem c.tem#c.tem cL.tem cL.tem#cL.tem dP dP2 pre c.pre#c.pre cL.pre cL.pre#cL.pre
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

local vars dT dT2 tem c.tem#c.tem cL.tem cL.tem#cL.tem dP dP2 pre c.pre#c.pre cL.pre cL.pre#cL.pre L1.dgdp
local row_names ""
foreach var of local vars {
    local clean_name = subinstr("`var'", "c.", "", .)
    local row_names "`row_names' `clean_name' `clean_name'_se"
}
matrix rownames results = `row_names'


esttab matrix(results,fmt(%9.3g)) using "02table\Table4.rtf",  se(%5.4f)  replace


esttab r1 r2 using "02table\Table4_1.rtf", star(* .1 ** .05  *** .01) nogap nonumber replace se(%5.4f) r2 ar2 aic(%10.4f) bic(%10.4f)
