clear all

cd E:\06博士论文\002出国交流\02cliamte_economy\output_ld_new
insheet using 01data\gdppc_world_bank_merge.csv ,clear

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

reghdfe  dgdp l(1/3).dgdp poor#c.(c.dT c.dT2 c.tem##c.tem  c.dP c.dP2 c.pre##c.pre ) dpop , absorb(i.year i.country1 i.subc#c.year ) cluster(country1)

reghdfe  dgdp_wb l(1/3).dgdp_wb poor#c.(c.dT c.dT2 c.tem##c.tem  c.dP c.dP2 c.pre##c.pre ) dpop, absorb(i.year i.country1 i.subc#c.year ) cluster(country1)
estimates store r2
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

reghdfe dgdp l(1/3).dgdp poor#c.( c.dT c.dT2 c.tem##c.tem c.l.tem c.l.tem#c.l.tem c.dP c.dP2 c.pre##c.pre c.l.pre c.l.pre##c.l.pre ) dpop ,absorb(i.country1 i.year i.subc#c.year) vce( cluster country1)
estimates store r3
lincom 1.poor#c.tem+2*25*(1.poor#c.tem#c.tem)
reghdfe dgdp_wb l(1/3).dgdp_wb poor#c.( c.dT c.dT2 c.tem##c.tem c.l.tem c.l.tem#c.l.tem c.dP c.dP2 c.pre##c.pre c.l.pre c.l.pre##c.l.pre )dpop ,absorb(i.country1 i.year i.subc#c.year) vce( cluster country1)
estimates store r4
lincom 1.poor#c.tem+2*25*(1.poor#c.tem#c.tem)
restore

esttab r1 r2 r3 r4 using "02table\Table6.rtf", star(* .1 ** .05  *** .01) nogap nonumber replace se(%5.4f) r2 ar2 aic(%10.4f) bic(%10.4f)