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

egen gdppc_mean = mean(gdppc), by(gid_nmbr)
pctile pct = gdppc_mean, nq(100)
gen poor = 0 if gdppc_mean <= pct[50]
replace poor = 1 if gdppc_mean >pct[50] 

reghdfe dgdp poor#c.(l(1/5).dgdp c.dT c.dT2 c.tem##c.tem c.dP c.dP2 c.pre##c.pre dpop) [aweight=weight],absorb(i.gid_nmbr i.countsub1#i.year) vce( cluster country1)
estimates store r1
lincom 0.poor#c.tem+2*30*0.poor#c.tem#c.tem
replace dT = d.tem_era
replace dP = d.pre_era

replace T2 = tem_era*tem_era
replace P2 = pre_era*pre_era

replace dT2= d.T2
replace dP2 = d.P2

reghdfe dgdp poor#c.(l(1/5).dgdp c.dT c.dT2 c.tem##c.tem c.dP c.dP2 c.pre##c.pre dpop) [aweight=weight],absorb(i.gid_nmbr i.countsub1#i.year) vce( cluster country1)
estimates store r2
lincom 0.poor#c.tem+2*30*0.poor#c.tem#c.tem
preserve
local deltaT = 5
gen N_usd = gdppc

sum year
gen y_min = r(min)

gen period = floor((year - y_min)/`deltaT')

collapse(mean) gdppc tem pre tem_era pre_era pop ///
   (first) countsub1 country1 weight ///
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

reghdfe dgdp poor#c.(l(1/3).dgdp c.dT c.dT2 c.tem##c.tem c.l.tem c.l.tem#c.l.tem c.dP c.dP2 c.pre##c.pre c.l.pre c.l.pre##c.l.pre dpop)  [aweight=weight],absorb(i.gid_nmbr i.year) vce( cluster country1)
estimates store r3
lincom 0.poor#c.l.tem+2*30*0.poor#c.l.tem#c.l.tem
replace dT = tem_era-l2.tem_era
replace dP = pre_era-l2.pre_era

replace T2 = tem_era*tem_era
replace P2 = pre_era*pre_era

replace dT2= T2-l2.T2
replace dP2 =P2-l2.P2

reghdfe dgdp poor#c.(l(1/3).dgdp c.dT c.dT2 c.tem##c.tem c.l.tem c.l.tem#c.l.tem c.dP c.dP2 c.pre##c.pre c.l.pre c.l.pre##c.l.pre dpop)  [aweight=weight],absorb(i.gid_nmbr i.year) vce( cluster country1)
estimates store r4
restore
lincom 0.poor#c.l.tem+2*30*0.poor#c.l.tem#c.l.tem
esttab r1 r2 r3 r4 using "02table\Table8.rtf", star(* .1 ** .05  *** .01) nogap nonumber replace se(%5.4f) r2 ar2 aic(%10.4f) bic(%10.4f)