clear all

cd E:\06博士论文\002出国交流\02cliamte_economy\output_ld_new
insheet using GDPpc.csv ,clear

*****************************************
*      panel regression for GDPpc       *
*****************************************

*Variable generation
drop if missing(gdppc, tem, pre,pop)

encode subcontinent, gen(subc)
encode countsub, gen(countsub1)
encode country, gen(country1)
xtset gid_nmbr year 

gen lngdp = log(gdppc)
gen dgdp = d.lngdp

gen lnpop = log(pop)
gen dpop = d.lnpop

gen dT = d.tem_p
gen dP = d.pre_p
gen year2 = year*year

gen T2 = tem_p*tem_p
gen P2 = pre_p*pre_p

gen dT2= d.T2
gen dP2 = d.P2

egen gdppc_mean = mean(gdppc), by(gid_nmbr)
pctile pct = gdppc_mean, nq(100)
gen poor = 0 if gdppc_mean <= pct[50]
replace poor = 1 if gdppc_mean >pct[50] 

reghdfe dgdp poor#c.(l(1/5).dgdp c.dT c.dT2 c.tem##c.tem c.dP c.dP2 c.pre##c.pre dpop) [aweight=weight],absorb(i.gid_nmbr i.countsub1#i.year) vce( cluster country1) 
estimates store r3
lincom 0.poor#c.tem+2*25*0.poor#c.tem#c.tem


preserve
local deltaT = 5
gen N_usd = gdppc

sum year
gen y_min = r(min)

gen period = floor((year - y_min)/`deltaT')

collapse(mean) gdppc tem pre tem_p pre_p pop ///
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

gen dT = tem_p-l2.tem_p
gen dP = pre_p-l2.pre_p
gen year2 = year*year

gen T2 = tem_p*tem_p
gen P2 = pre_p*pre_p

gen dT2= T2-l2.T2
gen dP2 =P2-l2.P2

egen gdppc_mean = mean(gdppc), by(gid_nmbr)
pctile pct = gdppc_mean, nq(100)
gen poor = 0 if gdppc_mean <= pct[50]
replace poor = 1 if gdppc_mean >pct[50] 

reghdfe dgdp poor#c.(l(1/3).dgdp c.dT c.dT2 c.tem##c.tem c.l.tem c.l.tem#c.l.tem c.dP c.dP2 c.pre##c.pre c.l.pre c.l.pre##c.l.pre dpop)  [aweight=weight],absorb(i.gid_nmbr i.year) vce( cluster country1) 
estimates store r`gap'_2
lincom 0.poor#c.l.tem+2*25*0.poor#c.l.tem#c.l.tem
restore



esttab r1 r2 r3 using "02table\Table3.rtf", star(* .1 ** .05  *** .01) nogap nonumber replace se(%5.4f) r2 ar2 aic(%10.4f) bic(%10.4f)