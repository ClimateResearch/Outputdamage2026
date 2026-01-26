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
gen poor = 0 if gdppc_mean < pct[30]
replace poor = 1 if gdppc_mean >= pct[30] & gdppc_mean < pct[70]
replace poor = 2 if gdppc_mean >= pct[70]

reghdfe dgdp c.l(1/3).dgdp poor#c.(dT dT2 c.tem##c.tem dP dP2 c.pre##c.pre) dpop  [aweight=weight], absorb(i.gid_nmbr i.year i.subc#c.year) cluster(country1) 
estimates store r1
lincom 0.poor#c.tem+2*20*0.poor#c.tem#c.tem
lincom 1.poor#c.tem+2*20*1.poor#c.tem#c.tem
lincom 2.poor#c.tem+2*20*2.poor#c.tem#c.tem

reghdfe dgdp poor#c.(dT dT2 c.tem##c.tem dP dP2 c.pre##c.pre) dpop  [aweight=weight], absorb(i.gid_nmbr i.year i.subc#c.year) cluster(country1) 
estimates store r2
lincom 0.poor#c.tem+2*20*0.poor#c.tem#c.tem
lincom 1.poor#c.tem+2*20*1.poor#c.tem#c.tem
lincom 2.poor#c.tem+2*20*2.poor#c.tem#c.tem

esttab r1 r2 using "02table\TableA3.rtf", star(* .1 ** .05  *** .01) nogap nonumber replace se(%5.4f) r2 ar2 aic(%10.4f) bic(%10.4f)