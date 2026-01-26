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

gen dT = d.tem
gen dP = d.pre
gen year2 = year*year

gen T2 = tem*tem
gen P2 = pre*pre

gen dT2= d.T2
gen dP2 = d.P2

reghdfe dgdp c.tem##c.tem c.pre##c.pre dpop [aweight=weight],absorb(i.gid_nmbr i.year i.subc#c.year) vce( cluster country1) 
estimates store r1
lincom c.tem+2*25*c.tem#c.tem

reghdfe dgdp  c.dT c.dT2 c.tem##c.tem c.dP c.dP2 c.pre##c.pre dpop [aweight=weight],absorb(i.gid_nmbr i.year i.subc#c.year) vce( cluster country1)
estimates store r2
lincom c.tem+2*25*c.tem#c.tem

reghdfe dgdp l.dgdp c.dT c.dT2 c.tem##c.tem c.dP c.dP2 c.pre##c.pre dpop [aweight=weight],absorb(i.gid_nmbr i.year i.subc#c.year) vce( cluster country1)
estimates store r3
lincom c.tem+2*25*c.tem#c.tem

reghdfe dgdp l(1/3).dgdp c.dT c.dT2 c.tem##c.tem c.dP c.dP2 c.pre##c.pre dpop [aweight=weight],absorb(i.gid_nmbr i.year i.subc#c.year ) vce( cluster country1)
estimates store r4
lincom c.tem+2*25*c.tem#c.tem

xtabond2 dgdp c.l(1/3).dgdp dT dT2 c.tem##c.tem dP dP2 c.pre##c.pre  dpop i.year i.subc#c.year [aweight=weight], iv(dT dT2 dP dP2 c.pre##c.pre  i.year i.subc#c.year dpop) gmm(c.L(1/3).dgdp, l(4 6) )  nolevel cluster(country1) or
estimates store r5
lincom c.tem+2*25*c.tem#c.tem


esttab r1 r2 r3 r4 r5 using "02table\Table2.rtf", star(* 0.1 ** 0.05 *** 0.01) se(%5.4f) nogap nonumber replace r2 ar2 stats(N N_g ar1 ar2 sargan hansen) 