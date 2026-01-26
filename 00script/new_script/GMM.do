clear all

cd E:\06博士论文\002出国交流\02cliamte_economy\output_ld_new
insheet using GDPpc.csv ,clear

*****************************************
*      panel regression for GDPpc       *
*****************************************

*Variable generation
drop if year <1980
drop if missing(tem ,pre)
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

xtabond2 dgdp c.l(1/1).dgdp dT dT2 c.tem##c.tem dP dP2 c.pre##c.pre poor#c.(dT dT2 c.tem##c.tem dP dP2 c.pre##c.pre) dpop i.year i.subc#c.year i.subc#c.year2  [aweight=weight], iv(dT dT2  dP dP2 i.year i.subc#c.year i.subc#c.year2  dpop poor#c.(dT dT2 dP dP2)) /// 
gmm(c.L.dgdp , l(3 6) ) ///
gmm(c.tem c.pre c.tem#c.tem c.pre#c.pre poor#(c.tem c.pre c.tem#c.tem c.pre#c.pre), l(1 3) c) ///
 nolevel cluster(country1) or 

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

reghdfe  dgdp l(1/1).dgdp c.dT c.dT2 c.tem##c.tem c.l.tem##c.l.tem c.dP c.dP2 c.pre##c.pre c.l.pre##c.l.pre poor#c.(c.dT c.dT2 c.tem##c.tem c.l.tem##c.l.tem c.dP c.dP2 c.pre##c.pre c.l.pre##c.l.pre) dpop [aweight=weight], absorb(i.year i.gid_nmbr i.subc#c.year ) cluster(country1)

xtabond2 dgdp l(1/1).dgdp c.dT c.dT2 c.tem##c.tem c.l.tem##c.l.tem c.dP c.dP2 c.pre##c.pre c.l.pre##c.l.pre poor#c.(c.dT c.dT2 c.tem##c.tem c.l.tem##c.l.tem c.dP c.dP2 c.pre##c.pre c.l.pre##c.l.pre) dpop i.year i.subc#c.year  [aweight=weight], iv(c.dT c.dT2 c.tem##c.tem c.l.tem##c.l.tem c.dP c.dP2 c.pre##c.pre c.l.pre##c.l.pre poor#c.(c.dT c.dT2 c.tem##c.tem c.l.tem##c.l.tem c.dP c.dP2 c.pre##c.pre c.l.pre##c.l.pre) dpop i.year i.subc#c.year ) gmm(l(1/1).dgdp, l(3 .)) nolevel cluster(country1) artests(4) or
lincom cL.tem +1.poor#cL.tem
lincom cL.tem#cL.tem +1.poor#cL.tem#cL.tem 

lincom cL.tem +1.poor#cL.tem +2*25*(cL.tem#cL.tem +1.poor#cL.tem#cL.tem )

lincom cL.tem  +2*25*(cL.tem#cL.tem )
lincom c.dT +2*12*dT2