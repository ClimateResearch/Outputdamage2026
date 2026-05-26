clear all

cd "~"
insheet using GDPpc.csv ,clear

*********Make Table A1*************

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
gen poor = 0 if gdppc_mean >= pct[50]
replace poor = 1 if gdppc_mean <pct[50] 

egen tem_mean = mean(tem), by(gid_nmbr)
pctile pct_tem = tem_mean, nq(100)
gen cool = 0 if tem_mean <= pct_tem[50]
replace cool = 1 if tem_mean > pct_tem[50] 

reghdfe dgdp c.l(1/3).dgdp  c.tem##c.tem c.pre##c.pre poor#c.( c.tem##c.tem c.pre##c.pre) dpop  [aweight=weight], absorb( i.gid_nmbr i.subc#c.year i.poor#i.year) cluster(country1)
estimates store r1
lincom tem+1.poor#c.tem +2*25*(c.tem#c.tem+1.poor#c.tem#c.tem)

reghdfe dgdp c.l(1/3).dgdp  c.tem##c.tem c.pre##c.pre poor#c.( c.tem##c.tem c.pre##c.pre)  cool#c.(c.tem##c.tem c.pre##c.pre) dpop  [aweight=weight], absorb( i.gid_nmbr i.subc#c.year i.poor#i.year) cluster(country1)
estimates store r2
lincom tem+1.poor#c.tem +2*25*(c.tem#c.tem+1.poor#c.tem#c.tem)
lincom tem+1.cool#c.tem +2*25*(c.tem#c.tem+1.cool#c.tem#c.tem)

reghdfe dgdp c.l(1/5).dgdp  c.tem##c.tem c.pre##c.pre poor#c.( c.tem##c.tem c.pre##c.pre)  cool#c.(c.tem##c.tem c.pre##c.pre) dpop  [aweight=weight], absorb( i.gid_nmbr i.subc#c.year i.poor#i.year) cluster(country1)
estimates store r3
lincom tem+1.poor#c.tem +2*25*(c.tem#c.tem+1.poor#c.tem#c.tem)
lincom tem+1.cool#c.tem +2*25*(c.tem#c.tem+1.cool#c.tem#c.tem)

xtabond2 dgdp c.l(1/5).dgdp c.tem##c.tem c.pre##c.pre poor#c.( c.tem##c.tem c.pre##c.pre)  cool#c.(c.tem##c.tem c.pre##c.pre) dpop  i.subc#c.year i.poor#i.year [aweight=weight], iv(c.tem##c.tem c.pre##c.pre poor#c.( c.tem##c.tem c.pre##c.pre)  cool#c.(c.tem##c.tem c.pre##c.pre)  i.subc#c.year  dpop i.poor#i.year) gmm(c.L(1/5).dgdp, l(6 8)  ) nolevel cluster(country1)  artests(3) or
estimates store r4
lincom tem+1.poor#c.tem +2*25*(c.tem#c.tem+1.poor#c.tem#c.tem)
lincom tem+1.cool#c.tem +2*25*(c.tem#c.tem+1.cool#c.tem#c.tem)

esttab r1 r2 r3 r4 using "02table\TableA1.rtf", star(* .1 ** .05  *** .01) nogap nonumber replace se(%5.4f) r2 ar2 aic(%10.4f) bic(%10.4f)


