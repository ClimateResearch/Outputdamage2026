clear all

cd "E:\06博士论文\002出国交流\02cliamte_economy\output_ld_new"
insheet using GDPpc.csv ,clear

*********Make Table A7*************

drop if missing(tem ,pre , pop)
keep if year<=2014
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

reghdfe dgdp c.l(1/3).dgdp dT dT2 c.tem##c.tem dP dP2 c.pre##c.pre dpop [aweight=weight],absorb( i.gid_nmbr i.year i.subc#c.year ) vce( cluster country1)
estimates store r1
lincom c.tem+2*25*(c.tem#c.tem)

preserve
keep if kalkuhl==1
reghdfe dgdp c.l(1/3).dgdp dT dT2 c.tem##c.tem dP dP2 c.pre##c.pre dpop [aweight=weight],absorb( i.gid_nmbr i.year i.subc#c.year ) cluster(country1)
estimates store r2
lincom c.tem+2*25*(c.tem#c.tem)
restore


local deltaT = 4
gen N_usd = gdppc

sum year
gen y_min = r(min)

gen period = floor((year - y_min)/`deltaT')

collapse(mean) gdppc tem pre pop ///
   (first) countsub1 country1 weight subc kalkuhl ///
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

preserve
reghdfe dgdp l(1/1).dgdp c.dT c.dT2 c.tem##c.tem c.l.tem##c.l.tem c.dP c.dP2 c.pre##c.pre c.l.pre##c.l.pre dpop   [aweight=weight], absorb(i.gid_nmbr i.year i.subc#c.year) cluster(country1)
estimates store r4
lincom c.l.tem+2*25*(c.l.tem#c.l.tem)
restore

preserve
keep if kalkuhl==1
reghdfe dgdp l(1/1).dgdp c.dT c.dT2 c.tem##c.tem c.l.tem##c.l.tem c.dP c.dP2 c.pre##c.pre c.l.pre##c.l.pre dpop   [aweight=weight], absorb(i.gid_nmbr i.year i.subc#c.year) cluster(country1)
estimates store r5
lincom c.l.tem+2*25*(c.l.tem#c.l.tem)
restore

use 01data\kalkuhl_gdppc1.dta,clear
egen province_id = group(ID)
egen num_max = max(province_id), by(wrld1id_0)
egen num_min = min(province_id), by(wrld1id_0)
replace num_max =. if (num_max ==1544 & wrld1id_0 !=250)
replace num_min =. if (num_min ==1 & wrld1id_0 !=4)
gen num_prov = num_max-num_min +1
gen weight = 1/num_prov
drop province_id num_max num_min num_prov
drop if year > 2014
keep if year >= 1990

encode WorldSubregion, gen(subc)
rename wrld1id_0 country1
gen dgdp = d.lgdp_pc_usd
gen dT = d.temp
gen dP = d.prec
rename temp tem
rename prec pre
gen T2 = tem*tem
gen P2 = pre*pre

gen dT2= d.T2
gen dP2 = d.P2
gen lnpop = log(pop)
gen dpop = d.lnpop

reghdfe dgdp c.l(1/3).dgdp dT dT2 c.tem##c.tem dP dP2 c.pre##c.pre dpop [aweight=weight],absorb( i.ID i.year i.subc#c.year ) cluster(country1)
estimates store r3
lincom c.tem+2*25*(c.tem#c.tem)

local deltaT = 4
gen N_usd = lgdp_pc_usd

sum year
gen y_min = r(min)

gen period = floor((year - y_min)/`deltaT')

collapse(mean) lgdp_pc_usd tem pre pop ///
   (first) country1 weight subc StructChange ///
   (count) N_usd  ///
   , by(ID period)

keep if N_usd >1

sum period
gen T_max = r(max)

gen year = period
xtset ID year 

gen dgdp = lgdp_pc_usd-l2.lgdp_pc_usd

gen lnpop = log(pop)
gen dpop = lnpop-l2.lnpop

gen dT = tem-l2.tem
gen dP = pre-l2.pre
gen year2 = year*year

gen T2 = tem*tem
gen P2 = pre*pre

gen dT2= T2-l2.T2
gen dP2 =P2-l2.P2

reghdfe dgdp l(1/1).dgdp c.dT c.dT2 c.tem##c.tem c.l.tem##c.l.tem c.dP c.dP2 c.pre##c.pre c.l.pre##c.l.pre dpop   [aweight=weight], absorb(i.ID i.year i.subc#c.year) cluster(country1)
estimates store r6
lincom c.l.tem+2*25*(c.l.tem#c.l.tem)

esttab r1 r2 r3 r4 r5 r6 using "02table\TableA7.rtf", star(* .1 ** .05  *** .01) nogap nonumber replace se(%5.4f) r2 ar2 aic(%10.4f) bic(%10.4f)