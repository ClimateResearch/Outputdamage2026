clear all

cd E:\06博士论文\002出国交流\02cliamte_economy\output_ld_new
insheet using GDPpc.csv ,clear

*****************************************
*      panel regression for GDPpc       *
*****************************************

*Variable generation
encode countsub, gen(countsub1)
encode country, gen(country1)
xtset gid_nmbr year

gen lngdp = log(gdppc)
gen dgdp = d.lngdp

gen lnpop = log(pop)
gen dpop = d.lnpop

gen dT = d.tem
gen dP = d.pre

keep if year <=2014

reghdfe dgdp  c.dT c.dT#c.tem  c.tem##c.tem c.dP c.dP#c.pre c.pre##c.pre [aweight=weight],absorb(i.year gid_nmbr#c.year) vce( cluster country1) 
estimates store r1

preserve
local deltaT = 5
gen N_usd = gdp

sum year
gen y_min = r(min)

gen period = floor((year - y_min)/`deltaT')

collapse(mean) gdppc tem pre pop ///
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
gen year2 = year*year

gen T2 = tem*tem
gen P2 = pre*pre

gen dT2= T2-l2.T2
gen dP2 =P2-l2.P2

reghdfe dgdp c.dT c.dT#c.tem c.tem##c.tem c.l.tem c.l.tem#c.l.tem c.dP c.dP#c.tem c.pre##c.pre c.l.pre c.l.pre#c.l.pre  [aweight=weight],absorb(i.year i.gid_nmbr) vce( cluster country1)
estimates store r3
restore

keep if kalkuhl ==1
reghdfe dgdp c.dT c.dT#c.tem  c.tem##c.tem c.dP c.dP#c.pre c.pre##c.pre dpop [aweight=weight],absorb(i.year gid_nmbr#c.year) vce( cluster country1)  
estimates store r2

preserve
local deltaT = 5
gen N_usd = gdp

sum year
gen y_min = r(min)

gen period = floor((year - y_min)/`deltaT')

collapse(mean) gdppc tem pre pop ///
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
gen year2 = year*year

gen T2 = tem*tem
gen P2 = pre*pre

gen dT2= T2-l2.T2
gen dP2 =P2-l2.P2

reghdfe dgdp c.dT c.dT#c.tem c.tem##c.tem c.l.tem c.l.tem#c.l.tem c.dP c.dP#c.tem c.pre##c.pre c.l.pre c.l.pre#c.l.pre  [aweight=weight],absorb(i.year i.gid_nmbr) vce( cluster country1)
estimates store r3
restore

use 01data\kalkuhl_gdppc.dta,clear
egen province_id = group(ID)
egen num_max = max(province_id), by(wrld1id_0)
egen num_min = min(province_id), by(wrld1id_0)
replace num_max =. if (num_max ==1544 & wrld1id_0 !=250)
replace num_min =. if (num_min ==1 & wrld1id_0 !=4)
gen num_prov = num_max-num_min +1
gen weight_r = 1/num_prov
drop province_id num_max num_min num_prov
drop if year > 2014

gen dlgdp_pc_usd = d.lgdp_pc_usd
gen year_sqr = year*year
gen dT = d.temp
gen dP = d.prec

rename temp tem
rename prec pre

keep if year >= 1991

reghdfe dlgdp_pc_usd  c.dT c.dT#c.tem c.tem##c.tem c.dP c.dP#c.pre c.pre##c.pre  [aweight=weight],absorb(i.StructChange  i.ID wrld1id_0#c.year) vce(cluster wrld1id_0)
estimates store r5
 local deltaT = 10
gen N_usd = lgdp_pc_usd

sum year
gen y_max = 2014

gen p = floor(year / `deltaT')
sum p
gen p_max = r(max)

gen YY = year - y_max +`deltaT'*p_max -1
gen period = floor(YY/`deltaT')


collapse(mean) lgdp_pc_usd tem pre ///
   (first) wrld1id_0 weight_r ///
   (count) N_usd  ///
   , by(ID period)

keep if N_usd>= floor(`deltaT'/4)

sum period
gen T_max = r(max)

gen year = period
xtset ID year  

gen dlgdp_pc_usd=d.lgdp_pc_usd
gen dT=d.tem
gen dP=d.pre

reghdfe dlgdp_pc_usd c.dT c.dT#c.tem  c.tem##c.tem c.dP c.dP#c.pre c.pre##c.pre [aweight=weight_r],absorb(i.wrld1id_0) vce( cluster wrld1id_0) 
estimates store r6
esttab r1 r2 r3 r4 r5 r6 using "02table\Table5.rtf", star(* .1 ** .05  *** .01) nogap nonumber replace se(%5.4f) r2 ar2 aic(%10.4f) bic(%10.4f)

