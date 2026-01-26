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

save "GDPpc.dta", replace

postfile Bootrstrap T2_panel0 T_panel0 T2_panel1 T_panel1 T2_panel2 T_panel2   T2_ld0 T_ld0 T2_ld1 T_ld1 T2_ld2 T_ld2   using "01data\04scenarios\Bootrstrap.dta", replace

forvalues i = 1/1000 {
    * 随机抽取m-1个地区
use GDPpc.dta,replace    
bsample, cluster(gid_nmbr)
sort gid_nmbr year
bysort gid_nmbr year: gen seq = _n
gsort gid_nmbr seq year
egen  region = seq(), from(1) block(33)

qui{ 
xtset region year

egen gdppc_mean = mean(gdppc), by(region)
pctile pct = gdppc_mean, nq(100)
gen poor = 0 if gdppc_mean < pct[30]
replace poor = 1 if gdppc_mean >= pct[30] & gdppc_mean < pct[70]
replace poor = 2 if gdppc_mean >= pct[70]

reghdfe dgdp poor#c.(l(1/5).dgdp c.dT c.dT2 c.tem##c.tem c.dP c.dP2 c.pre##c.pre dpop) [aweight=weight],absorb(i.region i.countsub1#i.year) vce( cluster country1) 
	
scalar T2_panel0 = _b[0.poor#c.tem#c.tem]
scalar T_panel0 = _b[0.poor#c.tem]
scalar T2_panel1 = _b[1.poor#c.tem#c.tem]
scalar T_panel1 = _b[1.poor#c.tem]
scalar T2_panel2 = _b[2.poor#c.tem#c.tem]
scalar T_panel2 = _b[2.poor#c.tem]

local deltaT = 5
gen N_usd = gdppc

sum year
gen y_min = r(min)

gen period = floor((year - y_min)/`deltaT')

collapse(mean) gdppc tem pre pop ///
   (first) countsub1 country1 weight ///
   (count) N_usd  ///
   , by(region gid_nmbr period)

sum period
gen T_max = r(max)

gen year = period
xtset region year 
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

egen gdppc_mean = mean(gdppc), by(region)
pctile pct = gdppc_mean, nq(100)
gen poor = 0 if gdppc_mean < pct[30]
replace poor = 1 if gdppc_mean >= pct[30] & gdppc_mean < pct[70]
replace poor = 2 if gdppc_mean >= pct[70] 

reghdfe dgdp poor#c.(l(1/3).dgdp c.dT c.dT2 c.tem##c.tem c.l.tem c.l.tem#c.l.tem c.dP c.dP2 c.pre##c.pre c.l.pre c.l.pre##c.l.pre dpop)  [aweight=weight],absorb(i.region i.year) vce( cluster country1) 
 
scalar T2_ld0 = _b[0.poor#c.l.tem#c.l.tem]
scalar T_ld0 = _b[0.poor#c.l.tem]
scalar T2_ld1 = _b[1.poor#c.l.tem#c.l.tem]
scalar T_ld1 = _b[1.poor#c.l.tem]
scalar T2_ld2 = _b[2.poor#c.l.tem#c.l.tem]
scalar T_ld2 = _b[2.poor#c.l.tem]
}
* 将临时变量的值存储到 MakeFigure4 中的新行
post Bootrstrap (T2_panel0) (T_panel0) (T2_panel1) (T_panel1) (T2_panel2) (T_panel2)  (T2_ld0) (T_ld0) (T2_ld1) (T_ld1) (T2_ld2) (T_ld2)

dis `i'
}

* 关闭postfile
postclose Bootrstrap

use 01data\04scenarios\Bootrstrap.dta,clear
export delimited using "01data\04scenarios\Bootstrap1.csv", replace
