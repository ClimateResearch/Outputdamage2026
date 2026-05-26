clear all

cd "~"
insheet using GDPpc.csv ,clear

*********Bootrstrap*************

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

save "GDPpc.dta", replace

postfile Bootrstrap T2_panel0 T_panel0 T2_panel1 T_panel1 dT2_panel0 dT_panel0 dT2_panel1 dT_panel1 T2_ld0 T_ld0 lT2_ld0 lT_ld0 lT2_ld1 lT_ld1   dT2_ld0 dT_ld0 dT2_ld1 dT_ld1 using "01data\04scenarios\Bootrstrap.dta", replace

forvalues i = 1/1000 {
    * 随机抽取m-1个地区
use GDPpc.dta,replace    
bsample, cluster(gid_nmbr)
sort gid_nmbr year
bysort gid_nmbr year: gen seq = _n
gsort gid_nmbr seq year
egen  region = seq(), from(1) block(43)

qui{ 
xtset region year

egen gdppc_mean = mean(gdppc), by(region)
pctile pct = gdppc_mean, nq(100)
gen poor = 0 if gdppc_mean < pct[50]
replace poor = 1 if gdppc_mean >= pct[50]

xtabond2 dgdp c.l(1/3).dgdp dT dT2 c.tem##c.tem dP dP2 c.pre##c.pre poor#c.(dT dT2 c.tem##c.tem dP dP2 c.pre##c.pre) dpop i.poor#i.year i.subc#c.year  [aweight=weight], iv(dT dT2 c.tem##c.tem dP dP2 c.pre##c.pre  poor#c.(dT dT2 c.tem##c.tem dP dP2 c.pre##c.pre) i.poor#i.year i.subc#c.year  dpop ) gmm(c.L(1/3).dgdp, l(4 6) ) nolevel cluster(country1)  or


scalar T2_panel0 = _b[c.tem#c.tem]
scalar T_panel0 = _b[c.tem]
scalar dT2_panel0 = _b[c.dT2]
scalar dT_panel0 = _b[c.dT]


lincom c.tem#c.tem + 1.poor#c.tem#c.tem
scalar T2_panel1 = r(estimate)
lincom c.tem + 1.poor#c.tem
scalar T_panel1 = r(estimate)
	
lincom c.dT2 + 1.poor#c.dT2
scalar dT2_panel1 = r(estimate)
lincom c.dT + 1.poor#c.dT
scalar dT_panel1 = r(estimate)


local deltaT = 4
gen N_usd = gdppc

sum year
gen y_min = r(min)

gen period = floor((year - y_min)/`deltaT')

collapse(mean) gdppc tem pre pop ///
   (first) countsub1 country1 weight subc ///
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
gen poor = 0 if gdppc_mean < pct[50]
replace poor = 1 if gdppc_mean >= pct[50] 

xtabond2 dgdp l(1/1).dgdp c.dT c.dT2 c.tem##c.tem c.l.tem##c.l.tem c.dP c.dP2 c.pre##c.pre c.l.pre##c.l.pre poor#c.(c.dT c.dT2 c.tem##c.tem c.l.tem##c.l.tem c.dP c.dP2 c.pre##c.pre c.l.pre##c.l.pre) dpop i.poor#i.year i.subc#c.year  [aweight=weight], iv(c.dT c.dT2 c.tem##c.tem  c.l.tem##c.l.tem c.dP c.dP2 c.pre##c.pre c.l.pre##c.l.pre poor#c.(c.dT c.dT2 c.tem##c.tem c.l.tem##c.l.tem c.dP c.dP2 c.pre##c.pre c.l.pre##c.l.pre) dpop i.poor#i.year i.subc#c.year ) gmm(l(1/1).dgdp, l(3 4))  nolevel cluster(country1) artests(4) 

scalar T2_ld0 = _b[c.tem#c.tem]
scalar T_ld0 = _b[c.tem]
scalar lT2_ld0 = _b[c.l.tem#c.l.tem]
scalar lT_ld0 = _b[c.l.tem]
scalar dT2_ld0 = _b[dT2]
scalar dT_ld0 = _b[dT]

lincom c.l.tem#c.l.tem + 1.poor#c.l.tem#c.l.tem
scalar lT2_ld1 = r(estimate)
lincom c.l.tem + 1.poor#c.l.tem
scalar lT_ld1 = r(estimate)
	
lincom c.dT2 + 1.poor#c.dT2
scalar dT2_ld1 = r(estimate)
lincom c.dT + 1.poor#c.dT
scalar dT_ld1 = r(estimate)


}

post Bootrstrap (T2_panel0) (T_panel0) (T2_panel1) (T_panel1) (dT2_panel0) (dT_panel0) (dT2_panel1) (dT_panel1) (T2_ld0) (T_ld0) (lT2_ld0) (lT_ld0) (lT2_ld1) (lT_ld1) (dT2_ld0) (dT_ld0) (dT2_ld1) (dT_ld1)

dis `i'
}


postclose Bootrstrap

use 01data\04scenarios\Bootrstrap.dta,clear
export delimited using "01data\04scenarios\Bootstrap.csv", replace
