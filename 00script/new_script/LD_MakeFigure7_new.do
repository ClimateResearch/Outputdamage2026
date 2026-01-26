clear all

cd E:\06博士论文\002出国交流\02cliamte_economy\output_ld_new
insheet using GDPpc.csv ,clear

*****************************************
*      panel regression for GDPpc       *
*****************************************

* 创建一个postfile，用于保存回归系数
postfile MakeFigure7 T0 T1 T2 T3 T4 T5 T6 T7 T8 T9 T10 T11 T12 T13 T14 T15 T16 T17 T18 T19 T20 T21 T22 T23 T24 T25 T26 T27 T28 T29 T30 model using "02table\MakeFigure7.dta", replace


forvalues i = 1/1000 {
    * 随机抽取m-1个地区
use GDPpc.dta,replace    
bsample, cluster(gid_nmbr)
sort gid_nmbr year
bysort gid_nmbr year: gen seq = _n
gsort gid_nmbr seq year
egen  region = seq(), from(1) block(33)
qui {
xtset region year 

egen gdppc_mean = mean(gdppc), by(gid_nmbr)
pctile pct = gdppc_mean, nq(100)
gen poor = 0 if gdppc_mean <= pct[50]
replace poor = 1 if gdppc_mean > pct[50]

reghdfe dgdp poor#c.(l(1/5).dgdp c.dT c.dT2 c.tem##c.tem c.dP c.dP2 c.pre##c.pre dpop) [aweight=weight],absorb(i.region i.countsub1#i.year) vce( cluster country1) 
	
		forvalues j = 0/30 {
		    lincom (0.poor#c.tem#c.tem)*2*`j' + 0.poor#c.tem
		    scalar T_`j' = r(estimate)
		}
		
		scalar model = 1

		post MakeFigure7 (T_0) (T_1) (T_2) (T_3) (T_4) (T_5) (T_6) (T_7) (T_8) (T_9) (T_10) (T_11) (T_12) (T_13) (T_14) (T_15) (T_16) (T_17) (T_18) (T_19) (T_20) (T_21) (T_22) (T_23) (T_24) (T_25) (T_26) (T_27) (T_28) (T_29) (T_30) (model)

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

egen gdppc_mean = mean(gdppc), by(gid_nmbr)
pctile pct = gdppc_mean, nq(100)
gen poor = 0 if gdppc_mean <= pct[50]
replace poor = 1 if gdppc_mean > pct[50]

reghdfe dgdp poor#c.(l(1/3).dgdp c.dT c.dT2 c.tem##c.tem c.l.tem c.l.tem#c.l.tem c.dP c.dP2 c.pre##c.pre c.l.pre c.l.pre##c.l.pre dpop)  [aweight=weight],absorb(i.region i.year) vce( cluster country1) 
 
	
forvalues j = 0/30 {
		    lincom (0.poor#c.l.tem#c.l.tem)*2*`j' + 0.poor#c.l.tem
		    scalar T_`j' = r(estimate)
		}
		
		scalar model = 2
		
		post MakeFigure7 (T_0) (T_1) (T_2) (T_3) (T_4) (T_5) (T_6) (T_7) (T_8) (T_9) (T_10) (T_11) (T_12) (T_13) (T_14) (T_15) (T_16) (T_17) (T_18) (T_19) (T_20) (T_21) (T_22) (T_23) (T_24) (T_25) (T_26) (T_27) (T_28) (T_29) (T_30) (model)
}
dis `i'
}

postclose MakeFigure7

use 02table\MakeFigure7.dta,clear 
preserve
keep if model ==1
tempfile stats
postfile stats mean lb ub using `stats'
forval i = 0/30 {
    qui {
        local varname T`i'
        summarize `varname',detail
        local median = r(p50)
        local se = r(sd) 

        local lb = `median' - 1.645*`se'
        local ub = `median' + 1.645*`se'

        post stats  (`median') (`lb') (`ub') 
	}
}
postclose stats

* 读取临时文件
use `stats', clear

gen tem = _n-1

twoway /// 
(scatter mean tem, msymbol(o) msize(small) mcolor(black)  ) ///  
(rcap lb ub tem, lcolor(black)),  /// 
yline(0, lp(longdash) lc(gs8)) ///
xtitle("Temperature (°C)") /// 
ylabel(-0.04(0.02)0.04, ) ytitle("Marginal effects on output growth",color(black)) ///
title("Panel A: Annual Panel Estimates", color(black) ) ///
legend(off) graphregion(color(white))
graph save Graph "03figure\Figure7_1.gph", replace      
restore

clear all
use 02table\MakeFigure7.dta,clear 
preserve
keep if model ==2
tempfile stats
postfile stats mean lb ub using `stats'
forval i = 0/30 {
    qui {
        local varname T`i'
        summarize `varname',detail
        local median = r(p50)
        local se = r(sd) 

        local lb = `median' - 1.645*`se'
        local ub = `median' + 1.645*`se'

        post stats  (`median') (`lb') (`ub') 
	}
}
postclose stats

* 读取临时文件
use `stats', clear

gen tem = _n-1

twoway /// 
(scatter mean tem, msymbol(o) msize(small) mcolor(black)  ) ///  
(rcap lb ub tem, lcolor(black)),  /// 
yline(0, lp(longdash) lc(gs8)) ///
xtitle("Temperature (°C)") /// 
ylabel(-0.4(0.2)0.4, ) ///
title("Panel B: Long-difference Estimates", color(black) ) ///
legend(off) graphregion(color(white))
graph save Graph "03figure\Figure7_2.gph", replace      
restore
graph combine 03figure\Figure7_1.gph 03figure\Figure7_2.gph, cols(2)ysize(2)  imargin(vsmall) graphregion(color(white))