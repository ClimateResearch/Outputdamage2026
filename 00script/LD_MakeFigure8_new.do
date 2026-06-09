clear all

cd "~"
insheet using GDPpc.csv ,clear

*********Make Figure 8*************

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

postfile MakeFigure7 T0 T1 T2 T3 T4 T5 T6 T7 T8 T9 T10 T11 T12 T13 T14 T15 T16 T17 T18 T19 T20 T21 T22 T23 T24 T25 T26 T27 T28 T29 T30 model using "02table\MakeFigure7.dta", replace

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

xtabond2 dgdp c.l(1/5).dgdp dT dT2 c.tem##c.tem dP dP2 c.pre##c.pre poor#c.(dT dT2 c.tem##c.tem dP dP2 c.pre##c.pre) dpop i.poor#i.year i.subc#c.year  [aweight=weight], iv(dT dT2 c.tem##c.tem dP dP2 c.pre##c.pre  poor#c.(dT dT2 c.tem##c.tem dP dP2 c.pre##c.pre) i.poor#i.year i.subc#c.year  dpop ) gmm(c.L(1/5).dgdp, l(6 8) ) nolevel cluster(country1)  artests(3) or

forvalues j = 0/30 {
		    lincom (c.tem#c.tem)*2*`j' + c.tem
		    scalar T_`j' = r(estimate)
		}
		
		scalar model = 1

		post MakeFigure7 (T_0) (T_1) (T_2) (T_3) (T_4) (T_5) (T_6) (T_7) (T_8) (T_9) (T_10) (T_11) (T_12) (T_13) (T_14) (T_15) (T_16) (T_17) (T_18) (T_19) (T_20) (T_21) (T_22) (T_23) (T_24) (T_25) (T_26) (T_27) (T_28) (T_29) (T_30) (model)
		
forvalues j = 0/30 {
		    lincom (c.tem#c.tem+1.poor#c.tem#c.tem)*2*`j' + c.tem + 1.poor#c.tem
		    scalar T_`j' = r(estimate)
		}
		
		scalar model = 2

		post MakeFigure7 (T_0) (T_1) (T_2) (T_3) (T_4) (T_5) (T_6) (T_7) (T_8) (T_9) (T_10) (T_11) (T_12) (T_13) (T_14) (T_15) (T_16) (T_17) (T_18) (T_19) (T_20) (T_21) (T_22) (T_23) (T_24) (T_25) (T_26) (T_27) (T_28) (T_29) (T_30) (model)
		
		
		
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
 
forvalues j = 0/30 {
		    lincom (c.l.tem#c.l.tem)*2*`j' + c.l.tem
		    scalar T_`j' = r(estimate)
		}
		
		scalar model = 3
		
		post MakeFigure7 (T_0) (T_1) (T_2) (T_3) (T_4) (T_5) (T_6) (T_7) (T_8) (T_9) (T_10) (T_11) (T_12) (T_13) (T_14) (T_15) (T_16) (T_17) (T_18) (T_19) (T_20) (T_21) (T_22) (T_23) (T_24) (T_25) (T_26) (T_27) (T_28) (T_29) (T_30) (model)

forvalues j = 0/30 {
		    lincom (c.l.tem#c.l.tem+1.poor#c.l.tem#c.l.tem)*2*`j' + c.l.tem+1.poor#c.l.tem
		    scalar T_`j' = r(estimate)
		}
		
		scalar model = 4
		
		post MakeFigure7 (T_0) (T_1) (T_2) (T_3) (T_4) (T_5) (T_6) (T_7) (T_8) (T_9) (T_10) (T_11) (T_12) (T_13) (T_14) (T_15) (T_16) (T_17) (T_18) (T_19) (T_20) (T_21) (T_22) (T_23) (T_24) (T_25) (T_26) (T_27) (T_28) (T_29) (T_30) (model)

}


dis `i'
}

* 关闭postfile
postclose MakeFigure7

use 02table\MakeFigure7.dta,clear

forvalues j = 1/2{
if "`j'" == "1"{
		local title = "Panel A: Annual Panel Estimates in Poor"
	}
if "`j'" == "2"{
		local title = "Panel B: Annual Panel Estimates in Rich"
	}
preserve
keep if model ==`j'
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
ylabel(-0.04(0.02)0.04, ) ytitle("Marginal effects on output growth",color(black) size(medsmall)) ///
title(`title', color(black) size(medium)) ///
legend(off) graphregion(color(white))
graph save Graph "03figure\Figure7_`j'.gph", replace  
restore    
} 


forvalues j = 3/4{
if "`j'" == "3"{
		local title = "Panel C: Long-difference Estimates in Poor"
	}
if "`j'" == "4"{
		local title = "Panel D: Long-difference Estimates in Rich"
	}
preserve
keep if model ==4
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
ylabel(-0.4(0.2)0.4, ) ytitle("Marginal effects on output growth",color(black) size(medsmall)) ///
title(`title', color(black) size(medium)) ///
legend(off) graphregion(color(white))
graph save Graph "03figure\Figure7_`j'.gph", replace  
restore    
} 

graph combine 03figure\Figure7_1.gph  03figure\Figure7_2.gph 03figure\Figure7_3.gph  03figure\Figure7_4.gph, cols(2)  imargin(vsmall) graphregion(color(white))
graph save Graph "03figure\Figure7.png", replace

