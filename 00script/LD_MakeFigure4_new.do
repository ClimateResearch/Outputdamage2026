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

egen gdppc_mean = mean(gdppc), by(gid_nmbr)
pctile pct = gdppc_mean, nq(100)
gen poor = 0 if gdppc_mean <= pct[50]
replace poor = 1 if gdppc_mean >pct[50] 


xtabond2 dgdp c.l(1/3).dgdp dT dT2 c.tem##c.tem dP dP2 c.pre##c.pre poor#c.(dT dT2 c.tem##c.tem dP dP2 c.pre##c.pre) dpop i.year i.subc#c.year  [aweight=weight], iv(dT dT2 c.tem##c.tem dP dP2 c.pre##c.pre  poor#c.(dT dT2 c.tem##c.tem dP dP2 c.pre##c.pre) i.year i.subc#c.year  dpop ) gmm(c.L(1/3).dgdp, l(4 6) ) nolevel cluster(country1)  artests(3) or
estimates store r1

matrix results = J(93, 5, .) 
local index =1
forvalues i = 0/30 {
	qui lincom c.tem+2*`i'*c.tem#c.tem
	local T_`index' = r(estimate)
	local T_`index'_se= r(se)
	local ci_lower_`index' = `T_`index'' - 1.645*`T_`index'_se'
    local ci_upper_`index' = `T_`index'' + 1.645*`T_`index'_se'
    * 将结果存储到矩阵
    matrix results[`index', 1] = `i'
    matrix results[`index', 2] = `T_`index''
    matrix results[`index', 3] = `ci_lower_`index''
    matrix results[`index', 4] = `ci_upper_`index''
	matrix results[`index', 5] = 3
	local index = `index'+1
}

xtabond2 dgdp c.l(1/5).dgdp dT dT2 c.tem##c.tem dP dP2 c.pre##c.pre poor#c.(dT dT2 c.tem##c.tem dP dP2 c.pre##c.pre) dpop i.year i.subc#c.year  [aweight=weight], iv(dT dT2 c.tem##c.tem dP dP2 c.pre##c.pre  poor#c.(dT dT2 c.tem##c.tem dP dP2 c.pre##c.pre) i.year i.subc#c.year  dpop ) gmm(c.L(1/5).dgdp, l(6 9) ) nolevel cluster(country1)  artests(3) or
estimates store r3

forvalues i = 0/30 {
	qui lincom c.tem+2*`i'*c.tem#c.tem
	local T_`index' = r(estimate)
	local T_`index'_se= r(se)
	local ci_lower_`index' = `T_`index'' - 1.645*`T_`index'_se'
    local ci_upper_`index' = `T_`index'' + 1.645*`T_`index'_se'
    * 将结果存储到矩阵
    matrix results[`index', 1] = `i'
    matrix results[`index', 2] = `T_`index''
    matrix results[`index', 3] = `ci_lower_`index''
    matrix results[`index', 4] = `ci_upper_`index''
	matrix results[`index', 5] = 5
	local index = `index'+1
}

xtabond2 dgdp c.l(1/8).dgdp dT dT2 c.tem##c.tem dP dP2 c.pre##c.pre poor#c.(dT dT2 c.tem##c.tem dP dP2 c.pre##c.pre) dpop i.year i.subc#c.year  [aweight=weight], iv(dT dT2 c.tem##c.tem dP dP2 c.pre##c.pre  poor#c.(dT dT2 c.tem##c.tem dP dP2 c.pre##c.pre) i.year i.subc#c.year  dpop ) gmm(c.L(1/8).dgdp, l(9 10) ) nolevel cluster(country1)  artests(3) or
estimates store r4
lincom c.tem+2*20*c.tem#c.tem
lincom c.tem+1.poor#c.tem+2*20*(c.tem#c.tem+1.poor#c.tem#c.tem)

forvalues i = 0/30 {
	qui lincom c.tem+2*`i'*c.tem#c.tem
	local T_`index' = r(estimate)
	local T_`index'_se= r(se)
	local ci_lower_`index' = `T_`index'' - 1.645*`T_`index'_se'
    local ci_upper_`index' = `T_`index'' + 1.645*`T_`index'_se'
    * 将结果存储到矩阵
    matrix results[`index', 1] = `i'
    matrix results[`index', 2] = `T_`index''
    matrix results[`index', 3] = `ci_lower_`index''
    matrix results[`index', 4] = `ci_upper_`index''
	matrix results[`index', 5] = 8
	local index = `index'+1
}
preserve
svmat results, names(col)
twoway ///
(line c2 c1 if c5 == 3, lcolor("252 141 98") )  ///
(rarea c3 c4 c1 if c5 == 5, fcolor( "60 180 160%30") lcolor(%0) ) ///
(line c2 c1  if c5 == 5,lcolor("60 180 160") )  ///
(line c2 c1  if c5 == 8,lcolor("240 180 50") )  ///
 , ///
ylabel(-0.04(0.02)0.04, labsize(small) nogrid) ///
yline(0,lwidth(thin) lcolor(gs10) ) ///
 legend(off) title("Panel A: Short-term Effects in Poor Regions",color(black)) ytitle("Marginal effects on output growth",color(black)) xtitle("Temperature(℃)",color(black) size(medium)) graphregion(color(white))
graph save Graph "03figure\Figure3_poor.gph",replace
restore

xtabond2 dgdp c.l(1/3).dgdp dT dT2 c.tem##c.tem dP dP2 c.pre##c.pre poor#c.(dT dT2 c.tem##c.tem dP dP2 c.pre##c.pre) dpop i.year i.subc#c.year  [aweight=weight], iv(dT dT2 c.tem##c.tem dP dP2 c.pre##c.pre  poor#c.(dT dT2 c.tem##c.tem dP dP2 c.pre##c.pre) i.year i.subc#c.year  dpop ) gmm(c.L(1/3).dgdp, l(4 6) ) nolevel cluster(country1)  artests(3) or
estimates store r1

matrix results = J(93, 5, .) 
local index =1
forvalues i = 0/30 {
	qui lincom c.tem+1.poor#c.tem+2*`i'*(c.tem#c.tem+1.poor#c.tem#c.tem)
	local T_`index' = r(estimate)
	local T_`index'_se= r(se)
	local ci_lower_`index' = `T_`index'' - 1.645*`T_`index'_se'
    local ci_upper_`index' = `T_`index'' + 1.645*`T_`index'_se'
    * 将结果存储到矩阵
    matrix results[`index', 1] = `i'
    matrix results[`index', 2] = `T_`index''
    matrix results[`index', 3] = `ci_lower_`index''
    matrix results[`index', 4] = `ci_upper_`index''
	matrix results[`index', 5] = 3
	local index = `index'+1
}

xtabond2 dgdp c.l(1/5).dgdp dT dT2 c.tem##c.tem dP dP2 c.pre##c.pre poor#c.(dT dT2 c.tem##c.tem dP dP2 c.pre##c.pre) dpop i.year i.subc#c.year  [aweight=weight], iv(dT dT2 c.tem##c.tem dP dP2 c.pre##c.pre  poor#c.(dT dT2 c.tem##c.tem dP dP2 c.pre##c.pre) i.year i.subc#c.year  dpop ) gmm(c.L(1/5).dgdp, l(6 9) ) nolevel cluster(country1)  artests(3) or
estimates store r3

forvalues i = 0/30 {
	qui lincom c.tem+1.poor#c.tem+2*`i'*(c.tem#c.tem+1.poor#c.tem#c.tem)
	local T_`index' = r(estimate)
	local T_`index'_se= r(se)
	local ci_lower_`index' = `T_`index'' - 1.645*`T_`index'_se'
    local ci_upper_`index' = `T_`index'' + 1.645*`T_`index'_se'
    * 将结果存储到矩阵
    matrix results[`index', 1] = `i'
    matrix results[`index', 2] = `T_`index''
    matrix results[`index', 3] = `ci_lower_`index''
    matrix results[`index', 4] = `ci_upper_`index''
	matrix results[`index', 5] = 5
	local index = `index'+1
}

xtabond2 dgdp c.l(1/8).dgdp dT dT2 c.tem##c.tem dP dP2 c.pre##c.pre poor#c.(dT dT2 c.tem##c.tem dP dP2 c.pre##c.pre) dpop i.year i.subc#c.year  [aweight=weight], iv(dT dT2 c.tem##c.tem dP dP2 c.pre##c.pre  poor#c.(dT dT2 c.tem##c.tem dP dP2 c.pre##c.pre) i.year i.subc#c.year  dpop ) gmm(c.L(1/8).dgdp, l(9 10) ) nolevel cluster(country1)  artests(3) or
estimates store r4
lincom c.tem+2*20*c.tem#c.tem
lincom c.tem+1.poor#c.tem+2*20*(c.tem#c.tem+1.poor#c.tem#c.tem)

forvalues i = 0/30 {
	qui lincom c.tem+1.poor#c.tem+2*`i'*(c.tem#c.tem+1.poor#c.tem#c.tem)
	local T_`index' = r(estimate)
	local T_`index'_se= r(se)
	local ci_lower_`index' = `T_`index'' - 1.645*`T_`index'_se'
    local ci_upper_`index' = `T_`index'' + 1.645*`T_`index'_se'
    * 将结果存储到矩阵
    matrix results[`index', 1] = `i'
    matrix results[`index', 2] = `T_`index''
    matrix results[`index', 3] = `ci_lower_`index''
    matrix results[`index', 4] = `ci_upper_`index''
	matrix results[`index', 5] = 8
	local index = `index'+1
}

svmat results, names(col)
twoway ///
(line c2 c1 if c5 == 3, lcolor("252 141 98") )  ///
(rarea c3 c4 c1 if c5 == 5, fcolor( "60 180 160%30") lcolor(%0) ) ///
(line c2 c1  if c5 == 5,lcolor("60 180 160") )  ///
(line c2 c1  if c5 == 8,lcolor("240 180 50") )  ///
 , ///
ylabel(-0.04(0.02)0.04, labsize(small) nogrid) ///
yline(0,lwidth(thin) lcolor(gs10) ) ///
 legend(off) title("Panel B: Short-term Effects in Rich Regions",color(black))  ytitle("Marginal effects on output growth",color(black)) xtitle("Temperature(℃)",color(black) size(medium)) graphregion(color(white))
graph save Graph "03figure\Figure3_rich.gph",replace

graph combine 03figure\Figure3_poor.gph  03figure\Figure3_rich.gph, cols(2)  imargin(vsmall) graphregion(color(white))
graph save Graph "03figure\Figure3.png", replace