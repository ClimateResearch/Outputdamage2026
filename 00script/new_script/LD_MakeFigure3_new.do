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

local lags 3 5 
matrix results = J(124, 5, .) 
local index =1
	
preserve
foreach lag of local lags{
qui	reghdfe dgdp poor#c.(l(1/`lag').dgdp c.dT c.dT2 c.tem##c.tem c.dP c.dP2 c.pre##c.pre dpop) [aweight=weight],absorb(i.gid_nmbr i.countsub1#i.year) vce( cluster country1)

forvalues i = 0/30 {

	qui lincom 0.poor#c.tem+2*`i'*0.poor#c.tem#c.tem
	local T_`index' = r(estimate)
	local T_`index'_se= r(se)
	local ci_lower_`index' = `T_`index'' - 1.645*`T_`index'_se'
    local ci_upper_`index' = `T_`index'' + 1.645*`T_`index'_se'
    * 将结果存储到矩阵
    matrix results[`index', 1] = `i'
    matrix results[`index', 2] = `T_`index''
    matrix results[`index', 3] = `ci_lower_`index''
    matrix results[`index', 4] = `ci_upper_`index''
	matrix results[`index', 5] = `lag'
	local index = `index'+1
}
}
* 将矩阵保存到一个新数据集
svmat results, names(col)
twoway ///
(rarea c3 c4 c1 if c5 == 3, fcolor( "252 141 98%30") lcolor(%0) ) ///
(line c2 c1 if c5 == 3, lcolor("252 141 98") )  ///
(rarea c3 c4 c1 if c5 == 5, fcolor( "60 180 160%30") lcolor(%0) ) ///
(line c2 c1  if c5 == 5,lcolor("60 180 160") )  ///
 , ///
ylabel(-0.04(0.02)0.04, labsize(small) nogrid) ///
yline(0,lwidth(thin) lcolor(gs10) ) ///
 legend(off) ytitle("Marginal effects",color(black)) xtitle("Temperature(℃)",color(black) size(medium)) graphregion(color(white))
graph save Graph "03figure\Figure3.gph",replace
restore