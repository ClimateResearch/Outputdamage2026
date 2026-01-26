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

local gaps 5
local groups 0
local lags 0 1 
foreach gap of local gaps{

local deltaT = `gap'
gen N_usd = gdp

sum year
gen y_min = r(min)

gen period = floor((year - y_min)/`deltaT')

collapse(mean) gdppc tem pre pop ///
   (first) countsub1 country1 weight ///
   (count) N_usd  ///
   , by(gid_nmbr period)

keep if N_usd >1

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

reghdfe dgdp poor#c.(l(1/3).dgdp c.dT c.dT2 c.tem##c.tem c.l.tem c.l.tem#c.l.tem c.dP c.dP2 c.pre##c.pre c.l.pre c.l.pre##c.l.pre dpop)  [aweight=weight],absorb(i.gid_nmbr i.year) vce( cluster country1) 

foreach lag of local lags{
if "`lag'" == "1"{
		local title = "Panel A: Initial Output Response"
	}
if "`lag'" == "0"{
		local title = "Panel B: Subsequent Output Response"
	}
foreach group of local groups{
preserve
matrix results = J(31, 4, .) 
local index =1
forvalues i = 0/30 {

	qui lincom `group'.poor#c.l`lag'.tem+2*`i'*`group'.poor#c.l`lag'.tem#c.l`lag'.tem
	local T_`index' = r(estimate)
	local T_`index'_se= r(se)
	local ci_lower_`index' = `T_`index'' - 1.645*`T_`index'_se'
    local ci_upper_`index' = `T_`index'' + 1.645*`T_`index'_se'
    * 将结果存储到矩阵
    matrix results[`index', 1] = `i'
    matrix results[`index', 2] = `T_`index''
    matrix results[`index', 3] = `ci_lower_`index''
    matrix results[`index', 4] = `ci_upper_`index''
	local index = `index' +1
}

svmat results, names(col)
twoway ///
(rarea c3 c4 c1 , fcolor( "252 141 98%30") lcolor(%0) ) ///
(line c2 c1 , lcolor("252 141 98") )  ///
 , ///
ylabel(-0.4(0.2)0.4, labsize(small) nogrid) ///
yline(0,lwidth(thin) lcolor(gs10) ) ///
 legend(off) ytitle("Marginal effects on output growth",color(black)) xtitle("") title(`title',color(black) size(medium)) graphregion(color(white))
graph save Graph "03figure\Figure5_`lag'.gph",replace
restore
}
}
}
graph combine 03figure\Figure5_1.gph  03figure\Figure5_0.gph , cols(2)  imargin(vsmall) graphregion(color(white))
graph save Graph "03figure\Figure4.png", replace

