clear all

cd "~"
insheet using GDPpc.csv ,clear

*********Make Table 7*************

drop if missing(gdppc, tem, pre, pop)

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

gen T2 = tem*tem
gen P2 = pre*pre

gen dT2= d.T2
gen dP2 = d.P2

gen year2=year*year


egen gdppc_mean = mean(gdppc), by(gid_nmbr)
pctile pct = gdppc_mean, nq(100)
gen poor = 0 if gdppc_mean <= pct[50]
replace poor = 1 if gdppc_mean >pct[50] 

local lags 3 5
local reg = 0
matrix results = J(8, 3, .) 
foreach lag of local lags {

local lmin = `lag' +1
local lmax = `lag' +1

local index =1
local reg = 1 + `reg'
local T2var c.tem#c.tem
local P2var c.pre#c.pre

forvalue l=1/`lag' {
    local T2var `T2var' c.l`l'.tem#c.l`l'.tem
    local P2var `P2var' c.l`l'.pre#c.l`l'.pre
}

xtabond2 dgdp c.l(1/`lag').dgdp c.l(0/`lag').tem `T2var' c.l(0/`lag').pre `P2var'  poor#( c.l(0/`lag').tem `T2var' c.l(0/`lag').pre `P2var' ) c.dpop i.poor#i.year i.subc#c.year [aweight=weight], iv(c.l(0/`lag').tem `T2var' c.l(0/`lag').pre `P2var'  poor#( c.l(0/`lag').tem `T2var' c.l(0/`lag').pre `P2var' ) c.dpop i.poor#i.year i.subc#c.year) gmm(c.L(1/`lag').dgdp, l(`lmin' `lmax') ) nolevel cluster(country1)  artests(3) or

    local sumT_0 0
    local sumT2_0 0
	local sumT_1 0
    local sumT2_1 0

forvalues l = 0/`lag' {
    local sumT2_0 `sumT2_0' + cL`l'.tem#cL`l'.tem
    local sumT_0  `sumT_0'  + cL`l'.tem
}
	
forvalues l = 0/`lag' {
    local sumT2_1 `sumT2_1' + 1.poor#cL`l'.tem#cL`l'.tem+cL`l'.tem#cL`l'.tem
    local sumT_1  `sumT_1'  + 1.poor#cL`l'.tem+cL`l'.tem
}

    lincom `sumT2_0'
	matrix results[`index', `reg'] = r(estimate)
	local index = `index'+1
	matrix results[`index', `reg'] = round(r(se), 0.0001)

    lincom `sumT_0'
	local index = `index'+1
    matrix results[`index', `reg'] = r(estimate)
	local index = `index'+1
	matrix results[`index', `reg'] = round(r(se), 0.0001)
	
	lincom `sumT2_1'
    local index = `index'+1
    matrix results[`index', `reg'] = r(estimate)
	local index = `index'+1
	matrix results[`index', `reg'] = round(r(se), 0.0001)
	
    lincom `sumT_1'
    local index = `index'+1
    matrix results[`index', `reg'] = r(estimate)
	local index = `index'+1
	matrix results[`index', `reg'] = round(r(se), 0.0001)
	
	lincom `sumT_0'+2*20*(`sumT2_0')
	lincom `sumT_1'+2*10*(`sumT2_1')
}

local vars sumT2_0 sumT_0 sumT2_1 sumT_1
local row_names ""
foreach var of local vars {
    local clean_name = subinstr("`var'", "c.", "", .)
    local row_names "`row_names' `clean_name' `clean_name'_se"
}
matrix rownames results = `row_names'

esttab matrix(results,fmt(%9.3g)) using "02table\Table7.rtf",  se(%5.4f)  replace
