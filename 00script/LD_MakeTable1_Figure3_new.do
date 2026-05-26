clear all

cd "~"
insheet using GDPpc.csv ,clear

drop if missing(gdppc,tem,pre,pop)
encode country, gen(country1)
xtset gid_nmbr year 
gen lngdp = log(gdppc)
gen dgdp= lngdp-l.lngdp

*********Make Table 1*************
preserve
summ gdppc dgdp pop urb [aweight=pop]
summ tem pre [aweight=area]
duplicates drop gid_nmbr, force
summ gid_nmbr
duplicates drop country1, force
summ country1
restore

preserve
gen temw=tem*area
gen prew=pre*area
collapse(sum) temw prew area,  by(country1 year)
gen tem = temw/area
gen pre = prew/area
summ tem pre [aweight=area]
restore

*********Make Figure 3*************
local vars "gdppc dgdp tem pre "
* 循环处理每个变量
foreach var of local vars {
    * 循环处理每个权重
        if "`var'" == "tem" {
            local title = "Panel C: Temperature"
            local l2title = "Temperature (℃)"
            local color = "252 141 98"
			local weight = "area"
        }
        else if "`var'" == "pre" {
            local title = "Panel D: Precipitation"
            local l2title = "Precipitation (m)"
            local color = "60 180 160"
			local weight = "area"
        }
        else if "`var'" == "gdppc" {
            local title = "Panel A: GDP per capita"
            local l2title = "GDP per capita"
            local color = "140 160 200"
			local weight = "pop"
        }
        else if "`var'" == "dgdp" {
            local title = "Panel B: GDP per capita Growth"
            local l2title = "GDP per capita growth"
            local color = "180 120 180"
			local weight = "pop"
        }

        preserve
        
        * 计算加权均值和分位点
        collapse (mean) mean_temp=`var' [aweight=`weight'], by(year)
		
		 tsset year
        
        * 生成5年移动平均值 - 关键新增步骤
        tssmooth ma ma5_`var' = mean_temp, window(4 1 0)
		replace ma5_`var' = . if _n < 5

    * 绘制合并后的图形
    twoway ///
        (scatter mean_temp year, connect(direct) lpattern(solid) msymbol(none) mcolor(navy) lcolor("`color'") sort) ///
		(line ma5_`var' year, lpattern(dash) lcolor("`color'")) ///
        , title(`title',color(black)) l2title(`l2title') xtitle("") ytitle("") legend(off) graphregion(color(white))
    
    * 保存图形
    graph save "03figure\Figure1_`var'.gph", replace
	restore
}

graph combine 03figure\Figure1_gdppc.gph 03figure\Figure1_dgdp.gph 03figure\Figure1_tem.gph 03figure\Figure1_pre.gph  ,  graphregion(color(white)) 
graph save Graph "Figure3.png", replace

*********Unit Root Tests (Footnote 7)*************
local vars dgdp tem pre
foreach var of loc vars  {
	xtunitroot ht `var', demean
}


/*
histogram tem,  width(1)  discrete horizontal fraction ytitle("", height(0)) xtitle("", height(0)) ylabel(,nogrid) xlabel(none) xscale(off)  fcolor(bluishgray)  lcolor(bluishgray) plotregion(margin(zero)) title("")  subtitle("")    graphregion(color(white)) ysize(10)
*/
