clear all

cd "~"
insheet using Data_and_Result_Comparison.csv ,clear

encode country, gen(country1)
xtset country1 year
gen lngdp = log(gdp)
gen dgdp = d.lngdp
*Data Comparison
drop if missing(gdp_b, dgdp, gdp_k)

*********Make Table A4*************
preserve
duplicates drop country1, force
summ country1
restore
summ gdp_b
summ dgdp
summ gdp_k

*********Make Table A5*************
reghdfe dgdp gdp_k, absorb(i.country1 i.year)
estimates store r1
reghdfe dgdp gdp_b, absorb(i.country1 i.year)
estimates store r2
reghdfe gdp_k dgdp , absorb(i.country1 i.year)
estimates store r3
reghdfe gdp_k gdp_b, absorb(i.country1 i.year )
estimates store r4
reghdfe gdp_b dgdp , absorb(i.country1 i.year)
estimates store r5
reghdfe gdp_b gdp_k, absorb(i.country1 i.year)
estimates store r6
esttab r1 r2 r3 r4 r5 r6 using "02table\TableA5.rtf", star(* .1 ** .05  *** .01) nogap nonumber replace se(%5.4f) r2 ar2 aic(%10.4f) bic(%10.4f)


*********Make Table A6*************
pwcorr gdp_b gdp_k dgdp, sig star (.01) print(.01)
spearman gdp_b gdp_k dgdp,pw star(0.01)
