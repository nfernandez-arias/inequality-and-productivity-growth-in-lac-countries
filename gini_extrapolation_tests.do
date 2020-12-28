clear
set more off

cd "Z:/home/nico/nfernand@princeton.edu/PhD - Thesis/Research/Inequality and productivity growth in LAC"

insheet using data/pwt91_cleaned_swiid.csv

encode alpha3, gen(alpha3code)

xtset alpha3code year

** Disposable gini

reghdfe gini_disp lrgdpnapercapita lcgdpopercapita_gap year, absorb(alpha3code) cluster(alpha3code)
reg D.(gini_disp lrgdpnapercapita lcgdpopercapita_gap), cluster(alpha3code)

reghdfe gini_disp i.alpha3code#c.year i.year, absorb(alpha3code)

reg D.(gini_disp) i.alpha3code i.year lrgdpnapercapita lcgdpopercapita_gap, robust
reg D.(gini_disp) i.year, robust cluster(alpha3code) noconstant
reg D.(D.(gini_disp)) i.year D.(lcgdpopercapita_gap D.(lrgdpnapercapita)), robust cluster(alpha3code)

gen lrgdpnapercapita2 = lrgdpnapercapita^2
gen lcgdpopercapita_gap2 = lcgdpopercapita_gap^2

reghdfe gini_disp lrgdpnapercapita lrgdpnapercapita2 lcgdpopercapita_gap lcgdpopercapita_gap2, absorb(alpha3code year) cluster(alpha3code)

** Market gini

reghdfe gini_mkt lrgdpnapercapita lcgdpopercapita_gap year, absorb(alpha3code) cluster(alpha3code)
reg D.(gini_mkt lrgdpnapercapita lcgdpopercapita_gap), cluster(alpha3code)


******* 
* 7-year regressions

insheet using data/pwt_swiid.csv
