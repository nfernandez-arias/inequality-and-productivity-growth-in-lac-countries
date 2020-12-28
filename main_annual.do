clear
set more off

cd "Z:/home/nico/Insync/nfernand@princeton.edu/Google Drive/PhD - Thesis/Research/inequality-and-convergence-in-lac"

insheet using data/pwt91_cleaned_swiid.csv

encode alpha3, gen(alpha3_code)

xtset alpha3_code year

keep if year >= 1968
* Generate islac interactions

gen islacPre1990 = 0
gen islacPost1990 = 0
gen pre1990 = 0
gen post1990 = 0

replace pre1990 = 1 if year <= 1989
replace post1990 = 1 if year >= 1996

replace islacPre1990 = 1 if islac == 1 & year <= 1989

* Avoiding residual group whose inclusive trailing 7-year windows intersect 1990
replace islacPost1990 = 1 if islac == 1 & year >= 1996

* Construct variable labels

label variable lrgdpnapercapita_chg7 "Per capita output growth"
label variable lrtfpna_chg7 "Productivity growth"
label variable lrgdpnapercapita_chg7_nontfp "Factor accumulation"
label variable lcgdpopercapita_gap_avg7 "Per capita output gap (rel. USA)"

label variable islac "LAC"
label variable iseat "East Asian Tiger"
label variable isafrica "Africa"

label variable islacPre1990 "LAC $\times$ pre 1990"
label variable islacPost1990 "LAC $\times$ post 1990"


** Regressions

* No country / region dummies

quietly eststo: reg lrtfpna_chg7 lcgdpopercapita_gap_avg7 ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp lcgdpopercapita_gap_avg7 ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 lcgdpopercapita_gap_avg7 ib2017.year, robust cluster(alpha3_code)

esttab using "figures/tables/sec2_mainReg_noCountryRegionDummies_annual.tex", se star(* 0.1 ** 0.05 *** 0.01) label replace compress substitute(\_ _) keep(lcgdpopercapita_gap_avg7) addnote("\textbf{Regression equation:} \(g_{it} = \alpha + \gamma_t + \beta \big(\log (\frac{\textrm{cgdpo}_{it}}{\textrm{pop}_{it}} ) - \log (\frac{\textrm{cgdpo}_{USA,t}}{\textrm{pop}_{USA,t}}  ) \big) + \epsilon_{it}\)") wrap style(tex) title(Growth dynamics worldwide) booktabs b(a2)
esttab using "figures/tables/sec2_mainReg_noCountryRegionDummies_annual.csv", label replace cells(b) nonumbers plain keep(lcgdpopercapita_gap_avg7 *.year _cons)
eststo clear

preserve

keep if hasginidisp == 1

quietly eststo: reg lrtfpna_chg7 lcgdpopercapita_gap_avg7 ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp lcgdpopercapita_gap_avg7 ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 lcgdpopercapita_gap_avg7 ib2017.year, robust cluster(alpha3_code)

esttab using "figures/tables/sec2_mainReg_noCountryRegionDummies_SWIDsample_annual.tex", se star(* 0.1 ** 0.05 *** 0.01) label replace compress substitute(\_ _) keep(lcgdpopercapita_gap_avg7) addnote("\textbf{Regression equation:} \(g_{it} = \alpha + \gamma_t + \beta \big(\log (\frac{\textrm{cgdpo}_{it}}{\textrm{pop}_{it}} ) - \log (\frac{\textrm{cgdpo}_{USA,t}}{\textrm{pop}_{USA,t}}  ) \big) + \epsilon_{it}\)") wrap style(tex) title(Growth dynamics worldwide (country-years with gini data)) booktabs b(a2)
esttab using "figures/tables/sec2_mainReg_noCountryRegionDummies_SWIDsample_annual.csv", label replace cells(b) nonumbers plain keep(lcgdpopercapita_gap_avg7 *.year _cons)
eststo clear

restore


* Reg with only LAC dummy

quietly eststo: reg lrtfpna_chg7 lcgdpopercapita_gap_avg7 islac ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp lcgdpopercapita_gap_avg7 islac ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 lcgdpopercapita_gap_avg7 islac ib2017.year, robust cluster(alpha3_code)

esttab using "figures/tables/sec2_mainReg_onlyLACdummy_annual.tex", se label star(* 0.1 ** 0.05 *** 0.01) replace compress substitute(\_ _) drop(_cons *.year) addnote("\textbf{Regression equation:} \(g_{it} = \alpha + \sigma \mathbf{1}_{\textrm{LAC}} + \gamma_t + \beta \big(\log (\frac{\textrm{cgdpo}_{it}}{\textrm{pop}_{it}} ) - \log (\frac{\textrm{cgdpo}_{USA,t}}{\textrm{pop}_{USA,t}}  ) \big) + \epsilon_{it}\)") wrap style(tex) title(Statistical significance of LAC growth gap (Non-LAC Benchmark)) booktabs b(a2)

eststo clear

preserve

keep if hasginidisp == 1

quietly eststo: reg lrtfpna_chg7 lcgdpopercapita_gap_avg7 islac ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp lcgdpopercapita_gap_avg7 islac ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 lcgdpopercapita_gap_avg7 islac ib2017.year, robust cluster(alpha3_code)

esttab using "figures/tables/sec2_mainReg_onlyLACdummy_SWIDsample_annual.tex", se label star(* 0.1 ** 0.05 *** 0.01) replace compress substitute(\_ _) drop(_cons *.year) addnote("\textbf{Regression equation:} \(g_{it} = \alpha + \sigma \mathbf{1}_{\textrm{LAC}} + \gamma_t + \beta \big(\log (\frac{\textrm{cgdpo}_{it}}{\textrm{pop}_{it}} ) - \log (\frac{\textrm{cgdpo}_{USA,t}}{\textrm{pop}_{USA,t}}  ) \big) + \epsilon_{it}\)") wrap style(tex) title(Statistical significance of LAC growth gap (Non-LAC Benchmark, country-years with gini data)) booktabs b(a2)

eststo clear

restore

* Reg with LAC country dummies (but no other countries)

encode countrylac, gen(countrylac_code)

quietly eststo: reg lrtfpna_chg7 lcgdpopercapita_gap_avg7 ib2017.year ib(last).countrylac_code, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp lcgdpopercapita_gap_avg7 ib2017.year ib(last).countrylac_code, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 lcgdpopercapita_gap_avg7 ib2017.year ib(last).countrylac_code, robust cluster(alpha3_code)

esttab using "figures/tables/sec2_mainReg_LACCountryDummies_annual.tex", label star(* 0.1 ** 0.05 *** 0.01) replace compress drop(_cons *.year) nobaselevels nogaps not noparentheses substitute(\_ _) addnote("\textbf{Regression equation:} \(g_{it} = \alpha  + \omega_i + \gamma_t + \beta \big(\log (\frac{\textrm{cgdpo}_{it}}{\textrm{pop}_{it}} ) - \log (\frac{\textrm{cgdpo}_{USA,t}}{\textrm{pop}_{USA,t}}  ) \big) + \epsilon_{it}\)" "Estimated values of \(\omega_i\) for each country \(i\) shown above.") wrap style(tex) title(Statistical significance of LAC individual country growth gaps (Non-LAC Benchmark)) booktabs b(a2) 
*tex3pt "figures/tables/sec2_mainReg_LACCountryDummies_annual.tex" using "figures/tables/sec2_mainReg_LACCountryDummies_fixed_annual.tex", fontsize(\footnotesize)
esttab using "figures/tables/sec2_mainReg_LACCountryDummies_annual.csv", not label replace keep(*.countrylac_code) plain
eststo clear

preserve

keep if hasginidisp == 1

quietly eststo: reg lrtfpna_chg7 lcgdpopercapita_gap_avg7 ib2017.year ib(last).countrylac_code, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp lcgdpopercapita_gap_avg7 ib2017.year ib(last).countrylac_code, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 lcgdpopercapita_gap_avg7 ib2017.year ib(last).countrylac_code, robust cluster(alpha3_code)

esttab using "figures/tables/sec2_mainReg_LACCountryDummies_SWIDsample_annual.tex", label star(* 0.1 ** 0.05 *** 0.01) replace compress drop(_cons *.year) nobaselevels nogaps not noparentheses substitute(\_ _) addnote("\textbf{Regression equation:} \(g_{it} = \alpha  + \omega_i + \gamma_t + \beta \big(\log (\frac{\textrm{cgdpo}_{it}}{\textrm{pop}_{it}} ) - \log (\frac{\textrm{cgdpo}_{USA,t}}{\textrm{pop}_{USA,t}}  ) \big) + \epsilon_{it}\)" "Estimated values of \(\omega_i\) for each country \(i\) shown above.") wrap style(tex) title(Statistical significance of LAC individual country growth gaps (Non-LAC Benchmark, country-years with gini data)) booktabs b(a2) 
*tex3pt "figures/tables/sec2_mainReg_LACCountryDummies_annual.tex" using "figures/tables/sec2_mainReg_LACCountryDummies_fixed_annual.tex", fontsize(\footnotesize)
esttab using "figures/tables/sec2_mainReg_LACCountryDummies_SWIDsample_annual.csv", not label replace keep(*.countrylac_code) plain
eststo clear

restore

* Reg with LAC'-pre1990 and LAC-post1990 dummies

quietly eststo: reg lrtfpna_chg7 lcgdpopercapita_gap_avg7 islacPre1990 islacPost1990 ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp lcgdpopercapita_gap_avg7 islacPre1990 islacPost1990 ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 lcgdpopercapita_gap_avg7 islacPre1990 islacPost1990 ib2017.year, robust cluster(alpha3_code)

esttab using "figures/tables/sec2_mainReg_LACdummy_PrePost_annual.tex", se label star(* 0.1 ** 0.05 *** 0.01) replace compress drop(_cons *.year) substitute(\_ _) addnote("\textbf{Regression equation:} \(g_{it} = \alpha + \sigma_1 \mathbf{1}_{\textrm{LAC,pre1990}} + \sigma_2 \mathbf{1}_{\textrm{LAC,post1990}} + \gamma_t + \beta \big(\log (\frac{\textrm{cgdpo}_{it}}{\textrm{pop}_{it}} ) - \log (\frac{\textrm{cgdpo}_{USA,t}}{\textrm{pop}_{USA,t}}  ) \big) + \epsilon_{it}\)") wrap style(tex) title(Statistical significance of LAC growth gap, pre- and post-1990 (Non-LAC Benchmark)) booktabs b(a2)
eststo clear

preserve

keep if hasginidisp == 1

quietly eststo: reg lrtfpna_chg7 lcgdpopercapita_gap_avg7 islacPre1990 islacPost1990 ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp lcgdpopercapita_gap_avg7 islacPre1990 islacPost1990 ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 lcgdpopercapita_gap_avg7 islacPre1990 islacPost1990 ib2017.year, robust cluster(alpha3_code)

esttab using "figures/tables/sec2_mainReg_LACdummy_PrePost_SWIDsample_annual.tex", se label star(* 0.1 ** 0.05 *** 0.01) replace compress drop(_cons *.year) substitute(\_ _) addnote("\textbf{Regression equation:} \(g_{it} = \alpha + \sigma_1 \mathbf{1}_{\textrm{LAC,pre1990}} + \sigma_2 \mathbf{1}_{\textrm{LAC,post1990}} + \gamma_t + \beta \big(\log (\frac{\textrm{cgdpo}_{it}}{\textrm{pop}_{it}} ) - \log (\frac{\textrm{cgdpo}_{USA,t}}{\textrm{pop}_{USA,t}}  ) \big) + \epsilon_{it}\)") wrap style(tex) title(Statistical significance of LAC growth gap, pre- and post-1990 (Non-LAC Benchmark, country-years with gini data)) booktabs b(a2)
eststo clear

restore

* Reg with country-pre1990 and country-post1990 dummies (only for LAC countries)

quietly eststo: reg lrtfpna_chg7 lcgdpopercapita_gap_avg7 ib2017.year ib(last).countrylac_code#i.pre1990 ib(last).countrylac_code#i.post1990, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp lcgdpopercapita_gap_avg7 ib2017.year ib(last).countrylac_code#i.pre1990 ib(last).countrylac_code#i.post1990, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 lcgdpopercapita_gap_avg7 ib2017.year ib(last).countrylac_code#i.pre1990 ib(last).countrylac_code#i.post1990, robust cluster(alpha3_code)

esttab using "figures/tables/sec2_mainReg_LACCountryDummies_PrePost_annual.tex", label star(* 0.1 ** 0.05 *** 0.01) replace compress nogaps not drop(_cons *.year) noparentheses  substitute(\_ _) addnote("\textbf{Regression equation:} \(g_{it} = \alpha + \omega_{i\tau} + \gamma_t + \beta \big(\log (\frac{\textrm{cgdpo}_{it}}{\textrm{pop}_{it}} ) - \log (\frac{\textrm{cgdpo}_{USA,t}}{\textrm{pop}_{USA,t}}  ) \big) + \epsilon_{it}\)" "Variable \(\tau\) is equal to 1 if \(t > 1990\), 0 otherwise. Estimated values of \(\omega_{i\tau}\) for each country \(i\) shown above.") wrap style(tex) title(Statistical significance of LAC individual country growth gaps, pre- and post-1990 (Non-LAC Benchmark)) booktabs b(a2) 
*tex3pt "figures/tables/sec2_mainReg_LACCountryDummies_PrePost_annual.tex" using "figures/tables/sec2_mainReg_LACCountryDummies_PrePost_fixed_annual.tex", fontsize(\footnotesize)
esttab using "figures/tables/sec2_mainReg_LACCountryDummies_PrePost_annual.csv", se label replace 
esttab using "figures/tables/sec2_mainReg_LACCountryDummies_PrePost_clean_annual.csv", se label replace keep(*.countrylac_code*1990) plain nobaselevels
eststo clear

preserve

keep if hasginidisp == 1

quietly eststo: reg lrtfpna_chg7 lcgdpopercapita_gap_avg7 ib2017.year ib(last).countrylac_code#i.pre1990 ib(last).countrylac_code#i.post1990, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp lcgdpopercapita_gap_avg7 ib2017.year ib(last).countrylac_code#i.pre1990 ib(last).countrylac_code#i.post1990, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 lcgdpopercapita_gap_avg7 ib2017.year ib(last).countrylac_code#i.pre1990 ib(last).countrylac_code#i.post1990, robust cluster(alpha3_code)

esttab using "figures/tables/sec2_mainReg_LACCountryDummies_PrePost_SWIDsample_annual.tex", label star(* 0.1 ** 0.05 *** 0.01) replace compress nogaps nobaselevels not drop(_cons *.year) noparentheses  substitute(\_ _) addnote("\textbf{Regression equation:} \(g_{it} = \alpha + \omega_{i\tau} + \gamma_t + \beta \big(\log (\frac{\textrm{cgdpo}_{it}}{\textrm{pop}_{it}} ) - \log (\frac{\textrm{cgdpo}_{USA,t}}{\textrm{pop}_{USA,t}}  ) \big) + \epsilon_{it}\)" "Variable \(\tau\) is equal to 1 if \(t > 1990\), 0 otherwise. Estimated values of \(\omega_{i\tau}\) for each country \(i\) shown above.") wrap style(tex) title(Statistical significance of LAC individual country growth gaps, pre- and post-1990 (Non-LAC Benchmark, country-years with gini data)) booktabs b(a2) 
*tex3pt "figures/tables/sec2_mainReg_LACCountryDummies_PrePost_annual.tex" using "figures/tables/sec2_mainReg_LACCountryDummies_PrePost_fixed_annual.tex", fontsize(\footnotesize)
esttab using "figures/tables/sec2_mainReg_LACCountryDummies_PrePost_SWIDsample_annual.csv", se label replace 
esttab using "figures/tables/sec2_mainReg_LACCountryDummies_PrePost_clean_SWIDsample_annual.csv", se label replace keep(*.countrylac_code*1990) plain nobaselevels
eststo clear

restore




* Reg with only LAC dummy and LAC dummy interaction with lag gap

gen islac_nonlagGap = islac * lcgdpopercapita_gap_avg7
label variable islac_nonlagGap "LAC * RGDP gap (rel. USA)"

quietly eststo: reg lrtfpna_chg7 lcgdpopercapita_gap_avg7 islac_nonlagGap islac ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp lcgdpopercapita_gap_avg7 islac_nonlagGap islac ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 lcgdpopercapita_gap_avg7 islac_nonlagGap islac ib2017.year, robust cluster(alpha3_code)

esttab using "figures/tables/sec2_mainReg_onlyLACdummy_LACnonlagGapInteraction_annual.tex", se label replace style(tex) 
eststo clear


* NON LAG GAP Reg with only LAC dummy and LAC dummy interaction with all variables (equiv. to two separate regressions)

quietly eststo: reg lrtfpna_chg7 lcgdpopercapita_gap_avg7 islac_nonlagGap islac ib2017.year ib2017.year#i.islac, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp lcgdpopercapita_gap_avg7 islac_nonlagGap islac ib2017.year ib2017.year#i.islac, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 lcgdpopercapita_gap_avg7 islac_nonlagGap islac ib2017.year ib2017.year#i.islac, robust cluster(alpha3_code)

* csv with some coefficients, for use in constructing predictions
esttab using "figures/tables/sec2_mainReg_timeDummies_LACinteractionsAll_nonlagGap_annual.csv", label replace cells(b) nonumbers plain keep(lcgdpopercapita_gap_avg7 islac_nonlagGap *.year _cons)

* csv with ALL coefficients, including interactions etc
esttab using "figures/tables/excel/mainRegLACdummiesInteractions_allCoefficients_annual.csv", se label replace
esttab using "figures/tables/sec2_mainReg_timeDummies_LACinteractionsAll_nonlagGap_annual.tex", label replace se keep(lcgdpopercapita_gap_avg7 islac_nonlagGap islac *.year _cons) compress substitute(\_ _) addnote("\textbf{Regression equation:} \(g_{it} = \alpha + \gamma_t + \beta \big(\log (\frac{\textrm{cgdpo}_{it}}{\textrm{pop}_{it}} ) - \log (\frac{\textrm{cgdpo}_{USA,t}}{\textrm{pop}_{USA,t}}  ) \big) + \epsilon_{it}\), " "plus LAC dummies and interactions with LAC.") wrap style(tex) title(Statistical significance of difference in unconditional convergence / divergence, LAC vs non-LAC) booktabs b(a2)
eststo clear


* NON LAG GAP Reg with only NON-LAC sample

preserve

keep if islac == 0

quietly eststo: reg lrtfpna_chg7 lcgdpopercapita_gap_avg7 ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp lcgdpopercapita_gap_avg7 ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 lcgdpopercapita_gap_avg7 ib2017.year, robust cluster(alpha3_code)

* csv with some coefficients, for use in constructing predictions
esttab using "figures/tables/sec2_mainReg_nonLACsample_annual.csv", label replace cells(b) nonumbers plain keep(lcgdpopercapita_gap_avg7 *.year _cons)

* csv with ALL coefficients, including interactions etc
esttab using "figures/tables/excel/mainReg_nonLACsample_allCoefficients_annual.csv", se label replace
esttab using "figures/tables/sec2_mainReg_nonLACsample_annual.tex", keep(lcgdpopercapita_gap_avg7) star(* 0.1 ** 0.05 *** 0.01) se label replace compress substitute(\_ _) addnote("\textbf{Regression equation:} \(g_{it} = \alpha + \gamma_t + \beta \big(\log (\frac{\textrm{cgdpo}_{it}}{\textrm{pop}_{it}} ) - \log (\frac{\textrm{cgdpo}_{USA,t}}{\textrm{pop}_{USA,t}}  ) \big) + \epsilon_{it}\)" "\textbf{Notes:} Only non-LAC sample.") wrap style(tex) title(Growth dynamics in non-LAC countries) booktabs b(a2)

eststo clear


keep if hasginidisp == 1

quietly eststo: reg lrtfpna_chg7 lcgdpopercapita_gap_avg7 ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp lcgdpopercapita_gap_avg7 ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 lcgdpopercapita_gap_avg7 ib2017.year, robust cluster(alpha3_code)

* csv with some coefficients, for use in constructing predictions
esttab using "figures/tables/sec2_mainReg_nonLACsample_SWIDsample_annual.csv", label replace cells(b) nonumbers plain keep(lcgdpopercapita_gap_avg7 *.year _cons)

* csv with ALL coefficients, including interactions etc
esttab using "figures/tables/excel/mainReg_nonLACsample_allCoefficients_SWIDsample_annual.csv", se label replace
esttab using "figures/tables/sec2_mainReg_nonLACsample_SWIDsample_annual.tex", keep(lcgdpopercapita_gap_avg7) star(* 0.1 ** 0.05 *** 0.01) se label replace compress substitute(\_ _) addnote("\textbf{Regression equation:} \(g_{it} = \alpha + \gamma_t + \beta \big(\log (\frac{\textrm{cgdpo}_{it}}{\textrm{pop}_{it}} ) - \log (\frac{\textrm{cgdpo}_{USA,t}}{\textrm{pop}_{USA,t}}  ) \big) + \epsilon_{it}\)" "\textbf{Notes:} Only non-LAC sample.") wrap style(tex) title(Growth dynamics in non-LAC countries (country-years with gini data)) booktabs b(a2)

eststo clear


restore

* NON LAG GAP Reg with only LAC sample

preserve

keep if islac == 1

quietly eststo: reg lrtfpna_chg7 lcgdpopercapita_gap_avg7 ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp lcgdpopercapita_gap_avg7 ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 lcgdpopercapita_gap_avg7 ib2017.year, robust cluster(alpha3_code)

* csv with some coefficients, for use in constructing predictions
esttab using "figures/tables/sec2_mainReg_LACsample_annual.csv", label replace cells(b) nonumbers plain keep(lcgdpopercapita_gap_avg7 *.year _cons)

* csv with ALL coefficients, including interactions etc
esttab using "figures/tables/excel/mainReg_LACsample_allCoefficients_annual.csv", se label replace
esttab using "figures/tables/sec2_mainReg_LACsample_annual.tex", keep(lcgdpopercapita_gap_avg7) se label replace compress substitute(\_ _) addnote("\textbf{Regression equation:} \(g_{it} = \alpha + \gamma_t + \beta \big(\log (\frac{\textrm{cgdpo}_{it}}{\textrm{pop}_{it}} ) - \log (\frac{\textrm{cgdpo}_{USA,t}}{\textrm{pop}_{USA,t}}  ) \big) + \epsilon_{it}\)" "\textbf{Notes:} Only LAC sample.") wrap style(tex) title(Growth dynamics in LAC countries) booktabs b(a2)


eststo clear

restore



**** East Asian Tiger Benchmark
* same as above, but with East Asia dummy  and interactions as well

gen iseat_nonlagGap = iseat * lcgdpopercapita_gap_avg7
label variable iseat_nonlagGap "LAC * RGDP gap (rel. USA)"

quietly eststo: reg lrtfpna_chg7 lcgdpopercapita_gap_avg7 islac_nonlagGap islac ib2017.year ib2017.year#i.islac, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp lcgdpopercapita_gap_avg7 islac_nonlagGap islac ib2017.year ib2017.year#i.islac, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 lcgdpopercapita_gap_avg7 islac_nonlagGap islac iseat_nonlagGap iseat ib2017.year ib2017.year#i.islac ib2017.year#i.iseat, robust cluster(alpha3_code)

esttab using "figures/tables/sec2_mainReg_timeDummies_LACandEATinteractionsAll_nonlagGap_annual.csv", label replace cells(b) nonumbers plain keep(lcgdpopercapita_gap_avg7 islac_nonlagGap *.year _cons)
esttab using "figures/tables/sec2_mainReg_timeDummies_LACandEATinteractionsAll_nonlagGap_annual.tex", label replace style(tex) se keep(lcgdpopercapita_gap_avg7 islac_nonlagGap islac iseat_nonlagGap iseat)

eststo clear

**** Africa Benchmark
* same as above, but with East Asia dummy  and interactions as well

gen isafrica_nonlagGap = isafrica * lcgdpopercapita_gap_avg7
label variable isafrica_nonlagGap "LAC * RGDP gap (rel. USA)"

quietly eststo: reg lrtfpna_chg7 lcgdpopercapita_gap_avg7 islac_nonlagGap islac ib2017.year ib2017.year#i.islac, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp lcgdpopercapita_gap_avg7 islac_nonlagGap islac ib2017.year ib2017.year#i.islac, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 lcgdpopercapita_gap_avg7 islac_nonlagGap islac isafrica_nonlagGap isafrica ib2017.year ib2017.year#i.islac ib2017.year#i.isafrica, robust cluster(alpha3_code)

esttab using "figures/tables/sec2_mainReg_timeDummies_LACandAFRinteractionsAll_nonlagGap_annual.csv", label replace cells(b) nonumbers plain keep(lcgdpopercapita_gap_avg7 islac_nonlagGap *.year _cons)
esttab using "figures/tables/sec2_mainReg_timeDummies_LACandAFRinteractionsAll_nonlagGap_annual.tex", label replace style(tex) se keep(lcgdpopercapita_gap_avg7 islac_nonlagGap islac isafrica_nonlagGap isafrica)

eststo clear




**** Inequality analysis

label variable gini_disp "Gini (disp. inc.)"
label variable gini_mkt "Gini (mkt. inc.)"
gen gini_disp2 = gini_disp^2
gen gini_mkt2 = gini_mkt^2 

label variable gini_disp2 "Gini squared (disp. inc.)"
label variable gini_mkt2 "Gini squared (mkt. inc.)"

** Basic regression 

quietly eststo: reg lrtfpna_chg7 gini_disp lcgdpopercapita_gap_avg7 ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp gini_disp lcgdpopercapita_gap_avg7 ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 gini_disp lcgdpopercapita_gap_avg7 ib2017.year, robust cluster(alpha3_code)

esttab using "figures/tables/sec3_mainReg_noCountryRegionDummies_annual.tex", se star(* 0.1 ** 0.05 *** 0.01) drop(_cons *.year) label replace compress substitute(\_ _ {table} {sidewaystable}) addnote("\textbf{Regression equation:} \(g_{it} = \alpha + \gamma_t + \beta \big(\log (\frac{\textrm{cgdpo}_{it}}{\textrm{pop}_{it}} ) - \log (\frac{\textrm{cgdpo}_{USA,t}}{\textrm{pop}_{USA,t}}  ) \big) + \epsilon_{it}\)") wrap style(tex) title(Growth dynamics worldwide) b(a2)
esttab using "figures/tables/sec3_mainReg_noCountryRegionDummies_annual.csv", label replace cells(b) nonumbers plain keep(*lcgdpopercapita_gap_avg7* _cons)
eststo clear

quietly eststo: reg lrtfpna_chg7 gini_disp gini_disp2 lcgdpopercapita_gap_avg7 ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp gini_disp gini_disp2 lcgdpopercapita_gap_avg7 ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 gini_disp gini_disp2 lcgdpopercapita_gap_avg7 ib2017.year, robust cluster(alpha3_code)

esttab using "figures/tables/sec3_mainReg_noCountryRegionDummies_giniSquared_annual.tex", se star(* 0.1 ** 0.05 *** 0.01) drop(_cons *.year) label replace compress substitute(\_ _ {table} {sidewaystable}) addnote("\textbf{Regression equation:} \(g_{it} = \alpha + \gamma_t + \beta \big(\log (\frac{\textrm{cgdpo}_{it}}{\textrm{pop}_{it}} ) - \log (\frac{\textrm{cgdpo}_{USA,t}}{\textrm{pop}_{USA,t}}  ) \big) + \epsilon_{it}\)") wrap style(tex) title(Growth dynamics worldwide) b(a2)
esttab using "figures/tables/sec3_mainReg_noCountryRegionDummies_giniSquared_annual.csv", label replace cells(b) nonumbers plain keep(*lcgdpopercapita_gap_avg7* *.year _cons)
eststo clear

quietly eststo: reg lrtfpna_chg7 gini_mkt lcgdpopercapita_gap_avg7 ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp gini_mkt lcgdpopercapita_gap_avg7 ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 gini_mkt lcgdpopercapita_gap_avg7 ib2017.year, robust cluster(alpha3_code)

esttab using "figures/tables/sec3_mainReg_noCountryRegionDummies_ginimkt_annual.tex", se star(* 0.1 ** 0.05 *** 0.01) drop(_cons *.year) label replace compress substitute(\_ _ {table} {sidewaystable}) addnote("\textbf{Regression equation:} \(g_{it} = \alpha + \gamma_t + \beta \big(\log (\frac{\textrm{cgdpo}_{it}}{\textrm{pop}_{it}} ) - \log (\frac{\textrm{cgdpo}_{USA,t}}{\textrm{pop}_{USA,t}}  ) \big) + \epsilon_{it}\)") wrap style(tex) title(Growth dynamics worldwide) b(a2)
esttab using "figures/tables/sec3_mainReg_noCountryRegionDummies_ginimkt_annual.csv", label replace cells(b) nonumbers plain keep(*lcgdpopercapita_gap_avg7* *.year _cons)
eststo clear

quietly eststo: reg lrtfpna_chg7 gini_mkt gini_mkt2 lcgdpopercapita_gap_avg7 ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp gini_mkt gini_mkt2 lcgdpopercapita_gap_avg7 ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 gini_mkt gini_mkt2 lcgdpopercapita_gap_avg7 ib2017.year, robust cluster(alpha3_code)

esttab using "figures/tables/sec3_mainReg_noCountryRegionDummies_ginimktSquared_annual.tex", se star(* 0.1 ** 0.05 *** 0.01) drop(_cons *.year) label replace compress substitute(\_ _ {table} {sidewaystable}) addnote("\textbf{Regression equation:} \(g_{it} = \alpha + \gamma_t + \beta \big(\log (\frac{\textrm{cgdpo}_{it}}{\textrm{pop}_{it}} ) - \log (\frac{\textrm{cgdpo}_{USA,t}}{\textrm{pop}_{USA,t}}  ) \big) + \epsilon_{it}\)") wrap style(tex) title(Growth dynamics worldwide) b(a2)
esttab using "figures/tables/sec3_mainReg_noCountryRegionDummies_ginimktSquared_annual.csv", label replace cells(b) nonumbers plain keep(*lcgdpopercapita_gap_avg7* *.year _cons)
eststo clear

quietly eststo: reg lrtfpna_chg7 gini_disp gini_mkt lcgdpopercapita_gap_avg7 ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp gini_disp gini_mkt lcgdpopercapita_gap_avg7 ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 gini_disp gini_mkt lcgdpopercapita_gap_avg7 ib2017.year, robust cluster(alpha3_code)

esttab using "figures/tables/sec3_mainReg_noCountryRegionDummies_horseRace_annual.tex", se star(* 0.1 ** 0.05 *** 0.01) drop(_cons *.year) label replace compress substitute(\_ _ {table} {sidewaystable}) addnote("\textbf{Regression equation:} \(g_{it} = \alpha + \gamma_t + \beta \big(\log (\frac{\textrm{cgdpo}_{it}}{\textrm{pop}_{it}} ) - \log (\frac{\textrm{cgdpo}_{USA,t}}{\textrm{pop}_{USA,t}}  ) \big) + \epsilon_{it}\)") wrap style(tex) title(Growth dynamics worldwide) b(a2)
esttab using "figures/tables/sec3_mainReg_noCountryRegionDummies_horseRace_annual.csv", label replace cells(b) nonumbers plain keep(*lcgdpopercapita_gap_avg7* *.year _cons)
eststo clear




* Reg with only LAC dummy

quietly eststo: reg lrtfpna_chg7 gini_disp lcgdpopercapita_gap_avg7 islac ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp gini_disp lcgdpopercapita_gap_avg7 islac ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 gini_disp lcgdpopercapita_gap_avg7 islac ib2017.year, robust cluster(alpha3_code)

esttab using "figures/tables/sec3_mainReg_onlyLACdummy_annual.tex", se label star(* 0.1 ** 0.05 *** 0.01) replace compress substitute(\_ _ {table} {sidewaystable}) addnote("\textbf{Regression equation:} \(g_{it} = \alpha + \sigma \mathbf{1}_{\textrm{LAC}} + \gamma_t + \beta \big(\log (\frac{\textrm{cgdpo}_{it}}{\textrm{pop}_{it}} ) - \log (\frac{\textrm{cgdpo}_{USA,t}}{\textrm{pop}_{USA,t}}  ) \big) + \epsilon_{it}\)") wrap style(tex) title(Statistical significance of LAC growth gap (Non-LAC Benchmark)) booktabs b(a2)

eststo clear


quietly eststo: reg lrtfpna_chg7 gini_disp gini_disp2 lcgdpopercapita_gap_avg7 islac ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp gini_disp gini_disp2 lcgdpopercapita_gap_avg7 islac ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 gini_disp gini_disp2 lcgdpopercapita_gap_avg7 islac ib2017.year, robust cluster(alpha3_code)

esttab using "figures/tables/sec3_mainReg_onlyLACdummy_giniSquared_annual.tex", se label star(* 0.1 ** 0.05 *** 0.01) replace compress substitute(\_ _ {table} {sidewaystable}) addnote("\textbf{Regression equation:} \(g_{it} = \alpha + \sigma \mathbf{1}_{\textrm{LAC}} + \gamma_t + \beta \big(\log (\frac{\textrm{cgdpo}_{it}}{\textrm{pop}_{it}} ) - \log (\frac{\textrm{cgdpo}_{USA,t}}{\textrm{pop}_{USA,t}}  ) \big) + \epsilon_{it}\)") wrap style(tex) title(Statistical significance of LAC growth gap (Non-LAC Benchmark)) booktabs b(a2)

eststo clear

quietly eststo: reg lrtfpna_chg7 gini_mkt lcgdpopercapita_gap_avg7 islac ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp gini_mkt lcgdpopercapita_gap_avg7 islac ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 gini_mkt lcgdpopercapita_gap_avg7 islac ib2017.year, robust cluster(alpha3_code)

esttab using "figures/tables/sec3_mainReg_onlyLACdummy_ginimkt_annual.tex", se label star(* 0.1 ** 0.05 *** 0.01) replace compress substitute(\_ _ {table} {sidewaystable}) addnote("\textbf{Regression equation:} \(g_{it} = \alpha + \sigma \mathbf{1}_{\textrm{LAC}} + \gamma_t + \beta \big(\log (\frac{\textrm{cgdpo}_{it}}{\textrm{pop}_{it}} ) - \log (\frac{\textrm{cgdpo}_{USA,t}}{\textrm{pop}_{USA,t}}  ) \big) + \epsilon_{it}\)") wrap style(tex) title(Statistical significance of LAC growth gap (Non-LAC Benchmark)) booktabs b(a2)

eststo clear


quietly eststo: reg lrtfpna_chg7 gini_mkt gini_mkt2 lcgdpopercapita_gap_avg7 islac ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp gini_mkt gini_mkt2 lcgdpopercapita_gap_avg7 islac ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 gini_mkt gini_mkt2 lcgdpopercapita_gap_avg7 islac ib2017.year, robust cluster(alpha3_code)

esttab using "figures/tables/sec3_mainReg_onlyLACdummy_ginimktSquared_annual.tex", se label star(* 0.1 ** 0.05 *** 0.01) replace compress substitute(\_ _ {table} {sidewaystable}) addnote("\textbf{Regression equation:} \(g_{it} = \alpha + \sigma \mathbf{1}_{\textrm{LAC}} + \gamma_t + \beta \big(\log (\frac{\textrm{cgdpo}_{it}}{\textrm{pop}_{it}} ) - \log (\frac{\textrm{cgdpo}_{USA,t}}{\textrm{pop}_{USA,t}}  ) \big) + \epsilon_{it}\)") wrap style(tex) title(Statistical significance of LAC growth gap (Non-LAC Benchmark)) booktabs b(a2)

eststo clear

quietly eststo: reg lrtfpna_chg7 gini_disp gini_mkt lcgdpopercapita_gap_avg7 islac ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp gini_disp gini_mkt lcgdpopercapita_gap_avg7 islac ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 gini_disp gini_mkt lcgdpopercapita_gap_avg7 islac ib2017.year, robust cluster(alpha3_code)

esttab using "figures/tables/sec3_mainReg_onlyLACdummy_horseRace_annual.tex", se label star(* 0.1 ** 0.05 *** 0.01) replace compress substitute(\_ _ {table} {sidewaystable}) addnote("\textbf{Regression equation:} \(g_{it} = \alpha + \sigma \mathbf{1}_{\textrm{LAC}} + \gamma_t + \beta \big(\log (\frac{\textrm{cgdpo}_{it}}{\textrm{pop}_{it}} ) - \log (\frac{\textrm{cgdpo}_{USA,t}}{\textrm{pop}_{USA,t}}  ) \big) + \epsilon_{it}\)") wrap style(tex) title(Statistical significance of LAC growth gap (Non-LAC Benchmark)) booktabs b(a2)

eststo clear



* Reg with LAC'-pre1990 and LAC-post1990 dummies

quietly eststo: reg lrtfpna_chg7 gini_disp lcgdpopercapita_gap_avg7 islacPre1990 islacPost1990 ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp gini_disp lcgdpopercapita_gap_avg7 islacPre1990 islacPost1990 ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 gini_disp lcgdpopercapita_gap_avg7 islacPre1990 islacPost1990 ib2017.year, robust cluster(alpha3_code)

esttab using "figures/tables/sec3_mainReg_LACdummy_PrePost_annual.tex", se label star(* 0.1 ** 0.05 *** 0.01) replace compress substitute(\_ _ {table} {sidewaystable}) addnote("\textbf{Regression equation:} \(g_{it} = \alpha + \sigma_1 \mathbf{1}_{\textrm{LAC,pre1990}} + \sigma_2 \mathbf{1}_{\textrm{LAC,post1990}} + \gamma_t + \beta \big(\log (\frac{\textrm{cgdpo}_{it}}{\textrm{pop}_{it}} ) - \log (\frac{\textrm{cgdpo}_{USA,t}}{\textrm{pop}_{USA,t}}  ) \big) + \epsilon_{it}\)") wrap style(tex) title(Statistical significance of LAC growth gap, pre- and post-1990 (Non-LAC Benchmark)) booktabs b(a2)
eststo clear

quietly eststo: reg lrtfpna_chg7 gini_disp gini_disp2 lcgdpopercapita_gap_avg7 islacPre1990 islacPost1990 ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp gini_disp gini_disp2 lcgdpopercapita_gap_avg7 islacPre1990 islacPost1990 ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 gini_disp gini_disp2 lcgdpopercapita_gap_avg7 islacPre1990 islacPost1990 ib2017.year, robust cluster(alpha3_code)

esttab using "figures/tables/sec3_mainReg_LACdummy_PrePost_giniSquared_annual.tex", se label star(* 0.1 ** 0.05 *** 0.01) replace compress substitute(\_ _ {table} {sidewaystable}) addnote("\textbf{Regression equation:} \(g_{it} = \alpha + \sigma_1 \mathbf{1}_{\textrm{LAC,pre1990}} + \sigma_2 \mathbf{1}_{\textrm{LAC,post1990}} + \gamma_t + \beta \big(\log (\frac{\textrm{cgdpo}_{it}}{\textrm{pop}_{it}} ) - \log (\frac{\textrm{cgdpo}_{USA,t}}{\textrm{pop}_{USA,t}}  ) \big) + \epsilon_{it}\)") wrap style(tex) title(Statistical significance of LAC growth gap, pre- and post-1990 (Non-LAC Benchmark)) booktabs b(a2)
eststo clear

quietly eststo: reg lrtfpna_chg7 gini_mkt lcgdpopercapita_gap_avg7 islacPre1990 islacPost1990 ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp gini_mkt lcgdpopercapita_gap_avg7 islacPre1990 islacPost1990 ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 gini_mkt lcgdpopercapita_gap_avg7 islacPre1990 islacPost1990 ib2017.year, robust cluster(alpha3_code)

esttab using "figures/tables/sec3_mainReg_LACdummy_PrePost_ginimkt_annual.tex", se label star(* 0.1 ** 0.05 *** 0.01) replace compress substitute(\_ _ {table} {sidewaystable}) addnote("\textbf{Regression equation:} \(g_{it} = \alpha + \sigma_1 \mathbf{1}_{\textrm{LAC,pre1990}} + \sigma_2 \mathbf{1}_{\textrm{LAC,post1990}} + \gamma_t + \beta \big(\log (\frac{\textrm{cgdpo}_{it}}{\textrm{pop}_{it}} ) - \log (\frac{\textrm{cgdpo}_{USA,t}}{\textrm{pop}_{USA,t}}  ) \big) + \epsilon_{it}\)") wrap style(tex) title(Statistical significance of LAC growth gap, pre- and post-1990 (Non-LAC Benchmark)) booktabs b(a2)
eststo clear

quietly eststo: reg lrtfpna_chg7 gini_mkt gini_mkt2 lcgdpopercapita_gap_avg7 islacPre1990 islacPost1990 ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp gini_mkt gini_mkt2 lcgdpopercapita_gap_avg7 islacPre1990 islacPost1990 ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 gini_mkt gini_mkt2 lcgdpopercapita_gap_avg7 islacPre1990 islacPost1990 ib2017.year, robust cluster(alpha3_code)

esttab using "figures/tables/sec3_mainReg_LACdummy_PrePost_ginimktSquared_annual.tex", se label star(* 0.1 ** 0.05 *** 0.01) replace compress substitute(\_ _ {table} {sidewaystable}) addnote("\textbf{Regression equation:} \(g_{it} = \alpha + \sigma_1 \mathbf{1}_{\textrm{LAC,pre1990}} + \sigma_2 \mathbf{1}_{\textrm{LAC,post1990}} + \gamma_t + \beta \big(\log (\frac{\textrm{cgdpo}_{it}}{\textrm{pop}_{it}} ) - \log (\frac{\textrm{cgdpo}_{USA,t}}{\textrm{pop}_{USA,t}}  ) \big) + \epsilon_{it}\)") wrap style(tex) title(Statistical significance of LAC growth gap, pre- and post-1990 (Non-LAC Benchmark)) booktabs b(a2)
eststo clear

quietly eststo: reg lrtfpna_chg7 gini_disp gini_mkt lcgdpopercapita_gap_avg7 islacPre1990 islacPost1990 ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp gini_disp gini_mkt lcgdpopercapita_gap_avg7 islacPre1990 islacPost1990 ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 gini_disp gini_mkt lcgdpopercapita_gap_avg7 islacPre1990 islacPost1990 ib2017.year, robust cluster(alpha3_code)

esttab using "figures/tables/sec3_mainReg_LACdummy_PrePost_horseRace_annual.tex", se label star(* 0.1 ** 0.05 *** 0.01) replace compress substitute(\_ _ {table} {sidewaystable}) addnote("\textbf{Regression equation:} \(g_{it} = \alpha + \sigma_1 \mathbf{1}_{\textrm{LAC,pre1990}} + \sigma_2 \mathbf{1}_{\textrm{LAC,post1990}} + \gamma_t + \beta \big(\log (\frac{\textrm{cgdpo}_{it}}{\textrm{pop}_{it}} ) - \log (\frac{\textrm{cgdpo}_{USA,t}}{\textrm{pop}_{USA,t}}  ) \big) + \epsilon_{it}\)") wrap style(tex) title(Statistical significance of LAC growth gap, pre- and post-1990 (Non-LAC Benchmark)) booktabs b(a2)
eststo clear


* Reg with LAC country dummies (but no other countries)

quietly eststo: reg lrtfpna_chg7 gini_disp lcgdpopercapita_gap_avg7 ib2017.year ib(last).countrylac_code, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp gini_disp lcgdpopercapita_gap_avg7 ib2017.year ib(last).countrylac_code, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 gini_disp lcgdpopercapita_gap_avg7 ib2017.year ib(last).countrylac_code, robust cluster(alpha3_code)

esttab using "figures/tables/sec3_mainReg_LACCountryDummies_annual.tex", label star(* 0.1 ** 0.05 *** 0.01) replace compress drop(*year) nobaselevels nogaps not noparentheses substitute(\_ _ {table} {sidewaystable}) addnote("\textbf{Regression equation:} \(g_{it} = \alpha  + \omega_i + \gamma_t + \beta \big(\log (\frac{\textrm{cgdpo}_{it}}{\textrm{pop}_{it}} ) - \log (\frac{\textrm{cgdpo}_{USA,t}}{\textrm{pop}_{USA,t}}  ) \big) + \epsilon_{it}\)" "Estimated values of \(\omega_i\) for each country \(i\) shown above.") wrap style(tex) title(Statistical significance of LAC individual country growth gaps (Non-LAC Benchmark)) booktabs b(a2) 
*tex3pt "figures/tables/sec2_mainReg_LACCountryDummies_annual.tex" using "figures/tables/sec2_mainReg_LACCountryDummies_fixed_annual.tex", fontsize(\footnotesize)
esttab using "figures/tables/sec3_mainReg_LACCountryDummies_annual.csv", not label replace keep(*.countrylac_code) plain
eststo clear

quietly eststo: reg lrtfpna_chg7 gini_disp gini_disp2 lcgdpopercapita_gap_avg7 ib2017.year ib(last).countrylac_code, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp gini_disp gini_disp2 lcgdpopercapita_gap_avg7 ib2017.year ib(last).countrylac_code, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 gini_disp gini_disp2 lcgdpopercapita_gap_avg7 ib2017.year ib(last).countrylac_code, robust cluster(alpha3_code)

esttab using "figures/tables/sec3_mainReg_LACCountryDummies_giniSquared_annual.tex", label star(* 0.1 ** 0.05 *** 0.01) replace compress drop(*year) nobaselevels nogaps not noparentheses substitute(\_ _ {table} {sidewaystable}) addnote("\textbf{Regression equation:} \(g_{it} = \alpha  + \omega_i + \gamma_t + \beta \big(\log (\frac{\textrm{cgdpo}_{it}}{\textrm{pop}_{it}} ) - \log (\frac{\textrm{cgdpo}_{USA,t}}{\textrm{pop}_{USA,t}}  ) \big) + \epsilon_{it}\)" "Estimated values of \(\omega_i\) for each country \(i\) shown above.") wrap style(tex) title(Statistical significance of LAC individual country growth gaps (Non-LAC Benchmark)) booktabs b(a2) 
*tex3pt "figures/tables/sec2_mainReg_LACCountryDummies_annual.tex" using "figures/tables/sec2_mainReg_LACCountryDummies_fixed_annual.tex", fontsize(\footnotesize)
esttab using "figures/tables/sec3_mainReg_LACCountryDummies_giniSquared_annual.csv", not label replace keep(*.countrylac_code) plain
eststo clear

quietly eststo: reg lrtfpna_chg7 gini_mkt gini_mkt2 lcgdpopercapita_gap_avg7 ib2017.year ib(last).countrylac_code, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp gini_mkt gini_mkt2 lcgdpopercapita_gap_avg7 ib2017.year ib(last).countrylac_code, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 gini_mkt gini_mkt2 lcgdpopercapita_gap_avg7 ib2017.year ib(last).countrylac_code, robust cluster(alpha3_code)

esttab using "figures/tables/sec3_mainReg_LACCountryDummies_ginimktSquared_annual.tex", label star(* 0.1 ** 0.05 *** 0.01) replace compress drop(*year) nobaselevels nogaps not noparentheses substitute(\_ _ {table} {sidewaystable}) addnote("\textbf{Regression equation:} \(g_{it} = \alpha  + \omega_i + \gamma_t + \beta \big(\log (\frac{\textrm{cgdpo}_{it}}{\textrm{pop}_{it}} ) - \log (\frac{\textrm{cgdpo}_{USA,t}}{\textrm{pop}_{USA,t}}  ) \big) + \epsilon_{it}\)" "Estimated values of \(\omega_i\) for each country \(i\) shown above.") wrap style(tex) title(Statistical significance of LAC individual country growth gaps (Non-LAC Benchmark)) booktabs b(a2) 
*tex3pt "figures/tables/sec2_mainReg_LACCountryDummies_annual.tex" using "figures/tables/sec2_mainReg_LACCountryDummies_fixed_annual.tex", fontsize(\footnotesize)
esttab using "figures/tables/sec3_mainReg_LACCountryDummies_ginimktSquared_annual.csv", not label replace keep(*.countrylac_code) plain
eststo clear





* Reg with country-pre1990 and country-post1990 dummies (only for LAC countries)

quietly eststo: reg lrtfpna_chg7 gini_disp lcgdpopercapita_gap_avg7 ib2017.year ib(last).countrylac_code#i.pre1990 ib(last).countrylac_code#i.post1990, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp gini_disp lcgdpopercapita_gap_avg7 ib2017.year ib(last).countrylac_code#i.pre1990 ib(last).countrylac_code#i.post1990, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 gini_disp lcgdpopercapita_gap_avg7 ib2017.year ib(last).countrylac_code#i.pre1990 ib(last).countrylac_code#i.post1990, robust cluster(alpha3_code)

esttab using "figures/tables/sec3_mainReg_LACCountryDummies_PrePost_annual.tex", label star(* 0.1 ** 0.05 *** 0.01) replace compress nogaps nobaselevels drop(*.year) not noparentheses  substitute(\_ _ {table} {sidewaystable}) addnote("\textbf{Regression equation:} \(g_{it} = \alpha + \omega_{i\tau} + \gamma_t + \beta \big(\log (\frac{\textrm{cgdpo}_{it}}{\textrm{pop}_{it}} ) - \log (\frac{\textrm{cgdpo}_{USA,t}}{\textrm{pop}_{USA,t}}  ) \big) + \epsilon_{it}\)" "Variable \(\tau\) is equal to 1 if \(t > 1990\), 0 otherwise. Estimated values of \(\omega_{i\tau}\) for each country \(i\) shown above.") wrap style(tex) title(Statistical significance of LAC individual country growth gaps, pre- and post-1990 (Non-LAC Benchmark)) booktabs b(a2) 
*tex3pt "figures/tables/sec2_mainReg_LACCountryDummies_PrePost_annual.tex" using "figures/tables/sec2_mainReg_LACCountryDummies_PrePost_fixed_annual.tex", fontsize(\footnotesize)
esttab using "figures/tables/sec3_mainReg_LACCountryDummies_PrePost_annual.csv", se label replace 
esttab using "figures/tables/sec3_mainReg_LACCountryDummies_PrePost_clean_annual.csv", se label replace keep(*.countrylac_code*1990) plain
eststo clear

quietly eststo: reg lrtfpna_chg7 gini_disp gini_disp2 lcgdpopercapita_gap_avg7 ib2017.year ib(last).countrylac_code#i.pre1990 ib(last).countrylac_code#i.post1990, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp gini_disp gini_disp2 lcgdpopercapita_gap_avg7 ib(last).countrylac_code#i.pre1990 ib2017.year ib(last).countrylac_code#i.post1990, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 gini_disp gini_disp2 lcgdpopercapita_gap_avg7 ib(last).countrylac_code#i.pre1990 ib2017.year ib(last).countrylac_code#i.post1990, robust cluster(alpha3_code)

esttab using "figures/tables/sec3_mainReg_LACCountryDummies_PrePost_giniSquared_annual.tex", label star(* 0.1 ** 0.05 *** 0.01) replace compress nogaps nobaselevels drop(*.year) not noparentheses  substitute(\_ _ {table} {sidewaystable}) addnote("\textbf{Regression equation:} \(g_{it} = \alpha + \omega_{i\tau} + \gamma_t + \beta \big(\log (\frac{\textrm{cgdpo}_{it}}{\textrm{pop}_{it}} ) - \log (\frac{\textrm{cgdpo}_{USA,t}}{\textrm{pop}_{USA,t}}  ) \big) + \epsilon_{it}\)" "Variable \(\tau\) is equal to 1 if \(t > 1990\), 0 otherwise. Estimated values of \(\omega_{i\tau}\) for each country \(i\) shown above.") wrap style(tex) title(Statistical significance of LAC individual country growth gaps, pre- and post-1990 (Non-LAC Benchmark)) booktabs b(a2) 
*tex3pt "figures/tables/sec2_mainReg_LACCountryDummies_PrePost_annual.tex" using "figures/tables/sec2_mainReg_LACCountryDummies_PrePost_fixed_annual.tex", fontsize(\footnotesize)
esttab using "figures/tables/sec3_mainReg_LACCountryDummies_PrePost_giniSquared_annual.csv", se label replace 
esttab using "figures/tables/sec3_mainReg_LACCountryDummies_PrePost_giniSquared_clean_annual.csv", se label replace keep(*.countrylac_code*1990) plain nobaselevels
eststo clear

quietly eststo: reg lrtfpna_chg7 gini_mkt gini_mkt2 lcgdpopercapita_gap_avg7 ib2017.year ib(last).countrylac_code#i.pre1990 ib(last).countrylac_code#i.post1990, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp gini_mkt gini_mkt2 lcgdpopercapita_gap_avg7 ib(last).countrylac_code#i.pre1990 ib2017.year ib(last).countrylac_code#i.post1990, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 gini_mkt gini_mkt2 lcgdpopercapita_gap_avg7 ib(last).countrylac_code#i.pre1990 ib2017.year ib(last).countrylac_code#i.post1990, robust cluster(alpha3_code)

esttab using "figures/tables/sec3_mainReg_LACCountryDummies_PrePost_ginimktSquared_annual.tex", label star(* 0.1 ** 0.05 *** 0.01) replace compress nogaps nobaselevels drop(*.year) not noparentheses  substitute(\_ _ {table} {sidewaystable}) addnote("\textbf{Regression equation:} \(g_{it} = \alpha + \omega_{i\tau} + \gamma_t + \beta \big(\log (\frac{\textrm{cgdpo}_{it}}{\textrm{pop}_{it}} ) - \log (\frac{\textrm{cgdpo}_{USA,t}}{\textrm{pop}_{USA,t}}  ) \big) + \epsilon_{it}\)" "Variable \(\tau\) is equal to 1 if \(t > 1990\), 0 otherwise. Estimated values of \(\omega_{i\tau}\) for each country \(i\) shown above.") wrap style(tex) title(Statistical significance of LAC individual country growth gaps, pre- and post-1990 (Non-LAC Benchmark)) booktabs b(a2) 
*tex3pt "figures/tables/sec2_mainReg_LACCountryDummies_PrePost_annual.tex" using "figures/tables/sec2_mainReg_LACCountryDummies_PrePost_fixed_annual.tex", fontsize(\footnotesize)
esttab using "figures/tables/sec3_mainReg_LACCountryDummies_PrePost_ginimktSquared_annual.csv", se label replace 
esttab using "figures/tables/sec3_mainReg_LACCountryDummies_PrePost_ginimktSquared_clean_annual.csv", se label replace keep(*.countrylac_code*1990) plain 
eststo clear

