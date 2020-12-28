clear
set more off

cd "Z:/home/nico/Insync/nfernand@princeton.edu/Google Drive/PhD - Thesis/Research/inequality-and-convergence-in-lac"

insheet using data/pwt_swiid.csv

encode alpha3, gen(alpha3_code)

xtset alpha3_code year

* Generate islac interactions

gen islacPre1990 = 0
gen islacPost1990 = 0

replace islacPre1990 = 1 if islac == 1 & year <= 1989
replace islacPost1990 = 1 if islac == 1 & year > 1989

* Construct variable labels

label variable lrgdpnapercapita_chg7 "Per capita output growth"
label variable lrtfpna_chg7 "Productivity growth"
label variable lrgdpnapercapita_chg7_nontfp "Factor accumulation"
label variable lag_lcgdpopercapita_gap_avg7 "Per capita output gap (rel. USA, lag)"
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

esttab using "figures/tables/sec2_mainReg_noCountryRegionDummies.tex", se star(* 0.1 ** 0.05 *** 0.01) label replace compress substitute(\_ _) addnote("\textbf{Regression equation:} \(g_{it} = \alpha + \gamma_t + \beta \big(\log (\frac{\textrm{cgdpo}_{it}}{\textrm{pop}_{it}} ) - \log (\frac{\textrm{cgdpo}_{USA,t}}{\textrm{pop}_{USA,t}}  ) \big) + \epsilon_{it}\)") wrap style(tex) title(Growth dynamics worldwide) booktabs b(a2)
esttab using "figures/tables/sec2_mainReg_noCountryRegionDummies.csv", label replace cells(b) nonumbers plain keep(lcgdpopercapita_gap_avg7 *.year _cons)
eststo clear

preserve

keep if hasginidisp == 1

quietly eststo: reg lrtfpna_chg7 lcgdpopercapita_gap_avg7 ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp lcgdpopercapita_gap_avg7 ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 lcgdpopercapita_gap_avg7 ib2017.year, robust cluster(alpha3_code)

esttab using "figures/tables/sec2_mainReg_noCountryRegionDummies_SWIDsample.tex", se star(* 0.1 ** 0.05 *** 0.01) label replace compress substitute(\_ _) addnote("\textbf{Regression equation:} \(g_{it} = \alpha + \gamma_t + \beta \big(\log (\frac{\textrm{cgdpo}_{it}}{\textrm{pop}_{it}} ) - \log (\frac{\textrm{cgdpo}_{USA,t}}{\textrm{pop}_{USA,t}}  ) \big) + \epsilon_{it}\)") wrap style(tex) title(Growth dynamics worldwide (country-years with gini data)) booktabs b(a2)
esttab using "figures/tables/sec2_mainReg_noCountryRegionDummies_SWIDsample.csv", label replace cells(b) nonumbers plain keep(lcgdpopercapita_gap_avg7 *.year _cons)
eststo clear

restore

* Reg with no country / region dummies, but with time dummies and time * RGDP interaction, not using LAC information

preserve

keep if islac == 0

quietly eststo: reg lrtfpna_chg7 lag_lcgdpopercapita_gap_avg7 ib2017.year c.lag_lcgdpopercapita_gap_avg7#year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp lag_lcgdpopercapita_gap_avg7 ib2017.year c.lag_lcgdpopercapita_gap_avg7#year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 lag_lcgdpopercapita_gap_avg7 ib2017.year c.lag_lcgdpopercapita_gap_avg7#year, robust cluster(alpha3_code)

esttab using "figures/tables/sec2_mainReg_noCountryRegionDummies_yearGapInteraction.tex", se star(* 0.1 ** 0.05 *** 0.01) label replace style(tex) booktabs b(a2)
eststo clear

restore

* Main reg with all regional dummies

quietly eststo: reg lrtfpna_chg7 lag_lcgdpopercapita_gap_avg7 islac iseat isafrica ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp lag_lcgdpopercapita_gap_avg7 islac iseat isafrica ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 lag_lcgdpopercapita_gap_avg7 islac iseat isafrica ib2017.year, robust cluster(alpha3_code)

esttab using "figures/tables/sec2_mainReg.tex", se label star(* 0.1 ** 0.05 *** 0.01) replace style(tex) booktabs b(a2)
eststo clear

* Reg with only LAC dummy

quietly eststo: reg lrtfpna_chg7 lcgdpopercapita_gap_avg7 islac ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp lcgdpopercapita_gap_avg7 islac ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 lcgdpopercapita_gap_avg7 islac ib2017.year, robust cluster(alpha3_code)

esttab using "figures/tables/sec2_mainReg_onlyLACdummy.tex", se label star(* 0.1 ** 0.05 *** 0.01) replace compress substitute(\_ _) addnote("\textbf{Regression equation:} \(g_{it} = \alpha + \sigma \mathbf{1}_{\textrm{LAC}} + \gamma_t + \beta \big(\log (\frac{\textrm{cgdpo}_{it}}{\textrm{pop}_{it}} ) - \log (\frac{\textrm{cgdpo}_{USA,t}}{\textrm{pop}_{USA,t}}  ) \big) + \epsilon_{it}\)") wrap style(tex) title(Statistical significance of LAC growth gap (Non-LAC Benchmark)) booktabs b(a2)

eststo clear

preserve

keep if hasginidisp == 1

quietly eststo: reg lrtfpna_chg7 lcgdpopercapita_gap_avg7 islac ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp lcgdpopercapita_gap_avg7 islac ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 lcgdpopercapita_gap_avg7 islac ib2017.year, robust cluster(alpha3_code)

esttab using "figures/tables/sec2_mainReg_onlyLACdummy_SWIDsample.tex", se label star(* 0.1 ** 0.05 *** 0.01) replace compress substitute(\_ _) addnote("\textbf{Regression equation:} \(g_{it} = \alpha + \sigma \mathbf{1}_{\textrm{LAC}} + \gamma_t + \beta \big(\log (\frac{\textrm{cgdpo}_{it}}{\textrm{pop}_{it}} ) - \log (\frac{\textrm{cgdpo}_{USA,t}}{\textrm{pop}_{USA,t}}  ) \big) + \epsilon_{it}\)") wrap style(tex) title(Statistical significance of LAC growth gap (Non-LAC Benchmark, country-years with gini data)) booktabs b(a2)

eststo clear

restore

* Reg with LAC country dummies (but no other countries)

encode countrylac, gen(countrylac_code)

quietly eststo: reg lrtfpna_chg7 lcgdpopercapita_gap_avg7 ib2017.year ib(last).countrylac_code, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp lcgdpopercapita_gap_avg7 ib2017.year ib(last).countrylac_code, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 lcgdpopercapita_gap_avg7 ib2017.year ib(last).countrylac_code, robust cluster(alpha3_code)

esttab using "figures/tables/sec2_mainReg_LACCountryDummies.tex", label star(* 0.1 ** 0.05 *** 0.01) replace compress nogaps not noparentheses substitute(\_ _) addnote("\textbf{Regression equation:} \(g_{it} = \alpha  + \omega_i + \gamma_t + \beta \big(\log (\frac{\textrm{cgdpo}_{it}}{\textrm{pop}_{it}} ) - \log (\frac{\textrm{cgdpo}_{USA,t}}{\textrm{pop}_{USA,t}}  ) \big) + \epsilon_{it}\)" "Estimated values of \(\omega_i\) for each country \(i\) shown above.") wrap style(tex) title(Statistical significance of LAC individual country growth gaps (Non-LAC Benchmark)) booktabs b(a2) 
*tex3pt "figures/tables/sec2_mainReg_LACCountryDummies.tex" using "figures/tables/sec2_mainReg_LACCountryDummies_fixed.tex", fontsize(\footnotesize)
esttab using "figures/tables/sec2_mainReg_LACCountryDummies.csv", not label replace keep(*.countrylac_code) plain
eststo clear

preserve

keep if hasginidisp == 1

quietly eststo: reg lrtfpna_chg7 lcgdpopercapita_gap_avg7 ib2017.year ib(last).countrylac_code, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp lcgdpopercapita_gap_avg7 ib2017.year ib(last).countrylac_code, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 lcgdpopercapita_gap_avg7 ib2017.year ib(last).countrylac_code, robust cluster(alpha3_code)

esttab using "figures/tables/sec2_mainReg_LACCountryDummies_SWIDsample.tex", label star(* 0.1 ** 0.05 *** 0.01) replace compress nogaps not noparentheses substitute(\_ _) addnote("\textbf{Regression equation:} \(g_{it} = \alpha  + \omega_i + \gamma_t + \beta \big(\log (\frac{\textrm{cgdpo}_{it}}{\textrm{pop}_{it}} ) - \log (\frac{\textrm{cgdpo}_{USA,t}}{\textrm{pop}_{USA,t}}  ) \big) + \epsilon_{it}\)" "Estimated values of \(\omega_i\) for each country \(i\) shown above.") wrap style(tex) title(Statistical significance of LAC individual country growth gaps (Non-LAC Benchmark, country-years with gini data)) booktabs b(a2) 
*tex3pt "figures/tables/sec2_mainReg_LACCountryDummies.tex" using "figures/tables/sec2_mainReg_LACCountryDummies_fixed.tex", fontsize(\footnotesize)
esttab using "figures/tables/sec2_mainReg_LACCountryDummies_SWIDsample.csv", not label replace keep(*.countrylac_code) plain
eststo clear

restore

* Reg with LAC'-pre1990 and LAC-post1990 dummies

quietly eststo: reg lrtfpna_chg7 lcgdpopercapita_gap_avg7 islacPre1990 islacPost1990 ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp lcgdpopercapita_gap_avg7 islacPre1990 islacPost1990 ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 lcgdpopercapita_gap_avg7 islacPre1990 islacPost1990 ib2017.year, robust cluster(alpha3_code)

esttab using "figures/tables/sec2_mainReg_LACdummy_PrePost.tex", se label star(* 0.1 ** 0.05 *** 0.01) replace compress substitute(\_ _) addnote("\textbf{Regression equation:} \(g_{it} = \alpha + \sigma_1 \mathbf{1}_{\textrm{LAC,pre1990}} + \sigma_2 \mathbf{1}_{\textrm{LAC,post1990}} + \gamma_t + \beta \big(\log (\frac{\textrm{cgdpo}_{it}}{\textrm{pop}_{it}} ) - \log (\frac{\textrm{cgdpo}_{USA,t}}{\textrm{pop}_{USA,t}}  ) \big) + \epsilon_{it}\)") wrap style(tex) title(Statistical significance of LAC growth gap, pre- and post-1990 (Non-LAC Benchmark)) booktabs b(a2)
eststo clear

preserve

keep if hasginidisp == 1

quietly eststo: reg lrtfpna_chg7 lcgdpopercapita_gap_avg7 islacPre1990 islacPost1990 ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp lcgdpopercapita_gap_avg7 islacPre1990 islacPost1990 ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 lcgdpopercapita_gap_avg7 islacPre1990 islacPost1990 ib2017.year, robust cluster(alpha3_code)

esttab using "figures/tables/sec2_mainReg_LACdummy_PrePost_SWIDsample.tex", se label star(* 0.1 ** 0.05 *** 0.01) replace compress substitute(\_ _) addnote("\textbf{Regression equation:} \(g_{it} = \alpha + \sigma_1 \mathbf{1}_{\textrm{LAC,pre1990}} + \sigma_2 \mathbf{1}_{\textrm{LAC,post1990}} + \gamma_t + \beta \big(\log (\frac{\textrm{cgdpo}_{it}}{\textrm{pop}_{it}} ) - \log (\frac{\textrm{cgdpo}_{USA,t}}{\textrm{pop}_{USA,t}}  ) \big) + \epsilon_{it}\)") wrap style(tex) title(Statistical significance of LAC growth gap, pre- and post-1990 (Non-LAC Benchmark, country-years with gini data)) booktabs b(a2)
eststo clear

restore

* Reg with country-pre1990 and country-post1990 dummies (only for LAC countries)

quietly eststo: reg lrtfpna_chg7 lcgdpopercapita_gap_avg7 ib2017.year ib(last).countrylac_code#i.post1990, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp lcgdpopercapita_gap_avg7 ib2017.year ib(last).countrylac_code#i.post1990, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 lcgdpopercapita_gap_avg7 ib2017.year ib(last).countrylac_code#i.post1990, robust cluster(alpha3_code)

esttab using "figures/tables/sec2_mainReg_LACCountryDummies_PrePost.tex", label star(* 0.1 ** 0.05 *** 0.01) replace compress nonogaps not drop(*.year) noparentheses  substitute(\_ _) addnote("\textbf{Regression equation:} \(g_{it} = \alpha + \omega_{i\tau} + \gamma_t + \beta \big(\log (\frac{\textrm{cgdpo}_{it}}{\textrm{pop}_{it}} ) - \log (\frac{\textrm{cgdpo}_{USA,t}}{\textrm{pop}_{USA,t}}  ) \big) + \epsilon_{it}\)" "Variable \(\tau\) is equal to 1 if \(t > 1990\), 0 otherwise. Estimated values of \(\omega_{i\tau}\) for each country \(i\) shown above.") wrap style(tex) title(Statistical significance of LAC individual country growth gaps, pre- and post-1990 (Non-LAC Benchmark)) booktabs b(a2) 
*tex3pt "figures/tables/sec2_mainReg_LACCountryDummies_PrePost.tex" using "figures/tables/sec2_mainReg_LACCountryDummies_PrePost_fixed.tex", fontsize(\footnotesize)
esttab using "figures/tables/sec2_mainReg_LACCountryDummies_PrePost.csv", se label replace 
esttab using "figures/tables/sec2_mainReg_LACCountryDummies_PrePost_clean.csv", se label replace keep(*.countrylac_code*post1990) plain
eststo clear

preserve

keep if hasginidisp == 1

quietly eststo: reg lrtfpna_chg7 lcgdpopercapita_gap_avg7 ib2017.year ib(last).countrylac_code#i.post1990, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp lcgdpopercapita_gap_avg7 ib2017.year ib(last).countrylac_code#i.post1990, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 lcgdpopercapita_gap_avg7 ib2017.year ib(last).countrylac_code#i.post1990, robust cluster(alpha3_code)

esttab using "figures/tables/sec2_mainReg_LACCountryDummies_PrePost_SWIDsample.tex", label star(* 0.1 ** 0.05 *** 0.01) replace compress nonogaps not drop(*.year) noparentheses  substitute(\_ _) addnote("\textbf{Regression equation:} \(g_{it} = \alpha + \omega_{i\tau} + \gamma_t + \beta \big(\log (\frac{\textrm{cgdpo}_{it}}{\textrm{pop}_{it}} ) - \log (\frac{\textrm{cgdpo}_{USA,t}}{\textrm{pop}_{USA,t}}  ) \big) + \epsilon_{it}\)" "Variable \(\tau\) is equal to 1 if \(t > 1990\), 0 otherwise. Estimated values of \(\omega_{i\tau}\) for each country \(i\) shown above.") wrap style(tex) title(Statistical significance of LAC individual country growth gaps, pre- and post-1990 (Non-LAC Benchmark, country-years with gini data)) booktabs b(a2) 
*tex3pt "figures/tables/sec2_mainReg_LACCountryDummies_PrePost.tex" using "figures/tables/sec2_mainReg_LACCountryDummies_PrePost_fixed.tex", fontsize(\footnotesize)
esttab using "figures/tables/sec2_mainReg_LACCountryDummies_PrePost_SWIDsample.csv", se label replace 
esttab using "figures/tables/sec2_mainReg_LACCountryDummies_PrePost_clean_SWIDsample.csv", se label replace keep(*.countrylac_code*post1990) plain
eststo clear

restore



* Reg with only LAC dummy and LAC dummy interaction with lag gap

gen islac_lagGap = islac * lag_lcgdpopercapita_gap_avg7
label variable islac_lagGap "LAC * RGDP gap (rel. USA, lag)"

quietly eststo: reg lrtfpna_chg7 lag_lcgdpopercapita_gap_avg7 islac_lagGap islac ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp lag_lcgdpopercapita_gap_avg7 islac_lagGap islac ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 lag_lcgdpopercapita_gap_avg7 islac_lagGap islac ib2017.year, robust cluster(alpha3_code)

esttab using "figures/tables/sec2_mainReg_onlyLACdummy_LAClagGapInteraction.tex", se label replace style(tex) 
eststo clear

* Reg with only LAC dummy and LAC dummy interaction with lag gap

gen islac_nonlagGap = islac * lcgdpopercapita_gap_avg7
label variable islac_nonlagGap "LAC * RGDP gap (rel. USA)"

quietly eststo: reg lrtfpna_chg7 lcgdpopercapita_gap_avg7 islac_nonlagGap islac ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp lcgdpopercapita_gap_avg7 islac_nonlagGap islac ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 lcgdpopercapita_gap_avg7 islac_nonlagGap islac ib2017.year, robust cluster(alpha3_code)

esttab using "figures/tables/sec2_mainReg_onlyLACdummy_LACnonlagGapInteraction.tex", se label replace style(tex) 
eststo clear

* LAG GAP Reg with only LAC dummy and LAC dummy interaction with all variables (equiv. to two separate regressions)

quietly eststo: reg lrtfpna_chg7 lag_lcgdpopercapita_gap_avg7 islac_lagGap islac ib2017.year ib2017.year#i.islac, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp lag_lcgdpopercapita_gap_avg7 islac_lagGap islac ib2017.year ib2017.year#i.islac, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 lag_lcgdpopercapita_gap_avg7 islac_lagGap islac ib2017.year ib2017.year#i.islac, robust cluster(alpha3_code)

esttab using "figures/tables/sec2_mainReg_timeDummies_LACinteractionsAll_lagGap.csv", label replace cells(b) nonumbers plain keep(lag_lcgdpopercapita_gap_avg7 islac_lagGap *.year _cons)
esttab using "figures/tables/sec2_mainReg_timeDummies_LACinteractionsAll_lagGap.tex", label replace se keep(lag_lcgdpopercapita_gap_avg7 islac_lagGap islac *.year _cons) style(tex)

eststo clear

* NON LAG GAP Reg with only LAC dummy and LAC dummy interaction with all variables (equiv. to two separate regressions)

quietly eststo: reg lrtfpna_chg7 lcgdpopercapita_gap_avg7 islac_nonlagGap islac ib2017.year ib2017.year#i.islac, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp lcgdpopercapita_gap_avg7 islac_nonlagGap islac ib2017.year ib2017.year#i.islac, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 lcgdpopercapita_gap_avg7 islac_nonlagGap islac ib2017.year ib2017.year#i.islac, robust cluster(alpha3_code)

* csv with some coefficients, for use in constructing predictions
esttab using "figures/tables/sec2_mainReg_timeDummies_LACinteractionsAll_nonlagGap.csv", label replace cells(b) nonumbers plain keep(lcgdpopercapita_gap_avg7 islac_nonlagGap *.year _cons)

* csv with ALL coefficients, including interactions etc
esttab using "figures/tables/excel/mainRegLACdummiesInteractions_allCoefficients.csv", se label replace
esttab using "figures/tables/sec2_mainReg_timeDummies_LACinteractionsAll_nonlagGap.tex", label replace se keep(lcgdpopercapita_gap_avg7 islac_nonlagGap islac *.year _cons) compress substitute(\_ _) addnote("\textbf{Regression equation:} \(g_{it} = \alpha + \gamma_t + \beta \big(\log (\frac{\textrm{cgdpo}_{it}}{\textrm{pop}_{it}} ) - \log (\frac{\textrm{cgdpo}_{USA,t}}{\textrm{pop}_{USA,t}}  ) \big) + \epsilon_{it}\), " "plus LAC dummies and interactions with LAC.") wrap style(tex) title(Statistical significance of difference in unconditional convergence / divergence, LAC vs non-LAC) booktabs b(a2)
eststo clear


* NON LAG GAP Reg with only NON-LAC sample

preserve

keep if islac == 0

quietly eststo: reg lrtfpna_chg7 lcgdpopercapita_gap_avg7 ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp lcgdpopercapita_gap_avg7 ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 lcgdpopercapita_gap_avg7 ib2017.year, robust cluster(alpha3_code)

* csv with some coefficients, for use in constructing predictions
esttab using "figures/tables/sec2_mainReg_nonLACsample.csv", label replace cells(b) nonumbers plain keep(lcgdpopercapita_gap_avg7 *.year _cons)

* csv with ALL coefficients, including interactions etc
esttab using "figures/tables/excel/mainReg_nonLACsample_allCoefficients.csv", se label replace
esttab using "figures/tables/sec2_mainReg_nonLACsample.tex", keep(lcgdpopercapita_gap_avg7 *.year _cons) star(* 0.1 ** 0.05 *** 0.01) se label replace compress substitute(\_ _) addnote("\textbf{Regression equation:} \(g_{it} = \alpha + \gamma_t + \beta \big(\log (\frac{\textrm{cgdpo}_{it}}{\textrm{pop}_{it}} ) - \log (\frac{\textrm{cgdpo}_{USA,t}}{\textrm{pop}_{USA,t}}  ) \big) + \epsilon_{it}\)" "\textbf{Notes:} Only non-LAC sample.") wrap style(tex) title(Growth dynamics in non-LAC countries) booktabs b(a2)

eststo clear


keep if hasginidisp == 1

quietly eststo: reg lrtfpna_chg7 lcgdpopercapita_gap_avg7 ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp lcgdpopercapita_gap_avg7 ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 lcgdpopercapita_gap_avg7 ib2017.year, robust cluster(alpha3_code)

* csv with some coefficients, for use in constructing predictions
esttab using "figures/tables/sec2_mainReg_nonLACsample_SWIDsample.csv", label replace cells(b) nonumbers plain keep(lcgdpopercapita_gap_avg7 *.year _cons)

* csv with ALL coefficients, including interactions etc
esttab using "figures/tables/excel/mainReg_nonLACsample_allCoefficients_SWIDsample.csv", se label replace
esttab using "figures/tables/sec2_mainReg_nonLACsample_SWIDsample.tex", keep(lcgdpopercapita_gap_avg7 *.year _cons) star(* 0.1 ** 0.05 *** 0.01) se label replace compress substitute(\_ _) addnote("\textbf{Regression equation:} \(g_{it} = \alpha + \gamma_t + \beta \big(\log (\frac{\textrm{cgdpo}_{it}}{\textrm{pop}_{it}} ) - \log (\frac{\textrm{cgdpo}_{USA,t}}{\textrm{pop}_{USA,t}}  ) \big) + \epsilon_{it}\)" "\textbf{Notes:} Only non-LAC sample.") wrap style(tex) title(Growth dynamics in non-LAC countries (country-years with gini data)) booktabs b(a2)

eststo clear


restore

* NON LAG GAP Reg with only LAC sample

preserve

keep if islac == 1

quietly eststo: reg lrtfpna_chg7 lcgdpopercapita_gap_avg7 ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp lcgdpopercapita_gap_avg7 ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 lcgdpopercapita_gap_avg7 ib2017.year, robust cluster(alpha3_code)

* csv with some coefficients, for use in constructing predictions
esttab using "figures/tables/sec2_mainReg_LACsample.csv", label replace cells(b) nonumbers plain keep(lcgdpopercapita_gap_avg7 *.year _cons)

* csv with ALL coefficients, including interactions etc
esttab using "figures/tables/excel/mainReg_LACsample_allCoefficients.csv", se label replace
esttab using "figures/tables/sec2_mainReg_LACsample.tex", keep(lcgdpopercapita_gap_avg7 *.year _cons) se label replace compress substitute(\_ _) addnote("\textbf{Regression equation:} \(g_{it} = \alpha + \gamma_t + \beta \big(\log (\frac{\textrm{cgdpo}_{it}}{\textrm{pop}_{it}} ) - \log (\frac{\textrm{cgdpo}_{USA,t}}{\textrm{pop}_{USA,t}}  ) \big) + \epsilon_{it}\)" "\textbf{Notes:} Only LAC sample.") wrap style(tex) title(Growth dynamics in LAC countries) booktabs b(a2)


eststo clear

restore



**** East Asian Tiger Benchmark
* same as above, but with East Asia dummy  and interactions as well

gen iseat_nonlagGap = iseat * lcgdpopercapita_gap_avg7
label variable iseat_nonlagGap "LAC * RGDP gap (rel. USA)"

quietly eststo: reg lrtfpna_chg7 lcgdpopercapita_gap_avg7 islac_nonlagGap islac ib2017.year ib2017.year#i.islac, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp lcgdpopercapita_gap_avg7 islac_nonlagGap islac ib2017.year ib2017.year#i.islac, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 lcgdpopercapita_gap_avg7 islac_nonlagGap islac iseat_nonlagGap iseat ib2017.year ib2017.year#i.islac ib2017.year#i.iseat, robust cluster(alpha3_code)

esttab using "figures/tables/sec2_mainReg_timeDummies_LACandEATinteractionsAll_nonlagGap.csv", label replace cells(b) nonumbers plain keep(lcgdpopercapita_gap_avg7 islac_nonlagGap *.year _cons)
esttab using "figures/tables/sec2_mainReg_timeDummies_LACandEATinteractionsAll_nonlagGap.tex", label replace style(tex) se keep(lcgdpopercapita_gap_avg7 islac_nonlagGap islac iseat_nonlagGap iseat)

eststo clear

**** Africa Benchmark
* same as above, but with East Asia dummy  and interactions as well

gen isafrica_nonlagGap = isafrica * lcgdpopercapita_gap_avg7
label variable isafrica_nonlagGap "LAC * RGDP gap (rel. USA)"

quietly eststo: reg lrtfpna_chg7 lcgdpopercapita_gap_avg7 islac_nonlagGap islac ib2017.year ib2017.year#i.islac, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp lcgdpopercapita_gap_avg7 islac_nonlagGap islac ib2017.year ib2017.year#i.islac, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 lcgdpopercapita_gap_avg7 islac_nonlagGap islac isafrica_nonlagGap isafrica ib2017.year ib2017.year#i.islac ib2017.year#i.isafrica, robust cluster(alpha3_code)

esttab using "figures/tables/sec2_mainReg_timeDummies_LACandAFRinteractionsAll_nonlagGap.csv", label replace cells(b) nonumbers plain keep(lcgdpopercapita_gap_avg7 islac_nonlagGap *.year _cons)
esttab using "figures/tables/sec2_mainReg_timeDummies_LACandAFRinteractionsAll_nonlagGap.tex", label replace style(tex) se keep(lcgdpopercapita_gap_avg7 islac_nonlagGap islac isafrica_nonlagGap isafrica)

eststo clear



* Reg with only LAC dummy and LAC dummy interaction with all variables (equiv. to two separate regressions), EXCLUDING AFRICA

preserve

drop if subregion_new == "Africa"


quietly eststo: reg lrtfpna_chg7 lag_lcgdpopercapita_gap_avg7 islac_lagGap islac ib2017.year ib2017.year#i.islac, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp lag_lcgdpopercapita_gap_avg7 islac_lagGap islac ib2017.year ib2017.year#i.islac, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 lag_lcgdpopercapita_gap_avg7 islac_lagGap islac ib2017.year ib2017.year#i.islac, robust cluster(alpha3_code)

esttab using "figures/tables/sec2_mainReg_timeDummies_LACinteractionsAll_noAfrica.csv", label replace cells(b) nonumbers plain keep(lag_lcgdpopercapita_gap_avg7 islac_lagGap *.year _cons)
esttab using "figures/tables/sec2_mainReg_timeDummies_LACinteractionsAll_noAfrica.tex", label replace style(tex) se keep(lag_lcgdpopercapita_gap_avg7 islac_lagGap islac *.year _cons)

eststo clear

restore

* Reg with individual country dummies

quietly eststo: reg lrtfpna_chg7 lag_lcgdpopercapita_gap_avg7 i.alpha3_code ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp lag_lcgdpopercapita_gap_avg7 i.alpha3_code ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 lag_lcgdpopercapita_gap_avg7 i.alpha3_code ib2017.year, robust cluster(alpha3_code)

esttab using "figures/tables/sec2_mainReg_countryDumies_countryCoefs.csv", label replace cells(b) nonumbers plain keep(*.alpha3_code _cons)
esttab using "figures/tables/sec2_mainReg_countryDumies_otherCoefs.tex", label replace style(tex) se drop(*.alpha3_code _cons)
eststo clear





** Pre and post 1990

* Pre 1990
preserve
keep if year < 1996

reg lrgdpnapercapita_chg7 lag_lcgdpopercapita_gap_avg7 ib68.alpha3_code i.year

quietly eststo: reg lrtfpna_chg7 lag_lcgdpopercapita_gap_avg7 islac iseat isafrica i.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp lag_lcgdpopercapita_gap_avg7 islac iseat isafrica i.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 lag_lcgdpopercapita_gap_avg7 islac iseat isafrica i.year, robust cluster(alpha3_code)



esttab using "figures/tables/sec2_pre1990.tex", se style(tex) label replace
eststo clear

restore

* Post 1990

preserve
keep if year >= 1989

*reg lrgdpnapercapita_chg7 lag_lcgdpopercapita_gap_avg7 ib68.alpha3_code i.year

quietly eststo: reg lrtfpna_chg7 lag_lcgdpopercapita_gap_avg7 islac iseat isafrica i.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp lag_lcgdpopercapita_gap_avg7 islac iseat isafrica i.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 lag_lcgdpopercapita_gap_avg7 islac iseat isafrica i.year, robust cluster(alpha3_code)

esttab using "figures/tables/sec2_post1990.tex", se style(tex) label replace
eststo clear

restore




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

esttab using "figures/tables/sec3_mainReg_noCountryRegionDummies.tex", se star(* 0.1 ** 0.05 *** 0.01) label replace compress substitute(\_ _ {table} {sidewaystable}) addnote("\textbf{Regression equation:} \(g_{it} = \alpha + \gamma_t + \beta \big(\log (\frac{\textrm{cgdpo}_{it}}{\textrm{pop}_{it}} ) - \log (\frac{\textrm{cgdpo}_{USA,t}}{\textrm{pop}_{USA,t}}  ) \big) + \epsilon_{it}\)") wrap style(tex) title(Growth dynamics worldwide) b(a2)
esttab using "figures/tables/sec3_mainReg_noCountryRegionDummies.csv", label replace cells(b) nonumbers plain keep(*lcgdpopercapita_gap_avg7* *.year _cons)
eststo clear

quietly eststo: reg lrtfpna_chg7 gini_disp gini_disp2 lcgdpopercapita_gap_avg7 ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp gini_disp gini_disp2 lcgdpopercapita_gap_avg7 ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 gini_disp gini_disp2 lcgdpopercapita_gap_avg7 ib2017.year, robust cluster(alpha3_code)

esttab using "figures/tables/sec3_mainReg_noCountryRegionDummies_giniSquared.tex", se star(* 0.1 ** 0.05 *** 0.01) label replace compress substitute(\_ _ {table} {sidewaystable}) addnote("\textbf{Regression equation:} \(g_{it} = \alpha + \gamma_t + \beta \big(\log (\frac{\textrm{cgdpo}_{it}}{\textrm{pop}_{it}} ) - \log (\frac{\textrm{cgdpo}_{USA,t}}{\textrm{pop}_{USA,t}}  ) \big) + \epsilon_{it}\)") wrap style(tex) title(Growth dynamics worldwide) b(a2)
esttab using "figures/tables/sec3_mainReg_noCountryRegionDummies_giniSquared.csv", label replace cells(b) nonumbers plain keep(*lcgdpopercapita_gap_avg7* *.year _cons)
eststo clear

quietly eststo: reg lrtfpna_chg7 gini_mkt lcgdpopercapita_gap_avg7 ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp gini_mkt lcgdpopercapita_gap_avg7 ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 gini_mkt lcgdpopercapita_gap_avg7 ib2017.year, robust cluster(alpha3_code)

esttab using "figures/tables/sec3_mainReg_noCountryRegionDummies_ginimkt.tex", se star(* 0.1 ** 0.05 *** 0.01) label replace compress substitute(\_ _ {table} {sidewaystable}) addnote("\textbf{Regression equation:} \(g_{it} = \alpha + \gamma_t + \beta \big(\log (\frac{\textrm{cgdpo}_{it}}{\textrm{pop}_{it}} ) - \log (\frac{\textrm{cgdpo}_{USA,t}}{\textrm{pop}_{USA,t}}  ) \big) + \epsilon_{it}\)") wrap style(tex) title(Growth dynamics worldwide) b(a2)
esttab using "figures/tables/sec3_mainReg_noCountryRegionDummies_ginimkt.csv", label replace cells(b) nonumbers plain keep(*lcgdpopercapita_gap_avg7* *.year _cons)
eststo clear

quietly eststo: reg lrtfpna_chg7 gini_mkt gini_mkt2 lcgdpopercapita_gap_avg7 ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp gini_mkt gini_mkt2 lcgdpopercapita_gap_avg7 ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 gini_mkt gini_mkt2 lcgdpopercapita_gap_avg7 ib2017.year, robust cluster(alpha3_code)

esttab using "figures/tables/sec3_mainReg_noCountryRegionDummies_ginimktSquared.tex", se star(* 0.1 ** 0.05 *** 0.01) label replace compress substitute(\_ _ {table} {sidewaystable}) addnote("\textbf{Regression equation:} \(g_{it} = \alpha + \gamma_t + \beta \big(\log (\frac{\textrm{cgdpo}_{it}}{\textrm{pop}_{it}} ) - \log (\frac{\textrm{cgdpo}_{USA,t}}{\textrm{pop}_{USA,t}}  ) \big) + \epsilon_{it}\)") wrap style(tex) title(Growth dynamics worldwide) b(a2)
esttab using "figures/tables/sec3_mainReg_noCountryRegionDummies_ginimktSquared.csv", label replace cells(b) nonumbers plain keep(*lcgdpopercapita_gap_avg7* *.year _cons)
eststo clear

quietly eststo: reg lrtfpna_chg7 gini_disp gini_mkt lcgdpopercapita_gap_avg7 ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp gini_disp gini_mkt lcgdpopercapita_gap_avg7 ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 gini_disp gini_mkt lcgdpopercapita_gap_avg7 ib2017.year, robust cluster(alpha3_code)

esttab using "figures/tables/sec3_mainReg_noCountryRegionDummies_horseRace.tex", se star(* 0.1 ** 0.05 *** 0.01) label replace compress substitute(\_ _ {table} {sidewaystable}) addnote("\textbf{Regression equation:} \(g_{it} = \alpha + \gamma_t + \beta \big(\log (\frac{\textrm{cgdpo}_{it}}{\textrm{pop}_{it}} ) - \log (\frac{\textrm{cgdpo}_{USA,t}}{\textrm{pop}_{USA,t}}  ) \big) + \epsilon_{it}\)") wrap style(tex) title(Growth dynamics worldwide) b(a2)
esttab using "figures/tables/sec3_mainReg_noCountryRegionDummies_horseRace.csv", label replace cells(b) nonumbers plain keep(*lcgdpopercapita_gap_avg7* *.year _cons)
eststo clear




* Reg with only LAC dummy

quietly eststo: reg lrtfpna_chg7 gini_disp lcgdpopercapita_gap_avg7 islac ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp gini_disp lcgdpopercapita_gap_avg7 islac ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 gini_disp lcgdpopercapita_gap_avg7 islac ib2017.year, robust cluster(alpha3_code)

esttab using "figures/tables/sec3_mainReg_onlyLACdummy.tex", se label star(* 0.1 ** 0.05 *** 0.01) replace compress substitute(\_ _ {table} {sidewaystable}) addnote("\textbf{Regression equation:} \(g_{it} = \alpha + \sigma \mathbf{1}_{\textrm{LAC}} + \gamma_t + \beta \big(\log (\frac{\textrm{cgdpo}_{it}}{\textrm{pop}_{it}} ) - \log (\frac{\textrm{cgdpo}_{USA,t}}{\textrm{pop}_{USA,t}}  ) \big) + \epsilon_{it}\)") wrap style(tex) title(Statistical significance of LAC growth gap (Non-LAC Benchmark)) booktabs b(a2)

eststo clear


quietly eststo: reg lrtfpna_chg7 gini_disp gini_disp2 lcgdpopercapita_gap_avg7 islac ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp gini_disp gini_disp2 lcgdpopercapita_gap_avg7 islac ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 gini_disp gini_disp2 lcgdpopercapita_gap_avg7 islac ib2017.year, robust cluster(alpha3_code)

esttab using "figures/tables/sec3_mainReg_onlyLACdummy_giniSquared.tex", se label star(* 0.1 ** 0.05 *** 0.01) replace compress substitute(\_ _ {table} {sidewaystable}) addnote("\textbf{Regression equation:} \(g_{it} = \alpha + \sigma \mathbf{1}_{\textrm{LAC}} + \gamma_t + \beta \big(\log (\frac{\textrm{cgdpo}_{it}}{\textrm{pop}_{it}} ) - \log (\frac{\textrm{cgdpo}_{USA,t}}{\textrm{pop}_{USA,t}}  ) \big) + \epsilon_{it}\)") wrap style(tex) title(Statistical significance of LAC growth gap (Non-LAC Benchmark)) booktabs b(a2)

eststo clear

quietly eststo: reg lrtfpna_chg7 gini_mkt lcgdpopercapita_gap_avg7 islac ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp gini_mkt lcgdpopercapita_gap_avg7 islac ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 gini_mkt lcgdpopercapita_gap_avg7 islac ib2017.year, robust cluster(alpha3_code)

esttab using "figures/tables/sec3_mainReg_onlyLACdummy_ginimkt.tex", se label star(* 0.1 ** 0.05 *** 0.01) replace compress substitute(\_ _ {table} {sidewaystable}) addnote("\textbf{Regression equation:} \(g_{it} = \alpha + \sigma \mathbf{1}_{\textrm{LAC}} + \gamma_t + \beta \big(\log (\frac{\textrm{cgdpo}_{it}}{\textrm{pop}_{it}} ) - \log (\frac{\textrm{cgdpo}_{USA,t}}{\textrm{pop}_{USA,t}}  ) \big) + \epsilon_{it}\)") wrap style(tex) title(Statistical significance of LAC growth gap (Non-LAC Benchmark)) booktabs b(a2)

eststo clear


quietly eststo: reg lrtfpna_chg7 gini_mkt gini_mkt2 lcgdpopercapita_gap_avg7 islac ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp gini_mkt gini_mkt2 lcgdpopercapita_gap_avg7 islac ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 gini_mkt gini_mkt2 lcgdpopercapita_gap_avg7 islac ib2017.year, robust cluster(alpha3_code)

esttab using "figures/tables/sec3_mainReg_onlyLACdummy_ginimktSquared.tex", se label star(* 0.1 ** 0.05 *** 0.01) replace compress substitute(\_ _ {table} {sidewaystable}) addnote("\textbf{Regression equation:} \(g_{it} = \alpha + \sigma \mathbf{1}_{\textrm{LAC}} + \gamma_t + \beta \big(\log (\frac{\textrm{cgdpo}_{it}}{\textrm{pop}_{it}} ) - \log (\frac{\textrm{cgdpo}_{USA,t}}{\textrm{pop}_{USA,t}}  ) \big) + \epsilon_{it}\)") wrap style(tex) title(Statistical significance of LAC growth gap (Non-LAC Benchmark)) booktabs b(a2)

eststo clear

quietly eststo: reg lrtfpna_chg7 gini_disp gini_mkt lcgdpopercapita_gap_avg7 islac ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp gini_disp gini_mkt lcgdpopercapita_gap_avg7 islac ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 gini_disp gini_mkt lcgdpopercapita_gap_avg7 islac ib2017.year, robust cluster(alpha3_code)

esttab using "figures/tables/sec3_mainReg_onlyLACdummy_horseRace.tex", se label star(* 0.1 ** 0.05 *** 0.01) replace compress substitute(\_ _ {table} {sidewaystable}) addnote("\textbf{Regression equation:} \(g_{it} = \alpha + \sigma \mathbf{1}_{\textrm{LAC}} + \gamma_t + \beta \big(\log (\frac{\textrm{cgdpo}_{it}}{\textrm{pop}_{it}} ) - \log (\frac{\textrm{cgdpo}_{USA,t}}{\textrm{pop}_{USA,t}}  ) \big) + \epsilon_{it}\)") wrap style(tex) title(Statistical significance of LAC growth gap (Non-LAC Benchmark)) booktabs b(a2)

eststo clear



* Reg with LAC'-pre1990 and LAC-post1990 dummies

quietly eststo: reg lrtfpna_chg7 gini_disp lcgdpopercapita_gap_avg7 islacPre1990 islacPost1990 ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp gini_disp lcgdpopercapita_gap_avg7 islacPre1990 islacPost1990 ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 gini_disp lcgdpopercapita_gap_avg7 islacPre1990 islacPost1990 ib2017.year, robust cluster(alpha3_code)

esttab using "figures/tables/sec3_mainReg_LACdummy_PrePost.tex", se label star(* 0.1 ** 0.05 *** 0.01) replace compress substitute(\_ _ {table} {sidewaystable}) addnote("\textbf{Regression equation:} \(g_{it} = \alpha + \sigma_1 \mathbf{1}_{\textrm{LAC,pre1990}} + \sigma_2 \mathbf{1}_{\textrm{LAC,post1990}} + \gamma_t + \beta \big(\log (\frac{\textrm{cgdpo}_{it}}{\textrm{pop}_{it}} ) - \log (\frac{\textrm{cgdpo}_{USA,t}}{\textrm{pop}_{USA,t}}  ) \big) + \epsilon_{it}\)") wrap style(tex) title(Statistical significance of LAC growth gap, pre- and post-1990 (Non-LAC Benchmark)) booktabs b(a2)
eststo clear

quietly eststo: reg lrtfpna_chg7 gini_disp gini_disp2 lcgdpopercapita_gap_avg7 islacPre1990 islacPost1990 ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp gini_disp gini_disp2 lcgdpopercapita_gap_avg7 islacPre1990 islacPost1990 ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 gini_disp gini_disp2 lcgdpopercapita_gap_avg7 islacPre1990 islacPost1990 ib2017.year, robust cluster(alpha3_code)

esttab using "figures/tables/sec3_mainReg_LACdummy_PrePost_giniSquared.tex", se label star(* 0.1 ** 0.05 *** 0.01) replace compress substitute(\_ _ {table} {sidewaystable}) addnote("\textbf{Regression equation:} \(g_{it} = \alpha + \sigma_1 \mathbf{1}_{\textrm{LAC,pre1990}} + \sigma_2 \mathbf{1}_{\textrm{LAC,post1990}} + \gamma_t + \beta \big(\log (\frac{\textrm{cgdpo}_{it}}{\textrm{pop}_{it}} ) - \log (\frac{\textrm{cgdpo}_{USA,t}}{\textrm{pop}_{USA,t}}  ) \big) + \epsilon_{it}\)") wrap style(tex) title(Statistical significance of LAC growth gap, pre- and post-1990 (Non-LAC Benchmark)) booktabs b(a2)
eststo clear

quietly eststo: reg lrtfpna_chg7 gini_mkt lcgdpopercapita_gap_avg7 islacPre1990 islacPost1990 ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp gini_mkt lcgdpopercapita_gap_avg7 islacPre1990 islacPost1990 ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 gini_mkt lcgdpopercapita_gap_avg7 islacPre1990 islacPost1990 ib2017.year, robust cluster(alpha3_code)

esttab using "figures/tables/sec3_mainReg_LACdummy_PrePost_ginimkt.tex", se label star(* 0.1 ** 0.05 *** 0.01) replace compress substitute(\_ _ {table} {sidewaystable}) addnote("\textbf{Regression equation:} \(g_{it} = \alpha + \sigma_1 \mathbf{1}_{\textrm{LAC,pre1990}} + \sigma_2 \mathbf{1}_{\textrm{LAC,post1990}} + \gamma_t + \beta \big(\log (\frac{\textrm{cgdpo}_{it}}{\textrm{pop}_{it}} ) - \log (\frac{\textrm{cgdpo}_{USA,t}}{\textrm{pop}_{USA,t}}  ) \big) + \epsilon_{it}\)") wrap style(tex) title(Statistical significance of LAC growth gap, pre- and post-1990 (Non-LAC Benchmark)) booktabs b(a2)
eststo clear

quietly eststo: reg lrtfpna_chg7 gini_mkt gini_mkt2 lcgdpopercapita_gap_avg7 islacPre1990 islacPost1990 ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp gini_mkt gini_mkt2 lcgdpopercapita_gap_avg7 islacPre1990 islacPost1990 ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 gini_mkt gini_mkt2 lcgdpopercapita_gap_avg7 islacPre1990 islacPost1990 ib2017.year, robust cluster(alpha3_code)

esttab using "figures/tables/sec3_mainReg_LACdummy_PrePost_ginimktSquared.tex", se label star(* 0.1 ** 0.05 *** 0.01) replace compress substitute(\_ _ {table} {sidewaystable}) addnote("\textbf{Regression equation:} \(g_{it} = \alpha + \sigma_1 \mathbf{1}_{\textrm{LAC,pre1990}} + \sigma_2 \mathbf{1}_{\textrm{LAC,post1990}} + \gamma_t + \beta \big(\log (\frac{\textrm{cgdpo}_{it}}{\textrm{pop}_{it}} ) - \log (\frac{\textrm{cgdpo}_{USA,t}}{\textrm{pop}_{USA,t}}  ) \big) + \epsilon_{it}\)") wrap style(tex) title(Statistical significance of LAC growth gap, pre- and post-1990 (Non-LAC Benchmark)) booktabs b(a2)
eststo clear

quietly eststo: reg lrtfpna_chg7 gini_disp gini_mkt lcgdpopercapita_gap_avg7 islacPre1990 islacPost1990 ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp gini_disp gini_mkt lcgdpopercapita_gap_avg7 islacPre1990 islacPost1990 ib2017.year, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 gini_disp gini_mkt lcgdpopercapita_gap_avg7 islacPre1990 islacPost1990 ib2017.year, robust cluster(alpha3_code)

esttab using "figures/tables/sec3_mainReg_LACdummy_PrePost_horseRace.tex", se label star(* 0.1 ** 0.05 *** 0.01) replace compress substitute(\_ _ {table} {sidewaystable}) addnote("\textbf{Regression equation:} \(g_{it} = \alpha + \sigma_1 \mathbf{1}_{\textrm{LAC,pre1990}} + \sigma_2 \mathbf{1}_{\textrm{LAC,post1990}} + \gamma_t + \beta \big(\log (\frac{\textrm{cgdpo}_{it}}{\textrm{pop}_{it}} ) - \log (\frac{\textrm{cgdpo}_{USA,t}}{\textrm{pop}_{USA,t}}  ) \big) + \epsilon_{it}\)") wrap style(tex) title(Statistical significance of LAC growth gap, pre- and post-1990 (Non-LAC Benchmark)) booktabs b(a2)
eststo clear


* Reg with LAC country dummies (but no other countries)

quietly eststo: reg lrtfpna_chg7 gini_disp lcgdpopercapita_gap_avg7 ib2017.year ib(last).countrylac_code, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp gini_disp lcgdpopercapita_gap_avg7 ib2017.year ib(last).countrylac_code, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 gini_disp lcgdpopercapita_gap_avg7 ib2017.year ib(last).countrylac_code, robust cluster(alpha3_code)

esttab using "figures/tables/sec3_mainReg_LACCountryDummies.tex", label star(* 0.1 ** 0.05 *** 0.01) replace compress nogaps not noparentheses substitute(\_ _ {table} {sidewaystable}) addnote("\textbf{Regression equation:} \(g_{it} = \alpha  + \omega_i + \gamma_t + \beta \big(\log (\frac{\textrm{cgdpo}_{it}}{\textrm{pop}_{it}} ) - \log (\frac{\textrm{cgdpo}_{USA,t}}{\textrm{pop}_{USA,t}}  ) \big) + \epsilon_{it}\)" "Estimated values of \(\omega_i\) for each country \(i\) shown above.") wrap style(tex) title(Statistical significance of LAC individual country growth gaps (Non-LAC Benchmark)) booktabs b(a2) 
*tex3pt "figures/tables/sec2_mainReg_LACCountryDummies.tex" using "figures/tables/sec2_mainReg_LACCountryDummies_fixed.tex", fontsize(\footnotesize)
esttab using "figures/tables/sec3_mainReg_LACCountryDummies.csv", not label replace keep(*.countrylac_code) plain
eststo clear

quietly eststo: reg lrtfpna_chg7 gini_disp gini_disp2 lcgdpopercapita_gap_avg7 ib2017.year ib(last).countrylac_code, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp gini_disp gini_disp2 lcgdpopercapita_gap_avg7 ib2017.year ib(last).countrylac_code, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 gini_disp gini_disp2 lcgdpopercapita_gap_avg7 ib2017.year ib(last).countrylac_code, robust cluster(alpha3_code)

esttab using "figures/tables/sec3_mainReg_LACCountryDummies_giniSquared.tex", label star(* 0.1 ** 0.05 *** 0.01) replace compress nogaps not noparentheses substitute(\_ _ {table} {sidewaystable}) addnote("\textbf{Regression equation:} \(g_{it} = \alpha  + \omega_i + \gamma_t + \beta \big(\log (\frac{\textrm{cgdpo}_{it}}{\textrm{pop}_{it}} ) - \log (\frac{\textrm{cgdpo}_{USA,t}}{\textrm{pop}_{USA,t}}  ) \big) + \epsilon_{it}\)" "Estimated values of \(\omega_i\) for each country \(i\) shown above.") wrap style(tex) title(Statistical significance of LAC individual country growth gaps (Non-LAC Benchmark)) booktabs b(a2) 
*tex3pt "figures/tables/sec2_mainReg_LACCountryDummies.tex" using "figures/tables/sec2_mainReg_LACCountryDummies_fixed.tex", fontsize(\footnotesize)
esttab using "figures/tables/sec3_mainReg_LACCountryDummies_giniSquared.csv", not label replace keep(*.countrylac_code) plain
eststo clear

quietly eststo: reg lrtfpna_chg7 gini_mkt gini_mkt2 lcgdpopercapita_gap_avg7 ib2017.year ib(last).countrylac_code, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp gini_mkt gini_mkt2 lcgdpopercapita_gap_avg7 ib2017.year ib(last).countrylac_code, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 gini_mkt gini_mkt2 lcgdpopercapita_gap_avg7 ib2017.year ib(last).countrylac_code, robust cluster(alpha3_code)

esttab using "figures/tables/sec3_mainReg_LACCountryDummies_ginimktSquared.tex", label star(* 0.1 ** 0.05 *** 0.01) replace compress nogaps not noparentheses substitute(\_ _ {table} {sidewaystable}) addnote("\textbf{Regression equation:} \(g_{it} = \alpha  + \omega_i + \gamma_t + \beta \big(\log (\frac{\textrm{cgdpo}_{it}}{\textrm{pop}_{it}} ) - \log (\frac{\textrm{cgdpo}_{USA,t}}{\textrm{pop}_{USA,t}}  ) \big) + \epsilon_{it}\)" "Estimated values of \(\omega_i\) for each country \(i\) shown above.") wrap style(tex) title(Statistical significance of LAC individual country growth gaps (Non-LAC Benchmark)) booktabs b(a2) 
*tex3pt "figures/tables/sec2_mainReg_LACCountryDummies.tex" using "figures/tables/sec2_mainReg_LACCountryDummies_fixed.tex", fontsize(\footnotesize)
esttab using "figures/tables/sec3_mainReg_LACCountryDummies_ginimktSquared.csv", not label replace keep(*.countrylac_code) plain
eststo clear





* Reg with country-pre1990 and country-post1990 dummies (only for LAC countries)

quietly eststo: reg lrtfpna_chg7 gini_disp lcgdpopercapita_gap_avg7 ib2017.year ib(last).countrylac_code#i.post1990, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp gini_disp lcgdpopercapita_gap_avg7 ib2017.year ib(last).countrylac_code#i.post1990, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 gini_disp lcgdpopercapita_gap_avg7 ib2017.year ib(last).countrylac_code#i.post1990, robust cluster(alpha3_code)

esttab using "figures/tables/sec3_mainReg_LACCountryDummies_PrePost.tex", label star(* 0.1 ** 0.05 *** 0.01) replace compress nonogaps not drop(*.year) noparentheses  substitute(\_ _ {table} {sidewaystable}) addnote("\textbf{Regression equation:} \(g_{it} = \alpha + \omega_{i\tau} + \gamma_t + \beta \big(\log (\frac{\textrm{cgdpo}_{it}}{\textrm{pop}_{it}} ) - \log (\frac{\textrm{cgdpo}_{USA,t}}{\textrm{pop}_{USA,t}}  ) \big) + \epsilon_{it}\)" "Variable \(\tau\) is equal to 1 if \(t > 1990\), 0 otherwise. Estimated values of \(\omega_{i\tau}\) for each country \(i\) shown above.") wrap style(tex) title(Statistical significance of LAC individual country growth gaps, pre- and post-1990 (Non-LAC Benchmark)) booktabs b(a2) 
*tex3pt "figures/tables/sec2_mainReg_LACCountryDummies_PrePost.tex" using "figures/tables/sec2_mainReg_LACCountryDummies_PrePost_fixed.tex", fontsize(\footnotesize)
esttab using "figures/tables/sec3_mainReg_LACCountryDummies_PrePost.csv", se label replace 
esttab using "figures/tables/sec3_mainReg_LACCountryDummies_PrePost_clean.csv", se label replace keep(*.countrylac_code*post1990) plain
eststo clear

quietly eststo: reg lrtfpna_chg7 gini_disp gini_disp2 lcgdpopercapita_gap_avg7 ib2017.year ib(last).countrylac_code#i.post1990, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp gini_disp gini_disp2 lcgdpopercapita_gap_avg7 ib2017.year ib(last).countrylac_code#i.post1990, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 gini_disp gini_disp2 lcgdpopercapita_gap_avg7 ib2017.year ib(last).countrylac_code#i.post1990, robust cluster(alpha3_code)

esttab using "figures/tables/sec3_mainReg_LACCountryDummies_PrePost_giniSquared.tex", label star(* 0.1 ** 0.05 *** 0.01) replace compress nonogaps not drop(*.year) noparentheses  substitute(\_ _ {table} {sidewaystable}) addnote("\textbf{Regression equation:} \(g_{it} = \alpha + \omega_{i\tau} + \gamma_t + \beta \big(\log (\frac{\textrm{cgdpo}_{it}}{\textrm{pop}_{it}} ) - \log (\frac{\textrm{cgdpo}_{USA,t}}{\textrm{pop}_{USA,t}}  ) \big) + \epsilon_{it}\)" "Variable \(\tau\) is equal to 1 if \(t > 1990\), 0 otherwise. Estimated values of \(\omega_{i\tau}\) for each country \(i\) shown above.") wrap style(tex) title(Statistical significance of LAC individual country growth gaps, pre- and post-1990 (Non-LAC Benchmark)) booktabs b(a2) 
*tex3pt "figures/tables/sec2_mainReg_LACCountryDummies_PrePost.tex" using "figures/tables/sec2_mainReg_LACCountryDummies_PrePost_fixed.tex", fontsize(\footnotesize)
esttab using "figures/tables/sec3_mainReg_LACCountryDummies_PrePost_giniSquared.csv", se label replace 
esttab using "figures/tables/sec3_mainReg_LACCountryDummies_PrePost_giniSquared_clean.csv", se label replace keep(*.countrylac_code*post1990) plain
eststo clear

quietly eststo: reg lrtfpna_chg7 gini_mkt gini_mkt2 lcgdpopercapita_gap_avg7 ib2017.year ib(last).countrylac_code#i.post1990, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7_nontfp gini_mkt gini_mkt2 lcgdpopercapita_gap_avg7 ib2017.year ib(last).countrylac_code#i.post1990, robust cluster(alpha3_code)
quietly eststo: reg lrgdpnapercapita_chg7 gini_mkt gini_mkt2 lcgdpopercapita_gap_avg7 ib2017.year ib(last).countrylac_code#i.post1990, robust cluster(alpha3_code)

esttab using "figures/tables/sec3_mainReg_LACCountryDummies_PrePost_ginimktSquared.tex", label star(* 0.1 ** 0.05 *** 0.01) replace compress nonogaps not drop(*.year) noparentheses  substitute(\_ _ {table} {sidewaystable}) addnote("\textbf{Regression equation:} \(g_{it} = \alpha + \omega_{i\tau} + \gamma_t + \beta \big(\log (\frac{\textrm{cgdpo}_{it}}{\textrm{pop}_{it}} ) - \log (\frac{\textrm{cgdpo}_{USA,t}}{\textrm{pop}_{USA,t}}  ) \big) + \epsilon_{it}\)" "Variable \(\tau\) is equal to 1 if \(t > 1990\), 0 otherwise. Estimated values of \(\omega_{i\tau}\) for each country \(i\) shown above.") wrap style(tex) title(Statistical significance of LAC individual country growth gaps, pre- and post-1990 (Non-LAC Benchmark)) booktabs b(a2) 
*tex3pt "figures/tables/sec2_mainReg_LACCountryDummies_PrePost.tex" using "figures/tables/sec2_mainReg_LACCountryDummies_PrePost_fixed.tex", fontsize(\footnotesize)
esttab using "figures/tables/sec3_mainReg_LACCountryDummies_PrePost_ginimktSquared.csv", se label replace 
esttab using "figures/tables/sec3_mainReg_LACCountryDummies_PrePost_ginimktSquared_clean.csv", se label replace keep(*.countrylac_code*post1990) plain
eststo clear




