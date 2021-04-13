  
  
  
  library(data.table)
  library(readstata13)
  library(openxlsx)
  library(gridExtra)
  library(xtable)
  library(ggplot2)
  library(ggthemr)
  library(kableExtra)
  

  
  
  rm(list = ls())
  setwd("/home/nico/Insync/nfernand@princeton.edu/Google Drive/PhD - Thesis/Research/inequality-and-convergence-in-lac")
  
  ggthemr("flat", spacing = 0.9, layout = "clear")  
  
  
  pwt <- data.table(read.dta13("data/raw/pwt91.dta"))
  
  pwt <- pwt[ , .(countrycode,country,year,cgdpo,rtfpna,rgdpna,rgdpo,pop,ck)]
  
  # To put things in capital deepening terms, uncomment the following line
  #pwt[ , rtfpna := rtfpna^(1/0.7)]
  
  pwt <- pwt[order(countrycode,year)]
  
  ##### Select time frame
  pwt <- pwt[ year >= 1960] 
  
  ##### Select sample of countries
  pwt[ , numComplete := sum(complete.cases(cgdpo,rgdpna,rtfpna,pop)), by = "countrycode"]
  pwt[ , numObs := .N, by = "countrycode"]
  pwt[ numComplete == numObs, noIncomplete := 1]
  temp <- unique(pwt, by = "countrycode")[order(noIncomplete)]
  noIncompletes <- temp[ noIncomplete == 1]
  pwt <- pwt[numComplete >= 54]
  
  
  
  ###### Variable construction
  # Construct US pop variable
  usPop <- pwt[ countrycode == "USA", .(year,pop)]
  
  names(usPop) <- c("year","usPop")
  
  setkey(usPop,year)
  setkey(pwt,year)
  
  pwt <- usPop[pwt]
  
  # Construct per capita variables
  pwt[ , rgdpnaPerCapita := rgdpna / pop]
  pwt[ , rgdpoPerCapita := rgdpo / pop]
  pwt[ , cgdpoPerCapita := cgdpo /pop]
  
  # Constuct log transformation of tfp and rgdp per capita (three measures of it)
  pwt[ , lrtfpna := log(rtfpna)]
  pwt[ , lrgdpnaPerCapita := log(rgdpnaPerCapita)]
  pwt[ , lrgdpoPerCapita := log(rgdpoPerCapita)]
  pwt[ , lcgdpoPerCapita := log(cgdpoPerCapita)]
  
  ## Extrapolation
  
  # Construct log changes (focusing now on rgdpna) 

  setkey(pwt, countrycode, year)
    
  pwt[ , lrtfpna_chg1 := lrtfpna - shift(lrtfpna, n = 1L, type = "lag"), by = .(countrycode)]
  pwt[ , lrgdpnaPerCapita_chg1 := lrgdpnaPerCapita - shift(lrgdpnaPerCapita, n = 1L, type = "lag"), by = .(countrycode)]
  
  # Run regression of log change of rtfpna on log change of rgdpna, by country
  regCoefs <- pwt[ , c(extrapRegCoefs = as.list(coef(lm(lrtfpna_chg1 ~ lrgdpnaPerCapita_chg1))), extrapRegSEs = as.list(coef(summary(lm(lrtfpna_chg1 ~ lrgdpnaPerCapita_chg1)))[,2]), extrapRegPVals = as.list(coef(summary(lm(lrtfpna_chg1 ~ lrgdpnaPerCapita_chg1)))[,4])), by = .(countrycode)]
    
  # Merge regression coefficients back to main data
  setkey(regCoefs,countrycode)
  setkey(pwt,countrycode)
  pwt <- regCoefs[ , .(countrycode,`extrapRegCoefs.(Intercept)`,extrapRegCoefs.lrgdpnaPerCapita_chg1)][pwt]
  
  # Impute changes when lrtfpna_chg1 is na, impute using regression coefficients
  pwt[ is.na(lrtfpna_chg1) , lrtfpna_chg1 := `extrapRegCoefs.(Intercept)` + lrgdpnaPerCapita_chg1 * extrapRegCoefs.lrgdpnaPerCapita_chg1]
  
  # Finally, extrapolate -- this was kind of tricky..there's probably a much better way, no?
  pwt <- pwt[order(country,-year)]
  pwt[, lrtfpna_cumChg := cumsum(-lrtfpna_chg1), by = .(countrycode)]
  pwt[, lrtfpna_extrap := .SD[1]$lrtfpna + shift(lrtfpna_cumChg, n = 1L, type = "lag"), by = .(countrycode)]
  
  # When missing, construct indicator for extrapolation 
  # and then compute extrapolation 
  pwt[is.na(lrtfpna) & !is.na(lrtfpna_extrap), isExtrapolated := 1]
  pwt[is.na(isExtrapolated), isExtrapolated := 0]
  
  fwrite(pwt[ isExtrapolated == 1, .(country, countrycode, year)], "data/extrapolatedCountryYears.csv")
  
  pwt[is.na(lrtfpna), lrtfpna := lrtfpna_extrap]
  
  # Final diagnostic to check that nothing is missing
  
  pwt[ , numComplete := sum(complete.cases(lrtfpna,lrgdpnaPerCapita,lrgdpoPerCapita,lcgdpoPerCapita)), by = "countrycode"]
  pwt[ , numObs := .N, by = "countrycode"]
  pwt[ numComplete == numObs, noIncomplete := 1]
  pwt[ noIncomplete != 1]
  
  pwt <- pwt[ , .(country,countrycode,isExtrapolated,year,lrtfpna,lrgdpnaPerCapita,lrgdpoPerCapita,lcgdpoPerCapita)][order(country,year)]
  
  fwrite(pwt,"data/pwt91_cleaned.csv")
  
  ##### Next stage: analyze data
  
  rm(list = ls())
  
  ggthemr("flat", spacing = 0.9, layout = "clear")
  
  pwt <- fread("data/pwt91_cleaned.csv")
  
  setkey(pwt,countrycode,year)
  
  # Construct 7-year changes
  pwt[ , lrgdpnaPerCapita_chg7 := (lrgdpnaPerCapita - shift(lrgdpnaPerCapita, n = 7L, type = "lag"))/7, by = "countrycode"]
  pwt[ , lrtfpna_chg7 := (lrtfpna - shift(lrtfpna, n = 7L, type = "lag"))/7, by = "countrycode"]
  
  # Construt 1-year changes and decomposition
  pwt[ , lrgdpnaPerCapita_chg1 := (lrgdpnaPerCapita - shift(lrgdpnaPerCapita, n = 1L, type = "lag")), by = "countrycode"]
  pwt[ , lrtfpna_chg1 := (lrtfpna - shift(lrtfpna, n = 1L, type = "lag")), by = "countrycode"]
  
  # Put in % terms
  pwt[ ,c("lrgdpnaPerCapita_chg7","lrtfpna_chg7","lrgdpnaPerCapita_chg1",
          "lrtfpna_chg1") := lapply(.SD, function(x) 100*x), ,.SDcols = c("lrgdpnaPerCapita_chg7","lrtfpna_chg7","lrgdpnaPerCapita_chg1",
                                                                     "lrtfpna_chg1")]
  
  # Construct decompositions
  pwt[ , lrgdpnaPerCapita_chg7_nonTFP := lrgdpnaPerCapita_chg7 - lrtfpna_chg7]
  pwt[ , lrgdpnaPerCapita_chg1_nonTFP := lrgdpnaPerCapita_chg1 - lrtfpna_chg1]
  
  # Construct cgdpoPerCapita relative to US (7-year averages, same as before)
  
  lcgdpoPerCapitaUSA <- pwt[countrycode == "USA"][ , .(year,lcgdpoPerCapita)]
  
  setkey(lcgdpoPerCapitaUSA,year)
  setnames(lcgdpoPerCapitaUSA,"lcgdpoPerCapita","lcgdpoPerCapitaUSA")
  
  setkey(pwt,year)
    
  pwt <- lcgdpoPerCapitaUSA[pwt]
  
  pwt[ , lcgdpoPerCapita_gap :=  lcgdpoPerCapita - lcgdpoPerCapitaUSA]
  
  setkey(pwt,countrycode,year)
    
  pwt[ , lcgdpoPerCapita_gap_avg7 := Reduce(`+`,shift(lcgdpoPerCapita_gap,n= 0L:6L,type = "lag"))/7, by = "countrycode"]

  

  #############
  ### Classify into regions
  #############
    
  isocodesRegions <- fread("data/raw/isocodesRegions.csv")
  
  setnames(isocodesRegions,"alpha-3","alpha3")
  setnames(isocodesRegions,"sub-region","subRegion")
  
  setkey(isocodesRegions,alpha3)
  setkey(pwt,countrycode)
  
  isocodesRegions <- isocodesRegions[ , .(alpha3,region,subRegion)]
  
  pwt <- isocodesRegions[pwt]
  
  pwt <- pwt[order(country,year)]
  
  pwt[ , subRegion_new := subRegion]
  
  pwt[ subRegion == "Sub-Saharan Africa" , subRegion_new := "Africa"]
  pwt[ subRegion == "Northern Africa" , subRegion_new := "Africa"]
  
  pwt[alpha3 == "HKG" | alpha3 == "KOR" | alpha3 == "TWN" | alpha3 == "SGP" , subRegion_new := "East Asian Tigers"]
  
  pwt[alpha3 == "USA" , subRegion_new := "United States"]
  
  pwt[ subRegion_new != "United States" & subRegion_new != "East Asian Tigers" & subRegion_new != "Africa" & subRegion_new != "Latin America and the Caribbean", subRegion_new := "Others"]
  
  ### Generate regional dummies
  
  pwt[subRegion_new == "Latin America and the Caribbean" , isLAC := 1]
  pwt[is.na(isLAC) , isLAC := 0]
  
  pwt[subRegion_new == "Africa" , isAfrica := 1]
  pwt[is.na(isAfrica) , isAfrica := 0]
  
  pwt[subRegion_new == "East Asian Tigers" , isEAT := 1]
  pwt[is.na(isEAT) , isEAT := 0]
  
  ## Make some quick plots for diagnostics

  ggplot(pwt[alpha3 == "USA" ], aes(year)) + 
    geom_line(aes(y = lrtfpna_chg7, color = "lrtfpna_chg7")) + 
    geom_line(aes(y = lrgdpnaPerCapita_chg7_nonTFP, color = "lrgdpnaPerCapSita_chg7_nonTFP"))
  

  
  #############
  ####### Bring in SWID data to do extrapolation of Gini
  ##########
  
  swiid <- fread("data/raw/swiid8_2_summary.csv")[ , .(country,year,gini_disp,gini_mkt)]
  setkey(swiid,country,year)
  
  setkey(pwt,country,year)
  
  pwt <- swiid[pwt]
  
  Z <- lm(gini_disp ~ lrgdpnaPerCapita + lcgdpoPerCapita + year + factor(alpha3), data = pwt, na.action = na.exclude)
  summary(Z)
  
  Zfe <- plm(gini_disp ~ lrgdpnaPerCapita + lcgdpoPerCapita + year, data = pwt, na.action = na.exclude, index = c("alpha3"), model = "within")
  summary(Zfe)
  predictions <- predict(Zfe)
  
  # Run regression and extract coefficients
  pwt[ , gini_disp_chg1 := gini_disp - shift(gini_disp), by = country]
  pwt[ , gini_mkt_chg1 := gini_mkt - shift(gini_mkt), by = country]
  pwt[ , lcgdpoPerCapita_gap_chg1 := 100 * (lcgdpoPerCapita_gap - shift(lcgdpoPerCapita_gap)), by = country]
  
  pwt[ , gini_disp_avg7 := Reduce(`+`,shift(gini_disp,n= 0L:6L,type = "lag"))/7, by = "alpha3"]
  pwt[ , gini_mkt_avg7 := Reduce(`+`,shift(gini_mkt,n= 0L:6L,type = "lag"))/7, by = "alpha3"]
  
  p1 <- ggplot(pwt[subRegion_new == "Latin America and the Caribbean"], aes(x = year, y = gini_disp, by = alpha3, color = subRegion_new)) + 
    geom_line()
  
  p2 <- ggplot(pwt[subRegion_new == "Latin America and the Caribbean"], aes(x = year, y = gini_mkt, by = alpha3, color = subRegion_new)) + 
    geom_line()
  
  grid.arrange(p1,p2,nrow = 1)
  
  pwt[ subRegion_new == "Latin America and the Caribbean", countryLAC := alpha3]
  pwt[ is.na(countryLAC), countryLAC := "nonLAC"]
  
  pwt[ !is.na(gini_disp), hasGiniDisp := 1]
  pwt[ is.na(gini_disp), hasGiniDisp := 0]
  
  pwt[ !is.na(gini_mkt), hasGiniMkt := 1]
  pwt[ is.na(gini_mkt), hasGiniMkt := 0]

  
  fwrite(pwt,"data/pwt91_cleaned_swiid.csv")
  
  #pwt <- fread("data/pwt91_cleaned_swiid.csv")

  # Disposable gini
  regCoefs_disp <- pwt[ , c(extrapRegCoefs = as.list(coef(lm(gini_disp_chg1 ~ lrgdpnaPerCapita_chg1 + lcgdpoPerCapita_gap_chg1))), 
                            extrapRegSEs = as.list(coef(summary(lm(gini_disp_chg1 ~ lrgdpnaPerCapita_chg1 + lcgdpoPerCapita_gap_chg1)))[,2]), 
                            extrapRegPVals = as.list(coef(summary(lm(gini_disp_chg1 ~ lrgdpnaPerCapita_chg1 + lcgdpoPerCapita_gap_chg1)))[,4]))]
  
  
  #residuals <- resid(lm(gini_disp_chg1 ~ lrgdpnaPerCapita_chg1 + lcgdpoPerCapita_gap_chg1, data = pwt), na.action=na.exclude)
  Zfd <- lm(gini_disp_chg1 ~ factor(alpha3) + factor(year), data = pwt, na.action = na.exclude)
  summary(Zfd)
    
  Z2 <- plm(gini_disp ~ lrgdpnaPerCapita + lcgdpoPerCapita_gap, data = pwt, na.action = na.exclude)
  
  residuals <- data.table(residuals(Zfd))
  
  pwt[ , resid := residuals$V1]
  
  ggplot(pwt[ alpha3 %in% c("URY","ARG","USA")], aes(x = year, y = resid, color = country)) + 
    geom_line()
  
  residualsAutocovarianceReg <- lm(resid ~ shift(resid) + shift(resid, n = 2L, type = "lag")
                                   + shift(resid, n = 3L, type = "lag") + shift(resid, n = 4L, type = "lag")
                                   + shift(resid, n = 5L, type = "lag") + shift(resid, n = 6L, type = "lag")
                                   + shift(resid, n = 7L, type = "lag"), data = pwt)
  
  results <- tail(data.table(coef(summary(residualsAutocovarianceReg))),-1)
  
  results[ , lag := seq(1,length(results$Estimate))]
  
  names(results) <- c("coef","se","tstat","pval","lag")
  
  results[ , `:=`(upperbound = coef + 1.96*se, lowerbound = coef - 1.96*se)]
  
  results <- melt(results,id.vars = "lag", measure.vars = c("coef","lowerbound","upperbound"))
  
  ggplot(results, aes(x = lag,y = value, linetype = variable)) + 
    geom_line() + 
    labs(title = "Autocovariance of residuals in FD regression") + 
    scale_linetype_discrete(labels = c("Estimate","-1.96 * SE", "+1.96 * SE"))
  
  ggsave("figures/giniExtrapErrorACF.pdf",plot = last_plot(), width = 9, height = 6.5, units = "in")
  

  regCoefs_mkt <- pwt[ , c(extrapRegCoefs = as.list(coef(lm(gini_mkt_chg1 ~ lrgdpnaPerCapita_chg1 + lcgdpoPerCapita_gap_chg1))), 
                           extrapRegSEs = as.list(coef(summary(lm(gini_mkt_chg1 ~ lrgdpnaPerCapita_chg1 + lcgdpoPerCapita_gap_chg1)))[,2]), 
                           extrapRegPVals = as.list(coef(summary(lm(gini_mkt_chg1 ~ lrgdpnaPerCapita_chg1 + lcgdpoPerCapita_gap_chg1)))[,4]))]
  
  
  
  
  
  
  
  
  
  #### Make some plots
  
  #### Make plot 1: Typical LAC country (mean, median) RGDPNA growth during period
  #### (plotting both annual change and 7-year changes)
  
  # First, construct "pseudo-year" variable: equal to end year when in given long-period
  pwt[ year >= 1962, pseudoYear := 1968]
  pwt[ year >= 1969, pseudoYear := 1975]
  pwt[ year >= 1976, pseudoYear := 1982]
  pwt[ year >= 1983, pseudoYear := 1989]
  pwt[ year >= 1990, pseudoYear := 1996]
  pwt[ year >= 1997, pseudoYear := 2003]
  pwt[ year >= 2004, pseudoYear := 2010]
  pwt[ year >= 2011, pseudoYear := 2017]
  
  # Construct dataset of just these years
  pwt_7_temp <- pwt[ , .(alpha3,subRegion_new,year,lrgdpnaPerCapita_chg7)][ year == 2017 | year == 2010 | year == 2003 | year == 1996 | year == 1989 | year == 1982 | year == 1975 | year == 1968]
  
  # Construct average for each region
  
  pwt_7_temp[ , lrgdpnaPerCapita_chg7_subRegionMean := mean(lrgdpnaPerCapita_chg7), by = .(subRegion_new,year)]
  
  setnames(pwt_7_temp,"year","pseudoYear")
  
  setkey(pwt_7_temp,alpha3,pseudoYear)
  setnames(pwt_7_temp,"lrgdpnaPerCapita_chg7","lrgdpnaPerCapita_chg7_temp")
  setkey(pwt,alpha3,pseudoYear)
  
  pwt <- pwt_7_temp[ , .(alpha3,pseudoYear,lrgdpnaPerCapita_chg7_temp,lrgdpnaPerCapita_chg7_subRegionMean)][pwt]
  
  LAC_growth_mean <- pwt[ , .(alpha3,country,subRegion_new,year,lrgdpnaPerCapita_chg7_temp,lrgdpnaPerCapita_chg1)][ subRegion_new == "Latin America and the Caribbean" & year >= 1962][ , lapply(.SD[ , .(lrgdpnaPerCapita_chg7_temp,lrgdpnaPerCapita_chg1)],mean), by = .(year)]
  LAC_growth_median <- pwt[ , .(alpha3,country,subRegion_new,year,lrgdpnaPerCapita_chg7_temp,lrgdpnaPerCapita_chg1)][ subRegion_new == "Latin America and the Caribbean" & year >= 1962][ , lapply(.SD[ , .(lrgdpnaPerCapita_chg7_temp,lrgdpnaPerCapita_chg1)],median), by = .(year)]
  LAC_growth_q1 <- pwt[ , .(alpha3,country,subRegion_new,year,lrgdpnaPerCapita_chg7_temp,lrgdpnaPerCapita_chg1)][ subRegion_new == "Latin America and the Caribbean" & year >= 1962][ , lapply(.SD[ , .(lrgdpnaPerCapita_chg7_temp,lrgdpnaPerCapita_chg1)],quantile,0.25), by = .(year)]
  LAC_growth_q3 <- pwt[ , .(alpha3,country,subRegion_new,year,lrgdpnaPerCapita_chg7_temp,lrgdpnaPerCapita_chg1)][ subRegion_new == "Latin America and the Caribbean" & year >= 1962][ , lapply(.SD[ , .(lrgdpnaPerCapita_chg7_temp,lrgdpnaPerCapita_chg1)],quantile,0.75), by = .(year)]
  
  
  LAC_onlyCarib_growth_mean <- pwt[alpha3 == "DOM" | alpha3 == "TTO" | alpha3 == "BRB" | alpha3 == "JAM"][ , .(alpha3,country,year,lrgdpnaPerCapita_chg7_temp,lrgdpnaPerCapita_chg1)][ year >= 1962][ , lapply(.SD[ , .(lrgdpnaPerCapita_chg7_temp,lrgdpnaPerCapita_chg1)],mean), by = .(year)]
  
  ggplot(LAC_growth_mean, aes(year)) + 
    geom_line(aes(y = lrgdpnaPerCapita_chg1, linetype  = "lrgdpnaPerCapita_chg1")) + 
    geom_line(aes(y = lrgdpnaPerCapita_chg7_temp, linetype = "lrgdpnaPerCapita_chg7_temp")) + 
    #geom_bar(aes(y = lrgdpnaPerCapita_chg7_temp), stat = "identity") + 
    labs(title = "LAC historical per capita output growth dynamics") + 
    labs(subtitle = "Mean country, %") +
    xlab("Year") + 
    ylab("Per capita output growth") + 
    scale_x_continuous(breaks = round(seq(min(LAC_growth_mean$year), max(LAC_growth_mean$year), by = 7),1)) +
    scale_linetype_discrete( name = "Legend", labels = c("Annual","7-year average")) + 
    #labs(color = "Legend") +
    theme(legend.position = "bottom")
    ##theme_bw()
  
  ggsave("figures/rgdpGrowth_LACmean.pdf",plot = last_plot(), width = 9, height = 6.5, units = "in")
  
  ggplot(LAC_growth_median, aes(year)) + 
    geom_line(aes(y = lrgdpnaPerCapita_chg1, linetype  = "lrgdpnaPerCapita_chg1")) + 
    geom_line(aes(y = lrgdpnaPerCapita_chg7_temp, linetype = "lrgdpnaPerCapita_chg7_temp")) + 
    #geom_bar(aes(y = lrgdpnaPerCapita_chg7_temp), stat = "identity") + 
    labs(title = "LAC historical per capita output growth dynamics") + 
    labs(subtitle = "Median country, %") +
    xlab("Year") + 
    ylab("Per capita output growth") + 
    scale_x_continuous(breaks = round(seq(min(LAC_growth_mean$year), max(LAC_growth_mean$year), by = 7),1)) +
    scale_linetype_discrete( name = "Legend", labels = c("Annual","7-year average")) + 
    #labs(color = "Legend") +
    theme(legend.position = "bottom")
  ##theme_bw()
  
  ggsave("figures/rgdpGrowth_LACmedian.pdf",plot = last_plot(), width = 9, height = 6.5, units = "in")
  
  ggplot(LAC_growth_q1, aes(year)) + 
    geom_line(aes(y = lrgdpnaPerCapita_chg1, color = "lrgdpnaPerCapita_chg1")) + 
    geom_line(aes(y = lrgdpnaPerCapita_chg7_temp, color = "lrgdpnaPerCapita_chg7_temp")) + 
    ggtitle("25th percentile RGDP / growth rates in LAC countries") +
    xlab("Year") + 
    ylab("Per-capita RGDP growth") + 
    scale_color_discrete( labels = c("Annual RGDP growth","7-year average")) + 
    labs(color = "Legend") +
    ##theme_bw()
  
  ggsave("figures/rgdpGrowth_LACq1.pdf",plot = last_plot())
  
  ggplot(LAC_growth_q3, aes(year)) + 
    geom_line(aes(y = lrgdpnaPerCapita_chg1, color = "lrgdpnaPerCapita_chg1")) + 
    geom_line(aes(y = lrgdpnaPerCapita_chg7_temp, color = "lrgdpnaPerCapita_chg7_temp")) + 
    ggtitle("75th percentile RGDP / growth rates in LAC countries") +
    xlab("Year") + 
    ylab("Per-capita RGDP growth") + 
    scale_color_discrete( labels = c("Annual RGDP growth","7-year average")) + 
    labs(color = "Legend") +
    ##theme_bw()
  
  ggsave("figures/rgdpGrowth_LACq3.pdf",plot = last_plot())
  
  ggplot(LAC_onlyCarib_growth_mean, aes(year)) + 
    geom_line(aes(y = lrgdpnaPerCapita_chg1, color = "lrgdpnaPerCapita_chg1")) + 
    geom_line(aes(y = lrgdpnaPerCapita_chg7_temp, color = "lrgdpnaPerCapita_chg7_temp")) + 
    ggtitle("Mean percentile RGDP / growth rates in Caribbean countries") +
    xlab("Year") + 
    ylab("Per-capita RGDP growth") + 
    scale_color_discrete( labels = c("Annual RGDP growth","7-year average")) + 
    labs(color = "Legend") +
    ##theme_bw()
  
  ggsave("figures/rgdpGrowth_LAC_onlyCarib_mean.pdf",plot = last_plot())
  
  
  #### Plot 1, part 2: compare individual country cases to LAC average
  
  ggplot(pwt[subRegion_new == "Latin America and the Caribbean"],aes(year)) +
    geom_line(aes(y = lrgdpnaPerCapita_chg7_temp, linetype  = "lrgdpnaPerCapita_chg7_temp")) + 
    geom_line(aes(y = lrgdpnaPerCapita_chg7_subRegionMean, linetype  = "lrgdpnaPerCapita_chg7_subRegionMean")) +
    labs(title = "LAC historical per capita output growth dynamics")+
    labs(subtitle = "Individual countries, %") +
    xlab("Year") +
    ylab("Per capita output growth") +
    scale_x_continuous(breaks = round(seq(min(LAC_growth_mean$year), max(LAC_growth_mean$year), by = 14),1)) +
    scale_linetype_discrete( name = "Legend", labels = c("LAC (mean)","Country")) + 
    facet_wrap(~ alpha3, ncol = 4) +
    theme(legend.position = "bottom")
    
  
  ggsave("figures/rgdpnaGrowth_LAC_countryCases.pdf", plot = last_plot(), width = 12, height = 9, units = "in")
  
  
    ##### Construct cumulative growth variables
  
  pwt_7 <- pwt[ year == 2017 | year == 2010 | year == 2003 | year == 1996 | year == 1989 | year == 1982 | year == 1975 | year == 1968]
  
  pwt_7[ , cumulativeTFP := cumsum(lrtfpna_chg7) / 8, by = .(alpha3)]
  pwt_7[ , cumulativeNonTFP := cumsum(lrgdpnaPerCapita_chg7_nonTFP) / 8, by = .(alpha3)]
  
  #pwt_7[ , cumulativeTFPcorrected := cumsum(lrtfpna_chg7 * 1.4) / 8, by = .(alpha3)]
  #pwt_7[ , cumulativeNonTFPcorrected := cumsum(lrgdpnaPerCapita_chg7_nonTFP / 1.4) / 8, by = .(alpha3)]
  
  pwt_7[ year >= 1996, post1990 := 1]
  pwt_7[ is.na(post1990) , post1990 := 0]
  
  pwt_7[ , cumulativeTFP1990 := cumsum(lrtfpna_chg7 * post1990) / 4, by = .(alpha3)]
  pwt_7[ , cumulativeNonTFP1990 := cumsum(lrgdpnaPerCapita_chg7_nonTFP * post1990) / 4, by = .(alpha3)]
  
  last_obs <- pwt_7[ , .SD[.N ], by = .(alpha3)]
  
  ### Make scatter plot version of Table 1 for whole world not just LAC
  
  ggplot(last_obs, aes(x = cumulativeTFP, y = cumulativeNonTFP, color = subRegion_new)) + 
    #geom_point(size = 0, shape = 1) + 
    geom_text(aes(label = alpha3), size = 2) + 
    labs(title = "Decomposition of per capita output growth worldwide ") + 
    labs(subtitle = "1962-2017, annualized, %") +
    labs( x= "Average RTFPNA growth", y = "Average non-RTFPNA RGDPNA / capita growth", color = "Legend") + 
    geom_abline(slope = 1, linetype = 2, size = 0.4) + 
    geom_abline(slope = -1, intercept = -1, size = 0.4, linetype = 3) + 
    geom_abline(slope = -1, intercept = 0, size = 0.4, linetype = 3) + 
    geom_abline(slope = -1, intercept = 1, size = 0.4, linetype = 3) + 
    geom_abline(slope = -1, intercept = 2, size = 0.4, linetype = 3) + 
    geom_abline(slope = -1, intercept = 3, size = 0.4, linetype = 3) + 
    geom_abline(slope = -1, intercept = 4, size = 0.4, linetype = 3) + 
    geom_abline(slope = -1, intercept = 5, size = 0.4, linetype = 3) + 
    geom_abline(slope = -1, intercept = 6, size = 0.4, linetype = 3) + 
    geom_vline(xintercept = 0, linetype = 5) + 
    geom_hline(yintercept = 0, linetype = 5) + 
    ylab("Factors") + 
    xlab("Productivity") + 
    labs(color = "Legend") +
    xlim(-2.5,2.5) + 
    ylim(-1,5) + 
    coord_fixed() +
    theme(legend.position = "bottom") 
    ##theme_bw()
  
  ggsave("figures/rgdpnaPerCapita_TFPvsNonTFP_scatterPlot.pdf", plot = last_plot(),  width = 9, height = 7, units = "in")
  
  ggplot(last_obs, aes(x = cumulativeTFP1990, y = cumulativeNonTFP1990, color = subRegion_new)) + 
    #geom_point(size = 0, shape = 1) + 
    geom_text(aes(label = alpha3), size = 2) + 
    labs(title = "Decomposition of per capita output growth worldwide ") + 
    labs(subtitle = "1990-2017, annualized, %") +
    labs( x= "Average RTFPNA growth", y = "Average non-RTFPNA RGDPNA / capita growth", color = "Legend") + 
    geom_abline(slope = 1, linetype = 2, size = 0.4) + 
    geom_abline(slope = -1, intercept = -1, size = 0.4, linetype = 3) + 
    geom_abline(slope = -1, intercept = 0, size = 0.4, linetype = 3) + 
    geom_abline(slope = -1, intercept = 1, size = 0.4, linetype = 3) + 
    geom_abline(slope = -1, intercept = 2, size = 0.4, linetype = 3) + 
    geom_abline(slope = -1, intercept = 3, size = 0.4, linetype = 3) + 
    geom_abline(slope = -1, intercept = 4, size = 0.4, linetype = 3) + 
    geom_abline(slope = -1, intercept = 5, size = 0.4, linetype = 3) + 
    geom_abline(slope = -1, intercept = 6, size = 0.4, linetype = 3) + 
    geom_vline(xintercept = 0, linetype = 5) + 
    geom_hline(yintercept = 0, linetype = 5) + 
    ylab("Factors") + 
    xlab("Productivity") + 
    labs(color = "Legend") +
    xlim(-2.5,2.5) + 
    ylim(-1,5) + 
    coord_fixed() +
    theme(legend.position = "bottom") 
  ##theme_bw()
  
  ggsave("figures/rgdpnaPerCapita_TFPvsNonTFP_scatterPlot_post1990.pdf", plot = last_plot(),  width = 9, height = 7, units = "in")
  
  
  ### Make Table 1: LAC countries TFP and non-TFP cumulative growth
  
  
  cumulativeGrowthLAC <- last_obs[ , .(alpha3,subRegion_new,cumulativeTFP,cumulativeNonTFP,cumulativeTFP1990,cumulativeNonTFP1990)][ subRegion_new == "Latin America and the Caribbean"]
  newObs <- list("LAC","Latin America and the Caribbean",mean(cumulativeGrowthLAC$cumulativeTFP), mean(cumulativeGrowthLAC$cumulativeNonTFP),
                 mean(cumulativeGrowthLAC$cumulativeTFP1990), mean(cumulativeGrowthLAC$cumulativeNonTFP1990))
  cumulativeGrowthLAC <- rbind(cumulativeGrowthLAC,newObs)
  
  cumulativeGrowthLAC_copy <- copy(cumulativeGrowthLAC)
  cumulativeGrowthLAC_copy[ , Total := cumulativeTFP + cumulativeNonTFP]
  cumulativeGrowthLAC_copy[ , Total1990 := cumulativeTFP1990 + cumulativeNonTFP1990]
  setkey(cumulativeGrowthLAC_copy,alpha3)
    
  cumulativeGrowthLAC <- melt(cumulativeGrowthLAC, id.vars = c("alpha3","subRegion_new"), measure.vars = c("cumulativeTFP","cumulativeNonTFP","cumulativeTFP1990","cumulativeNonTFP1990"))
  setkey(cumulativeGrowthLAC,alpha3)
  
  cumulativeGrowthLAC <- cumulativeGrowthLAC_copy[cumulativeGrowthLAC]
  
  ggplot( cumulativeGrowthLAC[variable %in% c("cumulativeTFP","cumulativeNonTFP")], aes(x = reorder(alpha3, Total))) + 
    geom_bar(stat = "identity", aes(y = value, fill = variable)) +
    geom_col(position = position_stack(reverse = TRUE), aes(y = value, fill = variable)) + 
    geom_point(aes(y = Total, shape = "Total"), size = 2, color = "black") + 
    xlab("\nCountry") + 
    ylab("Growth decomposition\n") +  
    labs(title = "Decomposition of per capita output growth in LAC countries")+
    labs(subtitle = "1962-2017, annualized, %") + 
    scale_fill_discrete(name = "Legend", labels = c("Productivity","Factors")) + 
    scale_shape_discrete(name = "", labels = "Total") + 
    coord_flip() +
    theme(legend.position = "bottom")
    #theme_bw()
  
  ggsave("figures/cumulativeGrowthDecomposition_LAC_barplot.pdf",plot = last_plot(),  width = 9, height = 6.5, units = "in")
  
  ggplot( cumulativeGrowthLAC[variable %in% c("cumulativeTFP1990","cumulativeNonTFP1990")], aes(x = reorder(alpha3, Total1990))) + 
    geom_bar(stat = "identity", aes(y = value, fill = variable)) +
    geom_col(position = position_stack(reverse = TRUE), aes(y = value, fill = variable)) + 
    geom_point(aes(y = Total1990, shape = "Total"), size = 2, color = "black") + 
    xlab("\nCountry") + 
    ylab("Growth decomposition\n") +  
    labs(title = "Decomposition of per capita output growth in LAC countries")+
    labs(subtitle = "1990-2017, annualized, %") + 
    scale_fill_discrete(name = "Legend", labels = c("Productivity","Factors")) + 
    scale_shape_discrete(name = "", labels = "Total") + 
    coord_flip() +
    theme(legend.position = "bottom") +
    #scale_fill_grey(name = "Legend", labels = c("Productivity","Factors"))
  #theme_bw()
  
  ggsave("figures/cumulativeGrowthDecomposition_LAC_barplot_post1990.pdf",plot = last_plot(),  width = 9, height = 6.5, units = "in")
    
  ### Make Table 2: LAC average, decomposition of each 8-year period
  
  
    allPeriodsGrowthLAC_7 <- pwt_7[ subRegion_new == "Latin America and the Caribbean", lapply(.SD[ ,.(lrgdpnaPerCapita_chg7_nonTFP, lrtfpna_chg7)], mean), by = .(year)]
  obsMeanPre1990 <- list("Average pre 1990",mean(allPeriodsGrowthLAC_7[year <= 1989]$lrgdpnaPerCapita_chg7_nonTFP), mean(allPeriodsGrowthLAC_7[year <= 1989]$lrtfpna_chg7))
  obsMeanPost1990 <- list("Average post 1990",mean(allPeriodsGrowthLAC_7[year > 1989]$lrgdpnaPerCapita_chg7_nonTFP), mean(allPeriodsGrowthLAC_7[year > 1989]$lrtfpna_chg7))
  obsMeanAll <- list("Overall average",mean(allPeriodsGrowthLAC_7$lrgdpnaPerCapita_chg7_nonTFP), mean(allPeriodsGrowthLAC_7$lrtfpna_chg7))
  
  allPeriodsGrowthLAC_7 <- rbindlist(list(allPeriodsGrowthLAC_7[1:8,],obsMeanPre1990,obsMeanPost1990,obsMeanAll))
  allPeriodsGrowthLAC_7[ , lrgdpnaPerCapita_chg7 := lrgdpnaPerCapita_chg7_nonTFP + lrtfpna_chg7]
  
  ## Print to xls
  allPeriodsGrowthLAC_7_excel <- allPeriodsGrowthLAC_7 
  names(allPeriodsGrowthLAC_7_excel) <- c("Period end","Factors","Productivity","Total")
  setcolorder(allPeriodsGrowthLAC_7_excel,c("Period end","Productivity","Factors","Total"))
  write.xlsx(allPeriodsGrowthLAC_7_excel,"figures/tables/excel/LACgrowthDecomposition.xlsx")
    
  kable(allPeriodsGrowthLAC_7_excel, "latex", booktabs = T, digits = 2, linesep = "", caption = "LAC per capita output growth decomposition.") %>% kable_styling(latex_options = c("hold_position")) %>% 
    pack_rows("Averages",nrow(allPeriodsGrowthLAC_7_excel)-2,nrow(allPeriodsGrowthLAC_7_excel)) %>% cat(., file = "figures/tables/LACgrowthDecomposition_table.tex") 
  
      #table2 <- xtable(allPeriodsGrowthLAC_7_excel,align = c("l","r","r","r","r"), caption = c("LAC output growth decomposition."), caption.placement = "top", digits = 2)
  #names(table2) <- c("Period end","non-TFP","TFP","Total")
  #print(table2,"figures/tables/LACgrowthDecomposition_table.tex", type = "latex", caption.placement = "top", hline.after = c(-1,0,nrow(table2)-3,nrow(table2)), include.rownames = FALSE, booktabs = TRUE)
  
    # Clean up temporary
  #rm(allPeriodsGrowthLAC_7_excel)
  
  ## additional: compute correlation
  
  # Correlation of growth rates
  cor_rgdpnaPerCapitaGrowth_rtfpnaGrowth <- cor(allPeriodsGrowthLAC_7$lrgdpnaPerCapita_chg7,allPeriodsGrowthLAC_7$lrtfpna_chg7)
  cor_rgdpnaPerCapitaGrowth_nonTFPGrowth <- cor(allPeriodsGrowthLAC_7$lrgdpnaPerCapita_chg7,allPeriodsGrowthLAC_7$lrgdpnaPerCapita_chg7_nonTFP)
  
  allPeriodsGrowthLAC_7[ , lrgdpnaPerCapita_chg7_chg := lrgdpnaPerCapita_chg7 - shift(lrgdpnaPerCapita_chg7,n=1L,type="lag")]
  allPeriodsGrowthLAC_7[ , lrtfpna_chg7_chg := lrtfpna_chg7 - shift(lrtfpna_chg7,n=1L,type="lag")]
  allPeriodsGrowthLAC_7[ , lrgdpnaPerCapita_chg7_nonTFP_chg := lrgdpnaPerCapita_chg7_nonTFP - shift(lrgdpnaPerCapita_chg7_nonTFP,n=1L,type="lag")]
  
  cor_rgdpnaPerCapitaGrowthChg_rtfpnaGrowthChg <- cor(na.omit(allPeriodsGrowthLAC_7$lrgdpnaPerCapita_chg7_chg),na.omit(allPeriodsGrowthLAC_7$lrtfpna_chg7_chg))
  cor_rgdpnaPerCapitaGrowthChg_nonTFPGrowthChg <- cor(na.omit(allPeriodsGrowthLAC_7$lrgdpnaPerCapita_chg7_chg),na.omit(allPeriodsGrowthLAC_7$lrgdpnaPerCapita_chg7_nonTFP_chg))
  
  obs1 <- list("RGDPNA per capita growth","RTFPNA growth",cor_rgdpnaPerCapitaGrowth_rtfpnaGrowth)
  obs2 <- list("RGDPNA per capita growth","non-RTFPNA growth",cor_rgdpnaPerCapitaGrowth_nonTFPGrowth)
  obs3 <- list("RGDPNA per capita growth chg","RTFPNA growth chg",cor_rgdpnaPerCapitaGrowthChg_rtfpnaGrowthChg)
  obs4 <- list("RGDPNA per capita growth chg","non-RTFPNA growth chg",cor_rgdpnaPerCapitaGrowthChg_nonTFPGrowthChg)
  
  table2b_correlations_excel <- rbind(obs1,obs2,obs3,obs4)
  write.xlsx(table2b_correlations_excel,"figures/tables/excel/LACgrowthDecomposition_correlations.xlsx")
  
  table2b_correlations <- xtable(rbind(obs1,obs2,obs3,obs4), caption = c("RGDPNA per capita growth, and growth changes, are highly correlated with TFP growth, and TFP growth changes. Moreso than with non-TFP growth, growth changes."))
  names(table2b_correlations) <- c("Series 1","Series 2","Correlation")
  print(table2b_correlations,"figures/tables/LACgrowthDecomposition_correlations_table.tex", type = "latex", include.rownames = FALSE)
  
  ## Finally, compare LAC to benchmarks
  
  allPeriodsGrowth_7 <- pwt_7[, lapply(.SD[ ,.(lrgdpnaPerCapita_chg7_nonTFP, lrtfpna_chg7)], mean), by = .(subRegion_new,year)]
    
  allPeriodsGrowth_7[ , lrgdpna_chg7 := lrgdpnaPerCapita_chg7_nonTFP + lrtfpna_chg7]
  
  allPeriodsGrowth_7 <- dcast(allPeriodsGrowth_7, year ~ subRegion_new, value.var = c("lrgdpna_chg7","lrgdpnaPerCapita_chg7_nonTFP","lrtfpna_chg7"))
  
  ## Latin America vs Africa
  
  LACvsAfrica <- allPeriodsGrowth_7[ , .(year = year,
                                         lrgdpna_gap = `lrgdpna_chg7_Latin America and the Caribbean` - `lrgdpna_chg7_Africa`, lrgdpna_nonTFP_gap = `lrgdpnaPerCapita_chg7_nonTFP_Latin America and the Caribbean` - `lrgdpnaPerCapita_chg7_nonTFP_Africa`, 
                                         lrtfpna_gap = `lrtfpna_chg7_Latin America and the Caribbean` - `lrtfpna_chg7_Africa`)]
  
  pre1990 <- LACvsAfrica[ year <= 1989, lapply(.SD,mean)]
  pre1990$year[1] <- "Average pre 1990"
  
  post1990 <- LACvsAfrica[ year > 1989, lapply(.SD,mean)]
  post1990$year[1] <- "Average post 1990"
  
  meanAll <- LACvsAfrica[ , lapply(.SD,mean)]
  meanAll$year[1] <- "Overall average"
  
  LACvsAfrica <- rbindlist(list(LACvsAfrica[1:8],pre1990,post1990,meanAll))
  
  # Convert to percentage units
  #LACvsAfrica[ , c("lrgdpna_gap","lrgdpna_nonTFP_gap","lrtfpna_gap") := lapply(.SD[ , .(lrgdpna_gap,lrgdpna_nonTFP_gap,lrtfpna_gap)], "*",100) ]
  
  names(LACvsAfrica) <- c("Period end","Total","Factors","Productivity")
  
  setcolorder(LACvsAfrica,c("Period end","Productivity","Factors","Total"))
  
  LACvsAFR_raw <- LACvsAfrica
  
  write.xlsx(LACvsAfrica,"figures/tables/excel/LACgrowthDecomposition_benchmark_Africa.xlsx")
  
  kable(LACvsAfrica, "latex", booktabs = T, digits = 2, linesep = "", caption = "LAC per capita output growth gap: Africa benchmark") %>% kable_styling(latex_options = c("hold_position")) %>% 
    pack_rows("Averages",nrow(LACvsAfrica)-2,nrow(LACvsAfrica)) %>% cat(., file = "figures/tables/LACgrowthDecomposition_benchmark_Africa.tex") 
  
  #LACvsAfrica <- xtable(LACvsAfrica,align = c("r","r","r","r","r"), caption = c("LAC growth gap: Africa benchmark"), digits = 2) %>% xtable2kable("latex", booktabs = T) %>% kable_styling(latex_options = "striped") %>%
   # pack_rows("Averages",nrow(LACvsAfrica)-3,nrow(LACvsAfrica)) %>% cat(., file = "figures/tables/LACgrowthDecomposition_benchmark_Africa.tex") 
        
#  print(LACvsAfrica,"figures/tables/LACgrowthDecomposition_benchmark_Africa.tex", caption.placement = "top", type = "latex",  booktabs = TRUE, include.rownames = FALSE, size = "\\footnotesize")
  
  
  ## Latin America vs East Asian Tigers
  
  LACvsEAT <- allPeriodsGrowth_7[ , .(year = year,
                                         lrgdpna_gap = `lrgdpna_chg7_Latin America and the Caribbean` - `lrgdpna_chg7_East Asian Tigers`, lrgdpna_nonTFP_gap = `lrgdpnaPerCapita_chg7_nonTFP_Latin America and the Caribbean` - `lrgdpnaPerCapita_chg7_nonTFP_East Asian Tigers`, 
                                         lrtfpna_gap = `lrtfpna_chg7_Latin America and the Caribbean` - `lrtfpna_chg7_East Asian Tigers`)]
  
  pre1990 <- LACvsEAT[ year <= 1989, lapply(.SD,mean)]
  pre1990$year[1] <- "Average pre 1990"
  
  post1990 <- LACvsEAT[ year > 1989, lapply(.SD,mean)]
  post1990$year[1] <- "Average post 1990"
  
  meanAll <- LACvsEAT[ , lapply(.SD,mean)]
  meanAll$year[1] <- "Overall average"
  
  LACvsEAT <- rbindlist(list(LACvsEAT[1:8],pre1990,post1990,meanAll))
  
  # Convert to percentage units
  #LACvsEAT[ , c("lrgdpna_gap","lrgdpna_nonTFP_gap","lrtfpna_gap") := lapply(.SD[ , .(lrgdpna_gap,lrgdpna_nonTFP_gap,lrtfpna_gap)], "*",100) ]
  
  names(LACvsEAT) <- c("Period end","Total","Factors","Productivity")
  
  setcolorder(LACvsEAT,c("Period end","Productivity","Factors","Total"))
  # Store For use later
  LACvsEAT_raw <- LACvsEAT
  
  write.xlsx(LACvsEAT,"figures/tables/excel/LACgrowthDecomposition_benchmark_EAT.xlsx")
  
  kable(LACvsEAT, "latex", booktabs = T, digits = 2, linesep = "", caption = "LAC per capita output growth gap: EAT benchmark") %>% kable_styling(latex_options = c("hold_position")) %>% 
    pack_rows("Averages",nrow(LACvsEAT)-2,nrow(LACvsEAT)) %>% cat(., file = "figures/tables/LACgrowthDecomposition_benchmark_EAT.tex") 
  
  
  #LACvsEAT <- xtable(LACvsEAT,align = c("r","r","r","r","r"), caption = c("LAC vs East Asian Tigers benchmark: RGDPNA per capita growth gap decomposition by 7-yr period and cumulative."), digits = 2)
  #print(LACvsEAT,"figures/tables/LACgrowthDecomposition_benchmark_EAT.tex", hline.after = c(-1,0,nrow(LACvsEAT)-3,nrow(LACvsEAT)), type = "latex", booktabs = TRUE, include.rownames = FALSE, size = "\\footnotesize")
  
  
  ## Latin America vs US
  
  LACvsUSA <- allPeriodsGrowth_7[ , .(year = year,
                                      lrgdpna_gap = `lrgdpna_chg7_Latin America and the Caribbean` - `lrgdpna_chg7_United States`, lrgdpna_nonTFP_gap = `lrgdpnaPerCapita_chg7_nonTFP_Latin America and the Caribbean` - `lrgdpnaPerCapita_chg7_nonTFP_United States`, 
                                      lrtfpna_gap = `lrtfpna_chg7_Latin America and the Caribbean` - `lrtfpna_chg7_United States`)]
  
  pre1990 <- LACvsUSA[ year <= 1989, lapply(.SD,mean)]
  pre1990$year[1] <- "Average pre 1990"
  
  post1990 <- LACvsUSA[ year > 1989, lapply(.SD,mean)]
  post1990$year[1] <- "Average post 1990"
  
  meanAll <- LACvsUSA[ , lapply(.SD,mean)]
  meanAll$year[1] <-  "Overall average"
  
  LACvsUSA <- rbindlist(list(LACvsUSA[1:8],pre1990,post1990,meanAll))
  
  # Convert to percentage units
  #LACvsUSA[ , c("lrgdpna_gap","lrgdpna_nonTFP_gap","lrtfpna_gap") := lapply(.SD[ , .(lrgdpna_gap,lrgdpna_nonTFP_gap,lrtfpna_gap)], "*",100) ]
  
  names(LACvsUSA) <- c("Period end","Total","Factors","Productivity")
  
  setcolorder(LACvsUSA,c("Period end","Productivity","Factors","Total"))
  
  # Store For use later
  LACvsUSA_raw <- LACvsUSA
  
  write.xlsx(LACvsUSA,"figures/tables/excel/LACgrowthDecomposition_benchmark_USA.xlsx")
  
  kable(LACvsUSA, "latex", booktabs = T, digits = 2, linesep = "", caption = "LAC per capita output growth gap: USA benchmark") %>% kable_styling(latex_options = c("hold_position")) %>% 
    pack_rows("Averages",nrow(LACvsUSA)-2,nrow(LACvsUSA)) %>% cat(., file = "figures/tables/LACgrowthDecomposition_benchmark_USA.tex") 
  
  #LACvsUSA <- xtable(LACvsUSA,align = c("r","r","r","r","r"), caption = c("LAC vs USA benchmark: RGDPNA per capita growth gap decomposition by 7-yr period and cumulative."), digits = 2)
  #print(LACvsUSA,"figures/tables/LACgrowthDecomposition_benchmark_USA.tex", hline.after = c(-1,0,nrow(LACvsUSA)-3,nrow(LACvsUSA)),  type = "latex", booktabs = TRUE, include.rownames = FALSE, size = "\\footnotesize")
  
  
  ### LAC vs non-LAC benchmark
  
  LAC_nonLAC_allPeriodsGrowth_7 <- pwt_7[, lapply(.SD[ ,.(lrgdpnaPerCapita_chg7_nonTFP, lrtfpna_chg7)], mean), by = .(isLAC,year)]
  
  LAC_nonLAC_allPeriodsGrowth_7[ , lrgdpna_chg7 := lrgdpnaPerCapita_chg7_nonTFP + lrtfpna_chg7]
  
  LAC_nonLAC_allPeriodsGrowth_7 <- dcast(LAC_nonLAC_allPeriodsGrowth_7, year ~ isLAC, value.var = c("lrgdpna_chg7","lrgdpnaPerCapita_chg7_nonTFP","lrtfpna_chg7"))
  
  LACvsnonLAC <- LAC_nonLAC_allPeriodsGrowth_7[ , .(year = year,
                                      lrgdpna_gap = `lrgdpna_chg7_1` - `lrgdpna_chg7_0`, lrgdpna_nonTFP_gap = `lrgdpnaPerCapita_chg7_nonTFP_1` - `lrgdpnaPerCapita_chg7_nonTFP_0`, 
                                      lrtfpna_gap = `lrtfpna_chg7_1` - `lrtfpna_chg7_0`)]
  
      
  
  pre1990 <- LACvsnonLAC[ year <= 1989, lapply(.SD,mean)]
  pre1990$year[1] <- "Average pre 1990"
  
  post1990 <- LACvsnonLAC[ year > 1989, lapply(.SD,mean)]
  post1990$year[1] <- "Average post 1990"
  
  meanAll <- LACvsnonLAC[ , lapply(.SD,mean)]
  meanAll$year[1] <- "Overall average"
  
  LACvsnonLAC <- rbindlist(list(LACvsnonLAC[1:8],pre1990,post1990,meanAll))
  
  # Convert to percentage units
  #LACvsnonLAC[ , c("lrgdpna_gap","lrgdpna_nonTFP_gap","lrtfpna_gap") := lapply(.SD[ , .(lrgdpna_gap,lrgdpna_nonTFP_gap,lrtfpna_gap)], "*",100) ]
  
  names(LACvsnonLAC) <- c("Period end","Total","Factors","Productivity")
  
  setcolorder(LACvsnonLAC,c("Period end","Productivity","Factors","Total"))
  
  # Store For use later
  LACvsnonLAC_raw <- LACvsnonLAC
  
  write.xlsx(LACvsnonLAC,"figures/tables/excel/LACgrowthDecomposition_benchmark_nonLAC.xlsx")
  
  kable(LACvsnonLAC, "latex", booktabs = T, digits = 2, linesep = "", caption = "LAC per capita output growth gap: non-LAC benchmark") %>% kable_styling(latex_options = c("hold_position")) %>% 
    pack_rows("Averages",nrow(LACvsnonLAC)-2,nrow(LACvsnonLAC)) %>% cat(., file = "figures/tables/LACgrowthDecomposition_benchmark_nonLAC.tex") 
  
  #LACvsnonLAC <- xtable(LACvsnonLAC,align = c("r","r","r","r","r"), caption = c("LAC vs non-LAC benchmark: RGDPNA per capita growth gap decomposition by 7-yr period and cumulative."), digits = 2)
  #print(LACvsnonLAC,"figures/tables/LACgrowthDecomposition_benchmark_nonLAC.tex",  hline.after = c(-1,0,nrow(LACvsnonLAC)-3,nrow(LACvsnonLAC)), type = "latex", booktabs = TRUE,  include.rownames = FALSE, size = "\\footnotesize")
  
  
  
  ###############
  ##### Make some plots...
  #############
  
  ### Make Plot: LAC changes in GDP growth vs changes in TFP growth
  
  allPeriodsGrowthLAC_1 <- pwt[ subRegion_new == "Latin America and the Caribbean", lapply(.SD[ ,.(lrgdpnaPerCapita_chg1_nonTFP, lrtfpna_chg1)], mean), by = .(year)]
  allPeriodsGrowthLAC_1 <- allPeriodsGrowthLAC_1[order(year)]
  allPeriodsGrowthLAC_1[ , lrgdpnaPerCapita_chg1 := lrgdpnaPerCapita_chg1_nonTFP + lrtfpna_chg1]
  allPeriodsGrowthLAC_1[ , `:=` (lrgdpnaPerCapita_chg1_nonTFP_chg1 = lrgdpnaPerCapita_chg1_nonTFP - shift(lrgdpnaPerCapita_chg1_nonTFP,n=1L,type = "lag") , lrgdpnaPerCapita_chg1_chg1 = lrgdpnaPerCapita_chg1 - shift(lrgdpnaPerCapita_chg1,n=1L,type = "lag"), lrtfpna_chg1_chg1 = lrtfpna_chg1 - shift(lrtfpna_chg1,n=1L,type = "lag"))]
  
  ggplot(allPeriodsGrowthLAC_1, aes(year)) + 
    geom_line(aes(y = lrgdpnaPerCapita_chg1_chg1, color = "lrgdpnaPerCapita_chg1_chg1")) + 
    geom_line(aes(y = lrtfpna_chg1_chg1, color = "lrtfpna_chg1_chg1")) + 
    ggtitle("1-year chg in growth of rgdpna / capita, rtfpna move together (LAC average)") + 
    labs(color = "Legend") +
    #theme_bw()
  
  ggsave("figures/rgdpnaPerCapitaGrowthChg1_rtfpnaGrowthChg1_LAC_mean.pdf", plot = last_plot())
  
  ggplot(allPeriodsGrowthLAC_1, aes(year)) + 
    geom_line(aes(y = lrgdpnaPerCapita_chg1_chg1, color = "lrgdpnaPerCapita_chg1_chg1")) + 
    geom_line(aes(y = lrgdpnaPerCapita_chg1_nonTFP_chg1, color = "lrgdpnaPerCapita_chg1_nonTFP_chg1")) +
    ggtitle("1-year chg in growth of rgdpna / capita, non-TFP rgdpna / capita move together, but less (LAC average)") + 
    labs(color = "Legend") +
    #theme_bw()
  
  ggsave("figures/rgdpnaPerCapitaGrowthChg1_rgdpnaPerCapitaGrowthNonTFPChg1_LAC_mean.pdf", plot = last_plot())
  
  allPeriodsGrowthLAC <- pwt[ subRegion_new == "Latin America and the Caribbean", lapply(.SD[ ,.(lrgdpnaPerCapita_chg7_nonTFP, lrtfpna_chg7)], mean), by = .(year)]
  allPeriodsGrowthLAC <- allPeriodsGrowthLAC[order(year)]
  allPeriodsGrowthLAC[ , lrgdpnaPerCapita_chg7 := lrgdpnaPerCapita_chg7_nonTFP + lrtfpna_chg7]
  allPeriodsGrowthLAC[ , `:=` (lrgdpnaPerCapita_chg7_chg1 = lrgdpnaPerCapita_chg7 - shift(lrgdpnaPerCapita_chg7,n=1L,type = "lag") , lrgdpnaPerCapita_chg7_nonTFP_chg1 = lrgdpnaPerCapita_chg7_nonTFP - shift(lrgdpnaPerCapita_chg7_nonTFP,n=1L,type = "lag"), lrtfpna_chg7_chg1 = lrtfpna_chg7 - shift(lrtfpna_chg7,n=1L,type = "lag"))]
  
  ggplot(allPeriodsGrowthLAC, aes(year)) + 
    geom_line(aes(y = lrgdpnaPerCapita_chg7_chg1, color = "lrgdpnaPerCapita_chg7_chg1")) + 
    geom_line(aes(y = lrtfpna_chg7_chg1, color = "lrtfpna_chg7_chg1")) + 
    ggtitle("1-year chg in growth of rgdpna / capita, TFP move together, but less (LAC average, 7-year windows)") +
    labs(color = "Legend") +
    #theme_bw()
  
  ggsave("figures/rgdpnaPerCapitaGrowth7yrChg1_rtfpnaGrowth7yrChg1_LAC_mean.pdf", plot = last_plot())
    
  ggplot(allPeriodsGrowthLAC, aes(year)) + 
    geom_line(aes(y = lrgdpnaPerCapita_chg7_chg1, color = "lrgdpnaPerCapita_chg7_chg1")) + 
    geom_line(aes(y = lrgdpnaPerCapita_chg7_nonTFP_chg1, color = "lrgdpnaPerCapita_chg7_nonTFP_chg1")) + 
    ggtitle("1-year chg in growth of rgdpna / capita, non-TFP rgdpna / capita move together, but less (LAC average, 7-year windows)") + 
    labs(color = "Legend") +
    #theme_bw()
  
  ggsave("figures/rgdpnaPerCapitaGrowth7yrChg1_rgdpnaPerCapitaGrowth7yrNonTFPChg1_LAC_mean.pdf", plot = last_plot())
  
  # Record 7-year data for use in Stata
  
  # Contruct lag 
  pwt_7[ , lag_lcgdpoPerCapita_gap_avg7 := shift(lcgdpoPerCapita_gap_avg7, n = 1L, type = "lag"), by = "alpha3"]
  
  # Construct LAC country dummies + nonLAC
  
  #pwt_7[ subRegion_new == "Latin America and the Caribbean", countryLAC := alpha3]
  #pwt_7[ is.na(countryLAC), countryLAC := "nonLAC"]
  
  fwrite(pwt_7,"data/pwt_7.csv")
                  
  
  ####################
  ###### Construct expected growth rates for LAC countries using
  ###### regression based on non-LAC sample. Then compute gaps, and 
  ###### attribute gap to differential LAC convergence vs. other factors
  
  pwt_7 <- fread("data/pwt_7.csv")
  
  regResults <- fread("figures/tables/sec2_mainReg_timeDummies_LACinteractionsAll_nonlagGap_rollingWindows.csv")
  regResults2 <- fread("figures/tables/sec2_mainReg_noCountryRegionDummies_rollingWindows.csv")
  
  #regResults_annual <- fread("figures/tables/sec2_mainReg_timeDummies_LACinteractionsAll_nonlagGap_annual.csv")
  #regResults2_annual <- fread("figures/tables/sec2_mainReg_noCountryRegionDummies_annual.csv")
  
  # Clean
  regResults <- regResults[-1:-2]
  regResults <- regResults[-.N]
  
  regResults2 <- regResults2[-1:-2]
  regResults2 <- regResults2[-.N]
  
  # Extract constants and add to main dataset
  
  constant <- regResults[53]
  constant <- constant[ , .(V2,V3,V4)]
  
  constant2 <- regResults2[52]
  constant2 <- constant2[ , .(V2,V3,V4)]
  

  pwt_7[ , constant_tfpGrowth := as.numeric(constant$V2)]
  pwt_7[ , constant_nontfpGrowth := as.numeric(constant$V3)]
  pwt_7[ , constant_rgdpGrowth := as.numeric(constant$V4)]

  pwt_7[ , constant2_tfpGrowth := as.numeric(constant2$V2)]
  pwt_7[ , constant2_nontfpGrowth := as.numeric(constant2$V3)]
  pwt_7[ , constant2_rgdpGrowth := as.numeric(constant2$V4)]
  
  # Extract RGDP gap coefficients and add to main dataset
  
  rgdpGap <- regResults[1]
  rgdpGap2 <- regResults2[1]

  pwt_7[ , rgdpGap_tfpGrowth := as.numeric(rgdpGap$V2)]
  pwt_7[ , rgdpGap_nontfpGrowth := as.numeric(rgdpGap$V3)]
  pwt_7[ , rgdpGap_rgdpGrowth := as.numeric(rgdpGap$V4)]

  pwt_7[ , rgdpGap2_tfpGrowth := as.numeric(rgdpGap2$V2)]
  pwt_7[ , rgdpGap2_nontfpGrowth := as.numeric(rgdpGap2$V3)]
  pwt_7[ , rgdpGap2_rgdpGrowth := as.numeric(rgdpGap2$V4)]
  
  # Extract time dummy coefficients and link to main dataset
  
  timeDummies <- regResults[3:52]
  names(timeDummies) <- c("year","timeDummy_tfpGrowth","timeDummy_nontfpGrowth","timeDummy_rgdpGrowth")
  timeDummies[ , year := as.numeric(substr(year,6,9))]
  
  timeDummies2 <- regResults2[2:51]
  names(timeDummies2) <- c("year","timeDummy2_tfpGrowth","timeDummy2_nontfpGrowth","timeDummy2_rgdpGrowth")
  timeDummies2[ , year := as.numeric(substr(year,6,9))]
  
  setkey(timeDummies,year)
  setkey(pwt_7,year)
  
  
  
  pwt_7 <- timeDummies[pwt_7]
  
  setkey(timeDummies2,year)
  
  pwt_7 <- timeDummies2[pwt_7]
  
  ### Now, use coefficients to construct regression-predicted growth rates by country-year
  
  pwt_7[ , predicted_rgdpGrowth := constant_rgdpGrowth + as.numeric(timeDummy_rgdpGrowth) + lcgdpoPerCapita_gap_avg7 * rgdpGap_rgdpGrowth]
  pwt_7[ , predicted_tfpGrowth := constant_tfpGrowth + as.numeric(timeDummy_tfpGrowth) + lcgdpoPerCapita_gap_avg7 * rgdpGap_tfpGrowth]
  pwt_7[ , predicted_nontfpGrowth := constant_nontfpGrowth + as.numeric(timeDummy_nontfpGrowth) + lcgdpoPerCapita_gap_avg7 * rgdpGap_nontfpGrowth]
  
  pwt_7[ , predicted2_rgdpGrowth := constant2_rgdpGrowth + as.numeric(timeDummy2_rgdpGrowth) + lcgdpoPerCapita_gap_avg7 * rgdpGap2_rgdpGrowth]
  pwt_7[ , predicted2_tfpGrowth := constant2_tfpGrowth + as.numeric(timeDummy2_tfpGrowth) + lcgdpoPerCapita_gap_avg7 * rgdpGap2_tfpGrowth]
  pwt_7[ , predicted2_nontfpGrowth := constant2_nontfpGrowth + as.numeric(timeDummy2_nontfpGrowth) + lcgdpoPerCapita_gap_avg7 * rgdpGap2_nontfpGrowth]
  
  pwt_7[ , rgdpGrowthGap := lrgdpnaPerCapita_chg7 - predicted_rgdpGrowth]
  pwt_7[ , tfpGrowthGap := lrtfpna_chg7 - predicted_tfpGrowth]
  pwt_7[ , nontfpGrowthGap := lrgdpnaPerCapita_chg7_nonTFP - predicted_nontfpGrowth]
  
  pwt_7[ , rgdpGrowthGap2 := lrgdpnaPerCapita_chg7 - predicted2_rgdpGrowth]
  pwt_7[ , tfpGrowthGap2 := lrtfpna_chg7 - predicted2_tfpGrowth]
  pwt_7[ , nontfpGrowthGap2 := lrgdpnaPerCapita_chg7_nonTFP - predicted2_nontfpGrowth]
  
  
  ## Calculating average gaps by region :
  # Version 1
  
  LACaverageGaps <- pwt_7[subRegion_new == "Latin America and the Caribbean"][ , lapply(.SD[ , .(rgdpGrowthGap,tfpGrowthGap,nontfpGrowthGap)],mean), by = year]
  
  EATaverageGaps <- pwt_7[subRegion_new == "East Asian Tigers"][ , lapply(.SD[ , .(rgdpGrowthGap,tfpGrowthGap,nontfpGrowthGap)],mean), by = year]
  
  AFRaverageGaps <- pwt_7[subRegion_new == "Africa"][ , lapply(.SD[ , .(rgdpGrowthGap,tfpGrowthGap,nontfpGrowthGap)],mean), by = year]
  
  USAaverageGaps <- pwt_7[subRegion_new == "United States"][ , lapply(.SD[ , .(rgdpGrowthGap,tfpGrowthGap,nontfpGrowthGap)],mean), by = year]
  
  # Check that they're zero as verification that my procedure is correctly implemented
  NONLACaverageGaps <- pwt_7[subRegion_new != "Latin America and the Caribbean"][ , lapply(.SD[ , .(rgdpGrowthGap,tfpGrowthGap,nontfpGrowthGap)],mean), by = year]
  
  
  # Version 2
  
  LACaverageGaps2 <- pwt_7[subRegion_new == "Latin America and the Caribbean"][ , lapply(.SD[ , .(rgdpGrowthGap2,tfpGrowthGap2,nontfpGrowthGap2)],mean), by = year]
  
  NONLACaverageGaps2 <- pwt_7[subRegion_new != "Latin America and the Caribbean"][ , lapply(.SD[ , .(rgdpGrowthGap2,tfpGrowthGap2,nontfpGrowthGap2)],mean), by = year]
  
  EATaverageGaps2 <- pwt_7[subRegion_new == "East Asian Tigers"][ , lapply(.SD[ , .(rgdpGrowthGap2,tfpGrowthGap2,nontfpGrowthGap2)],mean), by = year]
  
  AFRaverageGaps2 <- pwt_7[subRegion_new == "Africa"][ , lapply(.SD[ , .(rgdpGrowthGap2,tfpGrowthGap2,nontfpGrowthGap2)],mean), by = year]
  
  USAaverageGaps2 <- pwt_7[subRegion_new == "United States"][ , lapply(.SD[ , .(rgdpGrowthGap2,tfpGrowthGap2,nontfpGrowthGap2)],mean), by = year]
  
  
  # rename for merging
  
  names(EATaverageGaps) <- c("year","EATrgdpGrowthGap","EATtfpGrowthGap","EATnontfpGrowthGap")
  names(AFRaverageGaps) <- c("year","AFRrgdpGrowthGap","AFRtfpGrowthGap","AFRnontfpGrowthGap")
  names(USAaverageGaps) <- c("year","USArgdpGrowthGap","USAtfpGrowthGap","USAnontfpGrowthGap")
  
  names(LACaverageGaps2) <- c("year","LACrgdpGrowthGap2","LACtfpGrowthGap2","LACnontfpGrowthGap2")
  names(EATaverageGaps2) <- c("year","EATrgdpGrowthGap2","EATtfpGrowthGap2","EATnontfpGrowthGap2")
  names(AFRaverageGaps2) <- c("year","AFRrgdpGrowthGap2","AFRtfpGrowthGap2","AFRnontfpGrowthGap2")
  names(USAaverageGaps2) <- c("year","USArgdpGrowthGap2","USAtfpGrowthGap2","USAnontfpGrowthGap2")
  names(NONLACaverageGaps2) <- c("year","NONLACrgdpGrowthGap2","NONLACtfpGrowthGap2","NONLACnontfpGrowthGap2")
  
  
  ###############################
  ############## Construct tables
  ############## comparing LAC to 
  ############## benchmarks.
  ##############################
  ##############################
  
  ### LAC vs non-LAC
  ###################
  
  #### Version 1
  
  # First step easy in this case -- no differencing, since all non-LAC benchmark
  LACvsnonLAC <- LACaverageGaps
  
  # Compute pre 1990, post 1990 and all obs averages
  pre1990 <- LACvsnonLAC[ year <= 1989 , lapply(.SD[ , .(rgdpGrowthGap,tfpGrowthGap,nontfpGrowthGap)], mean)]
  pre1990[ , year := "Average pre 1990"]
  setcolorder(pre1990,"year")
  
  post1990 <- LACvsnonLAC[ year > 1989 , lapply(.SD[ , .(rgdpGrowthGap,tfpGrowthGap,nontfpGrowthGap)], mean)]
  post1990[ , year := "Average post 1990"]
  setcolorder(post1990,"year")
  
  meanAll <- LACvsnonLAC[ , lapply(.SD[ , .(rgdpGrowthGap,tfpGrowthGap,nontfpGrowthGap)], mean)]
  meanAll[ , year := "Overall average"]
  setcolorder(meanAll,"year")
  
  # Put together into one table
  LACvsnonLAC <- rbindlist(list(LACvsnonLAC[1:8,],pre1990,post1990,meanAll))
  
  # Convert to percentages
  #LACvsnonLAC[ , c("rgdpGrowthGap","tfpGrowthGap","nontfpGrowthGap") := lapply(.SD[ , .(rgdpGrowthGap,tfpGrowthGap,nontfpGrowthGap)],"*",100)]
  
  # Rename columns
  names(LACvsnonLAC) <- c("Period end","Total (adj., robust)","Produc- tivity (adj., robust)","Factors (adj., robust)")
  
  ### Version 2 - pooled
  
  # First step easy in this case -- no differencing, since all non-LAC benchmark
  
  setkey(NONLACaverageGaps2,year)
  setkey(LACaverageGaps2,year)
  LACvsnonLAC2 <- NONLACaverageGaps2[LACaverageGaps2]
  
  LACvsnonLAC2 <-LACvsnonLAC2[ , .(year = year,
                           rgdpGrowthGap2 = LACrgdpGrowthGap2 - NONLACrgdpGrowthGap2,
                           tfpGrowthGap2 = LACtfpGrowthGap2 - NONLACtfpGrowthGap2,
                           nontfpGrowthGap2 = LACnontfpGrowthGap2 - NONLACnontfpGrowthGap2)] 
  
  
  # Compute pre 1990, post 1990 and all obs averages
  pre1990 <- LACvsnonLAC2[ year <= 1989 , lapply(.SD[ , .(rgdpGrowthGap2,tfpGrowthGap2,nontfpGrowthGap2)], mean)]
  pre1990[ , year := "Average pre 1990"]
  setcolorder(pre1990,"year")
  
  post1990 <- LACvsnonLAC2[ year > 1989 , lapply(.SD[ , .(rgdpGrowthGap2,tfpGrowthGap2,nontfpGrowthGap2)], mean)]
  post1990[ , year := "Average post 1990"]
  setcolorder(post1990,"year")
  
  meanAll <- LACvsnonLAC2[ , lapply(.SD[ , .(rgdpGrowthGap2,tfpGrowthGap2,nontfpGrowthGap2)], mean)]
  meanAll[ , year := "Overall average"]
  setcolorder(meanAll,"year")
  
  # Put together into one table
  LACvsnonLAC2 <- rbindlist(list(LACvsnonLAC2[1:8,],pre1990,post1990,meanAll)) 
  
  # Convert to percentages
  #LACvsnonLAC2[ , c("rgdpGrowthGap2","tfpGrowthGap2","nontfpGrowthGap2") := lapply(.SD[ , .(rgdpGrowthGap2,tfpGrowthGap2,nontfpGrowthGap2)],"*",100)]
  
  # Rename columns
  names(LACvsnonLAC2) <- c("Period end","Total (adj.)","Produc- tivity (adj.)","Factors (adj.)")
  
  ## Merge with raw benchmarks and reorganize table
  
  names(LACvsnonLAC_raw) <- c("Period end","Produc- tivity","Factors","Total")
  
  setkey(LACvsnonLAC_raw,"Period end")
  setkey(LACvsnonLAC,"Period end")
  
  LACvsnonLAC <- LACvsnonLAC_raw[LACvsnonLAC]
  
  setkey(LACvsnonLAC2,"Period end")
  
  LACvsnonLAC <- LACvsnonLAC2[LACvsnonLAC]
  
  # Change column order
  setcolorder(LACvsnonLAC,c("Period end","Produc- tivity","Produc- tivity (adj.)","Produc- tivity (adj., robust)",
                            "Factors","Factors (adj.)","Factors (adj., robust)","Total","Total (adj.)","Total (adj., robust)"))
  
  # Change row order
  LACvsnonLAC <- LACvsnonLAC[c(1:8,10,9,11)]
  
  # Write to excel
  write.xlsx(LACvsnonLAC,"figures/tables/excel/LACvsNONLAC_gapControlled_rollingWindows.xlsx")
  
  kable(LACvsnonLAC, "latex", booktabs = T, digits = 2, linesep = "", caption = "LAC per capita output growth gap: non-LAC benchmark") %>% kable_styling(full_width = F, latex_options = c("hold_position")) %>% 
    column_spec(2:10, width = "1.75cm") %>% pack_rows("Averages",nrow(LACvsnonLAC)-2,nrow(LACvsnonLAC)) %>% landscape() %>% cat(., file = "figures/tables/LACvsNONLAC_gapControlled_rollingWindows.tex") 
  
  # Construct xtable and print
  #tableLACvsnonLAC <- xtable(LACvsnonLAC,align = c("r","x{1.5cm}","x{1cm}","x{1cm}","x{1cm}","x{1cm}","x{1cm}","x{1cm}","x{1cm}","x{1cm}","x{1cm}"), caption = c("LAC gaps (vs. non-LAC benchmark), \\% terms"),digits = 2)
  #print(tableLACvsnonLAC,"figures/tables/LACvsNONLAC_gapControlled.tex", type = "latex", size = "\\footnotesize", include.rownames = FALSE, booktabs = TRUE,floating = TRUE)
  
  ## OThers: 
  
  names(LACaverageGaps) <- c("year","LACrgdpGrowthGap","LACtfpGrowthGap","LACnontfpGrowthGap")
  
  ### LAC vs EAT
  ###################
  
  ## Version 1 (non-pooled adjustment)
  
  setkey(LACaverageGaps,year)
  setkey(EATaverageGaps,year) 
  
  LACvsEAT <- EATaverageGaps[LACaverageGaps]
  
  
  
  LACvsEAT <-LACvsEAT[ , .(year = year,
                          rgdpGrowthGap = LACrgdpGrowthGap - EATrgdpGrowthGap,
                          tfpGrowthGap = LACtfpGrowthGap - EATtfpGrowthGap,
                          nontfpGrowthGap = LACnontfpGrowthGap - EATnontfpGrowthGap)] 
  
  # Compute pre 1990, post 1990 and all obs averages
  pre1990 <- LACvsEAT[ year <= 1989 , lapply(.SD[ , .(rgdpGrowthGap,tfpGrowthGap,nontfpGrowthGap)], mean)]
  pre1990[ , year := "Average pre 1990"]
  setcolorder(pre1990,"year")
  
  post1990 <- LACvsEAT[ year > 1989 , lapply(.SD[ , .(rgdpGrowthGap,tfpGrowthGap,nontfpGrowthGap)], mean)]
  post1990[ , year := "Average post 1990"]
  setcolorder(post1990,"year")
  
  
  meanAll <- LACvsEAT[ , lapply(.SD[ , .(rgdpGrowthGap,tfpGrowthGap,nontfpGrowthGap)], mean)]
  meanAll[ , year := "Overall average"]
  setcolorder(meanAll,"year")
  
  # Put together into one table
  LACvsEAT <- rbindlist(list(LACvsEAT[1:8,],pre1990,post1990,meanAll))
  
  # Convert to percentages
  #LACvsEAT[ , c("rgdpGrowthGap","tfpGrowthGap","nontfpGrowthGap") := lapply(.SD[ , .(rgdpGrowthGap,tfpGrowthGap,nontfpGrowthGap)],"*",100)]
  
  # Rename columns
  names(LACvsEAT) <- c("Period end","Total (adj., robust)","Produc- tivity (adj., robust)","Factors (adj., robust)")
  
  
  #### Version 2 (pooled)
  
  setkey(LACaverageGaps2,year)
  setkey(EATaverageGaps2,year) 
  
  LACvsEAT2 <- EATaverageGaps2[LACaverageGaps2]
  
  
  
  LACvsEAT2 <-LACvsEAT2[ , .(year = year,
                           rgdpGrowthGap2 = LACrgdpGrowthGap2 - EATrgdpGrowthGap2,
                           tfpGrowthGap2 = LACtfpGrowthGap2 - EATtfpGrowthGap2,
                           nontfpGrowthGap2 = LACnontfpGrowthGap2 - EATnontfpGrowthGap2)] 
  
  # Compute pre 1990, post 1990 and all obs averages
  pre1990 <- LACvsEAT2[ year <= 1989 , lapply(.SD[ , .(rgdpGrowthGap2,tfpGrowthGap2,nontfpGrowthGap2)], mean)]
  pre1990[ , year := "Average pre 1990"]
  setcolorder(pre1990,"year")
  
  post1990 <- LACvsEAT2[ year > 1989 , lapply(.SD[ , .(rgdpGrowthGap2,tfpGrowthGap2,nontfpGrowthGap2)], mean)]
  post1990[ , year := "Average post 1990"]
  setcolorder(post1990,"year")
  
  
  meanAll <- LACvsEAT2[ , lapply(.SD[ , .(rgdpGrowthGap2,tfpGrowthGap2,nontfpGrowthGap2)], mean)]
  meanAll[ , year := "Overall average"]
  setcolorder(meanAll,"year")
  
  # Put together into one table
  LACvsEAT2 <- rbindlist(list(LACvsEAT2[1:8,],pre1990,post1990,meanAll))
  
  # Convert to percentages
  #LACvsEAT2[ , c("rgdpGrowthGap2","tfpGrowthGap2","nontfpGrowthGap2") := lapply(.SD[ , .(rgdpGrowthGap2,tfpGrowthGap2,nontfpGrowthGap2)],"*",100)]
  
  # Rename columns
  names(LACvsEAT2) <- c("Period end","Total (adj.)","Produc- tivity (adj.)","Factors (adj.)")
  
  ## Merge with raw benchmarks and reorganize table
    
  names(LACvsEAT_raw) <- c("Period end","Produc- tivity","Factors","Total")
  
  setkey(LACvsEAT_raw,"Period end")
  setkey(LACvsEAT,"Period end")
  
  LACvsEAT <- LACvsEAT_raw[LACvsEAT]
  
  setkey(LACvsEAT2,"Period end")
  LACvsEAT <- LACvsEAT2[LACvsEAT]
    
  # Change column order
  setcolorder(LACvsEAT,c("Period end","Produc- tivity","Produc- tivity (adj.)","Produc- tivity (adj., robust)",
                         "Factors","Factors (adj.)","Factors (adj., robust)","Total","Total (adj.)","Total (adj., robust)"))
  
  # Change row order
  LACvsEAT <- LACvsEAT[c(1:8,10,9,11)]
  
  # Write to excel
  write.xlsx(LACvsEAT,"figures/tables/excel/LACvsEAT_gapControlled_rollingWindows.xlsx")
  
  kable(LACvsEAT, "latex", booktabs = T, digits = 2, linesep = "", caption = "LAC per capita output growth gap: EAT benchmark") %>% kable_styling(full_width = F, latex_options = c("hold_position")) %>% 
    column_spec(2:10, width = "1.75cm") %>% pack_rows("Averages",nrow(LACvsEAT)-2,nrow(LACvsEAT)) %>% landscape() %>% cat(., file = "figures/tables/LACvsEAT_gapControlled_rollingWindows.tex") 
  
  # Construct xtable and print
  #tableLACvsEAT <- xtable(LACvsEAT,align = c("r","x{1.5cm}","x{1cm}","x{1cm}","x{1cm}","x{1cm}","x{1cm}","x{1cm}","x{1cm}","x{1cm}","x{1cm}"), caption = c("LAC gaps (vs. EAT benchmark), \\% terms"),digits = 2)
  #print(tableLACvsEAT,"figures/tables/LACvsEAT_gapControlled.tex", size = "\\footnotesize", type = "latex", include.rownames = FALSE, booktabs = TRUE, floating = TRUE)
  
  
  
  ### LAC vs AFR
  ###################
  
  ## Version 1
  
  setkey(LACaverageGaps,year)
  setkey(AFRaverageGaps,year)
  
  LACvsAFR <- AFRaverageGaps[LACaverageGaps]
  
  LACvsAFR <-LACvsAFR[ , .(year = year,
                           rgdpGrowthGap = LACrgdpGrowthGap - AFRrgdpGrowthGap,
                           tfpGrowthGap = LACtfpGrowthGap - AFRtfpGrowthGap,
                           nontfpGrowthGap = LACnontfpGrowthGap - AFRnontfpGrowthGap)] 
  
  # Compute pre 1990, post 1990 and all obs averages
  pre1990 <- LACvsAFR[ year <= 1989 , lapply(.SD[ , .(rgdpGrowthGap,tfpGrowthGap,nontfpGrowthGap)], mean)]
  pre1990[ , year := "Average pre 1990"]
  setcolorder(pre1990,"year")
  
  post1990 <- LACvsAFR[ year > 1989 , lapply(.SD[ , .(rgdpGrowthGap,tfpGrowthGap,nontfpGrowthGap)], mean)]
  post1990[ , year := "Average post 1990"]
  setcolorder(post1990,"year")
  
  meanAll <- LACvsAFR[ , lapply(.SD[ , .(rgdpGrowthGap,tfpGrowthGap,nontfpGrowthGap)], mean)]
  meanAll[ , year := "Overall average"]
  setcolorder(meanAll,"year")
  
  # Put together into one table
  LACvsAFR <- rbindlist(list(LACvsAFR[1:8,],pre1990,post1990,meanAll))
  
  # Convert to percentages
  #LACvsAFR[ , c("rgdpGrowthGap","tfpGrowthGap","nontfpGrowthGap") := lapply(.SD[ , .(rgdpGrowthGap,tfpGrowthGap,nontfpGrowthGap)],"*",100)]
  
  # Rename columns
  names(LACvsAFR) <- c("Period end","Total (adj., robust)","Produc- tivity (adj., robust)","Factors (adj., robust)")
  
  
  
  ### Version 2 (pooled)
  
  setkey(LACaverageGaps2,year)
  setkey(AFRaverageGaps2,year)
  
  LACvsAFR2 <- AFRaverageGaps2[LACaverageGaps2]
  
  LACvsAFR2 <-LACvsAFR2[ , .(year = year,
                           rgdpGrowthGap2 = LACrgdpGrowthGap2 - AFRrgdpGrowthGap2,
                           tfpGrowthGap2 = LACtfpGrowthGap2 - AFRtfpGrowthGap2,
                           nontfpGrowthGap2 = LACnontfpGrowthGap2 - AFRnontfpGrowthGap2)] 
  
  # Compute pre 1990, post 1990 and all obs averages
  pre1990 <- LACvsAFR2[ year <= 1989 , lapply(.SD[ , .(rgdpGrowthGap2,tfpGrowthGap2,nontfpGrowthGap2)], mean)]
  pre1990[ , year := "Average pre 1990"]
  setcolorder(pre1990,"year")
  
  post1990 <- LACvsAFR2[ year > 1989 , lapply(.SD[ , .(rgdpGrowthGap2,tfpGrowthGap2,nontfpGrowthGap2)], mean)]
  post1990[ , year := "Average post 1990"]
  setcolorder(post1990,"year")
  
  meanAll <- LACvsAFR2[ , lapply(.SD[ , .(rgdpGrowthGap2,tfpGrowthGap2,nontfpGrowthGap2)], mean)]
  meanAll[ , year := "Overall average"]
  setcolorder(meanAll,"year")
  
  # Put together into one table
  LACvsAFR2 <- rbindlist(list(LACvsAFR2[1:8,],pre1990,post1990,meanAll))
  
  # Convert to percentages
  #LACvsAFR2[ , c("rgdpGrowthGap2","tfpGrowthGap2","nontfpGrowthGap2") := lapply(.SD[ , .(rgdpGrowthGap2,tfpGrowthGap2,nontfpGrowthGap2)],"*",100)]
  
  # Rename columns
  names(LACvsAFR2) <- c("Period end","Total (adj.)","Produc- tivity (adj.)","Factors (adj.)")
  
    
  ## Merge with raw benchmarks and reorganize table
  
  names(LACvsAFR_raw) <- c("Period end","Produc- tivity","Factors","Total")
  
  setkey(LACvsAFR_raw,"Period end")
  setkey(LACvsAFR,"Period end")
  
  LACvsAFR <- LACvsAFR_raw[LACvsAFR]
  
  setkey(LACvsAFR2,"Period end")
  
  LACvsAFR <- LACvsAFR2[LACvsAFR]
  
  # Change column order
  setcolorder(LACvsAFR,c("Period end","Produc- tivity","Produc- tivity (adj.)","Produc- tivity (adj., robust)",
                         "Factors","Factors (adj.)","Factors (adj., robust)","Total","Total (adj.)","Total (adj., robust)"))
  # Change row order
  LACvsAFR <- LACvsAFR[c(1:8,10,9,11)]
  
  # Write to excel
  write.xlsx(LACvsAFR,"figures/tables/excel/LACvsAFR_gapControlled_rollingWindows.xlsx")
  
  kable(LACvsAFR, "latex", booktabs = T, digits = 2, linesep = "", caption = "LAC per capita output growth gap: AFR benchmark") %>% kable_styling(full_width = F, latex_options = c("hold_position")) %>% 
    column_spec(2:10, width = "1.75cm") %>% pack_rows("Averages",nrow(LACvsAFR)-2,nrow(LACvsAFR)) %>% landscape() %>% cat(., file = "figures/tables/LACvsAFR_gapControlled_rollingWindows.tex") 
  
  
  # Construct xtable and print
  #tableLACvsAFR <- xtable(LACvsAFR,align = c("r","x{1.5cm}","x{1cm}","x{1cm}","x{1cm}","x{1cm}","x{1cm}","x{1cm}","x{1cm}","x{1cm}","x{1cm}"), caption = c("LAC gaps (vs. AFR benchmark), \\% terms"),digits = 2)
  #print(tableLACvsAFR,"figures/tables/LACvsAFR_gapControlled.tex", size = "\\footnotesize", type = "latex", include.rownames = FALSE, booktabs = TRUE, floating = TRUE)
  
  
  
  
  ### LAC vs USA
  ###################
  
  ##### Version 1 (non-pooled adjustment)
  
  setkey(LACaverageGaps,year)
  setkey(USAaverageGaps,year)
  
  LACvsUSA <- USAaverageGaps[LACaverageGaps]
  
  LACvsUSA <-LACvsUSA[ , .(year = year,
                           rgdpGrowthGap = LACrgdpGrowthGap - USArgdpGrowthGap,
                           tfpGrowthGap = LACtfpGrowthGap - USAtfpGrowthGap,
                           nontfpGrowthGap = LACnontfpGrowthGap - USAnontfpGrowthGap)] 
  
  # Compute pre 1990, post 1990 and all obs averages
  pre1990 <- LACvsUSA[ year <= 1989 , lapply(.SD[ , .(rgdpGrowthGap,tfpGrowthGap,nontfpGrowthGap)], mean)]
  pre1990[ , year := "Average pre 1990"]
  setcolorder(pre1990,"year")
  
  post1990 <- LACvsUSA[ year > 1989 , lapply(.SD[ , .(rgdpGrowthGap,tfpGrowthGap,nontfpGrowthGap)], mean)]
  post1990[ , year := "Average post 1990"]
  setcolorder(post1990,"year")
  
  meanAll <- LACvsUSA[ , lapply(.SD[ , .(rgdpGrowthGap,tfpGrowthGap,nontfpGrowthGap)], mean)]
  meanAll[ , year := "Overall average"]
  setcolorder(meanAll,"year")
  
  # Put together into one table
  LACvsUSA <- rbindlist(list(LACvsUSA[1:8,],pre1990,post1990,meanAll))
  
  # Convert to percentages
  #LACvsUSA[ , c("rgdpGrowthGap","tfpGrowthGap","nontfpGrowthGap") := lapply(.SD[ , .(rgdpGrowthGap,tfpGrowthGap,nontfpGrowthGap)],"*",100)]
  
  # Rename columns
  names(LACvsUSA) <- c("Period end","Total (adj., robust)","Produc- tivity (adj., robust)","Factors (adj., robust)")
  
  
  
  #### Version 2 (pooled)
  
  setkey(LACaverageGaps2,year)
  setkey(USAaverageGaps2,year)
  
  LACvsUSA2 <- USAaverageGaps2[LACaverageGaps2]
  
  LACvsUSA2 <-LACvsUSA2[ , .(year = year,
                           rgdpGrowthGap2 = LACrgdpGrowthGap2 - USArgdpGrowthGap2,
                           tfpGrowthGap2 = LACtfpGrowthGap2 - USAtfpGrowthGap2,
                           nontfpGrowthGap2 = LACnontfpGrowthGap2 - USAnontfpGrowthGap2)] 
  
  # Compute pre 1990, post 1990 and all obs averages
  pre1990 <- LACvsUSA2[ year <= 1989 , lapply(.SD[ , .(rgdpGrowthGap2,tfpGrowthGap2,nontfpGrowthGap2)], mean)]
  pre1990[ , year := "Average pre 1990"]
  setcolorder(pre1990,"year")
  
  post1990 <- LACvsUSA2[ year > 1989 , lapply(.SD[ , .(rgdpGrowthGap2,tfpGrowthGap2,nontfpGrowthGap2)], mean)]
  post1990[ , year := "Average post 1990"]
  setcolorder(post1990,"year")
  
  meanAll <- LACvsUSA2[ , lapply(.SD[ , .(rgdpGrowthGap2,tfpGrowthGap2,nontfpGrowthGap2)], mean)]
  meanAll[ , year := "Overall average"]
  setcolorder(meanAll,"year")
  
  # Put together into one table
  LACvsUSA2 <- rbindlist(list(LACvsUSA2[1:8,],pre1990,post1990,meanAll))
  
  # Convert to percentages
  #LACvsUSA2[ , c("rgdpGrowthGap2","tfpGrowthGap2","nontfpGrowthGap2") := lapply(.SD[ , .(rgdpGrowthGap2,tfpGrowthGap2,nontfpGrowthGap2)],"*",100)]
  
  # Rename columns
  names(LACvsUSA2) <- c("Period end","Total (adj.)","Produc- tivity (adj.)","Factors (adj.)")
  
  
  ## Merge with raw benchmarks and reorganize table
  
  names(LACvsUSA_raw) <- c("Period end","Produc- tivity","Factors","Total")
  
  setkey(LACvsUSA_raw,"Period end")
  setkey(LACvsUSA,"Period end")
  
  LACvsUSA <- LACvsUSA_raw[LACvsUSA]
  
  setkey(LACvsUSA2,"Period end")
  
  LACvsUSA <- LACvsUSA2[LACvsUSA]
  
  # Change column order
  setcolorder(LACvsUSA,c("Period end","Produc- tivity","Produc- tivity (adj.)","Produc- tivity (adj., robust)",
                         "Factors","Factors (adj.)","Factors (adj., robust)","Total","Total (adj.)","Total (adj., robust)"))
  
  # Change row order
  LACvsUSA <- LACvsUSA[c(1:8,10,9,11)]
  
  # Write to excel
  write.xlsx(LACvsUSA,"figures/tables/excel/LACvsUSA_gapControlled_rollingWindows.xlsx")
  
  kable(LACvsUSA, "latex", booktabs = T, digits = 2, linesep = "", caption = "LAC per capita output growth gap: USA benchmark") %>% kable_styling(full_width = F, latex_options = c("hold_position")) %>% 
    column_spec(2:10, width = "1.75cm") %>% pack_rows("Averages",nrow(LACvsUSA)-2,nrow(LACvsUSA)) %>% landscape() %>% cat(., file = "figures/tables/LACvsUSA_gapControlled_rollingWindows.tex") 
  
  # Construct xtable and print
  #tableLACvsUSA <- xtable(LACvsUSA,align = c("r","x{1.5cm}","x{1cm}","x{1cm}","x{1cm}","x{1cm}","x{1cm}","x{1cm}","x{1cm}","x{1cm}","x{1cm}"), caption = c("LAC gaps (vs. USA benchmark), \\% terms"),digits = 2)
  #print(tableLACvsUSA,"figures/tables/LACvsUSA_gapControlled.tex", size = "\\footnotesize", type = "latex", include.rownames = FALSE, booktabs = TRUE, floating = TRUE)
  
  ### Construct plots for countries, predicted vs actual
  
  pwt_7_temp <- pwt_7[ , .(pseudoYear,alpha3,lrgdpnaPerCapita_chg7,lrtfpna_chg7,lrgdpnaPerCapita_chg7_nonTFP,
                           predicted_rgdpGrowth,predicted_tfpGrowth,predicted_nontfpGrowth,
                           predicted2_rgdpGrowth,predicted2_tfpGrowth,predicted2_nontfpGrowth)]
  
  pwt_temp <- pwt[ , .(pseudoYear,year,alpha3,subRegion_new)]
  
  setkey(pwt_7_temp,pseudoYear,alpha3)
  setkey(pwt_temp,pseudoYear,alpha3)
  
  pwt_temp <- pwt_7_temp[pwt_temp]
  
  ## TFP
  
  ggplot(pwt_temp[subRegion_new == "Latin America and the Caribbean"],aes(year)) +
    geom_line(aes(y = lrtfpna_chg7, color = "lrtfpna_chg7")) + 
    geom_line(aes(y = predicted_tfpGrowth, color = "predicted_tfpGrowth")) +
    #geom_line(aes(y = predicted2_tfpGrowth, color = "predicted2_tfpGrowth")) + 
    scale_color_discrete( labels = c("Actual","Predicted")) + 
    labs(color = "Legend") +
    ggtitle("TFP growth: actual vs predicted (LAC countries, 1962-2017)") +
    xlab("Year") +
    ylab("Growth rate") +
    facet_wrap(~ alpha3, ncol = 4) 
  
  ggsave("figures/rtfpnaGrowth_LAC_ActualVsPredicted_rollingWindows.pdf", plot = last_plot())
  
  # Nontfp
  
  ggplot(pwt_temp[subRegion_new == "Latin America and the Caribbean"],aes(year)) +
    geom_line(aes(y = lrgdpnaPerCapita_chg7_nonTFP, color = "lrgdpnaPerCapita_chg7_nonTFP")) + 
    geom_line(aes(y = predicted_nontfpGrowth, color = "predicted_nontfpGrowth")) +
    #geom_line(aes(y = predicted2_nontfpGrowth, color = "predicted2_nontfpGrowth")) +
    scale_color_discrete( labels = c("Actual","Predicted")) + 
    labs(color = "Legend") +
    ggtitle("non-TFP growth: actual vs predicted (LAC countries, 1962-2017)") +
    xlab("Year") +
    ylab("Growth rate") +
    facet_wrap(~ alpha3, ncol = 4) 
  
  ggsave("figures/nonrtfpnaGrowth_LAC_ActualVsPredicted_rollingWindows.pdf", plot = last_plot())
  
  
  # Total
  
  ggplot(pwt_temp[subRegion_new == "Latin America and the Caribbean"],aes(year)) +
    geom_line(aes(y = lrgdpnaPerCapita_chg7, color = "lrgdpnaPerCapita_chg7")) + 
    geom_line(aes(y = predicted_rgdpGrowth, color = "predicted_rgdpGrowth")) +
    #geom_line(aes(y = predicted2_rgdpGrowth, color = "predicted2_rgdpGrowth")) +
    scale_color_discrete( labels = c("Actual","Predicted")) + 
    labs(color = "Legend") +
    ggtitle("RGDP per capita growth: actual vs predicted (LAC countries, 1962-2017)") +
    xlab("Year") +
    ylab("Growth rate") +
    facet_wrap(~ alpha3, ncol = 4) 
  
  ggsave("figures/rgdpnaGrowth_LAC_ActualVsPredicted_rollingWindows.pdf", plot = last_plot())
  
  
  
    
#--------------------------------------------#
# Construct scatter plot of pre and post 1990 growth shortfall
# across countries
#--------------------------------------------#
  
data <- fread("figures/tables/sec2_mainReg_LACCountryDummies_PrePost_clean_rollingWindows.csv")

# Clean up

data <- data[ V1 != ""]

data[ , country := substr(V1,1,3)]
data[ , post1990 := substr(V1,nchar(V1),nchar(V1))]
data <- head(data,-3)

data <- data[ , .(country,post1990,V2)]

data[ , V2 := as.double(V2)]

data <- dcast(data, country ~ post1990, value.var = "V2")

setnames(data,"0","Pre1990")
setnames(data,"1","Post1990")

  
ggplot(data, aes(x = Pre1990, y = Post1990)) + 
  #geom_point(size = 5) + 
  geom_text(aes(label = country), size = 3) + 
  labs(title = "LAC productivity growth gap") +
  labs(subtitle = "Pre- and Post-1990") +
  labs( x= "Pre-1990 productivity growth gap", y = "Post-1990 productivity growth gap") + 
  geom_abline(slope = 1, linetype = 2) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) + 
  #xlim(-0.03,0.03) + 
  #ylim(-.03, 0.03) + 
  labs(color = "Legend") +
  coord_fixed()
  ##theme_bw()


ggsave("figures/lacCountriesPrePost1990Gaps_rollingWindows.pdf",  width = 9, height = 6.5, units = "in")


## Calculate growth compared to USA for all LAC countries

temp <- pwt[ subRegion_new %in% c("Latin America and the Caribbean","United States") & year >= 1989][order(alpha3,year)]
cumtfp <- temp[ , .(year = year, cumtfp = cumsum(lrtfpna_chg1)), by = .(alpha3)]

cumtfp[ alpha3 %in% c("ARG","BOL","BRA","BRB"), subPlot := 1]
cumtfp[ alpha3 %in% c("CHL","COL","CRI","DOM"), subPlot := 2]
cumtfp[ alpha3 %in% c("ECU","GTM","JAM","MEX"), subPlot := 3]
cumtfp[ alpha3 %in% c("PER","TTO","URY","VEN"), subPlot := 4]

cumtfpCountries <- cumtfp[!is.na(subPlot)]
cumtfpUSA <- cumtfp[is.na(subPlot)]

cumtfpUSA[ , subPlot := 1]
cumtfpUSA_2 <- copy(cumtfpUSA)
cumtfpUSA_3 <- copy(cumtfpUSA)
cumtfpUSA_4 <- copy(cumtfpUSA)

cumtfpUSA_2[ , subPlot := 2]
cumtfpUSA_3[ , subPlot := 3]
cumtfpUSA_4[ , subPlot := 4]

cumtfpUSA <- rbind(cumtfpUSA,cumtfpUSA_2)
cumtfpUSA <- rbind(cumtfpUSA,cumtfpUSA_3)
cumtfpUSA <- rbind(cumtfpUSA,cumtfpUSA_4)

dataSplit <- split(cumtfpCountries, f = cumtfpCountries$subPlot)

p1 <- ggplot(dataSplit$"1", aes(x = year, y = cumtfp, color = alpha3)) + 
  geom_line() + 
  geom_line(aes(y = cumtfp, linetype = "USA"), data = cumtfpUSA)

p2 <- p1 %+% dataSplit$"2"
p3 <- p1 %+% dataSplit$"3"
p4 <- p1 %+% dataSplit$"4"

grid.arrange(p1,p2,p3,p4)

cumtfpLeaders <- cumtfp[ alpha3 %in% c("ARG","PER","URY","CRI","DOM","USA")]

setkey(cumtfpLeaders,alpha3,year)
indx <- cumtfpLeaders[ , .I[1L] , by = alpha3]$V1
cumtfpLeaders[ indx, cumtfp := 0]

ggplot(cumtfpLeaders, aes(x = year, y = cumtfp, group = alpha3)) +
  geom_line(aes(y = cumtfp), size = 0.5) + 
  geom_point(aes(y = cumtfp, shape = alpha3), size = 2) +
  labs(title = "Cumulative productivity growth since 1990 in LAC countries with positive gaps", subtitle = "1989 = 0, Annual, %") + 
  theme(legend.position = "bottom") +
  labs(x = "Year", y = "Cumulative productivity growth") + 
  scale_x_continuous(breaks = c(round(seq(min(cumtfpLeaders$year)+1, max(cumtfpLeaders$year), by = 7),1))) +
  #scale_linetype_discrete( name = "Legend") + 
  scale_shape_discrete("Legend")

ggsave("figures/LAC_post1990leaders_compared_to_USA_rollingWindows.pdf",plot = last_plot(), width = 9, height = 6.5, units = "in")



#--------------------------------------------#
# Make table of LAC country dummies
#--------------------------------------------#

dummies <- fread("figures/tables/sec2_mainReg_LACCountryDummies_rollingWindows.csv")


#dummies <- dummies[ , .(V1,V4,V3)]

dummies <- dummies[3:18]

names(dummies) <- c("alpha3","Productivity","Factors","Total")  

dummies[ , Productivity := as.double(Productivity)]
dummies[ , Factors := as.double(Factors)]
dummies[ , Total := as.double(Total)]

dummiesCopy <- copy(dummies)
setkey(dummiesCopy,alpha3)

dummies <- melt(dummies, id.vars = "alpha3", measure.vars = c("Productivity","Factors"))
setkey(dummies,alpha3)



dummies <- dummiesCopy[dummies]


ggthemr("flat", spacing = 0.9, layout = "clear")

ggplot( dummies, aes(x = reorder(alpha3, Total))) + 
  geom_bar(stat = "identity", aes(y = value, fill = variable)) +
  geom_col(position = position_stack(reverse = FALSE), aes(y = value, fill = variable)) + 
  #geom_line(aes(x = alpha3, y = Total, group = 1), size = 4, linetype = "dashed", color = "Black") + 
  geom_point(aes(x = alpha3, y = Total, shape = "Total"), size = 2, color = "black")  +
  xlab("\nCountry") + 
  ylab("Average shortfall\n") +  
  labs(title = "Decomposition of country per capita output growth shortfalls")+
  labs(subtitle = "Annualized, %") + 
  scale_fill_discrete(name = "", labels = c("Productivity","Factors")) +
  scale_shape_discrete(name = "", labels = "Total") +
  #scale_linetype_discrete( name = "", labels = "Sum") + 
  coord_flip() +
  #scale_y_reverse() +
  theme(legend.position = "bottom")
        #theme_bw()

ggsave("figures/LAC_Country_Dummmies_Decomposition_rollingWindows.pdf",plot = last_plot(), width = 9, height = 6.5, units = "in")
      

