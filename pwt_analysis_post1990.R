
library(data.table)
library(readstata13)

rm(list = ls())
setwd("~/nfernand@princeton.edu/PhD - Thesis/Research/Inequality and productivity growth in LAC")

pwt <- data.table(read.dta13("data/raw/pwt91.dta"))

pwt <- pwt[ , .(countrycode,country,year,cgdpo,ctfp,rtfpna,rgdpna,rgdpo,pop)]

pwt <- pwt[order(countrycode,year)]

##### Select time frame
pwt <- pwt[ year >= 1960] 

##### Select sample of countries
pwt[ , numComplete := sum(complete.cases(cgdpo,rgdpna,rtfpna,pop)), by = "countrycode"]
pwt[ , numObs := .N, by = "countrycode"]
pwt[ numComplete == numObs, noIncomplete := 1]
temp <- unique(pwt, by = "countrycode")[order(noIncomplete)]
noIncompletes <- temp[ noIncomplete == 1]
#pwt <- pwt[numComplete >= 24]
pwt[numComplete >= 24, include1990 := 1]


temp <- pwt[ , sum(complete.cases(rgdpna,rtfpna,pop,cgdpo)), by = "countrycode"]
###### Variable construction

# Construct per capita variables
pwt[ , rgdpnaPerCapita := rgdpna / pop]
pwt[ , rgdpoPerCapita := rgdpo / pop]
pwt[ , cgdpoPerCapita := cgdpo / pop]

# Constuct log transformation of 
pwt[ , lrtfpna := log(rtfpna)]
pwt[ , lctfp := log(ctfp)]
pwt[ , lrgdpnaPerCapita := log(rgdpnaPerCapita)]
pwt[ , lrgdpoPerCapita := log(rgdpoPerCapita)]
pwt[ , lcgdpoPerCapita := log(cgdpoPerCapita)]

## Extrapolation

# Construct log changes

pwt[ , lrtfpna_chg1 := lrtfpna - shift(lrtfpna, n = 1L, type = "lag"), by = .(countrycode)]
pwt[ , lrgdpnaPerCapita_chg1 := lrgdpnaPerCapita - shift(lrgdpnaPerCapita, n = 1L, type = "lag"), by = .(countrycode)]

pwt <- pwt[year >= 1988 & include1990 == 1]

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

fwrite(pwt[ isExtrapolated == 1, .(country, countrycode, year)], "data/extrapolatedCountryYears_1990.csv")

pwt[is.na(lrtfpna), lrtfpna := lrtfpna_extrap]

# Final diagnostic to check that nothing is missing

pwt[ , numComplete := sum(complete.cases(lrtfpna,lrgdpnaPerCapita,lcgdpoPerCapita)), by = "countrycode"]
pwt[ , numObs := .N, by = "countrycode"]
pwt[ numComplete == numObs, noIncomplete := 1]
pwt[ noIncomplete != 1] 

pwt <- pwt[ , .(country,countrycode,isExtrapolated,year,lctfp,lrtfpna,lrgdpnaPerCapita,lrgdpoPerCapita,lcgdpoPerCapita)][order(country,year)]

fwrite(pwt,"data/pwt91_cleaned_1990.csv")

##### Next stage: analyze data

rm(list = ls())

pwt <- fread("data/pwt91_cleaned_1990.csv")

# Construct 7-year changes

pwt[ , lrgdpnaPerCapita_chg7 := (lrgdpnaPerCapita - shift(lrgdpnaPerCapita, n = 7L, type = "lag"))/7, by = "countrycode"]
pwt[ , lrgdpoPerCapita_chg7 := (lrgdpoPerCapita - shift(lrgdpoPerCapita, n = 7L, type = "lag"))/7, by = "countrycode"]
pwt[ , lrtfpna_chg7 := (lrtfpna - shift(lrtfpna, n = 7L, type = "lag"))/7, by = "countrycode"]

pwt[ , lrgdpnaPerCapita_chg1 := (lrgdpnaPerCapita - shift(lrgdpnaPerCapita, n = 1L, type = "lag")), by = "countrycode"]
pwt[ , lrgdpoPerCapita_chg1 := (lrgdpoPerCapita - shift(lrgdpoPerCapita, n = 1L, type = "lag")), by = "countrycode"]
pwt[ , lrtfpna_chg1 := (lrtfpna - shift(lrtfpna, n = 1L, type = "lag")), by = "countrycode"]

# Construct cgdpoPerCapita relative to US (7-year averages, same as before)

lcgdpoPerCapitaUSA <- pwt[countrycode == "USA"][ , .(year,lcgdpoPerCapita)]

setkey(lcgdpoPerCapitaUSA,year)
setnames(lcgdpoPerCapitaUSA,"lcgdpoPerCapita","lcgdpoPerCapitaUSA")

setkey(pwt,year)

pwt <- lcgdpoPerCapitaUSA[pwt]

pwt[ , lcgdpoPerCapita_gap := lcgdpoPerCapitaUSA - lcgdpoPerCapita]

setkey(pwt,countrycode,year)

pwt[ , lcgdpoPerCapita_gap_avg7 := Reduce(`+`,shift(lcgdpoPerCapita_gap,n= 0L:6L,type = "lag"))/7, by = "countrycode"]
### Classify into regions

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

### Decompose RGDPNA per capita growth into TFP growth and other

pwt[ , lrgdpnaPerCapita_chg7_nonTFP := lrgdpnaPerCapita_chg7 - lrtfpna_chg7]
pwt[ , lrgdpnaPerCapita_chg1_nonTFP := lrgdpnaPerCapita_chg1 - lrtfpna_chg1]

## Make some quick plots for diagnostics

library(ggplot2)

ggplot(pwt[alpha3 == "USA" ], aes(year)) + 
  geom_line(aes(y = lrtfpna_chg7, color = "lrtfpna_chg7")) + 
  geom_line(aes(y = lrgdpnaPerCapita_chg7_nonTFP, color = "lrgdpnaPerCapita_chg7_nonTFP"))

#### Make some plots


#### Make plot 1: Typical LAC country (mean, median) RGDPNA growth during period
#### (plotting both annual change and 7-year changes)

# First, construct "pseudo-year" variable: equal to end year when in given long-period
pwt[ year >= 1990, pseudoYear := 1996]
pwt[ year >= 1997, pseudoYear := 2003]
pwt[ year >= 2004, pseudoYear := 2010]
pwt[ year >= 2011, pseudoYear := 2017]

# Construct dataset of just these years
pwt_7_temp <- pwt[ , .(alpha3,year,lrgdpnaPerCapita_chg7)][ year == 2017 | year == 2010 | year == 2003 | year == 1996 ]

setnames(pwt_7_temp,"year","pseudoYear")

setkey(pwt_7_temp,alpha3,pseudoYear)
setnames(pwt_7_temp,"lrgdpnaPerCapita_chg7","lrgdpnaPerCapita_chg7_temp")
setkey(pwt,alpha3,pseudoYear)

pwt <- pwt_7_temp[pwt]

LAC_growth_mean <- pwt[ , .(alpha3,country,subRegion_new,year,lrgdpnaPerCapita_chg7_temp,lrgdpnaPerCapita_chg1)][ subRegion_new == "Latin America and the Caribbean" & year >= 1990][ , lapply(.SD[ , .(lrgdpnaPerCapita_chg7_temp,lrgdpnaPerCapita_chg1)],mean), by = .(year)]
LAC_growth_median <- pwt[ , .(alpha3,country,subRegion_new,year,lrgdpnaPerCapita_chg7_temp,lrgdpnaPerCapita_chg1)][ subRegion_new == "Latin America and the Caribbean" & year >= 1990][ , lapply(.SD[ , .(lrgdpnaPerCapita_chg7_temp,lrgdpnaPerCapita_chg1)],median), by = .(year)]
LAC_growth_q1 <- pwt[ , .(alpha3,country,subRegion_new,year,lrgdpnaPerCapita_chg7_temp,lrgdpnaPerCapita_chg1)][ subRegion_new == "Latin America and the Caribbean" & year >= 1990][ , lapply(.SD[ , .(lrgdpnaPerCapita_chg7_temp,lrgdpnaPerCapita_chg1)],quantile,0.25), by = .(year)]
LAC_growth_q3 <- pwt[ , .(alpha3,country,subRegion_new,year,lrgdpnaPerCapita_chg7_temp,lrgdpnaPerCapita_chg1)][ subRegion_new == "Latin America and the Caribbean" & year >= 1990][ , lapply(.SD[ , .(lrgdpnaPerCapita_chg7_temp,lrgdpnaPerCapita_chg1)],quantile,0.75), by = .(year)]


LAC_onlyCarib_growth_mean <- pwt[alpha3 == "DOM" | alpha3 == "TTO" | alpha3 == "BRB" | alpha3 == "JAM"][ , .(alpha3,country,year,lrgdpnaPerCapita_chg7_temp,lrgdpnaPerCapita_chg1)][ year >= 1990][ , lapply(.SD[ , .(lrgdpnaPerCapita_chg7_temp,lrgdpnaPerCapita_chg1)],mean), by = .(year)]

ggplot(LAC_growth_mean, aes(year)) + 
  geom_line(aes(y = lrgdpnaPerCapita_chg1, color = "lrgdpnaPerCapita_chg1")) + 
  geom_line(aes(y = lrgdpnaPerCapita_chg7_temp, color = "lrgdpnaPerCapita_chg7_temp")) + 
  #geom_bar(aes(y = lrgdpnaPerCapita_chg7_temp), stat = "identity") + 
  ggtitle("Mean RGDP / growth rates in LAC countries (1990-2017)") +
  xlab("Year") + 
  ylab("Per-capita RGDP growth") + 
  scale_color_hue( labels = c("Annual RGDP growth","7-year average")) + 
  labs(color = "Legend") +
  theme_bw()

ggsave("figures/rgdpGrowth_LACmean_1990.png",plot = last_plot())

ggplot(LAC_growth_median, aes(year)) + 
  geom_line(aes(y = lrgdpnaPerCapita_chg1, color = "lrgdpnaPerCapita_chg1")) + 
  geom_line(aes(y = lrgdpnaPerCapita_chg7_temp, color = "lrgdpnaPerCapita_chg7_temp")) + 
  ggtitle("Median RGDP / growth rates in LAC countries (1990-2017)") +
  xlab("Year") + 
  ylab("Per-capita RGDP growth") + 
  scale_color_hue( labels = c("Annual RGDP growth","7-year average")) + 
  labs(color = "Legend") +
  theme_bw()

ggsave("figures/rgdpGrowth_LACmedian_1990.png",plot = last_plot())

ggplot(LAC_growth_q1, aes(year)) + 
  geom_line(aes(y = lrgdpnaPerCapita_chg1, color = "lrgdpnaPerCapita_chg1")) + 
  geom_line(aes(y = lrgdpnaPerCapita_chg7_temp, color = "lrgdpnaPerCapita_chg7_temp")) + 
  ggtitle("25th percentile RGDP / growth rates in LAC countries (1990-2017)") +
  xlab("Year") + 
  ylab("Per-capita RGDP growth") + 
  scale_color_hue( labels = c("Annual RGDP growth","7-year average")) + 
  labs(color = "Legend") +
  theme_bw()

ggsave("figures/rgdpGrowth_LACq1_1990.png",plot = last_plot())

ggplot(LAC_growth_q3, aes(year)) + 
  geom_line(aes(y = lrgdpnaPerCapita_chg1, color = "lrgdpnaPerCapita_chg1")) + 
  geom_line(aes(y = lrgdpnaPerCapita_chg7_temp, color = "lrgdpnaPerCapita_chg7_temp")) + 
  ggtitle("75th percentile RGDP / growth rates in LAC countries (1990-2017)") +
  xlab("Year") + 
  ylab("Per-capita RGDP growth") + 
  scale_color_hue( labels = c("Annual RGDP growth","7-year average")) + 
  labs(color = "Legend") +
  theme_bw()

ggsave("figures/rgdpGrowth_LACq3_1990.png",plot = last_plot())

ggplot(LAC_onlyCarib_growth_mean, aes(year)) + 
  geom_line(aes(y = lrgdpnaPerCapita_chg1, color = "lrgdpnaPerCapita_chg1")) + 
  geom_line(aes(y = lrgdpnaPerCapita_chg7_temp, color = "lrgdpnaPerCapita_chg7_temp")) + 
  ggtitle("Mean percentile RGDP / growth rates in Caribbean countries (1990-2017)") +
  xlab("Year") + 
  ylab("Per-capita RGDP growth") + 
  scale_color_hue( labels = c("Annual RGDP growth","7-year average")) + 
  labs(color = "Legend") +
  theme_bw()

ggsave("figures/rgdpGrowth_LAC_onlyCarib_mean_1990.png",plot = last_plot())


##### Construct cumulative growth variables

pwt_7 <- pwt[ year == 2017 | year == 2010 | year == 2003 | year == 1996]

pwt_7[ , cumulativeTFP := cumsum(lrtfpna_chg7) / 4, by = .(alpha3)]
pwt_7[ , cumulativeNonTFP := cumsum(lrgdpnaPerCapita_chg7_nonTFP) / 4, by = .(alpha3)]

last_obs <- pwt_7[ , .SD[.N - 1], by = .(alpha3)]

### Make scatter plot version of Table 1 for whole world not just LAC

ggplot(last_obs, aes(x = cumulativeTFP, y = cumulativeNonTFP, color = subRegion_new)) + 
  #geom_point(size = 0, shape = 1) + 
  geom_text(aes(label = alpha3), size = 3) + 
  ggtitle("Average RTFPNA growth typically smaller than average nonTFP growth of RGDPNA / capita (1990-2017)") + 
  labs( x= "Average RTFPNA growth", y = "Average non-RTFPNA RGDPNA / capita growth", color = "Legend") + 
  geom_abline(slope = 1) + 
  labs(color = "Legend") +
  theme_bw()

ggsave("figures/rgdpnaPerCapita_TFPvsNonTFP_scatterPlot_1990.png", plot = last_plot())


### Make Table 1: LAC countries TFP and non-TFP cumulative growth


cumulativeGrowthLAC <- last_obs[ , .(alpha3,subRegion_new,cumulativeTFP,cumulativeNonTFP)][ subRegion_new == "Latin America and the Caribbean"]
newObs <- list("LAC","Latin America and the Caribbean",mean(cumulativeGrowthLAC$cumulativeTFP), mean(cumulativeGrowthLAC$cumulativeNonTFP))
cumulativeGrowthLAC <- rbind(cumulativeGrowthLAC,newObs)
cumulativeGrowthLAC_justTFP <- cumulativeGrowthLAC[ , .(alpha3,cumulativeTFP)]
setkey(cumulativeGrowthLAC_justTFP,alpha3)

cumulativeGrowthLAC <- cumulativeGrowthLAC[order(-cumulativeTFP)]
cumulativeGrowthLAC <- melt(cumulativeGrowthLAC, id.vars = c("alpha3","subRegion_new"), measure.vars = c("cumulativeTFP","cumulativeNonTFP"))
setkey(cumulativeGrowthLAC,alpha3)
cumulativeGrowthLAC <- cumulativeGrowthLAC_justTFP[cumulativeGrowthLAC]

ggplot( cumulativeGrowthLAC, aes(x = reorder(alpha3, cumulativeTFP), y = value, fill = variable)) + 
  geom_bar(stat = "identity") +
  geom_col(position = position_stack(reverse = TRUE)) + 
  xlab("\nCountry") + 
  ylab("Growth decomposition\n") +  
  ggtitle("Average non-TFP RGDPNA / capita growth exceeds average TFP growth in LAC countries (1990-2017)") + 
  scale_fill_discrete(name = "", labels = c("Average TFP growth","Average non-TFP growth")) + 
  coord_flip() + 
  theme_bw()

ggsave("figures/cumulativeGrowthDecomposition_LAC_barplot_1990.png",plot = last_plot())

### Make Table 2: LAC average, decomposition of each 8-year period

library(xtable)

allPeriodsGrowthLAC_7 <- pwt_7[ subRegion_new == "Latin America and the Caribbean", lapply(.SD[ ,.(lrgdpnaPerCapita_chg7_nonTFP, lrtfpna_chg7)], mean), by = .(year)]
newObs <- list("Mean",mean(allPeriodsGrowthLAC_7$lrgdpnaPerCapita_chg7_nonTFP), mean(allPeriodsGrowthLAC_7$lrtfpna_chg7))
allPeriodsGrowthLAC_7 <- rbind(allPeriodsGrowthLAC_7,newObs)
allPeriodsGrowthLAC_7 <- allPeriodsGrowthLAC_7[order(year)]
allPeriodsGrowthLAC_7[ , lrgdpnaPerCapita_chg7 := lrgdpnaPerCapita_chg7_nonTFP + lrtfpna_chg7]

table2 <- xtable(allPeriodsGrowthLAC_7,align = c("l","l","l","l","l"), caption = c("LAC RGDPNA / capita growth decomposition (post 1990)"),digits = 3)
names(table2) <- c("Period end","non-TFP","TFP","Total")
print(table2,"figures/tables/LACgrowthDecomposition_table_1990.tex", type = "latex")

### Make plot: LAC changes in GDP growth vs changes in TFP growth

allPeriodsGrowthLAC_1 <- pwt[ subRegion_new == "Latin America and the Caribbean", lapply(.SD[ ,.(lrgdpnaPerCapita_chg1_nonTFP, lrtfpna_chg1)], mean), by = .(year)]
allPeriodsGrowthLAC_1 <- allPeriodsGrowthLAC_1[order(year)]
allPeriodsGrowthLAC_1[ , lrgdpnaPerCapita_chg1 := lrgdpnaPerCapita_chg1_nonTFP + lrtfpna_chg1]
allPeriodsGrowthLAC_1[ , `:=` (lrgdpnaPerCapita_chg1_nonTFP_chg1 = lrgdpnaPerCapita_chg1_nonTFP - shift(lrgdpnaPerCapita_chg1_nonTFP,n=1L,type = "lag") , lrgdpnaPerCapita_chg1_chg1 = lrgdpnaPerCapita_chg1 - shift(lrgdpnaPerCapita_chg1,n=1L,type = "lag"), lrtfpna_chg1_chg1 = lrtfpna_chg1 - shift(lrtfpna_chg1,n=1L,type = "lag"))]

ggplot(allPeriodsGrowthLAC_1, aes(year)) + 
  geom_line(aes(y = lrgdpnaPerCapita_chg1_chg1, color = "lrgdpnaPerCapita_chg1_chg1")) + 
  geom_line(aes(y = lrtfpna_chg1_chg1, color = "lrtfpna_chg1_chg1")) + 
  ggtitle("1-year chg in growth of rgdpna / capita, rtfpna move together (LAC average) (1990-2017)") + 
  labs(color = "Legend") +
  theme_bw()

ggsave("figures/rgdpnaPerCapitaGrowthChg1_rtfpnaGrowthChg1_LAC_mean_1990.png", plot = last_plot())

ggplot(allPeriodsGrowthLAC_1, aes(year)) + 
  geom_line(aes(y = lrgdpnaPerCapita_chg1_chg1, color = "lrgdpnaPerCapita_chg1_chg1")) + 
  geom_line(aes(y = lrgdpnaPerCapita_chg1_nonTFP_chg1, color = "lrgdpnaPerCapita_chg1_nonTFP_chg1")) +
  ggtitle("1-year chg in growth of rgdpna / capita, non-TFP rgdpna / capita move together, but less (LAC average) (1990-2017)") + 
  labs(color = "Legend") +
  theme_bw()

ggsave("figures/rgdpnaPerCapitaGrowthChg1_rgdpnaPerCapitaGrowthNonTFPChg1_LAC_mean_1990.png", plot = last_plot())

allPeriodsGrowthLAC <- pwt[ subRegion_new == "Latin America and the Caribbean", lapply(.SD[ ,.(lrgdpnaPerCapita_chg7_nonTFP, lrtfpna_chg7)], mean), by = .(year)]
allPeriodsGrowthLAC <- allPeriodsGrowthLAC[order(year)]
allPeriodsGrowthLAC[ , lrgdpnaPerCapita_chg7 := lrgdpnaPerCapita_chg7_nonTFP + lrtfpna_chg7]
allPeriodsGrowthLAC[ , `:=` (lrgdpnaPerCapita_chg7_chg1 = lrgdpnaPerCapita_chg7 - shift(lrgdpnaPerCapita_chg7,n=1L,type = "lag") , lrgdpnaPerCapita_chg7_nonTFP_chg1 = lrgdpnaPerCapita_chg7_nonTFP - shift(lrgdpnaPerCapita_chg7_nonTFP,n=1L,type = "lag"), lrtfpna_chg7_chg1 = lrtfpna_chg7 - shift(lrtfpna_chg7,n=1L,type = "lag"))]

ggplot(allPeriodsGrowthLAC, aes(year)) + 
  geom_line(aes(y = lrgdpnaPerCapita_chg7_chg1, color = "lrgdpnaPerCapita_chg7_chg1")) + 
  geom_line(aes(y = lrtfpna_chg7_chg1, color = "lrtfpna_chg7_chg1")) + 
  ggtitle("1-year chg in growth of rgdpna / capita, TFP move together, but less (LAC average, 7-year windows) (1990-2017)") +
  labs(color = "Legend") +
  theme_bw()

ggsave("figures/rgdpnaPerCapitaGrowth7yrChg1_rtfpnaGrowth7yrChg1_LAC_mean_1990.png", plot = last_plot())

ggplot(allPeriodsGrowthLAC, aes(year)) + 
  geom_line(aes(y = lrgdpnaPerCapita_chg7_chg1, color = "lrgdpnaPerCapita_chg7_chg1")) + 
  geom_line(aes(y = lrgdpnaPerCapita_chg7_nonTFP_chg1, color = "lrgdpnaPerCapita_chg7_nonTFP_chg1")) + 
  ggtitle("1-year chg in growth of rgdpna / capita, non-TFP rgdpna / capita move together, but less (LAC average, 7-year windows) (1990-2017)") + 
  labs(color = "Legend") +
  theme_bw()

ggsave("figures/rgdpnaPerCapitaGrowth7yrChg1_rgdpnaPerCapitaGrowth7yrNonTFPChg1_LAC_mean_1990.png", plot = last_plot())

# Record 7-year data for use in Stata

# Contruct lag 
pwt_7[ , lag_lcgdpoPerCapita_gap_avg7 := shift(lcgdpoPerCapita_gap_avg7, n = 1L, type = "lag"), by = "alpha3"]

fwrite(pwt_7,"data/pwt_7_1990.csv")








