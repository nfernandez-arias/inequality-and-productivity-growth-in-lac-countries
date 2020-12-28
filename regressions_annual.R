
library(plm)
library(sandwich)

data <- fread("data/pwt91_cleaned_swiid.csv")

setkey(data,alpha3,year)

data <- data[ year >= 1968]

data[ , gini_disp := (1/7) * Reduce(`+`, shift(gini_disp, n = 0L:6L, type = "lag")), by = .(alpha3)]
data[ , gini_mkt := (1/7) * Reduce(`+`, shift(gini_mkt, n = 0L:6L, type = "lag")), by = .(alpha3)]

data[ !is.na(gini_disp), hasGiniDisp := 1]
data[ is.na(gini_disp), hasGiniDisp := 0]
data[ !is.na(gini_mkt), hasGiniMkt := 1]
data[ is.na(gini_mkt), hasGiniMkt := 0]

## Country residuals

#data <- data[ hasGiniDisp == 1 & hasGiniMkt == 1]
# Turn countryLAC into factor variable (so can choose reference group)
data[ , countryLAC := relevel(factor(countryLAC),"nonLAC")]


temp <- data[ , .(isLAC = isLAC, V1 = mean(lrtfpna_chg7)), by = .(countryLAC, year)]
temp[ , V1 := mean(na.omit(V1)), by = .(isLAC, year)]

temp2 <- data[ , mean(lrtfpna_chg7), by = .(isLAC,year)]

rtfpModelSimple <- plm(lrtfpna_chg7 ~ lcgdpoPerCapita_gap_avg7, data, effect = "time", model = "within")
rtfpModelSimpleCoefs <- cbind(names(coefficients(rtfpModelSimple)),data.table(summary(rtfpModelSimple)$coefficients))[3:51]
rtfpModelSimpleCoefs[ , V1 := substr(V1,13,16)]

data[ , rtfpModelSimpleResid := resid(rtfpModelSimple)]

data[ , rtfpModelSimpleResidAvg := mean(na.omit(rtfpModelSimpleResid)), by = .(isLAC,year)]

ggplot(data, aes(x = year, y = rtfpModelSimpleResidAvg, linetype = factor(isLAC))) + geom_line()

# Adjusting for stage of development 

rtfpModel <- plm(lrtfpna_chg7 ~ lcgdpoPerCapita_gap_avg7 + countryLAC, data, effect = "time", model = "within")
nonTfpModel <- plm(lrgdpnaPerCapita_chg7_nonTFP ~ lcgdpoPerCapita_gap_avg7 + countryLAC, data, effect = "time", model = "within")
rgdpnaPerCapitaModel <- plm(lrgdpnaPerCapita_chg7 ~ lcgdpoPerCapita_gap_avg7 + countryLAC, data, effect = "time", model = "within")

rtfpModelCoefs <- cbind(names(coefficients(rtfpModel)),data.table(summary(rtfpModel)$coefficients))[2:.N]
nonTfpModelCoefs <- cbind(names(coefficients(nonTfpModel)),data.table(summary(nonTfpModel)$coefficients))[2:.N]
rgdpnaPerCapitaModelCoefs <- cbind(names(coefficients(rgdpnaPerCapitaModel)),data.table(summary(rgdpnaPerCapitaModel)$coefficients))[2:.N]

rtfpModelCoefs[ , V1 := substr(V1,11,13)]
nonTfpModelCoefs[ , V1 := substr(V1,11,13)]
rgdpnaPerCapitaModelCoefs[ , V1 := substr(V1,11,13)]

setnames(rtfpModelCoefs,"Estimate","rtfpModelCountryDummy")
setnames(nonTfpModelCoefs,"Estimate","nonTfpModelCountryDummy")
setnames(rgdpnaPerCapitaModelCoefs,"Estimate","rgdpnaPerCapitaModelCountryDummy")

setkey(rtfpModelCoefs,V1)
setkey(nonTfpModelCoefs,V1)
setkey(rgdpnaPerCapitaModelCoefs,V1)
setkey(data,alpha3)

data <- rtfpModelCoefs[ , .(V1,rtfpModelCountryDummy)][data]
setkey(data,V1)
data <- nonTfpModelCoefs[ , .(V1,nonTfpModelCountryDummy)][data]
data <- rgdpnaPerCapitaModelCoefs[ , .(V1,rgdpnaPerCapitaModelCountryDummy)][data]

data[ is.na(rtfpModelCountryDummy), rtfpModelCountryDummy := 0]
data[ is.na(nonTfpModelCountryDummy), nonTfpModelCountryDummy := 0]
data[ is.na(rgdpnaPerCapitaModelCountryDummy), rgdpnaPerCapitaModelCountryDummy := 0]

setnames(data,"V1","alpha3")

data[ , rtfpModelResid := resid(rtfpModel) + rtfpModelCountryDummy]
data[ , nonTfpModelResid := resid(nonTfpModel) + nonTfpModelCountryDummy]
data[ , rgdpnaPerCapitaModelResid := resid(rgdpnaPerCapitaModel) + rgdpnaPerCapitaModelCountryDummy]

setkey(data,alpha3,year)

data[ , rtfpModelResidAvg := resid(rtfpModel)]
data[ isLAC == 0, rtfpModelResid := rtfpModelResidAvg]
data[ , rtfpModelResidAvg := mean(na.omit(rtfpModelResidAvg)), by = .(isLAC,year)]

data[ , rtfpModelResidDummyAvg := mean(na.omit(rtfpModelResid)), by = .(isLAC,year)]

data[ , ]

ggplot(data[subRegion_new == "Latin America and the Caribbean"], aes(x = year, y = rtfpModelResid)) + 
  geom_line() + 
  labs(title = "Country residuals after adjusting for stage of development",
       subtitle = "LAC countries, productivity growth") + 
  ylab("Annual growth rate (%)") + 
  xlab("Year") + 
  facet_wrap(~alpha3) +  
  ggsave("figures/tfpRegs_cgdpGap_annual_countryResiduals.pdf", width = 12, height = 9, units = "in")

ggplot(data[subRegion_new == "Latin America and the Caribbean"], aes(x = year, y = nonTfpModelResid)) + 
  geom_line() + 
  labs(title = "Country residuals after adjusting for stage of development",
       subtitle = "LAC countries, factor accumulation") + 
  ylab("Annual growth rate (%)") + 
  xlab("Year") + 
  facet_wrap(~alpha3) + 
  ggsave("figures/nonTfpRegs_cgdpGap_annual_countryResiduals.pdf", width = 12, height = 9, units = "in")

ggplot(data[subRegion_new == "Latin America and the Caribbean"], aes(x = year, y = rgdpnaPerCapitaModelResid)) + 
  geom_line() + 
  labs(title = "Country residuals after adjusting for stage of development",
       subtitle = "LAC countries, output per capita growth") + 
  ylab("Annual growth rate (%)") + 
  xlab("Year") + 
  facet_wrap(~alpha3) + 
  ggsave("figures/rgdpnaPerCapitaRegs_cgdpGap_annual_countryResiduals.pdf", width = 12, height = 9, units = "in")


LACandNonLACrtfpResid <- data[ , .(rtfpmodelResid = mean(na.omit(rtfpModelResid))), by = .(isLAC, year)]

LACrtfpResid <- data[ subRegion_new == "Latin America and the Caribbean", .(rtfpModelResid = mean(rtfpModelResid)), by = .(year) ]
LACnonTfpResid <- data[ subRegion_new == "Latin America and the Caribbean", .(nonTfpModelResid = mean(nonTfpModelResid)), by = .(year) ]
LACrgdpnaPerCapitaResid <- data[ subRegion_new == "Latin America and the Caribbean", .(rgdpnaPerCapitaModelResid = mean(rgdpnaPerCapitaModelResid)), by = .(year) ]

setkey(LACrtfpResid,year)
setkey(LACnonTfpResid,year)
setkey(LACrgdpnaPerCapitaResid,year)

LACseries <- LACnonTfpResid[LACrtfpResid]
setkey(LACseries,year)
LACseries <- LACrgdpnaPerCapitaResid[LACseries]

ggplot(LACseries, aes(x = year)) + 
  geom_line(aes(y = rtfpModelResid)) + 
  labs(title = "Average LAC residual after adjusting for stage of development", 
       subtitle = "Productivity growth") + 
  ylab("Annual growth rate (%)") + 
  xlab("Year") + 
  ggsave("figures/tfpRegs_cgdpGap_annual_LACresiduals.pdf", width = 12, height = 9, units = "in")

ggplot(LACseries, aes(x = year)) + 
  geom_line(aes(y = nonTfpModelResid)) + 
  labs(title = "Average LAC residual after adjusting for stage of development", 
       subtitle = "Factor accumulation") + 
  ylab("Annual growth rate (%)") + 
  xlab("Year") + 
  ggsave("figures/nonTfpRegs_cgdpGap_annual_LACresiduals.pdf", width = 12, height = 9, units = "in")

ggplot(LACseries, aes(x = year)) + 
  geom_line(aes(y = rgdpnaPerCapitaModelResid)) + 
  labs(title = "Average LAC residual after adjusting for stage of development", 
       subtitle = "Output per capita growth") + 
  ylab("Annual growth rate (%)") + 
  xlab("Year") + 
  ggsave("figures/rgdpnaPerCapitaRegs_cgdpGap_annual_LACresiduals.pdf", width = 12, height = 9, units = "in")


# Adjusting for stage of development and Gini

setkey(data,alpha3,year)

rtfpModelGini <- plm(lrtfpna_chg7 ~ lcgdpoPerCapita_gap_avg7 + gini_disp + gini_mkt + countryLAC, data2, effect = "time", model = "within", index = c("alpha3","year"))
nonTfpModelGini <- plm(lrgdpnaPerCapita_chg7_nonTFP ~ lcgdpoPerCapita_gap_avg7 + gini_disp + gini_mkt + countryLAC, data, effect = "time", model = "within", index = c("alpha3","year"))
rgdpnaPerCapitaModelGini <- plm(lrgdpnaPerCapita_chg7 ~ lcgdpoPerCapita_gap_avg7 + gini_disp + gini_mkt + countryLAC, data, effect = "time", model = "within", index = c("alpha3","year"))

rtfpModelGiniCoefs <- cbind(names(coefficients(rtfpModelGini)),data.table(summary(rtfpModelGini)$coefficients))[4:.N]
nonTfpModelGiniCoefs <- cbind(names(coefficients(nonTfpModelGini)),data.table(summary(nonTfpModelGini)$coefficients))[4:.N]
rgdpnaPerCapitaModelGiniCoefs <- cbind(names(coefficients(rgdpnaPerCapitaModelGini)),data.table(summary(rgdpnaPerCapitaModelGini)$coefficients))[4:.N]

rtfpModelGiniCoefs[ , V1 := substr(V1,11,13)]
nonTfpModelGiniCoefs[ , V1 := substr(V1,11,13)]
rgdpnaPerCapitaModelGiniCoefs[ , V1 := substr(V1,11,13)]

setnames(rtfpModelGiniCoefs,"Estimate","rtfpModelGiniCountryDummy")
setnames(nonTfpModelGiniCoefs,"Estimate","nonTfpModelGiniCountryDummy")
setnames(rgdpnaPerCapitaModelGiniCoefs,"Estimate","rgdpnaPerCapitaModelGiniCountryDummy")

setkey(rtfpModelGiniCoefs,V1)
setkey(nonTfpModelGiniCoefs,V1)
setkey(rgdpnaPerCapitaModelGiniCoefs,V1)
setkey(data,alpha3)

data <- rtfpModelGiniCoefs[ , .(V1,rtfpModelGiniCountryDummy)][data]
setkey(data,V1)
data <- nonTfpModelGiniCoefs[ , .(V1,nonTfpModelGiniCountryDummy)][data]
data <- rgdpnaPerCapitaModelGiniCoefs[ , .(V1,rgdpnaPerCapitaModelGiniCountryDummy)][data]

setnames(data,"V1","alpha3")

dataHasGini <- data[ hasGiniDisp == 1 & hasGiniMkt == 1]

dataHasGini[ , rtfpModelResidGini := resid(rtfpModelGini) + rtfpModelGiniCountryDummy]
dataHasGini[ , nonTfpModelResidGini := resid(nonTfpModelGini)+ nonTfpModelGiniCountryDummy]
dataHasGini[ , rgdpnaPerCapitaModelResidGini := resid(rgdpnaPerCapitaModelGini)+ rgdpnaPerCapitaModelGiniCountryDummy]

ggplot(dataHasGini[subRegion_new == "Latin America and the Caribbean"], aes(x = year, y = rtfpModelResidGini)) + 
  geom_line() + 
  labs(title = "Country residuals after adjusting for stage of development and Gini coef (disposable and market)",
       subtitle = "LAC countries, productivity growth") + 
  ylab("Annual growth rate (%)") + 
  xlab("Year") + 
  facet_wrap(~alpha3) 

ggplot(dataHasGini[subRegion_new == "Latin America and the Caribbean"], aes(x = year, y = nonTfpModelResidGini)) + 
  geom_line() + 
  labs(title = "Country residuals after adjusting for stage of development",
       subtitle = "LAC countries, factor accumulation") + 
  ylab("Annual growth rate (%)") + 
  xlab("Year") + 
  facet_wrap(~alpha3)

ggplot(dataHasGini[subRegion_new == "Latin America and the Caribbean"], aes(x = year, y = rgdpnaPerCapitaModelResidGini)) + 
  geom_line() + 
  labs(title = "Country residuals after adjusting for stage of development",
       subtitle = "LAC countries, output per capita growth") + 
  ylab("Annual growth rate (%)") + 
  xlab("Year") + 
  facet_wrap(~alpha3)


## Comparing the two on a single plot

setkey(dataHasGini, alpha3, year)
setkey(data, alpha3, year)

data <- dataHasGini[ , .(alpha3, year, rtfpModelResidGini, nonTfpModelResidGini, rgdpnaPerCapitaModelResidGini)][data[ , .(subRegion_new, alpha3,year,rtfpModelResid, nonTfpModelResid, rgdpnaPerCapitaModelResid)]]


ggplot(data[subRegion_new == "Latin America and the Caribbean"], aes(x = year)) + 
  geom_line(aes(y = rtfpModelResid, linetype = "rtfpModelResid")) + 
  geom_line(aes(y = rtfpModelResidGini, linetype = "rtfpModelResidGini")) + 
  labs(title = "Country residuals after adjusting for output per capita gap (relative USA) and Gini (disp, mkt)",
       subtitle = "LAC countries, productivity growth") + 
  ylab("Annual growth rate (%)") + 
  xlab("Year") + 
  scale_linetype_discrete(name = "",labels = c("Adjusting for output per capita gap","Adjusting for output per capita gap and Gini indices")) +  
  theme(legend.position = "bottom") + 
  facet_wrap(~alpha3) + 
  ggsave("figures/tfpRegs_cgdpGap_giniDisp_giniMkt_annual_countryResiduals.pdf", width = 12, height = 9, units = "in")

ggplot(data[subRegion_new == "Latin America and the Caribbean"], aes(x = year, y = nonTfpModelResidGini)) + 
  geom_line(aes(y = rtfpModelResid, linetype = "nonTfpModelResid")) + 
  geom_line(aes(y = rtfpModelResidGini, linetype = "nonTfpModelResidGini")) + 
  labs(title = "Country residuals after adjusting for output per capita gap (relative USA) and Gini (disp, mkt)",
       subtitle = "LAC countries, factor accumulation") + 
  ylab("Annual growth rate (%)") + 
  xlab("Year") + 
  scale_linetype_discrete(name = "",labels = c("Adjusting for output per capita gap","Adjusting for output per capita gap and Gini indices")) + 
  theme(legend.position = "bottom") + 
  facet_wrap(~alpha3) + 
  ggsave("figures/nontfpRegs_cgdpGap_giniDisp_giniMkt_annual_countryResiduals.pdf", width = 12, height = 9, units = "in")

ggplot(data[subRegion_new == "Latin America and the Caribbean"], aes(x = year, y = rgdpnaPerCapitaModelResidGini)) + 
  geom_line(aes(y = rtfpModelResid, linetype = "rgdpnaPerCapitaModelResid")) + 
  geom_line(aes(y = rtfpModelResidGini, linetype = "rgdpnaPerCapitaModelResidGini")) + 
  labs(title = "Country residuals after adjusting for output per capita gap (relative USA) and Gini (disp, mkt)",
       subtitle = "LAC countries, output per capita growth") + 
  ylab("Annual growth rate (%)") + 
  xlab("Year") + 
  scale_linetype_discrete(name = "",labels = c("Adjusting for output per capita gap","Adjusting for output per capita gap and Gini indices")) + 
  theme(legend.position = "bottom") + 
  facet_wrap(~alpha3) + 
  ggsave("figures/rgdpnaPerCapitaRegs_cgdpGap_giniDisp_giniMkt_annual_countryResiduals.pdf", width = 12, height = 9, units = "in")
