


rm(list = ls())

library(data.table)
library(readstata13)
library(openxlsx)
library(gridExtra)
library(xtable)
library(ggplot2)
library(ggthemr)
library(kableExtra)
library(plm)

setwd("/home/nico/Insync/nfernand@princeton.edu/Google Drive/PhD - Thesis/Research/inequality-and-convergence-in-lac")

ggthemr("flat", spacing = 0.9, layout = "scientific")

#####################################
# Computing raw gaps
#####################################

# Write function to compute raw gaps in a given metric
# Two functions (easier to do it this way): 
# One for LAC vs non-LAC
# One for LAC vs a given other region
computeRawGaps_LACvsNonLAC <- function(data, metric) {
  
  # data is a data.table
  # metric is a string
  
  temp <- data[ , .(average = mean(na.omit(get(metric)))), by = .(countryLAC, year)]
  tempNonLAC <- temp[countryLAC == "nonLAC"][ , .(year,average)]
  setnames(tempNonLAC,"average","nonLAC")
  
  tempLAC <- temp[countryLAC != "nonLAC"]
  
  setkey(tempNonLAC,year)
  setkey(tempLAC,year)
  
  temp <- tempNonLAC[tempLAC]
  #temp <- dcast(temp, year ~ countryLAC, value.var = "average")
  
  temp[ , gap := average - nonLAC]
  return(temp[ , .(countryLAC,year,gap)])
  
}

computeRawGaps_LACvsRegion <- function(data, tempMetric, tempRegion) {

  temp <- data[ subRegion_new %in% c("LAC",tempRegion), .(average = mean(na.omit(get(tempMetric)))), by = .(countryLAC, year)]
  tempComparison <- temp[countryLAC == "nonLAC"][ , .(year,average)]
  
  setnames(tempComparison,"average",tempRegion)
  
  tempLAC <- temp[countryLAC != "nonLAC"]
  
  setkey(tempComparison,year)
  setkey(tempLAC,year)
  
  temp <- tempComparison[tempLAC]
  
  temp[ , gap := average - get(tempRegion)]

  return(temp[ , .(countryLAC,year,gap)])

}

#####################################
# Computing adjusted gaps (baseline)
#
# The baseline adjustment uses a regression with time 
# dummies and GDP gap to form the adjustment
# This is equivalent to considering the average difference in residuals
#####################################

computeAdjustments_LACvsNonLAC <- function(data, tempMetric) {
  
  # data is a data.table
  # metric is a string
  
  # Calculate coefficients for adjustment for stage of development
  model <- plm(get(tempMetric) ~ lcgdpoPerCapita_gap_avg7 + factor(isLAC), data, index = c("alpha3","year"), effect = "time", model = "within")
  modelRobust <- plm(get(tempMetric) ~ lcgdpoPerCapita_gap_avg7 + isLAC.pre1990 + isLAC.post1990, data, index = c("alpha3","year"), effect = "time", model = "within")
  modelGinis <- plm(get(tempMetric) ~ lcgdpoPerCapita_gap_avg7 + isLAC + gini_disp_avg7 + gini_mkt_avg7, data, index = c("alpha3","year"), effect = "time", model = "within")
    
  temp <- data[ , .(adjustment = mean(na.omit(lcgdpoPerCapita_gap_avg7 * model$coefficients[1]))), by = .(countryLAC,year)]
  tempRobust <- data[ , .(adjustmentRobust = mean(na.omit(lcgdpoPerCapita_gap_avg7 * modelRobust$coefficients[1]))), by = .(countryLAC,year)]
  tempGinis <- data[ , .(adjustmentGinisOutputGap = mean(na.omit(lcgdpoPerCapita_gap_avg7 * modelGinis$coefficients[1])), 
                         adjustmentGinisGiniDisp = mean(na.omit(gini_disp_avg7 * modelGinis$coefficients[3])), 
                         adjustmentGinisGiniMkt = mean(na.omit(gini_mkt_avg7 * modelGinis$coefficients[4]))), by = .(countryLAC,year)]
  
  tempNonLAC <- temp[countryLAC == "nonLAC"][ , .(year,adjustment)]
  setnames(tempNonLAC,"adjustment","nonLAC")
  tempLAC <- temp[countryLAC != "nonLAC"]
  
  tempRobustNonLAC <- tempRobust[countryLAC == "nonLAC"][ , .(year,adjustmentRobust)]
  setnames(tempRobustNonLAC,"adjustmentRobust","nonLAC")
  tempRobustLAC <- tempRobust[countryLAC != "nonLAC"]
  
  tempGinisNonLAC <- tempGinis[countryLAC == "nonLAC"][ , .(year,adjustmentGinisOutputGap, adjustmentGinisGiniDisp, adjustmentGinisGiniMkt)]
  setnames(tempGinisNonLAC,"adjustmentGinisOutputGap","nonLAC_outputGap")
  setnames(tempGinisNonLAC,"adjustmentGinisGiniDisp","nonLAC_giniDisp")
  setnames(tempGinisNonLAC,"adjustmentGinisGiniMkt","nonLAC_giniMkt")
  tempGinisLAC <- tempGinis[countryLAC != "nonLAC"]
  
  setkey(tempLAC,year)
  setkey(tempNonLAC,year)
  setkey(tempRobustLAC,year)
  setkey(tempRobustNonLAC,year)
  setkey(tempGinisLAC,year)
  setkey(tempGinisNonLAC,year)
  
  temp <- tempNonLAC[tempLAC]
  tempRobust <- tempRobustNonLAC[tempRobustLAC]
  tempGinis <- tempGinisNonLAC[tempGinisLAC]
  
  temp[ , adjustment := adjustment - nonLAC]
  tempRobust[ , adjustmentRobust := adjustmentRobust - nonLAC]
  tempGinis[ , adjustmentGinisOutputGap := adjustmentGinisOutputGap - nonLAC_outputGap]
  tempGinis[ , adjustmentGinisGiniDisp := adjustmentGinisGiniDisp - nonLAC_giniDisp]
  tempGinis[ , adjustmentGinisGiniMkt := adjustmentGinisGiniMkt - nonLAC_giniMkt]
  
  temp <- temp[ , .(countryLAC,year,adjustment)]
  tempRobust <- tempRobust[ , .(countryLAC,year,adjustmentRobust)]
  tempGinis <- tempGinis[ , .(countryLAC,year,adjustmentGinisOutputGap,adjustmentGinisGiniDisp,adjustmentGinisGiniMkt)]
  
  setkey(temp,countryLAC,year)
  setkey(tempRobust,countryLAC,year)
  setkey(tempGinis,countryLAC,year)
  
  temp <- tempGinis[tempRobust[temp]]
    
  return(temp)
  
}

computeAdjustments_LACvsRegion <- function(data, tempMetric, tempRegion) {
  
  model <- plm(get(tempMetric) ~ lcgdpoPerCapita_gap_avg7, data, index = c("alpha3","year"), effect = "time", model = "within")
  modelRobust <- plm(get(tempMetric) ~ lcgdpoPerCapita_gap_avg7, data[isLAC == 0], index = c("alpha3","year"), effect = "time", model = "within")
    
  model <- plm(get(tempMetric) ~ lcgdpoPerCapita_gap_avg7 + factor(isLAC), data, index = c("alpha3","year"), effect = "time", model = "within")
  modelRobust <- plm(get(tempMetric) ~ lcgdpoPerCapita_gap_avg7 + isLAC.pre1990 + isLAC.post1990, data, index = c("alpha3","year"), effect = "time", model = "within")
  modelGinis <- plm(get(tempMetric) ~ lcgdpoPerCapita_gap_avg7 + factor(countryLAC), data, index = c("alpha3","year"), effect = "time", model = "within")
  
  temp <- data[ subRegion_new %in% c("LAC",tempRegion) , .(adjustment = mean(na.omit(lcgdpoPerCapita_gap_avg7 * model$coefficients[1]))), by = .(countryLAC,year)]
  tempRobust <- data[ subRegion_new %in% c("LAC",tempRegion) , .(adjustmentRobust = mean(na.omit(lcgdpoPerCapita_gap_avg7 * modelRobust$coefficients[1]))), by = .(countryLAC,year)]
  tempGinis <- data[subRegion_new %in% c("LAC",tempRegion)  , .(adjustmentGinis = mean(na.omit(lcgdpoPerCapita_gap_avg7 * modelGinis$coefficients[1]))), by = .(countryLAC,year)]
  
  tempNonLAC <- temp[countryLAC == "nonLAC"][ , .(year,adjustment)]
  setnames(tempNonLAC,"adjustment","nonLAC")
  tempLAC <- temp[countryLAC != "nonLAC"]
  
  tempRobustNonLAC <- tempRobust[countryLAC == "nonLAC"][ , .(year,adjustmentRobust)]
  setnames(tempRobustNonLAC,"adjustmentRobust","nonLAC")
  tempRobustLAC <- tempRobust[countryLAC != "nonLAC"]
  
  tempGinisNonLAC <- tempGinis[countryLAC == "nonLAC"][ , .(year,adjustmentGinis)]
  setnames(tempGinisNonLAC,"adjustmentGinis","nonLAC")
  tempGinisLAC <- tempGinis[countryLAC != "nonLAC"]
  
  setkey(tempLAC,year)
  setkey(tempNonLAC,year)
  setkey(tempRobustLAC,year)
  setkey(tempRobustNonLAC,year)
  setkey(tempGinisLAC,year)
  setkey(tempGinisNonLAC,year)
  
  temp <- tempNonLAC[tempLAC]
  tempRobust <- tempRobustNonLAC[tempRobustLAC]
  tempGinis <- tempGinisNonLAC[tempGinisLAC]
  
  temp[ , adjustment := adjustment - nonLAC]
  tempRobust[ , adjustmentRobust := adjustmentRobust - nonLAC]
  tempGinis[ , adjustmentGinis := adjustmentGinis - nonLAC]
  
  temp <- temp[ , .(countryLAC,year,adjustment)]
  tempRobust <- tempRobust[ , .(countryLAC,year,adjustmentRobust)]
  tempGinis <- tempGinis[ , .(countryLAC,year,adjustmentGinis)]
  
  setkey(temp,countryLAC,year)
  setkey(tempRobust,countryLAC,year)
  setkey(tempGinis,countryLAC,year)
  
  temp <- tempGinis[tempRobust[temp]]
  
  return(temp)
  
} 

######################################################################################################
######################################################################################################
########################
### DOING THE COMPUTATIONS using the functions defined above
########################
######################################################################################################
######################################################################################################

data <- fread("data/pwt91_cleaned_swiid.csv")

setkey(data,alpha3,year)

# Start in 1968, since using 7-year windows
data <- data[ year >= 1968]

# Create isLAC x pre1990 indicator
data[ isLAC == 1 & year <= 1992  , isLAC.pre1990 := 1]
data[ is.na(isLAC.pre1990) , isLAC.pre1990 := 0]
data[ isLAC == 1 & year > 1992  , isLAC.post1990 := 1]
data[ is.na(isLAC.post1990) , isLAC.post1990 := 0]

# Clean up names for easier reading
data[ subRegion_new == "Africa", subRegion_new := "AFR"]
data[ subRegion_new == "East Asian Tigers", subRegion_new := "EAT"]
data[ subRegion_new == "United States", subRegion_new := "USA"]
data[ subRegion_new == "Latin America and the Caribbean", subRegion_new := "LAC"]

# Compute gaps relative to NonLAC first



# First raw gaps
#LACvsNonLAC_lrtfpna_chg7 <- computeRawGaps_LACvsNonLAC(data,"lrtfpna_chg7")
#LACvsNonLAC_lrgdpnaPerCapita_chg7_nonTFP <- computeRawGaps_LACvsNonLAC(data,"lrgdpnaPerCapita_chg7_nonTFP")
#LACvsNonLAC_lrgdpnaPerCapita_chg7 <- computeRawGaps_LACvsNonLAC(data,"lrgdpnaPerCapita_chg7")

# Next, adjusted gaps

#LACvsNonLAC_lrtfpna_chg7_adjustments <- computeAdjustments_LACvsNonLAC(data,"lrtfpna_chg7")
#LACvsNonLAC_lrgdpnaPerCapita_chg7_nonTFP_adjustments <- computeAdjustments_LACvsNonLAC(data,"lrgdpnaPerCapita_chg7_nonTFP")
#LACvsNonLAC_lrgdpnaPerCapita_chg7_adjustments <- computeAdjustments_LACvsNonLAC(data,"lrgdpnaPerCapita_chg7")





# Now compute gaps

for (metric in c("lrtfpna_chg7","lrgdpnaPerCapita_chg7_nonTFP","lrgdpnaPerCapita_chg7")) 
{

  tempStringRaw <- paste0("LACvsNonLAC_",metric)
  tempStringAdjustments <- paste0("LACvsNonLAC_",metric,"_adjustments")
  
  assign(tempStringRaw, computeRawGaps_LACvsNonLAC(data,metric))
  assign(tempStringAdjustments, computeAdjustments_LACvsNonLAC(data,metric))
  
  setkey(get(tempStringRaw), countryLAC, year)
  setkey(get(tempStringAdjustments), countryLAC, year)
  
  assign(tempStringRaw, get(tempStringAdjustments)[get(tempStringRaw)])
  
  get(tempStringRaw)[ , gap_adjusted := gap - adjustment]
  get(tempStringRaw)[ , gap_adjustedRobust := gap - adjustmentRobust]
  #get(tempStringRaw)[ , gap_adjustedGinisOutputGap := gap -]
  get(tempStringRaw)[ , gap_adjustedGinis := gap - adjustmentGinisOutputGap - adjustmentGinisGiniDisp - adjustmentGinisGiniMkt]
    
  assign(tempStringRaw, get(tempStringRaw)[ , .(countryLAC, year, gap, gap_adjusted, gap_adjustedRobust, gap_adjustedGinis, adjustment, adjustmentGinisOutputGap, adjustmentGinisGiniDisp, adjustmentGinisGiniMkt)])
  
  assign(tempStringRaw, melt(get(tempStringRaw), id.vars = c("countryLAC","year"),
                                                  measure.vars = c("gap","gap_adjusted","gap_adjustedRobust","gap_adjustedGinis","adjustment","adjustmentGinisOutputGap","adjustmentGinisGiniDisp","adjustmentGinisGiniMkt")))

  #for (region in c("AFR","EAT","USA"))
  #{
  #  tempStringRaw <- paste0("LACvs",region,"_",metric)
  #  tempStringAdjustments <- paste0("LACvs",region,"_",metric,"_adjustments")
  #  assign(tempStringRaw, computeRawGaps_LACvsRegion(data,metric,region))
  #  assign(tempStringAdjustments, computeAdjustments_LACvsRegion(data,metric,region))
    
  #  setkey(get(tempStringRaw),countryLAC,year)
  #  setkey(get(tempStringAdjustments),countryLAC,year) 
    
  #  assign(tempStringRaw , get(tempStringAdjustments)[get(tempStringRaw)])
    
  #  get(tempStringRaw)[ , gap_adjusted := gap - adjustment]
  #  get(tempStringRaw)[ , gap_adjustedRobust := gap - adjustmentRobust]
  #  get(tempStringRaw)[ , gap_adjustedGinis := gap - adjustmentGinisOutputGap - adjustmentGinisGiniDisp - adjustmentGinisGiniMkt]
    
  #  assign(tempStringRaw, get(tempStringRaw)[ , .(countryLAC, year, gap, gap_adjusted ,gap_adjustedRobust, gap_adjustedGinis, adjustmentGinisOutputGap - adjustmentGinisGiniDisp - adjustmentGinisGiniMkt)])
  
      
  #}
}

### Plot growth graps adjusted by gini regression

ggplot(LACvsNonLAC_lrgdpnaPerCapita_chg7[variable %in% c("gap","gap_adjustedGinis")], aes(x = year-3, y = value, linetype = variable)) + 
  geom_line() + 
  #geom_line( aes(y = gap_adjustedRobust, linetype = "gap_adjustedRobust")) + 
  #geom_line( aes(y = gap_adjustedGinis, linetype = "gap_adjustedGinis")) + 
  facet_wrap(~countryLAC) + 
  labs(title = "Country-level gap vs. non-LAC after adjusting for output per capita gap (rel. USA) and Gini indices (disp. and mkt.)",
       subtitle = "LAC countries, output per capita") + 
  ylab("Annual growth rate (%)") +
  xlab("Year") + 
  scale_linetype_discrete(name = "", labels = c("Raw","Adjusted")) + 
  theme(legend.position = "bottom") + 
  ggsave("figures/rgdpnaPerCapitaRegs_countryGaps_adjustedByGinis.pdf", width = 12, height = 9, units = "in")

ggplot(LACvsNonLAC_lrtfpna_chg7[variable %in% c("gap","gap_adjustedGinis")], aes(x = year-3, y = value, linetype = variable)) + 
  geom_line() + 
  #geom_line( aes(y = gap_adjustedRobust, linetype = "gap_adjustedRobust")) + 
  #geom_line( aes(y = gap_adjustedGinis, linetype = "gap_adjustedGinis")) + 
  facet_wrap(~countryLAC) + 
  labs(title = "Country-level gap vs. non-LAC after adjusting for output per capita gap (rel. USA) and Gini indices (disp. and mkt.)",
       subtitle = "LAC countries, productivity") + 
  ylab("Annual growth rate (%)") +
  xlab("Year") + 
  scale_linetype_discrete(name = "", labels = c("Raw","Adjusted")) + 
  theme(legend.position = "bottom") +
  ggsave("figures/tfpRegs_countryGaps_adjustedByGinis.pdf", width = 12, height = 9, units = "in")

ggplot(LACvsNonLAC_lrgdpnaPerCapita_chg7_nonTFP[variable %in% c("gap","gap_adjustedGinis")], aes(x = year-3, y = value, linetype = variable)) + 
  geom_line() + 
  #geom_line( aes(y = gap_adjustedRobust, linetype = "gap_adjustedRobust")) + 
  #geom_line( aes(y = gap_adjustedGinis, linetype = "gap_adjustedGinis")) + 
  facet_wrap(~countryLAC) + 
  labs(title = "Country-level gap vs. non-LAC after adjusting for output per capita gap (rel. USA) and Gini indices (disp. and mkt.)",
       subtitle = "LAC countries, factors") + 
  ylab("Annual growth rate (%)") +
  xlab("Year") + 
  scale_linetype_discrete(name = "", labels = c("Raw","Adjusted")) + 
  theme(legend.position = "bottom") +
  ggsave("figures/nontfpRegs_countryGaps_adjustedByGinis.pdf", width = 12, height = 9, units = "in")






### Plot adjustments based on Gini regression

ggplot(LACvsNonLAC_lrgdpnaPerCapita_chg7[variable %in% c("adjustmentGinisOutputGap","adjustmentGinisGiniDisp","adjustmentGinisGiniMkt")], aes(x = year-3, y = -value, color = variable)) + 
  geom_line() + 
  #geom_line( aes(y = gap_adjustedRobust, linetype = "gap_adjustedRobust")) + 
  #geom_line( aes(y = gap_adjustedGinis, linetype = "gap_adjustedGinis")) + 
  facet_wrap(~countryLAC) + 
  labs(title = "Adjustments for output per capita gap (rel. USA) and Gini indices (disp. and mkt.) vs. non-LAC ",
       subtitle = "LAC countries, output per capita") + 
  ylab("Annual growth rate (%)") +
  xlab("Year") + 
  scale_color_discrete(name = "", labels = c("Output per capita gap","Gini (disp.)","Gini (mkt.)")) + 
  theme(legend.position = "bottom") + 
  ggsave("figures/rgdpnaPerCapitaRegs_countryGaps_giniAdjustments.pdf", width = 12, height = 9, units = "in")

ggplot(LACvsNonLAC_lrtfpna_chg7[variable %in% c("adjustmentGinisOutputGap","adjustmentGinisGiniDisp","adjustmentGinisGiniMkt")], aes(x = year-3, y = -value, color = variable)) + 
  geom_line() + 
  #geom_line( aes(y = gap_adjustedRobust, linetype = "gap_adjustedRobust")) + 
  #geom_line( aes(y = gap_adjustedGinis, linetype = "gap_adjustedGinis")) + 
  facet_wrap(~countryLAC) + 
  labs(title = "Adjustments for output per capita gap (rel. USA) and Gini indices (disp. and mkt.) vs. non-LAC ",
       subtitle = "LAC countries, productivity") + 
  ylab("Annual growth rate (%)") +
  xlab("Year") + 
  scale_color_discrete(name = "", labels = c("Output per capita gap","Gini (disp.)","Gini (mkt.)")) + 
  theme(legend.position = "bottom") + 
  ggsave("figures/nontfpRegs_countryGaps_giniAdjustments.pdf", width = 12, height = 9, units = "in")

ggplot(LACvsNonLAC_lrgdpnaPerCapita_chg7_nonTFP[variable %in% c("adjustmentGinisOutputGap","adjustmentGinisGiniDisp","adjustmentGinisGiniMkt")], aes(x = year-3, y = -value, color = variable)) + 
  geom_line() + 
  #geom_line( aes(y = gap_adjustedRobust, linetype = "gap_adjustedRobust")) + R
  #geom_line( aes(y = gap_adjustedGinis, linetype = "gap_adjustedGinis")) + 
  facet_wrap(~countryLAC) + 
  labs(title = "Adjustments for output per capita gap (rel. USA) and Gini indices (disp. and mkt.) vs. non-LAC ",
       subtitle = "LAC countries, factors") + 
  ylab("Annual growth rate (%)") +
  xlab("Year") + 
  scale_color_discrete(name = "", labels = c("Output per capita","Gini (disp.)","Gini (mkt.)")) + 
  theme(legend.position = "bottom") + 
  ggsave("figures/tfpRegs_countryGaps_giniAdjustments.pdf", width = 12, height = 9, units = "in")



