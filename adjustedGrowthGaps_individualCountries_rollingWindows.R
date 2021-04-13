


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
  modelCountryDummies <- plm(get(tempMetric) ~ lcgdpoPerCapita_gap_avg7 + factor(countryLAC), data, index = c("alpha3","year"), effect = "time", model = "within")
    
  temp <- data[ , .(adjustment = mean(na.omit(lcgdpoPerCapita_gap_avg7 * model$coefficients[1]))), by = .(countryLAC,year)]
  tempRobust <- data[ , .(adjustmentRobust = mean(na.omit(lcgdpoPerCapita_gap_avg7 * modelRobust$coefficients[1]))), by = .(countryLAC,year)]
  tempCountryDummies <- data[ , .(adjustmentCountryDummies = mean(na.omit(lcgdpoPerCapita_gap_avg7 * modelCountryDummies$coefficients[1]))), by = .(countryLAC,year)]
  
  tempNonLAC <- temp[countryLAC == "nonLAC"][ , .(year,adjustment)]
  setnames(tempNonLAC,"adjustment","nonLAC")
  tempLAC <- temp[countryLAC != "nonLAC"]
  
  tempRobustNonLAC <- tempRobust[countryLAC == "nonLAC"][ , .(year,adjustmentRobust)]
  setnames(tempRobustNonLAC,"adjustmentRobust","nonLAC")
  tempRobustLAC <- tempRobust[countryLAC != "nonLAC"]
  
  tempCountryDummiesNonLAC <- tempCountryDummies[countryLAC == "nonLAC"][ , .(year,adjustmentCountryDummies)]
  setnames(tempCountryDummiesNonLAC,"adjustmentCountryDummies","nonLAC")
  tempCountryDummiesLAC <- tempCountryDummies[countryLAC != "nonLAC"]
  
  setkey(tempLAC,year)
  setkey(tempNonLAC,year)
  setkey(tempRobustLAC,year)
  setkey(tempRobustNonLAC,year)
  setkey(tempCountryDummiesLAC,year)
  setkey(tempCountryDummiesNonLAC,year)
  
  temp <- tempNonLAC[tempLAC]
  tempRobust <- tempRobustNonLAC[tempRobustLAC]
  tempCountryDummies <- tempCountryDummiesNonLAC[tempCountryDummiesLAC]
  
  temp[ , adjustment := adjustment - nonLAC]
  tempRobust[ , adjustmentRobust := adjustmentRobust - nonLAC]
  tempCountryDummies[ , adjustmentCountryDummies := adjustmentCountryDummies - nonLAC]
  
  temp <- temp[ , .(countryLAC,year,adjustment)]
  tempRobust <- tempRobust[ , .(countryLAC,year,adjustmentRobust)]
  tempCountryDummies <- tempCountryDummies[ , .(countryLAC,year,adjustmentCountryDummies)]
  
  setkey(temp,countryLAC,year)
  setkey(tempRobust,countryLAC,year)
  setkey(tempCountryDummies,countryLAC,year)
  
  temp <- tempCountryDummies[tempRobust[temp]]
    
  return(temp)
  
}

computeAdjustments_LACvsRegion <- function(data, tempMetric, tempRegion) {
  
  model <- plm(get(tempMetric) ~ lcgdpoPerCapita_gap_avg7, data, index = c("alpha3","year"), effect = "time", model = "within")
  modelRobust <- plm(get(tempMetric) ~ lcgdpoPerCapita_gap_avg7, data[isLAC == 0], index = c("alpha3","year"), effect = "time", model = "within")
    
  model <- plm(get(tempMetric) ~ lcgdpoPerCapita_gap_avg7 + factor(isLAC), data, index = c("alpha3","year"), effect = "time", model = "within")
  modelRobust <- plm(get(tempMetric) ~ lcgdpoPerCapita_gap_avg7 + isLAC.pre1990 + isLAC.post1990, data, index = c("alpha3","year"), effect = "time", model = "within")
  modelCountryDummies <- plm(get(tempMetric) ~ lcgdpoPerCapita_gap_avg7 + factor(countryLAC), data, index = c("alpha3","year"), effect = "time", model = "within")
  
  temp <- data[ subRegion_new %in% c("LAC",tempRegion) , .(adjustment = mean(na.omit(lcgdpoPerCapita_gap_avg7 * model$coefficients[1]))), by = .(countryLAC,year)]
  tempRobust <- data[ subRegion_new %in% c("LAC",tempRegion) , .(adjustmentRobust = mean(na.omit(lcgdpoPerCapita_gap_avg7 * modelRobust$coefficients[1]))), by = .(countryLAC,year)]
  tempCountryDummies <- data[subRegion_new %in% c("LAC",tempRegion)  , .(adjustmentCountryDummies = mean(na.omit(lcgdpoPerCapita_gap_avg7 * modelCountryDummies$coefficients[1]))), by = .(countryLAC,year)]
  
  tempNonLAC <- temp[countryLAC == "nonLAC"][ , .(year,adjustment)]
  setnames(tempNonLAC,"adjustment","nonLAC")
  tempLAC <- temp[countryLAC != "nonLAC"]
  
  tempRobustNonLAC <- tempRobust[countryLAC == "nonLAC"][ , .(year,adjustmentRobust)]
  setnames(tempRobustNonLAC,"adjustmentRobust","nonLAC")
  tempRobustLAC <- tempRobust[countryLAC != "nonLAC"]
  
  tempCountryDummiesNonLAC <- tempCountryDummies[countryLAC == "nonLAC"][ , .(year,adjustmentCountryDummies)]
  setnames(tempCountryDummiesNonLAC,"adjustmentCountryDummies","nonLAC")
  tempCountryDummiesLAC <- tempCountryDummies[countryLAC != "nonLAC"]
  
  setkey(tempLAC,year)
  setkey(tempNonLAC,year)
  setkey(tempRobustLAC,year)
  setkey(tempRobustNonLAC,year)
  setkey(tempCountryDummiesLAC,year)
  setkey(tempCountryDummiesNonLAC,year)
  
  temp <- tempNonLAC[tempLAC]
  tempRobust <- tempRobustNonLAC[tempRobustLAC]
  tempCountryDummies <- tempCountryDummiesNonLAC[tempCountryDummiesLAC]
  
  temp[ , adjustment := adjustment - nonLAC]
  tempRobust[ , adjustmentRobust := adjustmentRobust - nonLAC]
  tempCountryDummies[ , adjustmentCountryDummies := adjustmentCountryDummies - nonLAC]
  
  temp <- temp[ , .(countryLAC,year,adjustment)]
  tempRobust <- tempRobust[ , .(countryLAC,year,adjustmentRobust)]
  tempCountryDummies <- tempCountryDummies[ , .(countryLAC,year,adjustmentCountryDummies)]
  
  setkey(temp,countryLAC,year)
  setkey(tempRobust,countryLAC,year)
  setkey(tempCountryDummies,countryLAC,year)
  
  temp <- tempCountryDummies[tempRobust[temp]]
  
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
  get(tempStringRaw)[ , gap_adjustedCountryDummies := gap - adjustmentCountryDummies]
    
  assign(tempStringRaw, get(tempStringRaw)[ , .(countryLAC, year, gap, gap_adjusted, gap_adjustedRobust, gap_adjustedCountryDummies)])

  for (region in c("AFR","EAT","USA"))
  {
    tempStringRaw <- paste0("LACvs",region,"_",metric)
    tempStringAdjustments <- paste0("LACvs",region,"_",metric,"_adjustments")
    assign(tempStringRaw, computeRawGaps_LACvsRegion(data,metric,region))
    assign(tempStringAdjustments, computeAdjustments_LACvsRegion(data,metric,region))
    
    setkey(get(tempStringRaw),countryLAC,year)
    setkey(get(tempStringAdjustments),countryLAC,year)
    
    assign(tempStringRaw , get(tempStringAdjustments)[get(tempStringRaw)])
    
    get(tempStringRaw)[ , gap_adjusted := gap - adjustment]
    get(tempStringRaw)[ , gap_adjustedRobust := gap - adjustmentRobust]
    get(tempStringRaw)[ , gap_adjustedCountryDummies := gap - adjustmentCountryDummies]
    
    assign(tempStringRaw, get(tempStringRaw)[ , .(countryLAC, year, gap, gap_adjusted ,gap_adjustedRobust, gap_adjustedCountryDummies)])
  
      
  }
}


########
#### Figure 10
########

ggplot(LACvsNonLAC_lrtfpna_chg7, aes(x = year-3)) + 
  #geom_line( aes(y = gap, linetype = "gap")) + 
  geom_line( aes(y = gap_adjusted, linetype = "gap_adjusted")) + 
  #geom_line( aes(y = gap_adjustedRobust, linetype = "gap_adjustedRobust")) + 
  #geom_line( aes(y = gap_adjustedCountryDummies, linetype = "gap_adjustedCountryDummies")) + 
  facet_wrap(~countryLAC) + 
  labs(title = "Country-level gap vs. non-LAC after adjusting for stage of development",
       subtitle = "LAC countries, productivity") + 
  ylab("Annual growth rate (%)") + 
  xlab("Year") + 
  #scale_linetype_discrete(name = "", labels = c("Raw","Adjusted","Adjusted (robust)","Adjusted (country dummies regression)")) +
  scale_linetype_discrete(name = "", labels = c("Adjusted growth gap")) +
  theme(legend.position = "bottom") + 
  ggsave("figures/tfpRegs_cgdpGap_annual_countryResiduals.pdf", width = 12, height = 9, units = "in") + 
  ggsave("figures/finalFiguresAndTables/Figure10.pdf", width = 12, height = 9, units = "in")

########
#### Figure 11
########

ggplot(LACvsNonLAC_lrgdpnaPerCapita_chg7_nonTFP, aes(x = year-3)) + 
  #geom_line( aes(y = gap, linetype = "gap")) + 
  geom_line( aes(y = gap_adjusted, linetype = "gap_adjusted")) + 
  #geom_line( aes(y = gap_adjustedRobust, linetype = "gap_adjustedRobust")) + 
  #geom_line( aes(y = gap_adjustedCountryDummies, linetype = "gap_adjustedCountryDummies")) + 
  facet_wrap(~countryLAC) + 
  labs(title = "Country-level gap vs. non-LAC after adjusting for stage of development",
       subtitle = "LAC countries, factors") + 
  ylab("Annual growth rate (%)") + 
  xlab("Year") + 
  #scale_linetype_discrete(name = "", labels = c("Raw","Adjusted","Adjusted (robust)","Adjusted (country dummies regression)")) + 
  scale_linetype_discrete(name = "", labels = c("Adjusted growth gap")) +
  theme(legend.position = "bottom") + 
  ggsave("figures/nontfpRegs_cgdpGap_annual_countryResiduals.pdf", width = 12, height = 9, units = "in") + 
  ggsave("figures/finalFiguresAndTables/Figure11.pdf", width = 12, height = 9, units = "in")

########
#### Figure 9
########

ggplot(LACvsNonLAC_lrgdpnaPerCapita_chg7, aes(x = year-3)) + 
  #geom_line( aes(y = gap, linetype = "gap")) + 
  geom_line( aes(y = gap_adjusted, linetype = "gap_adjusted")) + 
  #geom_line( aes(y = gap_adjustedRobust, linetype = "gap_adjustedRobust")) + 
  #geom_line( aes(y = gap_adjustedCountryDummies, linetype = "gap_adjustedCountryDummies")) + 
  facet_wrap(~countryLAC) + 
  labs(title = "Country-level gap vs. non-LAC after adjusting for stage of development",
       subtitle = "LAC countries, output per capita") + 
  ylab("Annual growth rate (%)") + 
  xlab("Year") + 
  #scale_linetype_discrete(name = "", labels = c("Raw","Adjusted","Adjusted (robust)","Adjusted (country dummies regression)")) + 
  scale_linetype_discrete(name = "", labels = c("Adjusted growth gap")) +
  theme(legend.position = "bottom") + 
  ggsave("figures/rgdpnaPerCapitaRegs_cgdpGap_annual_countryResiduals.pdf", width = 12, height = 9, units = "in") +
  ggsave("figures/finalFiguresAndTables/Figure9.pdf", width = 12, height = 9, units = "in")



for (tempRegion in c("EAT","AFR","USA")) 
{
  
  tempString <- paste0("LACvs",tempRegion,"_lrtfpna_chg7")
  
  ggplot(get(tempString), aes(x = year-3)) + 
    geom_line( aes(y = gap, linetype = "gap")) + 
    geom_line( aes(y = gap_adjusted, linetype = "gap_adjusted")) + 
    #geom_line( aes(y = gap_adjustedRobust, linetype = "gap_adjustedRobust")) + 
    #geom_line( aes(y = gap_adjustedCountryDummies, linetype = "gap_adjustedCountryDummies")) + 
    facet_wrap(~countryLAC) + 
    labs(title = paste0("Country-level gap vs. ",tempRegion," after adjusting for stage of development"),
         subtitle = "LAC countries, productivity") + 
    ylab("Annual growth rate (%)") + 
    xlab("Year") + 
    scale_linetype_discrete(name = "", labels = c("Raw","Adjusted","Adjusted (robust)","Adjusted (country dummies regression)")) + 
    theme(legend.position = "bottom") + 
    ggsave(paste0("figures/tfpRegs_cgdpGap_annual_countryResiduals_",tempRegion,".pdf"), width = 12, height = 9, units = "in")
  
  
  tempString <- paste0("LACvs",tempRegion,"_lrgdpnaPerCapita_chg7_nonTFP")
  
  ggplot(get(tempString), aes(x = year-3)) + 
    geom_line( aes(y = gap, linetype = "gap")) + 
    geom_line( aes(y = gap_adjusted, linetype = "gap_adjusted")) + 
    #geom_line( aes(y = gap_adjustedRobust, linetype = "gap_adjustedRobust")) + 
    #geom_line( aes(y = gap_adjustedCountryDummies, linetype = "gap_adjustedCountryDummies")) + 
    facet_wrap(~countryLAC) + 
    labs(title = paste0("Country-level gap vs. ",tempRegion," after adjusting for stage of development"),
         subtitle = "LAC countries, factors") + 
    ylab("Annual growth rate (%)") + 
    xlab("Year") + 
    scale_linetype_discrete(name = "", labels = c("Raw","Adjusted","Adjusted (robust)","Adjusted (country dummies regression)")) + 
    theme(legend.position = "bottom") + 
    ggsave(paste0("figures/nontfpRegs_cgdpGap_annual_countryResiduals_",tempRegion,".pdf"), width = 12, height = 9, units = "in")
  
  tempString <- paste0("LACvs",tempRegion,"_lrgdpnaPerCapita_chg7")
  
  ggplot(get(tempString), aes(x = year-3)) + 
    geom_line( aes(y = gap, linetype = "gap")) + 
    geom_line( aes(y = gap_adjusted, linetype = "gap_adjusted")) + 
    #geom_line( aes(y = gap_adjustedRobust, linetype = "gap_adjustedRobust")) + 
    #geom_line( aes(y = gap_adjustedCountryDummies, linetype = "gap_adjustedCountryDummies")) + 
    facet_wrap(~countryLAC) + 
    labs(title = paste0("Country-level gap vs. ",tempRegion," after adjusting for stage of development"),
         subtitle = "LAC countries, output per capita") + 
    ylab("Annual growth rate (%)") + 
    xlab("Year") + 
    scale_linetype_discrete(name = "", labels = c("Raw","Adjusted","Adjusted (robust)","Adjusted (country dummies regression)")) + 
    theme(legend.position = "bottom") + 
    ggsave(paste0("figures/rgdpnaPerCapitaRegs_cgdpGap_annual_countryResiduals_",tempRegion,".pdf"), width = 12, height = 9, units = "in")
  
}


# Next construct scatterplot showing individual LAC country gaps compared to non-LAC, 
# after controlling for stage of development using regression based on LAC.pre1990 and LAC.post1990 dummies (and time dummies of course)

# First step is to average gap_adjustedRobust by country and pre1990 status.

LACvsNonLAC_lrtfpna_chg7[ year <= 1992, period := "Pre.1990"]
LACvsNonLAC_lrtfpna_chg7[ is.na(period), period := "Post.1990"]
countryGaps_PrePost1990 <- LACvsNonLAC_lrtfpna_chg7[ , mean(na.omit(gap_adjustedRobust)), by = .(countryLAC,period)]

countryGaps_PrePost1990 <- dcast(countryGaps_PrePost1990, countryLAC ~ period, value.var = "V1")


########
#### Figure 13
########

ggplot(countryGaps_PrePost1990, aes(x = Pre.1990, y = Post.1990)) + 
  #geom_point(size = 5) + 
  geom_text(aes(label = countryLAC), size = 3) + 
  labs(title = "LAC productivity growth gap") +
  labs(subtitle = "Pre- and Post-1990") +
  labs( x= "Pre-1990 productivity growth gap", y = "Post-1990 productivity growth gap") + 
  geom_abline(slope = 1, linetype = 2) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) + 
  #xlim(-0.03,0.03) + 
  #ylim(-.03, 0.03) + 
  labs(color = "Legend") +
  coord_fixed() + 
  ggsave("figures/lacCountriesPrePost1990Gaps_LACPrePost1990regression_rollingWindows.pdf", width = 9, height = 6.5, units = "in") + 
  ggsave("figures/finalFiguresAndTables/Figure13.pdf",  width = 9, height = 6.5, units = "in")

countryGaps_lrtfpna_chg7 <- LACvsNonLAC_lrtfpna_chg7[ , .(metric = "Productivity", gapAdjusted = mean(na.omit(gap_adjusted)), gapAdjustedCountryDummies = mean(na.omit(gap_adjustedCountryDummies))), by = .(countryLAC)]
countryGaps_lrgdpnaPerCapita_chg7_nonTFP <- LACvsNonLAC_lrgdpnaPerCapita_chg7_nonTFP[ , .(metric = "Factors", gapAdjusted = mean(na.omit(gap_adjusted)), gapAdjustedCountryDummies = mean(na.omit(gap_adjustedCountryDummies))), by = .(countryLAC)]
countryGaps_lrgdpnaPerCapita_chg7 <- LACvsNonLAC_lrgdpnaPerCapita_chg7[ , .(metric = "Total", gapAdjusted = mean(na.omit(gap_adjusted)), gapAdjustedCountryDummies = mean(na.omit(gap_adjustedCountryDummies))), by = .(countryLAC)]

countryGaps <- rbind(countryGaps_lrtfpna_chg7, countryGaps_lrgdpnaPerCapita_chg7_nonTFP)

setkey(countryGaps,countryLAC)

setnames(countryGaps_lrgdpnaPerCapita_chg7,"gapAdjusted","Total")
setnames(countryGaps_lrgdpnaPerCapita_chg7,"gapAdjustedCountryDummies","Total_gapAdjusted_countryDummies")
countryGaps_lrgdpnaPerCapita_chg7[ , metric := NULL]

setkey(countryGaps_lrgdpnaPerCapita_chg7)

countryGaps <- countryGaps_lrgdpnaPerCapita_chg7[countryGaps]

countryGaps[ , metric := factor(metric, levels = c("Productivity","Factors"))]


########
#### Figure 12
########

ggplot( countryGaps, aes(x = reorder(countryLAC, Total_gapAdjusted_countryDummies))) + 
  geom_bar(stat = "identity", aes(y = gapAdjustedCountryDummies, fill = metric)) +
  geom_col(position = position_stack(reverse = FALSE), aes(y = gapAdjustedCountryDummies, fill = metric)) + 
  #geom_line(aes(x = alpha3, y = Total, group = 1), size = 4, linetype = "dashed", color = "Black") + 
  geom_point(aes(x = countryLAC, y = Total_gapAdjusted_countryDummies, shape = "Total"), size = 2, color = "black")  +
  xlab("\nCountry") + 
  ylab("Average shortfall\n") +  
  labs(title = "Decomposition of country per capita output growth shortfalls in LAC countries")+
  labs(subtitle = "1962-2017, annualized, %, computed based on country fixed effects from regression analysis.") + 
  scale_fill_discrete(name = "", labels = c("Productivity","Factors")) +
  scale_shape_discrete(name = "", labels = "Total") +
  #scale_linetype_discrete( name = "", labels = "Sum") + 
  coord_flip() +
  #scale_y_reverse() +
  theme(legend.position = "bottom") + 
  ggsave("figures/lacCountriesGaps_LACDummyregression_rollingWindows.pdf",plot = last_plot(), width = 9, height = 6.5, units = "in") + 
  ggsave("figures/finalFiguresAndTables/Figure12.pdf",plot = last_plot(), width = 9, height = 6.5, units = "in")


