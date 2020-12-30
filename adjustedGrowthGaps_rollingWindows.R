


rm(list = ls())

library(data.table)
library(readstata13)
library(openxlsx)
library(gridExtra)
library(xtable)
library(ggplot2)
library(ggthemr)
library(ggpubr)
library(kableExtra)
library(plm)

setwd("/home/nico/Insync/nfernand@princeton.edu/Google Drive/PhD - Thesis/Research/inequality-and-convergence-in-lac")

ggthemr(palette = "flat", layout = "scientific", type = "inner", spacing = 0.9)

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
  
  temp <- data[ , .(average = mean(na.omit(get(metric)))), by = .(isLAC, year)]
  temp <- dcast(temp, year ~ isLAC, value.var = "average")
  setnames(temp,"0","non.LAC")
  setnames(temp,"1","LAC")
  temp[ , gap := LAC - non.LAC]
  return(temp[ , .(year,gap)])
  
}

computeRawGaps_LACvsRegion <- function(data, tempMetric, tempRegion) {

  temp <- data[ subRegion_new %in% c("LAC",tempRegion), .(average = mean(na.omit(get(tempMetric)))), by = .(isLAC, year)]
  temp <- dcast(temp, year ~ isLAC, value.var = "average")
  setnames(temp,"0",tempRegion)
  setnames(temp,"1","LAC")

  temp[ , gap := LAC - get(tempRegion)]
  return(temp[ , .(year,gap)])

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
  
  model <- plm(get(tempMetric) ~ lcgdpoPerCapita_gap_avg7, data, index = c("alpha3","year"), effect = "time", model = "within")
  modelRobust <- plm(get(tempMetric) ~ lcgdpoPerCapita_gap_avg7, data[isLAC == 0], index = c("alpha3","year"), effect = "time", model = "within")
  
  temp <- data[ , .(adjustment = mean(na.omit(lcgdpoPerCapita_gap_avg7 * model$coefficients[1]))), by = .(isLAC,year)]
  tempRobust <- data[ , .(adjustmentRobust = mean(na.omit(lcgdpoPerCapita_gap_avg7 * modelRobust$coefficients[1]))), by = .(isLAC,year)]
  
  setkey(temp,isLAC,year)
  setkey(tempRobust,isLAC,year)
  
  temp <- tempRobust[temp]
  
  temp <- dcast(temp, year ~ isLAC, value.var = c("adjustment","adjustmentRobust"))
  setnames(temp,"adjustment_0","adjustment.non.LAC")
  setnames(temp,"adjustment_1","adjustment.LAC")
  setnames(temp,"adjustmentRobust_0","adjustmentRobust.non.LAC")
  setnames(temp,"adjustmentRobust_1","adjustmentRobust.LAC")
  
  temp[ , baselineGapAdjustment := adjustment.LAC - adjustment.non.LAC]
  temp[ , robustGapAdjustment := adjustmentRobust.LAC - adjustmentRobust.non.LAC]
    
  return(temp)
  
}

computeAdjustments_LACvsRegion <- function(data, tempMetric, tempRegion) {
  
  model <- plm(get(tempMetric) ~ lcgdpoPerCapita_gap_avg7, data, index = c("alpha3","year"), effect = "time", model = "within")
  modelRobust <- plm(get(tempMetric) ~ lcgdpoPerCapita_gap_avg7, data[isLAC == 0], index = c("alpha3","year"), effect = "time", model = "within")
    
  temp <- data[ subRegion_new %in% c("LAC",tempRegion) , .(adjustment = mean(na.omit(lcgdpoPerCapita_gap_avg7 * model$coefficients[1]))), by = .(isLAC,year)]
  tempRobust <- data[ subRegion_new %in% c("LAC",tempRegion) , .(adjustmentRobust = mean(na.omit(lcgdpoPerCapita_gap_avg7 * modelRobust$coefficients[1]))), by = .(isLAC,year)]
  
  setkey(temp,isLAC,year)
  setkey(tempRobust,isLAC,year)
  
  temp <- tempRobust[temp]
  
  temp <- dcast(temp, year ~ isLAC, value.var = c("adjustment","adjustmentRobust"))
  setnames(temp,"adjustment_0",paste0("adjustment.",tempRegion))
  setnames(temp,"adjustment_1","adjustment.LAC")
  setnames(temp,"adjustmentRobust_0",paste0("adjustmentRobust.",tempRegion))
  setnames(temp,"adjustmentRobust_1","adjustmentRobust.LAC")
  temp[ , baselineGapAdjustment := adjustment.LAC - get(paste0("adjustment.",tempRegion))]
  temp[ , robustGapAdjustment := adjustmentRobust.LAC - get(paste0("adjustmentRobust.",tempRegion))]
  
  return(temp[ , .(year,baselineGapAdjustment,robustGapAdjustment)])
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





# Now compute gaps relative to individual regions

for (metric in c("lrtfpna_chg7","lrgdpnaPerCapita_chg7_nonTFP","lrgdpnaPerCapita_chg7")) 
{

  tempStringRaw <- paste0("LACvsNonLAC_",metric)
  tempStringAdjustments <- paste0("LACvsNonLAC_",metric,"_adjustments")
  
  assign(tempStringRaw, computeRawGaps_LACvsNonLAC(data,metric))
  assign(tempStringAdjustments, computeAdjustments_LACvsNonLAC(data,metric))
  
  setkey(get(tempStringRaw), year)
  setkey(get(tempStringAdjustments), year)
  
  assign(tempStringRaw, get(tempStringAdjustments)[get(tempStringRaw)])
  
  get(tempStringRaw)[ , gap_adjusted := gap - baselineGapAdjustment]
  get(tempStringRaw)[ , gap_adjustedRobust := gap - robustGapAdjustment]
    
  assign(tempStringRaw, get(tempStringRaw)[ , .(year, gap, gap_adjusted, gap_adjustedRobust)])
    
  for (region in c("AFR","EAT","USA"))
  {
    tempStringRaw <- paste0("LACvs",region,"_",metric)
    tempStringAdjustments <- paste0("LACvs",region,"_",metric,"_adjustments")
    assign(tempStringRaw, computeRawGaps_LACvsRegion(data,metric,region))
    assign(tempStringAdjustments, computeAdjustments_LACvsRegion(data,metric,region))
    
    setkey(get(tempStringRaw),year)
    setkey(get(tempStringAdjustments),year)
    
    assign(tempStringRaw , get(tempStringAdjustments)[get(tempStringRaw)])
    
    get(tempStringRaw)[ , gap_adjusted := gap - baselineGapAdjustment]
    get(tempStringRaw)[ , gap_adjustedRobust := gap - robustGapAdjustment]
    
    assign(tempStringRaw, get(tempStringRaw)[ , .(year, gap, gap_adjusted ,gap_adjustedRobust)])
      
  }
}

LACvsNonLAC_lrtfpna_chg7[ , metric := "Productivity"]
LACvsNonLAC_lrgdpnaPerCapita_chg7_nonTFP[ , metric := "Factors"]
LACvsNonLAC_lrgdpnaPerCapita_chg7[ , metric := "Output per capita"]

LACvsNonLAC <- rbind(LACvsNonLAC_lrtfpna_chg7,LACvsNonLAC_lrgdpnaPerCapita_chg7_nonTFP,LACvsNonLAC_lrgdpnaPerCapita_chg7)

LACvsNonLAC[ , metric := factor(metric, levels = c("Output per capita","Productivity","Factors"))]

ggplot(LACvsNonLAC, aes(x = year-3)) + 
  geom_line( aes(y = gap, linetype = "gap")) + 
  geom_line( aes(y = gap_adjusted, linetype = "gap_adjusted")) + 
  geom_line( aes(y = gap_adjustedRobust, linetype = "gap_adjustedRobust")) + 
  ylab("Annual growth rate (%)") +
  xlab("Year") + 
  labs(title = "LAC vs non-LAC growth gaps, raw and adjusted for stage of development (baseline and robust)") + 
  scale_linetype_discrete(name = "", labels = c("Raw","Adjusted","Adjusted (robust)")) + 
  theme(legend.position = "bottom") + 
  facet_wrap(~metric, nrow = 3) +
  #theme_minimal() + 
  ggsave("figures/LAC_nonLAC_gaps_plots.pdf", width = 12, height = 9, units = "in")
  




LACvsEAT_adjusted <- computeAdjustments_LACvsRegion(data,"lrtfpna_chg7","EAT")
LACvsAFR_adjusted <- computeAdjustments_LACvsRegion(data,"lrtfpna_chg7","AFR")
LACvsUSA_adjusted <- computeAdjustments_LACvsRegion(data,"lrtfpna_chg7","USA")
LACvsNonLAC_adjusted <- computeAdjustments_LACvsNonLAC(data,"lrtfpna_chg7")

