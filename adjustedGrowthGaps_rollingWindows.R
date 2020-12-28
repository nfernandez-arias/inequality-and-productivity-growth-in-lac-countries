


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

ggthemr("flat")



data <- fread("data/pwt91_cleaned_swiid.csv")

setkey(data,alpha3,year)

# Start in 1968, since using 7-year windows
data <- data[ year >= 1968]

# Clean up names for easier reading
data[ subRegion_new == "Africa", subRegion_new := "AFR"]
data[ subRegion_new == "East Asian Tigers", subRegion_new := "EAT"]
data[ subRegion_new == "United States", subRegion_new := "USA"]
data[ subRegion_new == "Latin America and the Caribbean", subRegion_new := "LAC"]

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
  return(temp)
  
}

computeRawGaps_LACvsRegion <- function(data, metric, region) {

  temp <- data[ subRegion_new %in% c("LAC",region), .(average = mean(na.omit(get(metric)))), by = .(isLAC, year)]
  temp <- dcast(temp, year ~ isLAC, value.var = "average")
  setnames(temp,"0",region)
  setnames(temp,"1","LAC")
  temp[ , (paste0(region,"_",metric,"_gap")) := LAC - get(region)]
  return(temp)

}

LACvsNonLAC_TFP <- computeRawGaps_LACvsNonLAC(data,"lrtfpna_chg7")
LACvsNonLAC_NonTFP <- computeRawGaps_LACvsNonLAC(data,"lrgdpnaPerCapita_chg7_nonTFP")
LACvsNonLAC_Total <- computeRawGaps_LACvsNonLAC(data,"lrgdpnaPerCapita_chg7")

for (region in c("AFR","EAT","USA")) 
{
  for (metric in c("lrtfpna_chg7","lrgdpnaPerCapita_chg7_nonTFP","lrgdpnaPerCapita_chg7"))
  {
    #print(comparisonRegion)
    #print(comparisonMetric)
    assign(paste0("LACvs",region,"_",metric), computeRawGaps_LACvsRegion(data,metric,region))
  }
}






