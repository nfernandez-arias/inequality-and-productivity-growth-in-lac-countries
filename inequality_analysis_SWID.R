  
  
  
  library(data.table)
  library(readstata13)
  library(openxlsx)
  library(gridExtra)
  library(xtable)
  library(ggplot2)
  library(ggthemr)
  library(kableExtra)
  
  
  
  rm(list = ls())
  
  na.locf <- zoo::na.locf
  
  setwd("~/nfernand@princeton.edu/PhD - Thesis/Research/Inequality and productivity growth in LAC")
  
  ggthemr("flat")
  
  swiid <- fread("data/raw/swiid8_2_summary.csv")
  
  # Extrapolation
  
  pwt_91_cleaned <- 
  
  
  
  pwt_7 <- fread("data/pwt_7.csv")
  
  pwt_7_1990 <- fread("data/pwt_7_1990.csv")
  
  setkey(swiid,country,year)
  
  pwt_7[ , pseudoYear := pseudoYear - 6]
  setkey(pwt_7,country,pseudoYear)
  
  pwt_7 <- swiid[pwt_7]
  
  pwt_7[ , year := i.year]
  pwt_7[ , i.year := NULL]
  
  #pwt_7[ !is.na(gini_disp), hasGiniDisp := 1]
  #pwt_7[ is.na(gini_disp), hasGiniDisp := 0]
  
  #pwt_7[ !is.na(gini_mkt), hasGiniMkt := 1]
  #pwt_7[ is.na(gini_mkt), hasGiniMkt := 0]
  
  fwrite(pwt_7,"data/pwt_swiid.csv")
  
  
        
  
