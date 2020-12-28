


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


## World bank

worldbank <- fread("data/raw/gini_WB.csv", skip = 2)

setnames(worldbank,"Country Code","countrycode")
setnames(worldbank,"Country Name","country")
setkey(worldbank,countrycode,Year)

setnames(worldbank,"Value","Gini")

worldbank[ Gini == "..", Gini := NA ]

worldbank[ , Gini := as.numeric(Gini)]

worldbank[ !is.na(Gini), hasData := 1]
worldbank[ is.na(hasData), hasData := 0]

worldbank[ , cumHasData := cumsum(hasData), by = countrycode]

aggYear_wb <- worldbank[ , .(percentHasData = 100* sum(hasData) / .N), by = Year]

p1 <- ggplot(aggYear_wb, aes(x = Year, y = percentHasData)) + 
  geom_line() +
  ylim(0,100) + 
  ylab("% of countries with data") +
  labs(title = "Gini coefficient data coverage",
       subtitle = "World Bank data (all countries)")

ggplot(worldbank, aes(x = Year, y = cumHasData)) + 
  geom_line() +
  facet_wrap( ~country) + 
  ylab("Cumulative years with data") + 
  labs(title = "Gini coefficient data coverage (by country)",
       subtitle = "World bank data (all countries)") + 
  theme(axis.text.x = element_text(angle = 90))


ggsave("figures/inequality/gini_coverage_wb_countries.pdf",plot = last_plot(),  width = 18, height = 12, units = "in")


# OECD

oecd <- fread("data/raw/gini_OECD.csv", skip = 3)
oecd <- head(oecd,-3)

names(oecd) <- c("Country","Year","Methodology","Gini")

oecd[ Country == "", Country := NA]
oecd[ , Country := na.locf(Country, na.rm = FALSE)]

oecd[ Gini == "..", Gini := NA]
oecd[ , Gini := as.numeric(Gini)]
oecd[ , Gini := na.locf(Gini, na.rm = FALSE), by = Country]

oecd[ , Year := as.integer(Year)]
oecd[ , Year := na.locf(Year, na.rm = FALSE), by = Country]

setkey(oecd,Country,Year)


oecd[ !is.na(Gini), hasData := 1]
oecd[ is.na(hasData), hasData := 0]
aggYear_oecd <- oecd[ , .(percentHasData = 100* sum(hasData) / .N), by = Year]


oecd[ , cumHasData := cumsum(hasData), by = Country]
ggplot(oecd, aes(x = Year, y = cumHasData)) + 
  geom_line() +
  facet_wrap( ~Country) + 
  xlim(1960,2020) +
  ylab("Cumulative years with data") + 
  labs(title = "Gini coefficient data coverage (by country)",
       subtitle = "OECD data (OECD countries)") + 
  theme(axis.text.x = element_text(angle = 90))


ggsave("figures/inequality/gini_coverage_oecd_countries.pdf",plot = last_plot(),  width = 14, height = 10, units = "in")



p2 <- ggplot(aggYear_oecd, aes(x = Year, y = percentHasData)) + 
  geom_line() +
  xlim(1960,2020) +
  ylab("% of countries with data") +
  labs(title = "Gini coefficient data coverage",
       subtitle = "OECD data (OECD countries)")

p <- grid.arrange(p1,p2)


ggsave("figures/inequality/gini_coverage.pdf",plot = p,  width = 9, height = 6.5, units = "in")



# World inequality database

wineq <- fread("data/raw/WID_SummaryTable_24May2019.csv")



