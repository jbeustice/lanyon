---
layout: page
title: Coding
---

My language of choice for scripting and analysis is R; however, I have used and am familiar with many other statistical/scripting languages/programs (i.e. Stata, SAS, etc.).

Currently, I am familiarizing myself with Python.

## Projects

The following projects can be found on my GitHub [account](https://github.com/jbeustice/pastCode).

* Text-based Yahtzee [using C]
* One player text-based Battleship [using C]
* Randomely generated math problems for elementary school kids [using C]
* Example of a circular linked list [using C++]
* Threading example [using C++]

## Code snippets

The following R code reshapes and merges multiple files into one longitudinal dataset.
``` sh
## This script reads in multiple files, reshapes the data in
## panel format, and merges all data into one dataset for
## further analysis

setwd("/Users/Bradley/Dropbox/...")

library(reshape2)

# load variables
hp <- read.csv("MedianPricesA.csv")
cpi <- read.csv("CPI_A.csv")
fips <- read.csv("fips.csv")
hi <- read.csv("HouseholdIncomeA.csv")
hu <- read.csv("HousingUnitsA.csv")
popD <- read.csv("PopulationDensityA.csv")
unemp <- read.csv("unemploymentA.csv")

# reshape data to panel format
hp <- melt(hp,id.vars="COUNTY")
hp <- hp[order(hp$COUNTY),]
colnames(hp) <- c("COUNTY","YEAR","PRICE")
hi <- melt(hi,id.vars="COUNTY")
hi <- hi[order(hi$COUNTY),]
colnames(hi) <- c("COUNTY","YEAR","INCOME")
hu <- melt(hu,id.vars="COUNTY")
hu <- hu[order(hu$COUNTY),]
colnames(hu) <- c("COUNTY","YEAR","UNITS")
popD <- melt(popD,id.vars="COUNTY")
popD <- popD[order(popD$COUNTY),]
colnames(popD) <- c("COUNTY","YEAR","POP_DENS")
unemp <- melt(unemp,id.vars="COUNTY")
unemp <- unemp[order(unemp$COUNTY),]
colnames(unemp) <- c("COUNTY","YEAR","UNEMPLOYMENT")

# merge data
agg <- merge(hp,cpi,by="YEAR")
agg <- merge(agg,fips,by="COUNTY")
agg <- merge(agg,hi,by=c("COUNTY","YEAR"))
agg <- merge(agg,hu,by=c("COUNTY","YEAR"))
agg <- merge(agg,popD,by=c("COUNTY","YEAR"))
agg <- merge(agg,unemp,by=c("COUNTY","YEAR"))

# export
write.csv(agg,"dataA.csv",row.names=F)
```

The following .do file [Stata] merges datasets together to create a contiguity weighting matrix used in spatial analysis.
``` sh
// This script creates a contiguity weighting matrix by
// merging the original dataset with the US Census TIGER
// shapefile

import delimited "/Users/Bradley/Dropbox/..."

//  unzip TIGER file and generate fips pre merge
copy ~/Downloads/tl_2016_us_county.zip .
unzipfile tl_2016_us_county.zip
spshape2dta tl_2016_us_county
use tl_2016_us_county, clear
describe
list in 1/2
generate long fips = real(STATEFP + COUNTYFP)
bysort fips: assert _N==1
assert fips != .
spset fips, modify replace
list in 1/2
save, replace

// format original data pre merge
format year %tq
assert fips!=.
assert year!=.
bysort fips year: assert _N==1
xtset, clear
xtset fips year
spbalance
spset fips
save "/Users/Bradley/Dropbox/..."

// merge orignial data with TIGER by fips
use dataA, clear
describe
merge m:1 fips using tl_2016_us_county
keep if _merge==3
drop _merge
drop STATEFP COUNTYFP COUNTYNS GEOID NAME NAMELSAD LSAD CLASSFP
drop MTFCC CSAFP CBSAFP METDIVFP FUNCSTAT ALAND AWATER INTPTLAT INTPTLON
save "/Users/Bradley/Dropbox/..."

// create contiguity matrix
spmatrix create contiguity W if year == 2007
```
