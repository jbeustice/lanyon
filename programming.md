---
layout: page
title: Programming
---

My language of choice for scripting and analysis is R; however, I have used and am familiar with many other statistical/scripting languages and programs (i.e., Stata, SAS, etc.). Two of my favorite, yet somewhat tangential R packages are [progress](https://cran.r-project.org/web/packages/progress/index.html) and [tikzDevice](https://cran.r-project.org/web/packages/tikzDevice/index.html).

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
```r
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
```
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

The following R code fits data to a lognormal distribution and then samples from the fitted distribution to find the first three moments (and the median).

```r
## This program fits a log-normal distribution to sample data based on outcomes 
## and quantiles. Simulates two-state models through 10,000 samples. Computes
## the mean, median, variance, and skewness.

setwd("/Users/Bradley/Dropbox/...")

library(progress)
library(rriskDistributions)
library(moments)

sampleData <- read.csv("meanVarData.csv",header=TRUE)
closure <- c(.2556,1-.2556) # closed, open
quantilesNear <- c(0.2,0.5,0.8)
obs <- nrow(sampleData)

# fits 3 quantiles to log-normal distribution
fitLog <- function(info,numObs,quant,tolerance){
  output <- matrix(NA,numObs,2)
  for(i in 1:numObs){
    temp <- get.lnorm.par(p=quant,q=c(info[i,1],info[i,2],info[i,3])
                          ,show.output=F,plot=F,tol=tolerance)
    output[i,1] <- temp[[1]]
    if(is.na(output[i,1])){
      output[i,2] <- NA
    }
    else{
      output[i,2] <- temp[[2]]
    }
  }
  output
}

# returns which observations could not be fit to a log-normal distribution
# i.e., does not meet tolerance level
meetTol <- function(params){
  output <- c()
  for(i in 1:obs){
    if(is.na(params[i])){
      output <- c(output,i)
    }
  }
  output
}

log1 <- fitLog(two[,2:4],obs,quantilesNear,0.01)
log2 <- fitLog(two[,5:7],obs,quantilesNear,0.01)

meetTol(log1)
meetTol(log2)

# drop the invalid observations for the whole dataset
drop <- c(meetTol(log1),meetTol(log2))
log1Drop <- log1[-drop,]
log2Drop <- log2[-drop,]
sampleDataDrop <- sampleData[-drop,]
obs <- nrow(routes)

# simulates log-normal samples for a two-state model
# and outputs the first 3 moments and the median
sampleDistPass <- function(logDroppedOpen,logDroppedClosed,size){
  output <- matrix(NA,obs,4)
  pb <- progress_bar$new(total = obs)
  for(i in 1:obs){
    hold <- vector(mode="numeric",length=size)
    for(j in 1:size){
      rv <- runif(1,0,1)
      if(rv<closure[2]){
        hold[j] <- rlnorm(1,logDroppedOpen[i,1],logDroppedOpen[i,2])
      }
      else{
        hold[j] <- rlnorm(1,logDroppedClosed[i,1],logDroppedClosed[i,2])
      }
    }
    output[i,1] <- mean(hold)
    output[i,2] <- median(hold)
    output[i,3] <- var(hold)
    output[i,4] <- skewness(hold)
  
    pb$tick()
  }
  output
}

results <- sampleDistPass(log1Drop,log2Drop,10000)
```
