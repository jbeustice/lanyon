---
layout: page
title: Job Market Paper
---

My job market paper is similar to [this](https://www.youtube.com/watch?v=QR10t-B9nYY).

My job market paper looks at how/if reference points (from Cumulative Prospect Theory) are updated within periods, i.e., after an event takes place, but before the outcome is realized. Transportation data is perfect to answer this question. Drivers choose a route or leave for a destination, but don't fully realize the outcome untill final ariival at their respective destinations.

> The defining feature separating (cumulative) prospect theory from expected utility theory is that potential outcomes are measured relative to a reference point as opposed to final assets. Determining the reference point, therefore, is vital to correct analysis. While many theories and assumptions have been made concerning reference point up- dating between repeated choices, few papers have looked at reference point updating within periods. This paper seeks to find if and when drivers change their reference point in a transportation setting when faced with an unexpected delay en route. Drivers are more likely to change their reference point if the unexpected delay occurs near the end- points of travel.

Download a preview of my job market paper [here]().

## Supplemental Material

The following R code calls the Google Maps Distance Matrix API to return the duration from Snoqualmie Pass to each reported destination.
``` sh
## This program finds the duration, in minutes, and distance, in miles,
## to all reported I-90 destinations from Snoqualmie Pass given a
## departure time via the Google Maps Distance Matrix API

setwd("/Users/Bradley/Dropbox/...")

library(bitops)
library(httr)
library(XML)
library(RCurl)
library(rJava)
library(xlsxjars)
library(xlsx)
library(progress)

# read in data
input <- read.csv("inputRoutes.csv",header=F)
allData <- as.matrix(input)
colnames(allData) <- NULL
numRow <- nrow(allData)
snoqPass <- "Snoqualmie+Pass+WA"
drive <- "best_guess"
key <- "sign_up_for_a_key"

# takes the prepared city+state data and returns duration from origin to SP
originSP <- matrix(nrow=numRow,ncol=1)
pb <- progress_bar$new(total = numRow)
for(i in 1:numRow){
  k <- 0
  while(k<4){
    url <- paste0('https://maps.googleapis.com/maps/api/distancematrix/xml?origins=',allData[i,2],
                  '&destinations=',snoqPass,'&units=imperial')
    Sys.sleep(1)
    tie <- xmlParse(GET(url))
    tryCatch(originSP[i,1] <- as.numeric(xpathApply(tie,"//duration/value",xmlValue))/60, error=function(e) NULL)
    k <- k+1
  }
  pb$tick()
}

# takes the prepared city+state data and returns duration from SP to destination
SPdest <- matrix(nrow=numRow,ncol=1)
pb <- progress_bar$new(total = numRow)
for(i in 1:numRow){
  k <- 0
  while(k<4){
    url <- paste0('https://maps.googleapis.com/maps/api/distancematrix/xml?origins=',snoqPass,
                  '&destinations=',allData[i,3],'&units=imperial')
    Sys.sleep(1)
    tie <- xmlParse(GET(url))
    tryCatch(SPdest[i,1] <- as.numeric(xpathApply(tie,"//duration/value",xmlValue))/60, error=function(e) NULL)
    k <- k+1
  }
  pb$tick()
}

# takes the prepared city+state data and returns duration from Snoqualmie Pass given departure time
SPhour <- matrix(nrow=numRow,ncol=1)
pb <- progress_bar$new(total = numRow)
for(i in 1:numRow){
  k <- 0
  while(k<4){
    url <- paste0('https://maps.googleapis.com/maps/api/distancematrix/xml?origins=',snoqPass,
                  '&destinations=',allData[i,3],'&units=imperial&departure_time=',allData[i,4],
                  '&traffic_model=',drive,'&key=',key)
    Sys.sleep(1)
    tie <- xmlParse(GET(url))
    tryCatch(SPhour[i,1] <- as.numeric(xpathApply(tie,"//duration_in_traffic/value",xmlValue))/60, error=function(e) NULL)
    k <- k+1
  }
  pb$tick()
}

# export to csv file
originalRoute <- cbind(allData,originSP,SPdest,SPhour)
colnames(originalRoute) <- c("west","origin","destination","epochtime","orginSP","SPdest","SPhour")
write.csv(originalRoute,file="originalRoute.csv")
```

The following R code calls the Google Maps Distance Matrix API to return the duration from Snoqualmie Pass to each reported destination over the 3 potential reroute options. Values returned by the Google Maps API include 3 different durations: pessimistic guess, best guess, and optimistic guess. The Google Maps API is called over 10,000 times.
``` sh
## This program finds the duration, in minutes, to all reported I-90 
## destinations from Snoqualmie Pass over three alternatives (2, 12, 84)
## given a departure time via the Google Maps Distance Matrix API

setwd("/Users/Bradley/Dropbox/...")

library(bitops)
library(httr)
library(XML)
library(RCurl)
library(rJava)
library(xlsxjars)
library(xlsx)
library(progress)

# read in data
input <- read.csv("inputRoutes.csv",header=F)
allData <- as.matrix(input)
colnames(allData) <- NULL
numRow <- nrow(allData)
penalty <- 10050
snoqPass <- "SNOQUALMIE+PASS+WA"
stevPass <- "STEVENS+PASS+WA"
drive <- c("pessimistic","best_guess","optimistic")
key <- "sign_up_for_a_key"

# routes from Snoqualmie Pass over wavepoints to final destination
f.wavepoint <- function(wavepointA,wavepointB,speed){
  results.A <- matrix(nrow=numRow,ncol=1)
  results.B <- matrix(nrow=numRow,ncol=1)
  results.C <- matrix(nrow=numRow,ncol=1)
  results.out <- matrix(nrow=numRow,ncol=1)
  pb <- progress_bar$new(total = numRow)
  for(i in 1:numRow){
    tempTime <- 0
    
    # routes from SP to first wavepoint
    k <- 0
    while(is.na(results.A[i]) && k<10){
      if(allData[i,1] == 1){
        url <- paste0('https://maps.googleapis.com/maps/api/distancematrix/xml?origins=',
                      snoqPass,'&destinations=',wavepointA,'&units=imperial&departure_time=',
                      allData[i,4],'&traffic_model=',speed,'&key=',key)
      }
      else{
        url <- paste0('https://maps.googleapis.com/maps/api/distancematrix/xml?origins=',
                      snoqPass,'&destinations=',wavepointB,'&units=imperial&departure_time=',
                      allData[i,4],'&traffic_model=',speed,'&key=',key)
      }
      Sys.sleep(0.3)
      tie <- xmlParse(GET(url))
      tryCatch(results.A[i] <- as.numeric(xpathApply(tie,"//duration_in_traffic/value",xmlValue)),error=function(e) NULL)
      k <- k+1
    }
    tempTime <- as.numeric(allData[i,4]) + results.A[i]
    
    # routes from first wavepoint to second wavepoint
    k <- 0
    while(is.na(results.B[i]) && k<10){
      if(allData[i,1] == 1){
        url <- paste0('https://maps.googleapis.com/maps/api/distancematrix/xml?origins=',
                      wavepointA,'&destinations=',wavepointB,'&units=imperial&departure_time=',
                      tempTime,'&traffic_model=',speed,'&key=',key)
      }
      else{
        url <- paste0('https://maps.googleapis.com/maps/api/distancematrix/xml?origins=',
                      wavepointB,'&destinations=',wavepointA,'&units=imperial&departure_time=',
                      tempTime,'&traffic_model=',speed,'&key=',key)
      }
      Sys.sleep(0.3)
      tie <- xmlParse(GET(url))
      tryCatch(results.B[i] <- as.numeric(xpathApply(tie,"//duration_in_traffic/value",xmlValue)), error=function(e) NULL)
      k <- k+1
    }
    tempTime <- tempTime + results.B[i]
    
    # routes from second wavepoint to final destination
    k <- 0
    while(is.na(results.C[i]) && k<10){
      if(allData[i,1] == 1){
        url <- paste0('https://maps.googleapis.com/maps/api/distancematrix/xml?origins=',
                      wavepointB,'&destinations=',allData[i,3],'&units=imperial&departure_time=',
                      tempTime,'&traffic_model=',speed,'&key=',key)
      }
      else{
        url <- paste0('https://maps.googleapis.com/maps/api/distancematrix/xml?origins=',
                      wavepointA,'&destinations=',allData[i,3],'&units=imperial&departure_time=',
                      tempTime,'&traffic_model=',speed,'&key=',key)
      }
      Sys.sleep(0.3)
      tie <- xmlParse(GET(url))
      tryCatch(results.C[i] <- as.numeric(xpathApply(tie,"//duration_in_traffic/value",xmlValue)), error=function(e) NULL)
      k <- k+1
    }
    
    # output
    results.out[i] <- (results.A[i] + results.B[i] + results.C[i])/60
    pb$tick()
  }
  results.out
}

# routes from Snoqualmie Pass over wavepoints to final destination
f.wavepointPLUS <- function(wavepointA,wavepointB,speed){
  results.A <- matrix(nrow=numRow,ncol=1)
  results.B <- matrix(nrow=numRow,ncol=1)
  results.C <- matrix(nrow=numRow,ncol=1)
  results.D <- matrix(nrow=numRow,ncol=1)
  results.out <- matrix(nrow=numRow,ncol=1)
  pb <- progress_bar$new(total = numRow)
  for(i in 1:numRow){
    
    # routes from SP to first wavepoint
    k <- 0
    while(is.na(results.A[i]) && k<10){
      if(allData[i,1] == 1){
        url <- paste0('https://maps.googleapis.com/maps/api/distancematrix/xml?origins=',
                      snoqPass,'&destinations=',wavepointA,'&units=imperial&departure_time=',
                      allData[i,4],'&traffic_model=',speed,'&key=',key)
      }
      else{
        url <- paste0('https://maps.googleapis.com/maps/api/distancematrix/xml?origins=',
                      snoqPass,'&destinations=',wavepointB,'&units=imperial&departure_time=',
                      allData[i,4],'&traffic_model=',speed,'&key=',key)
      }
      Sys.sleep(0.3)
      tie <- xmlParse(GET(url))
      tryCatch(results.A[i] <- as.numeric(xpathApply(tie,"//duration_in_traffic/value",xmlValue)), error=function(e) NULL)
      k <- k+1
    }
    tempTime <- as.numeric(allData[i,4]) + results.A[i]
    
    # routes from first wavepoint to Stevens Pass
    k <- 0
    while(is.na(results.B[i]) && k<10){
      if(allData[i,1] == 1){
        url <- paste0('https://maps.googleapis.com/maps/api/distancematrix/xml?origins=',
                      wavepointA,'&destinations=',stevPass,'&units=imperial&departure_time=',
                      tempTime,'&traffic_model=',speed,'&key=',key)
      }
      else{
        url <- paste0('https://maps.googleapis.com/maps/api/distancematrix/xml?origins=',
                      wavepointB,'&destinations=',stevPass,'&units=imperial&departure_time=',
                      tempTime,'&traffic_model=',speed,'&key=',key)
      }
      Sys.sleep(0.3)
      tie <- xmlParse(GET(url))
      tryCatch(results.B[i] <- as.numeric(xpathApply(tie,"//duration_in_traffic/value",xmlValue)), error=function(e) NULL)
      k <- k+1
    }
    tempTime <- tempTime + results.B[i] + penalty
    
    # routes from Stevens Pass to second wavepoint
    k <- 0
    while(is.na(results.C[i]) && k<10){
      if(allData[i,1] == 1){
        url <- paste0('https://maps.googleapis.com/maps/api/distancematrix/xml?origins=',
                      stevPass,'&destinations=',wavepointB,'&units=imperial&departure_time=',
                      tempTime,'&traffic_model=',speed,'&key=',key)
      }
      else{
        url <- paste0('https://maps.googleapis.com/maps/api/distancematrix/xml?origins=',
                      stevPass,'&destinations=',wavepointA,'&units=imperial&departure_time=',
                      tempTime,'&traffic_model=',speed,'&key=',key)
      }
      Sys.sleep(0.3)
      tie <- xmlParse(GET(url))
      tryCatch(results.C[i] <- as.numeric(xpathApply(tie,"//duration_in_traffic/value",xmlValue)), error=function(e) NULL)
      k <- k+1
    }
    tempTime <- tempTime + results.C[i]
    
    # routes from second wavepoint to final destination
    k <- 0
    while(is.na(results.D[i]) && k<10){
      if(allData[i,1] == 1){
        url <- paste0('https://maps.googleapis.com/maps/api/distancematrix/xml?origins=',
                      wavepointB,'&destinations=',allData[i,3],'&units=imperial&departure_time=',
                      tempTime,'&traffic_model=',speed,'&key=',key)
      }
      else{
        url <- paste0('https://maps.googleapis.com/maps/api/distancematrix/xml?origins=',
                      wavepointA,'&destinations=',allData[i,3],'&units=imperial&departure_time=',
                      tempTime,'&traffic_model=',speed,'&key=',key)
      }
      Sys.sleep(0.3)
      tie <- xmlParse(GET(url))
      tryCatch(results.D[i] <- as.numeric(xpathApply(tie,"//duration_in_traffic/value",xmlValue)), error=function(e) NULL)
      k <- k+1
    }
    
    results.out[i] <- (results.A[i] + results.B[i] + penalty + results.C[i] + results.D[i])/60
    pb$tick()
  }
  results.out
}


for(j in 1:3){
  route2 <- f.wavepoint("Leavenworth+WA","Sultan+WA",drive[j])
  route2plus <- f.wavepointPLUS("Leavenworth+WA","Sultan+WA",drive[j])
  route12 <- f.wavepoint("Gleed+WA","Fern+Gap+WA",drive[j])
  route84 <- f.wavepoint("Goldendale+WA","Rooster+Rock+Park+OR",drive[j])
  
  altRoutes <- cbind(allData,route2,route2plus,route12,route84)
  colnames(altRoutes) <- c("west","origin","destination","epochtime","route2","route2plus","route12","route84")
  write.xlsx(altRoutes,file="altRoutes.xlsx",sheetName=drive[j],col.names=T,row.names=F,append=T)
  
  route2 <- NULL
  route2plus <- NULL
  route12 <- NULL
  route84 <- NULL
}
```

The following R code calculates the prospect maximizing route for 1600+ combinations of the CPT parameters in the loss domain using the preferred route as the reference point.
```sh
## This program calculates the prospect of each route from the SP reference point
## for multiple values of lambda, beta, and delta and returns which combinations
## predict the stated preference of drivers.

ptm <- proc.time()

setwd("/Users/Bradley/Dropbox/...")

library(progress)
library(tikzDevice)
library(ggplot2)

routes <- read.csv("prospectData.csv",header=TRUE)
two <- routes[,1:7] # {west,short,middle,long,shortP,middleP,longP}
twelve <- routes[,c(8:10)] # {short,middle,long}
eightfour <- routes[,11:13] # {short,middle,long}
snoqpass <- routes[,14] # {middle}
statedPref <- data.matrix(routes[15])
routeDuration <- data.matrix(routes[16])
routeCompletion <- data.matrix(routes[17])
routeDeviation <- data.matrix(routes[18])
routeDeviationSP <- data.matrix(routes[19])
outOfOrder1 <- data.matrix(routes[20])
outOfOrder2 <- data.matrix(routes[21])
others <- routes[,22:34]
probGoogle <- c(.1,.8,.1) # optimistic, best_guess, pessimistic,
prob2passE <- c(.27,1-.27)
prob2passW <- c(.2556,1-.2556)

# outcome in order {longP, middleP, shortP, long, middle, short}
f.probability <- function(p1,p2){
  output <- array(dim=6)
  output[1] <- p1[3]*p2[1]
  output[2] <- p1[2]*p2[1]
  output[3] <- p1[1]*p2[1]
  output[4] <- p1[3]*p2[2]
  output[5] <- p1[2]*p2[2]
  output[6] <- p1[1]*p2[2]
  output
}

prob2E <- f.probability(probGoogle,prob2passE)
prob2W <- f.probability(probGoogle,prob2passW)
prob12 <- probGoogle
prob84 <- probGoogle

beta <- seq(0.3,1,0.1)
lambda <- seq(1,15,0.5)
delta <- seq(0.4,1,0.1)
combos <- length(beta)*length(lambda)*length(delta)
combos

# calculates whether or not RP was changed under a varieity of parameter
# values (via results of CPT) and reutrns number of times RP changed
f.reference <- function(ref){
  num <- length(ref)
  output <- matrix(0,num,1)
  size <- length(beta)*length(lambda)*length(delta)*length(statedPref)
  pb <- progress_bar$new(total = size)
  for(k in 1:num){
    for(i in 1:length(lambda)){
      for(j in 1:length(beta)){
        for(m in 1:length(delta)){
          
          # weighting function based on delta
          f.cWeight <- function(p1,p2){
            first <- (p1^delta[m]) / (p1^delta[m] + (1-p1)^delta[m])^(1/delta[m])
            second <- (p2^delta[m]) / (p2^delta[m] + (1-p2)^delta[m])^(1/delta[m])
            first-second
          }
          
          # calc route 2 prospect
          out2 <- 0
          temp2 <- c(0,0,0,0,0,0)
          cWeight2 <- c(0,0,0,0,0,0)
          if(outOfOrder1[k]==1){ # original mixes with penalty in ordering
            if(two[k,1]==1){ # west
              cWeight2[1] <- f.cWeight(prob2W[1],0)
              cWeight2[2] <- f.cWeight(sum(prob2W[1:2]),prob2W[1])
              cWeight2[3] <- f.cWeight(sum(prob2W[c(1,2,4)]),sum(prob2W[1:2]))
              cWeight2[4] <- f.cWeight(sum(prob2W[1:4]),sum(prob2W[c(1,2,4)]))
              cWeight2[5] <- f.cWeight(sum(prob2W[1:5]),sum(prob2W[1:4]))
              cWeight2[6] <- f.cWeight(sum(prob2W[1:6]),sum(prob2W[1:5]))
              for(p in 1:6){
                temp2[p] <- (-lambda[i]*(two[k,p+1] - ref[k])^beta[j]) * cWeight2[p]
              }
            }
            else{
              cWeight2[1] <- f.cWeight(prob2E[1],0)
              cWeight2[2] <- f.cWeight(sum(prob2E[1:2]),prob2E[1])
              cWeight2[3] <- f.cWeight(sum(prob2E[c(1,2,4)]),sum(prob2E[1:2]))
              cWeight2[4] <- f.cWeight(sum(prob2E[1:4]),sum(prob2E[c(1,2,4)]))
              cWeight2[5] <- f.cWeight(sum(prob2E[1:5]),sum(prob2E[1:4]))
              cWeight2[6] <- f.cWeight(sum(prob2E[1:6]),sum(prob2E[1:5]))
              for(p in 1:6){
                temp2[p] <- (-lambda[i]*(two[k,p+1] - ref[k])^beta[j]) * cWeight2[p]
              }
            }
          }
          else if(outOfOrder2[k]==1){ # original mixes with penalty in ordering
            if(two[k,1]==1){ # west
              cWeight2[1] <- f.cWeight(prob2W[1],0)
              cWeight2[2] <- f.cWeight(sum(prob2W[1:2]),prob2W[1])
              cWeight2[3] <- f.cWeight(sum(prob2W[c(1,2,4)]),sum(prob2W[1:2]))
              cWeight2[4] <- f.cWeight(sum(prob2W[c(1,2,4,5)]),sum(prob2W[c(1,2,4)]))
              cWeight2[5] <- f.cWeight(sum(prob2W[1:5]),sum(prob2W[c(1,2,4,5)]))
              cWeight2[6] <- f.cWeight(sum(prob2W[1:6]),sum(prob2W[1:5]))
              for(p in 1:6){
                temp2[p] <- (-lambda[i]*(two[k,p+1] - ref[k])^beta[j]) * cWeight2[p]
              }
            }
            else{
              cWeight2[1] <- f.cWeight(prob2E[1],0)
              cWeight2[2] <- f.cWeight(sum(prob2E[1:2]),prob2E[1])
              cWeight2[3] <- f.cWeight(sum(prob2E[c(1,2,4)]),sum(prob2E[1:2]))
              cWeight2[4] <- f.cWeight(sum(prob2E[c(1,2,4,5)]),sum(prob2E[c(1,2,4)]))
              cWeight2[5] <- f.cWeight(sum(prob2E[1:5]),sum(prob2E[c(1,2,4,5)]))
              cWeight2[6] <- f.cWeight(sum(prob2E[1:6]),sum(prob2E[1:5]))
              for(p in 1:6){
                temp2[p] <- (-lambda[i]*(two[k,p+1] - ref[k])^beta[j]) * cWeight2[p]
              }
            }
          }  
          else{
            if(two[k,1]==1){ # west
              cWeight2[1] <- f.cWeight(prob2W[1],0)
              cWeight2[2] <- f.cWeight(sum(prob2W[1:2]),prob2W[1])
              cWeight2[3] <- f.cWeight(sum(prob2W[1:3]),sum(prob2W[1:2]))
              cWeight2[4] <- f.cWeight(sum(prob2W[1:4]),sum(prob2W[1:3]))
              cWeight2[5] <- f.cWeight(sum(prob2W[1:5]),sum(prob2W[1:4]))
              cWeight2[6] <- f.cWeight(sum(prob2W[1:6]),sum(prob2W[1:5]))
              for(p in 1:6){
                  temp2[p] <- (-lambda[i]*(two[k,p+1] - ref[k])^beta[j]) * cWeight2[p]
              }
            }
            else{
              cWeight2[1] <- f.cWeight(prob2E[1],0)
              cWeight2[2] <- f.cWeight(sum(prob2E[1:2]),prob2E[1])
              cWeight2[3] <- f.cWeight(sum(prob2E[1:3]),sum(prob2E[1:2]))
              cWeight2[4] <- f.cWeight(sum(prob2E[1:4]),sum(prob2E[1:3]))
              cWeight2[5] <- f.cWeight(sum(prob2E[1:5]),sum(prob2E[1:4]))
              cWeight2[6] <- f.cWeight(sum(prob2E[1:6]),sum(prob2E[1:5]))
              for(p in 1:6){
                temp2[p] <- (-lambda[i]*(two[k,p+1] - ref[k])^beta[j]) * cWeight2[p]
              }
            }
          }
          out2 <- sum(temp2)
          
          # calc route 12 prospect
          out12 <- 0
          temp12 <- c(0,0,0)
          cWeight12 <- c(0,0,0)
          cWeight12[1] <- f.cWeight(prob12[1],0)
          cWeight12[2] <- f.cWeight(sum(prob12[1:2]),sum(prob12[1]))
          cWeight12[3] <- f.cWeight(sum(prob12[1:3]),sum(prob12[1:2]))
          for(p in 1:3){
            temp12[p] <- (-lambda[i]*(twelve[k,p] - ref[k])^beta[j]) * cWeight12[p]
          }
          out12 <- sum(temp12)

          # calc route 84 prospect
          out84 <- 0
          temp84 <- c(0,0,0)
          cWeight84 <- c(0,0,0)
          cWeight84[1] <- f.cWeight(prob84[1],0)
          cWeight84[2] <- f.cWeight(sum(prob84[1:2]),sum(prob84[1]))
          cWeight84[3] <- f.cWeight(sum(prob84[1:3]),sum(prob84[1:2]))
          for(p in 1:3){
            temp84[p] <- (-lambda[i]*(eightfour[k,p] - ref[k])^beta[j]) * cWeight84[p]
          }
          out84 <- sum(temp84)

          # determine if highest prospect == stated preference
          maxProspect <- max(out2,out12,out84)
          if(maxProspect == out2){
            prospectRoute <- 2
          }
          else if(maxProspect == out12){
            prospectRoute <- 12
          }
          else if(maxProspect == out84){
            prospectRoute <- 84
          }
          else{
            prospectRoute <- NULL
          }
          if(prospectRoute == statedPref[k]){
            output[k] <- output[k] + 1
          }
          
          pb$tick()
        }
      }
    }
  }
  output
}

rp <- f.reference(snoqpass)
proc.time() - ptm
top <- max(rp)

# determine which drivers changed their RP in every circumstance
index_rp <- matrix(NA,length(rp),1)
for(i in 1:length(rp)){
  if(rp[i] == top){
    index_rp[i,] <- i
  }
}
maxRP <- index_rp[!rowSums(!is.finite(index_rp)),]
length(maxRP)

# determine which drivers changed their RP at least once
changeRP <- rp
for(i in 1:length(rp)){
  if(rp[i]>0){
    changeRP[i] <- 1
  }
}

# exports publication quality graph
tikz(file = "originalRC.tex")
graph <- ggplot(mapping=aes(x=routeCompletion,y=1-changeRP))
         + stat_smooth(method=loess) + coord_fixed()
graph + theme(axis.text = element_text(colour="black"), axis.ticks = element_blank())
      + xlab("Percent of route completed before unexpected closure")
      + ylab("Percent of drivers who change their RP")
dev.off()

# exports results
bind <- cbind(changeRP,statedPref,routeCompletion,routeDeviation,
              routeDeviationSP,routeDuration,others)
for(i in 1:nrow(bind)){
  if(bind[i,1] != 0){
    bind[i,1] <- 1
  }
}
bind
mbind <- bind[-maxRP,]
write.csv(bind,"resultsCPT.csv",row.names = FALSE)
```
-----

