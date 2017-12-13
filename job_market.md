---
layout: page
title: Job Market Paper
---


<div class="message">
  The defining feature separating (cumulative) prospect theory from expected utility theory is that potential outcomes are measured relative to a reference point as opposed to final assets. Determining the reference point, therefore, is vital to correct analysis. While many theories and assumptions have been made concerning reference point up- dating between repeated choices, few papers have looked at reference point updating within periods. This paper seeks to find if and when drivers change their reference point in a transportation setting when faced with an unexpected delay en route. Drivers are more likely to change their reference point if the unexpected delay occurs near the end- points of travel.
</div>


> The defining feature separating (cumulative) prospect theory from expected utility theory is that potential outcomes are measured relative to a reference point as opposed to final assets. Determining the reference point, therefore, is vital to correct analysis. While many theories and assumptions have been made concerning reference point up- dating between repeated choices, few papers have looked at reference point updating within periods. This paper seeks to find if and when drivers change their reference point in a transportation setting when faced with an unexpected delay en route. Drivers are more likely to change their reference point if the unexpected delay occurs near the end- points of travel.


## Supplemental Material

Might be better to look [here]({{ site.baseurl }}/assets/RerouteAug2017Alt.R)

``` sh
// This program finds the duration, in seconds, to all reported I-90 
// destinations from Snoqualmie Pass over three alternatives (2, 12, 84)

setwd("C:/Users/jbradley.eustice/Dropbox/DATA_1_2")
// setwd("/Users/Bradley/Dropbox/DATA_1_2")

library(bitops)
library(httr)
library(XML)
library(RCurl)
library(rJava)
library(xlsxjars)
library(xlsx)
library(progress)

// read in data
input <- read.csv("inputRoutes.csv",header=F) // west, origin, destination, epochTime
allData <- as.matrix(input)
colnames(allData) <- NULL
numRow <- nrow(allData)
penalty <- 10050
snoqPass <- "SNOQUALMIE+PASS+WA"
stevPass <- "STEVENS+PASS+WA"
drive <- c("pessimistic","best_guess","optimistic")
key <- "AIzaSyDyfOxnwiFKSWSqNc9avgWMC04l_otTH0Y"

// routes from Snoqualmie Pass over wavepoints to final destination
f.wavepoint <- function(wavepointA,wavepointB,speed){
  results.A <- matrix(nrow=numRow,ncol=1)
  results.B <- matrix(nrow=numRow,ncol=1)
  results.C <- matrix(nrow=numRow,ncol=1)
  results.out <- matrix(nrow=numRow,ncol=1)
  pb <- progress_bar$new(total = numRow)
  for(i in c(848)){
    tempTime <- 0
    
    // routes from SP to first wavepoint
    k <- 0
    while(is.na(results.A[i]) && k<10){
      if(allData[i,1] == 1){
        url <- paste0('https://maps.googleapis.com/maps/api/distancematrix/xml?origins=',
                      snoqPass,'&destinations=',wavepointA,'&units=imperial&departure_time=',allData[i,4],
                      '&traffic_model=',speed,'&key=',key)
      }
      else{
        url <- paste0('https://maps.googleapis.com/maps/api/distancematrix/xml?origins=',
                      snoqPass,'&destinations=',wavepointB,'&units=imperial&departure_time=',allData[i,4],
                      '&traffic_model=',speed,'&key=',key)
      }
      Sys.sleep(0.3)
      tie <- xmlParse(GET(url))
      tryCatch(results.A[i] <- as.numeric(xpathApply(tie,"//duration_in_traffic/value",xmlValue)), error=function(e) NULL)
      k <- k+1
    }
    tempTime <- as.numeric(allData[i,4]) + results.A[i]
    
    // routes from first wavepoint to second wavepoint
    k <- 0
    while(is.na(results.B[i]) && k<10){
      if(allData[i,1] == 1){
        url <- paste0('https://maps.googleapis.com/maps/api/distancematrix/xml?origins=',
                      wavepointA,'&destinations=',wavepointB,'&units=imperial&departure_time=',tempTime,
                      '&traffic_model=',speed,'&key=',key)
      }
      else{
        url <- paste0('https://maps.googleapis.com/maps/api/distancematrix/xml?origins=',
                      wavepointB,'&destinations=',wavepointA,'&units=imperial&departure_time=',tempTime,
                      '&traffic_model=',speed,'&key=',key)
      }
      Sys.sleep(0.3)
      tie <- xmlParse(GET(url))
      tryCatch(results.B[i] <- as.numeric(xpathApply(tie,"//duration_in_traffic/value",xmlValue)), error=function(e) NULL)
      k <- k+1
    }
    tempTime <- tempTime + results.B[i]
    
    // routes from second wavepoint to final destination
    k <- 0
    while(is.na(results.C[i]) && k<10){
      if(allData[i,1] == 1){
        url <- paste0('https://maps.googleapis.com/maps/api/distancematrix/xml?origins=',
                      wavepointB,'&destinations=',allData[i,3],'&units=imperial&departure_time=',tempTime,
                      '&traffic_model=',speed,'&key=',key)
      }
      else{
        url <- paste0('https://maps.googleapis.com/maps/api/distancematrix/xml?origins=',
                      wavepointA,'&destinations=',allData[i,3],'&units=imperial&departure_time=',tempTime,
                      '&traffic_model=',speed,'&key=',key)
      }
      Sys.sleep(0.3)
      tie <- xmlParse(GET(url))
      tryCatch(results.C[i] <- as.numeric(xpathApply(tie,"//duration_in_traffic/value",xmlValue)), error=function(e) NULL)
      k <- k+1
    }
    
    // output
    results.out[i] <- (results.A[i] + results.B[i] + results.C[i])/60
    pb$tick()
  }
  results.out
}

// routes from Snoqualmie Pass over wavepoints to final destination
f.wavepointPLUS <- function(wavepointA,wavepointB,speed){
  results.A <- matrix(nrow=numRow,ncol=1)
  results.B <- matrix(nrow=numRow,ncol=1)
  results.C <- matrix(nrow=numRow,ncol=1)
  results.D <- matrix(nrow=numRow,ncol=1)
  results.out <- matrix(nrow=numRow,ncol=1)
  pb <- progress_bar$new(total = numRow)
  for(i in c()){
    
    // routes from SP to first wavepoint
    k <- 0
    while(is.na(results.A[i]) && k<10){
      if(allData[i,1] == 1){
        url <- paste0('https://maps.googleapis.com/maps/api/distancematrix/xml?origins=',
                      snoqPass,'&destinations=',wavepointA,'&units=imperial&departure_time=',allData[i,4],
                      '&traffic_model=',speed,'&key=',key)
      }
      else{
        url <- paste0('https://maps.googleapis.com/maps/api/distancematrix/xml?origins=',
                      snoqPass,'&destinations=',wavepointB,'&units=imperial&departure_time=',allData[i,4],
                      '&traffic_model=',speed,'&key=',key)
      }
      Sys.sleep(0.3)
      tie <- xmlParse(GET(url))
      tryCatch(results.A[i] <- as.numeric(xpathApply(tie,"//duration_in_traffic/value",xmlValue)), error=function(e) NULL)
      k <- k+1
    }
    tempTime <- as.numeric(allData[i,4]) + results.A[i]
    
    // routes from first wavepoint to Stevens Pass
    k <- 0
    while(is.na(results.B[i]) && k<10){
      if(allData[i,1] == 1){
        url <- paste0('https://maps.googleapis.com/maps/api/distancematrix/xml?origins=',
                      wavepointA,'&destinations=',stevPass,'&units=imperial&departure_time=',tempTime,
                      '&traffic_model=',speed,'&key=',key)
      }
      else{
        url <- paste0('https://maps.googleapis.com/maps/api/distancematrix/xml?origins=',
                      wavepointB,'&destinations=',stevPass,'&units=imperial&departure_time=',tempTime,
                      '&traffic_model=',speed,'&key=',key)
      }
      Sys.sleep(0.3)
      tie <- xmlParse(GET(url))
      tryCatch(results.B[i] <- as.numeric(xpathApply(tie,"//duration_in_traffic/value",xmlValue)), error=function(e) NULL)
      k <- k+1
    }
    tempTime <- tempTime + results.B[i] + penalty
    
    // routes from Stevens Pass to second wavepoint
    k <- 0
    while(is.na(results.C[i]) && k<10){
      if(allData[i,1] == 1){
        url <- paste0('https://maps.googleapis.com/maps/api/distancematrix/xml?origins=',
                      stevPass,'&destinations=',wavepointB,'&units=imperial&departure_time=',tempTime,
                      '&traffic_model=',speed,'&key=',key)
      }
      else{
        url <- paste0('https://maps.googleapis.com/maps/api/distancematrix/xml?origins=',
                      stevPass,'&destinations=',wavepointA,'&units=imperial&departure_time=',tempTime,
                      '&traffic_model=',speed,'&key=',key)
      }
      Sys.sleep(0.3)
      tie <- xmlParse(GET(url))
      tryCatch(results.C[i] <- as.numeric(xpathApply(tie,"//duration_in_traffic/value",xmlValue)), error=function(e) NULL)
      k <- k+1
    }
    tempTime <- tempTime + results.C[i]
    
    // routes from second wavepoint to final destination
    k <- 0
    while(is.na(results.D[i]) && k<10){
      if(allData[i,1] == 1){
        url <- paste0('https://maps.googleapis.com/maps/api/distancematrix/xml?origins=',
                      wavepointB,'&destinations=',allData[i,3],'&units=imperial&departure_time=',tempTime,
                      '&traffic_model=',speed,'&key=',key)
      }
      else{
        url <- paste0('https://maps.googleapis.com/maps/api/distancematrix/xml?origins=',
                      wavepointA,'&destinations=',allData[i,3],'&units=imperial&departure_time=',tempTime,
                      '&traffic_model=',speed,'&key=',key)
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


for(i in c(848)){
  route84[i] <- route84WHAT[i]
}

route2WHAT <- NULL
route2plusWHAT <- NULL
route12WHAT <- NULL
route84WHAT <- NULL

```

-----

