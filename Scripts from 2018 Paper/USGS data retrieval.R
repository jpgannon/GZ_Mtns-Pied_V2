#load data retrieval

#install.packages("dataRetrieval")

library(dataRetrieval)
library(EcoHydRology)

#vignette("dataRetrieval", package = "dataRetrieval")


# Tuck river at bryson city:
#siteNumber <- "03513000"
#First Broad
#siteNumber <- "02152474"
#little tennessee
#siteNumber <- "03503000"
parameterCd <- "00060"  # Discharge
#startDate <- "2009-10-01"
#endDate <- "2012-09-30"
startDate <- "2007-10-01"
endDate <- "2008-09-30"
siteNumber <- "02053200"
discharge <- readNWISdv(siteNumber, parameterCd, startDate, endDate)


plot(discharge$Date, discharge[,4], type = 'l')

filter <- .8 #filter parameter (.925 is recommended but can play)
pass <- 3 #passes through data

BFS <- BaseflowSeparation(discharge[,4], filter, pass)

#refs for baseflow sep
#Lyne, V. D. and M. Hollick (1979). Stochastic time-variable rainfall-runoff modelling. Hydrology and Water Resources Symposium, Perth, Institution of Engineers, Australia.
#Nathan, R. J. and T. A. McMahon (1990). "Evaluation of automated techniques for base flow and recession analysis." Water Resources Research 26(7): 1465-1473.

plot(discharge$Date, BFS$bt+BFS$qft, type = 'l', col = "red")
points(discharge$Date, BFS$bt, type = 'l')

#Daily Discharge CFS
#86400 seconds in a day
#don't really have to do this since we are calculating a proportion

totalSF <- sum(BFS$qft) * 86400
totalBF <- sum(BFS$bt) * 86400 

#Stormflow / Baseflow
#if this number is higher: a higher percentage of flow is from stormflow
SFoBF <- totalSF/totalBF

