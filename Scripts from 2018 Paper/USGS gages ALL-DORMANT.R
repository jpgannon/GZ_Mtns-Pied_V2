library(dataRetrieval)
library(EcoHydRology)

#vignette("dataRetrieval", package = "dataRetrieval")


# Tuck river at bryson city:
#siteNumber <- "03513000"
#First Broad
#siteNumber <- "02152474"
#little tennessee
#siteNumber <- "03503000"
#parameterCd <- "00060"  # Discharge
#startDate <- "2009-10-01"
#endDate <- "2012-09-30"
#startDate <- "2007-10-01"
#endDate <- "2008-09-30"
#siteNumber <- "02053200"

setwd("/Users/jpgannon/Desktop/USGS analysis")

IDs <- read.csv("GageIDs_LowDev.csv")
IDs$ID <- paste("0",as.character(IDs$ID), sep = '')

for(y in 1:length(IDs$ID)){

SBYr <- rep(0,10)
starts <- c("1995-11-01","1996-11-01", "1997-11-01","1998-11-01", "1999-11-01","2000-11-01", "2001-11-01","2002-11-01", "2003-11-01","2004-11-01", "2005-11-01","2006-11-01", "2007-11-01","2008-11-01", "2009-11-01","2010-11-01", "2011-11-01","2012-11-01", "2013-11-01","2014-11-01")
ends <- c("1996-03-31", "1997-03-31", "1998-03-31", "1999-03-31", "2000-03-31", "2001-03-31", "2002-03-31", "2003-03-31", "2004-03-31", "2005-03-31", "2006-03-31", "2007-03-31", "2008-03-31", "2009-03-31", "2010-03-31", "2011-03-31", "2012-03-31", "2013-03-31", "2014-03-31", "2015-03-31")

for (x in 1:20){
	startDate <- starts[x]
	endDate <- ends[x]
	siteNumber <- IDs$ID[y]
	parameterCd <- "00060"
	discharge <- readNWISdv(siteNumber, parameterCd, startDate, endDate)

	#plot(discharge$Date, discharge[,4], type = 'l')
if(length(discharge)>0){
if(length(discharge[,4])>150){
	filter <- .925 #filter parameter (.925 is recommended but can play)
	pass <- 3 #passes through data

	BFS <- BaseflowSeparation(discharge[,4], filter, pass)

	#plot(discharge$Date, BFS$bt+BFS$qft, type = 'l', col = "red")
	#points(discharge$Date, BFS$bt, type = 'l')

	#Daily Discharge CFS
	#86400 seconds in a day
	#don't really have to do this since we are calculating a proportion

	totalSF <- sum(BFS$qft)
	totalBF <- sum(BFS$bt) 

	#Stormflow / Baseflow
	#if this number is higher: a higher percentage of flow is from stormflow
	#SFoBF <- totalSF/totalBF
	
	#calculate BFI
	BFI <- totalBF / (totalBF+totalSF)
	
	#step through the data sheet, addding calculated values
	IDs[y,x+1] <- BFI
}}}}


names <- read.csv("NCgages_names.csv")
IDspre <- IDs
IDs[,1]<-as.numeric(IDs[,1])
IDs <- merge(IDs, names)

colnames(IDs) <- c("ID" ,         "WY96"  ,      "WY97" ,       "WY98"   ,     "WY99"   ,     "WY00"      
 ,"WY01"  ,      "WY02"    ,  "WY03"    ,    "WY04"    ,    "WY05"     ,   "WY06"       
, "WY07"    ,     "WY08"    ,      "WY09"       ,  "WY10"    ,     "WY11"   ,      "WY12"        
,"WY13"     ,    "WY14"   ,      "WY15"    ,     "site_name" ,  "dec_lat_va" , "dec_long_va","climate_div_num" , "climate_div_name" ,"X"     ,           "region")

#ADD REGION INFO


write.csv(IDs,"BFI_lowdev_96-16_DORMANT.csv")

#all <- read.csv("BFI_all_06-16.csv")
#all$avg <- mean(as.numeric(all[,3:12]), na.rm = TRUE)
#all$sd <- sd(all[,3:12], na.rm = TRUE)
#refs for baseflow sep
#Lyne, V. D. and M. Hollick (1979). Stochastic time-variable rainfall-runoff modelling. Hydrology and Water Resources Symposium, Perth, Institution of Engineers, Australia.
#Nathan, R. J. and T. A. McMahon (1990). "Evaluation of automated techniques for base flow and recession analysis." Water Resources Research 26(7): 1465-1473.
