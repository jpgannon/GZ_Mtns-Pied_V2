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

months <- c(1,31,61,92,123,151,182,212,243,273,304,335,365)
monthsYR <- rep(0,365)
monthnums<-c(1,2,3,4,5,6,7,8,9,10,11,12)
for(r in 1:12){
	monthsYR[months[r]:months[r+1]] <- monthnums[r]
}

monthsLY <- c(1,31,60,92,123,152,183,213,244,274,305,336,366)
monthsLYYR <- rep(0,366)
for(r in 1:12){
	monthsLYYR[monthsLY[r]:monthsLY[r+1]] <- monthnums[r]
}

realmonths <- read.csv("WYdates.csv")

WY <- c("WY96-","WY97-" ,       "WY98-"   ,     "WY99-"   ,     "WY00-"      
 ,"WY01-"  ,      "WY02-"    ,  "WY03-"    ,    "WY04-"    ,    "WY05-"     ,   "WY06-"       
, "WY07-"    ,     "WY08-"    ,      "WY09-"       ,  "WY10-"    ,     "WY11-"   ,      "WY12-"        
,"WY13-"     ,    "WY14-"   ,      "WY15-" ,"WY16-" )

for(y in 1:length(IDs$ID)){

SBYr <- rep(0,10)
starts <- c("1995-10-01","1996-10-01", "1997-10-01","1998-10-01", "1999-10-01","2000-10-01", "2001-10-01","2002-10-01", "2003-10-01","2004-10-01", "2005-10-01","2006-10-01", "2007-10-01","2008-10-01", "2009-10-01","2010-10-01", "2011-10-01","2012-10-01", "2013-10-01","2014-10-01","2015-10-01")
ends <- c("1996-09-30", "1997-09-30", "1998-09-30", "1999-09-30", "2000-09-30", "2001-09-30", "2002-09-30", "2003-09-30", "2004-09-30", "2005-09-30", "2006-09-30", "2007-09-30", "2008-09-30", "2009-09-30", "2010-09-30", "2011-09-30", "2012-09-30", "2013-09-30", "2014-09-30", "2015-09-30","2016-09-30")

count <- 1

for (x in 1:21){
	startDate <- starts[x]
	endDate <- ends[x]
	siteNumber <- IDs$ID[y]
	parameterCd <- "00060"
	discharge <- readNWISdv(siteNumber, parameterCd, startDate, endDate)

	#plot(discharge$Date, discharge[,4], type = 'l')
if(length(discharge)>0){
if(length(discharge[,4])>364){
	filter <- .925 #filter parameter (.925 is recommended but can play)
	pass <- 3 #passes through data

	BFS <- BaseflowSeparation(discharge[,4], filter, pass)

	if((x-1) %% 4 == 0 | x == 1) {BFS <- cbind(monthsLYYR, BFS)
	} else {BFS <- cbind(monthsYR, BFS)}
	
	#plot(discharge$Date, BFS$bt+BFS$qft, type = 'l', col = "red")
	#points(discharge$Date, BFS$bt, type = 'l')

	#totalSF <- sum(BFS$qft)
	#totalBF <- sum(BFS$bt) 

	#Stormflow / Baseflow
	#if this number is higher: a higher percentage of flow is from stormflow
	#SFoBF <- totalSF/totalBF
	
	#calculate BFI
	#BFI <- totalBF / (totalBF+totalSF)
	
	SFtotals <- tapply(BFS$qft,BFS$months, sum)
	BFtotals <- tapply(BFS$bt, BFS$months, sum)
	BFIs <- BFtotals / (SFtotals + BFtotals)
	
	names(SFtotals) <- paste(WY[x],names(SFtotals),  sep = '')
	names(BFtotals) <- paste(WY[x],names(BFtotals),  sep = '')
	names(BFIs) <- paste(WY[x],names(BFIs),  sep = '')
	
	Gstats <- data.frame(realmonths[((12*x)-11):(12*x),], names(SFtotals),unname(SFtotals), unname(BFtotals),unname(BFIs))
	colnames(Gstats) <- c("month","WYmonth","SFT","BFT","BFI")
	
	if(count == 1){
		GstatsFin <- Gstats
	}

	if(count > 1){
		GstatsFin <- rbind(GstatsFin, Gstats)
	}
	count <- count + 1
	
}}}
write.csv(GstatsFin, paste(siteNumber,".csv", sep = ''))
}


#names <- read.csv("NCgages_names.csv")
#IDspre <- IDs
#IDs[,1]<-as.numeric(IDs[,1])
#IDs <- merge(IDs, names)

#colnames(IDs) <- c("ID" ,   "WY96",   "WY97" ,       "WY98"   ,     "WY99"   ,     "WY00"      
# ,"WY01"  ,      "WY02"    ,  "WY03"    ,    "WY04"    ,    "WY05"     ,   "WY06"       
#, "WY07"    ,     "WY08"    ,      "WY09"       ,  "WY10"    ,     "WY11"   ,      "WY12"        
#,"WY13"     ,    "WY14"   ,      "WY15"  ,     "site_name" ,  "dec_lat_va" , #"dec_long_va","climate_div_num" , "climate_div_name" ,"X"     ,           "region")

#ADD REGION INFO


#write.csv(IDs,"BFI_lowdev_96-16.csv")

#all <- read.csv("BFI_all_06-16.csv")
#all$avg <- mean(as.numeric(all[,3:12]), na.rm = TRUE)
#all$sd <- sd(all[,3:12], na.rm = TRUE)
#refs for baseflow sep
#Lyne, V. D. and M. Hollick (1979). Stochastic time-variable rainfall-runoff modelling. Hydrology and Water Resources Symposium, Perth, Institution of Engineers, Australia.
#Nathan, R. J. and T. A. McMahon (1990). "Evaluation of automated techniques for base flow and recession analysis." Water Resources Research 26(7): 1465-1473.
