library(ggplot2)
library(RColorBrewer)
library(reshape2)
library(gridExtra)

setwd("/Users/jpgannon/Desktop/USGS analysis")

#bring in mean baseflow
mbf <- read.csv("MBF_lowdev_96-16.csv")

mbfPD <- mbf[which(mbf$region == "Piedmont"),]
mbfMTN <- mbf[which(mbf$region == "MTN"),]


mbfPD <- mbfPD[,3:23]
mbfMTN <- mbfMTN[,3:23]

mbfPD$LTmean <- rowMeans(mbfPD[,1:20], na.rm = TRUE) 
mbfMTN$LTmean <- rowMeans(mbfMTN[,1:20], na.rm = TRUE) 


#add column ID and combine PD and MTN for paired boxplots
mbfPD2 <- mbfPD
mbfPD2$region <- rep("PD", length(mbfPD[,2]))
mbfMTN2 <- mbfMTN
mbfMTN2$region <- rep("MTN", length(mbfMTN[,2]))

mbfALL <- rbind(mbfPD2,mbfMTN2)

#calculate percent deviation
mbfALL_LTM <- ((mbfALL[,1:20]-mbfALL$LTmean)/mbfALL$LTmean) *100

mbfALL_LTM <- cbind(mbfALL_LTM, mbfALL$site_name, mbfALL$region)
colnames(mbfALL_LTM)[21:22] <- c("site_name","region")

mbfALLm <- melt(mbfALL_LTM)

#mbfMelt <- melt(mbfALL)

#bring in BFI
#bring in mean baseflow
bfi <- read.csv("BFI_lowdev_96-16.csv")

bfiPD <- bfi[which(bfi$region == "Piedmont"),]
bfiMTN <- bfi[which(bfi$region == "MTN"),]


bfiPD <- bfiPD[,3:23]
bfiMTN <- bfiMTN[,3:23]
 

#add column ID and combine PD and MTN for paired boxplots
bfiPD2 <- bfiPD
bfiPD2$region <- rep("PD", length(bfiPD[,2]))
bfiMTN2 <- bfiMTN
bfiMTN2$region <- rep("MTN", length(bfiMTN[,2]))

bfiALL <- rbind(bfiPD2,bfiMTN2)


#colnames(mbfALL_LTM)[21:22] <- c("site_name","region")

bfiALLm <- melt(bfiALL)



#########
#Bring in and format precip data
#########

precip <- read.csv("precip.csv")
precip$MTNS <- rowMeans(cbind(precip$Southern_Mountains, precip$Nothern_Mountains))
precip$PD <- rowMeans(cbind(precip$Southern_Piedmont, precip$Northern_Peidmont, precip$Central_Piedmont))

temp<- transform(precip, SD=apply(cbind(precip$Southern_Mountains, precip$Nothern_Mountains), 1,sd))
precip$MTNSsd <- temp$SD
precip$MTNSse <- precip$MTNSsd / sqrt(2)

temp <- transform(precip, SD=apply(cbind(precip$Southern_Piedmont, precip$Northern_Peidmont, precip$Central_Piedmont), 1,sd))
precip$PDsd <- temp$SD
precip$PDse <- precip$PDsd / sqrt(3)

precip$WY <- paste("WY",as.character(precip$WY), sep = '')
precip$WY <- factor(precip$WY, levels = precip$WY)

#add precip to melted MBF
colnames(mbfALLm)[3] <- "WY"
mbf_P <- merge(precip[,c(1,10,11)], mbfALLm)




#plot precip vs mean baseflow deviation
plot(mbf_P$value[mbf_P$region == "MTN"], mbf_P$MTNS[mbf_P$region == "MTN"], ylim = c(800,2000), xlim = c(-100,200))
points(mbf_P$value[mbf_P$region == "PD"], mbf_P$PD[mbf_P$region == "PD"], col = "red")

#plot mean baseflow VS BFI
plot(mbfALLm$value[mbfALLm$region == "MTN"], bfiALLm$value[bfiALLm$region == "MTN"], ylim = c(0,1), xlim = c(-100,200), ylab = "Baseflow Index", xlab = "Deviation from Mean BF", pch = 20)
points(mbfALLm$value[mbfALLm$region == "PD"], bfiALLm$value[bfiALLm$region == "PD"], col = "red")