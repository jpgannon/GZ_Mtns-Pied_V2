library(ggplot2)
library(RColorBrewer)
library(reshape2)
library(gridExtra)

setwd("/Users/jpgannon/Desktop/USGS analysis")

mbf <- read.csv("MBF_lowdev_96-16.csv")

mbfPD <- mbf[which(mbf$region == "Piedmont"),]
mbfMTN <- mbf[which(mbf$region == "MTN"),]


mbfPD <- mbfPD[,3:23]
mbfMTN <- mbfMTN[,3:23]

mbfPD$LTmean <- rowMeans(mbfPD[,1:20], na.rm = TRUE) 
mbfMTN$LTmean <- rowMeans(mbfMTN[,1:20], na.rm = TRUE) 

#boxplot of deviations from mean
boxplot(mbfPD[,1:20]-mbfPD$LTmean)
boxplot(mbfMTN[,1:20]-mbfMTN$LTmean)

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

#format precip for side by side barplot
prec_bars <- precip[,c(1,10,11)]
prec_bars <- melt(prec_bars)

#######
#######

###Boxplots paired
bp1 <- ggplot(mbfALLm, aes(x = variable, y = value, fill = region)) +
        theme_bw() +
        geom_boxplot(alpha=0.7) +
        scale_y_continuous(name = "% Difference from MBF") +
        
        theme(plot.title = element_text(size = 14, family = "Tahoma"),
              text = element_text(size = 12, family = "Tahoma"),
              
              axis.text.x=element_text(size = 11, angle = 90)) +
        scale_fill_brewer(name = "Region",guide = "legend", palette = "Accent", labels = c("Mountains","Piedmont"))+
        xlab("Water Year")+
        geom_hline(yintercept=0)


#precip barplot
barplotPD <- ggplot(data = prec_bars, aes(x = WY, y = value, fill = variable)) +
	geom_bar(stat = "identity", width = .75, position = "dodge") + #theme_gray()+
	theme_bw()+
	theme(text = element_text(size = 12, family = "Tahoma"),
		axis.title.x = element_blank(), 
		plot.title = element_text(size = 14, family = "Tahoma"),
		axis.text.x = element_blank(),
		legend.position = "none", 	
		plot.margin = unit(c(.2, 6.3, 0, 0.1), "lines"))+
	#top, right, bottom, left
	labs(x = '')+ 
	scale_fill_brewer(name = "Region", guide = "legend", palette = "Accent")+
	scale_y_continuous(limits = c(0,2000))+
	ylab("P (mm)")
	
grid.arrange(barplotPD,bp1, nrow = 2, ncol = 1, widths = 100, heights = c(10,80))

