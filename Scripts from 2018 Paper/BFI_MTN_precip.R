library(ggplot2)
library(RColorBrewer)
library(reshape2)
library(gridExtra)



setwd("/Users/jpgannon/Desktop/USGS analysis")
#all <- read.csv("BFI_all_06-16_latlong.csv")
#all <- read.csv("BFI_all_95-15_names.csv")
#colnames(all)

all <- read.csv("BFI_lowdev_96-16.csv")


#undev <- read.csv("undeveloped_gages.csv")
#colnames(undev) <- "ID"

#remove all sites with AVG BFI over .7
#all <- all[-which(all$AVG>1.5),]

alltemp <- all

#all <- merge(all,undev)

allPD <- all[which(all$region == "Piedmont"),]
allMTN <- all[which(all$region == "MTN"),]

#allSTAT <- cbind(as.character(all$site_name),all$AVG, all$SD)
#allSTAT <- as.data.frame(allSTAT, col.names = c("site_name","AVG","SD"))
#names(allSTAT) <- c("site_name","AVG","SD")
#allSTAT <- melt(allSTAT)
#name rows by site name
#row.names(all) <- all$site_name

#get rid of columns not needed
allPD <- allPD[,3:23]
allPD <- allPD[,-29:-24]


allPD_m <- melt(allPD)

allMTN <- allMTN[,3:23]
allMTN <- allMTN[,-29:-24]


allMTN_m <- melt(allMTN)

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
#######
#######
#p <- ggplot(all, aes(variable, site_name)) + geom_tile(aes(fill = AVG), color = "white") + scale_fill_gradient(low = "blue", high = "red")
hm.palette <- colorRampPalette(rev(brewer.pal(11, 'Spectral')), space='Lab')


#####PIEDMONT PLOT
heatPD <- ggplot(allPD_m, aes(x = variable, y = site_name, fill = value)) + 
	geom_tile() +
	ylab("USGS Gages") +
	xlab("Water Year") +
	scale_fill_gradientn(colors = hm.palette(100), name = "Baseflow Index", limits = c(0,1)) +
	theme(axis.text.x = element_text(angle = 90, hjust = 0), plot.margin = unit(c(0, .1, 1, .1), "lines"))

#add barplot of precip to the top of heatmap
barplotPD <- ggplot(data = precip, aes(x = WY, y = PD)) +
	geom_bar(stat = "identity", aes(fill = PD)) + #theme_gray()+
	theme(axis.title.x = element_blank(), axis.text.x = element_blank(),legend.position = "none", 	plot.margin = unit(c(.2, 6.8, 0, 15), "lines"))+
	#top, right, bottom, left
	geom_errorbar(aes(ymin=PD-PDse, ymax = PD+PDse), size = .3, width = .2, position = 	 			position_dodge(.9))+
	labs(x = '')+ 
	scale_y_continuous(limits = c(0,2000))+
	ylab("P (mm)")
	
grid.arrange(barplotPD,heatPD, nrow = 2, ncol = 1, widths = 100, heights = c(10,80))





####MTN PLOT
heatMTN <- ggplot(allMTN_m, aes(x = variable, y = site_name, fill = value)) + 
	geom_tile() +
	ylab("USGS Gages") +
	xlab("Water Year") +
	scale_fill_gradientn(colors = hm.palette(100), name = "Baseflow Index", limits = c(0,1))+
	theme(axis.text.x = element_text(angle = 90, hjust = 0), plot.margin = unit(c(0, .1, 1, .1), 	"lines"))


#add barplot of precip to the top of heatmap
barplotMTN <- ggplot(data = precip, aes(x = WY, y = MTNS)) +
	geom_bar(stat = "identity", aes(fill = MTNS)) + #theme_gray()+
	theme(axis.title.x = element_blank(), axis.text.x = element_blank(),legend.position = "none", 	plot.margin = unit(c(.2, 6.8, 0, 16.6), "lines"))+
	#top, right, bottom, left
	geom_errorbar(aes(ymin=MTNS-MTNSse, ymax = MTNS+MTNSse), size = .3, width = .2, position = 	 	position_dodge(.9))+
	scale_y_continuous(limits = c(0,2000))+
	labs(x = '')+
	ylab("P (mm)")
	
grid.arrange(barplotMTN,heatMTN, nrow = 2, ncol = 1, widths = 100, heights = c(10,80))


#both on one?
grid.arrange(barplotPD,heatPD,barplotMTN, heatMTN, nrow = 4, ncol = 1, widths = 100, heights = c(10,80,10,80))
