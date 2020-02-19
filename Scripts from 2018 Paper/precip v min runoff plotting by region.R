library(reshape2)
MinRO <- read.csv("min_RO_lowdev_96-16.csv")
 # [1] "X.1"              "ID"               "WY96"             "WY97"            
 # [5] "WY98"             "WY99"             "WY00"             "WY01"            
 # [9] "WY02"             "WY03"             "WY04"             "WY05"            
# [13] "WY06"             "WY07"             "WY08"             "WY09"            
# [17] "WY10"             "WY11"             "WY12"             "WY13"            
# [21] "WY14"             "WY15"             "site_name"        "dec_lat_va"      
# [25] "dec_long_va"      "climate_div_num"  "climate_div_name" "X"               
# [29] "region"           "min_avg_ALLyrs"  

P <- read.csv("precip2.csv")

#[1] "WY"                     "Southern_Mountains"     "Nothern_Mountains"     
#[4] "Northern_Piedmont"      "Central_Piedmont"       "Southern_Piedmont"     
#[7] "Southern_Coastal_Plain" "Central_Coastal_Plain"  "Northern_Coastal_Plain"

tomelt <- cbind(MinRO[,3:22], MinRO$climate_div_name)
colnames(tomelt)[21] <- "climate_div_name"

ROnew <- melt(tomelt)

colnames(ROnew)[2:3] <- c("WY","Precip")

Pnew <- melt(P)

colnames(Pnew) <- c("WY","climate_div_name","Precip")

PROmin <- merge(Pnew,ROnew, by = c("WY","climate_div_name"))
colnames(PROmin) <- c("WY","Climate_Div","P","MinRO")

PROmin$Region <- PROmin$Climate_Div
PROmin$Region <- as.character(PROmin$Region)
PROmin$Region[PROmin$Region == "Southern_Piedmont"] <- "Piedmont"
PROmin$Region[PROmin$Region == "Northern_Piedmont"] <- "Piedmont"
PROmin$Region[PROmin$Region == "Central_Piedmont"] <- "Piedmont"
PROmin$Region[PROmin$Region == "Southern_Mountains"] <- "Mountains"
PROmin$Region[PROmin$Region == "Nothern_Mountains"] <- "Mountains"
PROmin$Region <- as.factor(PROmin$Region)

#piedmont Means
meansP<-aggregate(PROmin$MinRO[PROmin$Region == "Piedmont"], list(PROmin$P[PROmin$Region == "Piedmont"]), mean, na.rm = TRUE)
colnames(meansP)<- c("Precip","MinRO")
meansP$Region <- rep("Piedmont",length(meansP$Precip))

#MTN means
meansM<-aggregate(PROmin$MinRO[PROmin$Region == "Mountains"], list(PROmin$P[PROmin$Region == "Mountains"]), mean, na.rm = TRUE)
colnames(meansM)<- c("Precip","MinRO")
meansM$Region <- rep("Mountains",length(meansM$Precip))

meansALL <- rbind(meansM, meansP)

#piedmont SD
sdP<-aggregate(PROmin$MinRO[PROmin$Region == "Piedmont"], list(PROmin$P[PROmin$Region == "Piedmont"]), sd, na.rm = TRUE)
colnames(sdP)<- c("Precip","MinRO")
sdP$Region <- rep("Piedmont",length(sdP$Precip))

#MTN SD
sdM<-aggregate(PROmin$MinRO[PROmin$Region == "Mountains"], list(PROmin$P[PROmin$Region == "Mountains"]), sd, na.rm = TRUE)
colnames(sdM)<- c("Precip","MinRO")
sdM$Region <- rep("Mountains",length(sdM$Precip))
sdALL <- rbind(sdM, sdP)

colnames(sdALL)[2] <- "SDev"

#join sd to means

meansALL <- cbind(meansALL, sdALL$SDev)
colnames(meansALL)[4]<-"SDev"

meansALL$Region <- as.factor(meansALL$Region)

#######
#End prep, begin plotting
########

###runiff plotting two plots
par(mfrow = c(1,2))
###

cols = c("dark green", "purple")

plot(meansALL$P,meansALL$MinRO, col = cols[meansALL$Region], ylab = "Minimum Yearly Runoff (mm/d)", xlab = "Annual Precip (mm)", ylim = c(0,1.5))
segments(meansALL$P, meansALL$MinRO-meansALL$SDev, meansALL$P, meansALL$MinRO+meansALL$SDev, col = "gray")
points(meansALL$P,meansALL$MinRO, col = cols[meansALL$Region], pch = 19)

legend("topleft", legend = c("Mountains","Piedmont"), pch = 19, col = cols)

###Run precip v diffMBF plotting by region first
plot(meansALL2$P,meansALL2$MinRO, col = cols[meansALL2$Region], ylab = "% Difference from Mean Baseflow", xlab = "Annual Precip (mm)", ylim = c(-80,170))
segments(meansALL2$P, meansALL2$MinRO-meansALL2$SDev, meansALL2$P, meansALL2$MinRO+meansALL2$SDev, col = "gray")
points(meansALL2$P,meansALL2$MinRO, col = cols[meansALL2$Region], pch = 19)

#legend("topleft", legend = c("Mountains","Piedmont"), pch = 19, col = cols)



# Div <- "Southern_Mountains"
# abline(lm(PROmin$MinRO[PROmin$Climate_Div == Div]~PROmin$P[PROmin$Climate_Div == Div]), col = 1)

# Div <- "Nothern_Mountains"
# abline(lm(PROmin$MinRO[PROmin$Climate_Div == Div]~PROmin$P[PROmin$Climate_Div == Div]), col = 2)

# Div <- "Northern_Piedmont"
# abline(lm(PROmin$MinRO[PROmin$Climate_Div == Div]~PROmin$P[PROmin$Climate_Div == Div]), col = 3)

# Div <- "Central_Piedmont"
# abline(lm(PROmin$MinRO[PROmin$Climate_Div == Div]~PROmin$P[PROmin$Climate_Div == Div]), col = 4)

# Div <- "Southern_Piedmont"
# abline(lm(PROmin$MinRO[PROmin$Climate_Div == Div]~PROmin$P[PROmin$Climate_Div == Div]), col = 5)

