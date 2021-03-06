###NOTE
###
###
###Didn't change "min" variable names below bc they are just names but this plots MAX RO
library(reshape2)
MinRO <- read.csv("max_RO_lowdev_96-16.csv")
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

write.csv(PROmin, "YearlyMinRunoff_and_Precip.csv")
#######
#End prep, begin plotting
########
plot(PROmin$P, PROmin$MinRO, col = PROmin$Climate_Div, ylab = "Max Yearly Runoff (mm/d)", xlab = "Precip in Climate Division (mm)")
legend("topleft", legend = levels(PROmin$Climate_Div)[1:5], pch = 19, col = c(1,2,3,4,5))

Div <- "Southern_Mountains"
abline(lm(PROmin$MinRO[PROmin$Climate_Div == Div]~PROmin$P[PROmin$Climate_Div == Div]), col = 1)

Div <- "Nothern_Mountains"
abline(lm(PROmin$MinRO[PROmin$Climate_Div == Div]~PROmin$P[PROmin$Climate_Div == Div]), col = 2)

Div <- "Northern_Piedmont"
abline(lm(PROmin$MinRO[PROmin$Climate_Div == Div]~PROmin$P[PROmin$Climate_Div == Div]), col = 3)

Div <- "Central_Piedmont"
abline(lm(PROmin$MinRO[PROmin$Climate_Div == Div]~PROmin$P[PROmin$Climate_Div == Div]), col = 4)

Div <- "Southern_Piedmont"
abline(lm(PROmin$MinRO[PROmin$Climate_Div == Div]~PROmin$P[PROmin$Climate_Div == Div]), col = 5)

