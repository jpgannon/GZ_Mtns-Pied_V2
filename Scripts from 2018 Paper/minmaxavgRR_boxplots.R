library(reshape2)
RO<-read.csv("RRs_lowdev_96-16.csv")

 #[1] "ID"               "site_name"        "dec_lat_va"       "dec_long_va"     
 #[5] "climate_div_num"  "climate_div_name" "region"           "min_avg"         
 #[9] "avg_avg"          "max_avg"

tomelt <- RO[3:22]
tomelt <- cbind(tomelt, RO[27])

RRM <- melt(tomelt)

colnames(RRM) <- c("ClimateDiv","WY","RR")

boxplot(RRM$RR ~ RRM$ClimateDiv)

boxplot(RRM$RR ~ RRM$ClimateDiv, plot = FALSE)

tapply(RRM$RR[is.na(RRM$RR)==FALSE], RRM$ClimateDiv[is.na(RRM$RR)==FALSE], median)

