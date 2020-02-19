RO<-read.csv("average_runoff_info.csv")

 #[1] "ID"               "site_name"        "dec_lat_va"       "dec_long_va"     
 #[5] "climate_div_num"  "climate_div_name" "region"           "min_avg"         
 #[9] "avg_avg"          "max_avg"

attach(RO)

#region[region == "MTN"] <- "Mountains"

par(mfrow = c(1,3)) 
boxplot(min_avg~region, ylab = "Minimum Runoff (mm/d)", names = c("Mountains","Piedmont"))
boxplot(avg_avg~region, ylab = "Average Runoff (mm/d)", names = c("Mountains","Piedmont"))
boxplot(max_avg~region, ylab = "Maximum Runoff (mm/d)", names = c("Mountains","Piedmont"))

par(mfrow = c(1,3)) 
boxplot(min_avg~climate_div_name, ylab = "Minimum RO (mm/d)")
boxplot(avg_avg~climate_div_name, ylab = "Average RO (mm/d)")
boxplot(max_avg~climate_div_name, ylab = "Maximum RO (mm/d)")