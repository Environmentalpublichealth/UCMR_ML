setwd("~/Desktop/Jiali/TAMU/environment/UCMR/Rscript/")

# combine data
U3_data <- read.csv("../ucmr-3-occurrence-data/UCMR3_All.txt", sep = "\t", quote = "", 
                    row.names = NULL, stringsAsFactors = FALSE)
U4_data <- read.csv("../ucmr_4_occurrence_data/UCMR4_All.txt", sep = "\t", quote = "", 
                    row.names = NULL, stringsAsFactors = FALSE, fileEncoding = "latin1")
U5_data <- read.csv("../ucmr5-occurrence-data/UCMR5_All.txt", sep = "\t", quote = "", 
                    row.names = NULL, stringsAsFactors = FALSE,fileEncoding = "latin1")

PWSdetail <- "../States/Water System Detail_20240806.xlsx"
detail <- read_excel(PWSdetail, sheet = 1, skip = 4)
length(intersect(detail$`PWS ID`,U3_data$PWSID)) # 5946
length(intersect(detail$`PWS ID`,U5_data$PWSID)) # 3796
intersect(intersect(detail$`PWS ID`,U3_data$PWSID), intersect(detail$`PWS ID`,U5_data$PWSID)) # 1919

PWSviolation <- "../States/Water System Summary_20240806.xlsx"
violation <- read_excel(PWSviolation, sheet = 1, skip = 4)

PWSarea1 <- "../States/Water System Service Area_region1-5_20240806.xlsx"
PWSarea2 <- "../States/Water System Service Area_region6-10_20240806.xlsx"
ServeArea1 <- read_excel(PWSarea1, sheet = 1, skip = 4)
ServeArea2 <- read_excel(PWSarea2, sheet = 1, skip = 4)
ServeArea <- rbind(ServeArea1, ServeArea2)

length(intersect(detail$`PWS ID`, ServeArea$`PWS ID`)) # 143512

# add lat and long to the PWS by county (use data from u.s. weather gov)
library(maptools)
counties <- as.data.frame(readShapeLines("../CountyBoundary/c_05mr24.shp"))
violation$State <- substr(violation$`PWS ID`, start = 1, stop = 2)
vio_lat_long <- merge(violation, counties[!duplicated(counties$FIPS),c(1,3,7,8)], by.x = c("Counties Served","State"), by.y=c("COUNTYNAME","STATE"), all.x=T)
 # 143512 identified locations
 # NA numbers: 5841

#--- add the air quality data ---#
# AQ13 <- read.csv("../States/annual_aqi_by_county_2013.csv", header = T)
# AQ14 <- read.csv("../States/annual_aqi_by_county_2014.csv", header = T)
# AQ15 <- read.csv("../States/annual_aqi_by_county_2015.csv", header = T)
# 
# # combine the three years
# AQ_all <- rbind(AQ13, AQ14,AQ15)
# # extract the target states
# AQ_states <- AQ_all[AQ_all$State %in% c("Massachusetts", "Michigan","Texas","North Carolina","Washington"),]
# AQ_agg <- aggregate(cbind(Good.Days,Moderate.Days,Unhealthy.for.Sensitive.Groups.Days,Unhealthy.Days,Very.Unhealthy.Days,Hazardous.Days) ~ State + County, data = AQ_states, mean)
# AQ_agg$AQscore <- AQ_agg$Good.Days*1 + AQ_agg$Moderate.Days*2+AQ_agg$Unhealthy.for.Sensitive.Groups.Days*3+AQ_agg$Unhealthy.Days*4+AQ_agg$Very.Unhealthy.Days*5+AQ_agg$Hazardous.Days*6
# summary(AQ_agg)

# add AQ into the variables
# Geo_lat_long_AQ <- merge(Geo_lat_long, AQ_agg[,c(1,2,9)], by.x = c("Primacy.Agency","County.Served"), by.y =c("State","County"), all.x=T)

# combine all data from SDWIS and delete the variables don't need
# x variables - PWSID, type, source, wholesale, population,outstanding performer, source protected,  connection, lat, long, # of facilities, # of violations, # of visits, serving area
mydata <- merge(detail[,c(1,5,8,13,16,17,19,21)], vio_lat_long[,c(3,10:14)], by="PWS ID", all.x = T) %>%
  merge(ServeArea[!duplicated(ServeArea$`PWS ID`),c(1,7)], by = "PWS ID", all.x = T)
dim(mydata[is.na(mydata$LAT),])

write.csv(mydata, "../States/SDWIS_with_loc08072024.csv", row.names = F, quote = F)
  
# convert all categorical variables into dummy data
# install.packages("fastDummies")
library(fastDummies)
mydata_dum <- dummy_cols(mydata, select_columns = c("PWS Type","Primary Source","Service Area"), remove_selected_columns = TRUE)

# include the contaminant data from UCMR
# U3
library(reshape2)
library(dplyr)
U3_df <- data.frame()

for (PWS in unique(U3_data$PWSID)) {
PWS1 <- U3_data[U3_data$PWSID %in% PWS,]
PWS1$AnalyticalResultValue[PWS1$AnalyticalResultsSign == "<"] <- 0
sum_PWS1 <- aggregate(cbind(MRL,AnalyticalResultValue) ~ PWSID + Contaminant, data = PWS1, max)
sum_PWS1$label <- ifelse(sum_PWS1$AnalyticalResultValue < sum_PWS1$MRL, 0, 1)
PWS1_final <- dcast(sum_PWS1[,c(1,2,5)], PWSID ~ Contaminant)
U3_df <- bind_rows(U3_df, PWS1_final)
}

# U4
library(reshape2)
library(dplyr)
U3_df <- data.frame()

for (PWS in unique(U3_data$PWSID)) {
  PWS1 <- U3_data[U3_data$PWSID %in% PWS,]
  PWS1$AnalyticalResultValue[PWS1$AnalyticalResultsSign == "<"] <- 0
  sum_PWS1 <- aggregate(cbind(MRL,AnalyticalResultValue) ~ PWSID + Contaminant, data = PWS1, max)
  sum_PWS1$label <- ifelse(sum_PWS1$AnalyticalResultValue < sum_PWS1$MRL, 0, 1)
  PWS1_final <- dcast(sum_PWS1[,c(1,2,5)], PWSID ~ Contaminant)
  U3_df <- bind_rows(U3_df, PWS1_final)
}



# Assign label - D, ND
names(U3_df)

U3_df$label <- ifelse(rowSums(U3_df[,c(5,13,7,10,2,15:20)], na.rm = T) > 0, "D", "ND")
comb_data <- merge(mydata_dum, U3_df[,c(1,46)], by.x = "PWS ID", by.y = "PWSID", all = F)
table(comb_data$label)
# 4611 Detect, 1335 no detect
write.table(comb_data, "../TrainData/trainSet_all.csv", sep = "\t",row.names = F, quote = F) # save the training dataset


# prepare testing dataset from U5
U5_df <- data.frame()

for (PWS in unique(U5_data$PWSID)) {
  PWS1 <- U5_data[U5_data$PWSID %in% PWS,]
  PWS1$AnalyticalResultValue[PWS1$AnalyticalResultsSign == "<"] <- 0
  sum_PWS1 <- aggregate(cbind(MRL,AnalyticalResultValue) ~ PWSID + Contaminant, data = PWS1, max)
  sum_PWS1$label <- ifelse(sum_PWS1$AnalyticalResultValue < sum_PWS1$MRL, 0, 1)
  PWS1_final <- dcast(sum_PWS1[,c(1,2,5)], PWSID ~ Contaminant)
  U5_df <- bind_rows(U5_df, PWS1_final)
}
names(U5_df)
U5_df$label <- ifelse(rowSums(U5_df[,c(2:8,10:31)]) > 0, "D", "ND")
U5_detail <- merge(mydata_dum, U5_df[,c(1,32)], by.x = "PWS ID", by.y="PWSID", all=F)

# testSet <- merge(comb_data[,-59], U5_df[,c(1,32)], by.x = "PWS ID", by.y = "PWSID",all = F)
# testSet <- bind_rows(testSet, U5_detail[!(U5_detail$`PWS ID` %in% comb_data$`PWS ID`),])
write.table(U5_detail, "../TrainData/testSet_all.csv", sep = "\t",row.names = F, quote = F)


# Summarize U5 PFAS data
count_frequencies <- function(data) {
  frequency_list <- lapply(data, function(col) {
    as.data.frame(table(col))
  })
  
  # Adding column names for better readability
  for (i in seq_along(frequency_list)) {
    colnames(frequency_list[[i]]) <- c("Value", paste("Frequency_in", names(data)[i], sep = "_"))
  }
  
  # Combine all frequency dataframes into one
  result <- Reduce(function(x, y) merge(x, y, all = TRUE, by = "Value"), frequency_list)
  
  return(result)
}
U5_freq <- count_frequencies(U5_df[,-c(1,9,32)])
names(U5_freq)[2:30] <- names(U5_df)[-c(1,9,32)]

U5_freq_melt <- reshape2::melt(U5_freq)

ggplot(U5_freq_melt[U5_freq_melt$Value ==1,], aes(x = reorder(variable, value, decreasing=T), y = value))+
  geom_bar(stat = "identity")+
  labs(x = "",y = "Number of PWS")+
  theme_classic(base_size = 14)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

ggsave("../img/PWS counts in PFAS.pdf",height = 4,width = 6)

# make map
U5_df_loc <- merge(U5_df, U5_detail[,c(1,10,11)], by.x = "PWSID", by.y="PWS ID", all.x=T)
library(maps)

usa_map <- map_data("state")
ggplot() +
  # Draw the map of the USA
  geom_polygon(data = usa_map, aes(x = long, y = lat, group = group), 
               fill = "white", color = "black") +
  # Overlay data points using longitude and latitude
  geom_point(data = U5_df_loc[-921,], aes(x = LON, y = LAT, color=label), size = 0.5, alpha = 0.6) +
  # Set map coordinates for proper scaling
  coord_fixed(1.3) +
  # Labels for the plot
  labs(title = "UCMR 5 testing sites",
       x = "Longitude", y = "Latitude") +
  theme_minimal()
