setwd("~/Desktop/Jiali/TAMU/environment/UCMR/")

# collected from state databases that have PFAS data, all tables downloaded and saved in the ./States/ directory
# CO, CA, GA, IL, IN, KY, MA, MD, ME, MI, MO, , NC, OH, OR, PA, RI, SC, UT, WA
# MT (data from one facility, ID cannot be identified)
# PR (no identifier)
# download EPA state data for extending more States

# EPA state PFAS data
library(readxl)
Statedata <- read_excel("States/EPA_statePFAS.xlsx", sheet = "Sheet1", col_names = TRUE)
length(unique(Statedata$PWSID)) # 10,102
# aggregate data, if one PWS historically detected, it is yes
state_df <- data.frame()

for (PWS in unique(Statedata$PWSID)) {
  PWS1 <- Statedata[Statedata$PWSID %in% PWS,]
  tryCatch({
    sum_PWS1 <- aggregate(Concentration ~ PWSID + Contaminant, data = PWS1, max)
    # Rest of your code that depends on the aggregated data
    sum_PWS1$Detect <- ifelse(sum_PWS1$Concentration > 0, 1, 0)
    PWS1_final <- dcast(sum_PWS1[,c(1,2,4)], PWSID ~ Contaminant)
    state_df <- bind_rows(state_df, PWS1_final)
    
  }, error = function(e) {
    if (grepl("no rows to aggregate", e$message)) {
      cat("Warning: No rows to aggregate. Skipping aggregation and continuing with the script.\n")
  
      # Assign a default value or handle the case when aggregation is not possible
      aggregated_data <- NULL
      
      # Rest of your code that doesn't depend on the aggregated data
      PWS1$Detect <- ifelse(PWS1$Concentration > 0, 1, 0)
      PWS1_final <- dcast(PWS1[,c(1,14,17)], PWSID ~ Contaminant)
      state_df <- bind_rows(state_df, PWS1_final)
      
    } else {
      # Rethrow the error if it's not related to the "no rows to aggregate" issue
      stop(e)
    }
  })
}

state_df$label <- ifelse(rowSums(state_df[,c(2:28)], na.rm = T) > 0, 1, 0)
table(state_df$label) # 6613/3488 (34.5% detection), UCMR5, 1234/3691(33.4%), UCMR3, 3% detect
intersect(U5_df$PWSID, state_df$PWSID) # 739
intersect(U5_df$PWSID, U3_df$PWSID) # 1919
intersect(U3_df$PWSID, state_df$PWSID) # 1171
intersect(c(U3_df$PWSID, U5_df$PWSID), state_df$PWSID) # 1467

intersect(MA$PWS.ID, state_df$PWSID)

# collected state data
CO <- read.csv("States/CO PFAS2020SamplingProject_AllResults.csv")
unique(CO$LOC_NAME)
filter_CO <- CO[grep("PWS",CO$LOC_PURPOSE),]
CO_dcast<- dcast(filter_CO[,c(1,10,13)], `LOC_NAME` ~ `CHEMICAL_NAME`, value.var = "RESULT_NUMERIC",fun.aggregate = max)
CO_dcast$label <- ifelse(rowSums(CO_dcast[,c(2:21)], na.rm = T) > 0, 1, 0)
CO_detail <- merge(mydata_dum, CO_dcast[,c(1,22)], by.x = "PWS ID", by.y="LOC_NAME", all=F)
write.table(CO_detail, "../TrainData/COtrain.csv", sep = "\t",row.names = F, quote = F)

# State data
MAdata <- read.csv("../States/MA Drinking Water.csv")
MAdata$Result[MAdata$Result == "ND"] = 0
MAdata$Result = as.numeric(MAdata$Result)
MAdata_dcast<- dcast(MAdata[,c(1,6,8)], `PWS.ID` ~ `Chemical.Name`, value.var = "Result",fun.aggregate = max)
MAdata_dcast$label <- ifelse(rowSums(MAdata_dcast[,c(2:20)]) > 0, 1, 0)
table(MAdata_dcast$label)
MAdata_dcast$PWS.ID <- paste0("MA",MAdata_dcast$PWS.ID)
MA_detail <- merge(mydata_dum, MAdata_dcast[,c(1,21)], by.x = "PWS ID", by.y="PWS.ID", all=F)
write.table(MA_detail, "../TrainData/MAtrain.csv", sep = "\t",row.names = F, quote = F)

MIdata <- read.csv("../States/MI Statewide_PFAS_Survey_of_Public_Water_Supplies.csv", header = T)
length(unique(MIdata$WSSN)) # 2447
length(intersect(paste0("MI",MIdata$WSSN), detail$`PWS ID`)) # 1212
length(intersect(paste0("MI",MIdata$WSSN), state_df$PWSID)) # 1355

NCdata <- read.csv("../States/NC PFAS2023.csv", header = T)
length(unique(NCdata$X.System.PWS.ID))
length(intersect(NCdata$X.System.PWS.ID,detail$`PWS ID`))
state_df$PWSID[grep("NC",state_df$PWSID)] # 356
intersect(NC1$X.System.PWS.ID, state_df$PWSID[grep("NC",state_df$PWSID)]) # 1

NCdata[NCdata=="ND"] = 0
library(dplyr)
NCdata[,7:12] <- NCdata[,7:12] %>% mutate_if(is.character,as.numeric)
NCdata$label <- ifelse(rowSums(NCdata[,c(7:12)]) > 0, 1, 0)
table(NCdata$label)
NC_detail <- merge(mydata_dum, NCdata[,c(1,13)], by.x = "PWS ID", by.y="X.System.PWS.ID", all=F)
write.table(NC_detail, "../TrainData/NCtrain.csv", sep = "\t",row.names = F, quote = F)

col <- grep("Result", names(MIdata), value = TRUE)
MIdata$label <- ifelse(rowSums(MIdata[,col], na.rm = T) > 0, 1, 0)
MIsum <- aggregate(label ~ WSSN, data = MIdata, max)
MIsum$WSSN <- paste0("MI",MIsum$WSSN)
MI_detail <- merge(mydata_dum, MIsum[,c(1,2)], by.x = "PWS ID", by.y="WSSN", all=F)
write.table(MI_detail, "../TrainData/MItrain.csv", sep = "\t",row.names = F, quote = F)

CA <- read.csv("States/CA pfas_monitoring_Q1Q2Q3Q42019-2020 07032024.csv")
CA$System.No. <- sprintf("%07d", CA$System.No.) # padding PWS IDs into the right length
CA$PWSID <- paste0("CA",CA$System.No.)
CA_dcast<- dcast(CA[,c(9,10,12)], PWSID ~ Chemical, value.var = "Finding..ng.L...",fun.aggregate = max)
CA_dcast[CA_dcast == -Inf] <- NA
CA_dcast$label <- ifelse(rowSums(CA_dcast[,c(2:24)], na.rm = T) > 0, 1, 0)
table(CA_dcast$label)
CA_detail <- merge(mydata_dum, CA_dcast[,c(1,25)], by.x = "PWS ID", by.y="PWSID", all=F)
write.table(CA_detail, "../TrainData/CAtrain.csv", sep = "\t",row.names = F, quote = F)

unique(CA$System.No.)
GA <- read.csv("States/GA 2021-2022 07162024.csv")
unique(GA$Water.System.No..)

IL <- read.csv("States/IL PFAS2020-2023 07162024.csv")
length(unique(IL$Water.System.No..)) # 1014

IN <- read.csv("States/IN_2024PFAS_6-22-2024.csv")
unique(IN$PWSID)
intersect(IN$PWSID, state_df$PWSID) # 0
IN$label <- ifelse(IN$X..PFAS.Detected.in..Source.Untreated.Water..== "Yes", 1, 0)
IN_detail <- merge(mydata_dum, IN[,c(1,9)], by.x = "PWS ID", by.y="PWSID", all=F)
write.table(IN_detail, "../TrainData/INtrain.csv", sep = "\t",row.names = F, quote = F)


KY <- read.csv("States/KY PFAS data.csv")
unique(KY$Location)

MD <- read.csv("States/MD PFAS report.csv")
unique(MD$PWSID) #127
MD[c(8:25)] <- lapply(MD[c(8:25)], as.numeric)
MD$label <- ifelse(rowSums(MD[,c(8:25)], na.rm = T) > 0, 1, 0)
MD_details <- merge(mydata_dum, MD[,c(2,26)], by.x = "PWS ID", by.y="PWSID", all=F)
MD_details <- unique(MD_details[order(MD_details$label, decreasing = T),])
intersect(MD$PWSID, state_df$PWSID) #75
write.table(MD_details, "../TrainData/MDtrain.csv", sep = "\t",row.names = F, quote = F)

ME <- read.csv("States/ME PFAS2007-2023 07162024.csv")
unique(ME$CURRENT.SITE.NAME) # no ID

MO <- read.csv("States/MO PFAS Sample Location Summaries.csv")
unique(MO$Public.Water.System.ID)
MO$Public.Water.System.ID # no PFAS detection results

NC1 <- read.csv("States/NC PFAS2023.csv")
unique(NC1$X.System.PWS.ID) # 531
NC2 <- read.csv("States/NC_2019-2021_07022024.csv")
unique(NC2$Location.Code) # 67

OH <- read.csv("States/OH PFAS_Sampling_Results 07032024.csv")
length(unique(OH$PWSID))
OH$PWSID <- gsub("\\s+","", OH$PWSID)
OH_dcast<- dcast(OH[,c(4,14,17)], PWSID ~ ANALYTE_CD, value.var = "SAMPLE_RESULT",fun.aggregate = max)
OH_dcast[c(2:8)] <- lapply(OH_dcast[c(2:8)], as.numeric)
OH_dcast$label <- ifelse(rowSums(OH_dcast[,c(2:8)], na.rm = T) > 0, 1, 0)
table(OH_dcast$label) #1373/106
OH_detail <- merge(mydata_dum, OH_dcast[,c(1,9)], by.x = "PWS ID", by.y="PWSID", all=F)
write.table(OH_detail, "../TrainData/OHtrain.csv", sep = "\t",row.names = F, quote = F)

OR <- read.csv("States/OR PFAS Data 07032024.csv")
length(unique(OR$PWS))
OR$PWS <- sprintf("%05d", OR$PWS) # search on EPA, all PWS in the table started as OR41
OR$PWS <- paste0("OR41",OR$PWS)
OR_dcast<- dcast(OR[,c(3,9,11)], PWS ~ Analyte.Abbreviation, value.var = "Result..ng.L.",fun.aggregate = max)
OR_dcast[c(2:26)] <- lapply(OR_dcast[c(2:26)], as.numeric)
OR_dcast$label <- ifelse(rowSums(OR_dcast[,c(2:26)], na.rm = T) > 0, 1, 0)
table(OR_dcast$label) #138/5
OR_detail <- merge(mydata_dum, OR_dcast[,c(1,27)], by.x = "PWS ID", by.y="PWS", all=F)
write.table(OR_detail, "../TrainData/ORtrain.csv", sep = "\t",row.names = F, quote = F)

PA <- read.csv("States/PA PFAS data.csv")
unique(PA$X.PWSID)
PA[c(7:13)] <- lapply(PA[c(7:13)], as.numeric)
PA$X.PWSID <- paste0("PA",PA$X.PWSID)
PA$label <- ifelse(rowSums(PA[,c(7:13)], na.rm = T) > 0, 1, 0)
table(PA$label) #61/35
PA_detail <- merge(mydata_dum, PA[,c(2,14)], by.x = "PWS ID", by.y="X.PWSID", all=F)
write.table(PA_detail, "../TrainData/PAtrain.csv", sep = "\t",row.names = F, quote = F)

RI <- read.csv("States/RI PFAS report.csv")
unique(RI$Public.water.system.name)
#assign PWSID by name
RI_ID <- merge(RI, detail[,1:2], by.x ="Public.water.system.name", by.y="PWS Name", all=F)
RI_ID$detected <- as.numeric(gsub("\\D", "", RI_ID$Highest.PFAS.level.detected))
RI_ID$label <- ifelse(RI_ID$detected > 0, 1, 0)
RI_ID$label[is.na(RI_ID$label)] <- 0
table(RI_ID$label) #95/57
RI_detail <- merge(mydata_dum, RI_ID[,c(13,15)], by.x = "PWS ID", by.y="PWS ID", all=F)
write.table(RI_detail, "../TrainData/RItrain.csv", sep = "\t",row.names = F, quote = F)

SC_g <- read.csv("States/SC Groundwater Well Data_Final.csv")
unique(SC_g$Sys_Name)
SC_g$PWSID <- gsub(".*\\(.*?(\\d+).*?\\).*", "\\1", SC_g$Sys_Name)
SC_g$PWSID <- paste0("SC",SC_g$PWSID)
SC_s <- read.csv("States/SC surface Drinking Water Plants_Final_0.csv")
SC_s$Sys_Num <- sprintf("%07d", SC_s$Sys_Num)
SC_s$PWSID <- paste0("SC",SC_s$Sys_Num)
unique(SC_s$Sys_Num)
SC_all <- rbind(SC_g[,c(5:22,24)], SC_s[,c(4:21,24)])
SC_all[c(1:18)] <- lapply(SC_all[c(1:18)], as.numeric)
SC_all$label <- ifelse(rowSums(SC_all[,c(1:18)], na.rm = T) > 0, 1, 0)
table(SC_all$label) #729/196
SC_detail <- merge(mydata_dum, SC_all[,c(19,20)], by.x = "PWS ID", by.y="PWSID", all=F)
SC_detail <- unique(SC_detail[order(SC_detail$label, decreasing = T),])
write.table(SC_detail, "../TrainData/SCtrain.csv", sep = "\t",row.names = F, quote = F)

UT <- read.csv("../States/UT PFAS DEQ Data.csv")
unique(UT$System.Number)
UT$System.Number <- sprintf("%05d", UT$System.Number)
UT$PWSID <- paste0("UTAH",UT$System.Number)
table(UT$Note) # all PWS are non-detect
UT$label <- 0
UT_detail <- merge(mydata_dum, UT[,c(34,35)], by.x = "PWS ID", by.y="PWSID", all=F)
UT_detail <- unique(UT_detail[order(UT_detail$label, decreasing = T),])
write.table(UT_detail, "../TrainData/UTtrain.csv", sep = "\t",row.names = F, quote = F)


WA <- read.csv("States/WA PFAS data.csv")
unique(WA$Water.System.ID)
WA$Water.System.ID <- sprintf("%05d", as.numeric(WA$Water.System.ID))
WA$PWSID <- paste0("WA53", WA$Water.System.ID)
WA$detected <- as.numeric(gsub("ng/L", "", WA$Result))
WA_dcast<- dcast(WA[,c(9,13,14)], PWSID ~ PFAS.Measured, value.var = "detected",fun.aggregate = max)
WA_dcast[WA_dcast == -Inf] <- NA
WA_dcast$label <- ifelse(rowSums(WA_dcast[,c(2:30)], na.rm = T) > 0, 1, 0)
table(WA_dcast$label) #1166/146
WA_detail <- merge(mydata_dum, WA_dcast[,c(1,31)], by.x = "PWS ID", by.y="PWSID", all=F)
write.table(WA_detail, "../TrainData/WAtrain.csv", sep = "\t",row.names = F, quote = F)

length(intersect(state_df$PWSID, c(CO_detail$`PWS ID`,CA_detail$`PWS ID`,IN_detail$`PWS ID`,MA_detail$PWS.ID,
                            MD_details$`PWS ID`,MI_detail$`PWS ID`,NC_detail$`PWS ID`,OH_detail$`PWS ID`,OR$PWS,
                            PA_detail$`PWS ID`,RI_detail$`PWS ID`,SC_detail$`PWS ID`,UT_detail$`PWS ID`,WA_detail$`PWS ID`)))
          