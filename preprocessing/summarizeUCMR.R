setwd("~/Desktop/Jiali/TAMU/environment/UCMR/Rscript/")
library(readxl)
library(VennDiagram)
library(RColorBrewer)

# read the excel file
fileName <- "../UCMR 1-5 summary.xlsx"
excel_sheets(fileName)
UCMR1 <- read_excel(fileName, sheet = 1)
UCMR2 <- read_excel(fileName, sheet = 2)
UCMR3 <- read_excel(fileName, sheet = 3)
UCMR4 <- read_excel(fileName, sheet = 4)
UCMR5 <- read_excel(fileName, sheet = 5)

UCMR1$Contaminant
UCMR2$Contaminant
UCMR3$Contaminant
UCMR4$Contaminant
UCMR5$Contaminant

myCol <- brewer.pal(5, "Dark2")

#Make the plot
venn.diagram(
  x = list( UCMR1$Contaminant, UCMR2$Contaminant, UCMR3$Contaminant,UCMR4$Contaminant,UCMR5$Contaminant),
  category.names = c("UCMR1" , "UCMR2" , "UCMR3","UCMR4","UCMR5"),
  filename = '../vennUCMR.png',
  output = TRUE ,
  imagetype="png" ,
  height = 650 , 
  width = 650 , 
  resolution = 300,
  compression = "lzw",
  col = myCol,
  fill = myCol,
  lwd = 1,
  cex = 0.5,
  fontfamily = "sans",
  cat.cex = 0.4,
  cat.default.pos = "text",
  margin = 0.1
)

# load data
U2_data <- read.csv("../ucmr2_occurrencedata_jan12/UCMR2_All_OccurrenceData_Jan12.txt", sep = "\t", quote = "", 
                    row.names = NULL, stringsAsFactors = FALSE)

U3_data <- read.csv("../ucmr-3-occurrence-data/UCMR3_All.txt", sep = "\t", quote = "", 
                    row.names = NULL, stringsAsFactors = FALSE)

U4_data <- read.csv("../ucmr_4_occurrence_data/UCMR4_All.txt",sep = "\t", quote = "", 
                    row.names = NULL, stringsAsFactors = FALSE, fileEncoding = "latin1")
U5_data <- read.csv("../ucmr5-occurrence-data/UCMR5_All.txt", sep = "\t", quote = "", 
                    row.names = NULL, stringsAsFactors = FALSE,fileEncoding = "latin1")

length(unique(U2_data$PWSID))
length(unique(U3_data$PWSID))
length(unique(U4_data$PWSID))
length(unique(U5_data$PWSID))

length(intersect(U2_data$PWSID, U3_data$PWSID))
length(intersect(U2_data$PWSID, U4_data$PWSID))
length(intersect(U2_data$PWSID, U5_data$PWSID))
length(intersect(U3_data$PWSID, U4_data$PWSID))
length(intersect(U3_data$PWSID, U5_data$PWSID))
length(intersect(U4_data$PWSID, U5_data$PWSID))

names(U3_data)
table(U3_data$State[grep("PF",U3_data$Contaminant)])
U3_PFOA <- U3_data[grep("PFOA",U3_data$Contaminant),]
filterNA_U3_PFOA <- U3_PFOA[!(is.na(U3_PFOA$AnalyticalResultValue)),]

table(U5_data$State[-(grep("lithium",U5_data$Contaminant))])

## EPA water system 
EPAfile <- "../States/EPA Water System Detail_20240405.xlsx"
excel_sheets(EPAfile)
PWS <- read_excel(EPAfile, sheet = 1, skip = 4) # skip first four lines
table(PWS$`Primacy Agency`)
length(intersect(PWS$`PWS ID`,U3_data$PWSID))
length(intersect(PWS$`PWS ID`,U5_data$PWSID))
