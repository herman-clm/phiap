## This is the script to clean up file HERMANDA_RARV2.csv
setwd("~/repos/Daniel_Herman_Aldosterone_2017_03/code/R/PA_analysis")

library(dplyr)
library(data.table)
library(tibble)
source("/home/dingxi/repos/Daniel_Herman_Aldosterone_2017_03/code/R/common_anal/R_fxns.R")


res1 <- fread("/data/raw_data/PA/HERMANDA_RARV2.csv", stringsAsFactors = FALSE, header = TRUE)


res1$ORDER_START_DATE <- as.POSIXct(res1$ORDER_START_DATE, format = '%Y-%m-%d %H:%M:%S')
res1$BIRTH_DATE <- as.POSIXct(res1$BIRTH_DATE, format = '%Y-%m-%d %H:%M:%S')
res1$RESULT_DATE <- as.POSIXct(res1$RESULT_DATE, format = '%Y-%m-%d %H:%M:%S')
res1$PERFORMED_DATE <- as.POSIXct(res1$PERFORMED_DATE, format = '%Y-%m-%d %H:%M:%S')


res1$GENDER_MASTER_CODE <- as.factor(res1$GENDER_MASTER_CODE)
res1$EMPI <- as.character(res1$EMPI)


# Create provider field that defaults to ORDERING_PROV, and if empty ADMITTING_PROV

res1$prov <- res1$ORDERING_PROV
res1$prov[which(res1$prov=="")] <- res1$ADMITTING_PROV[which(res1$prov=="")]  


res1$Age <- floor(as.numeric(res1$ORDER_START_DATE - res1$BIRTH_DATE)/365.25)



## (0)  Exclude ORDER_ITEM_CODE in c("")
res1 <- res1[!(ORDER_ITEM_CODE %in% c("C9009900", "C9009995", "C9009997", "Q19573", "83497A"))]


## (1) Clean up ORDER_ITEM_CODE == "C9010071"
c71 <- res1[(ORDER_ITEM_CODE == "C9010071")]

c71$Lab <- "unknown"
c71$Test <- "unknown"
c71$Method <- "unknown"

####### ORDER_GROUP == "Historical Order"
c71 <- c71[ORDER_GROUP == "Historical Order" & RESULT_ITEM_CODE == "PLASMA RENIN ACTIVITY", 
           c("Lab","Test","Method") := list("unknown", "Renin Activity","unknown")]

c71 <- c71[ORDER_GROUP == "Historical Order" & RESULT_ITEM_CODE == "RENIN",
           c("Lab","Test","Method") := list("unknown", "Renin Activity","unknown")]

# Exclude other "Historical Order"
c71 <- c71[(ORDER_GROUP == "Historical Order" & RESULT_ITEM_CODE %in% c("PLASMA RENIN ACTIVITY", "RENIN")) | ORDER_GROUP != "Historical Order"]


####### ORDER_GROUP != "Historical Order"
c71[ORDER_GROUP != "Historical Order" & RESULT_RESOURCE ==  "QUEST" & RESULT_ITEM_CODE == "PLASMA RENIN ACTIVITY, LC/MS/MS",
    c("Lab","Test","Method") := list("Quest", "Renin Activity","Plasma Renin Activity, LC/MS/MS")]

c71[ORDER_GROUP != "Historical Order" & RESULT_RESOURCE ==  "QUEST" & RESULT_ITEM_CODE != "PLASMA RENIN ACTIVITY, LC/MS/MS",
    c("Lab","Test","Method") := list("unknown", "Renin","unknown")]


c71[ORDER_GROUP != "Historical Order" & RESULT_RESOURCE !=  "QUEST" & RESULT_ITEM_CODE == "RENIN" & RESULT_RESOURCE == "LANCASTER GENERAL",
    c("Lab","Test","Method") := list("Quest", "Renin Activity","Renin Activity")]

c71[ORDER_GROUP != "Historical Order" & RESULT_RESOURCE !=  "QUEST" & RESULT_ITEM_CODE == "RENIN" & RESULT_RESOURCE == "LABCORP",
    c("Lab","Test","Method") := list("Labcorp", "Renin Activity","Renin Activity, Plasma")]

c71[ORDER_GROUP != "Historical Order" & RESULT_RESOURCE !=  "QUEST" & RESULT_ITEM_CODE == "RENIN" & !(RESULT_RESOURCE %in% c("LABCORP", "LANCASTER GENERAL")) & UNIT_OF_MEASURE %in% c("NGMLHR", "ngmLhr","ng/mL/hr"),
    c("Lab","Test","Method") := list("ARUP", "Renin Activity","Renin Activity, Plasma")]

c71[ORDER_GROUP != "Historical Order" & RESULT_RESOURCE !=  "QUEST" & RESULT_ITEM_CODE == "RENIN" & !(RESULT_RESOURCE %in% c("LABCORP", "LANCASTER GENERAL")) & !(UNIT_OF_MEASURE %in% c("NGMLHR", "ngmLhr","ng/mL/hr")) & grepl("arup", RESULT_TEXT),
    c("Lab","Test","Method") := list("ARUP", "Renin Activity","Renin Activity, Plasma")]

c71[ORDER_GROUP != "Historical Order" & RESULT_RESOURCE !=  "QUEST" & RESULT_ITEM_CODE == "RENIN" & !(RESULT_RESOURCE %in% c("LABCORP", "LANCASTER GENERAL")) & !(UNIT_OF_MEASURE %in% c("NGMLHR", "ngmLhr","ng/mL/hr")) & !grepl("arup", RESULT_TEXT),
    c("Lab","Test","Method") := list("unknown", "Renin Activity","Renin Activity, Plasma")]




c71[ORDER_GROUP != "Historical Order" & RESULT_RESOURCE !=  "QUEST" & RESULT_ITEM_CODE == "RENIN ACTIVITY",
    c("Lab","Test","Method") := list("ARUP", "Renin Activity","Renin Activity, Plasma")]


c71[ORDER_GROUP != "Historical Order" & RESULT_RESOURCE !=  "QUEST" & RESULT_ITEM_CODE == "PLASMA RENIN ACTIVITY",
    c("Lab","Test","Method") := list("Quest", "Renin Activity","Renin Activity, Plasma")]


c71[ORDER_GROUP != "Historical Order" & RESULT_RESOURCE !=  "QUEST" & RESULT_ITEM_CODE == "PLASMA RENIN ACTIVITY",
    c("Lab","Test","Method") := list("Quest", "Renin Activity","Renin Activity, Plasma")]

c71 <-  c71[!(ORDER_GROUP != "Historical Order" & RESULT_RESOURCE != "QUEST" & !(RESULT_ITEM_CODE %in% c("RENIN", "RENIN ACTIVITY", "PLASMA RENIN ACTIVITY")))]

#########
## End ##
#########

## (2) Clean up ORDER_ITEM_CODE == "C9010005"

