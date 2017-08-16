## This is the script to clean up file HERMANDA_RARV2.csv
setwd("~/repos/Daniel_Herman_Aldosterone_2017_03/code/R/PA_analysis")

library(dplyr)
library(data.table)
library(tibble)
source("../common_anal/R_fxns.R")
source("../common_anal/RAR_fxns.R")

# read in raw data, and make some pre-process

res1 <- load_RAR_v3("/data/raw_data/PA/HERMANDA_RARV2.csv")
  


  
# Start funciton here
## (0)  Exclude ORDER_ITEM_CODE that are for AVS or urine specimens
res1 <- res1[!(ORDER_ITEM_CODE %in% c("C9009900", "C9009995", "C9009997", "Q19573", "83497A"))]


## (1) Clean up ORDER_ITEM_CODE == "C9010071"
c71 <- res1[(ORDER_ITEM_CODE == "C9010071")]

c71$Lab <- "unknown"
c71$Test <- "unknown"
c71$Method <- "unknown"

####### ORDER_GROUP == "Historical Order"
c71 <- c71[ORDER_GROUP == "Historical Order" & 
             RESULT_ITEM_CODE == "PLASMA RENIN ACTIVITY", 
           c("Lab","Test","Method") := list("unknown", "Renin Activity","unknown")]

c71 <- c71[ORDER_GROUP == "Historical Order" & 
             RESULT_ITEM_CODE == "RENIN",
           c("Lab","Test","Method") := list("unknown", "Renin Activity","unknown")]

# Exclude other "Historical Order"
c71 <- c71[(ORDER_GROUP == "Historical Order" & 
              RESULT_ITEM_CODE %in% c("PLASMA RENIN ACTIVITY", "RENIN")) | 
             ORDER_GROUP != "Historical Order"]


####### ORDER_GROUP != "Historical Order"
c71[ORDER_GROUP != "Historical Order" & 
      RESULT_RESOURCE ==  "QUEST" & 
      RESULT_ITEM_CODE == "PLASMA RENIN ACTIVITY, LC/MS/MS",
    c("Lab","Test","Method") := list("Quest", "Renin Activity","Plasma Renin Activity, LC/MS/MS")]

c71[ORDER_GROUP != "Historical Order" & 
      RESULT_RESOURCE ==  "QUEST" & 
      RESULT_ITEM_CODE != "PLASMA RENIN ACTIVITY, LC/MS/MS",
    c("Lab","Test","Method") := list("unknown", "Renin Activity","unknown")]


c71[ORDER_GROUP != "Historical Order" & 
      RESULT_RESOURCE !=  "QUEST" & 
      RESULT_ITEM_CODE == "RENIN" & 
      RESULT_RESOURCE == "LANCASTER GENERAL",
    c("Lab","Test","Method") := list("Quest", "Renin Activity","Renin Activity")]

c71[ORDER_GROUP != "Historical Order" & 
      RESULT_RESOURCE !=  "QUEST" & 
      RESULT_ITEM_CODE == "RENIN" & 
      RESULT_RESOURCE == "LABCORP",
    c("Lab","Test","Method") := list("Labcorp", "Renin Activity","Renin Activity, Plasma")]

c71[ORDER_GROUP != "Historical Order" & 
      RESULT_RESOURCE !=  "QUEST" & 
      RESULT_ITEM_CODE == "RENIN" & 
      !(RESULT_RESOURCE %in% c("LABCORP", "LANCASTER GENERAL")) & 
      UNIT_OF_MEASURE %in% c("NGMLHR", "ngmLhr","ng/mL/hr"),
    c("Lab","Test","Method") := list("ARUP", "Renin Activity","Renin Activity, Plasma")]

c71[ORDER_GROUP != "Historical Order" & 
      RESULT_RESOURCE !=  "QUEST" & 
      RESULT_ITEM_CODE == "RENIN" & 
      !(RESULT_RESOURCE %in% c("LABCORP", "LANCASTER GENERAL")) & 
      !(UNIT_OF_MEASURE %in% c("NGMLHR", "ngmLhr","ng/mL/hr")) & 
      grepl("arup", RESULT_TEXT),
    c("Lab","Test","Method") := list("ARUP", "Renin Activity","Renin Activity, Plasma")]

c71[ORDER_GROUP != "Historical Order" & 
      RESULT_RESOURCE !=  "QUEST" & 
      RESULT_ITEM_CODE == "RENIN" & 
      !(RESULT_RESOURCE %in% c("LABCORP", "LANCASTER GENERAL")) & 
      !(UNIT_OF_MEASURE %in% c("NGMLHR", "ngmLhr","ng/mL/hr")) & 
      !grepl("arup", RESULT_TEXT),
    c("Lab","Test","Method") := list("unknown", "Renin Activity","Renin Activity, Plasma")]




c71[ORDER_GROUP != "Historical Order" & 
      RESULT_RESOURCE !=  "QUEST" & 
      RESULT_ITEM_CODE == "RENIN ACTIVITY",
    c("Lab","Test","Method") := list("ARUP", "Renin Activity","Renin Activity, Plasma")]


c71[ORDER_GROUP != "Historical Order" & 
      RESULT_RESOURCE !=  "QUEST" & 
      RESULT_ITEM_CODE == "PLASMA RENIN ACTIVITY",
    c("Lab","Test","Method") := list("Quest", "Renin Activity","Renin Activity, Plasma")]


c71[ORDER_GROUP != "Historical Order" & 
      RESULT_RESOURCE !=  "QUEST" & 
      RESULT_ITEM_CODE == "PLASMA RENIN ACTIVITY",
    c("Lab","Test","Method") := list("Quest", "Renin Activity","Renin Activity, Plasma")]

c71 <-  c71[!(ORDER_GROUP != "Historical Order" & 
                RESULT_RESOURCE != "QUEST" & 
                !(RESULT_ITEM_CODE %in% c("RENIN", "RENIN ACTIVITY", "PLASMA RENIN ACTIVITY")))]

# #dat <- bind_rows(c71,
#                  ....)
# return(dat)
# }
#########
## End ##
#########

## (2) Clean up ORDER_ITEM_CODE == "C9010005"
c9105 <- res1[(ORDER_ITEM_CODE == "C9010005")]



c9105$Lab <- "unknown"
c9105$Test <- "unknown"
c9105$Method <- "unknown"


####### ORDER_GROUP == "Historical Order"
c9105[ORDER_GROUP == "Historical Order" & 
      RESULT_ITEM_CODE == "ALDOSTERONE",
    c("Lab","Test","Method") := list("unknown", "Aldosterone","unknown")]


# Exclude other "Historical Order"
c9105 <- c9105[!(ORDER_GROUP == "Historical Order" &
                 RESULT_ITEM_CODE != "ALDOSTERONE")]


####### ORDER_GROUP != "Historical Order"
c9105 <- c9105[ORDER_GROUP != "Historical Order" &
                 RESULT_ITEM_CODE == "ALDOSTERONE, LC/MS/MS",
               c("Lab","Test","Method") := list("Quest", "Aldosterone","Aldosteron, LC/MS/MS")]


c9105 <- c9105[ORDER_GROUP != "Historical Order" &
                 RESULT_ITEM_CODE == "ALDOSTERONE" &
                 RESULT_RESOURCE == "LABCORP",
               c("Lab","Test","Method") := list("Labcorp", "Aldosterone","Aldosteron, LC/MS/MS")]

c9105 <- c9105[ORDER_GROUP != "Historical Order" &
                 RESULT_ITEM_CODE == "ALDOSTERONE" &
                 RESULT_RESOURCE == "LANCASTER GENERAL",
               c("Lab","Test","Method") := list("Quest", "Aldosterone","Aldosteron, LC/MS/MS")]

c9105 <- c9105[ORDER_GROUP != "Historical Order" &
                 RESULT_ITEM_CODE == "ALDOSTERONE" &
                 RESULT_RESOURCE %in% c("UPHS CERNMILL LABORATORY", "CCH SUNQUEST", "PENN LAB", "UPHS LAB", "UPHS LABORATORY", "UPHS PAH LAB INCOMING", "UPHS PAH LABORATORY"),
               c("Lab","Test","Method") := list("ARUP", "Aldosterone","Quantitative Chemiluminescent Immunoassay")]


c9105 <- c9105[ORDER_GROUP != "Historical Order" &
                 RESULT_ITEM_CODE == "ALDOSTERONE" &
                 RESULT_RESOURCE == "" & 
                 grepl("arup", RESULT_TEXT),
               c("Lab","Test","Method") := list("ARUP", "Aldosterone","Quantitative Chemiluminescent Immunoassay")]


c9105 <- c9105[ORDER_GROUP != "Historical Order" &
                 RESULT_ITEM_CODE == "ALDOSTERONE" &
                 RESULT_RESOURCE == "" & 
                 !grepl("arup", RESULT_TEXT),
               c("Lab","Test","Method") := list("unknown", "Aldosterone","unknown")]

c9105 <- c9105[ORDER_GROUP == "Historical Order" |
                 RESULT_ITEM_CODE %in% c("ALDOSTERONE, LC/MS/MS", "ALDOSTERONE")]







##################    Save New cleaned data set
res1 <- rbind(c71, c9105)

save(res1, file = "RAR.RData")
