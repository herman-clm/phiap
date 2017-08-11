source("../common_anal/R_fxns.R")
library(assertthat)

pre_rar <- function(dat_file_in="/data/raw_data/PA/HERMANDA_RARV2.csv",
                    blood_only=FALSE){
  #' This is a pre-process function, used to read in raw laboratory data for renin and aldosterone testing, and to make some changes in date, adding provider field, including useful ORDER ITEM CODE groups
  #' @param dat_file_in string Raw data text file location
  #' @param blood_only Boolean If true, only use serum/plasma test codes
  #' @return res1 data.table pre-processed test results 
  
  res1 <- fread(dat_file_in, stringsAsFactors = FALSE, header = TRUE)
  
  # Convert to dates
  res1$ORDER_DATE <- as.POSIXct(res1$ORDER_DATE, format = "%Y-%m-%d %H:%M:%S")
  res1$ORDER_START_DATE <- as.POSIXct(res1$ORDER_START_DATE, format = '%Y-%m-%d %H:%M:%S')
  res1$BIRTH_DATE <- as.POSIXct(res1$BIRTH_DATE, format = '%Y-%m-%d %H:%M:%S')
  res1$RESULT_DATE <- as.POSIXct(res1$RESULT_DATE, format = '%Y-%m-%d %H:%M:%S')
  res1$PERFORMED_DATE <- as.POSIXct(res1$PERFORMED_DATE, format = '%Y-%m-%d %H:%M:%S')
  res1$ENC_DATE <- as.POSIXct(res1$ENC_DATE, format = '%Y-%m-%d %H:%M:%S')
  
  # Refine columns
  res1$GENDER_MASTER_CODE <- as.factor(res1$GENDER_MASTER_CODE)
  res1$EMPI <- as.character(res1$EMPI)
  res1$Age <- floor(as.numeric(res1$ORDER_START_DATE - res1$BIRTH_DATE)/365.25)
  
  # Create provider field that defaults to ORDERING_PROV, and if empty ADMITTING_PROV
  res1$prov <- res1$ORDERING_PROV
  res1$prov[which(res1$prov=="")] <- res1$ADMITTING_PROV[which(res1$prov=="")]  
  
  # Handle non-numeric Results (Convert less than strings to LRR * 0.5)
  res1$Val <- numericize(res1$RESULT_VALUE, adjust_down = 0.5)
  
  res1 %<>%
    filter(!duplicated(res1))
  warning("TODO: Duplicated rows in pre_rar input: need to explain")
  
  # Test (What is duplicated -- handle?)
  assert_that(nrow(res1) == nrow(res1 %>% distinct))
  
  if (blood_only) { # Exclude ORDER_ITEM_CODE that are for AVS or urine specimens
    res1 <- res1[!(ORDER_ITEM_CODE %in% c("C9009900", "C9009995", "C9009997", "Q19573", "83497A"))]
  }
  
  return(res1)
  
}

