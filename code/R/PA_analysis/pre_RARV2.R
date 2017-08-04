pre_rar <- function(dt){
  #' This is a pre-process function, used to read in raw RAR data, and make some changes in date, adding provider field, including useful ORDER ITEM CODE groups
  #' @param dt Raw data location
  #' @return res1, pre-processed test results 
  
  
  
  res1 <- fread(dt, stringsAsFactors = FALSE, header = TRUE)
  
  
  res1$ORDER_DATE <- as.POSIXct(res1$ORDER_DATE, format = "%Y-%m-%d %H:%M:%S")
  res1$ORDER_START_DATE <- as.POSIXct(res1$ORDER_START_DATE, format = '%Y-%m-%d %H:%M:%S')
  res1$BIRTH_DATE <- as.POSIXct(res1$BIRTH_DATE, format = '%Y-%m-%d %H:%M:%S')
  res1$RESULT_DATE <- as.POSIXct(res1$RESULT_DATE, format = '%Y-%m-%d %H:%M:%S')
  res1$PERFORMED_DATE <- as.POSIXct(res1$PERFORMED_DATE, format = '%Y-%m-%d %H:%M:%S')
  res1$ENC_DATE <- as.POSIXct(res1$ENC_DATE, format = '%Y-%m-%d %H:%M:%S')
  
  res1$GENDER_MASTER_CODE <- as.factor(res1$GENDER_MASTER_CODE)
  res1$EMPI <- as.character(res1$EMPI)
  
  
  # Create provider field that defaults to ORDERING_PROV, and if empty ADMITTING_PROV
  
  res1$prov <- res1$ORDERING_PROV
  res1$prov[which(res1$prov=="")] <- res1$ADMITTING_PROV[which(res1$prov=="")]  
  
  
  res1$Age <- floor(as.numeric(res1$ORDER_START_DATE - res1$BIRTH_DATE)/365.25)
  
  
  ## (0)  Exclude ORDER_ITEM_CODE that are for AVS or urine specimens
  res1 <- res1[!(ORDER_ITEM_CODE %in% c("C9009900", "C9009995", "C9009997", "Q19573", "83497A"))]
  
  return(res1)
  
}

