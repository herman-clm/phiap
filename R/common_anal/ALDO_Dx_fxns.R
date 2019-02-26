suppressMessages(library(data.table))
suppressMessages(library(assertthat))

prep_aldo_dx <- function(dat_file = "/data/raw_data/PA/HERMANDA_ALDO_DX_V2.csv"){
  #' This is a pre-process function, used to read in raw ALDO Dx data, and to make some changes in date
  #' @param dat_file string Raw data text file location
  #' @return dx tibble pre-processed test results 
  
  dx <- fread(file = dat_file, stringsAsFactors = FALSE, header = TRUE)
  
  
  # Change EMPI, MRN into character
  dx$EMPI <- as.character(dx$EMPI)
  dx$MRN <- as.character(dx$MRN)
  dx$PK_ENCOUNTER_ID <- as.character(dx$PK_ENCOUNTER_ID)
  dx$HAR_NUMBER <- as.character(dx$HAR_NUMBER)
  dx$NPI_ADMIT <- as.character(dx$NPI_ADMIT)
  dx$NPI_ATTEND <- as.character(dx$NPI_ATTEND)
  
  
  
  # Change PRIMARY_YN into Logical
  dx$PRIMARY_YN <- ifelse(dx$PRIMARY_YN == 1, TRUE, FALSE)
  dx$MASTER_LOCATION_FACILITY_YN <- ifelse(dx$MASTER_LOCATION_FACILITY_YN== 1, TRUE, FALSE)
  dx$MASTER_LOCATION_CLINIC_YN <- ifelse(dx$MASTER_LOCATION_CLINIC_YN == 1, TRUE, FALSE)
  
  # Modify Date
  dx$CODING_DATE <- as.POSIXct(dx$CODING_DATE, format = "%Y-%m-%d %H:%M:%S")
  dx$SOURCE_LAST_UPDATE_DATE <- as.POSIXct(dx$SOURCE_LAST_UPDATE_DATE, format = "%Y-%m-%d %H:%M:%S")
  dx$ENC_DATE <- as.POSIXct(dx$ENC_DATE, format = "%Y-%m-%d %H:%M:%S")
  
  
  # dx: from data.table to tibble
  dx <- as.tibble(dx)
  
  return(dx)
}


agg_dx_pk_enc <- function(dat, min_Dx = 2){
  #' This function is used to aggregate Dx data into PK_ENCOUNTER_ID level
  #' @param dat tibble Dx data after pre-process by prep_aldo_dx function
  #' @param min_Dx numeric Minimal (included) Dx to filter on
  #' @return dx tibble PK_ENCOUNTER_ID leve Dx data file 
  
  
  # a list for EMPI who meet the criteria
  empi <- dat %>% group_by(EMPI) %>%
    summarise(N=n_distinct(PK_ENCOUNTER_ID)) %>%
    filter(N >= min_Dx)
  
  # filter out those patients
  dat %<>% filter(EMPI %in% empi$EMPI)
  
  # Check Duplicated: if any, throw a WARNING
  # then remove duplicated ones in all fields and move on
  if(sum(duplicated(dat)) != 0){
    dat <- dat[!(duplicated(dat)), ]
    warning("DUPLICATED ROWS DETECTED. REMOVED TO MOVE ON. CHECK BACK LATER")
  }
  
  
  
  # DX_TYPE includes "Admitting" and "Final" for one PK_ENCOUNTER_ID
  # remove "Admitting"
  # If DX_TYPE is only "Admitting" for some PK_ENCOUNTER_ID, throw a WARNING,
  # and keep "Admitting"
  admitting_pk <- dat %>% filter(DX_TYPE == "Admitting") %>%
    distinct(PK_ENCOUNTER_ID)
  
  admit_final <- dat %>% filter(PK_ENCOUNTER_ID %in% admitting_pk$PK_ENCOUNTER_ID) %>%
          filter(DX_TYPE %in% c("Final", "Admitting")) %>%
          group_by(PK_ENCOUNTER_ID) %>%
          summarise(N = n_distinct(DX_TYPE)) %>%
          filter(N == 2)
  if(dim(admit_final)[1] != dim(admitting_pk)[1]){
    warning("DX_TYPE is only Admitting, no Final. Admitting Kept")
  }
  
  dat <- dat %>% filter(!(PK_ENCOUNTER_ID %in% admit_final$PK_ENCOUNTER_ID & DX_TYPE == "Admitting"))
  
  
  
    
  
  # check Duplicated Dx in each PK_ENCOUNTER_ID
  if(dup_dx_pk_YN(dat=dat)$res){
    warning("Duplicated Dx's in PK_ENCOUNTER_ID level")
  }
  # Apply Desicion to remove dups
  dx <- dup_dx_pk_RM(dat=dat)
  
  return(dx)
}







dup_dx_pk_YN <- function(dat){
  #' This function is used to check duplicated Dx's in PK_ENCOUNTER_ID level
  #' @param dat tibble A data table to check dups
  #' @return res logic The indicator for whether duplicates are found:
  #'  TRUE for Duplicates found; FALSE for no duplicates
  #' @return ck tibble A table for counts in PK_ENCOUNTER_ID level:
  #' N is total number
  #' dis_N is the number for distinct Dx's

  ck <- dat %>% group_by(PK_ENCOUNTER_ID) %>%
    summarise(N = n())
  ck$dis_N <- dat %>% group_by(PK_ENCOUNTER_ID) %>%
    summarise(N = n_distinct(CODE)) %>%
    .$N
  
  if(sum(ck$N != ck$dis_N) >= 1){
    return(list(res=TRUE, ck = ck))
  }else{
    return(list(res=FALSE, ck = ck))
  }
  
}


dup_dx_pk_RM <- function(dat){
  #' This function is used to check duplicated Dx's in PK_ENCOUNTER_ID level
  #' Decision Rule: For each PK_ENCOUNTER_ID, if everything is the same for one CODE (duplicated Dx's), except for DX_TYPE, COMMENTS, PRIMARY_YN, and DX_SEQUENCE, then:
  #' (1) Select DX_TYPE == "Final", if exists. If not,
  #' (2) Select PRIMARY_YN == TRUE. If same, then
  #' (3) Select the one with smaller DX_SEQUENCE. If same, then,
  #' (4) Check COMMENTS, select the one which is not null. If same (case-insensative), select the one with all Caps. If not same, throw an ERROR
  #' @param dat tibble A data table to remove dups
  #' @return dx tibble A table with Dup Dx's in PK_ENCOUNTER_ID Level removed

  
  ck <- dup_dx_pk_YN(dat)
  tp <- ck[[2]] %>% filter(N != dis_N) 
  
  dup_dx <- dat %>% filter(PK_ENCOUNTER_ID %in% tp$PK_ENCOUNTER_ID) 
  dup_dx$PK_DX <- paste(dup_dx$PK_ENCOUNTER_ID, dup_dx$CODE)
  
  
  dup_dx_rm <- data.frame()
  for(i in unique(dup_dx$PK_DX)){
    temp <- dup_dx %>% filter(PK_DX == i)
    
    # (1) DX_TYPE == "Final"
    if(sum(temp$DX_TYPE == "Final") >= 1){
      temp %<>% filter(DX_TYPE == "Final")
    }
    
    # (2) PRIMARY_YN == TRUE
    if(sum(temp$PRIMARY_YN == TRUE) >= 1){
      temp %<>% filter(PRIMARY_YN == TRUE)
    }
    
    # (3) smaller DX_SEQUENCE
    if(sum(is.na(temp$DX_SEQUENCE)) < dim(temp)[1]){
      min_dx_seq <- min(temp$DX_SEQUENCE, na.rm = TRUE)
      temp %<>% filter(DX_SEQUENCE == min_dx_seq)
    }
    
    
    # (4) COMMENTS
    if(sum(temp$COMMENTS == "") < dim(temp)[1]){
      temp %<>% filter(COMMENTS != "")
      
      ## if COMMENTS are the same, select first after descent arranging, 
      ## i.e., select UPPER LETTERS first
      assert_that(length(unique(toupper(temp$COMMENTS))) == 1, msg = "Mismatch COMMENTS")
      
      if(length(unique(toupper(temp$COMMENTS))) == 1){
        temp %<>%  arrange(desc(COMMENTS)) %>% 
          filter(row_number() == 1)
      }
      
      
    }else{
      ## Here, COMMENTS are all NULL, so pick the first row
      temp %<>% filter(row_number() == 1)
    }
    
    temp %<>% select(-PK_DX)
    
    dup_dx_rm <- rbind(dup_dx_rm, temp)
  }
  
  dup_dx %<>% select(-PK_DX)
  
  dat <- setdiff(dat, dup_dx)
  dat <- rbind(dat, dup_dx_rm)
  
  return(dat)
  
}
