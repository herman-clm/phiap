suppressMessages(library(assertthat))
suppressMessages(library(data.table))
suppressMessages(library(lubridate))
suppressMessages(library(tidyr))


source("common_anal/R_fxns.R")

load_RAR <- function() {
  #' Load in the renin aldosterone, and other laboratory data
  #' @return data.frame 
  
  res1 <- load_RAR_specific()  # load renin and aldosterone
  others <- load_RAR_surround()  # load other labs
  
  # Add vitals
  res_vitals <- load_RAR_vitals()  # load vital signs
  
  res1 <- merge(res1, res_vitals, 
                allow.cartesian=TRUE,
                by=c("EMPI"), all.x=T)
  
  # Capture RAR rows without vitals
  t1 <- res1 %>% filter(is.na(ENC_DATE))
  
  # When multiple vitals, pick a single set closest to the RAR measurement 
  # TODO: generalize to summary statistics
  res1 %<>% 
    group_by(EMPI, RESULT_ITEM_CODE, ORDER_START_DATE) %>%
    slice(which.min(abs(ENC_DATE - ORDER_START_DATE))[1]) %>%
    ungroup() 
  
  # Combine single vital and no vital data together
  res1 %<>% rbind(t1)
  
  # Add meds
  a <- load_RAR_meds()
  
  #pick highest
  a %<>%
    group_by(PK_ENCOUNTER_ID, DRUG_CLASS) %>%
    summarize(DOSE = max(DOSE, na.rm=T)) %>% ungroup()
  
  res1 <- merge(res1, 
                distinct(a[, c("PK_ENCOUNTER_ID", "DRUG_CLASS", "DOSE")]), 
                by="PK_ENCOUNTER_ID", all.x=T)
  res1 %<>% 
    spread(DRUG_CLASS, DOSE, fill=0)
  
  list(others=others, res1=res1)
}




load_RAR_specific <- function(file_in = "/data/raw_data/PA/select___from_RAR.csv") {
  #' Load in RAR.csv file that contains renin and aldosterone laboratory results
  #' @return data.frame
  
  res1 <- fread(file=file_in, 
                stringsAsFactors = F, h=T)

  res1$RESULT_ITEM_CODE <- gsub(" |,|\\/|\\#", "_", res1$RESULT_ITEM_CODE )  # replace extraneous characters
  res1$ORDER_NAME <- gsub(" ", "_", res1$ORDER_NAME )  # replace spaces
  
  # Handle dates
  res1$ORDER_START_DATE <- as.POSIXct(res1$ORDER_START_DATE, format = "%d-%b-%Y %H:%M:%S")
  res1$BIRTH_DATE <- as.POSIXct(res1$BIRTH_DATE, format = "%d-%b-%Y %H:%M:%S")
  res1$RESULT_DATE <- as.POSIXct(res1$RESULT_DATE, format = "%d-%b-%Y %H:%M:%S")
  res1$PERFORMED_DATE <- as.POSIXct(res1$PERFORMED_DATE, format = "%d-%b-%Y %H:%M:%S")
  
  res1$GENDER_MASTER_CODE <- as.factor(res1$GENDER_MASTER_CODE)
  res1$EMPI <- as.character(res1$EMPI)
  
  # Create provider field that defaults to ORDERING_PROV, and if empty ADMITTING_PROV
  res1$prov <- res1$ORDERING_PROV
  res1$prov[which(res1$prov=="")] <- res1$ADMITTING_PROV[which(res1$prov=="")]  
  
  res1$Age <- floor(as.numeric(res1$ORDER_START_DATE - res1$BIRTH_DATE)/365.25)  # Calc Age
  
  res1$Val <- numericize(res1$RESULT_VALUE, adjust_down = 0.5)  # Convert less than strings to LRR * 0.5
  
  res1 %<>% 
    dplyr::select(-BIRTH_DATE, -ORDERING_PROV, -ADMITTING_PROV) %>%
    distinct()
  
  res1
}

load_RAR_surround <- function() {
  #' Load in RAR_SURROUND.csv, which contains laboratory results
  #' @return data.frame
  
  res <- read.csv("/data/raw_data/PA/select___from_RAR_surround.csv", as.is = T, h=T, nrows = Inf)
  
  res$EMPI <- as.character(res$EMPI)
  
  res$RESULT_ITEM_CODE <- gsub(" |,|\\/|\\#", "_", res$RESULT_ITEM_CODE )  # Convert complex characters
  res$ORDER_NAME <- gsub(" ", "_", res$ORDER_NAME ) # Convert spaces
  
  # Handle dates
  res$ORDER_START_DATE <- as.POSIXct(res$ORDER_START_DATE, format = "%d-%b-%Y %H:%M:%S")
  res$RESULT_DATE <- as.POSIXct(res$RESULT_DATE, format = "%d-%b-%Y %H:%M:%S")
  res$RAR_DT <- as.POSIXct(res$RAR_DT, format = "%d-%b-%Y %H:%M:%S")
  res$PERFORMED_DATE <- as.POSIXct(res$PERFORMED_DATE, format = "%d-%b-%Y %H:%M:%S")
  
  # Pick a subset of tests to keep, based on frequency
  test_freq <- table(res$RESULT_ITEM_CODE)
  freq_tests <- c(names(test_freq)[which(test_freq > 3000 & !names(test_freq) %in% c("GLUCOSE_POINT_OF_CARE", "ANION_GAP"))], 
                  c("RENIN_ACTIVITY", "RENIN", "ALDOSTERONE", "ALDOSTERONE_LC_MS_MS"))
  others <- res %>% 
    filter(RESULT_ITEM_CODE %in% freq_tests) %>%
    distinct()
  
  others$Val <- numericize(others$RESULT_VALUE, adjust_down = 0.5)  # Handle < and > results
  
  others
}

consolidate_rows <- function(x) {
  #' Consolidate medication rows. Merge consecutive medication prescriptions together.
  #' - Estimate ORDER_STOP_DATE as START_DATE  + 1 month per refill + 1
  #' @param x data.frame with medication prescription data for a single patient-drug combination
  #' @return 
  #' 
  # TODO: Note that REFILLS are incorrect, because assuming a 1 month course
  
  i <- 1
  while(i < nrow(x)) {
    if(x$group[i] == x$group[i+1]) {
      next_stop <- x$ORDER_STOP_DATE[i+1]
      if (!is.na(next_stop)) {
        x$ORDER_STOP_DATE[i] <- next_stop
      } else { # is NA
        refills <- x$REFILLS[i+1]
        if(is.na(refills)) {  refills <- 3   }
        x$ORDER_STOP_DATE[i] <- x$ORDER_START_DATE[i+1] + ((refills + 1) * 2635200) # Assume refill are monthly (convert refills + 1 -> month)
      }
      x <- x[-(i+1),]
    } else {
      x$ORDER_STOP_DATE[i] <- x$ORDER_START_DATE[i+1]
      i <- i + 1
    }
  }
  # Massage the last STOP DATES
  if (is.na(x$ORDER_STOP_DATE[i])) {
    refills <- x$REFILLS[i]
    if(is.na(refills)) {  refills <- 3   }
        x$ORDER_STOP_DATE[i] <- x$ORDER_START_DATE[i] +  ((refills + 1) * 2635200) #days
  }
  x
}

### X.D.: I changed file from x="/data/raw_data/PA/select___from_RAR_meds_order_by_EMPI__si.csv" to x = "/data/raw_data/RHTN/RHTN_meds.csv" -- changed back
load_RAR_meds <- function(x="/data/raw_data/PA/select___from_RAR_meds_order_by_EMPI__si.csv") {
  #' Load medication data in RHTN_meds.csv format
  #' @return data.frame
   
  a <- fread(x, stringsAsFactors = F, h=T) 
  print("Loaded meds")
  
  a$EMPI <- as.character(a$EMPI)
  
  #a %<>%
  #  dplyr::select(-FK_ENCOUNTER_ID, -SOURCE_LAST_UPDATE_DATE)
  
  # Handle dates
  a$ORDER_START_DATE <- as.POSIXct(trunc(as.POSIXlt(a$ORDER_START_DATE, format = "%d-%b-%Y %H:%M:%S"), "days"))
  a$ORDER_STOP_DATE <- as.POSIXct(trunc(as.POSIXlt(a$ORDER_STOP_DATE, format = "%d-%b-%Y %H:%M:%S"), "days"))
  
  # Handle refill text (coerce)
  # TODO: improve handling of text
  #a$REFILLS <- as.numeric(as.character(a$REFILLS))
  
  # Hot fix to exclude some inpatient injections with different dosing patterns (than the standard oral meds), 
  # so that can summarize based on drug name
  # TODO: Improve handling of inpatient medications (consider excluding)
  # TODO: consider handling medication route as second grouping variable
  a %<>%
    filter(!grepl(x=toupper(a$GENERIC_NAME), pattern="INJ"))
  
  # Populate SIMPLE_GENERIC_NAME when missing based on GENERIC_NAME
  a %<>%
    filter(SIMPLE_GENERIC_NAME != "" | GENERIC_NAME != '')
  
  mask <- a$SIMPLE_GENERIC_NAME == "" & a$GENERIC_NAME != ""
  if("GENERIC_NAME" %in% names(a) && length(mask)) {
    a$SIMPLE_GENERIC_NAME[which(mask)] <- sapply(strsplit(x=a$GENERIC_NAME[which(mask)], 
                                                   split=" "), 
                                          function(x) {
                                            paste(x[1:2], collapse=" ")
                                          })
  }
   
  # Handle missing doses
  mask <- a$DOSE == ""
  a$DOSE[which(mask)] <- NA
  
  # Exclude some complex meds of minimal interest
  a %<>%
    filter(!SIMPLE_GENERIC_NAME %in% c("Buch-Crnslk-Ch Gras-Hydr-Ca",
                                      "Buchu-Cornsilk-Ch Grass-Hydran",
                                      "Buchu-Junip-K Gluc-Pars-Uva Ur"))
  
  # Handle dose errors in combo drugs
  a$DOSE[which(a$SIMPLE_GENERIC_NAME == "Esmolol HCl-Sodium Chloride")] <- "2500-250"
  
  # Split combo drugs into components (SIMPLE_GENERIC_NAME and DOSE)
  col_split <- "-| in "
  mask <- with(a, grepl(x=SIMPLE_GENERIC_NAME, pattern=col_split) & 
                 !grepl(x=DOSE, pattern="-"))
  
  ### X.D.: The data.table nature of a cannot get unnest step through, so I change a into a data.frame temporarily
  tmp <- as.data.frame(a) %>%
    filter(!mask) %>%
    transform(SIMPLE_GENERIC_NAME = strsplit(SIMPLE_GENERIC_NAME, col_split),
              DOSE = strsplit(DOSE, col_split)) %>%
    unnest(SIMPLE_GENERIC_NAME, DOSE)

  if (sum(mask))  {
      tmp1 <- as.data.frame(a) %>%
        filter(mask) %>%
        transform(SIMPLE_GENERIC_NAME = strsplit(SIMPLE_GENERIC_NAME, col_split)) %>%
        transform(DOSE = lapply(SIMPLE_GENERIC_NAME, function(x) {
          sub(x, pattern = "*", replacement = NA) }
          )) %>%
        unnest(SIMPLE_GENERIC_NAME, DOSE)
      a <- bind_rows(tmp, tmp1)
  } else {
    a <- tmp
    }

  # Simplify SIMPLE_GENERIC_NAME to first term only 
  a$SIMPLE_GENERIC_NAME <- sapply(strsplit(x=a$SIMPLE_GENERIC_NAME, split=" "), function(x) {toupper(x[1])})
  a$SIMPLE_GENERIC_NAME[which(a$SIMPLE_GENERIC_NAME == "HYDROCHLOROTHIAZIDE")] <- "HCTZ"
  
  # Impute missing doses -- Fill in NA with mode for each drug
  # TODO: improve dose imputation
  NAs <- which(is.na(a$DOSE))
  a$DOSE[NAs] <- sapply(NAs, function(x) {
    y <- na.omit(a$DOSE[which(a$SIMPLE_GENERIC_NAME == a$SIMPLE_GENERIC_NAME[x])])
    y <- table(y)
    names(y)[which.max(y)]
  })
  a$DOSE <- as.numeric(as.character(a$DOSE))
  
  # Consolidate orders 
  # TODO: Improve understanding of the STATUS field and how to handle
  a %<>%
    # filter(!STATUS_MASTER_DESCRIPTION %in% c("CANCELED", "DISCONTINUED")) %>%
    mutate(group = paste(EMPI, SIMPLE_GENERIC_NAME, DOSE, sep="_")) %>%   # used in consolidate_rows()
    arrange(EMPI, SIMPLE_GENERIC_NAME, ORDER_START_DATE)
  
  # a %<>%
  #   group_by(EMPI, SIMPLE_GENERIC_NAME) %>%
  #   arrange(ORDER_START_DATE, ORDER_STOP_DATE) %>%
  #   do(consolidate_rows(.)) %>%
  #   dplyr::select(-EMPI, -SIMPLE_GENERIC_NAME) %>% ungroup()
  
  #Classify
  a$DRUG_CLASS <- NA
  a$DRUG_CLASS[which(a$SIMPLE_GENERIC_NAME %in% c("FUROSEMIDE", "TORSEMIDE"))] <- "Loop"
  a$DRUG_CLASS[which(a$SIMPLE_GENERIC_NAME %in% c("AMLODIPINE", "NICARDIPINE", "NIFEDIPINE", "DILTIAZEM"))] <- "CCBv"
  a$DRUG_CLASS[which(a$SIMPLE_GENERIC_NAME %in% c("DILTIAZEM", "VERAPAMIL"))] <- "CCBh"
  a$DRUG_CLASS[which(a$SIMPLE_GENERIC_NAME %in% c("ATENOLOL", "CARVEDILOL", "LABETALOL", "ESMOLOL",
                                                      "METOPROLOL", "NEBIVOLOL", "PROPRANOLOL",
                                                      "SOTALOL"))] <- "BB"
  a$DRUG_CLASS[which(a$SIMPLE_GENERIC_NAME %in% c("BENAZEPRIL", "CANDESARTAN", "CAPTOPRIL", "ENALAPRIL",
                                                      "LISINOPRIL", "LOSARTAN", "OLMESARTAN",
                                                      "QUINAPRIL", "RAMIPRIL", "VALSARTAN"))] <- "ACE_ARB"
  a$DRUG_CLASS[which(a$SIMPLE_GENERIC_NAME %in% c("CHLORTHALIDONE", "HCTZ",
                                                      "METOLAZONE"))] <- "Thiazide"
  a$DRUG_CLASS[which(a$SIMPLE_GENERIC_NAME %in% c("CLONIDINE"))] <- "A2A"
  a$DRUG_CLASS[which(a$SIMPLE_GENERIC_NAME %in% c("TERAZOSIN"))] <- "A1ant"
  a$DRUG_CLASS[which(a$SIMPLE_GENERIC_NAME %in% c("SPIRONOLACTONE", "EPLERENONE",
                                                      "TRIAMTERENE", "AMILORIDE"))] <- "K_sparing"
  a$DRUG_CLASS[which(a$SIMPLE_GENERIC_NAME %in% c("HYDRALAZINE"))] <- "hydral"
  a$DRUG_CLASS[which(a$SIMPLE_GENERIC_NAME %in% c("MINOXIDIL"))] <- "mino"
  a$DRUG_CLASS[which(a$SIMPLE_GENERIC_NAME %in% c("POTASSIUM"))] <- "K"
  
  a %<>%
    filter(!SIMPLE_GENERIC_NAME %in% c("NA", "NACL", "SODIUM")) %>%
    distinct()
  
#  row.names(a) <- 1:nrow(a)
  
  
  a %<>%
    dplyr::select(-FREQUENCY_NAME, -QUANTITY, -GENERIC_NAME)
  
  
  a
}

meds_now_all <- function(x, y) {
  # What pDOSE is patient ever on?
  # Make date ranges inclusive on both sides
  tmp <- inner_join(x, y, by="EMPI")
  browser()
  
  tmp <- do.call('rbind', 
                 lapply(1:length(start_dates), function(i) { 
                   y <- meds_now(x, start_dates[i])
                   y$stop_date <- stop_dates[i]
                   y
                 })
  )
  tmp
}

meds_now <- function(x, DtTm_q) {
  # What pDOSE is patient on now?
  
  tmp <- x %>%
    filter(ORDER_START_DATE <= DtTm_q & ORDER_STOP_DATE >= DtTm_q ) %>%
    group_by(EMPI, DRUG_CLASS) %>%
    summarize(sum_pDOSE = sum(pDOSE, na.rm=T)) %>% 
    complete(DRUG_CLASS) %>%
    mutate(start_date = DtTm_q) %>% ungroup()
  
  if(nrow(tmp) == 0) {
    tmp <- data.frame(EMPI = factor(x$EMPI[1], levels=levels(x$EMPI)),
                      DRUG_CLASS = factor(levels(x$DRUG_CLASS)),
                      sum_pDOSE = NA,
                      start_date = DtTm_q) %>% tbl_df()
  }
  tmp
}

load_RAR_vitals <- function(x="/data/raw_data/PA/select___from_RAR_vitals.csv") {
  #' Load in RAR_vitals.csv vital sign information
  #' @return data.frame
  
  res_vitals <- fread(x, stringsAsFactors = F, h=T)
  res_vitals$EMPI <- as.character(res_vitals$EMPI)
  
  # Handle dates
  res_vitals$ENC_DATE <- as.POSIXct(res_vitals$ENC_DATE, format = "%d-%b-%Y %H:%M:%S")
  
  res_vitals %<>%
    arrange(EMPI, ENC_DATE) %>%
    distinct()
  
  res_vitals
}

prep_RAR <- function(a) {
  ren1 <- prep_RAR_renin(a$res1, a$others)
  
  rar1 <- ren1 %>%
    filter(!is.na(ALDOSTERONE))
  
  rar1 %<>%
    mutate(RAR=ALDOSTERONE / Val.x,
           lRAR = log(ALDOSTERONE / Val.x))
  
  list(ren1=ren1, rar1=rar1)
}

prep_RAR_renin <- function(res1, others) {
  
  ren1 <- res1 %>%
    filter(RESULT_ITEM_CODE %in% c("RENIN_ACTIVITY", "RENIN"))
  
  
  ### X.D.: got duplicates in ren1 & others, so I allowed cartesian projection here
  ren1 <- merge(ren1, others, by=c("EMPI"), allow.cartesian = TRUE)

  # Pick the closest for each component
  ren1 <- ren1 %>%
    group_by(EMPI, RESULT_ITEM_CODE.x, ORDER_START_DATE.x, RESULT_ITEM_CODE.y) %>%
    slice(which.min(abs(ORDER_START_DATE.y - ORDER_START_DATE.x))) %>%
    ungroup() %>%
    dplyr::select(EMPI, GENDER_MASTER_CODE, Age, ORDER_NAME.x, ORDER_START_DATE.x, ORDER_ITEM_CODE.x, ORDER_ITEM_DESCRIPTION.x, Val.x, BP_DIASTOLIC, BP_SYSTOLIC, RESULT_ITEM_CODE.y, Val.y,
                 Loop, BB, CCBv, Thiazide, ACE_ARB) %>%  # dropped "K", "spiro"
    spread(RESULT_ITEM_CODE.y, Val.y)
  
  ren1
}



load_RAR_enc <- function(dat_file = "/data/raw_data/PA/HERMANDA_RAR_PTS_ENC.csv", bp_only = FALSE, EMPI_DATE_Level = TRUE, outpatient_only = TRUE, logger = NULL){
  #' Load in the RAR Encounter data, and do some basic cleaning
  #' @param dat_file string Raw data text file location 
  #' @param bp_only logic If TRUE, only keep BP's, excluding all other detailed info. Default is FALSE
  #' @param EMPI_DATE_Level logic If TRUE, collapse into EMPI_DATE Level. Default is TRUE
  #' @param outpatient_only logic If TRUE, only include Outpatients. Default is TRUE
  #' @param logger logger For logging purposes, should be initialized beforehand
  #' @return rar_enc tibble pre-processed RAR Dx data

  
  # TODO in future: make a directory for different types of variables in a file, and read this file in for colClasses
  # var_char <- c("EMPI", "PK_ENCOUNTER_ID")
  # var_date <- c("ENC_DATE")
  # var_num <- c("BP_SYSTOLIC")
  
  
  # read in the files with specified column classes
  
  # specify Column Classes
  colClasses <- c(ENC_DATE = "PDS_DateTime", E_SOURCE_LAST_UPDATE = 'PDS_DateTime', 
                  EMPI = 'character', PK_ENCOUNTER_ID = 'character', 
                  HAR_NUMBER = 'character', PATIENT_MASTER_CLASS = 'character', ADMIT_SOURCE = 'character', 
                  ENCOUNTER_SUB_TYPE = 'character', 
                  ROLE_ADMIT = 'character', DISCIPLINE_ADMIT = 'character', ENC_TYPE_CODE = 'character', 
                  ENC_TYPE_MASTER_CODE = 'character', ENC_TYPE_MASTER_DESCRIPTION = 'character', 
                  MASTER_LOCATION_CODE = 'character', MASTER_LOCATION_DESCRIPTION = 'character', 
                  MASTER_LOCATION_FACILITY_YN = 'character', MASTER_LOCATION_CLINIC_YN = 'character',
                  MASTER_LOCATION_ENTITY = 'character', SERVICE_MASTER_CODE = 'character', 
                  SERVICE_MASTER_DESCRIPTION = 'character', 
                  BP_SYSTOLIC = 'numeric', BP_DIASTOLIC = 'numeric')
  
  
  
  rar_enc <- fread_epic(dat_file = dat_file, colClasses = colClasses, logger = logger)
  
  

  if(outpatient_only){
    # Assert that PATIENT_MASTER_CLASS is present
    if(!("PATIENT_MASTER_CLASS" %in% names(rar_enc))){
      if(!is.null(logger)) logger$error("load_RAR_enc: PATIENT_MASTER_CLASS does not exist")
      stop("PATIENT_MASTER_CLASS does not exist")
    }
    
    rar_enc %<>% 
      filter(PATIENT_MASTER_CLASS == "OUTPATIENT")
    if(!is.null(logger)) logger$info("Only OUTPATIENT is selected from ENC data.")
  }
  

    if(bp_only){
    # only keep BP's
    rar_enc %<>% 
      select(EMPI, PK_ENCOUNTER_ID, ENC_DATE, E_SOURCE_LAST_UPDATE, 
             BP_SYSTOLIC, BP_DIASTOLIC)
  }

  
  # Modify ID's and DateTime
  # rar_enc <- id_date(rar_enc)
  
  # Create new HAR: HAR_NUMBER or EMPI + ENC Date
  # rar_enc$HAR <- ifelse(is.na(rar_enc$HAR_NUMBER), 
  #                       paste(rar_enc$EMPI, format(rar_enc$ENC_DATE, "%Y-%m-%d")),
  #                       rar_enc$HAR_NUMBER)
  

  # Create new ENC ID: EMPI_DATE
  rar_enc$EMPI_DATE <- paste(rar_enc$EMPI, 
                             format(rar_enc$ENC_DATE, "%Y-%m-%d"))

  
  
  if(EMPI_DATE_Level){
    # collapse into HAR_NUMBER Level
    ##  For duplicated EMPI_DATE, use the Median E_SOURCE_LAST_UPDATE
    ## For details, refer to Explore_Raw.Rmd
    
    dups <- unique(rar_enc[duplicated(rar_enc$EMPI_DATE), "EMPI_DATE"])
    
    r0 <- rar_enc %>% filter(!(EMPI_DATE %in% dups$EMPI_DATE))
    r1 <- rar_enc %>% filter(EMPI_DATE %in% dups$EMPI_DATE)

    # factorize ENC_TYPE_MASTER_CODE, assign priority
    r1$ENC_TYPE_MASTER_CODE <- factor(r1$ENC_TYPE_MASTER_CODE, levels = c("OFFICE VISIT", "LAB","SURGERY", "DAY SURG", "SURGERY, GENERAL","APPOINTMENT","ORDERS","CONSULT", "FORMS","LETTER","PHONE","SCAN - OLD RECORDS", "HISTORY"))
    
    # Pick by ENC_TYPE_MASTER_CODE
    r1 %<>% 
      group_by(EMPI_DATE) %>%
      arrange(ENC_TYPE_MASTER_CODE, desc(E_SOURCE_LAST_UPDATE)) %>%
      mutate_at(vars(BP_SYSTOLIC:BP_DIASTOLIC), 
                funs(median(., na.rm = FALSE))) %>%
      slice(1) %>%
      ungroup()

    # re-assign ENC_TYPE_MASTER_CODE as character
    r1$ENC_TYPE_MASTER_CODE <- as.character(r1$ENC_TYPE_MASTER_CODE)
    
    # merge r1 back to r0
    rar_enc <- r0 %>% bind_rows(., r1) %>% ungroup()
    
    return(rar_enc)
    
  }

  
  # TODO: If HAR_Level = FALSE, need to collapse into PK_ENCOUNTER_ID level
  
  return(rar_enc)
  
  
}


load_RAR_Dx <- function(dat_file = "/data/raw_data/PA/HERMANDA_RAR_PTS_DX.csv", logger = NULL){
  #' Load in the RAR Dx data, and do some basic cleaning
  #' @param dat_file string Raw data text file location 
  #' @return rar_dx tibble pre-processed RAR Dx data
  
  # read in the files with specified column classes
  ## specify Column Classes
  colClasses <- c(ENC_DATE = "PDS_DateTime", CODING_DATE = "PDS_DateTime", SOURCE_LAST_UPDATE_DATE = "PDS_DateTime", 
                  EMPI = 'character', PK_ENCOUNTER_ID = 'character', CODE = 'character', CODE_STANDARD_NAME = 'character',
                  DESCRIPTION = 'character', PK_DX_ID = 'character', DX_TYPE = 'character', PRIMARY_YN = 'character',
                  COMMENTS = 'character', SOURCE_CODE = 'character', 
                  HAR_NUMBER = 'character', PATIENT_MASTER_CLASS = 'character',
                  DX_SEQUENCE = 'numeric')
  
  
  
  rar_dx <- fread_epic(dat_file = dat_file, colClasses = colClasses, logger = logger)
  
  
  # remove those without ICD indicator (Since they also do not have CODE)
  rar_dx %<>% filter(CODE_STANDARD_NAME != "")
  
  
  # change PRIMARY_YN into factor
  rar_dx$PRIMARY_YN <- ifelse(rar_dx$PRIMARY_YN == "1", TRUE, FALSE)
  
  return(rar_dx)
  
}

sub_RAR_dx <- function(dat, CODE_Level, hierarchy_dx,
                       EMPI_DATE_Level = TRUE, outpatient_only = TRUE,
                       logger = NULL){
  #' Getting the proper subset for Dx data
  #' @param dat tibble Pre-processed RAR Dx data
  #' @param CODE_Level character Specify on which level to pivot the Dx codes: "ICD", "Dx_h0", "Dx_h1", or "Dx_h2"
  #' @param hierachy_dx vector A character vector that specifies Dx in corresponding hierarchy level
  #' @param EMPI_DATE_Level logit If TRUE, collapse data into EMPI_DATE Level. Default is TRUE
  #' @param outpatient_only logit If TRUE, only include Outpatients. Default is TRUE.
  #' @return ret tibble The subset of cleaned Dx data
  
  ret <- tibble()  # return tibble
  spec_dx <- c() # specific dx's captured
  

  if(outpatient_only){
    dat %<>% 
      filter(PATIENT_MASTER_CLASS == "OUTPATIENT")
    
    if(!is.null(logger)) logger$info("RAR_dx: only Dx with PATIENT_MASTER_CLASS = OUTPATIENT are selected")
  }
 
  
  # filter based on Selected Dx's
  # CODE_Level = "Dx_h1"
  # hierarchy_dx <- c("Essential_HTN", "Primary_aldosteronism")
  icd_map <- icd_mapping(dx_hierarchy_level = CODE_Level, dx_hierarchy_level_value = hierarchy_dx)
  
  ret <- dat %>% filter(CODE %in% icd_map$CODE)
  
  
  # Create new ENC ID: EMPI_DATE
  # TODO: pull this out into function
  ret$EMPI_DATE <- paste(ret$EMPI, 
                         format(ret$ENC_DATE, "%Y-%m-%d"))
  

  empi_date_0 <- length(unique(ret$EMPI_DATE))
  
  if(EMPI_DATE_Level){
    # collapse into EMPI_DATE Level
    ##  For duplicated CODE in EMPI_DATE level, use the latest SOURCE_LAST_UPDATE, if same:
    ## (1) Pick PRIMARY_YN = TRUE, [first of desc(PRIMARY_YN), NA will always be the last]
    ## (2) DX_SEQUENCE minimal [first of arrange(DX_SEQUENCE), NA will be last]
    ## (3) First of arrange(desc(SOURCE_LAST_UPDATE_DATE), desc(COMMENTS)) [This will avoid "" in COMMENTS]
    
  
    ret %<>% 
      group_by(EMPI_DATE, CODE) %>%
      arrange(desc(SOURCE_LAST_UPDATE_DATE), 
              desc(PRIMARY_YN), DX_SEQUENCE, desc(COMMENTS)) %>% # Pick Unique CODE for each EMPI_DATE (for now)
      slice(1)
    
    # remove unuseful information
    ret %<>% 
      select(-one_of(c('EMPI', 'CODE_STANDARD_NAME', 'COMMENTS', 'CODING_DATE', 'PRIMARY_YN', 'DX_SEQUENCE', 'DESCRIPTION', 'PK_DX_ID', 
                       'SOURCE_LAST_UPDATE_DATE', 'ENC_DATE', 'PK_ENCOUNTER_ID', 'DX_TYPE', 'HAR_NUMBER')))
    
    
    # spread in each Dx level
    ret_icd <- ret %>% ungroup() %>% filter(!duplicated(.)) %>%
                mutate(value = 1) %>%
                spread(CODE, value, fill = 0, sep = "_")
      
    ret_h0 <- ret %>% left_join(., icd_map, by = "CODE") %>% ungroup() %>%
                select(EMPI_DATE, Dx_h0) %>%
                filter(!duplicated(.)) %>%
                mutate(value = 1) %>% spread(Dx_h0, value, fill = 0, sep = "_")
    
    ret_h1 <- ret %>% left_join(., icd_map, by = "CODE") %>% ungroup() %>%
      select(EMPI_DATE, Dx_h1) %>%
      filter(!duplicated(.)) %>%
      mutate(value = 1) %>% spread(Dx_h1, value, fill = 0, sep = "_")
    
    
    # QC: check whether some EMPI_DATE got dropped
    n0 <- length(unique(ret$EMPI_DATE))
    n_icd <- length(unique(ret_icd$EMPI_DATE))
    n_h0 <- length(unique(ret_h0$EMPI_DATE))
    n_h1 <- length(unique(ret_h1$EMPI_DATE))
    if(!(n0 == n_icd & n0 == n_icd & n0 == n_h0 & n0 == n_h1)){
      logger$warn("sub_RAR_dx: some EMPI_DATE got dropped in converting Dx to higher levels")
    }
      
    
    ret <- full_join(ret_icd, ret_h0, by = "EMPI_DATE") %>%
            full_join(., ret_h1, by = "EMPI_DATE")
      
      
  }
  
  
  empi_date_1 <- length(unique(ret$EMPI_DATE))
  
  
  if(empi_date_0 != empi_date_1){
    err_msg <- sprintf("sub_RAR_dx: missing EMPI_DATEs (Should be %d; Actually: %d)", empi_date_0, empi_date_1)
    if(!is.null(logger)) logger$warn(err_msg)
    warning(err_msg)
  }else{
    msg <- sprintf("sub_RAR_dx: all EMPI_DATEs are present (Number: %d)", empi_date_1)
    if(!is.null(logger)) logger$warn(msg)
  }
  
  # TODO: If HAR_Level = FALSE, need to collapse into PK_ENCOUNTER_ID level
  return(ret)
  
}



load_RAR_PtDemo <- function(dat_file = "/data/raw_data/PA/HERMANDA_RAR_PTS_DEMO.csv", logger = NULL){
  #' Load in the RAR Patients' Demo data, and do some basic cleaning
  #' @param dat_file string Raw data text file location 
  #' @return rar_demo tibble pre-processed RAR Dx data
  
  # read in the files with specified column classes
  
  ## specify Column Classes
  colClasses <- c(BIRTH_DATE = "PDS_DateTime", 
                  EMPI = 'character', PK_PATIENT_ID = 'character', GENDER_MASTER_CODE = 'character', 
                  RACE_MASTER_CODE = 'character', RACE_MASTER_HISPANIC_YN = 'character')
  
  
  rar_demo <- fread_epic(dat_file = dat_file, colClasses = colClasses, logger = logger)
  
  

  # For EMPI's with more than 1 PK_ENCOUNTER_ID
  # This method will first deal with NA in Gender, then RACE (UNKNOWN)
  # TODO: get timetamps on the patient table (so can pick based on that PK_PATIENT_ID dates)
  rar_demo %<>% group_by(EMPI) %>%
    arrange(EMPI, GENDER_MASTER_CODE, desc(RACE_MASTER_CODE)) %>%
    filter(row_number() == n())
  
  
  # Factorize
  rar_demo$GENDER_MASTER_CODE <- factor(rar_demo$GENDER_MASTER_CODE, levels = c("F","M"))
  # All other will be NA, like UNKNOWN
  rar_demo$RACE_MASTER_CODE <- factor(rar_demo$RACE_MASTER_CODE, levels = c("BLACK", "WHITE","OTHER","AM IND AK NATIVE", "ASIAN", "HI PAC ISLAND", "MIXED"))
  
  
  return(rar_demo)
  
}



load_Lab <- function(dat_file,  lab_source, adjust_up = 1.5, adjust_down = 0.5, logger = NULL){
  #' Load in the Lab data, and do some basic cleaning. Lab data includes RAR_Lab and RAR_V3
  #' @param dat_file string Raw data text file location 
  #' @param lab_source character Lab test source: ALL or RAR
  #' @param adjust_up numeric Rate for adjusting "> X", default is 0.5
  #' @param adjust_donw numeric Rate for adjusting "< X", default is 1.5
  #' @return rar_lab tibble pre-processed RAR Dx data
  
  # TODO: Not Finished.
  # read in the files with specified column classes
  
  if(lab_source == "ALL"){
    # specify Column Classes
    colClasses <- c(ENC_DATE = "PDS_DateTime", ORDER_START_DATE = "PDS_DateTime", O_SOURCE_LAST_UPDATE = "PDS_DateTime", 
                    RESULT_DATE = "PDS_DateTime", 
                    EMPI = 'character', PK_ENCOUNTER_ID = 'character', PK_ORDER_ID = 'character', ORDER_NAME = 'character', 
                    ORDER_ITEM_CODE = 'character', PK_ORDER_RESULT_ID = 'character', RESULT_VALUE = 'character',
                    UNIT_OF_MEASURE = 'character', RESULT_STATUS = 'character', RESULT_ITEM_CODE = 'character', 
                    ORDERING_PROV = 'character', HAR_NUMBER = 'character')
  }else if(lab_source == "RAR"){
    colClasses <- c(ENC_DATE = "PDS_DateTime", ORDER_START_DATE = "PDS_DateTime",
                    RESULT_DATE = "PDS_DateTime", ORDER_DATE = "PDS_DateTime", PERFORMED_DATE = "PDS_DateTime", 
                    EMPI = 'character', PK_ENCOUNTER_ID = 'character', PK_ORDER_ID = 'character', ORDER_NAME = 'character', 
                    ORDER_ITEM_CODE = 'character', PK_ORDER_RESULT_ID = 'character', RESULT_VALUE = 'character',
                    UNIT_OF_MEASURE = 'character', RESULT_STATUS = 'character', RESULT_ITEM_CODE = 'character', 
                    ORDERING_PROV = 'character', MRN = 'character', ORDER_GROUP = 'character', 
                    ORDER_ITEM_DESCRIPTION = 'character', RESULT_TEXT = 'character', RESULT_RESOURCE = 'character', 
                    RESULT_ITEM_DESCRIPTION = 'character',
                    LOINC_CODE = 'character', PK_ORDER_PERFORMED_ID = 'character', TASK_DESCRIPTION = 'character', 
                    ADMITTING_PROV = 'character', MASTER_LOCATION_DESCRIPTION = 'character')
    
  }else{
    err_msg <- sprintf("load_lab: Unspecified Lab Source: %s", lab_source)
    if(!is.null(logger)) logger$error(err_msg)
    stop(err_msg)
  }
 
  
  rar_lab <- fread_epic(dat_file = dat_file, colClasses = colClasses, logger = logger)

  # modify RESULT_VALUE, using numericize function
  rar_lab$RESULT_VALUE <- numericize(rar_lab$RESULT_VALUE, adjust_up = adjust_up, adjust_down = adjust_down)
  

  
  return(rar_lab)
  
}




clean_Lab <- function(dat, RAR_only = TRUE, potassium_in = TRUE, num_labs = 0, logger = NULL){
  #' Load in the Lab data, and do some basic cleaning
  #' @param dat tibble Pre-cleaned RAR data 
  #' This should only contain: "Corrected", "Final", "Incomplete", "Preliminary", "", "ALL"
  #' @param RAR_only logit If TRUE, will only return Aldo/Renin results
  #' @param potassium_in logit If TRUE, potassium lab results will be included. Default is TRUE
  #' Note: RESULT_ITEM_CODE may have several CODEs that include "POTASSIUM", but only "POTASSIUM"
  #' @param num_labs numeric Number of labs results included. Note that RAR and potassium labs are not counted in num_labs
  #' @return ret tibble pre-processed RAR Dx data

 
  dat %<>% filter(!is.na(dat$PK_ORDER_RESULT_ID))
  
  
  # Duplicates
  if(sum(duplicated(dat))){
    if(!is.null(logger)) logger$warn("TODO: Duplicated rows in pre_rar input: need to explain")
    dat %<>%
      filter(!duplicated(dat))
  }
  
  # Create new ENC ID: EMPI_DATE
  dat$EMPI_DATE <- paste(dat$EMPI, format(dat$ENC_DATE, "%Y-%m-%d"))
  
  # Create ret table
  ret <- as.tibble()
  
  # RAR
  ## catch "ALDO"|"RENIN"
  dat_rar <- dat %>% filter(grepl("renin|aldo", tolower(RESULT_ITEM_CODE))) %>%
    group_by(ORDER_ITEM_CODE, RESULT_ITEM_CODE)
  
  ## Exclude ORDER_ITEM_CODE that are for AVS or urine specimens
  dat_rar %<>% filter(!(ORDER_ITEM_CODE %in%  c("C9009900", "C9009995", "C9009997", "Q19573", "83497A")))
  
  
  ## dropped many rows here
  ret <- dat_rar %>% 
    filter(RESULT_ITEM_CODE %in% c("RENIN ACTIVITY","RENIN", "PLASMA RENIN ACTIVITY, LC/MS/MS", "PLASMA RENIN ACTIVITY")) %>%
    mutate(Test = "PRA") %>%
    bind_rows(., ret)
  
  
  ret <- dat_rar %>% 
    filter(RESULT_ITEM_CODE %in% c("DIRECT RENIN")) %>% 
    mutate(Test = "DRC") %>%
    bind_rows(., ret)
  
  ret <- dat_rar %>% 
    filter(RESULT_ITEM_CODE %in% c("ALDOSTERONE, SERUM", "ALDOSTERONE, LC/MS/MS", "ALDOSTERONE")) %>%
    mutate(Test = "Aldo") %>%
    bind_rows(., ret) %>% ungroup()
  

  
  
  if(RAR_only){
    # The picking function applied here
    ret <- collapse_lab_PK_ORDER_ID_RIC(ret)
    
    ret %<>% ungroup()
    
    return(ret)
  }
  
  
  
  # Include POTASSIUM
  if(potassium_in){
    ret <- dat  %>% filter(RESULT_ITEM_CODE == "POTASSIUM") %>% 
      mutate(Test = "POTASSIUM") %>%
      bind_rows(., ret)
  }
  
  
  if(num_labs != 0){
    # Get a list for RESULT_ITEM_CODE, excluding POTASSIUM and RAR
    ric_ranked_lt <- dat %>% group_by(RESULT_ITEM_CODE) %>%
      summarise(N = n()) %>%
      arrange(desc(N)) %>%
      na.omit(.) %>%
      filter(!(RESULT_ITEM_CODE %in% c("POTASSIUM") | RESULT_ITEM_CODE %in% dat_rar$RESULT_ITEM_CODE))
    
    # num_labs = 44 for CBC (Complete Blood Count) & CMP (Comprehensive Metabolic Panel)
    ric_lt <- ric_ranked_lt[1:num_labs,]
    
    ret <- dat %>% filter(RESULT_ITEM_CODE %in% ric_lt$RESULT_ITEM_CODE) %>%
      mutate(Test = RESULT_ITEM_CODE) %>%
      bind_rows(., ret)
  }
  
  
  ret %<>% ungroup()
  
  ret <- collapse_lab_PK_ORDER_ID_RIC(ret)
  

  return(ret)
}









enc_to_pts <- function(rar_enc_level, enc_id = "EMPI_DATE", num_dx, sbp_bar = 140, dbp_bar = 90, logger = NULL){
  #' This function is used to collapse encounter level data into patient level data. It deals with Dx's, Labs, Vitals
  #' @param rar_enc_level tibble Encounter level data set, with Dx's, Labs, Vitals for each encounter (may be missing)
  #' @param enc_id character Unique id for enc level data
  #' @param num_dx numeric Number of Dx codes
  #' @param sbp_bar numeric Systolic blood pressure criteria for high blood pressure
  #' @param dbp_bar numeric Diastolic blood pressure criteria for high blood pressure
  #' @return pts tibble Patient level data set, with aggregated Dx, Labs, Vitals for each patient
  
  
  # Collapse into Patients' Level
  pts <- rar_enc_level
  
  # ADD assert statment to confirm that there are no SBPs without DBPs and vice-a-versa
  # rar_enc_level %>%
  #   group_by(EMPI) %>%
  #   summarize(n_discordant_BP = sum(is.na(BP_SY)))
  # ....... is.na(BP_SYSTOLIC) & !is.na(BP_DIASTOLIC)
  
  ## SBP_n, DBP_n, High_BP_n, enc_n, encounter with bp
  pts %<>% 
    group_by(EMPI) %>% 
    mutate(enc_n = n(), enc_bp_n = sum(!is.na(BP_SYSTOLIC) & !is.na(BP_DIASTOLIC)),
           SBP_n = sum(BP_SYSTOLIC >= sbp_bar, na.rm = TRUE), 
           DBP_n = sum(BP_DIASTOLIC >= dbp_bar, na.rm = TRUE),
           High_BP_n = sum(BP_SYSTOLIC >= sbp_bar | BP_DIASTOLIC >= dbp_bar, na.rm = TRUE),
           High_BP_prop = High_BP_n / sum(!is.na(BP_SYSTOLIC) & !is.na(BP_DIASTOLIC))) %>%
    ungroup() 
  
  # get the start and end col number for Dx codes in merged data set
  dx_code_start <- which(names(rar_enc_level) == enc_id)[1] + 1
  dx_code_end <- dx_code_start + num_dx - 1
  
  
  ## Dx's
  pts %<>% 
    group_by(EMPI) %>%
    mutate_at(vars(dx_code_start:dx_code_end), 
              funs(sum)) %>%
    ungroup()
  
  ## RAR
  pts %<>% 
    group_by(EMPI) %>% 
    mutate(N_missing = recode(is.na(Aldo) + is.na(PRA) + is.na(DRC), `0` = 1, `1` = 1, `2` = 2, `3` = 3)) %>%   #0 & 1 to be equivalent LEVELS: (0,1), (2), (3)
    arrange(EMPI, N_missing, ENC_DATE) %>% 
    slice(1) %>%
    ungroup()
  
  pts %<>% select(-c(BP_SYSTOLIC, BP_DIASTOLIC, N_missing))
  
  return(pts)
}


collapse_lab_PK_ORDER_ID_RIC <- function(dat){
  #' This function is used to collapse raw lab function into PK_ORDER_ID + RESULT_ITEM_CODE level
  #' which means for PK_ORDER_ID + RESULT_ITEM_CODE is the unique ID here
  #' This function will pick according to RESULT_STATUS: Corrected > Final > Preliminary > Incomplete > NA
  #' @param dat tibble Raw lab data frame
  #' @return ret tibble Returned collapsed Lab data
  
  
  # apply picking function: from all lab into PK_ORDER_ID + RESULT_ITEM_CODE level
  dat$oid_ric <- paste(dat$PK_ORDER_ID, dat$RESULT_ITEM_CODE, sep = "_")
  
  # factor RESULT_STATUS and assign levels
  dat$RESULT_STATUS <- factor(dat$RESULT_STATUS, levels = c("Corrected", "Final", "Preliminary", "Incomplete", NA))
  
  # a list of PK_ORDER_ID + RESULT_ITEM_CODE which has multiple records
  dups <- dat[which(duplicated(dat$oid_ric)), "oid_ric"]
  
  # separate dat into two parts
  r0 <- subset(dat, !(oid_ric %in% dups$oid_ric))
  r1 <- subset(dat, oid_ric %in% dups$oid_ric)
  
  # deal with those oid_ric which has multiple records
  r1 %<>%
    group_by(oid_ric, RESULT_STATUS) %>%
    mutate_at(vars(RESULT_VALUE), funs(median(., na.rm = TRUE))) %>%
    ungroup() %>%
    group_by(oid_ric) %>%
    arrange(RESULT_STATUS) %>%
    slice(1)
  
  # returning data frame
  ret <- r0 %>% bind_rows(., r1) %>% select(-oid_ric) %>% ungroup()
  
  return(ret)
  
}


collapse_lab_EMPI_DATE <- function(dat){
  #' This function is applied to cleaned lab data set(on PK_ORDER_ID + RESULT_ITEM_CODE level, 
  #' Picked by Corrected > Final ...) into EMPI_DATE level
  #' @param dat tibble Cleaned lab data set
  #' @return ret tibble Returned data set on EMPI_DATE level
  
  # For RAR use collapse_RAR_Lab function
  rar_lab <- subset(dat, Test %in% c("PRA", "DRC", "Aldo"))
  if(nrow(rar_lab) != 0){
    rar_lab_collapse <- collapse_RAR_Lab(dat=rar_lab, EMPI_DATE_Level = TRUE)
    
    ## make some changes to rar_lab
    rar_lab_collapse %<>% select(-c(PK_ENCOUNTER_ID, ORDER_START_DATE))
    
    
  }
 
  # Other labs
  other_lab <- subset(dat, !(Test %in% c("PRA", "DRC", "Aldo")))

  if(nrow(other_lab) == 0){
    ret <- rar_lab_collapse
    return(ret)
  }
  
  ## create a new temporary ID: EMPI_DATE + RESULT_ITEM_CODE and split into two sets
  other_lab$EMPI_DATE_RIC <- paste(other_lab$EMPI_DATE, other_lab$RESULT_ITEM_CODE)
  dups <- other_lab[duplicated(other_lab$EMPI_DATE_RIC), 'EMPI_DATE_RIC']
  r0 <- subset(other_lab, !(EMPI_DATE_RIC %in% dups$EMPI_DATE_RIC))
  r1 <- subset(other_lab, EMPI_DATE_RIC %in% dups$EMPI_DATE_RIC)
  
  ## for r1 (multiple RIC in EMPI_DATE), pick by 1) count of ORDER_START_DATE 2) arbitrary PK_ORDER_ID
  ### same test at same time (but from different ORDER: LIVER FUNCTIONAL PANEL & CMP), so use median to combine
  r1 %<>% group_by(EMPI_DATE_RIC, ORDER_START_DATE) %>% mutate_at(vars(RESULT_VALUE), funs(median(., na.rm=TRUE))) %>% slice(1)
  
  ### Pick by 1) count of ORDER_START_DATE 2) PK_ORDER_ID
  r1$EMPI_DATE_OSD <- paste(r1$EMPI_DATE, r1$ORDER_START_DATE)
  r1 <- r1 %>% group_by(EMPI_DATE_OSD) %>% summarize(N=n()) %>% ungroup() %>% full_join(., r1, by = "EMPI_DATE_OSD") %>% group_by(EMPI_DATE_RIC) %>% arrange(desc(N), desc(PK_ORDER_ID)) %>% slice(1)
  
  
  ## combine r0 and r1 together
  other_lab_EMPI_DATE_RIC <- r0 %>% bind_rows(., r1)
  
  ## Harmonize Units
  ### Get a list for most used units for each RESULT_ITEM_CODE
  RIC_units_select <- other_lab_EMPI_DATE_RIC %>% group_by(RESULT_ITEM_CODE, UNIT_OF_MEASURE) %>% summarise(N = n()) %>% arrange(desc(N)) %>% slice(1)
  
  ### add a column for filtering
  other_lab_EMPI_DATE_RIC$RIC_units <- paste(other_lab_EMPI_DATE_RIC$RESULT_ITEM_CODE, other_lab_EMPI_DATE_RIC$UNIT_OF_MEASURE)
  
  ### filtering units
  RIC_units_select$RIC_unit <- paste(RIC_units_select$RESULT_ITEM_CODE, RIC_units_select$UNIT_OF_MEASURE)
  other_lab_EMPI_DATE_RIC %<>% filter(RIC_units %in% RIC_units_select$RIC_unit) %>% ungroup()
  
  ### put unit into Test
  other_lab_EMPI_DATE_RIC$Test <- paste(other_lab_EMPI_DATE_RIC$Test, other_lab_EMPI_DATE_RIC$UNIT_OF_MEASURE, sep = "_")
  
  ## remvoe cols
  other_lab_EMPI_DATE_RIC %<>% select(EMPI_DATE, Test, RESULT_VALUE)
  
  ## Spread on EMPI_DATE level
  other_lab_EMPI_DATE <- other_lab_EMPI_DATE_RIC %>% spread(., key = Test, value = RESULT_VALUE, sep = "_")
  
  ## re-define EMPI and ENC_DATE
  # other_lab_EMPI_DATE$EMPI <- substr(other_lab_EMPI_DATE$EMPI_DATE, 1, 10)
  # other_lab_EMPI_DATE$ENC_DATE <- as.Date(substr(other_lab_EMPI_DATE$EMPI_DATE, 12, 21), format = "%Y-%m-%d")
  
  # merge RAR and other labs
  ret <- full_join(rar_lab_collapse, other_lab_EMPI_DATE, by = "EMPI_DATE")
  
  return(ret)
}


collapse_RAR_Lab <- function(dat, EMPI_DATE_Level = TRUE, logger = NULL){
  #' This function is used to collapse raw RAR Lab data for ALDO/RENIN into EMPI_DATE level
  #' @param dat tibble Pre-cleaned RAR Lab data, with only ALDO/RENIN tests
  #' @param EMPI_DATE_Level logit If TRUE, collapse data into EMPI_DATE Level
  #' @return tmp tibble Collapsed RAR Lab results


  tmp <- dat %>%
    ungroup() %>%
    select(EMPI, PK_ENCOUNTER_ID, ENC_DATE, EMPI_DATE, PK_ORDER_ID, Test, RESULT_VALUE, RESULT_DATE, O_SOURCE_LAST_UPDATE, ORDER_START_DATE)
  
  # QC
  # tmp %>% summarize(N_rows = n(), N_distinct_enc = n_distinct(EMPI, PK_ENCOUNTER_ID), N_distinct_collects = n_distinct(EMPI, ORDER_START_DATE), N_distinct_HAR = n_distinct(HAR))
  
  # Summarize at PK_ORDER_ID (QC) (--> PK_ORDER_ID level)
  # TODO: make sure we are picking the best result
  tmp %<>%
    group_by(EMPI, PK_ENCOUNTER_ID, ENC_DATE, EMPI_DATE, PK_ORDER_ID, Test) %>%
    arrange(desc(RESULT_DATE), desc(O_SOURCE_LAST_UPDATE)) %>%
    slice(1) %>% ungroup()
  
  # QC
  # tmp %>% summarize(N_rows = n(), N_distinct_enc = n_distinct(EMPI, PK_ENCOUNTER_ID), N_distinct_collects = n_distinct(EMPI, ORDER_START_DATE), N_distinct_HAR = n_distinct(HAR))
  
  # -> dat at unique PK_ORDER_ID level

  # Summarize at Collect Time (--> EMPI + ORDER_START_DATE level)
  # Note:In EMPI + ORDER_START_DATE (PK_ENCOUNTER_ID + ORDER_START_DATE) level, dups for tests (like two ALDO tests with same ORDER_START_DATE), pick one based on RESULT_DATE and O_SOURCE_LAST_UPDATE
  # TODO: make sure we are picking the best result
  tmp %<>%
    group_by(EMPI, PK_ENCOUNTER_ID, ENC_DATE, EMPI_DATE, ORDER_START_DATE, Test) %>%
    arrange(desc(RESULT_DATE), desc(O_SOURCE_LAST_UPDATE)) %>%
    slice(1) %>%
    ungroup()
  
  # QC
  # tmp %>% summarize(N_rows = n(), N_distinct_enc = n_distinct(EMPI, PK_ENCOUNTER_ID), N_distinct_collects = n_distinct(EMPI, ORDER_START_DATE), N_distinct_HAR = n_distinct(HAR))
  
  
  # Start to Collapse on PK_ENCOUNTER_ID level
  # Pair based on Collect Time
  ## Spread tmp into wide first
  ## After spreading, for each EMPI - PK_ENCOUNTER_ID - ORDER_START_DATE, there will be only one row (containing 3 tests)
  tmp %<>%
    select(EMPI, PK_ENCOUNTER_ID, ENC_DATE, EMPI_DATE, ORDER_START_DATE, Test, RESULT_VALUE) %>%
    group_by(EMPI, PK_ENCOUNTER_ID, ENC_DATE, EMPI_DATE, ORDER_START_DATE) %>%
    spread(key = Test, value=RESULT_VALUE) %>% ungroup()
  
  # QC
  # tmp %>% summarize(N_rows = n(), N_distinct_enc = n_distinct(EMPI, PK_ENCOUNTER_ID), N_distinct_collects = n_distinct(EMPI, ORDER_START_DATE), N_distinct_HAR = n_distinct(HAR))
  
  # Could join back to get more columns

  # Merge if collect within 30 minutes
  tmp_merged_2 <- tmp %>%
    group_by(EMPI, PK_ENCOUNTER_ID, ENC_DATE, EMPI_DATE) %>%
    filter(n() == 2,
           abs(ORDER_START_DATE[1] - ORDER_START_DATE[2]) < 60*30) %>%  # merge if within 30 minutes 
    mutate_at(vars(Aldo:DRC), funs(na.omit(.)[1])) %>%
    slice(1) %>% ungroup()
  
  # Take first row if 2 rows (far apart in time) or >2 rows
  tmp_merged_multi <- tmp %>%
    group_by(EMPI, PK_ENCOUNTER_ID, ENC_DATE, EMPI_DATE) %>%
    filter((n() == 2 & abs(ORDER_START_DATE[1] - ORDER_START_DATE[2]) >= 60*30) |
             n() > 2) %>%  # merge if within 30 minutes 
    arrange(ORDER_START_DATE) %>%
    slice(1) %>% ungroup()
  
  # Combine with single row data (--> PK_ENCOUNTER_ID level)
  tmp %<>%
    group_by(EMPI, PK_ENCOUNTER_ID, ENC_DATE, EMPI_DATE) %>%
    filter(n() == 1) %>%
    ungroup() %>%
    bind_rows(tmp_merged_2) %>%
    bind_rows(tmp_merged_multi)
  
  # QC
  # tmp %>% summarize(N_rows = n(), N_distinct_enc = n_distinct(EMPI, PK_ENCOUNTER_ID), N_distinct_collects = n_distinct(EMPI, ORDER_START_DATE), N_distinct_HAR = n_distinct(HAR))
  
  
  
  if(EMPI_DATE_Level){
    # Start to Collect at the EMPI_DATE level
    # TODO: Resolve the collect times at 00:00:00 (Get Inlab or received ttime to help)
    
    # (1) dates are same and one of each -> merge
    # (2) dates are same and more than one of one of them, drop
    # (3) dates are different, pick first one
    
    # Explore multiple PK_ENCOUNTER_ID's in EMPI_DATE level
    temp <- tmp %>% group_by(EMPI_DATE) %>%
      filter(n() != 1) %>%
      arrange(EMPI_DATE)
    
    
    
    ## (1) (2)
    temp_12 <- temp %>% group_by(EMPI_DATE) %>% filter(n_distinct(ORDER_START_DATE) == 1) %>% ungroup()
    
    temp_12 %<>% group_by(EMPI_DATE) %>% 
      filter(n_distinct(Aldo, na.rm = TRUE) %in% c(0,1) & 
               n_distinct(DRC, na.rm = TRUE) %in% c(0,1) & 
               n_distinct(PRA, na.rm = TRUE) %in% c(0,1)) %>%
      mutate_at(vars(Aldo:DRC), funs(na.omit(.)[1])) %>%
      slice(1) %>% ungroup()
    
    ## (3)
    temp_3 <- temp %>% group_by(EMPI_DATE) %>%
      filter(n_distinct(ORDER_START_DATE) != 1) %>%
      arrange(ORDER_START_DATE) %>%
      slice(1) %>%
      ungroup()
    
    ## Merge temp_12 & temp_3 back to tmp
    tmp %<>% group_by(EMPI_DATE) %>%
      filter(n() == 1) %>%
      bind_rows(temp_12, temp_3) %>%
      ungroup()
    
    # QC
    # tmp %>% summarize(N_rows = n(), N_distinct_enc = n_distinct(EMPI, PK_ENCOUNTER_ID), N_distinct_collects = n_distinct(EMPI, ORDER_START_DATE), N_distinct_HAR = n_distinct(EMPI_DATE))
    
    # A check on Patients with different HAR's but same ORDER_START_DATE's
    # tmp %>% group_by(EMPI, ORDER_START_DATE) %>% filter(n() != 1)
    
    return(tmp)
  }
  
  return(tmp)
  
}



# TODO: not finished
clean_RAR <- function(dat, blood_only=TRUE){
  #' This function is used to clean up RAR lab results
  #' @param dat tibble Pre-cleaned RAR lab results
  #' @param blood_only Boolean If true, only use serum/plasma test codes
  
  if (blood_only) { # Exclude ORDER_ITEM_CODE that are for AVS or urine specimens
    dat <- dat[!(ORDER_ITEM_CODE %in% c("C9009900", "C9009995", "C9009997", "Q19573", "83497A"))]
  }
  
}


rar_merge <- function(rar_dx, rar_enc, rar_lab, rar_demo, id, pt_id = "EMPI", logger = NULL){
  #' This function is used to load in rar_enc,rar_dx, rar_lab, rar_demo data sets and merge them together
  #' for a encounter level data set
  #' @param rar_dx tibble Diagnosis data set
  #' @param rar_enc tibble Encounter data set
  #' @param rar_lab tibble Lab tests data set
  #' @param rar_demo tibble Patients' demographic data set
  #' @param id character Unique ID to merge the 4 data sets together
  #' @param pt_id character Unique ID for patients. Default is EMPI
  #' @return rar_mg tibble The merged data set
  

  ## drop some columns
  rar_enc %<>% 
    select(-one_of(c("E_SOURCE_LAST_UPDATE", "PK_ENCOUNTER_ID", "ENC_DATE")))
  rar_dx %<>% 
    select(-one_of(c('EMPI', 'CODE_STANDARD_NAME', 'COMMENTS', 'CODING_DATE', 'PRIMARY_YN', 'DX_SEQUENCE', 'DESCRIPTION', 'PK_DX_ID', 
              'SOURCE_LAST_UPDATE_DATE', 'ENC_DATE', 'PK_ENCOUNTER_ID', 'DX_TYPE', 'HAR_NUMBER')))
  rar_lab %<>% 
    select(-one_of(c('EMPI', 'PK_ENCOUNTER_ID', 'ENC_DATE')))
  rar_demo %<>% select(-one_of(c('PK_PATIENT_ID')))
  
  
  
  # get the number of Dx's
  n_dx <- sum(grepl("^CODE|^Dx", names(rar_dx)))
  
  

  ## id <- "EMPI_DATE"
  
  ## Full Join: keep all info
  rar_mg <- rar_dx %>% 
    full_join(., rar_enc, by = id) %>% 
    full_join(., rar_lab, by = id) %>% 
    full_join(., rar_demo, by = pt_id)
  
  
  # get the start and end col number for Dx codes in merged data set
  dx_code_start <- which(names(rar_mg) == id)[1] + 1
  dx_code_end <-  n_dx + dx_code_start - 1

  ## For NA's in Dx, change them into 0
  rar_mg[, dx_code_start:dx_code_end] <- lapply(rar_mg[, dx_code_start:dx_code_end], 
                                                function(x) { ifelse(is.na(x), 
                                                                     0, 
                                                                     x) })
  
  ## Add age
  ## extract ENC_DATE from EMPI_DATE
  # TODO: pass enc_date explicitly
  rar_mg$ENC_DATE <- as.POSIXct(substr(rar_mg$EMPI_DATE, 12, 21), 
                                format = "%Y-%m-%d")
  rar_mg$Age <- floor(as.numeric(rar_mg$ENC_DATE - rar_mg$BIRTH_DATE)/(3600 * 24 * 365.25))
  
  # TODO: add a log for Age of -1
  rar_mg$Age <- ifelse(rar_mg$Age <= 0, 0, rar_mg$Age)
  
  
  return(rar_mg)
}
