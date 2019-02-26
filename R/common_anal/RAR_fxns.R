suppressMessages(library(assertthat))
suppressMessages(library(data.table))
suppressMessages(library(lubridate))
suppressMessages(library(tidyr))

# script.dir <- dirname(sys.frame(1)$ofile)
# source(paste(script.dir, "R_fxns.R", sep="/"))
source("R_fxns.R")

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



load_RAR_enc <- function(dat_file, service_grouping_file = "../../ref_data/Services.DSH_11_17.csv", 
                         bp_only = FALSE, EMPI_DATE_Level = TRUE, outpatient_only = TRUE, left_censor_date = as.Date("1997-01-01"), logger = NULL){
  #' Load in the RAR Encounter data, and do some basic cleaning
  #' @param dat_file string Raw data text file location 
  #' @param service_grouping_file string Grouping file for SERVICE_MASTER_CODE
  #' @param bp_only logic If TRUE, only keep BP's, excluding all other detailed info. Default is FALSE
  #' @param EMPI_DATE_Level logic If TRUE, collapse into EMPI_DATE Level. Default is TRUE
  #' @param outpatient_only logic If TRUE, only include Outpatients. Default is TRUE
  #' @param left_censor_date POSIXct Left-censoring date
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
                  MASTER_LOCATION_ZIP = 'character',
                  MASTER_LOCATION_FACILITY_YN = 'character', MASTER_LOCATION_CLINIC_YN = 'character',
                  MASTER_LOCATION_ENTITY = 'character', SERVICE_MASTER_CODE = 'character', 
                  SERVICE_MASTER_DESCRIPTION = 'character', 
                  BP_SYSTOLIC = 'numeric', BP_DIASTOLIC = 'numeric')
  

  rar_enc <- fread_epic(dat_file = dat_file, colClasses = colClasses, logger = logger)
  
  
  
  
  # initial EMPI's
  msg <- sprintf("load_RAR_enc: Read in raw encounter data. %d unique EMPI's, in %d total rows.", length(unique(rar_enc$EMPI)), nrow(rar_enc))
  if(!is.null(logger)) logger$info(msg)
  
  # left censor data
  if(!is.null(left_censor_date)){
    rar_enc %<>% filter(ENC_DATE >= left_censor_date)
    msg <- sprintf("load_RAR_enc: Encounter data were left censored at %s", left_censor_date)
    if(!is.null(logger)) logger$info(msg)
  }

  if(outpatient_only){
    # Assert that PATIENT_MASTER_CLASS is present
    if(!("PATIENT_MASTER_CLASS" %in% names(rar_enc))){
      if(!is.null(logger)) logger$error("load_RAR_enc: PATIENT_MASTER_CLASS does not exist")
      stop("PATIENT_MASTER_CLASS does not exist")
    }
    
    rar_enc %<>% 
      filter(PATIENT_MASTER_CLASS == "OUTPATIENT")
    
    msg <- sprintf("load_RAR_enc: Only OUTPATIENT is selected from ENC data. %d unique EMPI's, in %d total rows.", length(unique(rar_enc$EMPI)), nrow(rar_enc))
    if(!is.null(logger)) logger$info(msg)
  }
  

    if(bp_only){
    # only keep BP's
    rar_enc %<>% 
      select(EMPI, PK_ENCOUNTER_ID, ENC_DATE, E_SOURCE_LAST_UPDATE, 
             BP_SYSTOLIC, BP_DIASTOLIC)
      
      msg <- sprintf("load_RAR_enc: ONLY BP info is selected from enc data.")
      if(!is.null(logger)) logger$info(msg)
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

  msg <- sprintf("load_RAR_enc: Before collapsing, %d unique EMPI_DATE, in %d total rows.", length(unique(rar_enc$EMPI_DATE)), nrow(rar_enc))
  if(!is.null(logger)) logger$info(msg)
  
  
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
                funs(median(., na.rm = TRUE))) %>%
      slice(1) %>%
      ungroup()

    # re-assign ENC_TYPE_MASTER_CODE as character
    r1$ENC_TYPE_MASTER_CODE <- as.character(r1$ENC_TYPE_MASTER_CODE)
    
    # merge r1 back to r0
    rar_enc <- r0 %>% bind_rows(., r1) %>% ungroup()
    
    
    
    # regrouping SERVICE_MASTER_CODE
    service_grp <- read.csv(file=service_grouping_file, stringsAsFactors = FALSE, header = TRUE)
    service_grp %<>% 
      group_by(SERVICE_MASTER_CODE) %>% 
      slice(1) %>% 
      select(SERVICE_GROUP_def) %>% 
      ungroup()
    
    rar_enc %<>% left_join(., service_grp, by="SERVICE_MASTER_CODE")
    
    
    msg <- sprintf("load_RAR_enc: SERVICE_MASTER_CODE[%s] was grouped into SERVICE_GROUP_def [%s].", 
                   length(unique(rar_enc$SERVICE_MASTER_CODE)),length(unique(rar_enc$SERVICE_GROUP_def)))
    if(!is.null(logger)) logger$info(msg)
    
    
    
    msg <- sprintf("load_RAR_enc: After collapsing into EMPI_DATE level, %d unique EMPI_DATE, in %d total rows.", 
                   length(unique(rar_enc$EMPI_DATE)), nrow(rar_enc))
    if(!is.null(logger)) logger$info(msg)
    
    return(rar_enc)
    
  }

  
  # TODO: If HAR_Level = FALSE, need to collapse into PK_ENCOUNTER_ID level
  
  
  # regrouping SERVICE_MASTER_CODE
  service_grp <- read.csv(file=service_grouping_file, stringsAsFactors = FALSE, header = TRUE)
  service_grp %<>% 
    group_by(SERVICE_MASTER_CODE) %>% 
    slice(1) %>% 
    select(SERVICE_GROUP_def) %>% 
    ungroup()
  
  rar_enc %<>% left_join(., service_grp, by="SERVICE_MASTER_CODE")
  
  msg <- sprintf("load_RAR_enc: SERVICE_MASTER_CODE[%s] was grouped into SERVICE_GROUP_def [%s].", 
                 length(unique(rar_enc$SERVICE_MASTER_CODE)),length(unique(rar_enc$SERVICE_GROUP_def)))
  if(!is.null(logger)) logger$info(msg)
  
    
  
  
  msg <- sprintf("load_RAR_enc: Returned clean-up enc data. %d unique EMPI's, %d unique EMPI_DATE, in %d total rows.", 
                 length(unique(rar_enc$EMPI)),length(unique(rar_enc$EMPI_DATE)), nrow(rar_enc))
  if(!is.null(logger)) logger$info(msg)
  
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
  
  msg <- sprintf("load_RAR_Dx: Read in raw Dx data. %d unique EMPI, %d unique Dx codes, in %d total rows.", 
                 length(unique(rar_dx$EMPI)), length(unique(rar_dx$CODE)),nrow(rar_dx))
  if(!is.null(logger)) logger$info(msg)
  
  # remove those without ICD indicator (Since they also do not have CODE)
  rar_dx %<>% filter(CODE_STANDARD_NAME != "")
  
  
  # change PRIMARY_YN into factor
  rar_dx$PRIMARY_YN <- ifelse(rar_dx$PRIMARY_YN == "1", TRUE, FALSE)
  
  
  msg <- sprintf("load_RAR_Dx: After remove missing Dx from raw dx . %d unique EMPI, %d unique Dx codes, in %d total rows.", 
                 length(unique(rar_dx$EMPI)), length(unique(rar_dx$CODE)),nrow(rar_dx))
  if(!is.null(logger)) logger$info(msg)
  
  return(rar_dx)
  
}

sub_RAR_dx <- function(dat, 
                       icd_map_file, CODE_Level, hierarchy_dx,
                       EMPI_DATE_Level = TRUE, outpatient_only = TRUE,
                       left_censor_date = as.Date("1997-01-01"),
                       logger = NULL){
  #' Getting a subset for Dx data, in a wide data set
  #' @param dat tibble Pre-processed RAR Dx data
  #' @param icd_map_file character ICD mapping file
  #' @param CODE_Level character Specify on which level to select the Dx codes: "ICD", "Dx_h0", "Dx_h1", or "Dx_h2"
  #' @param hierachy_dx vector A character vector that specifies Dx in corresponding hierarchy level
  #' @param EMPI_DATE_Level logit If TRUE, collapse data into EMPI_DATE Level. Default is TRUE
  #' @param outpatient_only logit If TRUE, only include Outpatients. Default is TRUE.
  #' @param left_censor_date POSIXct Left-censoring date
  #' @return ret tibble A table for Dx's in ICD, Dx_h0, Dx_h1, Dx_h2 levels, and Dx counts for all EMPI_DATE (outpatient by default)

  ret <- tibble()  # return tibble
  spec_dx <- c() # specific dx's captured
  
  # left censor data
  if(!is.null(left_censor_date)){
    dat %<>% filter(ENC_DATE >= left_censor_date)
    msg <- sprintf("sub_RAR_dx: Dx data were left censored at %s. %d unique EMPI, %d unique Dx codes, in %d total rows.", 
                   left_censor_date, length(unique(dat$EMPI)), length(unique(dat$CODE)),nrow(dat))
    if(!is.null(logger)) logger$info(msg)
  }
  
  
  dat$EMPI_DATE <-  paste(dat$EMPI, 
                          format(dat$ENC_DATE, "%Y-%m-%d"))
  
  if(outpatient_only){
    dat %<>% 
      filter(PATIENT_MASTER_CLASS == "OUTPATIENT")
    
    msg <- sprintf("sub_RAR_dx: ONLY Dx with PATIENT_MASTER_CLASS = OUTPATIENT are selected. %d unique EMPI, %d unique Dx codes, in %d total rows.",
                   length(unique(dat$EMPI)), length(unique(dat$CODE)),nrow(dat))
    if(!is.null(logger)) logger$info(msg)
  }
  
  
  # filter based on Selected Dx's
  # CODE_Level = "Dx_h1"
  # hierarchy_dx <- c("Essential_HTN", "Primary_aldosteronism")
  icd_map <- icd_mapping(icd_map_file = icd_map_file, dx_hierarchy_level = CODE_Level, dx_hierarchy_level_value = hierarchy_dx)
  
  
  
  ret <- dat %>% filter(CODE %in% icd_map$CODE)
  msg <- sprintf("sub_RAR_dx: ONLY %s level Dx code(s) [%s] were selected!  %d unique EMPI, %d unique ICD codes (with those CODE's), in %d total rows.", 
                 CODE_Level, paste(hierarchy_dx, collapse = ", "),  length(unique(ret$EMPI)), length(unique(ret$CODE)),nrow(ret))
  if(!is.null(logger)) logger$info(msg)
  
  # # Create new ENC ID: EMPI_DATE
  # # TODO: pull this out into function
  # ret$EMPI_DATE <- paste(ret$EMPI, 
  #                        format(ret$ENC_DATE, "%Y-%m-%d"))
  
  # Add number of ALL Dx's for each encounter (EMPI_DATE) at the very beginning
  Dx_ct <- dat %>% 
    group_by(EMPI_DATE) %>%
    summarise(n_Dx_enc = n_distinct(CODE))
  
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
    ## fill Dx_h0, Dx_h1, Dx_h2 with sums, instead of 1/0
    ret_icd <- ret %>% ungroup() %>% 
      filter(!duplicated(.)) %>%
      mutate(value = 1) %>%
      spread(CODE, value, fill = 0, sep = "_")
    
    ## NOTE: since there are redundency in icd mapping (like 401.9 to Dx_h2 I10/I16), only the first Dx_h will be selected here and the following:
    ret_h0 <- ret %>% left_join(., icd_map %>% select(CODE, Dx_h0) %>% group_by(CODE) %>% arrange(Dx_h0) %>% slice(1), by = "CODE") %>% 
      ungroup() %>%
      select(EMPI_DATE, Dx_h0) %>%
      group_by(EMPI_DATE, Dx_h0) %>%
      summarise(N=n()) %>%
      ungroup() %>%
      spread(Dx_h0, N, fill = 0, sep = "_")
    
    ## See NOTE above
    ret_h1 <- ret %>% left_join(., icd_map %>% select(CODE, Dx_h1) %>% group_by(CODE) %>% arrange(Dx_h1) %>% slice(1), by = "CODE") %>% 
      ungroup() %>%
      select(EMPI_DATE, Dx_h1) %>%
      group_by(EMPI_DATE, Dx_h1) %>%
      summarise(N=n()) %>%
      ungroup() %>%
      spread(Dx_h1, N, fill = 0, sep = "_")
    
    ## See NOTE above
    ret_h2 <- ret %>% left_join(., icd_map %>% select(CODE, Dx_h2) %>% group_by(CODE) %>% arrange(Dx_h2) %>% slice(1), by = "CODE") %>% 
      ungroup() %>%
      select(EMPI_DATE, Dx_h2) %>%
      group_by(EMPI_DATE, Dx_h2) %>%
      summarise(N=n()) %>%
      ungroup() %>%
      spread(Dx_h2, N, fill = 0, sep = "_")
    
    
    # QC: check whether some EMPI_DATE got dropped
    n0 <- length(unique(ret$EMPI_DATE))
    n_icd <- length(unique(ret_icd$EMPI_DATE))
    n_h0 <- length(unique(ret_h0$EMPI_DATE))
    n_h1 <- length(unique(ret_h1$EMPI_DATE))
    n_h2 <- length(unique(ret_h2$EMPI_DATE))
    if(!(n0 == n_icd & n0 == n_icd & n0 == n_h0 & n0 == n_h1 & n0 == n_h2)){
      if(!is.null(logger)) logger$warn("sub_RAR_dx: some EMPI_DATE got dropped in converting Dx to higher levels")
    }
    
    
    ret <- full_join(ret_icd, ret_h0, by = "EMPI_DATE") %>%
      full_join(., ret_h1, by = "EMPI_DATE") %>%
      full_join(., ret_h2, by = "EMPI_DATE")
    
    
  }
  
  
  empi_date_1 <- length(unique(ret$EMPI_DATE))
  
  
  if(empi_date_0 != empi_date_1){
    err_msg <- sprintf("sub_RAR_dx: missing EMPI_DATEs after collapsing into EMPI_DATE level (Should be %d; Actually: %d)", empi_date_0, empi_date_1)
    if(!is.null(logger)) logger$warn(err_msg)
    warning(err_msg)
  }else{
    msg <- sprintf("sub_RAR_dx: all EMPI_DATEs are present (Number: %d) after collapsing to EMPI_DATE level", empi_date_1)
    if(!is.null(logger)) logger$warn(msg)
  }
  
  
  # Add dx counts in, put NA as 0
  ret %<>% full_join(., Dx_ct, by="EMPI_DATE")
  ret[is.na(ret)] <- 0
  
  # TODO: If HAR_Level = FALSE, need to collapse into PK_ENCOUNTER_ID level
  dx_icd_cat <- names(ret)[grepl("^CODE",names(ret))]
  dx_h0_cat <- names(ret)[grepl("^Dx_h0", names(ret))]
  dx_h1_cat <- names(ret)[grepl("^Dx_h1", names(ret))]
  dx_h2_cat <- names(ret)[grepl("^Dx_h2", names(ret))]
  empi <- substr(ret$EMPI_DATE, 1, 10)
  msg <- sprintf("sub_RAR_dx: Returned Dx data: %d unique EMPI, all %d unique EMPI_DATE (including all EMPI_DATE with/without selected ICD CODE's), %d unique EMPI_DATE (with selected ICD CODE's), %d unique ICD codes, %d Dx H0 Codes (%s), %d unique Dx H1 Codes (%s), %d unique Dx H2 Codes, in %d total rows.", 
                 length(unique(empi)), length(unique(ret$EMPI_DATE)), empi_date_1, 
                 length(unique(dx_icd_cat)), 
                 length(unique(dx_h0_cat)), paste(unique(dx_h0_cat), collapse = ", "),
                 length(unique(dx_h1_cat)), paste(unique(dx_h1_cat), collapse = ", "),
                 length(unique(dx_h2_cat)),
                 nrow(ret))
  if(!is.null(logger)) logger$info(msg)
  
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
                  RACE_MASTER_CODE = 'character', RACE_MASTER_HISPANIC_YN = 'character', ZIP='character')
  
  rar_demo <- fread_epic(dat_file = dat_file, colClasses = colClasses, logger = logger)
  
  
  
  
  msg <- sprintf("load_RAR_PtDemo: Read in Patient Demo data. %d unique EMPI's, %d total rows", length(unique(rar_demo$EMPI)), nrow(rar_demo))
  if(!is.null(logger)) logger$info(msg)
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
  
  msg <- sprintf("load_RAR_PtDemo: Cleaned Patient Demo data. %d unique EMPI's", length(unique(rar_demo$EMPI)))
  if(!is.null(logger)) logger$info(msg)
  
  return(rar_demo)
  
}



load_Lab <- function(dat_file,  lab_source, adjust_up = 1.5, adjust_down = 0.5, 
                     left_censor_date = as.Date("1997-01-01"), allow_missing_columns=FALSE,
                     logger = NULL, ...){
  #' Load in the Lab data, and do some basic cleaning. Lab data includes RAR_Lab and RAR_V3
  #' @param dat_file string Raw data text file location 
  #' @param lab_source character Lab test source: EPIC, CERNER or RAR
  #' @param adjust_up numeric Rate for adjusting "> X", default is 0.5
  #' @param adjust_donw numeric Rate for adjusting "< X", default is 1.5
  #' @param left_censor_date POSIXct Left-censoring date
  #' @param allow_missing_columns Bool If true, then allow columns to be missing from specified colClasses. Only use for exploratory work
  #' @param logger logger For logging purpose. Default is NULL
  #' @return rar_lab tibble pre-processed RAR Dx data
  
  
  
  # read in the files with specified column classes
  if(lab_source == "PDS_EPIC"){
    # specify Column Classes
    colClasses <- c(ENC_DATE = "PDS_DateTime", ORDER_START_DATE = "PDS_DateTime", O_SOURCE_LAST_UPDATE = "PDS_DateTime", 
                    RESULT_DATE = "PDS_DateTime", 
                    EMPI = 'character', PK_ENCOUNTER_ID = 'character', PK_ORDER_ID = 'character', ORDER_NAME = 'character', 
                    ORDER_ITEM_CODE = 'character', PK_ORDER_RESULT_ID = 'character', RESULT_VALUE = 'character',
                    UNIT_OF_MEASURE = 'character', RESULT_STATUS = 'character', RESULT_ITEM_CODE = 'character', 
                    ORDERING_PROV = 'character', HAR_NUMBER = 'character', 
                    ORDER_GROUP = 'character', RESULT_TEXT = 'character', RESULT_RESOURCE = "character", 
                    ORES_SOURCE_LAST_UPDATE = "PDS_DateTime", LOINC_CODE = 'character')
  }else if(lab_source == "PDS_CERNER"){
    colClasses <-  c(ENC_DATE = "PDS_DateTime", ORDER_START_DATE = "PDS_DateTime", O_SOURCE_LAST_UPDATE = "PDS_DateTime", 
                    RESULT_DATE = "PDS_DateTime", 
                    EMPI = 'character', PK_ENCOUNTER_ID = 'character', PK_ORDER_ID = 'character', ORDER_NAME = 'character', 
                    ORDER_ITEM_CODE = 'character', PK_ORDER_RESULT_ID = 'character', RESULT_VALUE = 'character',
                    UNIT_OF_MEASURE = 'character', RESULT_STATUS = 'character', RESULT_ITEM_CODE = 'character', 
                    HAR_NUMBER = 'character', 
                    ORDER_GROUP = 'character', RESULT_TEXT = 'character', RESULT_RESOURCE = "character", 
                    ORES_SOURCE_LAST_UPDATE = "PDS_DateTime", LOINC_CODE = 'character')
    
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
    err_msg <- sprintf("load_Lab: Unspecified Lab Source: %s", lab_source)
    if(!is.null(logger)) logger$error(err_msg)
    stop(err_msg)
  }
 
  if (allow_missing_columns) { #Exclude columns not in dataset
    data_columns <- read.csv(dat_file,nrows = 1, header = T) %>% names(.)
    cols_to_exclude <- names(colClasses)[which(!names(colClasses) %in% data_columns)]
    if(length(cols_to_exclude)) {
      if(!is.null(logger)) logger$warn(paste("load_Lab: Excluding columns:", paste(cols_to_exclude, collapse=", ")))
    
      colClasses <- colClasses[which(names(colClasses) %in% data_columns)]
    }
  }
  
  
  rar_lab <- fread_epic(dat_file = dat_file, colClasses = colClasses, logger = logger)

  
  msg <- sprintf("load_Lab: [%s] Read raw Lab data: %d EMPI's, in %d total rows.", 
                 lab_source,length(unique(rar_lab$EMPI)), nrow(rar_lab))
  if(!is.null(logger)) logger$info(msg)
  
  # modify RESULT_VALUE, using numericize function
  rar_lab$RESULT_VALUE <- numericize(rar_lab$RESULT_VALUE, adjust_up = adjust_up, adjust_down = adjust_down)
  
  
  # left censor data
  if(!is.null(left_censor_date)){
    rar_lab %<>% filter(ENC_DATE >= left_censor_date)
    msg <- sprintf("load_Lab: [%s] Encounter data were left censored at %s. %d EMPI's, in %d total rows.", 
                   lab_source, left_censor_date, length(unique(rar_lab$EMPI)), nrow(rar_lab))
    if(!is.null(logger)) logger$info(msg)
  }
  
  
  
  msg <- sprintf("load_Lab: [%s] Finish reading in ALL Lab data: %d EMPI's, in %d total rows.", 
                 lab_source, length(unique(rar_lab$EMPI)), nrow(rar_lab))
  if(!is.null(logger)) logger$info(msg)
  
  return(rar_lab)
  
}


select_RAR_lab <- function(dat, lab_source, 
                           RAR_only = TRUE, #EPIC
                           potassium_in = TRUE,  #EPIC
                           num_labs = 0,  #EPIC
                           logger = NULL, ...){
  #' This function is used to select labs, instead of using all labs. The returned data set is not collapsed.
  #' @param dat tibble Pre-cleaned RAR data 
  #' @param lab_source character Lab source: "PDS_EPIC" or "PDS_CERNER" 
  #' NOTE: Currently, only [AVS] tests were drawn from CERNER. RAR, potassium [U Aldo] and all other tests were from EPIC.
  #' NOTE: When lab_source is "CERNER", all other EPIC-related options would be no longer in use (since we don't need them in CERNER for now)
  #' @param RAR_only logit If TRUE, will only return Aldo/Renin results
  #' @param potassium_in logit If TRUE, potassium lab results will be included. Default is TRUE
  #' Note: RESULT_ITEM_CODE may have several CODEs that include "POTASSIUM", but only "POTASSIUM" (exact matching) would be selected now
  #' @param num_labs numeric Number of labs results included. Note that RAR and potassium labs are not counted in num_labs
  #' @return ret tibble pre-processed RAR Dx data

 
  dat %<>% filter(!is.na(dat$PK_ORDER_RESULT_ID))
  
  msg <- sprintf("lab_select: [%s] Records with missing PK_ORDER_RESULT_ID were removed. %d EMPI's, %d total rows.", 
                 lab_source, length(unique(dat$EMPI)), nrow(dat))
  if(!is.null(logger)) logger$info(msg)
  

  # Create new ENC ID: EMPI_DATE
  dat$EMPI_DATE <- paste(dat$EMPI, format(dat$ENC_DATE, "%Y-%m-%d"))
  
  msg <- sprintf("lab_select: [%s] All Labs: %d EMPI's, %d EMPI_Date's, %d total rows.", 
                 lab_source, length(unique(dat$EMPI)), length(unique(dat$EMPI_DATE)), nrow(dat))
  if(!is.null(logger)) logger$info(msg)
  
  
  
  # Create ret table
  ret <- as.tibble()
  
  
  
  
  
  if(lab_source == "PDS_CERNER"){
    # select Labs from CERNER
    # Note that to avoid `SAMPLE` fom RESULT_ITEM_CODE, only records with non-NA RESULT_VALUE were kept
    dat %<>% filter(!is.na(dat$RESULT_VALUE))
    
    
    ## AVS
    ### some Left Cortisols [C LCORT, LCORT] were all renamed to LEFT CORTISOL
    if(sum(dat$RESULT_ITEM_CODE %in% c("RIGHT CORTISOL", "LEFT CORTISOL", "IVC CORTISOL")) & !is.null(logger)){
      logger$info("lab_select: PDS_CERNER AVS lab of 'RIGHT CORTISOL', 'LEFT CORTISOL', 'IVC CORTISOL' were renamed.")
    }
    
    
    avs_lab <- dat %>% 
      filter(RESULT_ITEM_CODE %in% c("Right Aldos", "Left Aldos", "IVC Aldosterone", "RCORT", "RIGHT CORTISOL", "LCORT", "LEFT CORTISOL","ICORT", "IVC CORTISOL")) %>%  
      ## NOTE: after removing NA from result value, this mutate does nothing, since RIGHT/LEFT/IVC CORTISOL were removed in current CERNER data
      mutate(RESULT_ITEM_CODE = case_when(RESULT_ITEM_CODE %in% c("IVC Aldosterone") ~ "IVC Aldos",
                                          RESULT_ITEM_CODE %in% c("Right Aldos") ~ "Right Aldos",
                                          RESULT_ITEM_CODE %in% c("Left Aldos") ~ "Left Aldos",
                                          RESULT_ITEM_CODE %in% c("RCORT", "RIGHT CORTISOL") ~ "RCORT",
                                          RESULT_ITEM_CODE %in% c("LCORT", "LEFT CORTISOL") ~ "LCORT",
                                          RESULT_ITEM_CODE %in% c("ICORT", "IVC CORTISOL") ~ "ICORT",
                                          TRUE ~ RESULT_ITEM_CODE)) %>%
      mutate(Test = RESULT_ITEM_CODE) %>%
      ## NOTE: units were checked in AVS_lab.Rmd so that we could simply impute NA's, but for later version, it's not guaranteed
      mutate(UNIT_OF_MEASURE = case_when(RESULT_ITEM_CODE %in% c("IVC Aldos", "Right Aldos", "Left Aldos") ~ "ng/dL",
                                         TRUE ~ "ug/dL"))
      
    
    
    ret %<>% bind_rows(., avs_lab)
    
    msg <- sprintf("lab_select: [%s] %d different AVS lab tests were selected.",
                   lab_source, length(unique(ret$Test)))
    
    # ## Aldosterone, Urine
    # ualdo <- dat %>% 
    #   filter(RESULT_ITEM_CODE %in% c("U Aldosterone","Cre D Aldos")) %>%
    #   mutate(UNIT_OF_MEASURE = ifelse(RESULT_ITEM_CODE == "U Aldosterone", "ug/day", UNIT_OF_MEASURE)) %>% 
    #   mutate(Test = RESULT_ITEM_CODE)
    # 
    # ret %<>% bind_rows(., ualdo)
    
    
    # remove duplicates, if any
    if(sum(duplicated(ret))){
      if(!is.null(logger)) logger$warn("lab_select: [PDS_CERNER] Duplicated rows in Lab's are present. Removed duplicates.")
      ret %<>% filter(!duplicated(ret))
    }
   
    
    
  }else if(lab_source == "PDS_EPIC"){
    
    # select Labs from EPIC
    # ## Explicitly exclude RESULT_ITEM_CODE's that are imported from CERNER
    # ##### this is AVS in EPIC
    # dat %<>% filter(!(
    #                   # RESULT_ITEM_CODE %in% c("CRE D ALDOS", "24HR URINE CREATININE","ALDOSTERONE, URINE", "URINE ALDOSTERONE") | 
    #                   grepl("^ivc aldosterone|^right aldosterone|^left aldosterone|^ivc cortisol|^right cortisol|^left cortisol|^c lcort|^lcort", 
    #                         tolower(RESULT_ITEM_CODE))
    #                   )
    #                 ) 
    
    ### # NOTE: above filter was commented out, because we still hope to get AVS from EPIC, which serves as a complement for CERNER
    
    ##### this is Aldo U in CERNER
    dat %<>% filter(!(RESULT_ITEM_CODE %in% c(# "Right Aldos", "Left Aldos", "IVC Aldosterone", 
                                              # "RCORT", "RIGHT CORTISOL", "LCORT", "LEFT CORTISOL",
                                              # "ICORT", "IVC CORTISOL",
                                              "U Aldosterone","Cre D Aldos"))) 
    
    
    
    
    
    # Duplicates
    if(sum(duplicated(dat))){
      if(!is.null(logger)) logger$warn("lab_select: [PDS_EPIC] Duplicated rows in Lab's are present")
      dat %<>%
        filter(!duplicated(dat))
    }
    
    
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
    
    
    msg <- sprintf("lab_select: RAR Labs: %d EMPI's, %d EMPI_Date's, %d total rows.", 
                   length(unique(ret$EMPI)), length(unique(ret$EMPI_DATE)), nrow(ret))
    if(!is.null(logger)) logger$info(msg)
    
    
    
    if(RAR_only){
      # The picking function applied here
      ret <- collapse_lab_PK_ORDER_ID_RIC(ret)
      
      ret %<>% ungroup()
      
      
      msg <- sprintf("lab_select: [%s] ONLY RAR Labs are returnd as Lab data: %d EMPI's, %d EMPI_Date's, %d total rows.", 
                     lab_source, length(unique(ret$EMPI)), length(unique(ret$EMPI_DATE)), nrow(ret))
      if(!is.null(logger)) logger$info(msg)
      
      
      return(ret)
    }
    
    # AVS
    avs <- dat %>% 
      filter(RESULT_ITEM_CODE %in% c("IVC ALDOSTERONE", "IVC CORTISOL", "LEFT ALDOSTERONE", "LEFT CORTISOL", "RIGHT ALDOSTERONE", "RIGHT CORTISOL", "LCORT", "C LCORT")) %>%
      ## change the names according to CERNER
      mutate(RESULT_ITEM_CODE = case_when(RESULT_ITEM_CODE %in% c("LCORT", "C LCORT", "LEFT CORTISOL") ~ "LCORT",
                                          RESULT_ITEM_CODE %in% c("IVC ALDOSTERONE") ~ "IVC Aldos",
                                          RESULT_ITEM_CODE %in% c("IVC CORTISOL") ~ "ICORT",
                                          RESULT_ITEM_CODE %in% c("LEFT ALDOSTERONE") ~ "Left Aldos",
                                          RESULT_ITEM_CODE %in% c("RIGHT ALDOSTERONE") ~ "Right Aldos",
                                          RESULT_ITEM_CODE %in% c("RIGHT CORTISOL") ~ "RCORT", 
                                          TRUE ~ "Unknown")) %>%
      mutate(UNIT_OF_MEASURE = case_when(RESULT_ITEM_CODE %in% c("Left Aldos", "Right Aldos", "IVC Aldos") ~ "ng/dL",
                                         RESULT_ITEM_CODE %in% c("LCORT","RCORT","ICORT") ~ "ug/dL",
                                         TRUE ~ "Unknown")) %>%
      mutate(Test = RESULT_ITEM_CODE)
    
    
    
    ret <- avs %>% bind_rows(., ret)
    
    # U Aldo
    ualdo <- dat %>% 
      filter(RESULT_ITEM_CODE %in% c("24HR URINE CREATININE", "CRE D ALDOS", "URINE ALDOSTERONE", "ALDOSTERONE, URINE")) %>%
      filter(UNIT_OF_MEASURE != "mg/dL" | is.na(UNIT_OF_MEASURE)) %>%
      mutate(RESULT_VALUE = ifelse(RESULT_ITEM_CODE == "24HR URINE CREATININE" & UNIT_OF_MEASURE %in% c("g/24-h", "g/24 h", "G/24HR"),
                                   RESULT_VALUE*1000, RESULT_VALUE)) %>%
      mutate(UNIT_OF_MEASURE = case_when(RESULT_ITEM_CODE %in% c("24HR URINE CREATININE", "CRE D ALDOS") ~ "mg/day",
                                         RESULT_ITEM_CODE %in% c("ALDOSTERONE, URINE", "URINE ALDOSTERONE") ~ "ug/day",
                                         TRUE ~ "Unknown")) %>%
      ## NOTE: merge 4 to 2 tests
      mutate(RESULT_ITEM_CODE = case_when(RESULT_ITEM_CODE %in% c("24HR URINE CREATININE", "CRE D ALDOS") ~ "24HR URINE CREATININE",
                                         RESULT_ITEM_CODE %in% c("ALDOSTERONE, URINE", "URINE ALDOSTERONE") ~ "URINE ALDOSTERONE",
                                         TRUE ~ "Unknown")) %>%
      mutate(Test = RESULT_ITEM_CODE)
    
    ret <- ualdo %>% bind_rows(., ret)

    
   
    
    # Include POTASSIUM
    if(potassium_in){
      # list of labs to include?
      ret <- dat  %>% filter(RESULT_ITEM_CODE == "POTASSIUM") %>% 
        mutate(Test = "POTASSIUM") %>%
        bind_rows(., ret)
      
      msg <- sprintf("lab_select: [%s] Potassium Labs included: %d unique tests, %d EMPI's, %d EMPI_Date's, %d total rows.", 
                     lab_source, length(unique(ret$Test)),length(unique(ret$EMPI)), length(unique(ret$EMPI_DATE)), nrow(ret))
      if(!is.null(logger)) logger$info(msg)
      
      
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
      
      
      # explicitly exclude U Aldo labs here
      ric_lt %<>% filter(!(RESULT_ITEM_CODE %in% c("24HR URINE CREATININE", "CRE D ALDOS", "URINE ALDOSTERONE", "ALDOSTERONE, URINE")))
      
      ret <- dat %>% filter(RESULT_ITEM_CODE %in% ric_lt$RESULT_ITEM_CODE) %>%
        mutate(Test = RESULT_ITEM_CODE) %>%
        bind_rows(., ret)
      
      
      msg <- sprintf("lab_select: [%s] %s Labs included: %d unique tests, %d EMPI's, %d EMPI_Date's, %d total rows.", 
                     lab_source, num_labs, length(unique(ret$Test)),length(unique(ret$EMPI)), length(unique(ret$EMPI_DATE)), nrow(ret))
      if(!is.null(logger)) logger$info(msg)
      
      
    }
    
    
    ret %<>% ungroup()
    
  }else{
    # lab_source is neither EPIC nor CERNER
    
    msg <- sprintf("lab_select: [%s] unrecognized lab_source [%s]", 
                   lab_source, lab_source)
    if(!is.null(logger)) logger$error(err_msg)
    stop(err_msg)
    
  }
  
  
  
  
  msg <- sprintf("lab_select: [%s]. Returned Lab data: %d unique tests, %d EMPI's, %d EMPI_Date's, %d total rows.", 
                 lab_source, length(unique(ret$Test)),length(unique(ret$EMPI)), length(unique(ret$EMPI_DATE)), nrow(ret))
  if(!is.null(logger)) logger$info(msg)
  
  
  
  # add lab source
  ret$lab_source <- lab_source
  
  return(ret)
}




collapse_lab_EMPI_DATE <- function(dat, logger=NULL){
  #' This function is applied to cleaned lab data set[on PK_ORDER_ID + RESULT_ITEM_CODE level] with selected lab tests, 
  #' into EMPI_DATE level.
  #' The returned data set is long version of labs.
  #' NOTE: RESULT_STATUS should only be: "Verified", "Corrected", "Final", "Old Verified", "Old Corrected", "Preliminary", "Performed", "Old Performed", "Incomplete"
  #' All other RESULT_STATUS will be factored into NA.
  #' @param dat tibble Cleaned lab data set
  #' @return ret tibble Returned data set on EMPI_DATE level
  

  
  n_labs <- sum(grepl("^Test", names(dat))) + sum(names(dat) %in% c("Aldo", "PRA", "DRC"))
  msg <- sprintf("collapse_lab_EMPI_DATE: Before collapsing to EMPI_DATE level of Labs: %d EMPI's, %d EMPI_DATE's, in %d total rows",
                 length(unique(dat$EMPI)), length(unique(dat$EMPI_DATE)), n_labs, nrow(dat))
  if(!is.null(logger)) logger$info(msg)
  
  
  
  
  # For RAR use collapse_RAR_Lab function
  rar_lab <- subset(dat, Test %in% c("PRA", "DRC", "Aldo"))
  if(nrow(rar_lab) != 0){
    rar_lab_collapse <- collapse_RAR_Lab(dat=rar_lab, EMPI_DATE_Level = TRUE)
    
    ## make some changes to rar_lab
    rar_lab_collapse %<>% select(-c(PK_ENCOUNTER_ID, ORDER_START_DATE))
    
    ret <- rar_lab_collapse
    
  }
  
  
  # For AVS Labs
  avs_lab <- subset(dat, Test %in% c("Left Aldos", "Right Aldos", "IVC Aldos", "LCORT","RCORT","ICORT"))
  if(nrow(avs_lab) > 0){
    ### For RIC in both EPIC and CERNER, pick the PDS_CERNER one
    avs_lab %<>% mutate(EMPI_DATE_RIC = paste(EMPI_DATE, RESULT_ITEM_CODE), 
                        lab_source = factor(lab_source, levels = c("PDS_CERNER", "PDS_EPIC"))) %>%
      group_by(EMPI_DATE_RIC) %>%
      arrange(lab_source) %>%
      slice(1) %>% ungroup() %>%
      mutate(Test = RESULT_ITEM_CODE)
    
    ### put unit into Test
    avs_lab$Test <- paste(avs_lab$Test, avs_lab$UNIT_OF_MEASURE, sep = "_")
    
    
    ## remvoe cols
    avs_lab %<>% select(EMPI_DATE, Test, RESULT_VALUE)
    
    ## Spread on EMPI_DATE level
    avs_lab_EMPI_DATE <- avs_lab %>% spread(., key = Test, value = RESULT_VALUE, sep = "_")
    
   
    # merge RAR and other labs
    if(nrow(rar_lab) != 0){
      ret <- full_join(ret, avs_lab_EMPI_DATE, by = "EMPI_DATE")
    }else{
      ret <- avs_lab_EMPI_DATE
    }
   
    
    
  }

 
  
  
  # Other labs
  ### NOTE: the way we did for other labs is because we want to make sure most other labs were taken at the same time
  other_lab <- subset(dat, !(Test %in% c("PRA", "DRC", "Aldo", "Left Aldos", "Right Aldos", "IVC Aldos", "LCORT","RCORT","ICORT")))

  if(nrow(other_lab) == 0 & nrow(rar_lab) == 0 & nrow(avs_lab) == 0){
    stop("collapse_lab_EMPI_DATE: Lab is empty")
    
  }else if(nrow(other_lab) == 0){
    return(ret)
  }
  
  ## create a new temporary ID: EMPI_DATE + RESULT_ITEM_CODE and split into two sets
  other_lab$EMPI_DATE_RIC <- paste(other_lab$EMPI_DATE, other_lab$RESULT_ITEM_CODE)
  dups <- other_lab[duplicated(other_lab$EMPI_DATE_RIC), 'EMPI_DATE_RIC']
  r0 <- subset(other_lab, !(EMPI_DATE_RIC %in% dups$EMPI_DATE_RIC))
  r1 <- subset(other_lab, EMPI_DATE_RIC %in% dups$EMPI_DATE_RIC)
  
  ## for r1 (multiple RIC in EMPI_DATE), pick by 1) count of ORDER_START_DATE 2) arbitrary PK_ORDER_ID
  ### same test at same time (but from different ORDER: LIVER FUNCTIONAL PANEL & CMP), so use median to combine. After this, all RESULT_ITEM_CODE will have only one ORDER_START_DATE
  r1 %<>% group_by(EMPI_DATE_RIC, ORDER_START_DATE) %>% 
    mutate_at(vars(RESULT_VALUE), funs(median(., na.rm=TRUE))) %>% 
    slice(1)
  
  ### Pick by 1) count of ORDER_START_DATE 2) PK_ORDER_ID
  r1$EMPI_DATE_OSD <- paste(r1$EMPI_DATE, r1$ORDER_START_DATE)
  r1 <- r1 %>% group_by(EMPI_DATE_OSD) %>% 
    summarize(N=n()) %>% 
    ungroup() %>% 
    full_join(., r1, by = "EMPI_DATE_OSD") %>% 
    group_by(EMPI_DATE_RIC) %>% 
    arrange(desc(N), desc(PK_ORDER_ID)) %>% 
    slice(1)
  

  
  
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
  ret <- full_join(ret, other_lab_EMPI_DATE, by = "EMPI_DATE")
 
  # replace space in names with "_"
  names(ret) <- gsub(" ", "_", names(ret))
  
  
  # remake EMPI
  ret$EMPI <- substr(ret$EMPI_DATE, 1, 10)
  
  n_labs <- sum(grepl("^Test", names(ret))) + sum(names(ret) %in% c("Aldo", "PRA", "DRC"))
  msg <- sprintf("collapse_lab_EMPI_DATE: Returned EMPI_DATE level Lab data: %d EMPI's, %d EMPI_DATE's, %d unique labs, in %d total rows",
                 length(unique(ret$EMPI)), length(unique(ret$EMPI_DATE)), n_labs, nrow(ret))
  if(!is.null(logger)) logger$info(msg)
  
  return(ret)
}




collapse_lab_PK_ORDER_ID_RIC <- function(dat, lab_source){
  #' This function is used to collapse raw lab function into PK_ORDER_ID + RESULT_ITEM_CODE level
  #' which means for PK_ORDER_ID + RESULT_ITEM_CODE is the unique ID here
  #' This function will pick according to RESULT_STATUS: Verified > Corrected > Final > Old Verified > Old Corrected >
  #'  Preliminary > Performed > Old Performed> Incomplete > NA
  #' @param dat tibble Raw lab data frame
  #' @return ret tibble Returned collapsed Lab data
  
  
  # apply picking function: from all lab into PK_ORDER_ID + RESULT_ITEM_CODE level
  dat$oid_ric <- paste(dat$PK_ORDER_ID, dat$RESULT_ITEM_CODE, sep = "_")
  
  # factor RESULT_STATUS and assign levels
  if(lab_source == "PDS_EPIC"){
    dat$RESULT_STATUS <- factor(dat$RESULT_STATUS, levels = c("Corrected", "Final", "Preliminary", "Incomplete", NA))
  }else if(lab_source == "PDS_CERNER"){
    dat$RESULT_STATUS <- factor(dat$RESULT_STATUS, levels = c("Verified", "Corrected",  
                                                              "Old Verified", "Old Corrected", 
                                                              "Performed", "Old Performed", NA))
    
  }
  
  
  # a list of PK_ORDER_ID + RESULT_ITEM_CODE which has multiple records
  dups <- dat[which(duplicated(dat$oid_ric)), "oid_ric"]
  
  # separate dat into two parts
  r0 <- subset(dat, !(oid_ric %in% dups$oid_ric))
  r1 <- subset(dat, oid_ric %in% dups$oid_ric)
  
  # deal with those oid_ric which has multiple records
  # It's picking the median
  # TODO: 
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


get_labs <- function(dat_file, lab_source, select_lab=NULL,
                        logger=NULL, 
                     collapse_to_final_result=TRUE,
                     ...) {
  #' Capturing  labs from dat_file
  #' @param dat_file
  #' @param lab_source (EPIC, CERNER) [REALLY -- PDS/EPIC, PDS/CERNER]
  #' @param select_lab function for filter lab tests
  #' @return tibble

  lab_raw <- load_Lab(dat_file = dat_file, lab_source = lab_source,
                           logger=logger, ...)
  
  if (!is.null(select_lab)) {
    lab <- select_lab(dat = lab_raw, lab_source = lab_source, logger=logger, ...)
  }
  
  if (collapse_to_final_result) {
    lab <- collapse_lab_PK_ORDER_ID_RIC(lab, lab_source=lab_source)
  }
  
  msg <- sprintf("get_labs: Loaded and selected %s Labs", lab_source)
  if(!is.null(logger)) logger$info(msg)
  
  return(lab)
}



get_RAR_lab_EPIC_CERNER <- function(lab_file_epic, lab_file_cerner, ...) {
  
  get_lab_EPIC_CERNER(list("PDS_EPIC"=lab_file_epic, "PDS_CERNER"=lab_file_cerner),
                      RAR_only = FALSE, potassium_in = TRUE,
                      num_labs=44,
                      select_lab = select_RAR_lab, 
                      ...)
  
}




get_lab_EPIC_CERNER <- function(lab_sources = list("PDS_EPIC"=lab_file_PDS_epic, "PDS_CERNER"=lab_file_PDS_cerner),
                                logger = NULL, left_censor_date,
                                num_labs=0,
                                select_lab = NULL,
                                ...){
  #' This is a very generalized function to read in raw lab data from both EPIC and CERNER 
  #' and output a combined dataset for LAB data
  #' @param lab_file_epic character EPIC lab raw data csv file
  #' @param lab_file_cerner character CERNER lab raw data csv file
  #' @return ret tibble Wide version of combined EPIC and CERNER data sets.
  
  #############################################################
  #####    Read In EPIC & CERNER, Filtering Labs       ########
  #############################################################
  
  
  source_names <- names(lab_sources)
  
  lab_all <- do.call("rbind",
                     lapply(source_names,
                            function(x){
                              tmp <- get_labs(dat_file = unlist(lab_sources[x]), 
                                              lab_source = x, select_lab = select_lab,
                                              num_labs = num_labs, 
                                              logger=logger,...)
                              
                              
                              if(x == "PDS_EPIC"){
                                tmp %<>% select(-one_of("ORDERING_PROV"))
                              }
                              return(tmp)
                            }
                            )
                     )
  
  
  #############################################################
  #####           collapse to EMPI_DATE Level          ########
  #############################################################
  
  
  ret <- collapse_lab_EMPI_DATE(lab_all, 
                                logger=logger)
  
  
  
  
  
  
  return(ret)
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


rar_merge <- function(rar_dx, rar_enc, rar_lab, rar_demo, rar_note, rar_med, 
                      id, pt_id = "EMPI",
                      left_censor_date = as.Date("1997-01-01"),
                      join_to_ENC=TRUE, logger = NULL){
  #' This function is used to load in rar_enc,rar_dx, rar_lab, rar_demo data sets and merge them together
  #' for a encounter level data set, so all data sets are merged to encounter data
  #' @param rar_dx tibble Diagnosis data set
  #' @param rar_enc tibble Encounter data set
  #' @param rar_lab tibble Lab tests data set
  #' @param rar_demo tibble Patients' demographic data set
  #' @param rar_note tibble NOTE COUNT_REGEX data set
  #' @param rar_med tibble MED COUNT data set
  #' @param id character Unique ID to merge the 4 data sets together
  #' @param pt_id character Unique ID for patients. Default is EMPI
  #' @param left_censor_date POSIXct Left censoring date. Default is 1997-01-01
  #' @param join_to_ENC logit If FALSE, keep all info from all data sets. 
  #' Default is TRUE, and every other data sets were merged to Encounter data set.
  #' @return rar_mg tibble The merged data set
  
  
  ## drop some columns
  rar_enc %<>% 
    select(-one_of(c("E_SOURCE_LAST_UPDATE", "PK_ENCOUNTER_ID", "ENC_DATE", 'HAR_NUMBER')))
  rar_dx %<>% 
    select(-one_of(c('EMPI', 'CODE_STANDARD_NAME', 'COMMENTS', 'CODING_DATE', 'PRIMARY_YN', 'DX_SEQUENCE', 'DESCRIPTION', 'PK_DX_ID', 
              'SOURCE_LAST_UPDATE_DATE', 'ENC_DATE', 'PK_ENCOUNTER_ID', 'DX_TYPE', 'HAR_NUMBER','PATIENT_MASTER_CLASS')))
  rar_lab %<>% 
    select(-one_of(c('EMPI', 'PK_ENCOUNTER_ID', 'ENC_DATE')))
  rar_demo %<>% select(-one_of(c('PK_PATIENT_ID')))
  
  empi_before_merging <- union(rar_enc$EMPI, union(rar_dx$EMPI, union(rar_lab$EMPI, rar_demo$EMPI)))
  msg <- sprintf("rar_merge: Before merging, %d unique EMPI's in the union of ENC, Lab, Dx, Demo.", length(unique(empi_before_merging)))
  if(!is.null(logger)) logger$info(msg)
  
  # get the number of Dx's
  n_dx <- sum(grepl("^CODE|^Dx", names(rar_dx)))
  
  
  ## Join
  if(join_to_ENC){
    rar_mg <- rar_enc %>%
      left_join(., rar_dx, by = id) %>%
      left_join(., rar_lab, by = id) %>%
      left_join(., rar_note, by = id) %>%
      left_join(., rar_med, by = id) %>%
      left_join(., rar_demo, by = pt_id)
      
  }else{
    rar_mg <- rar_dx %>% 
      full_join(., rar_enc, by = id) %>% 
      full_join(., rar_lab, by = id) %>%
      full_join(., rar_note, by = id) %>%
      full_join(., rar_med, by = id) %>%
      full_join(., rar_demo, by = pt_id)
     
    
    rar_mg$EMPI <- substr(rar_mg$EMPI_DATE, 1, 10)
  }
  
  
  
  # add logger for join
  if(join_to_ENC){
    logger_join_header <- "rar_merge: All other data sets were joined to ENC data. After joining, "
  }else{
    logger_join_header <- "rar_merge: Full join was performed. After joining, "
  }
  
  dx_icd_cat <- names(rar_mg)[grepl("^CODE", names(rar_mg))]
  dx_h0_cat <- names(rar_mg)[grepl("^Dx_h0", names(rar_mg))]
  dx_h1_cat <- names(rar_mg)[grepl("^Dx_h1", names(rar_mg))]
  dx_h2_cat <- names(rar_mg)[grepl("^Dx_h2", names(rar_mg))]
  n_labs <- sum(grepl("^Test", names(rar_mg))) + sum(names(rar_mg) %in% c("Aldo", "PRA", "DRC"))
  
  msg <- sprintf("%s, %d unique EMPI, %d unique EMPI_Date, %d unique ICD codes, %d Dx H0 Codes (%s), %d unique Dx H1 Codes (%s), %d unique Dx H2 Codes, %d unique tests, in %d total rows.", 
                 logger_join_header, 
                 length(unique(rar_mg$EMPI)), length(unique(rar_mg$EMPI_DATE)),length(unique(dx_icd_cat)), 
                 length(unique(dx_h0_cat)), paste(unique(dx_h0_cat), collapse = ", "),
                 length(unique(dx_h1_cat)), paste(unique(dx_h1_cat), collapse = ", "),
                 length(unique(dx_h2_cat)),
                 n_labs,
                 nrow(rar_mg))
  if(!is.null(logger)) logger$info(msg)
  
  
  
  
  
  # get a vector of indicators for Dx codes in merged data set
  icd_cols <- grepl("^CODE|^Dx", names(rar_mg))

  ## For NA's in Dx, change them into 0
  rar_mg[, names(rar_mg)[icd_cols]] <- lapply(rar_mg[, names(rar_mg)[icd_cols]], 
                                                function(x) { ifelse(is.na(x), 
                                                                     0, 
                                                                     x) })
  
  ## Add age
  ## extract ENC_DATE from EMPI_DATE
  # TODO: pass enc_date explicitly
  rar_mg$ENC_DATE <- as.POSIXct(substr(rar_mg$EMPI_DATE, 12, 21), 
                                format = "%Y-%m-%d")
  rar_mg$Age <- floor(as.numeric(rar_mg$ENC_DATE - rar_mg$BIRTH_DATE)/(3600 * 24 * 365.25))
  
  # Age of -1 => 0
  rar_mg$Age <- ifelse(rar_mg$Age <= 0, 0, rar_mg$Age)
  
  
  # Add Time in System for that encounter
  rar_mg %<>% group_by(EMPI) %>%
    mutate(ENC_Time_in_Sys_days = as.numeric(difftime(ENC_DATE, min(ENC_DATE), units="days"))) %>%
    ungroup()
  
  
  # Apply time cutpoint
  rar_mg %<>% filter(ENC_DATE >= left_censor_date)
  
  
  # Add AVS, UAldo indicator
  rar_mg %<>% mutate(AVS_indicator = (!is.na(`Test_Left_Aldos_ng/dL`) | !is.na(`Test_Right_Aldos_ng/dL`) | !is.na(`Test_IVC_Aldos_ng/dL`)),
                    UAldo_indicator = !is.na(`Test_URINE_ALDOSTERONE_ug/day`))
  
  
  dx_icd_cat <- names(rar_mg)[grepl("^CODE", names(rar_mg))]
  dx_h0_cat <- names(rar_mg)[grepl("^Dx_h0", names(rar_mg))]
  dx_h1_cat <- names(rar_mg)[grepl("^Dx_h1", names(rar_mg))]
  dx_h2_cat <- names(rar_mg)[grepl("^Dx_h2", names(rar_mg))]
  n_labs <- sum(grepl("^Test", names(rar_mg))) + sum(names(rar_mg) %in% c("Aldo", "PRA", "DRC"))
  
  msg <- sprintf("rar_merge: Returned merged data, left censored at %s, %d unique EMPI, %d unique EMPI_Date, %d unique ICD codes, %d Dx H0 Codes (%s), %d unique Dx H1 Codes (%s), %d unique Dx H2 Codes, %d unique tests, in %d total rows.", 
                 left_censor_date,
                 length(unique(rar_mg$EMPI)), length(unique(rar_mg$EMPI_DATE)),length(unique(dx_icd_cat)), 
                 length(unique(dx_h0_cat)), paste(unique(dx_h0_cat), collapse = ", "),
                 length(unique(dx_h1_cat)), paste(unique(dx_h1_cat), collapse = ", "),
                 length(unique(dx_h2_cat)),
                 n_labs,
                 nrow(rar_mg))
  if(!is.null(logger)) logger$info(msg)
  
  
  rar_mg %<>% ungroup()
  
  return(rar_mg)
}




enc_to_pts <- function(rar_enc_level, sbp_lower_limit = 140, dbp_lower_limit = 90, 
                       lab_time_window = 14, logger = NULL){
  #' This function is used to collapse encounter level data into patient level data. It deals with Dx's, Labs, Vitals
  #' @param rar_enc_level tibble Encounter level data set, with Dx's, Labs, Vitals for each encounter (may be missing)
  #' @param sbp_lower_limit numeric Systolic blood pressure criteria for high blood pressure
  #' @param dbp_lower_limit numeric Diastolic blood pressure criteria for high blood pressure
  #' @param lab_time_window numeric A time window for selecting labs
  #' @return pts tibble Patient level data set, with aggregated Dx, Labs, Vitals for each patient
  
  
  enc <- rar_enc_level %>% ungroup()
  
  
  
  msg <- sprintf("enc_to_pts: Before collapsing to pts level, %d EMPI's, in %d total rows.", length(unique(enc$EMPI)), nrow(enc))
  if(!is.null(logger)) logger$info(msg)
  
  # Add demo's
  demo <- enc %>% 
    select(EMPI, GENDER_MASTER_CODE, BIRTH_DATE, RACE_MASTER_CODE, RACE_MASTER_HISPANIC_YN, ZIP) %>%
    group_by(EMPI) %>%
    slice(1) %>%
    ungroup()
  ## calculate zip distances to 19104
  demo <- zip_dist(demo, zip_col = "ZIP", destination="19104")
  
  
  pts <- demo
  
  
  # high_sbp_n, high_dbp_n, High_BP_n, enc_n, encounter with bp
  BP_sum <- enc %>% 
    group_by(EMPI) %>% 
    summarize(enc_n = n(), 
              enc_bp_n = sum(!is.na(BP_SYSTOLIC) & !is.na(BP_DIASTOLIC)),
              high_sbp_n = sum(BP_SYSTOLIC >= sbp_lower_limit, na.rm = TRUE), 
              high_dbp_n = sum(BP_DIASTOLIC >= dbp_lower_limit, na.rm = TRUE),
              high_BP_n = sum(BP_SYSTOLIC >= sbp_lower_limit | BP_DIASTOLIC >= dbp_lower_limit, na.rm = TRUE),
              high_BP_prop = high_BP_n / enc_bp_n) %>%
    ungroup() 
  
  pts %<>% full_join(., BP_sum, by="EMPI")
  
 
  # Add Primary Loc/ENTITY
  pri_ENTITY_sum <- enc %>%
    group_by(EMPI, MASTER_LOCATION_ENTITY) %>%
    summarise(N = n()) %>%
    slice(which.max(N)) %>%
    select(EMPI, pri_entity=MASTER_LOCATION_ENTITY) %>% ungroup()
  pri_loc_sum <- enc %>%
    group_by(EMPI, MASTER_LOCATION_DESCRIPTION) %>%
    summarise(N = n()) %>%
    slice(which.max(N)) %>%
    select(EMPI, pri_loc=MASTER_LOCATION_DESCRIPTION) %>% ungroup()
  
  pts %<>% full_join(., pri_ENTITY_sum, by="EMPI") %>%
    full_join(., pri_loc_sum, by="EMPI")
  
  
  
  # Add RAR Tests, RAR_DATE, RAR_Loc/ENTITY
  ## RAR
  rar_sum <- enc %>%
    select(EMPI, Aldo, PRA, DRC, ENC_DATE, MASTER_LOCATION_ENTITY, MASTER_LOCATION_DESCRIPTION) %>%
    # Prioritize: 1) Aldo 2) PRA > DRC
    filter(!(is.na(Aldo) & is.na(PRA) & is.na(DRC))) %>%
    mutate(rar_priority = case_when(!is.na(Aldo) & !is.na(PRA) ~ 1, 
                                    !is.na(Aldo) & !is.na(DRC) ~ 2,
                                    !is.na(Aldo) ~ 3,
                                    !is.na(PRA) | !is.na(DRC) ~ 4,
                                    TRUE ~ 9)) %>%
    group_by(EMPI) %>% 
    arrange(rar_priority, ENC_DATE) %>% 
    slice(1) %>%
    ungroup() %>%
    select(-rar_priority) %>%
    rename(RAR_DATE = ENC_DATE, rar_entity = MASTER_LOCATION_ENTITY, rar_loc = MASTER_LOCATION_DESCRIPTION)
  
  
  pts %<>% 
    full_join(., rar_sum, by="EMPI")
  
  
  # Add RAR Age
  pts$rar_age <- as.numeric(difftime(pts$RAR_DATE, pts$BIRTH_DATE, units = "days")/365.25)
  pts$rar_age <- ifelse(pts$rar_age < 0, 0, pts$rar_age)
  
  # Add a RAR_DATE time stamp into enc
  enc %<>% 
    full_join(., 
              rar_sum[,c("EMPI", "RAR_DATE")], by="EMPI")
  
  
  
  # Add number of RAR tests
  n_rar_sum <- enc %>% filter(!is.na(Aldo) | !is.na(PRA) | !is.na(DRC)) %>%
    group_by(EMPI) %>%
    summarise(RAR_tests_n = n()) %>%
    ungroup()
  
  pts %<>% full_join(., n_rar_sum, by="EMPI") %>%
    mutate(RAR_tests_n = ifelse(is.na(RAR_tests_n), 0, RAR_tests_n))
  
  
  
  # Add first AVS DATE, AVS Indicator
  # NOTE: AVS Test Results are taken here, which are last ones
  avs <- enc %>% select(EMPI, ENC_DATE, AVS_indicator, `Test_Left_Aldos_ng/dL`, `Test_LCORT_ug/dL`, `Test_Right_Aldos_ng/dL`, `Test_RCORT_ug/dL`, `Test_IVC_Aldos_ng/dL`, `Test_ICORT_ug/dL`) %>%
    filter(AVS_indicator) %>%
    group_by(EMPI) %>%
    arrange(desc(ENC_DATE)) %>%   ## pick the last value
    slice(1) %>%
    ungroup() %>%
    rename(AVS_first_DATE = ENC_DATE)
      
    
  pts %<>% full_join(., avs, by="EMPI")
  
  
  # Add U Aldo DATE, Indicator
  # NOTE: U Aldo Test Results are taken here, which are last ones
  # NOTE: UAldo_indicator is TRUE when there is Urine Aldo
  ualdo <- enc %>% select(EMPI, ENC_DATE, UAldo_indicator, `Test_24HR_URINE_CREATININE_mg/day`, `Test_URINE_ALDOSTERONE_ug/day`) %>%
    filter(UAldo_indicator) %>%
    group_by(EMPI) %>%
    arrange(desc(ENC_DATE)) %>%     ## pick the last value
    slice(1) %>%
    ungroup() %>%
    rename(UAldo_first_DATE = ENC_DATE)
  
  pts %<>% full_join(., ualdo, by="EMPI")
  
  
  
  # Add Time to/from 1st RAR Tests, Time to/from first Hyperaldo dx, Time to/from 1st AVS
  time_to_sum <- enc %>% 
    mutate(mask_RAR_test = !is.na(Aldo) | !is.na(PRA) | !is.na(DRC),
           mask_AVS = AVS_indicator) %>%
    group_by(EMPI) %>%
    summarize(
      time_in_sys_yr = max(ENC_Time_in_Sys_days/365.25),
      first_ENC_DATE = min(ENC_DATE),
      first_ENC_BP_DATE = min(ENC_DATE[which(!is.na(BP_SYSTOLIC) | !is.na(BP_DIASTOLIC))]),
      last_ENC_DATE = max(ENC_DATE),
      last_ENC_BP_DATE = max(ENC_DATE[which(!is.na(BP_SYSTOLIC) | !is.na(BP_DIASTOLIC))]),
      
      
      ### 1st enc to 1st RAR/HA Dx
      time_enc_to_1st_RAR_yr = min(ENC_Time_in_Sys_days[which(mask_RAR_test)])/365.25,
      time_enc_to_1st_hyperaldo_dx_yr = min(ENC_Time_in_Sys_days[which(Dx_h0_Hyperaldo > 0)])/365.25,
      
      
      ### last enc to 1st RAR/HA Dx
      time_enc_after_1st_RAR_yr = 
        (max(ENC_Time_in_Sys_days) - min(ENC_Time_in_Sys_days[which(mask_RAR_test)]))/365.25,
      
      time_enc_after_1st_hyperaldo_dx_yr = 
        (max(ENC_Time_in_Sys_days) - min(ENC_Time_in_Sys_days[which(Dx_h0_Hyperaldo > 0)]))/365.25,
      
      
      ### 1st bp_enc to 1st RAR/HA Dx
      time_bp_to_1st_RAR_yr = 
        min(ENC_Time_in_Sys_days[mask_RAR_test & (!is.na(BP_SYSTOLIC) | !is.na(BP_DIASTOLIC))])/365.25,
      
      time_bp_to_1st_hyperaldo_dx_yr = 
        min(ENC_Time_in_Sys_days[Dx_h0_Hyperaldo > 0 & (!is.na(BP_SYSTOLIC) | !is.na(BP_DIASTOLIC))])/365.25,
      
      ### last bp_enc to 1st RAR/HA Dx
      time_bp_after_1st_RAR_yr =
        (max(ENC_Time_in_Sys_days[!is.na(BP_SYSTOLIC) | !is.na(BP_DIASTOLIC)]) - min(ENC_Time_in_Sys_days[which(mask_RAR_test)]))/365.25,
      
      time_bp_after_1st_hyperaldo_dx_yr =
        (max(ENC_Time_in_Sys_days[!is.na(BP_SYSTOLIC) | !is.na(BP_DIASTOLIC)]) - min(ENC_Time_in_Sys_days[which(Dx_h0_Hyperaldo >0)]))/365.25,
      
      
      
      ## AVS
      ### 1st enc to 1st AVS
      time_enc_to_1st_AVS_yr = min(ENC_Time_in_Sys_days[which(mask_AVS)])/365.25,
      ### last enc to 1st AVS
      time_enc_after_1st_AVS_yr = (max(ENC_Time_in_Sys_days) - min(ENC_Time_in_Sys_days[which(mask_AVS)]))/365.25,
      ### 1st bp_enc to 1st AVS
      time_bp_to_1st_AVS_yr = 
        min(ENC_Time_in_Sys_days[mask_AVS & (!is.na(BP_SYSTOLIC) | !is.na(BP_DIASTOLIC))])/365.25,
      ### last bp_enc to 1st AVS
      time_bp_after_1st_AVS_yr =
        (max(ENC_Time_in_Sys_days[!is.na(BP_SYSTOLIC) | !is.na(BP_DIASTOLIC)]) - min(ENC_Time_in_Sys_days[which(mask_AVS)]))/365.25
      
      
      
    ) %>%
    ungroup() %>%
    mutate_all(funs(replace(., is.infinite(.), NA)))
  
  
  pts %<>% full_join(., time_to_sum, by="EMPI")
  
  
  
  
  
  # Add Regex Counts and NOTE counts
  note_cols <- grepl("^re_|NOTE_n", names(enc))
  note_agg <- enc %>%
    select(EMPI, names(.)[note_cols]) %>%
    group_by(EMPI) %>%
    summarise_all(sum, na.rm=TRUE)
  
  pts %<>% full_join(., note_agg, by='EMPI')
  
  # Add MED Counts
  med_cols <- grepl("^MED_", names(enc))
  med_agg <- enc %>%
    select(EMPI, names(.)[med_cols]) %>%
    group_by(EMPI) %>%
    summarise_all(sum, na.rm=TRUE)
  
  pts %<>% full_join(., med_agg, by='EMPI')
  
  # Add SBP/DBP
  ## select bp closest to RAR_DATE & within day window (default is 14 days)
  ## NOTE: by default of 14 days, 5929 missing; use 30 days, 5317 missing
  ## Only BP prior to RAR_DATE: 2995 missing
  ## BP Prior + 7 days after: 2956 missing
  ## Currently, using 1 year pri to 7 days after RAR date
  bp_sum <- enc %>% 
    select(EMPI, BP_DIASTOLIC, BP_SYSTOLIC, ENC_DATE, RAR_DATE) %>%
    mutate(time_to_RAR = as.numeric(difftime(ENC_DATE, RAR_DATE, units="days")),
           BP_from_past = time_to_RAR <= 0,
           time_to_RAR_abs = abs(time_to_RAR)) %>%
    filter(time_to_RAR > -365 & time_to_RAR <= 7) %>%
    #mutate(priority_time = ifelse(time_to_RAR > 0, time_to_RAR - 10000, time_to_RAR)) %>%
    mutate(priority_BP = case_when(!is.na(BP_SYSTOLIC) & !is.na(BP_DIASTOLIC) ~ 0,
                                   !is.na(BP_DIASTOLIC) | is.na(BP_SYSTOLIC) ~ 1,
                                   TRUE ~ 4)) %>%
    group_by(EMPI) %>%
    arrange(priority_BP,
            desc(BP_from_past),
            time_to_RAR_abs) %>%
    slice(1) %>%
    ungroup() %>%
    select(EMPI, bp_ENC_DATE=ENC_DATE, dbp=BP_DIASTOLIC, sbp=BP_SYSTOLIC)
  
  pts %<>% full_join(., bp_sum, by="EMPI")
  
  msg <- sprintf("enc_to_pts: BP: a time window of -365 to 7 regarding to RAR data was used to select BP's")
  if(!is.null(logger)) logger$info(msg)
  
  # Add Labs
  ## NOTE: currently using a 14-day time window for lab tests
  ## NOTE: explicitly exclude AVS and UAldo Lab tests here
  all_lab_cols <- grepl("^Test", names(enc))
  
  avs_ualdo_cols <- !(names(enc) %in%  c("Test_Left_Aldos_ng/dL", "Test_LCORT_ug/dL", "Test_Right_Aldos_ng/dL", "Test_RCORT_ug/dL", "Test_IVC_Aldos_ng/dL", "Test_ICORT_ug/dL", "Test_24HR_URINE_CREATININE_mg/day", "Test_URINE_ALDOSTERONE_ug/day"))
  
  lab_cols <- all_lab_cols & avs_ualdo_cols
  lab_sum <- enc %>% 
    select(EMPI, ENC_DATE, RAR_DATE, names(enc)[lab_cols]) %>%
    mutate(time_to_RAR = abs(as.numeric(difftime(ENC_DATE, RAR_DATE, units="days")))) %>%
    filter(time_to_RAR <= lab_time_window) %>%
    ungroup() %>%
    mutate(labs_n = rowSums(!is.na(.[names(enc)[lab_cols]]))) %>%
    group_by(EMPI) %>%
    arrange(desc(labs_n),  abs(time_to_RAR)) %>%
    mutate_at(vars(names(enc)[lab_cols]), funs(na.omit(.)[1] )) %>%
    slice(1) %>%
    ungroup() %>%
    select(-c(time_to_RAR, labs_n, ENC_DATE, RAR_DATE))
  
  
  pts %<>% full_join(., lab_sum, by="EMPI")
  
  msg <- sprintf("enc_to_pts: Labs: a time window of %d days prios or post to RAR data was used to select Labs", lab_time_window)
  if(!is.null(logger)) logger$info(msg)
  
  
  # Add Dx's
  ## Count
  icd_col <- grepl("^CODE|^Dx_h", names(enc))
  dx_sum <- enc %>% 
    select(names(enc)[icd_col], EMPI) %>%
    group_by(EMPI) %>%
    summarise_at(vars(names(enc)[icd_col]), 
                 funs(n=sum(., na.rm=TRUE))) %>%
    ungroup()
  
  ## Normalized Count
  dx_h_col <- names(enc)[grepl("^Dx_h", names(enc))]
  dx_sum_norm <- enc %>% 
    select(dx_h_col, EMPI, n_Dx_enc) %>%
    group_by(EMPI) %>%
    summarise_at(vars(dx_h_col), funs(normlized_n=sum(., na.rm=TRUE)/sum(n_Dx_enc, na.rm=TRUE))) %>%
    ungroup()
  
  
  ## total Dx's
  n_Dx <- enc %>% 
    select(n_Dx_enc, EMPI) %>%
    group_by(EMPI) %>%
    summarise(Dx_n = sum(n_Dx_enc, na.rm=TRUE)) %>%
    ungroup()
  
  pts %<>% 
    full_join(., n_Dx, by="EMPI") %>%
    full_join(., dx_sum, by="EMPI") %>%
    full_join(., dx_sum_norm, by="EMPI")
    
  
  
  

  n_labs <- sum(grepl("^Test", names(pts))) + sum(names(pts) %in% c("Aldo", "PRA", "DRC"))
  
  msg <- sprintf("enc_to_pts: Returned Patient Level data: %d unique EMPI, %d unique tests, in %d total rows.", 
                 length(unique(pts$EMPI)),
                 n_labs,
                 nrow(pts))
  if(!is.null(logger)) logger$info(msg)
  
    return(pts)
}

