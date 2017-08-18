library(assertthat)
source("../common_anal/R_fxns.R")
suppressMessages(library(data.table))

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




load_RAR_enc <- function(dat_file = "/data/raw_data/PA/HERMANDA_RAR_PTS_ENC.csv", bp_only = FALSE, EMPI_DATE_Level = TRUE, outpatient_only = TRUE){
  #' Load in the RAR Encounter data, and do some basic cleaning
  #' @param dat_file string Raw data text file location 
  #' @param bp_only logic If TRUE, only keep BP's, excluding all other detailed info. Default is FALSE
  #' @param EMPI_DATE_Level logic If TRUE, collapse into EMPI_DATE Level. Default is TRUE
  #' @param outpatient_only logic If TRUE, only include Outpatients. Default is TRUE
  #' @return rar_enc tibble pre-processed RAR Dx data
  
  # read in raw data file
  rar_enc <- fread(dat_file, header = TRUE, stringsAsFactors = FALSE)
  
  
  if(outpatient_only){
    rar_enc %<>% filter(PATIENT_MASTER_CLASS == "OUTPATIENT")
  }
  
  
  if(bp_only){
    # only keep BP's
    rar_enc %<>% select(EMPI, PK_ENCOUNTER_ID, ENC_DATE, E_SOURCE_LAST_UPDATE, 
             BP_SYSTOLIC, BP_DIASTOLIC)
  }
  
  # Modify ID's and DateTime
  rar_enc <- id_date(rar_enc)
  
  # Create new HAR: HAR_NUMBER or EMPI + ENC Date
  # rar_enc$HAR <- ifelse(is.na(rar_enc$HAR_NUMBER), 
  #                       paste(rar_enc$EMPI, format(rar_enc$ENC_DATE, "%Y-%m-%d")),
  #                       rar_enc$HAR_NUMBER)
  
  # Create new ENC ID: EMPI_DATE
  rar_enc$EMPI_DATE <- paste(rar_enc$EMPI, format(rar_enc$ENC_DATE, "%Y-%m-%d"))
  
  
  if(EMPI_DATE_Level){
    # collapse into HAR_NUMBER Level
    ##  For duplicated EMPI_DATE, use the Median E_SOURCE_LAST_UPDATE
    rar_enc %<>% group_by(EMPI_DATE) %>%
      arrange(desc(E_SOURCE_LAST_UPDATE)) %>%
      mutate_at(vars(BP_SYSTOLIC:BP_DIASTOLIC), funs(median(., na.rm = FALSE))) %>%
      slice(1) %>%
      ungroup()
    
  }
  
  # TODO: If HAR_Level = FALSE, need to collapse into PK_ENCOUNTER_ID level
  
  return(rar_enc)
  
  
}


load_RAR_Dx <- function(dat_file = "/data/raw_data/PA/HERMANDA_RAR_PTS_DX.csv"){
  #' Load in the RAR Dx data, and do some basic cleaning
  #' @param dat_file string Raw data text file location 
  #' @return rar_dx tibble pre-processed RAR Dx data
  
  # read in file
  rar_dx <- fread(file = dat_file, header = TRUE, stringsAsFactors = FALSE)
  
  # remove those without ICD indicator (Since they also do not have CODE)
  rar_dx <- rar_dx[CODE_STANDARD_NAME != ""]
  
  
  # Modify ID and DateTime
  rar_dx <- id_date(rar_dx)
  
  # change PRIMARY_YN into factor
  rar_dx$PRIMARY_YN <- ifelse(rar_dx$PRIMARY_YN == 1, TRUE, FALSE)
  
  

  rar_dx <- as.tibble(rar_dx)
  return(rar_dx)
  
}

sub_RAR_dx <- function(dat, ALDO.Dx = TRUE, HTN.Dx = TRUE, n.Dx = 10, EMPI_DATE_Level = TRUE, outpatient_only = TRUE){
  #' Getting the proper subset for Dx data
  #' @param dat tibble Pre-processed RAR Dx data
  #' @param ALDO.Dx logit Whether to include ALDO Dx's. Default is TRUE
  #' @param HTN.Dx logit Whether to include hypertension Dx's. Default is TRUE
  #' @param n.Dx numeric Number of total Dx's included, ALDO/HTN excluded. Default is 10
  #' @param EMPI_DATE_Level logit If TRUE, collapse data into EMPI_DATE Level. Default is TRUE
  #' @param outpatient_only logit If TRUE, only include Outpatients. Default is TRUE.
  #' @return ret tibble The subset of cleaned Dx data
  
  ret <- tibble()
  
  if(outpatient_only){
    dat %<>% filter(PATIENT_MASTER_CLASS == "OUTPATIENT")
  }
  
  
  # get ALDO Dx's, in a higher level
  if(ALDO.Dx){
    # catch Hyperaldo in a higher level
    ret <- dat %>% filter(CODE_STANDARD_NAME == "ICD-10" & grepl("E26\\.*", CODE))
   
    ret <- dat %>% filter(CODE_STANDARD_NAME == "ICD9" & grepl("255\\.1", CODE)) %>% rbind(.,ret)
    
    aldo_dx <- unique(ret$CODE)
    nd <- length(unique(ret$CODE))
  }
  
  # get HTN Dx's, in a higher level
  if(HTN.Dx){
    ret <- dat %>% filter(CODE_STANDARD_NAME == "ICD-10" & grepl("I10|I15", CODE))
    
    ret <- dat %>% filter(CODE_STANDARD_NAME == "ICD9" & grepl("401|405", CODE)) %>% rbind(.,ret)
    
  }
  
  if(n.Dx != 0){
    dx_left <- dat %>% filter(!(CODE %in% aldo_dx)) %>% 
      group_by(CODE) %>%
      summarise(N=n()) %>%
      arrange(desc(N)) %>%
      slice(., 1:n.Dx)
    
    ret <- dat %>% filter(CODE %in% dx_left$CODE) %>% rbind(., ret)
  }
  
  
  # Create new HAR: HAR_NUMBER or EMPI + ENC Date
  # ret$HAR <- ifelse(is.na(ret$HAR_NUMBER), 
  #                       paste(ret$EMPI, format(ret$ENC_DATE, "%Y-%m-%d")),
  #                       ret$HAR_NUMBER)
  
  # Create new ENC ID: EMPI_DATE
  ret$EMPI_DATE <- paste(ret$EMPI, format(ret$ENC_DATE, "%Y-%m-%d"))
  
  
  
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
    
    

  
    
  }
  
  # TODO: If HAR_Level = FALSE, need to collapse into PK_ENCOUNTER_ID level
  return(ret)
  
}



load_RAR_PtDemo <- function(dat_file = "/data/raw_data/PA/HERMANDA_RAR_PTS_DEMO.csv"){
  #' Load in the RAR Patients' Demo data, and do some basic cleaning
  #' @param dat_file string Raw data text file location 
  #' @return rar_demo tibble pre-processed RAR Dx data
  
  # read in raw data
  rar_demo <- fread(dat_file, header = TRUE, stringsAsFactors = FALSE)
  
  # Modify ID and DateTime
  rar_demo <- id_date(rar_demo)
  
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



load_Lab <- function(dat_file,  potassium = FALSE, adjust_up = 1.5, adjust_down = 0.5){
  #' Load in the Lab data, and do some basic cleaning. Lab data includes RAR_Lab and RAR_V3
  #' @param dat_file string Raw data text file location 
  #' @param adjust_up numeric Rate for adjusting "> X", default is 0.5
  #' @param adjust_donw numeric Rate for adjusting "< X", default is 1.5
  #' @param potassium logit If TRUE, potassium lab results will be included. Default is FALSE
  #' @return rar_lab tibble pre-processed RAR Dx data
  
  # TODO: Not Finished.
  
  # read in file
  rar_lab <- fread(file = dat_file, header = TRUE, stringsAsFactors = FALSE)
  
  
  
  
  # Modify ID and DateTime
  rar_lab <- id_date(rar_lab)
  
  # modify RESULT_VALUE, using numericize function
  rar_lab$RESULT_VALUE <- numericize(rar_lab$RESULT_VALUE, adjust_up = adjust_up, adjust_down = adjust_down)
  

  
  return(rar_lab)
  
}




clean_RAR_Lab <- function(dat, Result_Status = "Final", RAR_only = TRUE, EMPI_DATE_Level = TRUE){
  #' Load in the RAR Lab data, and do some basic cleaning
  #' @param dat tibble Pre-cleaned RAR data 
  #' @param Result_Status string If "Final", then only include RESULT_STATUS = "Final"
  #' Otherwise, include all labs.
  #' @param RAR_only logit If TRUE, will only return Aldo/Renin results
  #' @param EMPI_DATE_Level logit If TRUE, collapse data into EMPI_DATE Level. Default is TRUE
  #' @return ret tibble pre-processed RAR Dx data
  
  
  # Whether to select only "Final"
  if(Result_Status == "Final"){
    dat <- dat %>% filter(RESULT_STATUS == "Final")
  }
  # TODO: If not "Final", or Result_Status is a vector, should modify
  
  
  # Duplicates
  if(sum(duplicated(dat))){
    warning("TODO: Duplicated rows in pre_rar input: need to explain")
  }
  
  dat %<>%
    filter(!duplicated(dat))
  
  
  if(RAR_only){
    # catch "ALDO"|"RENIN"
    dat %<>% filter(grepl("renin|aldo", tolower(RESULT_ITEM_CODE))) %>%
      group_by(ORDER_ITEM_CODE, RESULT_ITEM_CODE)
    
    # Exclude ORDER_ITEM_CODE that are for AVS or urine specimens
    dat %<>% filter(!(ORDER_ITEM_CODE %in%  c("C9009900", "C9009995", "C9009997", "Q19573", "83497A")))
    
    # Create ret table
    ret <- as.tibble()
    ## dropped many rows here
    ret <- dat %>% 
      filter(RESULT_ITEM_CODE %in% c("RENIN ACTIVITY","RENIN", "PLASMA RENIN ACTIVITY, LC/MS/MS", "PLASMA RENIN ACTIVITY")) %>%
      mutate(Test = "PRA")
    
    
    ret <- dat %>% 
      filter(RESULT_ITEM_CODE %in% c("DIRECT RENIN")) %>% 
      mutate(Test = "DRC") %>%
      rbind(., ret)
    
    ret <- dat %>% 
      filter(RESULT_ITEM_CODE %in% c("ALDOSTERONE, SERUM", "ALDOSTERONE, LC/MS/MS", "ALDOSTERONE")) %>%
      mutate(Test = "Aldo") %>%
      rbind(., ret)
    
    # # Create HAR
    # ret$HAR <- ifelse(is.na(ret$HAR_NUMBER), 
    #                   paste(ret$EMPI, format(ret$ENC_DATE, "%Y-%m-%d")),
    #                   ret$HAR_NUMBER)
    
    # Create new ENC ID: EMPI_DATE
    ret$EMPI_DATE <- paste(ret$EMPI, format(ret$ENC_DATE, "%Y-%m-%d"))
    
    
    if(EMPI_DATE_Level){
      ret <- collapse_RAR_Lab(dat=ret, EMPI_DATE_Level = EMPI_DATE_Level)
    }
    
  }
  
  
  
  
}




collapse_RAR_Lab <- function(dat, EMPI_DATE_Level = TRUE){
  #' This function is used to collapse RAR Lab data for ALDO/RENIN
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
  
  
}




clean_RAR <- function(dat, blood_only=TRUE){
  #' This function is used to clean up RAR lab results
  #' @param dat tibble Pre-cleaned RAR lab results
  #' @param blood_only Boolean If true, only use serum/plasma test codes
  
  if (blood_only) { # Exclude ORDER_ITEM_CODE that are for AVS or urine specimens
    dat <- dat[!(ORDER_ITEM_CODE %in% c("C9009900", "C9009995", "C9009997", "Q19573", "83497A"))]
  }
  
}

