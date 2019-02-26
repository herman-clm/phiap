# R functions for general purpose
suppressMessages(suppressWarnings(library(magrittr)))
suppressMessages(suppressWarnings(library(dplyr)))
suppressMessages(suppressWarnings(library(doParallel)))
suppressMessages(suppressWarnings(library(readxl)))
suppressMessages(suppressWarnings(library(geosphere)))
suppressMessages(suppressWarnings(library(rlang)))

## @knitr general_purpose_fxns
cores <- min(20, floor(detectCores() * 0.75))
gg_color_hue <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}
countit <- function(x) {
  tmp <- x %>% summarize(test_count=n(), patient_count=n_distinct(PatNum)) %>%
    ungroup() %>%
    mutate(percent_tests=100 *test_count/sum(test_count)) %>%
    mutate(percent_patients=100 * patient_count/sum(patient_count))
  tmp <- rbind(tmp, colSums(tmp))
  tmp <- data.frame(LABEL=c(rep("", nrow(tmp)-1), "TOTAL"), tmp)
  tmp
}
kablep <- function(x) { x %>% kable %>% print}
decplaces <- function(n) {
  d<-0
  while(round(n, digits=d) != n) {
    d <- d+1
  }
  return(d)
}
round2 <- function(x, n) {
    posneg = sign(x)
    z <- abs(x)*10^n + 0.5
    z = trunc(z) / 10^n
    z*posneg
}

numericize <- function(x, adjust_up=1.05, adjust_down=0.95) {
  #' Convert results reported as greater than (>) or less than (<) to approximate value
  #'
  #' @param x character vector
  #' @param adjust_up numeric adjustment factor for ">XX"
  #' @param adjust_down numeric adjustment factor for "<XX"
  #' @return character vector
  
  x1 <- substr(x, 1, 1)
  mask <- x1 %in% c(">", "<")
  
  tmp_rest <- as.numeric(substring(x[mask], 2))
  tmp <- ifelse(x1[mask] == ">", tmp_rest * adjust_up,
       ifelse(x1[mask] == "<", tmp_rest * adjust_down, 
              tmp_rest))
  x[mask] <- tmp
  x <- suppressWarnings(as.numeric(x))
  return(x)
}
classify_result <- function(res, estimate_percentile=FALSE, log_list=c("TSH")) {
  #' Classify result value based on parallel normal_high and normal_low results
  #' 
  #' @param res data.frame
  #' @return data.frame
  
  # Classify each test based on cutoffs
  res$interp <- with(res, 
                     ifelse(!is.na(value), 
                            ifelse(value > normal_high, 'H',
                                   ifelse(value < normal_low, 'L', 'N')),
                            ifelse(grepl(x=result, pattern="^>"), 'H',
                                   ifelse(grepl(x=result, pattern="^<"), 'L', NA))))
  res$interp <- ordered(res$interp, levels=c("L", "N", "H", NA))
  
  if (estimate_percentile) {
    s1 <- res %>%
      filter(!task_name %in% log_list) %>%
      mutate(m=(normal_high+ normal_low)/2,
             s=(normal_high - m)/1.96,
             ptile=pnorm(mean=m, sd=s, q=value2)) %>%
       dplyr::select(-m, -s)
    s2 <- res %>%
      filter(task_name %in% log_list) %>%
      mutate(m=(log(normal_high) + log(normal_low))/2,
             s=(log(normal_high) - m)/1.96,
             ptile=plnorm(mean=m, sd=s, q=value2)) %>%
      dplyr::select(-m, -s)
    res <- rbind(s1, s2)
  }
  
  return(res)   
}
group_labs <- function(rdat, ID="MRN", DtTm="CollDtTm", hours=24) {
  #' Group laboratories based on MRN, DtTm within X hours into episodes of care
  #' @param rdat data.frame
  #' @param ID string patient id
  #' @param DtTm string Date grouping variable
  #' @param hours int Number of hours to group on

   rdat %<>%
     arrange_(.dots=c(ID, DtTm))
   deltat <- rdat[2:nrow(rdat), DtTm] - rdat[1:(nrow(rdat)-1), DtTm]
   change <- (rdat[2:nrow(rdat), ID] != rdat[1:(nrow(rdat)-1), ID]) | (deltat > hours*60*60)
   rdat$group <- c(0,cumsum(change))

   # Within groups, number tests and calc dt (first test, dt=0)
    rdat %<>%
     arrange_(.groups=c("group", DtTm)) %>%
     group_by(group) %>%
     mutate_(deltat = lazyeval::interp(~ as.numeric(a - first(a))/(60*60), 
                                       a = as.name(DtTm))) %>% 
      mutate(N=row_number()) %>% ungroup()

   rdat
}
fscore <- function(sens, ppv, beta=0.33) {
  x <- as.numeric((1 + beta^2) * (ppv * sens) / ((beta^2 * ppv) + sens))
  return(x)
}



deidentify <- function(dat, 
                       seed,
                       mode,
                       primary_key=NULL, 
                       pt_id="EMPI", 
                       out_file_for_mapping=NULL, 
                       in_file_for_mapping=NULL,
                       dt_cols=c(),
                       drop_cols=c(),
                       logger = NULL) {
  #' This function is used to de-identify Patients' info, like EMPI, PK_PATIENT_ID, and Dates
  #' The function has two modes: 
  #' "create" - de-identify data file and create a de-id mapping file; 
  #' "load" - read in de-id mapping and use it to de-identify data file (use this for patient level, most time)
  #' Note: currently, the "load" function is only designed for de-identifying patient_level data
  #' @param dat tibble Data frame needs de-identified
  #' @param primary_key character Only unique ID for rows
  #' @param pt_id character Patient ID. Default is EMPI
  #' @param dt_cols vector A list of Dates that need to be modified
  #' @param drop_cols vector A list of variables to drop. (should include other IDs)
  #' @param out_file_for_mapping character String for location and file name to store the mapping info; 
  #' @param in_file_for_mapping character String for loacation where the mapping info is stored
  #' @param seed integer A fixed seed is best for reproductive research when generating Deidentified ID's
  
  tmp <- dat %>% ungroup()
  
  # drop specified columns
  tmp <- tmp %<>% select(-one_of(drop_cols))
  
  
  # If "load", then assert in_file_for_mapping exists and load
  # If "create", then assert primary_key is provided
  
  if(mode == "create"){
    # assert primary_key exists
    if(is.null(primary_key)){
      if(!is.null(logger)) logger$error("Main ID not provided in create mode.")
      stop("Main ID not provided in create mode")
    }
    # assert out_file_for_mapping exists
    if(is.null(out_file_for_mapping)){
      if(!is.null(logger)) logger$error("deidentify: Output Mapping File is not provided")
      stop("No output mapping file provided")
    }
    
    
    # get a unique vector for Patient ID
    pt_id_vec <- unique(tmp[[pt_id]])
   
    
    # Generate random DE_PT_ID and shift time for EMPI
    # Note: shift is in seconds
    set.seed(seed)
    pt_id_ls <- tibble(DE_PT_ID = sample(x = length(pt_id_vec), size = length(pt_id_vec), replace = FALSE), 
                      shift = sample(x=-14:14, size = length(pt_id_vec), replace = TRUE)*60*60*24)
    pt_id_ls[[pt_id]] <- pt_id_vec
    tmp <- tmp %<>% full_join(., pt_id_ls, by = pt_id)
    
    # Generate DE_primary_key for primary_key
    
    tmp$DE_primary_key <-  sample(x = nrow(tmp), size = nrow(tmp), replace = FALSE)
    
    # Get a full mapping between pt_id, DE_PT_ID, shift, primary_key, DE_primary_key
    ## get positions first for a fast subsetting
    temp <- match(c(pt_id, "DE_PT_ID","shift", primary_key, "DE_primary_key"), 
                  names(tmp))
    mapping_ls <- tmp %>% select(temp)
    if(!is.null(logger)) logger$info("deidentify: %d rows with %d Primary Keys are de-identified", nrow(tmp), length(unique(mapping_ls[[primary_key]])))
    
    write.csv(mapping_ls, file = out_file_for_mapping, row.names = FALSE)
    if(!is.null(logger)) logger$info("deidentify: mapping file created [%s]", out_file_for_mapping)
    
  }else if(mode == "load"){
    # assert in_file_for_mapping exists
    if(is.null(in_file_for_mapping)){
      logger$error("deidentify: Input Mapping File is not provided")
      stop("No stored mapping file provided")
    }
    
    mapping_ls <- read.csv(in_file_for_mapping, stringsAsFactors = FALSE, header = TRUE)
    mapping_ls <- unique(mapping_ls[c(pt_id, "DE_PT_ID","shift")])
    mapping_ls[[pt_id]] <- as.character(mapping_ls[[pt_id]])
    mapping_ls[["DE_PT_ID"]] <- as.character(mapping_ls[["DE_PT_ID"]])
    
    # assert whether all patient ID that need de-identified exist in mapping file
    if(length(setdiff(unique(tmp[[pt_id]]), mapping_ls[[pt_id]])) != 0){
      if(!is.null(logger)) logger$error("deidentify: Some patient ID does not exist in mapping file. Need generate new mapping")
      stop("Some patient ID not found in mapping file")
    }
    
    tmp %<>% full_join(., mapping_ls, by = pt_id)
    
  }else{
    if(!is.null(logger)) logger$error("deidentify: Unrecoginized Mode. Mode should only be 'create' or 'load'")
    stop("Unrecoginized Mode. Mode should only be 'create' or 'load'")
  }
  
  
 
  # Shift each patient by somewhere between -14 and +14 days
  if (length(dt_cols)) {
    tmp %<>% mutate_at(vars(dt_cols), funs(. + shift))
    if(!is.null(logger)) logger$info("deidentify: Specified Dates were shifted")
  }

  
  
  # remove ID's and shift
  if(!is.null(primary_key)){
    temp <- match(c(pt_id, primary_key, "shift"), names(tmp))
  }else{
    temp <- match(c(pt_id, "shift"), names(tmp))
  }
  
  tmp %<>% select(-temp)
  
  
  
  return(tmp)
}


id_date <- function(dat, hold_id = NA, hold_date = NA){
  #' This is the function used to change ID's into character, and Dates to DATETIME
  #' @param dat data.table very raw data after reading in from csv through fread
  #' @param hold_id character vector Names for ID that will not be changed into character
  #' @param hold_date character vector Names for Date that will not be changed into DATETIME
  #' @return dat tibble Returned data

  col_name <- names(dat)
  
  dat <- as.tibble(dat)
  
  # Pull out the char_col and date_col lists
  # assert that "ID" columns are in one and that "DATE" is in the other
  
  # Change ID's to character
  to_char <- col_name %in% 
    setdiff(c("EMPI", "PK_ENCOUNTER_ID", "HAR_NUMBER", "PK_ORDER_ID","PK_ORDER_RESULT_ID", "PK_DX_ID", "PK_PATIENT_ID", "MRN", "PK_ORDER_PERFORMED_ID"), hold_id)
  
  dat[,to_char] <- lapply(dat[,to_char], as.character)
  
  
  # Change DATE from character into DATETIME
  # Note: This changing to DATETIME only works for format "%Y-%m-%d %H:%M:%S"
  
  to_date <- col_name %in% setdiff(c("ENC_DATE", "E_SOURCE_LAST_UPDATE", "CODING_DATE", "SOURCE_LAST_UPDATE_DATE", "ORDER_START_DATE", "O_SOURCE_LAST_UPDATE", "RESULT_DATE", "BIRTH_DATE", "ORDER_DATE", "PERFORMED_DATE"), hold_date)
  
  dat[,to_date] <- lapply(dat[,to_date], function(x) as.POSIXct(x,format = "%Y-%m-%d %H:%M:%S"))
  
  
  return(dat)
}


fread_epic <- function(dat_file, colClasses, logger = NULL){
  #' This is a generalized version of fread, which could handle date time and do logging when reading raw data in
  #' @param dat_file character File of raw data
  #' @param colClasses A character vector of classes for all columns
  #' it only contain 4 classes: PDS_DateTime, character, numeric, factor
  #' @return dat tibble Loaded raw data
  
  

  # assert that all specified classes could be processed
  unspecified_class <- setdiff(colClasses,c("PDS_DateTime",'character', 'numeric', 'factor'))
  if(length(unspecified_class) != 0){
    err_msg <- sprintf("fread: There are unspecified Column Classes: %s", paste(unspecified_class, collapse = ","))
    if(!is.null(logger)) logger$error(err_msg)
    stop(err_msg)
  }
  
  # get Date-Time variables
  datetime_vars <- names(colClasses)[which(colClasses == "PDS_DateTime")]
  
  # for fread, specifiy Date-Time as factor, then read-in
  colClasses_fread <- ifelse(colClasses == "PDS_DateTime", 'factor', colClasses)
  dat <- fread(dat_file, header = TRUE, na.strings = "",
                   colClasses = colClasses_fread, stringsAsFactors = FALSE)
  
  
  dat <- as.tibble(dat)
  # change Date-Time into POSIXct format
  dat %<>% mutate_at(vars(datetime_vars), .funs=as_datetime)
  
  
  # check whether there is un-specified column
  unspecified_col <- setdiff(names(dat), names(colClasses))
  if(length(unspecified_col) != 0){
    
    err_msg <- sprintf("load function: There are unspecified Columns in raw data: %s", 
                       paste(unspecified_col, collapse = ","))
    if (!is.null(logger)) logger$error(err_msg) 
    stop(err_msg)
  }
  
  if(!is.null(logger)){
    logger$info("fread_epic: Raw data from file [%s] loaded in: Success", basename(dat_file))
  }
  
  return(dat)
  
}


icd_mapping <- function(icd_map_file, dx_hierarchy_level=NULL, dx_hierarchy_level_value=NULL, logger = NULL){
  #' This function is used to load in ICD-9/ICD-10 mappings and returns a mapping in tibble
  #' @param dx_hierarchy_level character Indicator for Dx_h0, Dx_h1, Dx_h2 categories
  #' @param dx_hierarchy_level_value vector A list of Hierarchy Dx's
  #' @return ret tibble The long list of mapping info, consisting of CODE, Dx_h0, Dx_h1, Dx_h2
  #' NOTE: the CODE may not be unique, since some codes are not specific, e.g. ICD9 250.12 could stand for both T2D and unspecified type
  


  dat <- read.csv(file = icd_map_file, 
                  header = TRUE, stringsAsFactors = FALSE, colClasses = "character")
  
  
  icd9 <- subset(dat, select = c(ICD_9, Dx_h0, Dx_h1, Dx_h2))
  icd10 <- subset(dat, select = c(ICD_10, Dx_h0, Dx_h1, Dx_h2))
  names(icd9)[1] <- "CODE"
  names(icd10)[1] <- "CODE"
  
  ret <- as.tibble(bind_rows(icd9, icd10)) %>% 
            filter(!duplicated(.) & !is.na(CODE)) %>%
            mutate_at(vars(Dx_h0, Dx_h1, Dx_h2), funs(gsub(" ","_",.)))
        
  
  
  if(sum(duplicated(ret$CODE)) != 0 & !is.null(logger)){
    logger$info("ICD-mapping: the sets of ICD codes are not mutually exclusive.")
  }
  
  # if category and level are both null, then return a full list - Default
  if(is.null(dx_hierarchy_level) & is.null(dx_hierarchy_level_value)){
    return(ret)
  }
  
  ret <- ret[which(ret[[dx_hierarchy_level]] %in% dx_hierarchy_level_value),]
  
  
  return(ret)
}



true_false_NA <- function(aldo, pra, criteria){
  #' This function is used to test whether a given set of Aldo, PRA tests meet different lab criteria. This function will give a TRUE is any one row of test result meets the criteria. It is designed to use in dplyr, in summarise() function to determine whether a patient HAS EVER met the criteria.
  #' @param aldo vector Aldo tests
  #' @param pra vector PRA tests
  #' @param criteria character Indicates what lab criteria to use; now it supports: RAR_Strict, RAR_PRA, RAR_Aldo, RAR_Lax, Aldo_Strict, Aldo_Lax, PRA_Strict, PRA_Lax, ALL_Strict, ALL_Lax
  
  
  if(criteria == "RAR_Strict" | criteria == "ALL_Strict"){
    aldo_criteria <- 15
    pra_criteria <- 0.5
    ratio_criteria <- 30
  }else if(criteria == "RAR_PRA"){
    aldo_criteria <- 15
    pra_criteria <- 1
    ratio_criteria <- 30
  }else if(criteria == "RAR_Aldo"){
    aldo_criteria <- 10
    pra_criteria <- 0.5
    ratio_criteria <- 30    
  }else if(criteria == "RAR_Lax" | criteria == "ALL_Lax"){
    aldo_criteria <- 10
    pra_criteria <- 1
    ratio_criteria <- 20   
  }else if(criteria == "Aldo_Strict"){
    aldo_criteria <- 15
  }else if(criteria == "Aldo_Lax"){
    aldo_criteria <- 10
  }else if(criteria == "PRA_Strict"){
    pra_criteria <- 0.5
  }else if(criteria == "PRA_Lax"){
    pra_criteria <- 1
  }
  
  
  df <- data.frame(aldo, pra)
  
  
  if(grepl("^RAR", criteria)){
    tmp <- df[!is.na(aldo) & !is.na(pra),]
    if(nrow(tmp) == 0) return(NA)
    
    return(any(tmp$aldo >= aldo_criteria & tmp$pra <= pra_criteria & tmp$aldo/tmp$pra >= ratio_criteria))
    
    
  }else if(grepl("^Aldo", criteria)){
    aldo <- aldo[!is.na(aldo)]
    if(length(aldo) == 0) return(NA)
    
    return(any(aldo >= aldo_criteria))
  }else if(grepl("^PRA", criteria)){
    pra <- pra[!is.na(pra)]
    if(length(pra) == 0) return(NA)
    
    return(any(pra <= pra_criteria))
  }else if(grepl("^ALL", criteria)){
    
    aldo_ls <- aldo >= aldo_criteria
    pra_ls <- pra <= pra_criteria
    ratio_ls <- aldo/pra >= ratio_criteria
    
    ret_ls <- aldo_ls + pra_ls + ratio_ls
    
    if(3 %in% ret_ls){
      return(TRUE)
    }else if(0 %in% ret_ls | 1 %in% ret_ls | 2 %in% ret_ls){
      return(FALSE)
    }else{
      aldo <- aldo[!is.na(aldo)]
      pra <- pra[!is.na(pra)]
      if(length(aldo) == 0 & length(pra) == 0){
        return(NA)
      }
        
      ret <- sum(aldo >= aldo_criteria, pra <= pra_criteria, na.rm = T) != 0

      return(ret)
      
    }
    
    
    
    
  }
  
  
}





zip_dist <- function(dat, zip_col, destination="19104", dist_func = distHaversine){
  #' This function is used to compute distances on a spheroid between zip codes, 
  #' and append this distance to the original data set
  #' NOTE: this only calculate zip codes in 5 digits; any more digits will be truncated to 5 digit zip
  #' NOTE: this function borrows information of ZIP codes to coordinates, 
  #' which mapping file was obtained from https://www.unitedstateszipcodes.org/zip-code-database/
  #' @param dat tibble Original data set with zip codes
  #' @param zip_col character ZIP code column name in original data set
  #' @param destination character Destination ZIP code
  #' @param dist_func function 4 functions which implement 4 different methods to compute distances from 'geosphere' package,
  #' 'Spherical law of cosines'[distCosine], 'Haversine'[distHaversine]
  #' 'Vincenty Sphere'[distVincentySphere], 'Vincenty Ellipsoid'[distVincentyEllipsoid]
  #' @return ret tibble Original data set with distances, in km
  
  # read in ZIP ~ coordinate mapping file
  us_zipcodes <- read.csv(file = "../../ref_data/zip_code_database.csv", header = TRUE, stringsAsFactors = FALSE, colClasses = "character")
  
  # change longitude and latitude into numeric
  us_zipcodes$longitude <- as.numeric(us_zipcodes$longitude)
  us_zipcodes$latitude <- as.numeric(us_zipcodes$latitude)
  

  # get a patient zip code and cood data frame, keeping original order
  # Aldo change name to ZIP
  tmp <- dat %>% select(ZIP = UQ(zip_col)) %>% 
    mutate(ZIP = substr(ZIP, 1, 5)) %>%
    left_join(., us_zipcodes, by=c(ZIP = "zip"))
  
  
  destination_cood <- us_zipcodes[which(us_zipcodes$zip == destination), c("longitude", "latitude")]
  
  
  distances <- dist_func(p1=tmp[, c("longitude", "latitude")],
                           p2=destination_cood)/1000
  
  dat$ZIP_distance_km <- distances
  
  return(dat)
  
  
  
  
  
}



