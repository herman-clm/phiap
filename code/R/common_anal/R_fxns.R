# R functions for general purpose
suppressMessages(suppressWarnings(library(magrittr)))
suppressMessages(suppressWarnings(library(dplyr)))
suppressMessages(suppressWarnings(library(doParallel)))

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



deidentify <- function(dat, main_id, pt_id="EMPI", dt_cols=c(), drop_cols=c(), out_file_for_mapping) {
  #' This function is used to de-identify Patients' info, like EMPI, PK_PATIENT_ID, and Dates
  #' @param dat tibble Data frame needs de-identified
  #' @param main_id character Only unique ID column.
  #' @param id character EMPI
  #' @param dt_cols vector A list of Dates that need to be modified
  #' @param drop_cols vector A list of variables to drop. (should include other IDs)
  #' @param out_file_for_mapping character String for location and file name to store the mapping info; 
  #' also, it is the file to read in mapping info
  #' Note that the ID columns should be unique themselves... i.e., each row of data could identified by its unique ID

  
  tmp <- dat %>% ungroup()
  
  # drop specified columns
  tmp <- tmp %<>% select(-one_of(drop_cols))
  
  if(!is.null(main_id)){
    # list of EMPI
    EMPI <- unique(tmp[[pt_id]])
    
    # Generate random DE_PT_ID and shift time for EMPI
    EMPI_ls <- tibble(EMPI = EMPI, 
                      DE_PT_ID = sample(x = length(EMPI), size = length(EMPI), replace = FALSE), 
                      shift = sample(x=-14:14, size = length(EMPI), replace = TRUE)*60*60*24)
    
    tmp <- tmp %<>% full_join(., EMPI_ls, by = "EMPI")
    
    # Generate DE_EMPI_DATE_ID for main_id
    # DE_EMPI_DATE_ID <- paste("DE_", main_id, "_ID", sep = "")
    tmp$DE_EMPI_DATE_ID <-  sample(x = nrow(tmp), size = nrow(tmp), replace = FALSE)
    
    # re-substract EMPI_ID_list and write out
    ## get positions first for a fast subsetting
    temp <- match(c(pt_id, "DE_PT_ID","shift", main_id, "DE_EMPI_DATE_ID"), names(tmp))
    EMPI_ID_ls <- tmp %>% select(temp)
    
    write.csv(EMPI_ID_ls, file = out_file_for_mapping, row.names = FALSE)
    
  }else{
    
    EMPI_ID_ls <- read.csv(out_file_for_mapping, stringsAsFactors = FALSE, header = TRUE)
    EMPI_ID_ls <- unique(EMPI_ID_ls[c(pt_id, "DE_PT_ID","shift")])
    EMPI_ID_ls$EMPI <- as.character(EMPI_ID_ls$EMPI)
    EMPI_ID_ls$DE_PT_ID <- as.character(EMPI_ID_ls$DE_PT_ID)
    
    tmp %<>% full_join(., EMPI_ID_ls, by = "EMPI")
  }
  
 
  # Shift each patient by somewhere between -14 and +14 days
  if (length(dt_cols)) {

    tmp %<>% mutate_at(vars(dt_cols), funs(. + shift))
  }
  
  
  
  # remove ID's and shift
  if(!is.null(main_id)){
    temp <- match(c(pt_id, main_id, "shift"), names(tmp))
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
  
  # Change ID's to character
  to_char <- col_name %in% setdiff(c("EMPI", "PK_ENCOUNTER_ID", "HAR_NUMBER", "PK_ORDER_ID","PK_ORDER_RESULT_ID", "PK_DX_ID", "PK_PATIENT_ID", "MRN", "PK_ORDER_PERFORMED_ID"), hold_id)
  
  dat[,to_char] <- lapply(dat[,to_char], as.character)
  
  
  # Change DATE from character into DATETIME
  # Note: This changing to DATETIME only works for format "%Y-%m-%d %H:%M:%S"
  
  to_date <- col_name %in% setdiff(c("ENC_DATE", "E_SOURCE_LAST_UPDATE", "CODING_DATE", "SOURCE_LAST_UPDATE_DATE", "ORDER_START_DATE", "O_SOURCE_LAST_UPDATE", "RESULT_DATE", "BIRTH_DATE", "ORDER_DATE", "PERFORMED_DATE"), hold_date)
  
  dat[,to_date] <- lapply(dat[,to_date], function(x) as.POSIXct(x,format = "%Y-%m-%d %H:%M:%S"))
  
  
  return(dat)
}
