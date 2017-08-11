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
deidentify <- function(dat, id_cols=c(), dt_cols=c(), drop_cols=c()) {
  tmp <- dat
  
  #drop columns
  tmp <- tmp %>% dplyr::select(-one_of(drop_cols))
  
  # deidentify columns
  for (col in id_cols) {
    tmp[[col]] <- as.numeric(as.factor(tmp[[col]]))
  }

    # Shift each id by somewhere between -14 and +14 days
  if (length(dt_cols)) {
    tmp %<>%
      group_by_(.dots=id_cols[1]) %>%
      mutate(shift = runif(min=-60*60*24*14, max=60*60*24*14, n=1)) %>%
      mutate_each_(funs(. + shift), dt_cols) %>%
      dplyr::select(-shift)
  }
  tmp
}
