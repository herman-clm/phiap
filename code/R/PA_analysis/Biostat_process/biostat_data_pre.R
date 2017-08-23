#!/usr/bin/env R
##############
# This is the script for preparing data set for Biostat's group
# based on RAR data
##############

library(dplyr)
library(data.table)
library(tibble)
source("../../common_anal/R_fxns.R")

w_dir <- "~/repos/Daniel_Herman_Aldosterone_2017_03/code/R/PA_analysis/Biostat_process/"
output_dir <- "~/repos/Daniel_Herman_Aldosterone_2017_03/output"
out_root <- "RAR.for_biostats"


## HERMANDA_RAR_PTS_ENC.csv
rar_enc <- load_RAR_enc(dat_file = "/data/raw_data/PA/HERMANDA_RAR_PTS_ENC.csv", 
                        bp_only = TRUE, EMPI_DATE_Level = TRUE, outpatient_only = FALSE)

# save(rar_enc, file = "bios_data.RData")



## HERMANDA_RAR_PTS_DX.csv
rar_dx <- load_RAR_Dx()
rar_dx <- sub_RAR_dx(dat=rar_dx, ALDO.Dx = TRUE, HTN.Dx = TRUE,
                     n.Dx = 10, EMPI_DATE_Level = TRUE, outpatient_only = TRUE)

# save(rar_enc, rar_dx, file = "bios_data.RData")


## HERMANDA_RAR_PTS_DEMO.csv
rar_demo <- load_RAR_PtDemo()

# save(rar_enc, rar_dx, rar_demo, file = "bios_data.RData")


## HERMANDA_RAR_PTS_LABS.csv
rar_lab <- load_Lab(dat_file = "/data/raw_data/PA/HERMANDA_RAR_PTS_LABS.csv", 
                    potassium = FALSE, adjust_up = 1.5, adjust_down = 0.5)
rar_lab <- clean_RAR_Lab(dat = rar_lab, RAR_only = TRUE, EMPI_DATE_Level = TRUE)

# save(rar_enc, rar_dx, rar_demo, rar_lab, file = "bios_data.RData")

## HERMANDA_RARV3.csv
rar <- load_Lab(dat_file = "/data/raw_data/PA/HERMANDA_RARV3.csv", 
                potassium = FALSE, adjust_up = 1.5, adjust_down = 0.5)
## TODO: Use this file to extract RAR Labs





load("bios_data.RData")
# Merge all files
# TODO: change Merge Part into a function
## drop some columns
rar_enc %<>% 
  select(-c(E_SOURCE_LAST_UPDATE, PK_ENCOUNTER_ID, ENC_DATE))
rar_dx %<>% 
  select(-c(EMPI, CODE_STANDARD_NAME, COMMENTS, CODING_DATE, PRIMARY_YN, DX_SEQUENCE,DESCRIPTION, PK_DX_ID, 
            SOURCE_LAST_UPDATE_DATE, ENC_DATE, PK_ENCOUNTER_ID, DX_TYPE, HAR_NUMBER))
rar_lab %<>% 
  select(-c(EMPI, PK_ENCOUNTER_ID, ENC_DATE))
rar_demo %<>% select(-PK_PATIENT_ID)

# spread rar_dx
rar_dx %<>% 
  mutate(value = 1) %>%
  spread(CODE, value, fill = 0)
## QC: check whether rar_dx has unique rows regarding to EMPI_DATE
## length(unique(rar_dx$EMPI)) == dim(rar_dx)[1]

id <- "EMPI_DATE"
## Full Join: keep all info
rar_mg <- rar_dx %>% 
  full_join(., rar_enc, by = "EMPI_DATE") %>% 
  full_join(., rar_lab, by = "EMPI_DATE") %>% 
  full_join(., rar_demo, by="EMPI")

dx_code_start <- which(names(rar_mg) == id)[1] + 1
dx_code_end <- ncol(rar_dx)

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



# Collapse into Patients' Level
pts <- rar_mg

# ADD assert statment to confirm that there are no SBPs without DBPs and vice-a-versa
# rar_mg %>%
#   group_by(EMPI) %>%
#   summarize(n_discordant_BP = sum(is.na(BP_SY)))
# ....... is.na(BP_SYSTOLIC) & !is.na(BP_DIASTOLIC)

## SBP_n, DBP_n, High_BP_n
pts %<>% 
  group_by(EMPI) %>% 
  mutate(SBP_n = sum(BP_SYSTOLIC >= 140, na.rm = TRUE), 
         DBP_n = sum(BP_DIASTOLIC >= 90, na.rm = TRUE),
      High_BP_n = sum(BP_SYSTOLIC >= 140 | BP_DIASTOLIC >= 90, na.rm = TRUE),
      High_BP_prop = High_BP_n / sum(!is.na(BP_SYSTOLIC) & !is.na(BP_DIASTOLIC))) %>% #Notice: I changed this logit into devided by sum of either is not NA, not sum of both is not NA
  ungroup() 


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


################    Adding Research Database
# read in Research Database
# read in AVS data
AVS_pts <- read_excel(path = "/data/raw_data/PA/Debbie_DB/AVS- total patient list 2001-2015.xlsx", 
                      sheet = "Sheet1")
AVS_pts <- na.omit(AVS_pts)

foo <- function(x, year=1999){
  m <- year(x) %% 100
  year(x) <- ifelse(m > year %% 100, 1900+m, 2000+m)
  x
}


# AVS_pts$enctr_date <- as.POSIXct(AVS_pts$enctr_date, format="%d-%b-%y")
# AVS_pts$date_birth <- foo(as.POSIXct(AVS_pts$date_birth, format="%d-%b-%y"), year = 2000)


# read in EMPI ~ MRN for RAR patients: all columns as characters when reading in
empi_MRN <- fread(input = "/data/raw_data/PA/HERMANDA_RAR_EMPI_MRN.csv", 
                  header=TRUE, colClasses = "character")
empi_MRN <- as.tibble(empi_MRN)


empi <- empi_MRN %>% select(c(MRN, EMPI)) %>%
  group_by(MRN, EMPI) %>%
  slice(1) %>%
  ungroup()

research_db <- AVS_pts %>% left_join(., empi, by = c("hosp_num" = "MRN"))
PA_empi <- na.omit(unique(research_db$EMPI))


rar_mg$PA_AVS_tot_0115 <- ifelse(rar_mg$EMPI %in% PA_empi, TRUE, FALSE)
pts$PA_AVS_tot_0115 <- ifelse(pts$EMPI %in% PA_empi, TRUE, FALSE)
###########   End of Research Database


## De-identify RAR- ENC Level
enc_level <- deidentify(dat = rar_mg, main_id = "EMPI_DATE", pt_id = "EMPI",
                        drop_cols = c("SOURCE_CODE", "PATIENT_MASTER_CLASS"), 
                        dt_cols = c("ENC_DATE", "BIRTH_DATE", "ORDER_START_DATE"),
                        out_file_for_mapping = paste(output_dir, 
                                                     paste(out_root, "de_id", "csv", sep="."),
                                                           sep="/"))

enc_level %<>% select(-BIRTH_DATE)

### reorder columns
enc_level %<>% select(DE_PT_ID, DE_EMPI_DATE_ID, PA_AVS_tot_0115, ENC_DATE, ORDER_START_DATE, everything())


## De-identify RAR - Patient Level
pt_level <- deidentify(dat = pts, main_id = NULL, pt_id = "EMPI",
                       drop_cols = c("SOURCE_CODE", "PATIENT_MASTER_CLASS", "EMPI_DATE", "ORDER_START_DATE", "ENC_DATE"), 
                       dt_cols = c("BIRTH_DATE"),
                       out_file_for_mapping = paste(output_dir, 
                                                    paste(out_root, "de_id", "csv", sep="."),
                                                    sep="/"))
pt_level %<>% select(-BIRTH_DATE)

### reorder columns
pt_level %<>% select(DE_PT_ID, PA_AVS_tot_0115, everything())


save(enc_level, pt_level, file = "bios_data_deid.RData")

# Write out resulting datasets
write.csv(enc_level, 
          file = paste(output_dir, 
                       paste(out_root, "enc_level", 
                             Sys.Date(), "csv", sep="."),
                       sep="/"),
          row.names = FALSE)
write.csv(pt_level, 
          file = paste(output_dir, 
                       paste(out_root, "pt_level", 
                             Sys.Date(), "csv", sep="."),
                       sep="/"),
          row.names = FALSE)

