#!/usr/bin/env Rscript
##############
# This is the script for preparing data set for Biostat's group
# based on RAR data
##############

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(tibble))
suppressPackageStartupMessages(library(ini))
suppressPackageStartupMessages(library(argparse, quietly=TRUE))
suppressPackageStartupMessages(library(tidyr, quietly=TRUE))
suppressPackageStartupMessages(library(readxl, quietly=TRUE))


# read in config file
config <- read.ini("~/repos/Daniel_Herman_Aldosterone_2017_03/code/R/PA_analysis/Biostat_process/config.ini")

# set directory
working_dir <- paste(config$Directories$repos_dir, config$Directories$working_dir_suffix, sep = "/")

output_dir <- paste(config$Directories$repos_dir, config$Directories$output_dir_suffix, sep = "/")
out_root <- config$RAR$out_root

# set working directory
setwd(working_dir)

# load functions
source("common_anal/R_fxns.R")
source("common_anal/RAR_fxns.R")



####################################
##    start main function here    ##
####################################
# main <- function(arguments){
#   parser <- ArgumentParser(description='Read-in RData or raw data')
#   parser$add_argument('project',
#                       choices=c("RAR", "other"),
#                       help="Project group")
#   parser$add_argument('-config', '--config_file',
#                       action="store",
#                       help="Configuration .ini file",
#                       default="config.ini")
#   
#   
#   args <- parser$parse_args(arguments)
#   
#   
#   
# }
# 
# main(commandArgs(trailingOnly = TRUE))

# specify left-censoring date
left_censor_date <- as.Date("1997-01-01")


## HERMANDA_RAR_PTS_ENC.csv

rar_enc <- load_RAR_enc(dat_file = "/data/raw_data/PA/HERMANDA_RAR_PTS_ENC.csv", 
                        bp_only = FALSE, EMPI_DATE_Level = TRUE, outpatient_only = TRUE)

# save(rar_enc, file = "bios_data.RData")



## HERMANDA_RAR_PTS_DX.csv
dx_all <- load_RAR_Dx()
dx_all <- sub_RAR_dx(dat=dx_all, CODE_Level = "Dx_h0", 
                     hierarchy_dx = c("Hyperaldo", "HTN", "Diabetes", "Sleep_Apnea"), 
                     EMPI_DATE_Level = TRUE, left_censor_date = left_censor_date,
                    outpatient_only = TRUE)


# save(rar_enc, rar_dx, file = "bios_data.RData")


## HERMANDA_RAR_PTS_DEMO.csv
rar_demo <- load_RAR_PtDemo()

# save(rar_enc, rar_dx, rar_demo, file = "PA_analysis/Biostat_process/bios_data.RData")


## HERMANDA_RAR_PTS_LABS.csv
lab_raw <- load_Lab(dat_file = "/data/raw_data/PA/HERMANDA_RAR_PTS_LABS.csv", lab_source = "ALL",
                    left_censor_date = left_censor_date,
                    adjust_up = 1.5, adjust_down = 0.5)


lab_PK_ORDER_ID_RIC <- clean_Lab(dat = lab_raw, RAR_only = FALSE, potassium_in = TRUE,
                     num_labs=44)

lab_all <- collapse_lab_EMPI_DATE(lab_PK_ORDER_ID_RIC)


# save(dx_all, lab_all, rar_demo, rar_enc, file = "PA_analysis/Biostat_process/bios_data_v0.2.1.RData")

## HERMANDA_RARV3.csv
# rar <- load_Lab(dat_file = "/data/raw_data/PA/HERMANDA_RARV3.csv", lab_source = "RAR",
#                 potassium = FALSE, adjust_up = 1.5, adjust_down = 0.5)
## TODO: Use this file to extract RAR Labs



# First version RData
# load("PA_analysis/Biostat_process/bios_data_v0.2.1.RData")

# Version 0.2.1 RData
# load("/data/processed_data/RAR/bios_data_v0.2.1.RData")
# Merge all files
rar_mg <- rar_merge(rar_dx = dx_all, rar_enc = rar_enc, rar_lab = lab_all, rar_demo = rar_demo, 
                  id = "EMPI_DATE", pt_id = "EMPI", left_censor_date = left_censor_date,
                  join_to_ENC=TRUE)


# Collapse into Patients' Level
pts <- enc_to_pts(rar_enc_level = rar_mg, lab_time_window = 14)


################    Adding Research Database
# read in Research Database
# read in AVS data
AVS_pts <- read_excel(path = "/data/raw_data/PA/Debbie_DB/AVS- total patient list 2001-2015.xlsx", 
                      sheet = "Sheet1")
AVS_pts <- na.omit(AVS_pts)


# foo <- function(x, year=1999){
#   m <- year(x) %% 100
#   year(x) <- ifelse(m > year %% 100, 1900+m, 2000+m)
#   x
# }
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

rar_mg %<>% select(PA_AVS_tot_0115, everything())
pts %<>% select(PA_AVS_tot_0115, everything())
###########   End of Research Database

save(rar_mg, pts, file = "PA_analysis/Biostat_process/bios_data_enc_pts_v0.2.1.RData")





## De-identify RAR- ENC Level

enc_level <- deidentify(dat = rar_mg, primary_key = "EMPI_DATE", pt_id = "EMPI", mode = "create", 
                        drop_cols = c("SOURCE_CODE", "PATIENT_MASTER_CLASS"), 
                        dt_cols = c("ENC_DATE", "BIRTH_DATE"),
                        out_file_for_mapping = paste(output_dir, 
                                                     paste(out_root, "de_id_mapping_v0.2.1", "csv", sep="."),
                                                     sep="/"), 
                        seed = config$RAR$seed)


enc_level %<>% select(-BIRTH_DATE)

### reorder columns
enc_level %<>% select(DE_PT_ID, DE_primary_key, PA_AVS_tot_0115, ENC_DATE,
                      ENC_Time_in_Sys_days, GENDER_MASTER_CODE, 
                      RACE_MASTER_CODE, RACE_MASTER_HISPANIC_YN,
                      Age, everything())



## De-identify RAR - Patient Level
pt_level <- deidentify(dat = pts, primary_key = NULL, pt_id = "EMPI", mode = "load",
                       drop_cols = c("SOURCE_CODE", "PATIENT_MASTER_CLASS", "EMPI_DATE", "ORDER_START_DATE", "ENC_DATE"), 
                       dt_cols = c("BIRTH_DATE", "first_ENC_DATE", "RAR_DATE","bp_ENC_DATE"),
                       in_file_for_mapping = paste(output_dir, 
                                                   paste(out_root, "de_id_mapping_v0.2.1", "csv", sep="."),
                                                   sep="/"))

pt_level %<>% select(-BIRTH_DATE)


### reorder columns
pt_level %<>% select(DE_PT_ID, PA_AVS_tot_0115, everything())



save(enc_level, pt_level, file = paste(output_dir, paste(out_root, "bios_data_deid_v0.2.1", "RData", sep="."),
                                       sep="/"))


# Write out resulting datasets
write.csv(enc_level, 
          file = paste(output_dir, 
                       paste(out_root, "de_enc_level_v0.2.1", "csv", sep="."),
                       sep="/"),
          row.names = FALSE)
write.csv(pt_level, 
          file = paste(output_dir, 
                       paste(out_root, "de_pt_level_v0.2.1", "csv", sep="."),
                       sep="/"),
          row.names = FALSE)


