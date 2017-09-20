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



## HERMANDA_RAR_PTS_ENC.csv

rar_enc <- load_RAR_enc(dat_file = "/data/raw_data/PA/HERMANDA_RAR_PTS_ENC.csv", 
                        bp_only = FALSE, EMPI_DATE_Level = TRUE, outpatient_only = TRUE)

# save(rar_enc, file = "bios_data.RData")



## HERMANDA_RAR_PTS_DX.csv
rar_dx <- load_RAR_Dx()
rar_dx <- sub_RAR_dx(dat=rar_dx, ALDO.Dx = TRUE, HTN.Dx = TRUE,
                     n.Dx = 0, EMPI_DATE_Level = TRUE, outpatient_only = TRUE)

# save(rar_enc, rar_dx, file = "bios_data.RData")


## HERMANDA_RAR_PTS_DEMO.csv
rar_demo <- load_RAR_PtDemo()

# save(rar_enc, rar_dx, rar_demo, file = "PA_analysis/Biostat_process/bios_data.RData")


## HERMANDA_RAR_PTS_LABS.csv
lab_raw <- load_Lab(dat_file = "/data/raw_data/PA/HERMANDA_RAR_PTS_LABS.csv", lab_source = "ALL",
                    adjust_up = 1.5, adjust_down = 0.5)


lab_PK_ORDER_ID_RIC <- clean_Lab(dat = lab_raw, RAR_only = FALSE, potassium_in = TRUE,
                     num_labs=44)

lab_all <- collapse_lab_EMPI_DATE(lab_PK_ORDER_ID_RIC)


# save(rar_enc, rar_dx, rar_demo, rar_lab, file = "PA_analysis/Biostat_process/bios_data.RData")


## HERMANDA_RARV3.csv
# rar <- load_Lab(dat_file = "/data/raw_data/PA/HERMANDA_RARV3.csv", lab_source = "RAR",
#                 potassium = FALSE, adjust_up = 1.5, adjust_down = 0.5)
## TODO: Use this file to extract RAR Labs





load("PA_analysis/Biostat_process/bios_data.RData")
# Merge all files
rar_mg <- rar_merge(rar_dx = rar_dx, rar_enc = rar_enc, rar_lab = rar_lab, rar_demo = rar_demo, 
                  id = "EMPI_DATE", pt_id = "EMPI")


# Collapse into Patients' Level
pts <- enc_to_pts(rar_enc_level = rar_mg$rar_mg, num_dx = rar_mg$num_dx)


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
###########   End of Research Database


## De-identify RAR- ENC Level

enc_level <- deidentify(dat = rar_mg$rar_mg, primary_key = "EMPI_DATE", pt_id = "EMPI", mode = "create", 
                        drop_cols = c("SOURCE_CODE", "PATIENT_MASTER_CLASS"), 
                        dt_cols = c("ENC_DATE", "BIRTH_DATE", "ORDER_START_DATE"),
                        out_file_for_mapping = paste(output_dir, 
                                                     paste(out_root, "de_id_enc_lv", "csv", sep="."),
                                                     sep="/"), 
                        seed = config$RAR$seed)


enc_level %<>% select(-BIRTH_DATE)

### reorder columns
enc_level %<>% select(DE_PT_ID, DE_EMPI_DATE_ID, PA_AVS_tot_0115, ENC_DATE, ORDER_START_DATE, everything())



## De-identify RAR - Patient Level
pt_level <- deidentify(dat = pts, primary_key = NULL, pt_id = "EMPI", mode = "load",
                       drop_cols = c("SOURCE_CODE", "PATIENT_MASTER_CLASS", "EMPI_DATE", "ORDER_START_DATE", "ENC_DATE"), 
                       dt_cols = c("BIRTH_DATE"),
                       in_file_for_mapping = paste(output_dir, 
                                                   paste(out_root, "de_id_enc_lv", "csv", sep="."),
                                                   sep="/"))

pt_level %<>% select(-BIRTH_DATE)


### reorder columns
pt_level %<>% select(DE_PT_ID, PA_AVS_tot_0115, everything())



save(enc_level, pt_level, file = paste(output_dir, paste(out_root, "bios_data_deid", "RData", sep="."),
                                       sep="/"))

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

