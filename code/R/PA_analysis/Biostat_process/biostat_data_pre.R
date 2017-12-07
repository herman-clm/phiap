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
suppressPackageStartupMessages(library(logging, quietly = TRUE))


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
source("common_anal/RAR_fxns.R", chdir = TRUE)

#########################
##       Parameters    ##
#########################

# Manually Specification for config
dat_version <- "v0.2.3"
## Dx Codes
CODE_Level <- "Dx_h0"
hierarchy_dx <- c("Hyperaldo", "HTN", "Diabetes", "Sleep_Apnea")


# read in config file
## specify left-censoring date
left_censor_date <- as.Date(config$RAR$left_censor_date_chr)

## specify files

enc_file <- config[[dat_version]]$enc_file
dx_file <- config[[dat_version]]$dx_file
demo_file <- config[[dat_version]]$demo_file
lab_file_PDS_epic <- config[[dat_version]]$lab_file_PDS_epic
lab_file_PDS_cerner <- config[[dat_version]]$lab_file_PDS_cerner
empi_mrn_file <- config[[dat_version]]$empi_mrn_file

service_grouping_file = config[[dat_version]]$service_grouping_file
icd_map_file <- config[[dat_version]]$icd_map_file

seed_config <- config[[dat_version]]$seed



#########################
##    Set up Log       ##
#########################
log_file <- paste(normalizePath(output_dir), 
                  paste(out_root, dat_version,"log.txt", sep='.'),
                  sep="/")
addHandler(writeToFile, file=log_file,
           level='INFO', logger='bios_data_cleaning')
logger <- getLogger('bios_data_cleaning')

logger$info("Biostat Data Cleaning Starts here: version: %s. Date: %s",
            dat_version, Sys.time())





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
#   args <- parser$parse_args(arguments)
#   
#   
#   
# }
# 
# main(commandArgs(trailingOnly = TRUE))



## Enc's

rar_enc <- load_RAR_enc(dat_file = enc_file, service_grouping_file = service_grouping_file,
                        bp_only = FALSE, EMPI_DATE_Level = TRUE, outpatient_only = TRUE, logger = logger)

# save(rar_enc, file = "bios_data.RData")



## Dx's
dx_all <- load_RAR_Dx(dat_file = dx_file, logger = logger)
dx_all <- sub_RAR_dx(dat=dx_all, icd_map_file = icd_map_file,
                     CODE_Level = CODE_Level, 
                     hierarchy_dx = hierarchy_dx, 
                     EMPI_DATE_Level = TRUE, left_censor_date = left_censor_date,
                    outpatient_only = TRUE, logger=logger)


# save(rar_enc, rar_dx, file = "bios_data.RData")


## Patient Demo
rar_demo <- load_RAR_PtDemo(dat_file = demo_file, logger = logger)

# save(rar_enc, rar_dx, rar_demo, file = "PA_analysis/Biostat_process/bios_data.RData")


## Labs from PDS_EPIC and PDS_CERNER
## lab_all <- get_RAR_lab_EPIC_CERNER(lab_file_epic = lab_file_epic, lab_file_cerner = lab_file_cerner, logger = logger)


lab_all <- get_RAR_lab_EPIC_CERNER(lab_file_epic = lab_file_PDS_epic, lab_file_cerner = lab_file_PDS_cerner,
                                   left_censor_date = left_censor_date, logger=logger)




save(dx_all, lab_all, rar_demo, rar_enc, file = paste("PA_analysis/Biostat_process/bios_data_",
                                                      dat_version,".RData", sep=""))




# Version 0.2.1.2 RData
# load("/data/processed_data/RAR/bios_data_v0.2.1.2.RData")
# Merge all files
rar_mg <- rar_merge(rar_dx = dx_all, rar_enc = rar_enc, rar_lab = lab_all, rar_demo = rar_demo, 
                  id = "EMPI_DATE", pt_id = "EMPI", left_censor_date = left_censor_date,
                  join_to_ENC=TRUE, logger=logger)


# Collapse into Patients' Level
pts <- enc_to_pts(rar_enc_level = rar_mg, lab_time_window = 14, logger=logger)


################    Adding Research Database: AVS- total patient list 2001-2015.xlsx
# read in Research Database
# read in AVS data
AVS_pts <- read_excel(path = "/data/raw_data/PA/Debbie_DB/AVS- total patient list 2001-2015.xlsx", 
                      sheet = "Sheet1")
msg <- sprintf("RDB: AVS 2001-2015: %d unique hosp_num, %d rows", length(unique(AVS_pts$hosp_num)), nrow(AVS_pts))
if(!is.null(logger)) logger$info(msg)

AVS_pts <- na.omit(AVS_pts)


# foo <- function(x, year=1999){
#   m <- year(x) %% 100
#   year(x) <- ifelse(m > year %% 100, 1900+m, 2000+m)
#   x
# }
# AVS_pts$enctr_date <- as.POSIXct(AVS_pts$enctr_date, format="%d-%b-%y")
# AVS_pts$date_birth <- foo(as.POSIXct(AVS_pts$date_birth, format="%d-%b-%y"), year = 2000)


# read in EMPI ~ MRN for RAR patients: all columns as characters when reading in
empi_MRN <- fread(input = empi_mrn_file, 
                  header=TRUE, colClasses = "character")
empi_MRN <- as.tibble(empi_MRN)

## remove duplicates
empi_MRN <- empi_MRN[!duplicated(empi_MRN),]

empi <- empi_MRN %>% select(c(MRN, EMPI)) %>%
  group_by(MRN, EMPI) %>%
  slice(1) %>%
  ungroup()

research_db <- AVS_pts %>% left_join(., empi, by = c("hosp_num" = "MRN"))
PA_empi <- na.omit(unique(research_db$EMPI))

msg <- sprintf("RDB: AVS 2001-2015: %d unique EMPI", length(unique(PA_empi)))
if(!is.null(logger)) logger$info(msg)


rar_mg$PA_AVS_tot_0115 <- ifelse(rar_mg$EMPI %in% PA_empi, TRUE, FALSE)
pts$PA_AVS_tot_0115 <- ifelse(pts$EMPI %in% PA_empi, TRUE, FALSE)

rar_mg %<>% select(PA_AVS_tot_0115, everything()) %>% ungroup()
pts %<>% select(PA_AVS_tot_0115, everything()) %>% ungroup()

msg <- sprintf("Pts in RDB (AVS 2001-2015): %d unique EMPI in our cohort(%d pts in total) are in RDB (%d pts in total)", 
               sum(pts$PA_AVS_tot_0115), length(unique(pts$EMPI)), length(unique(PA_empi)))
if(!is.null(logger)) logger$info(msg)


###########   End of Research Database


save(rar_mg, pts, file = paste("PA_analysis/Biostat_process/bios_data_enc_pts_", 
                               dat_version, ".RData", sep=""))




####################
###   Deidentify  ##
####################
## De-identify RAR- ENC Level

enc_deid <- deidentify(dat = rar_mg, primary_key = "EMPI_DATE", pt_id = "EMPI", mode = "create", 
                        drop_cols = c("SOURCE_CODE", "PATIENT_MASTER_CLASS"), 
                        dt_cols = c("ENC_DATE", "BIRTH_DATE"),
                        out_file_for_mapping = paste(output_dir, 
                                                     paste(out_root, paste("de_id_mapping_",dat_version, sep = ""), "csv", sep="."),
                                                     sep="/"), 
                        seed = seed_config, logger=logger)

msg <- sprintf("deidentify: enc level data deidentified")
if(!is.null(logger)) logger$info(msg)


enc_deid %<>% select(-BIRTH_DATE)

### reorder columns
enc_deid %<>% select(DE_PT_ID, DE_primary_key, PA_AVS_tot_0115, ENC_DATE,
                      ENC_Time_in_Sys_days, GENDER_MASTER_CODE, 
                      RACE_MASTER_CODE, RACE_MASTER_HISPANIC_YN,
                      Age, everything())



## De-identify RAR - Patient Level
pt_level <- deidentify(dat = pts, primary_key = NULL, pt_id = "EMPI", mode = "load",
                       drop_cols = c("SOURCE_CODE", "PATIENT_MASTER_CLASS", "EMPI_DATE", "ORDER_START_DATE", "ENC_DATE"), 
                       dt_cols = c("BIRTH_DATE", "first_ENC_DATE", "RAR_DATE","bp_ENC_DATE", "AVS_first_DATE", "UAldo_first_DATE"),
                       in_file_for_mapping = paste(output_dir, 
                                                   paste(out_root, paste("de_id_mapping_",dat_version, sep = ""), "csv", sep="."),
                                                   sep="/"), logger)

msg <- sprintf("deidentify: patient level data deidentified")
if(!is.null(logger)) logger$info(msg)

pt_level %<>% select(-BIRTH_DATE)


### reorder columns
pt_level %<>% select(DE_PT_ID, PA_AVS_tot_0115, everything())



save(enc_deid, pt_level, file = paste(output_dir, paste(out_root, paste("bios_data_deid_", dat_version, sep=""), "RData", sep="."),
                                       sep="/"))


# Write out resulting datasets
write.csv(enc_deid, 
          file = paste(output_dir, 
                       paste(out_root, paste("de_enc_level_",dat_version, sep=""), "csv", sep="."),
                       sep="/"),
          row.names = FALSE)
write.csv(pt_level, 
          file = paste(output_dir, 
                       paste(out_root,  paste("de_pt_level_",dat_version, sep=""), "csv", sep="."),
                       sep="/"),
          row.names = FALSE)


