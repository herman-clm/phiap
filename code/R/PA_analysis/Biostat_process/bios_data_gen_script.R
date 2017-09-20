#!/usr/bin/env Rscript
##############
# This is the script for preparing data set for Biostat's group based on RAR data
# This is used for running in bash and generating logs, in most functions, a logger should be passed as a parameter
##############

suppressPackageStartupMessages(library(tidyverse, quietly = TRUE))
suppressPackageStartupMessages(library(data.table, quietly = TRUE))
library(ini)
suppressPackageStartupMessages(library(argparse, quietly=TRUE))
suppressPackageStartupMessages(library(logging, quietly = TRUE))
basicConfig()
# source("../../common_anal/R_fxns.R")

main <- function(arguments){
  
  parser <- ArgumentParser(description='biostat data-gen script')
  parser$add_argument('project',
                      choices=c("RAR", "other"),
                      help="Project group")
  parser$add_argument('-config', '--config_file',
                      action="store",
                      help="Configuration .ini file",
                      default="config.ini")
  args <- parser$parse_args(arguments)
  
  # Read config and load
  config <- read.ini(args$config_file)
  # output_dir <- config$Directories$output_dir
  # out_root <- config[[args$project]]$out_root
  # working_dir <- config$Directories$working_dir
  
  working_dir <- paste(config$Directories$repos_dir, config$Directories$working_dir_suffix, sep = "/")
  
  output_dir <- paste(config$Directories$repos_dir, config$Directories$output_dir_suffix, sep = "/")
  out_root <- config$RAR$out_root
  
  # Set working dir
  setwd(working_dir)
  
  # Setup output dir
  dir.create(output_dir, showWarnings=FALSE)
  
  # Setup log
  log_file <- paste(normalizePath(output_dir), 
                    paste(out_root, "log.txt", sep='.'),
                    sep="/")
  addHandler(writeToFile, file=log_file,
             level='INFO', logger='general_logger')
  logger <- getLogger('general_logger')
  logger$info("Starts here")

  # Source
  source("common_anal/R_fxns.R")
  source("common_anal/RAR_fxns.R")
  
  ## HERMANDA_RAR_PTS_ENC.csv
  tm0 <- Sys.time()
  rar_enc <- load_RAR_enc(dat_file = "/data/raw_data/PA/HERMANDA_RAR_PTS_ENC.csv", 
                           bp_only = TRUE, EMPI_DATE_Level = TRUE, outpatient_only = FALSE, logger = logger)
  tm1 <- Sys.time()
  logger$info("Loaded encounters [%d ROWS] in %.4f secs", nrow(rar_enc), as.numeric(difftime(tm1, tm0, units = "secs")))
  
  
  
  
  ## HERMANDA_RAR_PTS_DX.csv
  tm0 <- Sys.time()
  rar_dx <- load_RAR_Dx(logger = logger)
  tm1 <- Sys.time()
  logger$info("Loaded Dx's [%d ROWS] in %.4f secs", nrow(rar_dx), as.numeric(difftime(tm1, tm0, units = "secs")))
  
  rar_dx <- sub_RAR_dx(dat=rar_dx, ALDO.Dx = TRUE, HTN.Dx = TRUE,
                       n.Dx = 0, EMPI_DATE_Level = TRUE, outpatient_only = TRUE, logger = logger)
  logger$info("Subsetted %d Dx codes [%d ROWS]", length(unique(rar_dx$CODE)),nrow(rar_dx))
  
  ## HERMANDA_RAR_PTS_DEMO.csv
  tm0 <- Sys.time()
  rar_demo <- load_RAR_PtDemo(logger = logger)
  tm1 <- Sys.time()
  logger$info("Loaded Dx's [%d ROWS] in %.4f secs", nrow(rar_dx), as.numeric(difftime(tm1, tm0, units = "secs")))
  

  
  ## HERMANDA_RAR_PTS_LABS.csv
  tm0 <- Sys.time()
  rar_lab <- load_Lab(dat_file = "/data/raw_data/PA/HERMANDA_RAR_PTS_LABS.csv", 
                      potassium = FALSE, adjust_up = 1.5, adjust_down = 0.5, logger = logger)
  tm1 <- Sys.time()
  logger$info("Loaded Dx's [%d ROWS] in %.4f secs", nrow(rar_dx), as.numeric(difftime(tm1, tm0, units = "secs")))
  
  rar_lab <- clean_RAR_Lab(dat = rar_lab, RAR_only = TRUE, EMPI_DATE_Level = TRUE, logger = logger)
  

  
  # Merge all files
  rar_mg <- rar_merge(rar_dx = rar_dx, rar_enc = rar_enc, rar_lab = rar_lab, rar_demo = rar_demo, 
                      id = "EMPI_DATE", pt_id = "EMPI")
  
  
  # Collapse into Patients' Level
  pts <- enc_to_pts(rar_enc_level = rar_mg$rar_mg, num_dx = rar_mg$num_dx)
  
  
  
  
  # read in Research Database
  # read in AVS data
  AVS_pts <- read_excel(path = "/data/raw_data/PA/Debbie_DB/AVS- total patient list 2001-2015.xlsx", 
                        sheet = "Sheet1")
  AVS_pts <- na.omit(AVS_pts)
  
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
                                                       sep="/"), 
                          seed = config$RAR$seed)
  
  enc_level %<>% select(-BIRTH_DATE)
  
  ### reorder columns
  enc_level %<>% select(DE_PT_ID, DE_EMPI_DATE_ID, PA_AVS_tot_0115, ENC_DATE, ORDER_START_DATE, everything())
  
  
  ## De-identify RAR - Patient Level
  pt_level <- deidentify(dat = pts, main_id = NULL, pt_id = "EMPI",
                         drop_cols = c("SOURCE_CODE", "PATIENT_MASTER_CLASS", "EMPI_DATE", "ORDER_START_DATE", "ENC_DATE"), 
                         dt_cols = c("BIRTH_DATE"),
                         out_file_for_mapping = paste(output_dir, 
                                                      paste(out_root, "de_id",Sys.Date(), "csv", sep="."),
                                                      sep="/"))
  pt_level %<>% select(-BIRTH_DATE)
  
  ### reorder columns
  pt_level %<>% select(DE_PT_ID, PA_AVS_tot_0115, everything())
  
  
  
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
  
  
}

main(commandArgs(trailingOnly=TRUE))




