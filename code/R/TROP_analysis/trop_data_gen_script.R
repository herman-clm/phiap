#!/usr/bin/env Rscript
##############
# This is the script for preparing data set for TROPONIN
# This is used for running in bash and generating logs, in most functions, a logger should be passed as a parameter
##############

suppressPackageStartupMessages(library(tidyverse, quietly = TRUE))
suppressPackageStartupMessages(library(data.table, quietly = TRUE))
library(ini)
suppressPackageStartupMessages(library(argparse, quietly=TRUE))
suppressPackageStartupMessages(library(logging, quietly = TRUE))
basicConfig()

main <- function(arguments){
  
  parser <- ArgumentParser(description='biostat data-gen script')
  parser$add_argument('project',
                      choices=c("TROP", "other"),
                      help="Project group")
  parser$add_argument('-config', '--config_file',
                      action="store",
                      help="Configuration .ini file",
                      default="config.ini")
  args <- parser$parse_args(arguments)
  
  # Read config and load
  config <- read.ini(args$config_file)
  
  working_dir <- paste(config$Directories$repos_dir, config$Directories$working_dir_suffix, sep = "/")
  output_dir_deid <- config$Directories$output_dir_deid
  output_dir_pro <- config$Directories$output_dir_pro
  out_root <- config$TROP$out_root
  log_dir <- config$Directories$log_dir
  
  # Set working dir
  setwd(working_dir)
  
  # Setup output dir
  dir.create(output_dir_deid, showWarnings=FALSE)
  dir.create(output_dir_pro, showWarnings=FALSE)
  
  # Setup log
  log_file <- paste(log_dir, 
                    paste(out_root, "log.txt", sep='.'),
                    sep="/")
  addHandler(writeToFile, file=log_file,
             level='INFO', logger='TROP_logger')
  logger <- getLogger('TROP_logger')
  logger$info("Starts here")

  # Source
  source("common_anal/R_fxns.R")
  source("common_anal/RAR_fxns.R")
  
  ## HERMANDA_RAR_PTS_LABS.csv
  tm0 <- Sys.time()
  dat_lab <- load_Lab(dat_file = "/data/raw_data/TROP/select___from_TROP.csv", 
                      adjust_up = 1.5, adjust_down = 0.5, lab_source = "RAR",logger = logger)
  tm1 <- Sys.time()
  logger$info("Loaded Dx's [%d ROWS] in %.4f secs", nrow(rar_lab), as.numeric(difftime(tm1, tm0, units = "secs")))
  
  #dat_lab <- clean_RAR_Lab(dat = dat_lab, RAR_only = TRUE, EMPI_DATE_Level = TRUE, logger = logger)
  

  # make a Primary key: PK_ORDER_ID + PK_ORDER_RESULT_ID
  dat_lab %<>% mutate(PK = paste(.$PK_ORDER_ID, .$PK_ORDER_RESULT_ID, sep="_"))
  
  
  # write out processed data set
  # TODO: this write.csv is a temporary solution. The commented one is the desired, but right now, R does not have permission to write into /data folder
  write.csv(dat_lab, file = paste("../../output", paste(out_root, "lab_lv", Sys.Date(), "csv", sep = "."), sep="/"))
  # write.csv(dat_lab, file = paste(output_dir_pro, 
  #                                paste(out_root, "lab_lv", Sys.Date(), "csv", sep="."),
  #                                sep="/"))

  
 
  ## De-identify RAR- ENC Level
  # TODO: this write.csv is a temporary solution. The commented one is the desired, but right now, R does not have permission to write into /data folder
  
  dat_lab <- deidentify(dat = dat_lab, 
                        seed = config$TROP$seed, logger = logger,
                        mode = "create",
                        primary_key = "PK", 
                        pt_id = "EMPI",
                        drop_cols = c("MRN", "PK_ENCOUNTER_ID", "PK_ORDER_ID", "PK_ORDER_RESULT_ID", "PK_ORDER_PERFORMED_ID", "ORDERING_PROV", "ADMITTING_PROV"), 
                        dt_cols = c("ENC_DATE", "ORDER_DATE", "ORDER_START_DATE", "RESULT_DATE", "PERFORMED_DATE"),
                        out_file_for_mapping =  paste("../../output", paste(out_root, "lab_lv_deid_mapping", Sys.Date(), "csv", sep = "."), sep="/")
  )
  
  # dat_lab <- deidentify(dat = dat_lab, 
  #                         seed = config$TROP$seed, logger = logger,
  #                         mode = "create",
  #                         primary_key = "PK", 
  #                         pt_id = "EMPI",
  #                         drop_cols = c("MRN", "PK_ENCOUNTER_ID", "PK_ORDER_ID", "PK_ORDER_RESULT_ID", "PK_ORDER_PERFORMED_ID", "ORDERING_PROV", "ADMITTING_PROV"), 
  #                         dt_cols = c("ENC_DATE", "ORDER_DATE", "ORDER_START_DATE", "RESULT_DATE", "PERFORMED_DATE"),
  #                         out_file_for_mapping = paste(output_dir_pro, 
  #                                                      paste(out_root, "lab_lv_deid_mapping",Sys.Date(), "csv", sep="."),
  #                                                      sep="/")
  #                         )
  

  ### reorder columns
  dat_lab %<>% select(DE_primary_key, DE_PT_ID, everything())
  
  # Write out resulting datasets
  # TODO: this write.csv is a temporary solution. The commented one is the desired, but right now, R does not have permission to write into /data folder
  
  write.csv(dat_lab, 
            file = paste("../../output", 
                         paste(out_root, "lab_lv_deid", 
                               Sys.Date(), "csv", sep="."),
                         sep="/"),
            row.names = FALSE)
  # write.csv(dat_lab, 
  #           file = paste(output_dir_deid, 
  #                        paste(out_root, "all_lab", 
  #                              Sys.Date(), "csv", sep="."),
  #                        sep="/"),
  #           row.names = FALSE)
}

main(commandArgs(trailingOnly=TRUE))




