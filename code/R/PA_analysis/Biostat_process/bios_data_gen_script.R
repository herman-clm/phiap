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
  
  # call biostat_data_pre.R 
 
  
  
}

main(commandArgs(trailingOnly=TRUE))




