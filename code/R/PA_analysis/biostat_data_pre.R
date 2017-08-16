##############
# This is the script for preparing data set for Biostat's group
# based on RAR data
##############

library(dplyr)
library(data.table)
library(tibble)
setwd("~/repos/Daniel_Herman_Aldosterone_2017_03/code/R/PA_analysis")
source("../common_anal/RAR_fxns.R")

## HERMANDA_RAR_PTS_ENC.csv
rar_enc <- load_RAR_enc(dat_file = "/data/raw_data/PA/HERMANDA_RAR_PTS_ENC.csv", bp_only = TRUE, HAR_Level = TRUE, outpatient_only = TRUE)

# save(rar_enc, file = "bios_data.RData")



## HERMANDA_RAR_PTS_DX.csv
rar_dx <- load_RAR_Dx()
rar_dx <- sub_RAR_dx(dat=rar_dx, ALDO.Dx = TRUE, n.Dx = 0, HAR_Level = TRUE, outpatient_only = TRUE)

# save(rar_enc, rar_dx, file = "bios_data.RData")


## HERMANDA_RAR_PTS_DEMO.csv
rar_demo <- load_RAR_PtDemo()

# save(rar_enc, rar_dx, rar_demo, file = "bios_data.RData")


## HERMANDA_RAR_PTS_LABS.csv






