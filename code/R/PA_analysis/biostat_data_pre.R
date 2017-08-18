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
rar_enc <- load_RAR_enc(dat_file = "/data/raw_data/PA/HERMANDA_RAR_PTS_ENC.csv", bp_only = TRUE, EMPI_DATE_Level = TRUE, outpatient_only = FALSE)

# save(rar_enc, file = "bios_data.RData")



## HERMANDA_RAR_PTS_DX.csv
rar_dx <- load_RAR_Dx()
rar_dx <- sub_RAR_dx(dat=rar_dx, ALDO.Dx = TRUE, n.Dx = 0, EMPI_DATE_Level = TRUE, outpatient_only = TRUE)

# save(rar_enc, rar_dx, file = "bios_data.RData")


## HERMANDA_RAR_PTS_DEMO.csv
rar_demo <- load_RAR_PtDemo()

# save(rar_enc, rar_dx, rar_demo, file = "bios_data.RData")


## HERMANDA_RAR_PTS_LABS.csv
rar_lab <- load_Lab(dat_file = "/data/raw_data/PA/HERMANDA_RAR_PTS_LABS.csv", potassium = FALSE, adjust_up = 1.5, adjust_down = 0.5)
rar_lab <- clean_RAR_Lab(dat = rar_lab, RAR_only = TRUE, EMPI_DATE_Level = TRUE)

# save(rar_enc, rar_dx, rar_demo, rar_lab, file = "bios_data.RData")

## HERMANDA_RARV3.csv
rar <- load_Lab(dat_file = "/data/raw_data/PA/HERMANDA_RARV3.csv", potassium = FALSE, adjust_up = 1.5, adjust_down = 0.5)
## TODO: Use this file to extract RAR Labs



# Merge all files
# TODO: change Merge Part into a function
## drop some columns
rar_enc %<>% select(-c(E_SOURCE_LAST_UPDATE, PK_ENCOUNTER_ID, ENC_DATE))
rar_dx %<>% select(-c(EMPI, CODE_STANDARD_NAME, COMMENTS, CODING_DATE, PRIMARY_YN, DX_SEQUENCE,DESCRIPTION, PK_DX_ID, SOURCE_LAST_UPDATE_DATE, ENC_DATE, PK_ENCOUNTER_ID, DX_TYPE, HAR_NUMBER))
rar_lab %<>% select(-c(EMPI, PK_ENCOUNTER_ID))


# spread rar_dx
rar_dx %<>% mutate(value = 1) %>%
  spread(CODE, value, fill = 0)
## QC: check whether rar_dx has unique rows regarding to EMPI_DATE
## length(unique(rar_dx$EMPI)) == dim(rar_dx)[1]


## Full Join: keep all info
rar_mg <- rar_dx %>% full_join(., rar_enc, by = "EMPI_DATE") %>% full_join(., rar_lab, by = "EMPI_DATE") %>% full_join(., rar_demo, by="EMPI")

## For NA's in Dx, change them into 0
rar_mg[,4:22] <- lapply(rar_mg[,4:22], function(x) x=ifelse(is.na(x), 0, x))
  

save(rar_mg, file = "bios_merged.RData")



# Collapse into Patients' Level
pts <- rar_mg

## SBP_n, DBP_n, High_BP_n
pts %<>% group_by(EMPI) %>% 
  mutate(SBP_n = sum(BP_SYSTOLIC >= 140, na.rm = TRUE), DBP_n = sum(BP_DIASTOLIC >= 90, na.rm = TRUE))

pts %<>% group_by(EMPI) %>% 
  mutate(High_BP_n = sum(BP_SYSTOLIC >= 140 | BP_DIASTOLIC >= 90, na.rm = TRUE))

## Drop BP's
pts %<>% select(-c(BP_SYSTOLIC, BP_DIASTOLIC))


## Dx's
pts %<>% group_by(EMPI) %>%
  mutate_at(vars(`401`:I15.9), funs(sum))

## RAR
pts %<>% group_by(EMPI) %>% 
  mutate(N = (is.na(Aldo) + is.na(PRA) + is.na(DRC))) %>%
  arrange(EMPI, N, ENC_DATE) %>% 
  slice(1) %>%
  select(EMPI, PK_PATIENT_ID, GENDER_MASTER_CODE, BIRTH_DATE, RACE_MASTER_CODE, RACE_MASTER_HISPANIC_YN,ENC_DATE, `401`:I15.9, Aldo:PRA, SBP_n, DBP_n, High_BP_n)




