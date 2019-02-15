IRB #827260

# IRB Compliance HIPAA Info Link
http://www.med.upenn.edu/dac/irb-compliance.html

# PDS Info., DataModel and Data Dictionary
http://www.med.upenn.edu/dac/penn-data-store-warehouse.html

# Data Sources
* Penn Data Store
  * Diagnosis codes
  * Blood pressures
  * Laboratory results
  * Medication Prescriptions
    * Consider using MedEx to extract dose information from prescription tables AND/OR from Notes
  * Notes (for NLP)
  * Demographics
  * Schema: https://www.med.upenn.edu/dac/assets/user-content/images/ODS_Schema_4_1.jpg

# Aldosteronism
Primary aldosteronism or PA is a hormonal disorder that leads to high blood pressure.  It occurs when the body produces too much of the hormone, aldosterone.  PA is highly treatable, however it is difficult to identify.  It exists in about 5% of high blood pressure cases.

# Biostat Project [edited by Xiruo]
*edited on 09/28/17*
The project is in cooperation with people from Biostatistics Department, focusing on phenotyping EHR through likelihood approach. There are de-identified data sets on two levels: Encounter Level (EMPI_DATE for now), and Patient Level. The versions of the data sets are listed below:

## Processing Step
### ini
1. modify `config_template.ini` file in `code/R/PA_analysis/Biostat_process/`:
  - repos_dir
  - seed
2. rename it as `config.ini`

### Query
1.  Create Cohort
In PDS, execute SQL in `code/sql/RAR_All_Pts.sql`. This will create a table, `rar_full_pts`, containing all patients eligible for the study.
2. Individual Data Tables
In PDS, execute `code/sql/extract_rar_data.sql` to create separate tables for encounters, diagnosis, lab tests, meidcations, and clinical notes. These tables should be outputed and stored in in location corresponding to `config.ini` file above.


### Making Encounter- and Patient-level Dataframes
1. EHR_Transform is needed to clean notes and medications.
    1.1  set up EHR_Transform (only refer to this commit): http://bic.med.upenn.edu/hermanlab/EHR_transform/tree/289c346c5166fa93602ae90cd24cd43dcd303f88
    1.2  clean notes and collapse to EMPI_DATE level
         Refer to Execute Step 2 & 3 in README
    1.3  clean medications and collapse to EMPI_DATE level
         Refer to Execute Step 4 in README
2. run through file `biostat_data_pre.R` in `code/R/` folder.



## Version Logs
### Version 0.1
This is the first version, and was shared on 08/23/17.

### Version 0.1.1
Fixed Age issue. Shared on 08/30/17.

### Version 0.2
*   Added more Dx's by group (Hyperaldo, HTN, Diabetes, Sleep Apnea).
*   Added more labs (most frequent 44 lab test). These 44 labs seem from common Panels (CMP, CBC), but not necessary.
*   Added a Dx's mapping file for reference. Besides ICD-9/ICD-10 codes, three hierarchy levels were added (Dx_h0, Dx_h1, Dx_h2). Here are reference sites: [ICD-9](http://www.icd9data.com), [ICD-10](http://www.icd10data.com). The data is not shared yet.

### Version 0.2.1
In this version, all data were left-censored at 1997-01-01.
Patient Level:
*   Delete ENC_DATE
*   Labs
    1.  Added number of RAR tests
*   Added RAR_DATE, BP_ENC_DATE
*   Dx
    1.  Added Dx_h2
    2.  Added total number of outpatient Dx's
    3.  Added normalized number for Dx h0/h1/h2
*   Time
    1.  Added Time_in_System (in years, from first enc to last enc)
    2.  Added Time to 1st RAR
    3.  Added Time to 1st hyperaldo Dx
*   Enc
    1. Added SBP/DBP
    2. Added labs in a 14-day time window regarding to RAR DATE
    3. Added primary/RAR location/entity

Encounter Level:
*   Remove ~40 EMPI_DATE's which are in rar_mg but not in rar_enc. This will put the focus on only OUTPATIENTS now. (every data set was left-joined to ENCOUNTER table)
*   Added Dx_h2
*   Dx codes in h0, h1, h2 levels were changed from 0/1 to sums of corresponding ICD codes
*   Added total number of outpatient Dx's for each encounter (EMPI_DATE)
*   Changed Dx h0/h1/h2 from 0/1 into sums
*   Added total number of Dx's for each encounter
*   Added time to first enc


### Version 0.2.1.1
Fixed Patient's Level BP's

### Version 0.2.1.2
Fixed NA's for Dx_h0/1/2 and ICD codes.

### Version 0.2.2
Pulled new full data for the cohort based on:
(1) In the Research Database (AVS- total patient list 2001-2015.xlsx)
(2) With Hyperaldo Dx codes (ICD 9: 255.1*, ICD10: E26*)
(3) With Aldo/Renin test


### Version 0.2.3
IMPORTANT UPDATES OVERVIEW:
* Added 6 more Dx codes:
	* 227.0, D35.00: PA
	* 255.3, E27.0: HA
	* 255.9, E27.9: HA
* Added some time stamps regarding to BP enc
* Added distance to 19104
* Added NOTES!
* Added MEDS!
* Added PDS_CERNER for AVS Tests!

PDS_CERNER data were taken in this version, but they were only used for AVS Tests.

##### Patient Level
At Patient level, added 14 new time stamps:
1. first_ENC_DATE
2. first_ENC_BP_DATE
3. last_ENC_DATE
4. last_ENC_BP_DATE
5. time_enc_after_1st_RAR_yr (last enc to 1st RAR test, year)
6. time_enc_after_1st_hyperaldo_dx_yr (last enc to 1st HA Dx, year)
7. time_bp_to_1st_RAR_yr (1st enc with BP to 1st RAR test, year)
8. time_bp_to_1st_hyperaldo_dx_yr (1st enc with BP to 1st HA Dx, year)
9. time_bp_after_1st_RAR_yr (last enc with BP to 1st RAR test, year)
10. time_bp_after_1st_hyperaldo_dx_yr (last enc with BP to 1st HA Dx, year)
11. time_enc_to_1st_AVS_yr (1st enc to 1st AVS test, year)
12. time_enc_after_1st_AVS_yr (last enc to 1st AVS test, year)
13. time_bp_to_1st_AVS_yr (1st enc with BP to 1st AVS test, year)
14. time_bp_after_1st_AVS_yr (last enc with BP to 1st AVS test, year)
15. AVS_first_DATE (1st AVS Date: when any of Left Aldos, Right Aldos, IVC Aldosterone exits, i.e. at least one of them)
16. UAldo_first_DATE (1st Urine Aldo test Date: whenever there is )


along with two old ones (renamed)
1. time_enc_to_1st_RAR_yr (1st enc to 1st RAR test, year)
2. time_enc_to_1st_hyperaldo_dx_yr (1st enc to 1st HA Dx, year)

Note, time_in_sys_yr remained unchanged from last version.

Also,
1. added AVS Tests Results[Left Aldos, LCORT, Right Aldos, RCORT, IVC Aldosterone, ICORT], which were defined by last one [PDS_CERNER]
2. added Urine Aldo Test Results ["24HR URINE CREATININE"("CRE D ALDOS"), "URINE ALDOSTERONE"("ALDOSTERONE, URINE"),  4 merged to 2], which were defined by last one
3. added AVS indicator, 0/1, 1 for any of 3 aldo tests exits
4. added urine aldo indicator, 0/1, 1 for wherever URINE ALDOSTERONE exits
5. added patients' zip codes
6. added distance from patients' zip to 19104 in km, 5 are missing
7. NOTES: counted occurances for specific words/terms in the med notes. The columns starts with "re_". "NOTE_n" is the number of notes a patient had.
8. MEDS: counted MEDICATION information. The columns starts with "MED_", ends with "_N" or "_UNIQUE_N", which stand for sum of MEDS and sum of UNIQUE MEDS respectively. "MED_N" is the total MEDICATION a patient had. "MED_K_"/"MED_HTN_", etc., are the sums for grouped meds.
Note, for AVS and Urine Aldo Tests, their TEST DATE are first date, but their RESULT VALUE are last one.

