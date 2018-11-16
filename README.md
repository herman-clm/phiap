IRB #827260


# Goal
* Reimplement algorithm specified in Teixera paper, evaluate how it works on PennMedicine data
    * [Teixera paper](http://doi.org/10.1093/jamia/ocw071)
    * Supplimentary info - http://bic.med.upenn.edu/cicTeam/Daniel_Herman_Aldosterone_2017_03/blob/master/ocw071_Supp.pdf

## Future Project Ideas
http://bic.med.upenn.edu/cicTeam/Daniel_Herman_Aldosterone_2017_03/blob/master/future_project.md



# Meeting 2017-07-11 Dan, Selah, Sunil
## Things to show/discuss
* Progess on issues
    * EMPI
    * Historical meds
    * Patient level compilation, (waiting on new regex)
* Preliminary modelling,
    * do we have outcome variables?
* RedCap next steps
* Knime, ConceptIndexer investigations

# Meeting 2017-06-22 Dan, Selah, Sunil
## Things to show/discuss
* compiling data on the patient level
* look at regex results
* note concatination
* go over and close gitlab issues
* next steps


# Meeting 2017-06-01 Dan, Selah, Sunil
## Things to show/discuss
* complete medication list!
* collecting tables we need for analysis, see [hypertension_table_prep.txt](hypertension_table_prep.txt)

# Meeting 2017-05-09 Dan & Sunil & Selah & Yancy?
## TODO
* create a list of possibly missed hypertension medication
    * take sunil medication list
    * group by full_name, with frequency column
    * exclude any pharmacy classes in Dan's original query
    * give it to dan for manual evaluation
* select random 1000 patients using same criteria as previous cohort, send it to Dan
* Dan - dan do chart review on some of these
* Sunil - get free text notes for 1000 cohort
* Selah - improve medication parsing (hold off) until medications
	* manual inspection of accuracy… get a number
	* investigate papers and suggestions
	* improve regex


# Meeting 2017-04-18 Selah & Sunil & Yuliya & Dan
## TODO
### For next meeting
* get one patient MRN from dan, pull up last version of progress notes (free-text) from Clarity - Sunil
    * Selah file ticket
* finalize preliminary list of hyptertension ICD codes - Dan
    * reference Teixera paper, get from Dan
    * [Teixera paper](http://doi.org/10.1093/jamia/ocw071)
* put together list of medications from given cohort - Sunil (Selah file ticket)
    * cohort is a random subset of the following: 2yrs, 2+ encounters, 2+ blood pressure ~~medication~~ measurements, outpatient only
        * size is TBD  ~1000k
    * select only patients with ICD codes related to hypertension
    * from the final cohort, generate a list of all blood pressure medications, send it to dan
    * final product is list of medications
    * Selah file ticket
* use NLP or regex to parse out the dosage/day info - Selah or Yancy

### Future
* Dan will do chart reviews on ~100 patients
* Get table of patients, all their meds, blood pressure medications, ICD codes, use to predict Dan's chart review results in a manner similar to Teixera paper
    * [Teixera paper](http://doi.org/10.1093/jamia/ocw071)

# Meeting 2017-04-10 Jason & Liat & Selah
* Liat emphasizes we cannot select a set of medications or ICD codes for a researcher, these need to come from the researcher
* Liat emphasizes that "selection of a cohort" should be the first step
* Liat recommends PennOmics as a tool that could be useful here

# Meeting 2017-04-10 Selah & Dan & Sunil
* Focused on RHTN_meds table from http://bic.med.upenn.edu/cicTeam/Daniel_Herman_Aldosterone_2017_03/blob/master/sql/resistant_htn.sql
* Three potential improvments to this query
    * More comprehensive list of medications that are relevent to hypertension
    * More reliable parsing to identify #pills/day or MG/day of the medication
    * MOST IMPORTANT - More reliable identification of the end date of a medication
        * Including unstructured EHR notes here could be key
        * Sunil will pull unstructured notes for one MRN so that Dan can take a look at it
* Eventually want to gather ICD information and BP measurements as well.
* Looking exclusively at outpatient data from 2015 and 2016

# Meeting 2017-04-07 Selah & Dan
* went over some of dan's SQL code
* adjusted goals for first steps
	* Zeroing in on paper Teixeira - Evaluating electronic health record data sources and algorithmic approaches to identify hypertensive individuals.  Looking at 3rd full paragraph on pg 167 that describes algorithm for identifying patients with hypertension.
    * [Teixera paper](http://doi.org/10.1093/jamia/ocw071)
*  Dan emphasizes the advantages of querying PDS directly from R or Python scripts, rather than storing intermediate datasets

### TODO's
* take above mentioned paragraph and reproduce the results
* attempt to get necessary data through standard DAC avenues.
	* PDS/Yuliya/Sunil

# LOG
Created an Athena Ticket- 3/21/2017 - Get a list of patients with BP medication - Selah/Sunil

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

# Initial goals
* Collect medication information
  * Collect prescription data from PDS
  * Capture medication prescription information from notes
    * Trial MedEx or equivalent
* Explore PDS population
  * Patient counts for # of visits, # of BP readings, # of medication prescriptions over time
* Build algorithm to identify patients with hypertension
  * Implement methods developped in Teixeira 2016.
    * First goal: Sum of normalized (1) hypertension ICD9, (2) meds, (3) BP, and (4) concept
      * ICD codes
      	* ICD9: 405.99 SECOND HYPERTENSION NEC,	405 SECONDARY HYPERTENSION, 405.9 SECOND HYPERTENSION NOS, 401 ESSENTIAL HYPERTENSION, 401.1 BENIGN HYPERTENSION, 401.0 MALIGNANT HYPERTENSION, 405.91 RENOVASC HYPERTENSION, 405.0 MAL SECOND HYPERTENSION, 401.9 HYPERTENSION NOS
	* ICD10: I1* (all hypertension) [subcategories: I10 Essential (primary) hypertension, I15* Secondary hypertension]
  * Subphenotypes
    * Controlled resistant hypertenstion: 4 medications
      * Medication categories: angiotensin converting enzyme inhibitors or angiotensin receptor blockers, beta blockers, non-dihydropyridine calcium channel blockers, dihydropyridine calcium channel blockers, hydralazine, minoxidil, central alpha agonists, direct renin antagonists, aldosterone antago- nists, alpha antagonists, and diuretics (thiazides, K-sparing, and loop diuretics)
    * Uncontrolled resistant hypertenstion: 3 medications AND "reasonable" doses AND SBP > 140 mmHg AND DBP > 90 mmHg after 1 month on medications
* Identify patients with primary aldosteronism
  * Data sources

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
1.  run SQL file `ar.sql` in `code/sql`
2. output individual result tables into files, put them in location corresponding to `config.ini` file above

### Cleaning
1. run file `biostat_data_pre.R` in `code/R/PA_analysis/Biostat_process/`



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


##### Encounter Level
At encounter level:
1. added SERVICE_GROUP_def, which is a regrouping for SERVICE_MASTER_CODE [PDS_CERNER]
2. added AVS Tests Results[Left Aldos, LCORT, Right Aldos, RCORT, IVC Aldosterone, ICORT]
3. added Urine Aldo Test Results ["24HR URINE CREATININE"("CRE D ALDOS"), "URINE ALDOSTERONE"("ALDOSTERONE, URINE"),  4 merged to 2]
4. added patients' zip codes
5. added MASTER_LOCATION_ZIP, but not useful now since majority are missing
6. NOTES: counted occurances for specific words/terms in the med notes. The columns starts with "re_". "NOTE_n" is the number of notes a patient had.
7. MEDS: counted MEDICATION information. The columns starts with "MED_", ends with "_N" or "_UNIQUE_N", which stand for sum of MEDS and sum of UNIQUE MEDS respectively. "MED_N" is the total MEDICATION a patient had. "MED_K_"/"MED_HTN_", etc., are the sums for grouped meds.
Note, for AVS and Urine Aldo Tests, their TEST DATE are first date, but their RESULT VALUE are last one.

This is from EPIC... just for later reference
AVS Labs from PDS/CERNER were included, covering 8 more tests (RESULT_ITEM_CODE from CERNER): LEFT CORTISOL, LCORT, C LCORT, LEFT ALDOSTERONE, RIGHT CORTISOL, RIGHT ALDOSTERONE, IVC CORTISOL, IVC ALDOSTERONE.
After removing NA RESULT_VALUE, these 6 RESULT_ITEM_CODE and ORDER_ITEM_CODE:

| RESULT_ITEM_CODE      | ORDER_ITEM_CODE | Note                  |
|-----------------------|-----------------|-----------------------|
| IVC ALDOSTERONE       | C9009997        |                       |
| IVC CORTISOL          | C3500690        |                       |
| IVC CORTISOL SAMPLE   | C3500690        | excluded using non-NA |
| LEFT ALDOSTERONE      | C9009990        |                       |
| LEFT CORTISOL         | C3500670        |                       |
| LCORT                 | C3500670        |                       |
| C LCORT               | C3500670        |                       |
| LEFT CORTISOL SAMPLE  | C3500670        | excluded using non-NA |
| RIGHT ALDOSTERONE     | C9009995        |                       |
| RIGHT CORTISOL        | C3500680        |                       |
| RIGHT CORTISOL SAMPLE | C3500680        | excluded using non-NA |

Function `clean_lab` in RAR_fxn.R was renamed as `lab_select`, which better reflects its usage. Also made generalized functions to load and clean labs.


# References
* MedEx:
  * Xu, H., Stenner, S. P., Doan, S., Johnson, K. B., Waitman, L. R., & Denny, J. C. (2010). MedEx: a medication information extraction system for clinical narratives. Journal of the American Medical Informatics Association, 17(1), 19–24. http://doi.org/10.1197/jamia.M3378
  * Xu, H., Jiang, M., Oetjens, M., Bowton, E. A., Ramirez, A. H., Jeff, J. M., et al. (2011). Facilitating pharmacogenetic studies using electronic health records and natural-language processing: a case study of warfarin. Journal of the American Medical Informatics Association, 18(4), 387–391. http://doi.org/10.1136/amiajnl-2011-000208
* EHR hypertension
  * Teixeira, P. L., Wei, W.-Q., Cronin, R. M., Mo, H., VanHouten, J. P., Carroll, R. J., et al. (2016). Evaluating electronic health record data sources and algorithmic approaches to identify hypertensive individuals. Journal of the American Medical Informatics Association, 24(1), 162–171. http://doi.org/10.1093/jamia/ocw071
  * Dumitrescu, L., Ritchie, M. D., Denny, J. C., Rouby, El, N. M., McDonough, C. W., Bradford, Y., et al. (2017). Genome-wide study of resistant hypertension identified from electronic health records. PloS One, 12(2), e0171745. http://doi.org/10.1371/journal.pone.0171745
  * Sun, J., McNaughton, C. D., Zhang, P., Perer, A., Gkoulalas-Divanis, A., Denny, J. C., et al. (2014). Predicting changes in hypertension control using electronic health records from a chronic disease management program. Journal of the American Medical Informatics Association, 21(2), 337–344. http://doi.org/10.1136/amiajnl-2013-002033
* EHR phenotyping
  * Sun, J., McNaughton, C. D., Zhang, P., Perer, A., Gkoulalas-Divanis, A., Denny, J. C., et al. (2014). Predicting changes in hypertension control using electronic health records from a chronic disease management program. Journal of the American Medical Informatics Association, 21(2), 337–344. http://doi.org/10.1136/amiajnl-2013-002033
  * Newton, K. M., Peissig, P. L., Kho, A. N., Bielinski, S. J., Berg, R. L., Choudhary, V., et al. (2013). Validation of electronic medical record-based phenotyping algorithms: results and lessons learned from the eMERGE network. Journal of the American Medical Informatics Association, 20(e1), e147–54. http://doi.org/10.1136/amiajnl-2012-000896
