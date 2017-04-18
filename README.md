# Goal
* Predict aldosteronism in patients based on EHR data.
 * First: use existing aldostorone identifying guidelines, implement those using SQL and PennDataStore (or any other relevent PennMedicine data resources)
 * Eventually: build a model independent of guidelines to predict aldosteronism as well as possible

# Background
Primary aldosteronism or PA is a hormonal disorder that leads to high blood pressure.  It occurs when the body produces too much of the hormone, aldosterone.  PA is highly treatable, however it is difficult to identify.  It exists in about 5% of high blood pressure cases.

# Meeting 2017-04-18 Selah & Sunil & Yuliya & Dan
## TODO
### For next meeting
* get one patient MRN from dan, pull up last version of progress notes (free-text) from Clarity - Sunil
    * Selah file ticket
* finalize preliminary list of hyptertension ICD codes - Dan
    * reference Teixera paper, get from Dan
* put together list of medications from given cohort - Sunil (Selah file ticket)
    * cohort is a random subset of the following: 2yrs, 2+ encounters, 2+ blood pressure medication, outpatient only
        * size is TBD  ~1000k
    * select only patients with ICD codes related to hypertension
    * from the final cohort, generate a list of all blood pressure medications, send it to dan
    * final product is list of medications
    * Selah file ticket
* use NLP or regex to parse out the dosage/day info - Selah

### Future
* Dan will do chart reviews on ~100 patients
* Get table of patients, all their meds, blood pressure medications, ICD codes, use to predict Dan's chart review results

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
	* 	Zeroing in on paper Teixeira - Evaluating electronic health record data sources and algorithmic approaches to identify hypertensive individuals.  Looking at 3rd full paragraph on pg 167 that describes algorithm for identifying patients with hypertension.
*  Dan emphasizes the advantages of querying PDS directly from R or Python scripts, rather than storing intermediate datasets

### TODO's
* take above mentioned paragraph and reproduce the results
* attempt to get necessary data through standard DAC avenues. 
	* PDS/Yuliya/Sunil

# LOG
Created an Athena Ticket- 3/21/2017 - Get a list of patients with BP medication - Selah/Sunil

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
