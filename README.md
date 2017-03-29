# Goal
* Predict aldosteronism in patients based on EHR data.
 * First: use existing aldostorone identifying guidelines, implement those using SQL and PennDataStore (or any other relevent PennMedicine data resources)
 * Eventually: build a model independent of guidelines to predict aldostoronism as well as possible

# Background
Primary aldosteronism or PA is a hormonal disorder that leads to high blood pressure.  It occurs when the body produces too much of the hormone, aldosterone.  PA is highly treatable, however it is difficult to identify.  It exists in about 5% of high blood pressure cases.

# LOG
Created an Athena Ticket- 3/21/2017 - Get a list of patients with BP medication - Selah/Sunil

# TODO
* generate a list of distinct guidelines that are currently used to identify aldosterone-ism - Daniel
* come up with a list of PDS (and perhaps other) data fields that will be relevent to the guidelines - Selah & Sunil
* attempt to write some queries that select patients in PDS according to the guidelines - Selah & Sunil

# Data Sources
* Penn Data Store
  * Diagnosis codes
  * Blood pressures
  * Laboratory results
  * Medication Prescriptions
    * Consider using MedEx to extract dose information from prescription tables AND/OR from Notes
  * Notes (for NLP)
  * Demographics

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
