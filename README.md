IRB #827260


# Goal
* Reimplement algorithm specified in Teixera paper, evaluate how it works on PennMedicine data
    * [Teixera paper](http://doi.org/10.1093/jamia/ocw071)
    * Supplimentary info - http://bic.med.upenn.edu/cicTeam/Daniel_Herman_Aldosterone_2017_03/blob/master/ocw071_Supp.pdf

## Future Project Thoughts
* tools for creative ideas
    * timeline plots, spaghetti plots
    * notes timeline
    * histograms?  clustering?
* what else can we diagnose? PA? What else
* what can we create that is new rather than validation
* how can we take better advantage of longitudinal data
* Predict aldosteronism in patients based on EHR data.
 * First: use existing aldostorone identifying guidelines, implement those using SQL and PennDataStore (or any other relevent PennMedicine data resources)
 * Eventually: build a model independent of guidelines to predict aldosteronism as well as possible


# Background
Primary aldosteronism or PA is a hormonal disorder that leads to high blood pressure.  It occurs when the body produces too much of the hormone, aldosterone.  PA is highly treatable, however it is difficult to identify.  It exists in about 5% of high blood pressure cases.

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
