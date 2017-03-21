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
 
