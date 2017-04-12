-- Exploring data that may be relevent to identifying patients with resistant hyper tension
-- Creates tables:
-- RHTN_meds
---- pulls out patient orders that are for medications relevant to hypertension
---- keyed by patient, medication, start date

-- RHTN_vitals
---- blood pressure measurements for patients
---- ??? multiple measurements for a given patient

-- Runs some exploratory statistics on these two tables

-- Uses Penn Data Store's ODS database

-- ??? how long does it take to run
-- ??? how many rows do these queries result in



--Specific antiHTN meds
DROP TABLE RHTN_meds;
CREATE TABLE RHTN_meds NOLOGGING AS
select /*+ALL_ROWS*/ 
    PI.EMPI, 
        O.FK_ENCOUNTER_ID,
        O.ORDER_START_DATE,
        O.ORDER_STOP_DATE,
        O.SOURCE_LAST_UPDATE_DATE,
        E.PATIENT_MASTER_CLASS,
        E.PK_ENCOUNTER_ID,
        M.SIMPLE_GENERIC_NAME,
        M.GENERIC_NAME,
        OM.QUANTITY,
        OM.REFILLS,
        OM.Dose,
        OM.Frequency_Name,
        ROS.STATUS_MASTER_DESCRIPTION
FROM ODS.ORDERS PARTITION(EPIC) O
JOIN ODS.Patient P
  ON O.FK_Patient_ID = P.PK_Patient_ID
JOIN ODS.ENCOUNTER PARTITION (EPIC) E
  ON E.PK_Encounter_ID = O.FK_Encounter_ID
JOIN ODS.Patient_Identifiers PI
  ON P.PK_Patient_ID = PI.FK_Patient_ID
JOIN ODS.ORDER_MED OM
  ON O.PK_Order_ID = OM.FK_Order_ID
JOIN ODS.R_ORDER_STATUS ROS
  ON ROS.PK_ORDER_STATUS_ID = O.FK_ORDER_STATUS_ID
JOIN ODS.R_Medication M
  ON OM.FK_Medication_ID = M.PK_Medication_ID
WHERE 
  (PHARMACY_CLASS IN ('Antihypertensive', 'Calcium blockers', 'Beta blockers', 'Diuretics') OR 
    SIMPLE_GENERIC_NAME IN ('Amlodipine-Atorvastatin', 'Isosorb Dinitrate-Hydralazine'))
  AND PI.Identifier_Name = 'EMPI'
  AND O.ORDER_START_DATE >= to_date('01-Dec-2014', 'dd-mon-yyyy')
  AND E.ENC_DATE >= to_date('01-Dec-2014', 'dd-mon-yyyy')
  AND PATIENT_MASTER_CLASS = 'OUTPATIENT'
  AND OM.SOURCE_CODE = 'EPIC' AND P.SOURCE_CODE = 'EPIC'
;

select * from RHTN_meds;
select count(*) as count,
count(distinct(EMPI)),
min(ORDER_START_DATE),
max(ORDER_START_DATE)
from RHTN_meds; --727326

/*
COUNT	COUNT(DISTINCT(EMPI))	MIN(ORDER_START_DATE)	MAX(ORDER_START_DATE)
727326	155659	01-DEC-2014 00:00:00	30-JUN-2017 00:00:00
*/


/*
select 
a.*, b.*
from all_ind_columns a, all_indexes b
where a.index_name=b.index_name 
and a.table_name = upper('Encounter')
and INDEX_OWNER = 'ODS' and OWNER = 'ODS'
order by a.table_name, a.index_name, a.column_position;

describe ods.encounter;
select * from all_ind_columns;

select PHARMACY_CLASS, PHARMACY_SUBCLASS , SIMPLE_GENERIC_NAME, count(*) from ODS.R_Medication
WHERE THERAPEUTIC_CLASS = 'Cardiovascular Agents'
GROUP BY PHARMACY_CLASS, PHARMACY_SUBCLASS, SIMPLE_GENERIC_NAME
ORDER BY PHARMACY_CLASS, PHARMACY_SUBCLASS, SIMPLE_GENERIC_NAME;
*/

-- Look for vital signs
DROP TABLE RHTN_vitals;
CREATE TABLE RHTN_vitals NOLOGGING AS
select /*+ALL_ROWS*/
      EMPI,
      ENC_DATE,
      BP_Diastolic,
      BP_Systolic
FROM ODS.ENCOUNTER PARTITION(EPIC) EVS
JOIN ODS.Patient P
  ON EVS.FK_Patient_ID = P.PK_Patient_ID
JOIN ODS.Patient_Identifiers PI
  ON P.PK_Patient_ID = PI.FK_Patient_ID
WHERE 
    EVS.ENC_DATE >= to_date('01-Jan-2014', 'dd-mon-yyyy')
    AND PI.Identifier_Name = 'EMPI'
    AND P.SOURCE_CODE = 'EPIC'
   AND BP_SYSTOLIC IS NOT NULL
   AND PATIENT_CLASS = 'Outpatient'
  AND (BP_SYSTOLIC > 140 AND BP_DIASTOLIC > 90)
;
  
select * from RHTN_vitals;
select count(*) as count,
count(distinct(EMPI)),
min(ENC_DATE),
max(ENC_DATE)
from RHTN_vitals; --727326

/*
COUNT	COUNT(DISTINCT(EMPI))	MIN(ENC_DATE)	MAX(ENC_DATE)
191810	98447	02-JAN-2014 07:00:00	14-JAN-2017 21:06:00
*/

select count(*),
        count(distinct(EMPI)),
        min(ENC_DATE),
      max(ENC_DATE)
FROM ODS.ENCOUNTER PARTITION(EPIC) EVS
JOIN ODS.Patient P
  ON EVS.FK_Patient_ID = P.PK_Patient_ID
JOIN ODS.Patient_Identifiers PI
  ON P.PK_Patient_ID = PI.FK_Patient_ID
WHERE 
    EVS.ENC_DATE >= to_date('01-Jan-2016', 'dd-mon-yyyy')
    AND PI.Identifier_Name = 'EMPI'
    AND P.SOURCE_CODE = 'EPIC'
   AND BP_SYSTOLIC IS NOT NULL
   AND PATIENT_CLASS = 'Outpatient'
  --AND (BP_SYSTOLIC > 140 OR BP_DIASTOLIC > 90)
;
  /*
  
  */
  
  select 
  PATIENT_CLASS, PATIENT_MASTER_CLASS, count(*) as count
  from ODS.Encounter
  group by PATIENT_CLASS, PATIENT_MASTER_CLASS
  order by count desc;
