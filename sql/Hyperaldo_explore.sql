-- Prelminary analysis of PA case counts

--ICD9 255.1 (255.10, 255.1, 
--ICD10 E26*
/*
select *
from ODS.R_Codes_Diagnosis CD
where code_standard_name = 'ICD-10' and
code LIKE '%E26%';
*/

-- Identify a toy subset with a hyperaldo-like diagnosis
-- Need to better understand organization of diagnosis codes

-- Find Aldo visits
DROP TABLE aldo_dx;
CREATE TABLE aldo_dx NOLOGGING AS
select DISTINCT /*+ALL_ROWS*/
  CODE,
  PI.EMPI,
  E.ENC_DATE,
  E.PK_ENCOUNTER_ID,
  E.VISIT_NUMBER_NUM,
  Dx.CODING_DATE, Dx.PRIMARY_YN, DX_SEQUENCE
from ODS.R_Codes_Diagnosis CD
JOIN ODS.Diagnosis Dx
  ON CD.PK_Dx_Code_ID = Dx.FK_Dx_Code_ID
JOIN ODS.ENCOUNTER PARTITION (EPIC) E
  ON E.PK_Encounter_ID = Dx.FK_Encounter_ID
JOIN ODS.PATIENT P
     ON P.PK_PATIENT_ID = E.FK_PATIENT_ID
JOIN ODS.PATIENT_IDENTIFIERS PARTITION (EPIC) PI
    ON P.PK_PATIENT_ID = PI.FK_PATIENT_ID
where
  ((CODE_STANDARD_NAME = 'ICD-10'
  AND (CODE LIKE 'E26.0%') OR CODE = 'E26.9')
  OR 
  (CODE_STANDARD_NAME = 'ICD9'
    AND CODE IN ('255.10', '255.11', '255.1', '255.12')))
  AND PI.Identifier_Name = 'EMPI'
  AND P.SOURCE_CODE = 'EPIC'
ORDER BY EMPI, ENC_DATE;
--DX TYPE field for inpatient

select count(distinct(EMPI)) from aldo_dx;  -- #776
select * from aldo_dx
order by EMPI, ENC_DATE; -- #23794


select count(distinct(EMPI)) 
from aldo_dx
where ENC_DATE > to_date('01-Jan-2014', 'dd-mon-yyyy');  -- #474

DROP TABLE aldo_dx_calc;
CREATE TABLE aldo_dx_calc NOLOGGING AS
select EMPI,
  count(*) as count
from aldo_dx
group by EMPI;

select * from aldo_dx_calc;

-- Filter out more specific cases
DROP TABLE aldo_dx2;
CREATE TABLE aldo_dx2 NOLOGGING AS
select aldo_dx.*
from aldo_dx
join aldo_dx_calc
  ON aldo_dx_calc.EMPI = aldo_dx.EMPI
where COUNT > 1 
and
aldo_dx.ENC_DATE >= to_date('01-Jan-2005', 'dd-mon-yyyy')  
;

select * from aldo_dx2 order by ENC_DATE;

-- When switch to ICD10
with 
c1_table
AS
(
select 
substr(CODE, 1,1) as C1,
CODING_DATE
from aldo_dx2
) 
select C1,
min(CODING_DATE),
max(CODING_DATE)
from c1_table
group by C1;

select count(distinct(EMPI)) --584
from aldo_dx2
--where ENC_DATE > to_date('01-Jan-2014', 'dd-mon-yyyy');  -- #393
;
-- Filter out more specific cases
DROP TABLE aldo_dx2;
CREATE TABLE aldo_dx2 NOLOGGING AS
select aldo_dx.*
from aldo_dx
join aldo_dx_calc
  ON aldo_dx_calc.EMPI = aldo_dx.EMPI
where COUNT > 1;

select * from aldo_dx2;
select count(distinct(EMPI)) 
from aldo_dx2; --584


--Find first dx for each patient
DROP TABLE aldo_dx_n;
CREATE TABLE aldo_dx_n NOLOGGING AS
select EMPI, 
  MIN(ENC_DATE) as first_dx,
  COUNT(DISTINCT(ENC_DATE)) as n_encs
from aldo_dx2
group by EMPI;

select count(*) from aldo_dx_n; --584
select * from aldo_dx_n;

--Find encounters before aldo_dx
DROP TABLE aldo_predx;
CREATE TABLE aldo_predx NOLOGGING AS
select distinct
  adn.*,
  E.ENC_DATE,
  E.PK_ENCOUNTER_ID,
  E.VISIT_NUMBER_NUM,
  ET.ENC_TYPE_MASTER_DESCRIPTION
from aldo_dx_n adn
JOIN ODS.PATIENT_IDENTIFIERS PARTITION (EPIC) PI
      ON PI.EMPI = adn.EMPI
JOIN ODS.PATIENT P      
      ON P.PK_PATIENT_ID = PI.FK_PATIENT_ID
JOIN ODS.ENCOUNTER PARTITION (EPIC) E
     ON P.PK_PATIENT_ID = E.FK_PATIENT_ID
join R_Encounter_Type ET
  ON E.FK_Enc_type_ID = ET.PK_Enc_Type_ID
WHERE E.ENC_DATE < adn.first_dx
    AND P.SOURCE_CODE = 'EPIC'
order by adn.empi, e.enc_date;

select * from aldo_predx;
select * from aldo_predx apdx
join ODS.ENCOUNTER PARTITION(EPIC) E
  ON apdx.PK_ENCOuNTER_ID = E.PK_ENCOUNTER_ID
join R_Encounter_Type ET
  ON E.FK_Enc_type_ID = ET.PK_Enc_Type_ID
where rownum < 10;

--Count earlier visits
DROP TABLE aldo_predx_n;
CREATE TABLE aldo_predx_n NOLOGGING AS
select EMPI, 
  MIN(ENC_DATE) as first_visit,
  FIRST_DX,
  MAX(FIRST_DX - ENC_DATE)/365.25 as years_predx,
  COUNT(DISTINCT(ENC_DATE)) as n_predx_encs
from aldo_predx
group by EMPI, FIRST_DX;


select * from aldo_predx_n;
select  
min(N_PREDX_ENCS), median(N_PREDX_ENCS), max(N_PREDX_ENCS) from aldo_predx_n;

select count(*) from aldo_predx_n; --569
select count(*) --380
from
(
select *
from aldo_predx_n
where N_PREDX_ENCS > 2 AND YEARS_PREDX > 2
)
;
select count(*) from
(select distinct 
    aldo_predx_n.*,
      Patient_Identifier
from aldo_predx_n
join aldo_dx
  ON aldo_dx.EMPI = aldo_predx_labs_n.EMPI
where YEARS_PREDX > 2 and N_PREDX_LABS > 2);


--Look for predx labs (Need to specify a little better)
DROP TABLE aldo_predx_labs;

CREATE TABLE aldo_predx_labs NOLOGGING AS
select /*+ALL_ROWS*/ EMPI, 
    FIRST_DX,
    ENC_DATE, 
    O.ORDER_NAME, O.ORDER_START_DATE, Ores.RESULT_VALUE, RI.*
from aldo_predx apdx
JOIN ODS.ORDERS PARTITION (EPIC) O
   ON apdx.PK_ENCOUNTER_ID = O.FK_ENCOUNTER_ID
JOIN ODS.R_ORDER_TYPE OT
    ON OT.PK_ORDER_TYPE_ID = O.FK_ORDER_TYPE_ID
JOIN ODS.Order_Result PARTITION(EPIC) ORes
    ON ORes.FK_Order_ID = O.PK_Order_ID
JOIN ODS.R_RESULT_ITEM RI
    ON RI.PK_Result_Item_ID = ORes.FK_Result_Item_ID
WHERE OT.ORDER_TYPE_CODE = 'Lab'
;
--AND
  --ORes......STATUS = 'FINAL' ????
  --LOINC_CODE = '2823-3';   
   
select count(*) from aldo_predx_labs; #126218
select * from aldo_predx_labs;
   
--Count earlier labs (probably should focus on non-acute labs...)
DROP TABLE aldo_predx_labs_n;
CREATE TABLE aldo_predx_labs_n NOLOGGING AS
select EMPI, 
  MIN(ORDER_START_DATE) as first_lab,
  FIRST_DX,
  (MAX(FIRST_DX - ORDER_START_DATE)/365.25) as years_predx,
  COUNT(DISTINCT(ORDER_START_DATE)) as n_predx_labs
from aldo_predx_labs
group by EMPI, FIRST_DX;

select count(*) from aldo_predx_labs_n; #427
select count(*) from
(select distinct 
    aldo_predx_labs_n.*,
      Patient_Identifier
from aldo_predx_labs_n
join aldo_dx
  ON aldo_dx.EMPI = aldo_predx_labs_n.EMPI
where YEARS_PREDX > 2 and N_PREDX_LABS > 2);

select
min(N_PREDX_labs), median(N_PREDX_labs), max(N_PREDX_labs) from aldo_predx_labs_n;


-- Look for vital signs
DROP TABLE aldo_predx_vitals;
CREATE TABLE aldo_predx_vitals NOLOGGING AS
select /*+ALL_ROWS*/ apdx.*, 
        BP_Diastolic,
    BP_Systolic
from aldo_predx apdx
JOIN ODS.ENCOUNTER PARTITION(EPIC) EVS
  ON apdx.PK_ENCOUNTER_ID = EVS.PK_ENCOUNTER_ID
--  AND ENC_TYPE_MASTER_DESCRIPTION = 'OFFICE VISITS';
  AND BP_SYSTOLIC IS NOT NULL;

select count(*) from aldo_predx_vitals; #8442
select * from aldo_predx_vitals;

--Count earlier BPs 
DROP TABLE aldo_predx_vitals_n;
CREATE TABLE aldo_predx_vitals_n NOLOGGING AS
select EMPI, 
  MIN(ENC_DATE) as first_vital,
  FIRST_DX,
  (MAX(FIRST_DX - ENC_DATE)/365.25) as years_predx,
  COUNT(DISTINCT(ENC_DATE)) as n_predx_labs
from aldo_predx_vitals
group by EMPI, FIRST_DX;

select count(*) from aldo_predx_vitals_n; #489
select count(*) from (
select distinct 
    aldo_predx_vitals_n.*,
      Patient_Identifier
from aldo_predx_vitals_n
join aldo_dx
  ON aldo_dx.EMPI = aldo_predx_vitals_n.EMPI
where YEARS_PREDX > 2 and N_PREDX_LABS > 2);

/*DROP TABLE aldo_predx_vitals;
CREATE TABLE aldo_predx_vitals NOLOGGING AS
select EMPI, 
    FIRST_DX,
    ENC_DATE
   -- EVS.*
from aldo_predx apdx
left outer JOIN ODS.Encounter_Vital_Sign PARTITION(EPIC) EVS
  ON apdx.PK_ENCOUNTER_ID = EVS.FK_ENCOUNTER_ID
  where FK_VITAL_SIGN_ID IN (450022, 3018);

select * from ods.r_vital_sign
where rownum < 100
and source_code = 'EPIC'
and (UPPER(VITAL_CODE) LIKE '%BP%' OR UPPER(VITAL_DESCRIPTION) LIKE '%pressure%');*/

--Look for meds
DROP TABLE aldo_predx_meds;
CREATE TABLE aldo_predx_meds NOLOGGING AS
select /*+ALL_ROWS*/ apdx.*, 
        OM.Dose,
        OM.Frequency_Name,
        --M.SIMPLE_GENERIC_NAME,
        M.*
from aldo_predx apdx
JOIN ODS.ORDERS PARTITION(EPIC) O
  ON apdx.PK_ENCOUNTER_ID = O.FK_ENCOUNTER_ID
JOIN ODS.ORDER_MED OM
  ON O.PK_Order_ID = OM.FK_Order_ID
JOIN ODS.R_Medication M
  ON OM.FK_Medication_ID = M.PK_Medication_ID
WHERE --THERAPEUTIC_CLASS = 'Cardiovascular Agents'
  PHARMACY_CLASS IN ('Antihypertensive', 'Calcium blockers', 'Beta blockers', 'Diuretics');
-- PHARMACY_SUBCLASS = 'Potassium'
--How to get meaningful information out of EPIC Med orders
--Medication list look in the full name
-- R_Meidcaiton to get dictionary
-- Use PK's from R_Medicaiton OR join to temp table
-- dosing from ORDER_MED
select * from orders partition(EPIC) where PK_ORDER_ID = 2410745064;
select * from order_med where rownum < 50;

--STATUS (Labs from ORder_Result_table [EPIC: FINAL, CERNER: VERIFIED/AUTOVERIFIED]

select count(*) from aldo_predx_meds; #8945
select * from aldo_predx_meds
order by empi, enc_date, simple_generic_name;

--Count earlier meds 
DROP TABLE aldo_predx_meds_n;
CREATE TABLE aldo_predx_meds_n NOLOGGING AS
select EMPI, 
  MIN(ENC_DATE) as first_med,
  FIRST_DX,
  (MAX(FIRST_DX - ENC_DATE)/365.25) as years_predx,
  COUNT(DISTINCT(ENC_DATE)) as n_predx_meds
from aldo_predx_meds
group by EMPI, FIRST_DX;

select * from aldo_predx_meds_n; #489
select distinct 
    aldo_predx_meds_n.*,
      Patient_Identifier
from aldo_predx_meds_n
join aldo_dx
  ON aldo_dx.EMPI = aldo_predx_meds_n.EMPI
where YEARS_PREDX > 2 and N_PREDX_meds > 2;


DROP TABLE aldo_predx_k;
CREATE TABLE aldo_predx_k NOLOGGING AS
select /*+ALL_ROWS*/ apdx.*, 
        OM.Dose,
        OM.Frequency_Name,
        M.SIMPLE_GENERIC_NAME
        
from aldo_predx apdx
JOIN ODS.ORDERS PARTITION(EPIC) O
  ON apdx.PK_ENCOUNTER_ID = O.FK_ENCOUNTER_ID
JOIN ODS.ORDER_MED OM
  ON O.PK_Order_ID = OM.FK_Order_ID
JOIN ODS.R_Medication M
  ON OM.FK_Medication_ID = M.PK_Medication_ID
WHERE --THERAPEUTIC_CLASS = 'Cardiovascular Agents'
  PHARMACY_SUBCLASS = 'Potassium';
  
  --Count earlier meds 
DROP TABLE aldo_predx_k_n;
CREATE TABLE aldo_predx_k_n NOLOGGING AS
select EMPI, 
  MIN(ENC_DATE) as first_med,
  FIRST_DX,
  (MAX(FIRST_DX - ENC_DATE)/365.25) as years_predx,
  COUNT(DISTINCT(ENC_DATE)) as n_predx_meds
from aldo_predx_k
group by EMPI, FIRST_DX;

select * from aldo_predx_k;_n; #250
select distinct 
    aldo_predx_k_n.*,
      Patient_Identifier
from aldo_predx_k_n
join aldo_dx
  ON aldo_dx.EMPI = aldo_predx_k_n.EMPI
where YEARS_PREDX > 2 and N_PREDX_meds > 2;



--Hypertension
DROP TABLE htn_dx;
CREATE TABLE htn_dx NOLOGGING AS;
select /*+FIRST_ROWS*/
  CODE,
  PI.EMPI,
  E.ENC_DATE,
  E.PK_ENCOUNTER_ID,
  Dx.CODING_DATE, Dx.PRIMARY_YN, DX_SEQUENCE
from ODS.R_Codes_Diagnosis CD
JOIN ODS.Diagnosis Dx
  ON CD.PK_Dx_Code_ID = Dx.FK_Dx_Code_ID
JOIN ODS.ENCOUNTER PARTITION (EPIC) E
  ON E.PK_Encounter_ID = Dx.FK_Encounter_ID
JOIN ODS.PATIENT P
     ON P.PK_PATIENT_ID = E.FK_PATIENT_ID
JOIN ODS.PATIENT_IDENTIFIERS PARTITION (EPIC) PI
    ON P.PK_PATIENT_ID = PI.FK_PATIENT_ID
where
 ((CODE_STANDARD_NAME = 'ICD-10'
  AND (SUBSTR(CODE,1,2) = 'I1'))
  OR 
  (CODE_STANDARD_NAME = 'ICD9'
    AND (SUBSTR(CODE,1,2) = '40')))
  AND PI.Identifier_Name = 'EMPI'
  AND P.SOURCE_CODE = 'EPIC'
  AND E.ENC_DATE >= to_date('01-Jan-2014', 'dd-mon-yyyy')
  --AND PATIENT_CLASS = 'Outpatient'
;

select count(distinct(EMPI)) from aldo_dx;  -- #776
select * from aldo_dx
order by EMPI, ENC_DATE; -- #23794

