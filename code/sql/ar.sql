-- Preliminary attempts at predicting aldosterone to renin lab results
---- Hihg aldosterone to renin values are indicative of Primary Aldosteronism (PA)
-- Using information other than aldosterone labs as inputs for prediction
---- This includes orders, medications, and medications
-- pulling aldosteronism specific lab data 
--Add to RAR
DROP TABLE RARv3;
CREATE TABLE RARv3 NOLOGGING AS
select /*+ALL_ROWS*/
PI.EMPI,
PI.PATIENT_IDENTIFIER as MRN,
E.PK_ENCOUNTER_ID,
E.ENC_DATE,
O.PK_ORDER_ID,
O.ORDER_NAME,
O.ORDER_DATE,
O.ORDER_START_DATE,
O.ORDER_GROUP,
OI.ORDER_ITEM_CODE,
OI.ORDER_ITEM_DESCRIPTION,
ORes.PK_ORDER_RESULT_ID,
ORes.RESULT_DATE,
ORes.Result_Value,
ORes.RESULT_TEXT,
ORes.UNIT_OF_MEASURE,
ORes.RESULT_RESOURCE,
ORes.RESULT_STATUS,
OResI.Result_Item_Code,
OResI.RESULT_ITEM_DESCRIPTION,
OResI.LOINC_CODE,
OP.PK_ORDER_PERFORMED_ID,
OP.TASK_DESCRIPTION,
OP.PERFORMED_DATE,
RP.NAME Ordering_Prov,
RPA.NAME Admitting_Prov,
L.MASTER_LOCATION_DESCRIPTION
from ODS.Orders PARTITION(EPIC) O
JOIN ODS.R_Order_Item OI
  ON O.FK_Order_Item_ID = OI.PK_Order_Item_ID
JOIN ODS.ENCOUNTER PARTITION(EPIC) E
  ON E.PK_Encounter_ID = O.FK_Encounter_ID
JOIN ODS.Patient P
  ON E.FK_Patient_ID = P.PK_Patient_ID
LEFT OUTER JOIN ODS.Patient_Identifiers PI
  ON P.PK_Patient_ID = PI.FK_Patient_ID
left outer JOIN ODS.Order_Result ORes
  ON O.PK_Order_ID = ORes.FK_Order_ID
left outer JOIN ODS.R_RESULT_ITEM OResI
  ON ORes.FK_RESULT_ITEM_ID = OResI.PK_Result_Item_ID
LEFT OUTER JOIN ODS.Order_Performed OP
  ON O.PK_Order_ID = OP.FK_Order_ID
LEFT OUTER JOIN ODS.R_Provider RP
  ON RP.PK_Provider_ID = O.FK_Provider_ID
LEFT OUTER JOIN ODS.R_Provider RPA
  ON RPA.PK_Provider_ID = E.FK_ADMITTING_Provider_ID
left OUTER JOIN ODS.R_LOCATION L
   ON E.FK_LOCATION_ID = L.PK_LOCATION_ID
LEFT OUTER JOIN ODS.R_Gender RG
  ON P.FK_Gender_ID = RG.PK_Gender_ID
where
  OI.ORDER_ITEM_MASTER_PRIMARY_TYPE = 'LABORATORY' -- Does this drop anything?
  AND (upper(OI.ORDER_ITEM_DESCRIPTION) LIKE '%ALDOSTERONE%' OR upper(OI.ORDER_ITEM_DESCRIPTION) LIKE '%RENIN%')
    AND (O.ORDER_NAME != 'ANTI MULLERIAN HORMONE')
    AND (OresI.RESULT_ITEM_CODE != 'ANTI MULLERIAN HORMONE')
  AND PI.Identifier_Facility = 'HUP'
  AND ORDER_START_DATE < to_date('01-Jul-2017', 'dd-mon-yyyy')
;
select * from RARv3;
select count(*) from RARv3; -- 21208
select count(distinct(EMPI)) from RARv3; -- 7137
select count(distinct(PK_ORDER_RESULT_ID)) from RARv3; -- 21208
select count(distinct(PK_ORDER_PERFORMED_ID)) from RARv3; -- 16976
select count(distinct(PK_ORDER_ID)) from RARv3; -- 16976
-- Get DB patients
-- First load in DB .csv -- 488 rows with 458 unique MRNs
-- Find all MRNs for EMPI
DROP TABLE RAR_db_to_empi;
CREATE TABLE RAR_db_to_empi NOLOGGING AS
SELECT /*+ALL_ROWS*/
  distinct
HOSP_NUM,
  CAST(HOSP_NUM AS INTEGER) as HOSP_NUM_NUM
--  CAST(HOSP_NUM_NUM AS VARCHAR) as HOSP_NUM_CHAR,
  , PI_1.EMPI_NUM,
  CAST(PI_1.EMPI_NUM AS VARCHAR(20)) as EMPI,
  PI_1.IDENTIFIER_NAME,
  PI_1.IDENTIFIER_FACILITY,
  PI_1.SOURCE_CODE
  from AVS_DB
left join ODS.PATIENT_IDENTIFIERS PI_1
    ON AVS_DB.HOSP_NUM = PI_1.PATIENT_IDENTIFIER
  where PI_1.IDENTIFIER_FACILITY = 'HUP'
  AND PI_1.ACTIVE_YN = 1
  AND PI_1.EMPI_NUM IS NOT NULL
  AND PI_1.SOURCE_CODE = 'EPIC'
;
select count(*) from RAR_db_to_empi; --458
select * from RAR_db_to_empi;
select count(distinct(HOSP_NUM)) from RAR_db_to_empi; --458
select count(distinct(EMPI_NUM)) from RAR_db_to_empi; --458
-- Find patients with at least one hyperaldo dx code
DROP TABLE rar_dx;
CREATE TABLE rar_dx NOLOGGING AS
select /*+ALL_ROWS*/
  PI.EMPI,
  CD.CODE,
  CD.CODE_STANDARD_NAME,
  E.PATIENT_MASTER_CLASS,
  E.ENC_DATE
from ODS.R_Codes_Diagnosis CD
left JOIN ODS.Diagnosis Dx
  ON CD.PK_Dx_Code_ID = Dx.FK_Dx_Code_ID
left JOIN ODS.ENCOUNTER PARTITION (EPIC) E
  ON E.PK_Encounter_ID = Dx.FK_Encounter_ID
left JOIN ODS.PATIENT P
     ON P.PK_PATIENT_ID = E.FK_PATIENT_ID
left JOIN ODS.PATIENT_IDENTIFIERS PARTITION (EPIC) PI
    ON P.PK_PATIENT_ID = PI.FK_PATIENT_ID
where
  ((CODE_STANDARD_NAME = 'ICD-10'
    AND (SUBSTR(CODE, 1, 3) = 'E26'))
    OR
   (CODE_STANDARD_NAME = 'ICD9'
     AND (SUBSTR(CODE, 1, 5) = '255.1')))
  AND PI.Identifier_Facility = 'HUP'
  AND P.SOURCE_CODE = 'EPIC'
  AND E.ENC_DATE < to_date('01-Jul-2017', 'dd-mon-yyyy')
;
select count(*) from RAR_dx; --6120
select * from RAR_dx;
select count(distinct(EMPI)) from RAR_dx; --879
-- TODO: Look for these results in PARTITION(CERNER) and look for the times that Nick was concerned about....
select * from RARv3;
select count(*) from RARv3;
-- Find set of EMPIs
DROP TABLE RAR_pts;
CREATE TABLE RAR_pts NOLOGGING AS
select /*+ALL_ROWS*/
  distinct
  RARv3.EMPI
from RARv3
;
select count(*) from RAR_pts; --7137
-- Find full set of EMPIs
DROP TABLE RAR_FULL_PTS;
CREATE TABLE RAR_pts_full NOLOGGING AS
  SELECT EMPI
    from RAR_pts
UNION
    SELECT EMPI
      from RAR_db_to_empi
UNION
      SELECT EMPI
        from RAR_dx
;
select count(*) from RAR_FULL_PTS; --7235 --7438
select count(distinct(EMPI)) from RAR_FULL_PTS; --7235 (+db) -- 7438 (+db + dx)
select * from RAR_FULL_PTS;
-- Find all RAR pts encounters
DROP TABLE RAR_pts_enc;
CREATE TABLE RAR_pts_enc NOLOGGING AS
SELECT /*+ALL_ROWS*/
  PI.EMPI,
  E.PK_ENCOUNTER_ID,
  E.ENC_DATE,
  E.HAR_NUMBER,
  E.PATIENT_MASTER_CLASS,
  E.ADMIT_SOURCE,
  E.ENCOUNTER_SUB_TYPE,
  E.BP_SYSTOLIC,
  E.BP_DIASTOLIC,
  E.SOURCE_LAST_UPDATE_DATE as E_SOURCE_LAST_UPDATE,
  RP_admit.ROLE Role_admit,
  RP_admit.DISCIPLINE discipline_admit,
  RET.ENC_TYPE_CODE,
  RET.ENC_TYPE_MASTER_CODE,
  RET.ENC_TYPE_MASTER_DESCRIPTION,
  RL.MASTER_LOCATION_CODE,
  RL.MASTER_LOCATION_DESCRIPTION,
  RL.MASTER_LOCATION_FACILITY_YN, -- hospital?
  RL.MASTER_LOCATION_CLINIC_YN, -- clinic?,
  RL.MASTER_LOCATION_ENTITY, --CCA or CPUP
  RS.SERVICE_MASTER_CODE,
  RS.SERVICE_MASTER_DESCRIPTION
from RAR_FULL_PTS
  JOIN ODS.PATIENT_IDENTIFIERS PARTITION (EPIC) PI
    ON RAR_FULL_PTS.EMPI = PI.EMPI
  JOIN ODS.PATIENT P
    ON P.PK_PATIENT_ID = PI.FK_PATIENT_ID
  left JOIN ODS.ENCOUNTER PARTITION (EPIC) E
    ON P.PK_PATIENT_ID = E.FK_PATIENT_ID
  left JOIN ODS.R_PROVIDER RP_admit
    ON RP_admit.PK_PROVIDER_ID = E.FK_ADMITTING_PROVIDER_ID
  left JOIN ODS.R_ENCOUNTER_TYPE RET
    ON RET.PK_ENC_TYPE_ID = E.FK_ENC_TYPE_ID
  left JOIN ODS.R_LOCATION RL
    ON RL.PK_LOCATION_ID = E.FK_LOCATION_ID
  left JOIN ODS.R_SERVICE RS
    ON RS.PK_SERVICE_ID = E.FK_SERVICE_ID
where
  PI.Identifier_Facility = 'HUP'
  AND P.SOURCE_CODE = 'EPIC'
  AND E.ENC_DATE < to_date('01-Jul-2017', 'dd-mon-yyyy')
;
select * from RAR_pts_enc;
select count(distinct(EMPI)) from RAR_pts_enc; --7137
select count(*) from RAR_pts_enc; -- 2199089
--select count(distinct *) from RAR_pts_enc;
select count(distinct(PK_ENCOUNTER_ID)) from RAR_pts_enc; --2199089 GOOD
-- Get demographics
DROP TABLE RAR_pts_demo;
CREATE TABLE RAR_pts_demo NOLOGGING AS
SELECT /*+ALL_ROWS*/
  PI.EMPI,
  P.PK_PATIENT_ID,
  RG.GENDER_MASTER_CODE,
  P.Birth_Date,
  RR.RACE_MASTER_CODE,
  RR.RACE_MASTER_HISPANIC_YN
from RAR_FULL_PTS
  JOIN ODS.PATIENT_IDENTIFIERS PARTITION (EPIC) PI
    ON RAR_FULL_PTS.EMPI = PI.EMPI
  JOIN ODS.PATIENT P
    ON P.PK_PATIENT_ID = PI.FK_PATIENT_ID
  LEFT OUTER JOIN ODS.R_Gender RG
    ON P.FK_Gender_ID = RG.PK_Gender_ID
  LEFT OUTER JOIN ODS.R_RACE RR
    ON P.FK_RACE_ID = RR.PK_RACE_ID
WHERE
  PI.Identifier_Facility = 'HUP'
  AND P.SOURCE_CODE = 'EPIC'
;
select * from RAR_pts_demo;
select count(*) from RAR_pts_demo; --7141
select count(distinct(EMPI)) from RAR_pts_demo; --7137
--select count(distinct *) from RAR_pts_demo;
select count(distinct(PK_PATIENT_ID)) from RAR_pts_demo; --7141
-- Get all labs for EMPIs
DROP TABLE RAR_pts_labs;
CREATE TABLE RAR_pts_labs NOLOGGING AS
select /*+ALL_ROWS*/
PI.EMPI,
--PI.PATIENT_IDENTIFIER as MRN,
E.PK_ENCOUNTER_ID,
E.ENC_DATE,
E.HAR_NUMBER,
O.PK_ORDER_ID,
O.ORDER_NAME,
--O.ORDER_DATE,
O.ORDER_START_DATE,
O.ORDER_GROUP,
O.SOURCE_LAST_UPDATE_DATE as O_SOURCE_LAST_UPDATE,
OI.ORDER_ITEM_CODE,
--OI.ORDER_ITEM_DESCRIPTION,
ORes.PK_ORDER_RESULT_ID,
ORes.RESULT_DATE,
ORes.Result_Value,
ORes.RESULT_TEXT,
ORes.UNIT_OF_MEASURE,
ORes.RESULT_RESOURCE,
ORes.RESULT_STATUS,
ORes.SOURCE_LAST_UPDATE_DATE as ORes_SOURCE_LAST_UPDATE,
OResI.Result_Item_Code,
--OResI.RESULT_ITEM_DESCRIPTION,
OResI.LOINC_CODE,
--OP.PK_ORDER_PERFORMED_ID,
--OP.TASK_DESCRIPTION,
--OP.PERFORMED_DATE,
--OP.SOURCE_LAST_UPDATE_DATE as OP_SOURCE_LAST_UPDATE,
RP.NAME Ordering_Prov
from RAR_FULL_PTS
JOIN ODS.Patient_Identifiers PI
  ON RAR_FULL_PTS.EMPI = PI.EMPI
JOIN ODS.Patient P
  ON P.PK_Patient_ID = PI.FK_Patient_ID
JOIN ODS.ENCOUNTER PARTITION(EPIC) E
  ON E.FK_Patient_ID = P.PK_Patient_ID
JOIN ODS.Orders PARTITION(EPIC) O
  ON E.PK_Encounter_ID = O.FK_Encounter_ID
left outer JOIN ODS.R_Order_Item OI
  ON O.FK_Order_Item_ID = OI.PK_Order_Item_ID
left outer JOIN ODS.Order_Result ORes
  ON O.PK_Order_ID = ORes.FK_Order_ID
left outer JOIN ODS.R_RESULT_ITEM OResI
  ON ORes.FK_RESULT_ITEM_ID = OResI.PK_Result_Item_ID
--LEFT OUTER JOIN ODS.Order_Performed OP
--  ON O.PK_Order_ID = OP.FK_Order_ID
LEFT OUTER JOIN ODS.R_Provider RP
  ON RP.PK_Provider_ID = O.FK_Provider_ID
where
  --OI.ORDER_ITEM_MASTER_PRIMARY_TYPE = 'LABORATORY' -- Does this drop anything?
  O.FK_ORDER_TYPE_ID IN (108, 413, 359) -- EPIC "Lab, Lab Only, Lab Panel"
  AND PI.Identifier_Facility = 'HUP'
  AND P.SOURCE_CODE = 'EPIC'
  AND ORDER_START_DATE < to_date('01-Jul-2017', 'dd-mon-yyyy')
;
select * from R_ORDER_TYPE
where upper(R_ORDER_TYPE.ORDER_TYPE_CODE) LIKE '%LAB%';
/*
108
10
169
31
413
359
*/
select * from RAR_pts_labs;
select count(*) from RAR_pts_labs; -- 6464229
select count(distinct(EMPI)) from RAR_pts_labs; -- 7137
--select count(distinct *) from RAR_pts_labs;
select count(distinct(PK_ORDER_RESULT_ID)) from RAR_pts_labs; --5420303   Why is this field duplicated?
select count(distinct(PK_ORDER_ID)) from RAR_pts_labs; -- 2163797
-- Get all DX codes for EMPI
DROP TABLE RAR_pts_dx;
CREATE TABLE RAR_pts_dx NOLOGGING AS
select /*+ALL_ROWS*/
  PI.EMPI,
  --PI.PATIENT_IDENTIFIER as MRN, --HUP
  CD.CODE,
  CD.CODE_STANDARD_NAME,
  CD.DESCRIPTION,
  Dx.PK_DX_ID,
  Dx.DX_TYPE,
  Dx.PRIMARY_YN, DX.DX_SEQUENCE,
  Dx.COMMENTS,
  Dx.CODING_DATE,
  Dx.SOURCE_CODE,
  Dx.SOURCE_LAST_UPDATE_DATE,
  E.PK_ENCOUNTER_ID,
  E.PATIENT_MASTER_CLASS,
  --E.ADMIT_SOURCE,
  E.ENC_DATE,
  E.HAR_NUMBER
  --E.ENCOUNTER_SUB_TYPE
from RAR_FULL_PTS
left JOIN ODS.PATIENT_IDENTIFIERS PARTITION (EPIC) PI
  ON RAR_FULL_PTS.EMPI = PI.EMPI
JOIN ODS.PATIENT P
  ON P.PK_PATIENT_ID = PI.FK_PATIENT_ID
JOIN ODS.ENCOUNTER PARTITION (EPIC) E
  ON P.PK_PATIENT_ID = E.FK_PATIENT_ID
JOIN ODS.Diagnosis Dx
  ON E.PK_Encounter_ID = Dx.FK_Encounter_ID
left join ODS.R_Codes_Diagnosis CD
  ON CD.PK_Dx_Code_ID = Dx.FK_Dx_Code_ID
where
  PI.Identifier_Facility = 'HUP'
  AND P.SOURCE_CODE = 'EPIC'
  -- AND Dx.SOURCE_CODE = 'EPIC'  -- ?Do we need this?
  AND E.ENC_DATE < to_date('01-Jul-2017', 'dd-mon-yyyy')
;
select * from RAR_pts_dx;
select count(*) from RAR_pts_dx; -- 1567314
select count(distinct(EMPI)) from RAR_pts_dx; -- 7077
--select count(distinct *) from RAR_pts_dx;
select count(distinct(PK_DX_ID)) from RAR_pts_dx; -- 1567314
select count(distinct(PK_ENCOUNTER_ID)) from RAR_pts_dx; --601423
-- Find all MRNs for EMPI
DROP TABLE RAR_empi_mrn;
CREATE TABLE RAR_empi_mrn NOLOGGING AS
select /*+ALL_ROWS*/
  RAR_FULL_PTS.EMPI,
  PI.IDENTIFIER_NAME,
  PI.IDENTIFIER_FACILITY,
  lpad(PI.PATIENT_IDENTIFIER, 9, 0) as MRN,
  PI.PATIENT_IDENTIFIER_NUM,
  PI.EMPI_NUM,
  ACTIVE_YN
  from RAR_FULL_PTS
JOIN ODS.PATIENT_IDENTIFIERS PI
    ON PI.EMPI = RAR_FULL_PTS.EMPI
WHERE
  IDENTIFIER_NAME LIKE '%MRN%'
  AND PI.IDENTIFIER_FACILITY = 'HUP'
  AND PI.ACTIVE_YN = 1
  AND PI.EMPI_NUM IS NOT NULL
  AND PI.SOURCE_CODE = 'EPIC'
;
select * from RAR_empi_mrn;
select count(*) from RAR_empi_mrn; --7443
select count(Distinct(EMPI)) from RAR_empi_mrn; --7438
select count(Distinct(EMPI)) from RAR_FULL_PTS; --7438
-- Look for radiology
select * from R_ORDER_TYPE
where (upper(R_ORDER_TYPE.ORDER_TYPE_CODE) LIKE '%IMAG%' OR upper(R_ORDER_TYPE.ORDER_TYPE_CODE) LIKE '%RAD%');
-- PK_ORDER_TYPE_ID IN (106, 32)
select
  PK_ORDER_ITEM_ID,
  ORDER_ITEM_DESCRIPTION
from R_ORDER_ITEM
WHERE
  ORDER_ITEM_DESCRIPTION LIKE 'CT %'
  AND ORDER_ITEM_DESCRIPTION LIKE '%ABD%'
  AND ORDER_ITEM_MASTER_PRIMARY_TYPE = 'IMAGING'
  AND SOURCE_CODE = 'EPIC'
;
/*
1140352, 1140354, 1140339, 1140427, 1141786, 1141789, 1142080, 1142083, 1141959, 1141987, 1231118, 1231126, 1231133, 1139006, 1132532, 934619, 934621, 602454, 602935, 602940, 598215, 602486, 602155, 603531, 603005, 603007, 603216, 585034, 585332, 585251
1140352 CT ABDOMEN W AND WO IV CONTRAST
1140354 CT ABDOMEN/PELVIS W IV CONTRAST
1140339 CT ABDOMEN W IV CONTRAST
1140427 CT ABDOMEN/PELVIS W AND WO IV CONTRAST
1141786 CT ABDOMEN WO  IV CONTRAST
1141789 CT ABDOMEN/PELVIS WO IV CONTRAST
1142080 CT ABDOMEN WO  IV CONTRAST POST PROCESSING
1142083 CT ABDOMEN/PELVIS WO IV CONTRAST POST PROCESSING
1141959 CT CHEST/ABDOMEN WO IV CONTRAST POST PROCESSING
1141987 CT CHEST/ABDOMEN/PELVIS WO IV CONTRAST POST PROCESSING
1231118 CT CHEST ABD PEL WO AND W (OS/N)
1231126 CT CHEST ABD PEL WO (OS/NON)
1231133 CT CHEST ABD PEL W (OS/NON)
1139006 CT ABD WITH & WITHOUT IV CONTRAST
1132532 CT ABDOMEN/PELVIS ANGIO W OR W AND WO IV CONTRAST
934619  CT ABDOMEN W/O IV CONTRAST  CARDIOVASCULAR
934621  CT ABDOMEN AND PELVIS W/O IV CONTRAST-CARDIOVASCULAR
602454  CT SCAN OF ABDOMEN & PELVIS W/ CONTRAST
602935  CT ABDOMEN ONLY W & WO IV CONTRAST (74170)
602940  CT ABDOMEN & PELVIS W & WO IV CONTRAST (74178)
598215  CT SCAN ABDOMEN/PELVIS PANEL
602486  CT SCAN OF ABDOMEN & PELVIS W/ & W/O CONTRAST
602155  CT SCAN OF ABDOMEN & PELVIS W/O CONTRAST
603531  CT ABDOMEN & PELVIS WO IV CONTRAST (74176)
603005  CT ABDOMEN ONLY W IV CONTRAST (74160)
603007  CT ABDOMEN & PELVIS W IV CONTRAST (74177)
603216  CT ABDOMEN ONLY WO IV CONTRAST (74150)
585034  CT SCAN OF ABDOMEN W/ & W/O CONTRAST
585332  CT SCAN OF ABDOMEN W/O CONTRAST
585251  CT SCAN OF ABDOMEN W/ CONTRAST
*/
DROP TABLE RAR_pts_rads;
CREATE TABLE RAR_pts_rads NOLOGGING AS
select /*+ALL_ROWS*/
PI.EMPI,
E.PK_ENCOUNTER_ID,
E.ENC_DATE,
E.HAR_NUMBER,
O.PK_ORDER_ID,
O.ORDER_NAME,
O.ORDER_START_DATE,
O.ORDER_GROUP,
O.SOURCE_LAST_UPDATE_DATE as O_SOURCE_LAST_UPDATE,
OI.ORDER_ITEM_CODE,
OI.ORDER_ITEM_DESCRIPTION,
ORes.PK_ORDER_RESULT_ID,
ORes.RESULT_DATE,
ORes.Result_Value,
ORes.RESULT_TEXT,
ORes.UNIT_OF_MEASURE,
ORes.RESULT_RESOURCE,
ORes.RESULT_STATUS,
ORes.SOURCE_LAST_UPDATE_DATE as ORes_SOURCE_LAST_UPDATE,
OResI.Result_Item_Code,
OResI.RESULT_ITEM_DESCRIPTION,
OResI.LOINC_CODE,
OP.PK_ORDER_PERFORMED_ID,
OP.TASK_DESCRIPTION,
OP.PERFORMED_DATE,
OP.SOURCE_LAST_UPDATE_DATE as OP_SOURCE_LAST_UPDATE,
RP.NAME Ordering_Prov
from RAR_FULL_PTS
JOIN ODS.Patient_Identifiers PI
  ON RAR_FULL_PTS.EMPI = PI.EMPI
JOIN ODS.Patient P
  ON P.PK_Patient_ID = PI.FK_Patient_ID
JOIN ODS.ENCOUNTER PARTITION(EPIC) E
  ON E.FK_Patient_ID = P.PK_Patient_ID
JOIN ODS.Orders PARTITION(EPIC) O
  ON E.PK_Encounter_ID = O.FK_Encounter_ID
left outer JOIN ODS.R_Order_Item OI
  ON O.FK_Order_Item_ID = OI.PK_Order_Item_ID
left outer JOIN ODS.Order_Result ORes
  ON O.PK_Order_ID = ORes.FK_Order_ID
left outer JOIN ODS.R_RESULT_ITEM OResI
  ON ORes.FK_RESULT_ITEM_ID = OResI.PK_Result_Item_ID
LEFT OUTER JOIN ODS.Order_Performed OP
  ON O.PK_Order_ID = OP.FK_Order_ID
LEFT OUTER JOIN ODS.R_Provider RP
  ON RP.PK_Provider_ID = O.FK_Provider_ID
where
  O.FK_ORDER_TYPE_ID IN (106, 32) -- EPIC and Theradoc rads/imaging
  AND O.FK_ORDER_ITEM_ID IN (1140352, 1140354, 1140339, 1140427, 1141786, 1141789, 1142080, 1142083, 1141959, 1141987, 1231118, 1231126, 1231133, 1139006, 1132532, 934619, 934621, 602454, 602935, 602940, 598215, 602486, 602155, 603531, 603005, 603007, 603216, 585034, 585332, 585251)
  AND PI.Identifier_Facility = 'HUP'
  AND P.SOURCE_CODE = 'EPIC'
  AND ORDER_START_DATE < to_date('01-Jul-2017', 'dd-mon-yyyy')
;
SELECT /*+FIRST ROWS*/
O.PK_ORDER_ID,
O.ORDER_NAME,
O.ORDER_START_DATE,
O.ORDER_GROUP,
O.SOURCE_LAST_UPDATE_DATE as O_SOURCE_LAST_UPDATE,
OI.ORDER_ITEM_CODE,
OI.ORDER_ITEM_DESCRIPTION,
O.SOURCE_CODE,
--   ORes.PK_ORDER_RESULT_ID,
-- ORes.RESULT_DATE,
-- ORes.Result_Value,
-- ORes.RESULT_TEXT,
-- ORes.UNIT_OF_MEASURE,
-- ORes.RESULT_RESOURCE,
-- ORes.RESULT_STATUS,
-- ORes.SOURCE_LAST_UPDATE_DATE as ORes_SOURCE_LAST_UPDATE,
-- OResI.Result_Item_Code,
-- OResI.RESULT_ITEM_DESCRIPTION,
-- OResI.LOINC_CODE
from ODS.Orders O
left outer JOIN ODS.R_Order_Item OI
  ON O.FK_Order_Item_ID = OI.PK_Order_Item_ID
-- left outer JOIN ODS.Order_Result ORes
--   ON O.PK_Order_ID = ORes.FK_Order_ID
-- left outer JOIN ODS.R_RESULT_ITEM OResI
--   ON ORes.FK_RESULT_ITEM_ID = OResI.PK_Result_Item_ID
-- LEFT OUTER JOIN ODS.Order_Performed OP
--   ON O.PK_Order_ID = OP.FK_Order_ID
WHERE
  O.FK_ORDER_TYPE_ID IN (106, 32) -- EPIC and Theradoc rads/imaging
  AND upper(O.ORDER_NAME) LIKE '%VENOUS SAMPLING%'
  AND ROWNUM < 50
;
select * from RAR_pts_rads;
select count(*) from RAR_pts_rads; -- 20811
select count(distinct(EMPI)) from RAR_pts_rads; -- 3140
--select count(distinct *) from RAR_pts_dx;
select count(distinct(PK_ORDER_ID)) from RAR_pts_rads; -- 20664
select count(distinct(PK_ENCOUNTER_ID)) from RAR_pts_rads; --14451
--======== STILL USING BELOW>>>>>?????? ===
-- pulling NON aldosterone specific lab information
DROP TABLE RAR_surround;
CREATE TABLE RAR_surround NOLOGGING AS
SELECT /*+ALL_ROWS*/
PI.EMPI,
PI.PATIENT_IDENTIFIER,
RAR_distinct.order_start_date as RAR_dt,
O.ORDER_NAME, 
O.ORDER_START_DATE, 
E.VISIT_NUMBER,
E.PK_ENCOUNTER_ID,
OI.ORDER_ITEM_CODE, 
OI.ORDER_ITEM_DESCRIPTION,
ORes.RESULT_DATE, 
ORes.Result_Value,
OResI.Result_Item_Code,
OP.PERFORMED_DATE,
abs((O.ORDER_START_DATE - RAR_distinct.ORDER_START_DATE)) as date_diff --days
FROM (select distinct empi, order_start_date
        from RARv2) RAR_distinct
join ODS.Patient_Identifiers PI
  ON PI.empi = RAR_distinct.empi
join ODS.Patient P
  ON P.PK_Patient_ID = PI.FK_Patient_ID
join ODS.Encounter E
  ON E.FK_Patient_ID = P.PK_Patient_ID
JOIN ODS.ORDERS PARTITION(EPIC) O
    ON E.PK_ENCOUNTER_ID = O.FK_ENCOUNTER_ID
JOIN ODS.R_Order_Item OI 
  ON O.FK_Order_Item_ID = OI.PK_Order_Item_ID
JOIN ODS.Order_Result ORes
  ON O.PK_Order_ID = ORes.FK_Order_ID
JOIN ODS.R_RESULT_ITEM OResI
  ON ORes.FK_RESULT_ITEM_ID = OResI.PK_Result_Item_ID
LEFT OUTER JOIN ODS.Order_Performed OP
  ON O.PK_Order_ID = OP.FK_Order_ID
WHERE
  PI.Identifier_Facility = 'HUP'
  AND OI.ORDER_ITEM_MASTER_PRIMARY_TYPE = 'LABORATORY'
  AND E.PATIENT_MASTER_CLASS = 'OUTPATIENT'
  AND abs((O.ORDER_START_DATE - RAR_distinct.ORDER_START_DATE)) <= 14
;
select count(*) from RAR_surround; --258624
select * from RAR_surround;
-- Lets get medications
DROP TABLE RAR_meds;
CREATE TABLE RAR_meds NOLOGGING AS
select /*+ALL_ROWS*/
        RAR_distinct.EMPI,
        E.ENC_DATE,
        E.PK_ENCOUNTER_ID,
        O.ORDER_START_DATE,
        O.ORDER_STOP_DATE,
        OM.Dose,
        OM.Frequency_Name,
        OM.QUANTITY,
        OM.REFILLS,
        O.ORDER_NAME,
        M.SIMPLE_GENERIC_NAME,
        M.GENERIC_NAME,
        M.FULL_NAME,
        M.AMOUNT,
      ROS.STATUS_MASTER_DESCRIPTION,
      O.ORDER_GROUP,
      RP.NAME Ordering_Prov,
      RPA.NAME Admitting_Prov
FROM (select distinct EMPI
        from RARv2) RAR_distinct
join ODS.Patient_Identifiers PI
  ON PI.empi = RAR_distinct.empi
join ODS.Patient P
  ON P.PK_Patient_ID = PI.FK_Patient_ID
join ODS.Encounter PARTITION (EPIC) E
  ON E.FK_Patient_ID = P.PK_Patient_ID
JOIN ODS.ORDERS PARTITION(EPIC) O
  ON E.PK_ENCOUNTER_ID = O.FK_ENCOUNTER_ID
JOIN ODS.ORDER_MED OM
  ON O.PK_Order_ID = OM.FK_Order_ID
JOIN ODS.R_ORDER_STATUS ROS
  ON ROS.PK_ORDER_STATUS_ID = O.FK_ORDER_STATUS_ID
JOIN ODS.R_Medication M
  ON OM.FK_Medication_ID = M.PK_Medication_ID
LEFT OUTER JOIN ODS.R_Provider RP
  ON RP.PK_Provider_ID = O.FK_Provider_ID
LEFT OUTER JOIN ODS.R_Provider RPA
  ON RPA.PK_Provider_ID = E.FK_ADMITTING_Provider_ID
WHERE
  (PHARMACY_CLASS IN ('Antihypertensive', 'Calcium blockers', 'Beta blockers', 'Diuretics')
  OR PHARMACY_SUBCLASS = 'Potassium')
  AND E.PATIENT_MASTER_CLASS = 'OUTPATIENT'
  AND OM.SOURCE_CODE = 'EPIC' AND P.SOURCE_CODE = 'EPIC'
;
--STATUS (Labs from ORder_Result_table [EPIC: FINAL, CERNER: VERIFIED/AUTOVERIFIED]
select count(*) from RAR_meds; --1.5 M
select count(distinct(EMPI)) from RAR_meds; # 5931
select * from RAR_meds
order by EMPI, simple_generic_name;
select count(*) from DRG_AKI_2017;
-- Look for vital signs
DROP TABLE RAR_vitals;
CREATE TABLE RAR_vitals NOLOGGING AS
select /*+ALL_ROWS*/
  RAR_distinct.EMPI,
      ENC_DATE,
        BP_Diastolic,
    BP_Systolic,
    RET.ENC_TYPE_MASTER_DESCRIPTION,
    EVS.PATIENT_MASTER_CLASS
FROM
  (select distinct EMPI
        from RARv2) RAR_distinct
JOIN ODS.Patient_Identifiers PI
  ON RAR_distinct.EMPI = PI.EMPI
JOIN ODS.Patient P
  ON P.PK_Patient_ID = PI.FK_Patient_ID
JOIN ODS.ENCOUNTER PARTITION(EPIC) EVS
  ON EVS.FK_Patient_ID = P.PK_Patient_ID
JOIN ODS.R_ENCOUNTER_TYPE RET
  ON EVS.FK_ENC_TYPE_ID = RET.PK_ENC_TYPE_ID
WHERE
  BP_SYSTOLIC IS NOT NULL
;
select count(*) from RAR_vitals; -- 2181686
select * from RAR_vitals;
DROP TABLE RAR_dx;
CREATE TABLE RAR_dx NOLOGGING AS
select /*+ALL_ROWS*/
  CODE,
  PI.EMPI,
  E.ENC_DATE,
  E.PK_ENCOUNTER_ID,
  Dx.CODING_DATE, Dx.PRIMARY_YN, DX_SEQUENCE,
  DX.Comments,
  DX.DX_Type
FROM (select distinct EMPI
        from RARv2) RAR_distinct
JOIN ODS.Patient_Identifiers PI
  ON RAR_distinct.EMPI = PI.EMPI
JOIN ODS.Patient P
  ON P.PK_Patient_ID = PI.FK_Patient_ID
JOIN ODS.ENCOUNTER PARTITION(EPIC) E
  ON E.FK_Patient_ID = P.PK_Patient_ID
JOIN ODS.Diagnosis Dx
  ON E.PK_Encounter_ID = Dx.FK_Encounter_ID
JOIN ODS.R_Codes_Diagnosis CD
  ON CD.PK_Dx_Code_ID = Dx.FK_Dx_Code_ID
where
  PI.Identifier_Name = 'EMPI'
  AND P.SOURCE_CODE = 'EPIC'
;
select * from RAR_DX;
select count(*) from RAR_DX;