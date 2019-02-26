/*********************
***   Encounters   ***
*********************/
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
  /* there is LOCATION_ZIP, which may be more precise,
     but here MASTER_LOCATION_ZIP is used to match MASTER_LOCATION_CODE */
  RL.MASTER_LOCATION_ZIP,
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
select count(distinct(EMPI)) from RAR_pts_enc; --10388

select count(*) from RAR_pts_enc; -- 2924932; (2199089)
--select count(distinct *) from RAR_pts_enc;
select count(distinct(PK_ENCOUNTER_ID)) from RAR_pts_enc; --2924932; (2199089) GOOD




/***********************
***   Demographics   ***
***********************/
-- Get demographics
DROP TABLE RAR_pts_demo;
CREATE TABLE RAR_pts_demo NOLOGGING AS
SELECT /*+ALL_ROWS*/
  PI.EMPI,
  P.PK_PATIENT_ID,
  RG.GENDER_MASTER_CODE,
  P.Birth_Date,
  P.ZIP,
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
select count(*) from RAR_pts_demo; --10396
select count(distinct(EMPI)) from RAR_pts_demo; --10390



--select count(distinct *) from RAR_pts_demo;
select count(distinct(PK_PATIENT_ID)) from RAR_pts_demo; --10396






/*********************
***       Dx       ***
*********************/

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


/*********************
***      Labs      ***
*********************/

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
JOIN ODS.ENCOUNTER PARTITION (EPIC) E
  ON E.FK_Patient_ID = P.PK_Patient_ID
JOIN ODS.Orders PARTITION (EPIC) O
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


select * from RAR_pts_labs;
select count(*) from RAR_pts_labs; -- 8176278 (OLD: 6464229)
select count(distinct(EMPI)) from RAR_pts_labs; -- 10263 (OLD: 7137)

--select count(distinct *) from RAR_pts_labs;
select count(distinct(PK_ORDER_RESULT_ID)) from RAR_pts_labs; --6833888 (OLD: 5420303   Why is this field duplicated?)

select count(distinct(PK_ORDER_ID)) from RAR_pts_labs; -- 2745063 (OLD: 2163797)

-- CERNER Labs
-- NOTE: for CERNER Labs, ORDERING_PROV was removed
-- since the "EMPTY" entry will end up in a strange string, like ^@^@^@^@^@...,
-- and also some "SYSTEM SYSTEM".
DROP TABLE RAR_pts_labsC;
CREATE TABLE RAR_pts_labsC NOLOGGING AS
select /*+ALL_ROWS*/
PI.EMPI,
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
OResI.LOINC_CODE
-- RP.NAME Ordering_Prov
from RAR_FULL_PTS
JOIN ODS.Patient_Identifiers PI
  ON RAR_FULL_PTS.EMPI = PI.EMPI
JOIN ODS.Patient P
  ON P.PK_Patient_ID = PI.FK_Patient_ID
JOIN ODS.ENCOUNTER PARTITION(CERNER) E
  ON E.FK_Patient_ID = P.PK_Patient_ID
JOIN ODS.Orders PARTITION(CERNER) O
  ON E.PK_Encounter_ID = O.FK_Encounter_ID
left outer JOIN ODS.R_Order_Item OI
  ON O.FK_Order_Item_ID = OI.PK_Order_Item_ID
left outer JOIN ODS.Order_Result ORes
  ON O.PK_Order_ID = ORes.FK_Order_ID
left outer JOIN ODS.R_RESULT_ITEM OResI
  ON ORes.FK_RESULT_ITEM_ID = OResI.PK_Result_Item_ID
-- LEFT OUTER JOIN ODS.R_Provider RP
--    ON RP.PK_Provider_ID = O.FK_Provider_ID
where
  PI.Identifier_Facility = 'HUP'
  --AND O.FK_ORDER_ITEM_ID IN (976529, 1191870) -- Adding the additional codes
  AND ORDER_START_DATE < to_date('01-Jul-2017', 'dd-mon-yyyy')
;




/*********************
***      Meds      ***
*********************/

DROP TABLE DINGXI.RAR_PTS_MEDS;
CREATE TABLE DINGXI.RAR_PTS_MEDS NOLOGGING AS
SELECT /*+ALL_ROWS*/
        PI.EMPI,
        E.PATIENT_MASTER_CLASS,
        E.PK_ENCOUNTER_ID,
        E.ENC_DATE,
        O.PK_ORDER_ID,
        O.ORDER_START_DATE,
        O.ORDER_STOP_DATE,
        O.SOURCE_LAST_UPDATE_DATE,
        O.ORDER_GROUP,  -- new
        RP.NAME Ordering_Prov,
        RP2.NAME Order_Entry_Prov,
        RP_admit.NAME Admit_Prov,
        ROT.ORDER_TYPE_CODE,
        ROS.STATUS_CODE,
        ROS.STATUS_DESCRIPTION,
        M.SIMPLE_GENERIC_NAME,
        M.GENERIC_NAME,
        M.FULL_NAME,
        O.ORDER_NAME,
        coalesce(M.SIMPLE_GENERIC_NAME, M.FULL_NAME, O.ORDER_NAME) MED_NAME,
        M.PHARMACY_CLASS,
        M.PHARMACY_SUBCLASS,
        OM.QUANTITY,
        OM.REFILLS,
        OM.DOSE,
        OM.UNIT_OF_MEASURE,  -- new
        OM.FREQUENCY_NAME,
        OM.ODS_LAST_UPDATE_DATE
FROM RAR_FULL_PTS RFP
LEFT JOIN ODS.Patient_Identifiers PI
  ON RFP.EMPI = PI.EMPI
LEFT JOIN ODS.Patient P
  ON PI.FK_PATIENT_ID = P.PK_PATIENT_ID
LEFT JOIN  ODS.ENCOUNTER PARTITION (EPIC) E
  ON E.FK_PATIENT_ID = P.PK_PATIENT_ID
LEFT JOIN ODS.ORDERS PARTITION(EPIC) O
  ON E.PK_ENCOUNTER_ID = O.FK_ENCOUNTER_ID
JOIN ODS.ORDER_MED OM
  ON O.PK_ORDER_ID = OM.FK_ORDER_ID
LEFT JOIN ODS.R_ORDER_STATUS ROS
  ON ROS.PK_ORDER_STATUS_ID = O.FK_ORDER_STATUS_ID
LEFT JOIN ODS.R_Medication M
  ON OM.FK_Medication_ID = M.PK_Medication_ID
LEFT JOIN ODS.R_ORDER_TYPE ROT
  ON O.FK_ORDER_TYPE_ID = ROT.PK_ORDER_TYPE_ID
LEFT OUTER JOIN ODS.R_Provider RP
  ON RP.PK_Provider_ID = O.FK_Provider_ID
LEFT OUTER JOIN ODS.R_Provider RP2
  ON RP2.PK_PROVIDER_ID = O.FK_ORDER_ENTRY_USER_ID
LEFT JOIN ODS.R_PROVIDER RP_admit
    ON RP_admit.PK_PROVIDER_ID = E.FK_ADMITTING_PROVIDER_ID
WHERE
  E.ENC_DATE < to_date('01-Jul-2017', 'dd-mon-yyyy')
  AND PI.Identifier_Facility = 'HUP'
  AND E.PATIENT_MASTER_CLASS = 'OUTPATIENT'
  AND OM.SOURCE_CODE = 'EPIC'
  AND P.SOURCE_CODE = 'EPIC'
;



SELECT count(DISTINCT EMPI)
FROM RAR_PTS_MEDS
;
-- 10007

SELECT count(DISTINCT EMPI)
  FROM DINGXI.RAR_EMPI_MRN
;
-- 10390

SELECT EMPI
  FROM DINGXI.RAR_EMPI_MRN
MINUS
SELECT EMPI
  FROM DINGXI.RAR_PTS_MEDS
;
-- 383


-- Look for the missing patients
select /*+ALL_ROWS*/

    PI.EMPI,
        E.PATIENT_MASTER_CLASS,

        E.PK_ENCOUNTER_ID,
        O.*,
  OM.*
FROM ODS.ENCOUNTER PARTITION (EPIC) E
LEFT JOIN ODS.ORDERS PARTITION(EPIC) O
  ON E.PK_Encounter_ID = O.FK_Encounter_ID
JOIN ODS.Patient P
  ON O.FK_Patient_ID = P.PK_Patient_ID
JOIN ODS.Patient_Identifiers PI
  ON P.PK_Patient_ID = PI.FK_Patient_ID
LEFT JOIN ODS.ORDER_MED OM
  ON O.PK_Order_ID = OM.FK_Order_ID
WHERE
  -- (PHARMACY_CLASS IN ('Antihypertensive', 'Calcium blockers', 'Beta blockers', 'Diuretics') OR
  --   SIMPLE_GENERIC_NAME IN ('Amlodipine-Atorvastatin', 'Isosorb Dinitrate-Hydralazine'))
  -- AND PI.Identifier_Name = 'EMPI'
  -- AND O.ORDER_START_DATE >= to_date('01-Dec-2014', 'dd-mon-yyyy')
  -- AND E.ENC_DATE >= to_date('01-Dec-2014', 'dd-mon-yyyy')
  E.ENC_DATE < to_date('01-Jul-2017', 'dd-mon-yyyy')
  AND PATIENT_MASTER_CLASS = 'OUTPATIENT'
  -- AND PI.EMPI IN ()    ## add the EMPI's from previous MINUS set to verify that these pts do not have MEDS
;
-- for these patients, they do not have med info; null PK_ORDER_MED_ID







/*********************
***  EMPI & MRN    ***
*********************/
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
LEFT JOIN ODS.PATIENT_IDENTIFIERS PI
    ON PI.EMPI = RAR_FULL_PTS.EMPI
WHERE IDENTIFIER_NAME LIKE '%MRN%'
  AND PI.IDENTIFIER_FACILITY = 'HUP'
  AND PI.ACTIVE_YN = 1
  AND PI.EMPI_NUM IS NOT NULL
  AND PI.SOURCE_CODE = 'EPIC'
;

select count(*) from RAR_empi_mrn; -- 132361 (OLD: 92601)
select count(Distinct(EMPI)) from RAR_empi_mrn; -- 10390 (OLD: 7137)

DROP TABLE FULL_EMPI;
CREATE TABLE FULL_EMPI NOLOGGING AS
SELECT
  DISTINCT EMPI
  FROM RAR_empi_mrn;

select count(Distinct(EMPI)) from FULL_EMPI; -- 10390 (OLD: 7137)




