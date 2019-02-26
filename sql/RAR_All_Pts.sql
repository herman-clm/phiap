/************
**  Dx Pts **
************/

-- Find patients with at least one hyperaldo dx code
DROP TABLE rar_dx_pts;
CREATE TABLE rar_dx_pts NOLOGGING AS
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
  (
    (CODE_STANDARD_NAME = 'ICD-10'
      AND ( (SUBSTR(CODE, 1, 3) = 'E26')
              OR CODE IN ('D35.00', 'E27.9', 'E27.0'))
    ) OR
    (CODE_STANDARD_NAME = 'ICD9'
      AND ( (SUBSTR(CODE, 1, 5) = '255.1')
            OR (CODE IN ('227.0', '255.9', '255.3'))
      )
    )
  )
  AND PI.Identifier_Facility = 'HUP'
  AND P.SOURCE_CODE = 'EPIC'
  AND E.ENC_DATE < to_date('01-Jul-2017', 'dd-mon-yyyy')
;

select count(*) from rar_dx; --23410
select * from rar_dx;
select count(distinct(EMPI)) from rar_dx; --4753: old-879

select CODE, count(*)
  from rar_dx
GROUP BY CODE;



/******************
**  RAR_Lab Pts **
******************/


-- pulling aldosteronism specific lab data
--Add to RAR
DROP TABLE rar_lab_pts;
CREATE TABLE rar_lab_pts NOLOGGING AS
select /*+ALL_ROWS*/
PI.EMPI,
PI.PATIENT_IDENTIFIER as MRN,
E.PK_ENCOUNTER_ID,
E.ENC_DATE,
O.ORDER_NAME,
OI.ORDER_ITEM_CODE,
OI.ORDER_ITEM_DESCRIPTION
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
where
  OI.ORDER_ITEM_MASTER_PRIMARY_TYPE = 'LABORATORY' -- Does this drop anything?
  AND (upper(OI.ORDER_ITEM_DESCRIPTION) LIKE '%ALDOSTERONE%' OR upper(OI.ORDER_ITEM_DESCRIPTION) LIKE '%RENIN%')
    AND (O.ORDER_NAME != 'ANTI MULLERIAN HORMONE')
    AND (OresI.RESULT_ITEM_CODE != 'ANTI MULLERIAN HORMONE')
  AND PI.Identifier_Facility = 'HUP'
  AND ORDER_START_DATE < to_date('01-Jul-2017', 'dd-mon-yyyy')
;


select count(*) from rar_lab_pts; -- 21211
select count(distinct(EMPI)) from rar_lab_pts; -- 7137

SELECT DISTINCT ORDER_NAME
FROM rar_lab_pts;


/*************
**  RDB Pts **
*************/
-- create table for reading in AVS_DB
DROP TABLE AVS_DB;
CREATE TABLE AVS_DB (
  enctr_date VARCHAR(10),
  hosp_num   VARCHAR(9),
  date_birth VARCHAR(9),
  fst_name   VARCHAR(25),
  lst_name   VARCHAR(25)
) NOLOGGING
;


DROP TABLE rar_db_pts;
CREATE TABLE rar_db_pts NOLOGGING AS
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


select count(*) from rar_db_pts; --458
select * from rar_db_pts;
select count(distinct(HOSP_NUM)) from rar_db_pts; --458
select count(distinct(EMPI)) from rar_db_pts; --458


/****************
**  UNION Pts **
****************/
DROP TABLE rar_full_pts;
CREATE TABLE rar_full_pts NOLOGGING AS
  (SELECT DISTINCT EMPI from rar_dx_pts)
  UNION
  (SELECT DISTINCT EMPI from rar_lab_pts)
  UNION
  (SELECT DISTINCT EMPI FROM rar_db_pts)
;

ALTER TABLE rar_full_pts
  ADD uni_id INTEGER
;
UPDATE rar_full_pts
    SET uni_id = ROWNUM;

ALTER TABLE rar_full_pts
ADD CONSTRAINT rar_full_pts
PRIMARY KEY (EMPI);

CREATE UNIQUE INDEX idx_pt_id
  ON rar_full_pts (uni_id, EMPI);



select count(*) from rar_full_pts; --10390
select count(distinct(EMPI)) from rar_full_pts; --10390

