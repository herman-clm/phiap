DROP TABLE RAR;
CREATE TABLE RAR NOLOGGING AS
select /*+ALL_ROWS*/ 
PI.EMPI, 
PI.PATIENT_IDENTIFIER, 
P.Birth_Date,
RG.GENDER_MASTER_CODE,
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
JOIN ODS.Order_Result ORes
  ON O.PK_Order_ID = ORes.FK_Order_ID
JOIN ODS.R_RESULT_ITEM OResI
  ON ORes.FK_RESULT_ITEM_ID = OResI.PK_Result_Item_ID
LEFT OUTER JOIN ODS.Order_Performed OP
  ON O.PK_Order_ID = OP.FK_Order_ID
LEFT OUTER JOIN ODS.R_Provider RP
  ON RP.PK_Provider_ID = O.FK_Provider_ID
LEFT OUTER JOIN ODS.R_Provider RPA
  ON RPA.PK_Provider_ID = E.FK_ADMITTING_Provider_ID
JOIN ODS.R_LOCATION L
   ON E.FK_LOCATION_ID = L.PK_LOCATION_ID
LEFT OUTER JOIN ODS.R_Gender RG
  ON P.FK_Gender_ID = RG.PK_Gender_ID
where
  --O.Order_Date > '01-Jan-2015'
  RESULT_STATUS = 'Final'
  AND OI.ORDER_ITEM_MASTER_PRIMARY_TYPE = 'LABORATORY'
  AND (upper(OI.ORDER_ITEM_DESCRIPTION) LIKE '%ALDO%' OR upper(OI.ORDER_ITEM_DESCRIPTION) LIKE '%RENIN%')
  AND PI.Identifier_Facility = 'HUP'
  AND ORDER_ITEM_CODE IN ('ALREACT', 'L500458', 'L000703', 'Q16845', 'Q16846', 'C9010071', 'L500467', 'C9010005')
  AND ORDER_ITEM_CODE NOT IN ('C9009997', 'Q19573', 'C9009995', 'C9009990', '83497A', 'C9010004') -- AVS, ALDOLASE, ...
;  

select count(*) from RAR; --17395
select count(distinct(EMPI)) from RAR; --7595
select * from RAR;

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
        from RAR) RAR_distinct
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
  AND abs((O.ORDER_START_DATE - RAR_distinct.ORDER_START_DATE)) <= 7
;

select count(*) from RAR_surround; --222201
select * from RAR_surround;

-- Lets get medications
DROP TABLE RAR_meds_current;
CREATE TABLE RAR_meds NOLOGGING AS
select /*+ALL_ROWS*/ 
        RAR_distinct.PK_ENCOUNTER_ID,
        OM.Dose,
        OM.Frequency_Name,
        M.SIMPLE_GENERIC_NAME,
        M.GENERIC_NAME,
        M.AMOUNT
FROM (select distinct PK_ENCOUNTER_ID
        from RAR) RAR_distinct
JOIN ODS.ORDERS PARTITION(EPIC) O
  ON RAR_distinct.PK_ENCOUNTER_ID = O.FK_ENCOUNTER_ID
JOIN ODS.ORDER_MED OM
  ON O.PK_Order_ID = OM.FK_Order_ID
JOIN ODS.R_Medication M
  ON OM.FK_Medication_ID = M.PK_Medication_ID
WHERE --THERAPEUTIC_CLASS = 'Cardiovascular Agents'
  (PHARMACY_CLASS IN ('Antihypertensive', 'Calcium blockers', 'Beta blockers', 'Diuretics') 
  OR PHARMACY_SUBCLASS = 'Potassium')
;

--STATUS (Labs from ORder_Result_table [EPIC: FINAL, CERNER: VERIFIED/AUTOVERIFIED]

select count(*) from RAR_meds; #500
select * from RAR_meds
order by PK_ENCOUNTER_ID, simple_generic_name;


-- Look for vital signs
DROP TABLE RAR_vitals;
CREATE TABLE RAR_vitals NOLOGGING AS
select /*+ALL_ROWS*/ RAR_distinct.*, 
      ENC_DATE,
        BP_Diastolic,
    BP_Systolic
FROM (select distinct EMPI
        from RAR) RAR_distinct
JOIN ODS.Patient_Identifiers PI
  ON RAR_distinct.EMPI = PI.EMPI
JOIN ODS.Patient P
  ON P.PK_Patient_ID = PI.FK_Patient_ID
JOIN ODS.ENCOUNTER PARTITION(EPIC) EVS
  ON EVS.FK_Patient_ID = P.PK_Patient_ID
--  AND ENC_TYPE_MASTER_DESCRIPTION = 'OFFICE VISITS';
WHERE EVS.ENC_DATE > '01-Jan-2014'
  AND BP_SYSTOLIC IS NOT NULL;
  
  select * from ODS.ENCOUNTER PARTITION(EPIC) where rownum < 10;
  select count(*) from RAR_vitals; -- 750
