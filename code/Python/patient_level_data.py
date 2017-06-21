import pandas as pd
import numpy as np

proj_dir = '/home/selah/Projects/Daniel_Herman_Aldosterone_2017_03/'

regexfile = proj_dir + "data/collapsed_regex_20170621_deid.csv"
df_regex = pd.read_csv(regexfile)


##### DIAGNOSIS ICD9 ########

#TODO - unique and all ICD9 diagnosises for a patient


##### MEDICATIONS #########


dfmed = pd.read_csv(proj_dir + "data/2_meds.csv")
dfumed = pd.read_excel(proj_dir + "data/UNIQUE_HYPERTENSION_MEDICATIONS.xlsx")
sgnlist = dfumed.SIMPLE_GENERIC_NAME.unique().tolist()

dfmed['is_hyp_med'] = dfmed.SIMPLE_GENERIC_NAME.isin(sgnlist) & (dfmed.GENERIC_NAME.isnull()==False) & (dfmed.SIMPLE_GENERIC_NAME.isnull()==False)
dfmedg = dfmed.groupby('MRN_HUP').agg({'is_hyp_med':['sum','count']})
dfmedg.columns = ['hyp_meds_count', 'all_meds_count']
dfmedg['hyp_meds_ratio'] = dfmedg.hyp_meds_count/dfmedg.all_meds_count

#TODO - unique meds for a patient