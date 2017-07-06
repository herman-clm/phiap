import pandas as pd
import numpy as np

proj_dir = '/home/selah/Projects/Daniel_Herman_Aldosterone_2017_03/'

regexfile = proj_dir + "data/collapsed_regex_20170621_deid.csv"
df_regex = pd.read_csv(regexfile)

#TODO - discuss how to handle Nan's

dfmed = pd.read_csv(proj_dir + "data/raw_data/2_meds.csv")
#TODO investigate why so few patients have medications
#dfmed.MRN_HUP.nunique()

dfhypmed = pd.read_excel(proj_dir + "data/raw_data/UNIQUE_HYPERTENSION_MEDICATIONS.xlsx")
sgnlist = dfhypmed.SIMPLE_GENERIC_NAME.unique().tolist()

dfmed['is_hyp_med'] = dfmed.SIMPLE_GENERIC_NAME.isin(sgnlist) & (dfmed.GENERIC_NAME.isnull()==False) & (dfmed.SIMPLE_GENERIC_NAME.isnull()==False)
dfmedg = dfmed.groupby('MRN_HUP').agg({'is_hyp_med':['sum','count']})
dfmedg.columns = ['hyp_meds_count', 'all_meds_count']
      
dfmedu = dfmed[['MRN_HUP','SIMPLE_GENERIC_NAME','is_hyp_med']].drop_duplicates()
dfmedug = dfmedu.groupby('MRN_HUP').agg({'is_hyp_med':['sum','count']})
dfmedug.columns = ['hyp_meds_uniq_count', 'all_meds_uniq_count']

dfmedcounts = dfmedg.merge(dfmedug, left_index=True, right_index=True, how="left").reset_index()

def get():
    return dfmedcounts

if __name__ == "__main__":
    d = pd.to_datetime('today').strftime("%Y%m%d")
    dfmedcounts.to_csv(proj_dir + "data/collapsed_meds_{}.csv".format(d))


#General purpose collapsing on multilevel column names resulting from agg
#dfmedg.columns = [' '.join(col).strip() for col in dfmedg.columns.values]
