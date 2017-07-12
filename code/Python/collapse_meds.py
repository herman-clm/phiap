import pandas as pd
import numpy as np

proj_dir = '/home/selah/Projects/Daniel_Herman_Aldosterone_2017_03/'


dfmed = pd.read_csv(proj_dir + "data/raw_data/7_5_SENT_MEDS.csv")
dfmedf = dfmed[['EMPI','ORDER_DATE', 'SIMPLE_GENERIC_NAME', 'GENERIC_NAME']]
#TODO investigate why so few patients have medications
#dfmed.MRN_HUP.nunique()

dfhypmed = pd.read_excel(proj_dir + "data/raw_data/UNIQUE_HYPERTENSION_MEDICATIONS.xlsx")
sgnlist = dfhypmed.SIMPLE_GENERIC_NAME.unique().tolist()

#TODO - discuss how to handle Nan's, why did i remove NaN's from GENERIC_NAME also before
dfmedf['is_hyp'] = dfmed.SIMPLE_GENERIC_NAME.isin(sgnlist) & (dfmed.SIMPLE_GENERIC_NAME.isnull()==False)
dfmedg = dfmedf.groupby('EMPI').agg({'is_hyp':['sum','count']})
dfmedg.columns = ['count_meds_hyp', 'count_meds_all']
      
dfmedu = dfmedf[['EMPI','SIMPLE_GENERIC_NAME','is_hyp']].drop_duplicates()
dfmedug = dfmedu.groupby('EMPI').agg({'is_hyp':['sum','count']})
dfmedug.columns = ['count_meds_hyp_uniq', 'count_meds_uniq']

dfmedcounts = dfmedg.merge(dfmedug, left_index=True, right_index=True, how="left").reset_index()

def get():
    return dfmedcounts

if __name__ == "__main__":
    d = pd.to_datetime('today').strftime("%Y%m%d")
    dfmedcounts.to_csv(proj_dir + "data/collapsed_meds_{}.csv".format(d))


#General purpose collapsing on multilevel column names resulting from agg
#dfmedg.columns = [' '.join(col).strip() for col in dfmedg.columns.values]
