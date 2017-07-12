#cd /home/selah/Projects/Daniel_Herman_Aldosterone_2017_03/code/Python

import pandas as pd
import numpy as np

import proj
import importlib
import collapse_ICD9, collapse_meds, collapse_regex, collapse_bp
importlib.reload(collapse_ICD9)
importlib.reload(collapse_meds)
importlib.reload(collapse_regex)
importlib.reload(collapse_bp)

#dfids =  pd.read_csv(proj.dir + 'data/MRN_deid_dict.csv')
dfi = collapse_ICD9.get()
dfm = collapse_meds.get()
#dfr = collapse_regex.get()
dfb = collapse_bp.get()

df = pd.DataFrame()
#df = dfids.merge(dfr, on='MRN_deid', how='left')
df = dfi
df = df.merge(dfm, on='EMPI', how='outer')
df = df.merge(dfb, on='EMPI', how='outer')
df.fillna(0, inplace=True)

#deidentify this puppy
df_deid = df.sample(len(df))
df_deid.insert(0, 'EMPI_deid', range(len(df)))


d = pd.to_datetime('today').strftime("%Y%m%d")
df_deid[['EMPI_deid', 'EMPI']].to_csv(proj.dir + "data/EMPI_deid_dict_{}.csv".format(d))
df_deid.drop('EMPI', axis=1).to_csv(proj.dir + "data/patient_level_data_{}_deid.csv".format(d))
