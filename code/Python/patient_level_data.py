#cd /home/selah/Projects/Daniel_Herman_Aldosterone_2017_03/code/Python

import pandas as pd
import numpy as np

import proj
import importlib
import collapse_ICD9, collapse_meds, collapse_regex
importlib.reload(collapse_ICD9)
importlib.reload(collapse_meds)
importlib.reload(collapse_regex)

#TODO
dfids =  pd.read_csv(proj.dir + 'data/MRN_deid_dict.csv')
dfi = collapse_ICD9.get()
dfm = collapse_meds.get()
dfr = collapse_regex.get()


df = dfids.merge(dfr, on='MRN_deid', how='left')
df = df.merge(dfi, on='MRN_HUP', how='left')
df = df.merge(dfm, on='MRN_HUP', how='left')
df.fillna(0, inplace=True)


d = pd.to_datetime('today').strftime("%Y%m%d")
df.to_csv(proj.dir + "data/patient_level_data_{}_deid.csv".format(d))
