import pandas as pd
import numpy as np

file = "/home/selah/Projects/Daniel_Herman_Aldosterone_2017_03/data/raw_data/2_enc_1000_mrn.csv"

df = pd.read_csv(file)
df2 = df[['MRN_HUP']].drop_duplicates().sample(frac=1).reset_index().reset_index()
df2.columns = ['MRN_deid', 'dummy', 'MRN_HUP']

df2[['MRN_deid', 'MRN_HUP']].to_csv("/home/selah/Projects/Daniel_Herman_Aldosterone_2017_03/data/MRN_deid_dict.csv", index=False)

dfqc = pd.read_csv("/home/selah/Projects/Daniel_Herman_Aldosterone_2017_03/data/MRN_deid_dict.csv")
