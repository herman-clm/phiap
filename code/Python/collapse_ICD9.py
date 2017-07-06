import pandas as pd
import numpy as np

proj_dir = '/home/selah/Projects/Daniel_Herman_Aldosterone_2017_03/'

df = pd.read_csv(proj_dir + "data/raw_data/2A_ALL_ICD9Diag.csv")

dfg = df.groupby('MRN_HUP').agg({'CODE':['count','nunique']}).reset_index()
dfg.columns = ['MRN_HUP', 'all_ICD9_count', 'all_ICD9_uniq_count']
#dfg.columns = [' '.join(col).strip() for col in dfg.columns.values]

#Match patterns: 405%, 401%
dfhyp = df[df.CODE.str.contains('401')|df.CODE.str.contains('405')]
dfhypg = dfhyp.groupby('MRN_HUP').agg({'CODE':['count','nunique']}).reset_index()
dfhypg.columns = ['MRN_HUP', 'hyp_ICD9_count', 'hyp_ICD9_uniq_count']

dfm = dfg.merge(dfhypg, on='MRN_HUP', how='left').fillna(0)


def get():
    return dfm

if __name__ == "__main__":
    d = pd.to_datetime('today').strftime("%Y%m%d")
    dfm.to_csv(proj_dir + "data/collapsed_ICD9_{}.csv".format(d))




