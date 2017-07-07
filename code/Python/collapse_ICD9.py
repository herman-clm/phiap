import pandas as pd
import numpy as np

proj_dir = '/home/selah/Projects/Daniel_Herman_Aldosterone_2017_03/'

df = pd.read_csv(proj_dir + "data/raw_data/7_5_ALL_ICD9.csv")

dfg = df.groupby('EMPI').agg({'CODE':['count','nunique']}).reset_index()
dfg.columns = ['EMPI', 'count_ICD9_all', 'count_ICD9_uniq']
#dfg.columns = [' '.join(col).strip() for col in dfg.columns.values]

#Match patterns: 405%, 401%
dfhyp = df[df.CODE.str.contains('401')|df.CODE.str.contains('405')]
dfhypg = dfhyp.groupby('EMPI').agg({'CODE':['count','nunique']}).reset_index()
dfhypg.columns = ['EMPI', 'count_ICD9_hyp', 'count_ICD9_hyp_uniq']

dfm = dfg.merge(dfhypg, on='EMPI', how='left').fillna(0)


def get():
    return dfm

if __name__ == "__main__":
    d = pd.to_datetime('today').strftime("%Y%m%d")
    dfm.to_csv(proj_dir + "data/collapsed_ICD9_{}.csv".format(d))




