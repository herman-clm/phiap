#cd /home/selah/Projects/Daniel_Herman_Aldosterone_2017_03/code/Python

import pandas as pd

import proj

dfbp = pd.read_csv(proj.dir + "data/raw_data/7_5_ENC.csv")
dfbpf = dfbp[['EMPI','ENC_DATE','BP_DIASTOLIC', 'BP_SYSTOLIC']]
dfbpf['is_hyp'] = (dfbpf.BP_SYSTOLIC >= 140) | (dfbpf.BP_DIASTOLIC >= 90)

df = dfbpf.groupby('EMPI').agg({'is_hyp':['count','sum']})
df.columns = ['count_bp_all','count_bp_hyp']
s_ratio = df.count_bp_hyp/df.count_bp_all
s_ratio.hist()
df.reset_index(inplace=True)

def get():
    return df

if __name__ == "__main__":
    d = pd.to_datetime('today').strftime("%Y%m%d")
    df.to_csv(proj.dir + "data/collapsed_bp_{}.csv".format(d))



#
#
#
#
#dfg = df.groupby('MRN_HUP').agg({'CODE':['count','nunique']}).reset_index()
#dfg.columns = ['MRN_HUP', 'all_ICD9_count', 'all_ICD9_uniq_count']
##dfg.columns = [' '.join(col).strip() for col in dfg.columns.values]
#
##Match patterns: 405%, 401%
#dfhyp = df[df.CODE.str.contains('401')|df.CODE.str.contains('405')]
#dfhypg = dfhyp.groupby('MRN_HUP').agg({'CODE':['count','nunique']}).reset_index()
#dfhypg.columns = ['MRN_HUP', 'hyp_ICD9_count', 'hyp_ICD9_uniq_count']
#
#dfm = dfg.merge(dfhypg, on='MRN_HUP', how='left').fillna(0)
