import pandas as pd
#import numpy as np

#proj_dir = '/home/selah/Projects/Daniel_Herman_Aldosterone_2017_03/'
import proj

notesdatafileregex = proj.dir + "data/regex_notes_20170620.csv"
df = pd.read_csv(notesdatafileregex)
df = df[df.Note_Text.isnull() == False]
df['word_count'] = df.Note_Text.apply(lambda s: len(s.split()))
df['regex_matches'] = df.regex1_cnt + df.regex2_cnt
dfg = df.groupby('MRN_deid').sum()[['word_count','regex_matches']].reset_index()
dfg['regex_ratio'] = dfg.regex_matches/dfg.word_count

def get():
    return dfg
   
if __name__ == "__main__":
   d = pd.to_datetime('today').strftime("%Y%m%d")
   dfg.to_csv(proj.dir + "data/collapsed_regex_{}_deid.csv".format(d))
   



