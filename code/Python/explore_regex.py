import pandas as pd
import numpy as np

proj_dir = '/home/selah/Projects/Daniel_Herman_Aldosterone_2017_03/'


notesdatafileregex = proj_dir + "data/2_notes_regex_20170620.csv"
dfqc = pd.read_csv(notesdatafileregex)

with open(proj_dir + "data/2_notes_regex_20170620.csv") as f:
    print(f.readline())
#    print(f.readline())
    line = f.readline()
    
    
with open(proj_dir + "data/raw_data/2_notes.csv", encoding='utf-8-sig') as f:
    liner = f.readline()
list(liner[0:10])
