import pandas as pd
import numpy as np

proj_dir = '/home/selah/Projects/Daniel_Herman_Aldosterone_2017_03/'

import sys
sys.path.append(proj_dir + 'code')
import utils

d = pd.to_datetime('today').strftime("%Y%m%d")

##### FIX ISSUES WITH FILE ############
#read in file and replace first two commas with ;; characters
notesdatafile = proj_dir + "data/raw_data/2_notes.csv"
notesdatafilefix = proj_dir + "data/OLD/2_notes_fix_{}.csv".format(d)


with open(notesdatafile, encoding='utf-8-sig') as fin:
    with open(notesdatafilefix, 'w') as fout:
        fout.write("MRN;;;NOTE_ID;;;Note_Text\n")
        for line in fin:  
            fout.write(line.replace(',', ';;;', 2))

df = pd.read_csv(notesdatafilefix, sep=';;;')
################################


notes = df.head(3).Note_Text.tolist()
note = notes[1]

import re
patt = re.compile(r'[^(]{12}(\d+\))(.{4})sdfsdfsd')
res = patt.findall(note)

# patt = re.compile(r'.{11}hyper.{11}')
patt = re.compile(r'.{,20}(?!pulm\w*\W*\w+\W+)\bhypertension.{,20}', re.IGNORECASE)
df['regex1'] = df['Note_Text'].astype(str).apply(lambda s: patt.findall(s)) 
df['regex1_cnt'] = df.regex1.apply(len)

#patt = re.compile(r'.{,20}(?!pulm\w*\W*\w+\W+)HTN.{,20}', re.IGNORECASE)
patt = re.compile(r'.{,20}(?!pulm\w*\W*\w+\W+)\bHTN.{,20}', re.IGNORECASE)
df['regex2'] = df['Note_Text'].astype(str).apply(lambda s: patt.findall(s)) 
df['regex2_cnt'] = df.regex2.apply(len)


df_deid_dict = pd.read_csv(proj_dir + "data/MRN_deid_dict.csv")

df_deid = df.merge(df_deid_dict, left_on='MRN', right_on='MRN_HUP', how="left").drop(['MRN', 'MRN_HUP'], axis=1)
cols = df_deid.columns.tolist()
cols = cols[-1:] + cols[:-1]

notesdatafileregex = proj_dir + "data/regex_notes_{}.csv".format(d)
df_deid[cols].to_csv(notesdatafileregex, index=False)
dfqc = pd.read_csv(notesdatafileregex)
    
#import re
#pattern = re.compile(r'\((M\d{4})\)(.*):')
#mcode_tuples = pattern.findall(data)             


#Regular expression 1: .*(?!pulm\w*\W*\w+\W+)hypertension.* (case insensitive)
#Regular expression 2: .*(?!pulm\w*\W*\w+\W+)HTN.* (case insensitive)

#TODO - read notes

#TODO - look for regex & count








