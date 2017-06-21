import pandas as pd
import numpy as np

proj_dir = '/home/selah/Projects/Daniel_Herman_Aldosterone_2017_03/'

notesdatafileregex = proj_dir + "data/regex_notes_20170620.csv"
df = pd.read_csv(notesdatafileregex)
df = df[df.Note_Text.isnull() == False]


dftight = df[df.Note_Text.str.contains("tightness")]

nt = df.ix[0].Note_Text
len(nt.split())
