import pandas as pd
df = pd.read_csv('/home/selah/Data/herman-study/3_meds_deidentified.csv')




dfg = df.groupby(['FULL_NAME', 'GENERIC_NAME', 'PHARMACY_CLASS']).size().to_frame('freq').reset_index().sort_values('freq', ascending=False)

dan_pharm_class = ['Antihypertensive', 'Calcium blockers', 'Beta blockers', 'Diuretics']
dan_generic = ['Amlodipine-Atorvastatin', 'Isosorb Dinitrate-Hydralazine']
dfgf = dfg[(dfg.PHARMACY_CLASS.isin(dan_pharm_class) == False) & (dfg.GENERIC_NAME.isin(dan_generic) == False)]

dfgf.to_csv('/home/selah/Data/herman-study/new_potential_hypertension_meds.csv')




