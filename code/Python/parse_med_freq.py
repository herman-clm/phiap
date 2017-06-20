import pandas as pd
import nltk
import re

df = pd.read_csv('/home/selah/Data/herman-study/medicine_names.csv')
sr = df.FREQUENCY_NAME
sr = sr[sr.isnull() == False]
s = sr
######################
#SYNONYMS - REPLACE WITH STANDARD

p = re.compile('TAB\w*|CAP\w*|PILL\w*')
sr = sr.apply(lambda x: p.sub('ITEM', str(x)))

p = re.compile('(PER|A|EVERY)\s+DAY')
sr = sr.apply(lambda x: p.sub('DAILY', str(x)))

p = re.compile('(PER|A|EVERY)\s+WEEK')
sr = sr.apply(lambda x: p.sub('WEEKLY', str(x)))

p = re.compile('ONE')
sr = sr.apply(lambda x: p.sub('1', str(x)))

p = re.compile('TWO')
sr = sr.apply(lambda x: p.sub('2', str(x)))

p = re.compile('THREE')
sr = sr.apply(lambda x: p.sub('3', str(x)))

p = re.compile('ONCE')
sr = sr.apply(lambda x: p.sub('1X', str(x)))

p = re.compile('TWICE')
sr = sr.apply(lambda x: p.sub('2X', str(x)))


s = pd.concat((s, sr.to_frame('SYNONYMS')), axis=1)

#######################
#IDENTIFY AND INTERPRET AMOUNT VALUE

p = re.compile('\d+')
def identify_amount(x):
    r = p.findall(str(x))
    return r[0] if r else None

amt = sr.apply(identify_amount).to_frame('AMOUNT')
s = pd.concat((s, amt), axis=1)

##########################
#IDENTIFY AND INTERPRET FREQUENCY VALUE
p = re.compile('(\d+)\s?X')
def identify_freq(x):
    r = p.findall(str(x))
    return r[0] if r else 1
freq = sr.apply(identify_freq).to_frame('FREQUENCY')
s = pd.concat((s, freq), axis=1)

#######################
#IDENTIFY TIME INTEVAL (IN DAYS)

pstrs = []
pstrs.append('DAILY')
pstrs.append('EVERY [MEN]')
pstrs.append('AT BED\s?TIME')
pstrs.append('AT NIGHT')
pstrs.append('IN THE [EM]')
p_daily = re.compile("|".join(pstrs))
p_weekly = re.compile("WEEKLY")
p_hourly = re.compile("EVERY\s+(\d+)\s+HOUR")
def identify_time_interval(x):
    ti = None
    if p_daily.findall(str(x)):
        ti = 1
    if p_weekly.findall(str(x)):
        ti = 7
    r = p_hourly.search(str(x)) 
    if r:
        ti = float(r.group(1))/24.0
    return ti
    
timeint = sr.apply(identify_time_interval).to_frame('TIME_INTERVAL')
s = pd.concat((s, timeint), axis=1)


def calc_items_per_day(r):
    if r.AMOUNT and r.FREQUENCY and r.TIME_INTERVAL:
        return float(r.AMOUNT) * float(r.FREQUENCY) / float(r.TIME_INTERVAL)
    else:
        return None

ipd = s.apply(calc_items_per_day, axis=1).to_frame('ITEMS_PER_DAY')
s = pd.concat((s, ipd), axis=1)

#############################3
#
#from nltk.corpus import treebank
#
#x = 'TAKE ONE TABLET DAILY (IN ADDITION TO DULOXETINE 60 MG FOR TOTAL DAILY DOSE OF DULOXETINE 90 MG)'
#tok = nltk.word_tokenize(x)
#tag = nltk.pos_tag(tok)
#tree = nltk.chunk.ne_chunk(tag)
#
#t = treebank.parsed_sents(tree)[0]
#t.draw()
#



#srt = sr.apply(lambda x: nltk.word_tokenize(str(x)))
