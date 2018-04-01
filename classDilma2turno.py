from textblob import TextBlob
import pandas as pd  
import numpy as np

dilma2turno=pd.read_csv("Dilma_2turno_limpo.txt",delimiter="\t",encoding='latin-1')

def analize_sentiment(tweet):
    analysis = TextBlob(tweet)
    if analysis.detect_language() != 'en':
        try:
            analysis = TextBlob(str(analysis.translate(to='en')))
        
            return analysis.sentiment.polarity
        except:
            return -1
    else:
        return analysis.sentiment.polarity

d2 = []

count = 0 
for tweet in dilma2turno['text']:
    d2.append(analize_sentiment(tweet))
    count = count+1
    if(count%100==0):
        print(count)

pd.reset_option('mode.chained_assignment')

with pd.option_context('mode.chained_assignment', None):
    dilma2turno_test['score'] = d2
dilma2turno_test.to_csv('clasdilma2_total.txt',sep='\t', mode='a',index=False)