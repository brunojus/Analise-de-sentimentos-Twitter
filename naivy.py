from textblob import TextBlob
import pandas as pd  
import numpy as np
from textblob.classifiers import NaiveBayesClassifier
from sklearn.metrics import confusion_matrix
from sklearn.model_selection import train_test_split

dilma1turno=pd.read_csv("Dados Treinamento/clasdilma1.txt",delimiter="\t",encoding='latin-1')

subset = dilma1turno[['text', 'sentiment']]
tuples_dilma_1turno = [tuple(x) for x in subset.values]

tuples_dilma_1turno_train = tuples_dilma_1turno
tuples_dilma_1turno_test = tuples_dilma_1turno[:15000]

cl = NaiveBayesClassifier(tuples_dilma_1turno_train)

print("Accuracy: {0}".format(cl.accuracy(tuples_dilma_1turno_test)*100))

frase = 'odeio a Dilma'
blob = TextBlob(frase,classifier=cl)

print('Esta frase é de caráter:{}'.format(blob.classify()))