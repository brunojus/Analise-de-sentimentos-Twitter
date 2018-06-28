# coding: utf-8
import pandas as pd  
import tweepy

consumer_key = "H3lOJNG2EzkVEAqBxbSlaq61Z"

consumer_secret = "DcgjAZFYw1tPJo4HzdDRWbj1aUwCg1jJ682UhH0ZLChovd4mzT"

access_token_secret = "dtc3IpefPHh223KJ9CeAhotdH7II9b2zRHmGbnpiP47Ig"

access_token = "52856333-MAGbQIsiLRNLeZcQaGzqHxsoK5tCPPxU437TJQrfZ"

auth = tweepy.OAuthHandler(consumer_key, consumer_secret)
auth.set_access_token(access_token, access_token_secret)

api = tweepy.API(auth)

deputados = ['adalberto.csv',
'andre_de_paula.csv',
'augusto_coutinho.csv',
'betinho_gomes.csv',
'bruno_araujo.csv',
'daniel_coelho.csv',
'danilo_cabral.csv',
'eduardo_fonte.csv',
'felipe_carreras.csv',
'fernando_coelho.csv',
'gonzaga_patriota.csv',
'jarbas_vasconcelos.csv',
'joao_coutinho.csv',
'jorge_corte.csv',
'kaio_manicoba.csv',
'luciana_santos.csv',
'marinaldo_rosendo.csv',
'mendonca_filho.csv',
'pastor_eurico.csv',
'ricardo_teobaldo.csv',
'sebastiao_oliveira.csv',
'silvio_costa.csv',
'tadeu_alencar.csv',
'wolney_queiroz.csv',
'zeca_cavalcanti.csv']


for deputado in deputados:

    dados = '~/artigoIEEE/classificados/' + deputado
    saida = '~/artigoIEEE/local/' + deputado
    entrada=pd.read_csv(dados,delimiter=";",encoding='utf-8')

    entrada['local'] = None




    location = []
    for tweet in entrada['username']:
        try:
            user = api.get_user(tweet)
            print(user.location)
            location.append(user.location)
        except tweepy.TweepError as e:
            location.append('Não Existe')




    entrada['local'] = location


    entrada.to_csv(saida, sep=';', mode='a',index=False)

