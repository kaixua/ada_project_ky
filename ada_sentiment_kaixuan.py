import tweepy
import csv
import pandas as pd
import random
import nltk
from nltk.sentiment import SentimentAnalyzer
from nltk.sentiment.util import *
from string import punctuation

consumer_key = 'TNN9sNlf4zkwee06BiE4TXinLXrb'
consumer_secret = 'deDpGE5G0tnPaI6REOaqc849x9aafij2YoZKLMkjcXBqDI0Bag7oK'
access_token = '964231391914344448-7afagdhR6wDyxSUUjxgDcdhHNTOUw3n9Jj4'
access_token_secret = 'DzU0YMl74ACFu2zCTCaAcq6Vsgs6RECtHImtOTHceCVRw1j'

auth = tweepy.OAuthHandler(consumer_key, consumer_secret)
auth.set_access_token(access_token, access_token_secret)
api = tweepy.API(auth,wait_on_rate_limit=True)
csvFile = open('CHINESE.csv', 'a')
csvWriter = csv.writer(csvFile)

for tweet in tweepy.Cursor(api.search,q="#CHINESE",count=100,
                           lang="en",
                           since="2017-04-03").items():
    print (tweet.created_at, tweet.text)
    csvWriter.writerow([tweet.created_at, tweet.text.encode('utf-8')])



sentiment_scores = {}

with open("tidytext_sentiments.txt",'r') as infile :
    next(infile)
    for line in infile.readlines() :
        line = line.strip().split("\t")
        if line[1] == "positive" :
            sentiment_scores[line[0]] = 1
        else :
            sentiment_scores[line[0]] = -1



exclude = set(punctuation)
exclude.add("“")
exclude.add("”")
sentiment_list = []
with open("Chinese.csv") as infile :
    for line in infile.readlines() :
        tweet = line.strip("\n").split(",")[1]
        tweet = ''.join([ch.lower() for ch in tweet if ch not in exclude])
        words = tweet.split()
        positive = 0
        negative = 0
        for word in words:
            if word in sentiment_scores:
                if sentiment_scores[word] == 1:
                    positive += 1
                else:
                    negative += 1
        sentiment_list.append([positive, negative, line.strip("\n").split(",")[0]])
print(sentiment_list)

no_score = 0
counter=0
with open("sentiment score_pos.csv", "w") as file:
    for score in sentiment_list:
        pos = score[0]
        neg = score[1]
        if (pos + neg > 0):
            file.write(",".join([str(pos/(pos + neg)),str(counter)]) + "\n")
        else:
            no_score += 1
        counter += 1
print(no_score)
print(len(sentiment_list))

no_score = 0
counter=0
with open("sentiment score_neg.csv", "w") as file:
    for score in sentiment_list:
        pos = score[0]
        neg = score[1]
        if (pos + neg != 0):
            file.write(",".join([str(-neg/(pos + neg)),str(counter)]) + "\n")
        else:
            no_score += 1
        counter += 1
print(no_score)
print(len(sentiment_list))

