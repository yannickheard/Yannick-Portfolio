#Importing needed packages

import tweepy
from tweepy import OAuthHandler
from tweepy.streaming import StreamListener
import pandas as pd
import os
import time
from datetime import datetime



#Twitter Authorization Setup, used my own Authorization codes from Twitter Dev Account when actually running this
auth = tweepy.OAuthHandler("Need your own")
auth.set_access_token("Need your own")
api = tweepy.API(auth)

    



#creating scraper function to use API and then build a dataframe to store the tweets, including timing elements to handle API limits. 
def tweetscraper(search_word, numTweets, numRuns):
    db_tweets = pd.DataFrame(columns = ['username', 'acctdesc', 'location', 'following',
                                        'followers', 'totaltweets', 'usercreatedts', 'tweetcreatedts',
                                        'retweetcount','text', 'hashtags'])

    program_start = time.time()
    for i in range(0, numRuns):
        start_run=time.time()

        tweets = tweepy.Cursor(api.search, q=search_word, lang='en', tweet_mode='extended').items(numTweets)

        tweet_list = [tweet for tweet in tweets]
        num_tweet = 0

        for tweet in tweet_list:
                username = tweet.user.screen_name
                acctdesc = tweet.user.description
                location = tweet.user.location
                following = tweet.user.friends_count
                followers = tweet.user.followers_count
                totaltweets = tweet.user.statuses_count
                usercreatedts = tweet.user.created_at
                tweetcreatedts = tweet.created_at
                retweetcount = tweet.retweet_count
                hashtags = tweet.entities['hashtags']
                try:
                    text = tweet.retweet_status.full_text
                except AttributeError:
                    text = tweet.full_text
                ith_tweet = [username, acctdesc, location, following, followers, totaltweets,usercreatedts, tweetcreatedts, retweetcount, text, hashtags]
                ith_series = pd.Series(ith_tweet, index=db_tweets.columns)
                db_tweets = db_tweets.append(ith_series, ignore_index = True)
                num_tweet += 1
            
        end_time = time.time()
        duration_run = round((end_time - start_run)/60, 2)
        print('no. of tweets scraped for run {} is {}'.format(i + 1, num_tweet))
        print('time take for {} run to complete is {} mins'.format(i+1, duration_run))            
        time.sleep(920)           

#Writing out the scraped dataframe to .csv file and printing a final time on how long it was running for                
    to_csv_timestamp = datetime.today().strftime('%Y%m%d_%H%M%S')   
    path = os.getcwd()
    filename = path + '\Desktop\\firsttest\\' + to_csv_timestamp + 'tweettest.csv'      
    db_tweets.to_csv(filename, index = False)
    program_end=time.time()
    print("Scraping Complete")   
    print("Total Scraping Time is {} minutes".format(round(program_end-program_start)/60, 2))
            
            
            
            
            
            
search_word = ['Liverpool', 'Arsenal', 'Leicester']                     
tweetscraper(search_word, 450, 70)            
            
            
#Created an alarm sound so that I have a way to hear when it finishes running           

import winsound
duration = 15000  # milliseconds
freq = 800  # Hz
winsound.Beep(freq, duration)
            