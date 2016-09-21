# Compile raw .json files from twitter feed into an organized file for
# performing analytics
import json
import matplotlib.pyplot as plt
import pandas as pd
import re

path_to_data = 'nuggets1002.json' # use smaller file for development
tweet_data = []
tweet_file = open(path_to_data, 'r')
#n_errors = 0

for line in tweet_file:
    try:
        tweet = json.loads(line)
        tweet_data.append(tweet)
    except:
        #n_errors += 1
        #print 'error loading line from json file:', n_errors
        continue

print len(tweet_data)
# NOTE: n_errors exactly = length of data; 'errors' are just blank lines
# between tweets

df = pd.DataFrame()
df['text'] = map(lambda tweet: tweet['text'], tweet_data)
df['lang'] = map(lambda tweet: tweet['lang'], tweet_data)
df['country'] = map(lambda tweet: tweet['place']['country']
                    if tweet['place'] != None else None,
                    tweet_data)



def word_in_text(word, text):
    '''
    Search for word in text (case insensitive), return True if found, else 
    return False
    '''
    match =  re.search(word.lower(), text.lower())
    if match:
        return True
    return False

# Test
#text = 'This is a text string.'
#print '"this" in text:', word_in_text('this', text)
#print '"TEXT" in text:', word_in_text('TEXT', text)
#print '"word" in text:', word_in_text('word', text)

keywords = ['Cisco', 'CCNA', 'AWS', 'Sharepoint', 'linux', 'powershell',
            'itil', 'sql', 'azure', 'ceh']

for word in keywords:
    df[word] = df['text'].apply(lambda tweet: word_in_text(word, tweet))


# Write DataFrame to .csv for potential further processing
# Note: encoding defaults to ascii, but will throw error if non-ascii text
# found (e.g., international text as found here):
print df['text'][4]
df.to_csv('twitter.csv', encoding = 'utf-8') 
