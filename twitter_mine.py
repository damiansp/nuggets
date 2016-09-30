from tweepy.streaming import StreamListener
from tweepy import OAuthHandler, Stream

# My API keys in twitterAPIKeys.txt
API_key = '<insert yours here>'
API_secret = '<insert yours here>'
access_token = '<insert yours here>'
access_token_secret = '<insert yours here>'

keywords = ['Cisco', 'CCNA', 'AWS', 'Sharepoint', 'linux', 'powershell',
            'itil', 'sql', 'azure', 'ceh']


class StdOutListener(StreamListener):
    def on_data(self, data):
        print data
        return True

    def on_error(self, status):
        print status


if __name__ == '__main__':
    # Authenticate and connect
    listener = StdOutListener()
    auth = OAuthHandler(API_key, API_secret)
    auth.set_access_token(access_token, access_token_secret)
    stream = Stream(auth, listener)

    stream.filter(track = keywords)

# run in command line and redirect output to nuggest.json via:
# > python twitter_mine.py > nuggests.json  # OR, >> to append
# Can track progress with
# > wc nuggets.json # while python script is running (first field is line
# count, includes tweet and line between, so first field = 2n)
# Run until samples >= 1000
# Start: 20 Sep ~5:30 pm; reached 1002 at 5:47 pm (17 mins)
