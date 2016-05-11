library(tm)
library(RCurl)
library(plyr)
library(pacman)
library(ggplot2)
library(twitteR)
library(stringr)
library(Rstem)
library(devtools)
library(sentiment)
library(wordcloud)
install_url("http://cran.r-project.org/src/contrib/Archive/Rstem/Rstem_0.4-1.tar.gz")
install_url("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz")

consumer_key<-'2ZgCNl24UekOA6XCrkrl44ckq'
consumer_secret<-'wgEIdy6VTanz5oQojjeiPdiVoQDTophXsxdx5vO0mylOX5xGvZ'
access_token<-'3013177956-a1Pkor6eYwO4RaVozAd1KWp7aXfTQZso2mvViRm'
access_secret<-'pgPQx0WTgBQWHQTGcEU6VFfSiscccvciPQg6ziYD8ejf3'
setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_secret)
amazon<-searchTwitter('AmazonIndia',n=1500,lang='en',resultType='recent')
snapdeal<-searchTwitter('Snapdeal',n=1500,lang='en',resultType='recent')
flipkart<-searchTwitter('Flipkart',n=1500,lang='en',resultType='recent')
paytm<-searchTwitter('Paytm',n=1500,lang='en',resultType='recent')
instamojo<-searchTwitter('Instamojo',n=1500,lang='en',resultType='recent')
zomato <- searchTwitter('Zomato',n=1500,lang='en',resultType ='recent')
#converting to text
amazon<-sapply(amazon,function(x) x$getText())
snapdeal<-sapply(snapdeal,function(x) x$getText())
flipkart<-sapply(flipkart,function(x) x$getText())
paytm<-sapply(paytm,function(x) x$getText())
zomato<-sapply(zomato,function(x) x$getText())
instamojo <- sapply(instamojo,function(x) x$getText())
# error handling during cleaning
catch.error = function(x)
{
  y=NA
  catch_error = tryCatch(tolower(x),error=function(e) e)
  if(!inherits(catch_error,'error'))
      y=tolower(x)
  return(y)
}

# to clean the tweets
cleanTweets<-function(tweet){
#removing links
tweet = gsub("(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", " ", tweet)
#retweet
tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", tweet)
# removing hashtags
tweet = gsub("#\\w+", " ", tweet)
# removing @people
tweet = gsub("@\\w+", " ", tweet)
#removing punctuations
tweet = gsub("[[:punct:]]", " ", tweet)
#removing numbers
tweet = gsub("[[:digit:]]", " ", tweet)
#removing emojis
tweet<-str_replace_all(tweet,"[^[:graph:]]"," ")
#removing spaces
tweet = gsub("[ \t]{2,}", " ", tweet)
tweet = gsub("^\\s+|\\s+$", "", tweet)
# covert to lower
tweet = catch.error(tweet)
tweet
}
cleanTweetsAndRemoveNAs<- function(tweet)
{
tweetscleaned<- sapply(tweet,cleanTweets)
# remove NAs tweets from this tweet
tweetscleaned <- tweetscleaned[!is.na(tweetscleaned)]
#remove the repetitive
tweetscleaned = unique(tweetscleaned)
tweetscleaned
}
amazon<-cleanTweetsAndRemoveNAs(amazon)
zomato <-cleanTweetsAndRemoveNAs(zomato)
flipkart <- cleanTweetsAndRemoveNAs(flipkart)
snapdeal <- cleanTweetsAndRemoveNAs(snapdeal)
instamojo <- cleanTweetsAndRemoveNAs(instamojo)
paytm <- cleanTweetsAndRemoveNAs(paytm)
# assessing emotions
amazon_emo<-classify_emotion(amazon,algorithm='bayes',prior=1.0)
zomato_emo<-classify_emotion(zomato,algorithm='bayes',prior=1.0)
flipkart_emo<-classify_emotion(flipkart,algorithm='bayes',prior=1.0)
snapdeal_emo<-classify_emotion(snapdeal,algorithm='bayes',prior=1.0)
instamojo_emo<-classify_emotion(instamojo,algorithm='bayes',prior=1.0)
paytm_emo <- classify_emotion(paytm,algorithm='bayes',prior=1.0)
# it classifies tweets into anger disgust fear joy sadness and surprise
# fetching emotion category
amazonEmotion<-amazon_emo[,7]
zomatoEmotion<-zomato_emo[,7]
flipkartEmotion<-flipkart_emo[,7]
snapdealEmotion<-snapdeal_emo[,7]
instamojoEmotion<-instamojo_emo[,7]
paytmEmotion<-paytm_emo[,7]
# removing NA's from emotions column
amazonEmotion[is.na(amazonEmotion)] ='unknown'
zomatoEmotion[is.na(zomatoEmotion)] ='unknown'
flipkartEmotion[is.na(flipkartEmotion)]='unknown'
snapdealEmotion[is.na(snapdealEmotion)]='unknown'
instamojoEmotion[is.na(instamojoEmotion)]='unknown'
paytmEmotion[is.na(paytmEmotion)]='unknown'
# classifying tweets to the given categories
amazonClassPol<-classify_polarity(amazon,algorithm='bayes')
zomatoClassPol<-classify_polarity(zomato,algorithm='bayes')
flipkartClassPol<-classify_polarity(flipkart,algorithm='bayes')
snapdealClassPol<-classify_polarity(snapdeal,algorithm='bayes')
instamojoClassPol<-classify_polarity(instamojo,algorithm='bayes')
paytmClassPol<- classify_polarity(paytm,algorithm='bayes')
# polarity category
amazonPol <- amazonClassPol[,4]
zomatoPol <- zomatoClassPol[,4]
flipkartPol <- flipkartClassPol[,4]
snapdealPol <- snapdealClassPol[,4]
instamojoPol <- instamojoClassPol[,4]
paytmPol <- paytmClassPol[,4]
#creating a data frame with the above results
amazonSentimentDataFrame = data.frame(text=amazon,emotion=amazonEmotion,polarity=amazonPol)
zomatoSentimentDataFrame = data.frame(text=zomato,emotion=zomatoEmotion,polarity=zomatoPol)
flipkartSentimentDataFrame = data.frame(text=flipkart,emotion=flipkartEmotion,polarity=flipkartPol)
snapdealSentimentDataFrame = data.frame(text=snapdeal,emotion=snapdealEmotion,polarity=snapdealPol)
instamojoSentimentDataFrame = data.frame(text=instamojo,emotion=instamojoEmotion,polarity=instamojoPol)
paytmSentimentDataFrame = data.frame(text=paytm,emotion=paytmEmotion,polarity=paytmPol)
# rearrange data inside by sorting it
amazonSentimentDataFrame = within(amazonSentimentDataFrame, emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
zomatoSentimentDataFrame = within(zomatoSentimentDataFrame, emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
flipkartSentimentDataFrame = within(flipkartSentimentDataFrame, emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
snapdealSentimentDataFrame = within(snapdealSentimentDataFrame, emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
instamojoSentimentDataFrame = within(instamojoSentimentDataFrame, emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
paytmSentimentDataFrame = within(paytmSentimentDataFrame, emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
plotSentiments1<- function (sentiment_dataframe,title) {
library(ggplot2)
ggplot(sentiment_dataframe, aes(x=emotion)) + 
geom_bar(aes(y=..count.., fill=emotion)) +
scale_fill_brewer(palette="Dark2") +
ggtitle(title) +
theme(legend.position='right') + ylab('Number of Tweets') + 
xlab('Emotion Categories')
}
#plotting tweets' nature
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
#png('Nature_of_Tweets.png')
png('amazon.png')
plotSentiments1(amazonSentimentDataFrame, 'Sentiment Analysis of Tweets on Twitter about Amazon')
dev.off()
png('Zomato.png')
plotSentiments1(zomatoSentimentDataFrame, 'Sentiment Analysis of Tweets on Twitter about Zomato')
dev.off()
png('flipkart.png')
plotSentiments1(flipkartSentimentDataFrame, 'Sentiment Analysis of Tweets on Twitter about Flipkart')
dev.off()
png('snapdeal.png')
plotSentiments1(snapdealSentimentDataFrame, 'Sentiment Analysis of Tweets on Twitter about Snapdeal')
dev.off()
png('Paytm.png')
plotSentiments1(paytmSentimentDataFrame, 'Sentiment Analysis of Tweets on Twitter about Paytm')
dev.off()
png('Instamojo.png')
plotSentiments1(instamojoSentimentDataFrame, 'Sentiment Analysis of Tweets on Twitter about Instamojo')
dev.off()



# Plotting polarity

plotSentiments2 <- function (sentiment_dataframe,title) {
library(ggplot2)
ggplot(sentiment_dataframe, aes(x=polarity)) +
geom_bar(aes(y=..count.., fill=polarity)) +
scale_fill_brewer(palette="RdGy") +
ggtitle(title) +
theme(legend.position='right') + ylab('Number of Tweets') + 
xlab('Polarity Categories')
}
png('amazon_pol.png')
plotSentiments2(amazonSentimentDataFrame, 'Polarity Analysis of Tweets on Twitter about Amazon')
dev.off()
png('zomato_pol.png')
plotSentiments2(zomatoSentimentDataFrame, 'Polarity Analysis of Tweets on Twitter about Zomato')
dev.off()
png('flipkart_pol.png')
plotSentiments2(flipkartSentimentDataFrame, 'Polarity Analysis of Tweets on Twitter about Flipkart')
dev.off()
png('snapdeal_pol.png')
plotSentiments2(snapdealSentimentDataFrame, 'Polarity Analysis of Tweets on Twitter about Snapdeal')
dev.off()
png('paytm_pol.png')
plotSentiments2(paytmSentimentDataFrame, 'Polarity Analysis of Tweets on Twitter about Paytm')
dev.off()
png('instamojo_pol.png')
plotSentiments2(instamojoSentimentDataFrame, 'Polarity Analysis of Tweets on Twitter about Instamojo')
dev.off()
removeCustomeWords <- function (TweetsCleaned) {
  for(i in 1:length(TweetsCleaned)){
    TweetsCleaned[i] <- tryCatch({
      TweetsCleaned[i] = removeWords(TweetsCleaned[i], 
                                     c(stopwords("english"), "care", "guys", "can", "dis", "didn", 
                                       "guy" ,"booked", "plz"))
      TweetsCleaned[i]
    }, error=function(cond) {
      TweetsCleaned[i]
    }, warning=function(cond) {
      TweetsCleaned[i]
    })
  }
  return(TweetsCleaned)
}

getWordCloud <- function (sentiment_dataframe, TweetsCleaned, Emotion) 
  {
  emos = levels(factor(sentiment_dataframe$emotion))
  n_emos = length(emos)
  emo.docs = rep("", n_emos)
  TweetsCleaned = removeCustomeWords(TweetsCleaned)
  for (i in 1:n_emos)
    {
    emo.docs[i] = paste(TweetsCleaned[Emotion == emos[i]], collapse=" ")
  }
  corpus = Corpus(VectorSource(emo.docs))
  tdm = TermDocumentMatrix(corpus)
  tdm = as.matrix(tdm)
  colnames(tdm) = emos
  require(wordcloud)
  suppressWarnings(comparison.cloud(tdm, colors = 
                                      brewer.pal(n_emos, "Dark2"), scale = c(3,.5), random.order = 
                                      FALSE, title.size = 1.5))
}
png('amazon_cloud.png')
getWordCloud(amazonSentimentDataFrame, amazon,amazonEmotion)
dev.off()
png('zomato_cloud.png')
getWordCloud(zomatoSentimentDataFrame, zomato,zomatoEmotion)
dev.off()
png('flipkart_cloud.png')
getWordCloud(flipkartSentimentDataFrame, flipkart,flipkartEmotion)
dev.off()
png('snapdeal_cloud.png')
getWordCloud(snapdealSentimentDataFrame, snapdeal,snapdealEmotion)
dev.off()
png('instamojo_cloud.png')
getWordCloud(instamojoSentimentDataFrame, instamojo,instamojoEmotion)
dev.off()
png('paytm_cloud.png')
getWordCloud(paytmSentimentDataFrame, paytm,paytmEmotion)
dev.off()






  
  



























