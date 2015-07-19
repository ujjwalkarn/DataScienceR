#install packages
install.packages("tm")
install.packages("tau")
install.packages("plyr")
install.packages("SnowballC")
install.packages("wordcloud")

#load packages
library(tm)
library(tau)
library(plyr)
library(SnowballC)
library(wordcloud)

#read the data
data<-read.csv("data.csv")

#convert the column to be analysed to character
data$message<-as.character(data$message)

#create a corpus
mycorpus<-Corpus(VectorSource(data$message))

#inspect the first few comments
inspect(mycorpus[1:4])

#Sentiment Analysis
#Text Segmentation

mycorpus<-tm_map(mycorpus,tolower)
mycorpus<-tm_map(mycorpus,removePunctuation)
mycorpus<-tm_map(mycorpus,removeNumbers)
mycorpus<-tm_map(mycorpus,removeWords,stopwords(kind="English"))
stopwords(kind="English")
mycorpus<-tm_map(mycorpus,stripWhitespace)
mycorpus<-tm_map(mycorpus,PlainTextDocument)

#stem document
dictCorpus<-mycorpus
mycorpus<-tm_map(mycorpus,stemDocument)
mycorpus<-tm_map(mycorpus,stemCompletion,dictCorpus)

#build Term Document Matrix
myTDM<-TermDocumentMatrix(mycorpus, control=list(minWordLength=1))
myTDMdf<-as.data.frame(as.matrix(t(myTDM)))

#find most frequent terms
findFreqTerms(myTDM,lowfreq=100)

#find associations
findAssoc(myTDM, terms='happy', 0.1)

#n-grams analysis
mydf<-data.frame(text=unlist(sapply(mycorpus,'[',"content")),stringAsFactors=FALSE)
mydf$text<-as.character(mydf$text)

ngram_1L<-textcnt(mydf$text,n=1L,method="string")
ngram_t1<-data.frame(counts=unclass(ngram_1L),size=nchar(names(ngram_1L)),text=names(ngram_1L))
n1L<-arrange(ngram_t1,desc(counts))
counts1<-n1L[,c(3,1)]
View(counts1)

ngram_3L<-textcnt(mydf$text,n=3L,method="string")
ngram_t3<-data.frame(counts=unclass(ngram_3L),size=nchar(names(ngram_3L)),text=names(ngram_3L))
n3L<-arrange(ngram_t3,desc(counts))
counts3<-n3L[,c(3,1)]
View(counts3)

#wordcloud
wordcloud(counts1$text,counts1$count,min.freq=20)

#finding the types of comments
#k-means clustering
wss<-(nrow(myTDMdf)-1)*sum(apply(myTDMdf,2,var))
for(i in 1:15){wss[i]<-sum(kmeans(myTDMdf,centers=i)$withinss)}
plot(1:15,wss,type="b",xlab="No. of Clusters",ylab="wss")

k<-kmeans(myTDMdf,5,nstart=20)

groups<-data.frame(k$cluster)
table(groups)

message<-as.data.frame(data[,"message"])
finalDF<-as.data.frame(cbind(message,groups))
names(finalDF)<-c("message","group")

x1<-subset(finalDF, group==2)
View(x1)
