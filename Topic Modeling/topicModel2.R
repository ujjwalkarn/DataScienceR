#code taken from http://www.codemiles.com/r-examples/topic-modeling-using-lda-in-r-t11119.html
require("ggplot2")
require("grid")
require("plyr")
library(reshape)
library(ScottKnott)
setwd("D:/SecondPaper/")
library(lda)
library(tm)
dataValues<- read.csv('MozillaCommentsFixed.csv',sep=",")
dataValues=dataValues[sample(nrow(dataValues),size=1000,replace=FALSE),]


dim(dataValues)
## Text Pre-processing.
## Creating a Corpus from the Orginal Function
## interprets each element of the vector x as a document
CorpusObj<- VectorSource(dataValues$text);
CorpusObj<-Corpus(CorpusObj);
CorpusObj <- tm_map(CorpusObj, tolower) # convert all text to lower case
CorpusObj <- tm_map(CorpusObj, removePunctuation) 
CorpusObj <- tm_map(CorpusObj, removeNumbers)
CorpusObj <- tm_map(CorpusObj, removeWords, stopwords("english")) 
CorpusObj <- tm_map(CorpusObj, stemDocument, language = "english") ## Stemming the words 
CorpusObj<-tm_map(CorpusObj,stripWhitespace)
##create a term document matrix 
CorpusObj.tdm <- TermDocumentMatrix(CorpusObj, control = list(minWordLength = 3))
inspect(CorpusObj.tdm[1:10,1:10])
findFreqTerms(CorpusObj.tdm, lowfreq=1003)
dim(CorpusObj.tdm)
CorpusObj.tdm.sp <- removeSparseTerms(CorpusObj.tdm, sparse=0.88)
dim(CorpusObj.tdm.sp)
## Show Remining words per 15 Document.
inspect(CorpusObj.tdm.sp[1:10,1:15])


## visualizing  the TD --  

## Words Cloud Visualizing
library(wordcloud)
library(RColorBrewer)


mTDM <- as.matrix(CorpusObj.tdm)
v <- sort(rowSums(mTDM),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
pal <- brewer.pal(9, "BuGn")
pal <- pal[-(1:2)]
png("wordcloud.png", width=1280,height=800)
wordcloud(d$word,d$freq, scale=c(8,.3),min.freq=2,max.words=100, random.order=T, rot.per=.15, colors=pal, vfont=c("sans serif","plain"))
dev.off()

CorpusObj.tdm.reformatted <- as.matrix(CorpusObj.tdm.sp)
## Reformatting should -- checking memory space ..
object.size(CorpusObj.tdm.sp)
object.size(CorpusObj.tdm.reformatted)
CorpusObj.tdm.reformatted = melt(CorpusObj.tdm.reformatted, value.name ="count")
head(CorpusObj.tdm.reformatted)


library(ggplot2)
head(CorpusObj.tdm.reformatted)
dim(CorpusObj.tdm.reformatted)
## I have many Documents, so I limiting the number of Docs in X axis



ggplot(CorpusObj.tdm.reformatted, aes(x = Docs, y = Terms, fill = log10( value) )) +
  geom_tile(colour = "white") +
  scale_fill_gradient(high="#FF0000" , low="#FFFFFF")+
  ylab("") +
  theme(panel.background = element_blank()) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()); 


## Controlling Sparse Terms
CorpusObj.tdm.sp <- removeSparseTerms(CorpusObj.tdm, sparse=0.5)
## Convert document term matrix to data frame
CorpusObj.tdm.sp.df <- as.data.frame(inspect(CorpusObj.tdm.sp ))
## Number of words remaining
nrow(CorpusObj.tdm.sp.df)

require(slam)
# transpose document term matrix, necessary for the next steps using mean term 
#frequency-inverse document frequency (tf-idf) 
#to select the vocabulary for topic modeling
CorpusObj.tdm.sp.t <- t(CorpusObj.tdm.sp)
summary(col_sums(CorpusObj.tdm.sp.t))
# calculate tf-idf values
term_tfidf <- tapply(CorpusObj.tdm.sp.t$v/row_sums(CorpusObj.tdm.sp.t)[CorpusObj.tdm.sp.t$i], CorpusObj.tdm.sp.t$j,mean) * log2(nDocs(CorpusObj.tdm.sp.t)/col_sums(CorpusObj.tdm.sp.t>0))
summary(term_tfidf)
# keep only those terms that are slightly less frequent that the median
CorpusObj.tdm.sp.t.tdif <- CorpusObj.tdm.sp.t[,term_tfidf>=1.0]
CorpusObj.tdm.sp.t.tdif <- CorpusObj.tdm.sp.t[row_sums(CorpusObj.tdm.sp.t) > 0, ]
summary(col_sums(CorpusObj.tdm.sp.t.tdif)) 

require(topicmodels)

myModel=builtModel<-LDA(CorpusObj.tdm, 10);
head(topics(myModel))


best.model <- lapply(seq(2, 50, by = 1), function(d){LDA(CorpusObj.tdm.sp.t.tdif, d)}) # this will make a topic model for every number of topics between 2 and 50... it will take some time! 
best.model.logLik <- as.data.frame(as.matrix(lapply(best.model, logLik)))  # this will produce a list of logLiks for each model... 

# Top 5 words per Topics
terms(myModel, 5 )



### Faster Way of doing LDA 

corpusLDA <- lexicalize(CorpusObj )
require(lda)

ldaModel=lda.collapsed.gibbs.sampler(corpusLDA$documents,K=10,vocab=corpusLDA$vocab,burnin=9999,num.iterations=1000,alpha=1,eta=0.1)
top.words <- top.topic.words(ldaModel$topics, 5, by.score=TRUE)
print(top.words) - 
  
# References:
# http://www.codemiles.com/r-examples/topic-modeling-using-lda-in-r-t11119.html
