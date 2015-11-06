library(tm)
require(lda)

dataValues<- read.csv('comments.csv')
dataValues=dataValues[sample(nrow(dataValues),size=10000,replace=FALSE),]


dim(dataValues)
## Text Pre-processing.
## Creating a Corpus from the Orginal Function
## interprets each element of the vector x as a document
CorpusObj<- VectorSource(dataValues$message);
CorpusObj<-Corpus(CorpusObj);
CorpusObj <- tm_map(CorpusObj, tolower) # convert all text to lower case
CorpusObj <- tm_map(CorpusObj, removePunctuation) 
CorpusObj <- tm_map(CorpusObj, removeNumbers)
inspect(CorpusObj[1:100])

myStopwords<-c("custom words",stopwords("english"))

CorpusObj <- tm_map(CorpusObj, removeWords,myStopwords) 
CorpusObj <- tm_map(CorpusObj, stemDocument, language = "english") ## Stemming the words 
CorpusObj<-tm_map(CorpusObj,stripWhitespace)

gsub("[^A-Za-z]", "", a)


corpusLDA <- lexicalize(CorpusObj )

ldaModel=lda.collapsed.gibbs.sampler(corpusLDA$documents,K=5,vocab=corpusLDA$vocab,burnin=9999,num.iterations=1000,alpha=0.7,eta=0.1)
top.words <- top.topic.words(ldaModel$topics, 8, by.score=TRUE)
print(top.words) 
