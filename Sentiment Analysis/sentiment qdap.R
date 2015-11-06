library(qdap)
#a good package, also takes into account  negative words and amplifiers
#see: http://www.inside-r.org/packages/cran/qdap/docs/polarity

data<- read.csv("comments.csv")
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


#convert corpus to data frame
mydf<-data.frame(text=unlist(sapply(mycorpus,'[',"content")),stringAsFactors=FALSE)
mydf$text<-as.character(mydf$text)


a <- unlist(apply(mydf,1,function(x) polarity(x[1])[[2]]$ave.polarity))
attributes(a) <- NULL
a  <- as.vector(a)
score <- data.frame("Sentence_Num" = 1:nrow(mydf),"Sentiment Score" = a)

str(score)
final<-cbind(data, score[,2])

write.csv(final,"final.csv")

# References:
# http://stackoverflow.com/questions/22774913/estimating-document-polarity-using-rs-qdap-package-without-sentsplit