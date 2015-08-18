#create corpus from data before running these commands
#use the file "text_mining" for this purpose

#build term document matrix
complaint_DTM <- TermDocumentMatrix(complaint_corpus, control=list(minWordLength=1))

dtmDF <- as.data.frame(as.matrix(t(complaint_DTM)))
dtmMAT <- as.matrix(t(complaint_DTM))

#frequent terms and associations
findFreqTerms(complaint_DTM, lowfreq=100)

#reduce dimensions using PCA
library(irlba)

pca1 <- prcomp(dtmDF,scale=TRUE)
summary(pca1)
View(pca1$rotation)

#retain first 1000 components based on the percenatge of variance explained
p <- as.matrix(pca1$rotation[,1:1000])
q <- as.matrix(dtmDF)
final <- as.data.frame(q%*%p)

#for clustering, either heirarichal or k means clustering can be used

#heirarichal clustering
d <- dist(final, method="euclidean")
fit <- hclust(d)
groups <- cutree(fit, k=3)
table(groups)
mydf <- dtmDF
mydf <- cbind(dtmDF,groups)
