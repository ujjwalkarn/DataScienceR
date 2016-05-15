library(ROCR)
pred<-prediction(c(0.1,.5,.3,.8,.9,.4,.9,.5), c(0,0,0,1,1,1,1,1))
perf <- performance(pred, "tpr", "fpr")
plot(perf)


pred<-prediction(c(0.1,.5,.9,.8,.5,.5,.5,.7), c(0,0,0,1,1,1,1,1))
perf <- performance(pred, "tpr", "fpr")
plot(perf)


perf <- performance(pred, "sens", "spec")
plot(perf)



data(ROCR.simple)
pred <- prediction( ROCR.simple$predictions, ROCR.simple$labels)
perf <- performance(pred,"tpr","fpr")
plot(perf)



cutoffs <- data.frame(cut=perf@alpha.values[[1]], fpr=perf@x.values[[1]], 
                      tpr=perf@y.values[[1]])

subset(cutoffs, fpr == tpr)


library(Epi)
x <- rnorm( 100 )
z <- rnorm( 100 )
w <- rnorm( 100 )
tigol <- function( x ) 1 - ( 1 + exp( x ) )^(-1)
y <- rbinom( 100, 1, tigol( 0.3 + 3*x + 5*z + 7*w ) )
rc <- ROC( form = y ~ x + z, plot="sp" ) 
## optimal combination
opt <- which.max(rowSums(rc$res[, c("sens", "spec")]))
## optimal cut-off point 
rc$res$lr.eta[opt]

ROC(form = y ~ x + z, plot = "ROC", MX = TRUE)
