There are three ways to perform PCA in R: princomp() , prcomp() and  pca() in labdsv library . Essentially, they compute the same values (technically, princomp() and labdsv package computes an eigen analysis and prcomp() computes a singular value decomposition.).

The prcomp() function is a numerically stable routine that returns a “prcomp object” that contains the square-root of the eigenvalues (“sdev”), the eigenvectors (“rotation”), and the scores. And so the preferred method is prcomp(). 

The princomp() function is slightly less stable, but has more features. It returns a “princomp object” that contains the square-root of the eigenvalues (“sdev”), the eigenvectors (“loadings”),  the means for each variable (“center”) and the scores (“scores”), as well as some other things. Typing summary(princomp) or summary(prcomp) will return the percent of variation explained. 


## princomp()

p1 <- princomp(USArrests, cor = TRUE)  ## using correlation matrix
## p1 <- princomp(USArrests)  ## using covariance matrix

summary(p1)
loadings(p1)
plot(p1)
biplot(p1)
p1$scores
screeplot(p1) ## identical with plot()
screeplot(p1, npcs=4, type="lines")

## Formula interface
princomp(~ ., data = USArrests, cor = TRUE) ## identical with princomp(USArrests, cor = TRUE)
p2 <- princomp(~ Murder + Assault + UrbanPop, data = USArrests, cor = TRUE)
p2$scores


## prcomp()

## USArrests data vary by orders of magnitude, so scaling is appropriate
p3 <- prcomp(USArrests, scale = TRUE) ## using correlation matrix
## p3 <- prcomp(USArrests)  ## using covariance matrix

print(p3) 
summary(p3) 
plot(p3) ## Scree plot
biplot(p3)

## Formula interface
p4 <- prcomp(~ Murder + Assault + UrbanPop, data = USArrests, scale = TRUE)




## pca() in “labdsv” package

library(labdsv)  ## You first have to load the LabDSV library.
p5 <- pca(USArrests, dim=4, cor = TRUE) ## using correlation matrix 
## p5 <-pca(USArrests, dim=4) ## using covariance matrix 

summary(p5)
varplot.pca(p5)  ## scree plot and cumulative variances plot
loadings.pca(p5)
plot(p5)


--By SungHyun Kang
