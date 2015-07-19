#Useful R commands
#taken from https://github.com/FlorianMuellerklein/R

#clear console
clc <- function() cat(rep("\n",50))
clc()

#Read SPSS
read.spss(file, use.value.labels = TRUE, to.data.frame = TRUE)

#Subset based on factor
Newdataframe = subest(maindataframe, maindataframe$factor == whatever)

#Label factors (categories)
dataframe$whatevercolumn = factor(dataframe$whatevercolumn, labels = c('blah', 'blah'))

#Change column names
colnames(data.fram)[x] = "newname"

#plot with factor colors
qplot(dataframe$whatevercolumn, dataframe$whatevercolumn, color = dataframe$factor)

#Pretty Histograms
ggplot(data.frame, aes(x = datacolumm, fill = factorcolumn)) + geom_bar()

#Standard Deviation of column 
sapply(dataframe$column, sd)

#T-test using a factor
t.test(dataframe$whatevercolumn ~ dataframe$factor)

#Count values in a factor
table(dataframe$factor)

#Summary stats
summary(dataframe$whatevercolumn)

#Check if data is normal
shapiro.test(x, y)

#ANOVA 1-way
anova = aov(data~factor)
summary(anova)

#ANOVA 2-way
anova = aov(data~factor*factor)
summary(anova)

#After running anova you can see the pair-wise comparison
TukeyHSD('nameofanova')

#Fit a linear regression
fit = lm(y ~ x, data = data.frame)
fit = lm(y ~ x1 + x2 + ... + xn, data = data.frame)

#predict using fitted regression (variable must match the ones used to fit)
predict(fit, newdata = data.frame(variable(x) = listofnewvalues))

#plotting and subsetting two time-series data sets on the same graph
ts.plot(ts(a.ts[100:150]), ts(b.ts[100:150]), gpars = list(col = c('black', 'red')))
#or
ggplot(df,aes(x=timevariable,y=value,color=variable,group=variable)) + geom_line()

#Check if a specific value is present in an array
'value' %in% array #or
is.element('value', array)