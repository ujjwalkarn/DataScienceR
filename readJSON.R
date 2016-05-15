#READING JSON FILE USING R
# library(rjson)
# myfile <- 'C:\\Users\\ujjwal.karn\\Desktop\\Tweets\\python.json'
# document2 <- fromJSON(file = myfile)
# 
# library(jsonlite)
# winners <- fromJSON(myfile)
# winners <- fromJSON(myfile, flatten=TRUE)
# 
# document$text
# 
# json_file <- stream_in(file(myfile))
# 
# 
# dat <- fromJSON(sprintf("[%s]", paste(readLines(myfile), collapse=",")))
# 
# dat$text


library(jsonlite)
options(encoding = "UTF-8")

# read in individual JSON lines
json_file <- "C:\\Users\\ujjwal.karn\\Desktop\\Tweets\\python.json"

# turn it into a proper array by separating each object with a "," and
# wrapping that up in an array with "[]"'s.

dat <- fromJSON(sprintf("[%s]", paste(readLines(json_file), collapse=",")))

dim(dat)
## [1] 3959   18

tweets<-dat$text
tweets



#tweetsdf <- data.frame(matrix(unlist(dat$text), nrow=nrow(dat), byrow=T))
