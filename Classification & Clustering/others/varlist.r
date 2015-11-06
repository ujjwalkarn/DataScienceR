#' Subset variables in data frame
#'
#' Get subset of variables in data frame based on type, regexp pattern and specified exclusions.
#'
#' @param df Data frame to be used
#' @param type One of "numeric", "factor", "character" or any combination as vector
#' @param pattern Regexp pattern used to filter variables
#' @param exclude List of variables to be excluded
#' @export
#' @examples
#' # All variable starting with cred
#' varlist(german_data,pattern="^cred")
#' 
#' # All numeric variable
#' varlist(german_data,type="numeric")
#' 
#' # All factor variable except variable gb and variables starting with c
#' varlist(german_data,type="factor",exclude=c("gb",varlist(german_data,pattern="^c")))
#' 
#' # Same as previous, only using pattern instead of c()
#' varlist(german_data,type="factor",exclude=varlist(german_data,pattern="^c|gb"))
#' 
#' # Everything but factors
#' varlist(german_data,exclude=varlist(german_data,"factor"))
#' 
#' # Use sapply to apply function for variables
#' sapply(german_data[,varlist(german_data,type="numeric",pattern="credit")], summary)
#' sapply(german_data[,varlist(german_data,type="numeric",pattern="credit")], 
#'    function (x) length(unique(x)))


varlist <- function (df=NULL,type=c("numeric","factor","character"), pattern="", exclude=NULL) {

  vars <- character(0)

  if (any(type %in% "numeric")) {
    vars <- c(vars,names(df)[sapply(df,is.numeric)])
  }
  if (any(type %in% "factor")) {
    vars <- c(vars,names(df)[sapply(df,is.factor)])
  }  
  if (any(type %in% "character")) {
    vars <- c(vars,names(df)[sapply(df,is.character)])
  }  

  vars[(!vars %in% exclude) & grepl(vars,pattern=pattern)]
}
