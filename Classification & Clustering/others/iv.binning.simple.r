#' Simple binning of numeric variable
#'
#' This functions bins numeric variable into bins with unique values.
#'
#' @param df data frame with at least one numeric column
#' @param vars numeric column(x) to be binned
#' @param by parameter for quantile based binning. Default value 0.1 will create 10 bins if possible, using quantile function
#' @param eout Exclude outliers. If TRUE (default) then outliers as defined in boxplot are excluded before binning
#' and included afterwards as separated bins.
#' @param verbose Prints additional details when TRUE. Useful mainly for debugging.
#' @export
#' @examples
#' iv.binning.simple(german_data,"credit_amount")
#' iv.binning.simple(german_data,"credit_amount",by=0.05)
#' iv.binning.simple(german_data,"credit_amount",eout=FALSE)
#' iv.binning.simple(german_data,"age",by=0.2)
#' german_data$cut_cra1 <- iv.binning.simple(german_data,"credit_amount",eout=TRUE)
#' german_data$cut_cra2 <- iv.binning.simple(german_data,"credit_amount",eout=FALSE)
#' iv.plot.woe(iv=iv.mult(german_data,"gb",vars=c("cut_cra1","cut_cra2")))


iv.binning.simple <- function (df,vars,by=0.1,eout=TRUE,verbose=FALSE) {

for (col in vars) {
  breaks <- numeric(0)

  if(eout) {
    x <- boxplot(df[,col][!df[[col]] %in% boxplot.stats(df[[col]])$out],plot=FALSE)    
    non_outliers <- df[,col][df[[col]] <= x$stats[5] & df[[col]] >= x$stats[1]]
    if (!(min(df[[col]])==min(non_outliers))) {
      breaks <- c(breaks, min(df[[col]]))
    }
  }

  breaks <- c(breaks, quantile(if(eout) non_outliers else df[[col]], probs=seq(0,1, by=by)))  

  if(eout) {
    if (!(max(df[[col]])==max(non_outliers))) {
      breaks <- c(breaks, max(df[[col]]))
    }    
  }

  return (cut(df[[col]],breaks=breaks,include.lowest=TRUE))
}

}

