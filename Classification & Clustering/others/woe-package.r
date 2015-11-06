#' Weight of Evidence and Information Value in Credit Scoring
#' 
#' Calculate Information Value and Weight of Evidence for variables in data frame. Information Value is used in credit scoring to compare predictive power among variables.
#' Weight of Evidence (WoE): \deqn{WoE = \ln(\frac{\%good_i}{\%bad_i})}
#' Information Value:
#' \deqn{IV = \Sigma_{i=1}^{n}(\ln(\frac{\%good_i}{\%bad_i})*(\%good_i - \%bad_i))}
#' @docType package
#' @name woe
#' @import ggplot2 
#' @importFrom plyr rbind.fill
#' @importFrom RColorBrewer brewer.pal
#' @import rpart
#' @importFrom sqldf sqldf
NULL