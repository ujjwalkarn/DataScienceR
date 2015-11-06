#' Replace raw variables with Weight of Evidence
#'
#' Replaces variables in data frame with Weight of Evidence. This will add new columns with "_woe" suffix to specified data frame.
#'
#' @param df data frame with original data
#' @param iv list of information values for variables - output from \code{\link{iv.mult}} with \code{summary=FALSE}.
#' @param verbose Prints additional details when TRUE. Useful mainly for debugging.
#' @export
#' @examples
#' # Replace WoE for list of variables
#' outiv <- iv.mult(german_data,"gb",vars=c("ca_status","housing","duration"))
#' x <- iv.replace.woe(german_data,outiv)
#' str(x)
#' 
#' # Replace WoE for all variables
#' outiv <- iv.mult(german_data,"gb")
#' x <- iv.replace.woe(german_data,outiv)
#' str(x)
#' 
#' # Replace WoE for all numeric variables- ultimate one-liner
#' x <- iv.replace.woe(german_data,iv.mult(german_data,"gb",vars=varlist(german_data,"numeric")))
#' str(x)

iv.replace.woe <- function(df,iv,verbose=FALSE) {

iv_df <- rbind.fill(iv)

 for (n in iv) { 
   variable_name <- n[1,1]
   variable_name_woe <- paste(variable_name,"_woe",sep="")

  if(verbose) {
    cat(paste0("Var Name: ",variable_name,"\n"))
    cat(paste0("WOE Name: ",variable_name_woe,"\n"))       
  }
 
   if(!("sql" %in% colnames(n)))
   {
     sqlstr <-  paste("select df.*, iv.woe as ", variable_name_woe ," from df join iv_df as iv on (df.", variable_name ," = iv.class and iv.variable ='",variable_name,"')",sep="")
     df <- sqldf(sqlstr,drv="SQLite")
   } else {
     sqlstr_woe <- ifelse(paste(n$sql,collapse= " ")=="when  then 0.0" || any(is.infinite(n$woe)) ,"0",paste("case ",paste(n$sql,collapse= " "),"else 0 end"))
     sqlstr <- paste("select df.*,",sqlstr_woe," as", variable_name_woe, "from df")
     df <-sqldf(sqlstr,drv="SQLite")
   }

 }
   df
}


