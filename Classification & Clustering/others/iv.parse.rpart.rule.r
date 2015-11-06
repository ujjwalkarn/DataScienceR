#' Parse rpart rules for one predictor
#'
#' This function is not intended to be used by end user. It is used by iv.num function.
#'
#' @param x Predictor variable
#' @param rpart.rules Output from path.rpart for leaf nodes
#' 
iv.parse.rpart.rule <- function(x,rpart.rules) {
  
out_rule <- data.frame(min=numeric(0),max=numeric(0),
                       min_comp=character(0),max_comp=character(0),
                       class_label=character(0),sql=character(0),
                       tree_node = character(0),
                       stringsAsFactors=F)

for (i in seq_along(rpart.rules)) {

t1 <- (gsub("df[, x]",fixed=TRUE,replacement="",x=rpart.rules[[i]]))
t1
ge <- gsub(pattern=">=",x=(t1[which(grepl("*>=[:digit:]*",x=t1,))]),fixed=T,replacement="")
g  <- gsub(pattern=">" ,x=(t1[which(grepl("*> [:digit:]*",x=t1,))]),fixed=T,replacement="")
l  <- gsub(pattern="<" ,x=(t1[which(grepl("*< [:digit:]*",x=t1,))]),fixed=T,replacement="")
le <- gsub(pattern="<=",x=(t1[which(grepl("*<=[:digit:]*",x=t1,))]),fixed=T,replacement="")

ge <- ifelse(length(ge)==0,NA,max(as.numeric(ge)))
g  <- ifelse(length(g) ==0,NA,max(as.numeric(g)))
le <- ifelse(length(le)==0,NA,min(as.numeric(le)))
l  <- ifelse(length(l) ==0,NA,min(as.numeric(l)))

out_rule[i,"min"] <- if (!(is.na(ge) && is.na(g))) max(ge,g,na.rm=T) else NA
out_rule[i,"max"] <- if (!(is.na(le) && is.na(l))) max(le,l,na.rm=T) else NA
out_rule[i,"min_comp"] <- ifelse(!is.na(ge),">=",ifelse(!is.na(g),">",""))
out_rule[i,"max_comp"] <- ifelse(!is.na(le),"<=",ifelse(!is.na(l),"<",""))
out_rule[i,"class_label"] <- paste0(
                    ifelse(!is.na(ge),"<","("),
                    ifelse(is.na(out_rule$min[i]),"",out_rule$min[i]),
                    ";",
                    ifelse(is.na(out_rule$max[i]),"",out_rule$max[i]),
                    ifelse(!is.na(le),">",")")
                    )
out_rule[i,"sql"] <- paste0("when ",
                              ifelse(is.na(out_rule$min[i]),"",paste(x,out_rule$min_comp[i],out_rule$min[i])),
                              ifelse (!is.na(out_rule$min[i]) && !is.na(out_rule$max[i])," AND ", ""),
                              ifelse(is.na(out_rule$max[i]),"",paste(x,out_rule$max_comp[i],out_rule$max[i])),
                            " then ")
out_rule[i,"tree_node"] <- names(rpart.rules)[i]
}

out_rule
}