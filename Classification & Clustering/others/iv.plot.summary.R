#' Plot summay information values for variables
#'
#' Plots calculated information values
#'
#' @param iv Information values summary for variables - output from \code{\link{iv.mult}} with \code{summary=TRUE}
#' @export
#' @examples
#' iv.plot.summary(iv.mult(german_data,"gb",TRUE))
#' iv.plot.summary(iv.mult(german_data,"gb",
#'      vars=c("ca_status","housing","job","duration"),summary=TRUE))

iv.plot.summary <- function(iv) {

  fills <- rev(brewer.pal(6, 'Blues'))
  Variable<-InformationValue<-Strength<-NULL
  ggplot(data=iv)+
    geom_bar(aes(x=reorder(Variable,InformationValue),y=InformationValue,fill=Strength), stat="identity") + 
    coord_flip() +
    scale_fill_manual(values=fills) +
    theme(
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(linetype="dashed",colour="grey"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()      
      ) +
     xlab("Variable") +
     ylab("Information Value") +
     ggtitle("Information Value Summary")   
}
