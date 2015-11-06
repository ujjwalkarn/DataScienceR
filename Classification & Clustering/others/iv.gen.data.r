#' Create bigger dataset by replicating german data dataset
#'
#' Create bigger dataset by replicating german data dataset N times
#'
#' @param reps Number of replications
#' @export
#' @examples
#' german_data_big <- iv.gen.data(reps=1)

iv.gen.data <- function(reps=1) {
  german_data_big <- german_data[rep(1:nrow(german_data),reps),]
  rownames(german_data) <- seq_along(german_data[,1])
  german_data_big
}
