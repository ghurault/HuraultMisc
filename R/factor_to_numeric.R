#' Change the type of the column of a dataframe from factor to numeric
#'
#' @param df dataframe
#' @param factor_name name of the factor to change into a numeric
#'
#' @return Same dataframe with type of the given column changed to numeric
#' @export
#'
#' @examples
factor_to_numeric <- function(df, factor_name) {

  df[, factor_name] <- as.numeric(levels(df[, factor_name]))[df[, factor_name]]
  return(df)

}
