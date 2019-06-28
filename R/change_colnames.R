#' Change column names of a dataframe
#'
#' @param df dataframe
#' @param current_names vector of column names to change
#' @param new_names vector of new names
#'
#' @return dataframe with new column names
#' @export
#'
#' @examples
change_colnames <- function(df, current_names, new_names) {

  if (length(current_names) != length(new_names)){
    stop("The two input names should have the same length.")
  } else {
    for (i in 1:length(current_names)) {
      colnames(df)[colnames(df) == current_names[i]] <- new_names[i]
    }
    return(df)
  }

}
