# Changing column names ---------------------------------------------------

#' Change column names of a dataframe
#'
#' @param df Dataframe
#' @param current_names Vector of column names to change.
#' @param new_names Vector of new names.
#'
#' @return Dataframe with new column names
#' @export
#'
#' @examples
#' \dontrun{
#' df <- data.frame(A = 1:2, B = 3:4, C = 5:6)
#' df <- change_colnames(df, c("A", "C"), c("Aa", "Cc"))
#' }
change_colnames <- function(df, current_names, new_names) {
  .Deprecated("Use `dplyr::rename(df, setNames(current_names, new_names))` instead.")

  stopifnot(
    is.data.frame(df),
    is.vector(current_names, mode = "character"),
    is.vector(new_names, mode = "character"),
    length(current_names) == length(new_names)
  )

  for (i in 1:length(current_names)) {
    colnames(df)[colnames(df) == current_names[i]] <- new_names[i]
  }
  return(df)
}
