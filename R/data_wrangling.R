# Changing column names ---------------------------------------------------

#' Change column names of a dataframe
#'
#' @param df Dataframe
#' @param current_names Vector of column names to change
#' @param new_names Vector of new names
#'
#' @return Dataframe with new column names
#' @export
#'
#' @examples
#' df <- data.frame(A = 1:2, B = 3:4, C = 5:6)
#' df <- change_colnames(df, c("A", "C"), c("Aa", "Cc"))
change_colnames <- function(df, current_names, new_names) {

  stopifnot(is.data.frame(df),
            is.vector(current_names, mode = "character"),
            is.vector(new_names, mode = "character"),
            length(current_names) == length(new_names))

  for (i in 1:length(current_names)) {
    colnames(df)[colnames(df) == current_names[i]] <- new_names[i]
  }
  return(df)
}

# Convert factor to numeric -----------------------------------------------

#' Change the type of the column of a dataframe from factor to numeric
#'
#' @param df Dataframe
#' @param factor_name Vector of names of factors to change into numerics
#'
#' @return Same dataframe with type of the given columns changed to numeric
#' @export
#'
#' @examples
#' df <- data.frame(A = rep(1:5, each = 10))
#' df$A <- factor(df$A)
#' df <- factor_to_numeric(df, "A")
factor_to_numeric <- function(df, factor_name) {

  stopifnot(is.data.frame(df),
            is.vector(factor_name, mode = "character"))

  factor_name <- intersect(colnames(df), factor_name)
  if (length(factor_name) == 0) {
    warning("The intersection of columns names of ",
         as.character(df),
         " and ",
         as.character(substitute(df)),
         " is empty")
  } else {
    for (i in 1:length(factor_name)) {
      tmp <- df[, factor_name[i]]
      if (is.factor(tmp)) {
        df[, factor_name[i]] <- as.numeric(levels(tmp))[tmp]
      }
    }
  }

  return(df)
}
