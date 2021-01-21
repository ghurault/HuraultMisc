# Predicates --------------------------------------------------------------

#' Test whether an object is of class "stanfit"
#'
#' @param obj Object
#'
#' @return Boolean
#' @noRd
is_stanfit <- function(obj) {
  any(class(obj) == "stanfit")
}

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
            is.character(factor_name))

  factor_name <- intersect(colnames(df), factor_name)
  if (length(factor_name) == 0) {
    warning("The intersection of columns names of df and factor_name is empty")
  } else {
    for (i in 1:length(factor_name)) {
      tmp <- df[[factor_name[i]]]
      if (is.factor(tmp)) {
        df[[factor_name[i]]] <- as.numeric(levels(tmp))[tmp]
      }
    }
  }

  return(df)
}

# Extract index inside bracket --------------------------------------------

#' Extract index inside bracket
#'
#' Not exported.
#'
#' @param x Character vector
#'
#' @return Dataframe with columns Variable and Index
#' @noRd
#'
#' @examples
#' extract_index_1d(c("sigma", "sigma[1]", "sigma[1, 1]", "sigma[1][2]"))
extract_index_1d <- function(x) {
  stopifnot(is.vector(x, mode = "character"))

  out <- data.frame(Variable = x, Index = NA)
  out$Variable <- as.character(out$Variable)
  re <- "(.*)\\[([0-9]+)\\]$"
  # Identify variables ending in with a single number inside bracket
  id_var <- grep(re, x)
  # Extract what's inside the bracket for Index and remove bracket for Variable
  out$Index[id_var] <- as.numeric(sub(re, "\\2", x[id_var], perl = TRUE))
  out$Variable[id_var] <- sub(re, "\\1", x[id_var], perl = TRUE)
  return(out)
}

#' Extract multiple indices inside bracket(s) as a list
#'
#' @param x Character vector
#'
#' @return  Dataframe with columns Variable and Index
#' @export
#'
#' @examples
#' extract_index_nd(c("sigma", "sigma[1]", "sigma[1, 1]", "sigma[1][2]"))
extract_index_nd <- function(x) {

  stopifnot(is.vector(x, mode = "character"))

  out <- data.frame(Variable = x, Index = NA)
  out$Variable <- as.character(out$Variable)

  ## Extract index in patterns such as x[1][2]
  # Identify the patterns
  re1 <- "^(.*)(\\[\\d+\\]){2,}"
  id_var <- grep(re1, out$Variable)
  # Remove prefix and split at the brackets
  re2 <- "^(\\w+)+(\\[.*\\])?$"
  out$Index[id_var] <- gsub(re2, "\\2", out$Variable[id_var]) %>%
    strsplit(., "[\\[\\]]", perl = TRUE) %>%
    lapply(., function(x) {as.numeric(x[x != ""])})
  # Rename variable
  out$Variable[id_var] <- gsub(re1, "\\1", out$Variable[id_var], perl = TRUE)

  ## Extract index in patterns such as x[1], x[1,2], x[1,2, 3]
  # Identify variables with the corresponding pattern
  re3 <- "^(.*)\\[(\\d(,\\s?\\d)*)\\]$"
  id_var <- grep(re3, out$Variable)
  # Extract what's inside the bracket and split at the comma
  out$Index[id_var] <- gsub(re3, "\\2", out$Variable[id_var], perl = TRUE) %>%
    strsplit(., ",") %>%
    lapply(., as.numeric)
  # Rename variable
  out$Variable[id_var] <- gsub(re3, "\\1", out$Variable[id_var], perl = TRUE)

  return(out)
}
