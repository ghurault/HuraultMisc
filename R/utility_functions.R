# Pipe --------------------------------------------------------------------

#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

# Convert factor to numeric -----------------------------------------------

#' Change the type of the column of a dataframe from factor to numeric
#'
#' @param df Dataframe.
#' @param factor_name Vector of names of factors to change to numeric.
#'
#' @return Same dataframe with type of the given columns changed to numeric.
#' @export
#'
#' @examples
#' df <- data.frame(A = rep(1:5, each = 10))
#' df$A <- factor(df$A)
#' str(df)
#' df <- factor_to_numeric(df, "A")
#' str(df)
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
#' @param x Character vector.
#'
#' @return Dataframe with columns `Variable` and `Index.`
#' @noRd
#'
#' @examples
#' extract_index_1d(c("sigma", "sigma[1]", "sigma[1, 1]", "sigma[1][2]"))
extract_index_1d <- function(x) {

  stopifnot(is.vector(x, mode = "character"))

  out <- data.frame(Variable = as.character(x),
                    Index = NA)
  rg <- "(.*)\\[(\\d+)\\]$"
  # Identify variables ending in with a single number inside bracket
  id_var <- grep(rg, x)
  # Extract what's inside the bracket for Index and remove bracket for Variable
  out$Index[id_var] <- as.numeric(sub(rg, "\\2", x[id_var], perl = TRUE))
  out$Variable[id_var] <- sub(rg, "\\1", x[id_var], perl = TRUE)

  return(out)
}

#' Extract multiple indices inside bracket(s) as a list
#'
#' @param x Character vector.
#' @param dim_names Optional character vector of dimension names.
#' If `dim_names` is not `NULL`, if the elements of `x` don't have the same number of indices,
#' the missing indices will be set to `NA`.
#'
#' @return  Dataframe with columns:
#' - `Variable`, containing `x` where brackets have been removed
#' - `Index`, a list containing values within the brackets.
#' If `dim_names` is not NULL, `Index` is replaced by columns with names `dim_names` containing numeric values.
#'
#' @export
#'
#' @examples
#' extract_index_nd(c("sigma", "sigma[1]", "sigma[1, 1]", "sigma[1][2]"))
extract_index_nd <- function(x, dim_names = NULL) {

  stopifnot(is.vector(x, mode = "character"))

  out <- data.frame(Variable = as.character(x),
                    Index = NA) %>%
    extract_index_nbracket() %>%
    extract_index_1bracket()

  if (!is.null(dim_names)) {
    stopifnot(all(vapply(out[["Index"]], function(x) {is.numeric(x) || (is_scalar(x) & is.na(x))}, logical(1))))
    id_length <- vapply(out[["Index"]], length, numeric(1))
    stopifnot(max(id_length) == length(dim_names),
              is.character(dim_names))

    if (!is_scalar(unique(id_length))) {
      warning("The elements in x don't have the same number of indices. The last missing indices will be set to NA.")
      max_dim <- max(id_length)
      out[["Index"]] <- lapply(out[["Index"]],
                               function(x) {
                                 c(x, rep(NA, max_dim - length(x)))
                               })
    }

    tmp <- do.call(rbind, out[["Index"]]) %>%
      as.data.frame()
    colnames(tmp) <- dim_names
    out <- bind_cols(out["Variable"], tmp)
  }

  return(out)
}

#' Extract index when there is a unique bracket
#'
#' For example, patterns such as x[1], x[1,2], x[1,2, 3]
#' Not exported.
#'
#' @param df Dataframe with columns `Variable` and `Index.`
#'
#' @return Dataframe with columns `Variable` and `Index`
#' @noRd
extract_index_1bracket <- function(df) {

  # Identify variables with the corresponding pattern
  rg <- "^(.*)\\[(\\d+(,\\s?\\d+)*)\\]$"
  id_var <- grep(rg, df$Variable)
  # Extract what's inside the bracket and split at the comma
  df$Index[id_var] <- gsub(rg, "\\2", df$Variable[id_var], perl = TRUE) %>%
    strsplit(",") %>%
    lapply(as.numeric)
  # Rename variable
  df$Variable[id_var] <- gsub(rg, "\\1", df$Variable[id_var], perl = TRUE)

  return(df)
}

#' Extract index when there are multiple brackets
#'
#' For example, patterns such as x[1][2]
#' Not exported.
#'
#' @param df Dataframe with columns `Variable` and `Index`.
#'
#' @return Dataframe with columns `Variable` and `Index`.
#' @noRd
extract_index_nbracket <- function(df) {

  # Identify variables with the corresponding pattern
  rg <- "^(.*?)((\\[\\d+\\])+)"
  id_var <- grep(rg, df$Variable)
  # Remove prefix and split at the brackets
  df$Index[id_var] <- gsub(rg, "\\2", df$Variable[id_var]) %>%
    strsplit("[\\[\\]]", perl = TRUE) %>%
    lapply(function(x) {as.numeric(x[x != ""])})
  # Rename variable
  df$Variable[id_var] <- gsub(rg, "\\1", df$Variable[id_var], perl = TRUE)

  return(df)
}

# Logit and inverse logit -------------------------------------------------

#' Logit and Inverse logit
#'
#' @param x Numeric vector.
#'
#' @return Numeric vector.
#'
#' @name logit
#'
#' @examples
#' logit(0.5)
#' inv_logit(0)
NULL

#' @rdname logit
#' @export
logit <- function(x) {log(x / (1 - x))}

#' @rdname logit
#' @export
inv_logit <- function(x) {1 / (1 + exp(-x))}


# Approximate equal -------------------------------------------------------

#' Approximate equal
#'
#' Compute whether x and y are approximately equal given a tolerance level
#'
#' @param x Numeric scalar.
#' @param y Numeric scalar.
#' @param tol Tolerance.
#'
#' @return Boolean
#'
#' @name approx_equal
#'
#' @examples
#' approx_equal(1, 1)
#' 1 %~% (1 + 1e-16)
#' 1 %~% 1.01
NULL

#' @rdname approx_equal
#' @export
approx_equal <- function(x, y, tol = .Machine$double.eps^0.5) {
  abs(x - y) < tol
}

#' @rdname approx_equal
#' @export
`%~%` <- function(x, y) {approx_equal(x, y)}
