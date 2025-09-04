# Scalar ------------------------------------------------------------------

#' Test whether x is of length 1
#'
#' @param x Object to be tested.
#'
#' @return Logical
#'
#' @export
#'
#' @examples
#' is_scalar(1) # TRUE
#' is_scalar("a") # TRUE
#' is_scalar(c(1, 2)) # FALSE
is_scalar <- function(x) {
  length(x) == 1
}

# Wholenumber ------------------------------------------------------------

#' Test whether x is a whole number
#'
#' - `is_wholenumber()` uses [base::round()] to test whether `x` is a whole number,
#' it will therefore issue an error if `x` is not of mode numeric.
#' If used in [base::stopifnot()] for example, this won't be a problem but it may be in conditionals.
#' - `is_scalar_wholenumber()` comes with the additional argument `check_numeric`
#' to check whether `x` is a numeric before checking it is a whole number.
#'
#' @param x Object to be tested
#' @param tol Tolerance
#' @param check_numeric Whether to check whether `x` is a numeric
#' @param ... Arguments to pass to `is_wholenumber()`
#'
#' @return Logical
#'
#' @name is_wholenumber
#'
#' @examples
#' is_wholenumber(1) # TRUE
#' is_wholenumber(1.0) # TRUE
#' is_wholenumber(1.1) # FALSE
#' is_scalar_wholenumber(1) # TRUE
#' is_scalar_wholenumber(c(1, 2)) # FALSE
NULL

#' @rdname is_wholenumber
#' @export
is_wholenumber <- function(x, tol = .Machine$double.eps^0.5) {
  approx_equal(x, round(x), tol = tol)
}

#' @rdname is_wholenumber
#' @export
is_scalar_wholenumber <- function(x, check_numeric = TRUE, ...) {
  is_scalar(x) && ifelse(check_numeric, is.numeric(x), TRUE) && is_wholenumber(x, ...)
}

# Stanfit -----------------------------------------------------------------

#' Test whether an object is of class "stanfit"
#'
#' @param obj Object.
#'
#' @return Boolean
#' @export
is_stanfit <- function(obj) {
  any(class(obj) == "stanfit")
}
