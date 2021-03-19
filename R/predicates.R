# Scalar ------------------------------------------------------------------

#' Test whether x is of length 1
#'
#' @param x Object to be tested
#'
#' @return Logical
#'
#' @export
#'
#' @examples
#' is_scalar(1) # TRUE
#' is_scalar("a") # TRUE
#' is_scalar(c(1, 2)) # FALSE
is_scalar <- function(x) {length(x) == 1}

# Wholenumber ------------------------------------------------------------

#' Test whether x is a whole number
#'
#' @param x Object to be tested
#' @param tol Tolerance
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
is_wholenumber <- function(x, tol = .Machine$double.eps^0.5) {abs(x - round(x)) < tol}

#' @rdname is_wholenumber
#' @export
is_scalar_wholenumber <- function(x, tol = .Machine$double.eps^0.5) {
  is_scalar(x) && is_wholenumber(x, tol)
}

# Stanfit -----------------------------------------------------------------

#' Test whether an object is of class "stanfit"
#'
#' @param obj Object
#'
#' @return Boolean
#' @export
is_stanfit <- function(obj) {
  any(class(obj) == "stanfit")
}
