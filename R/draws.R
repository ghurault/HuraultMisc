# Extract parameters from draws -------------------------------------------

#' Extract parameters' draws from an array
#'
#' Not exported.
#'
#' @param obj Array/Vector/Matrix (first dimension corresponds to draws).
#' @param draws Vector draws to extract.
#' @param parName Optional name to give to the parameter.
#'
#' @return Dataframe with columns: `Draw`, `Index` (for 1d vector), `Value`, `Parameter`.
#' @noRd
extract_draws_from_array <- function(obj, draws, parName = "") {

  stopifnot(is.vector(obj, mode = "numeric") || is.matrix(obj) || is.array(obj))

  obj <- as.array(obj)
  draws <- as.integer(draws)

  stopifnot(min(draws) >= 1,
            max(draws) <= dim(obj)[1])

  d <- dim(obj)
  if (length(d) == 1) {
    out <- data.frame(Draw = draws, Index = NA, Value = obj[draws], Parameter = parName)
  } else if (length(d) == 2) {
    # Faster implementation than code for length(d) > 2
    tmp <- obj[draws, ]
    if (length(draws) == 1) {
      out <- data.frame(Draw = draws, Index = 1:length(tmp), Value = tmp)
    } else {
      rownames(tmp) <- draws
      out <- reshape2::melt(tmp,
                            varnames = c("Draw", "Index"),
                            value.name = "Value")
    }
    out$Parameter <- parName
  } else {
    id_lbl <- paste0("i", 1:(length(d) - 1))
    out <- reshape2::melt(obj,
                          varnames = c("Draw", id_lbl),
                          value.name = "Value")
    out <- out[out[["Draw"]] %in% draws, ]
    lbl <- do.call(c, lapply(1:nrow(out), function(i) {paste0(out[i, id_lbl], collapse = ",")}))
    lbl <- paste0(parName, "[", lbl, "]")
    out$Index <- NA
    out[, id_lbl] <- NULL
    out$Parameter <- lbl
  }
  return(out)
}

#' Extract parameters' draws
#'
#' @param obj Array/Vector/Matrix of draws (cf. first dimension) or list of it.
#' @param draws Vector of draws to extract.
#'
#' @return Dataframe with columns: `Draw`, `Index`, `Value` and `Parameter`.
#' @export
#'
#' @examples
#' x <- rnorm(1e3)
#' X <- matrix(x, ncol = 10)
#' a <- array(rnorm(80), dim = c(10, 2, 2, 2))
#' extract_draws(x, sample(1:length(x), 10))
#' extract_draws(X, sample(1:nrow(X), 10)) %>% head()
#' extract_draws(a, sample(1:10, 5)) %>% head()
#' extract_draws(list(x = x, X = X, a = a), 1:10) %>% head()
extract_draws <- function(obj, draws) {

  if (!(inherits(obj, "list") || is.vector(obj, mode = "numeric") || is.matrix(obj) || is.array(obj))) {
    stop(as.character(substitute(obj)), " should be a vector or a matrix or a list of it")
  }

  if (is.list(obj)) {
    lapply(1:length(obj),
           function(i) {
             extract_draws_from_array(obj[[i]], draws, names(obj)[i])
           }) %>%
      bind_rows()
  } else {
    extract_draws_from_array(obj, draws)
  }
}
