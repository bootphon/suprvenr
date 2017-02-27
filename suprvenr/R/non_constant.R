#' Finds non-constant dimensions
#' @description Return indexes of non-constant dimensions
#' @param m A matrix(-like)
#' @return indices of non-constant dimensions of \code{m}
#' @export
non_constant <- function(m) {
  return(which(apply(m, 2, function(x) max(x) != min(x))))
}
