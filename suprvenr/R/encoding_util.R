#' Get all 1d sub-encodings
#' @param e An \code{\link{encoding}}
#' @param transformation A transformation to apply to each subencoding
#' @param ... Additional arguments to \code{transformation}
#' @return A \code{\link{dplyr::tbl}} containing one encoding for each
#' dimension of \code{e}, with a column \code{fname} labelling the
#' subencodings by the \code{fname} in the original encoding
#' @export
all_1d_subencodings <- function(e, transformation=NULL, ...) {
  result <- with(e,
                 dplyr::data_frame(
                   fname=fnames,
                   encoding=purrr::map(fnames,
                              ~ encoding(m[,.,drop=F], label, .))))
  
  return(result)
}
