#' Proportion of variance explained by PCA to \code{k} #' dimensions
#' @param m A matrix
#' @param k Number of dimensions
#' @return Sum of the singular values up to \code{k}
#' @export
pca_variance_proportion <- function(m, k) {
  p <- prcomp(m, scale=T)
  if (k > length(p$sdev)) {
    stop(paste0("k is larger than the dimension of the PCA (", length(p$sdev), ")"))
  }
  svals <- p$sdev^2
  return(sum(svals[1:k])/sum(svals))
}
