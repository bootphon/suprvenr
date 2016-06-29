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
  return(pcasds_variance_proportion(p$sdev)[k])
}

#' Proportion of variance explained by set of PCA sd's
#' @param sds A vector of sds as output by \code{\link{prcomp}}
#' @return Sum of the singular values up to \code{k}
#' @export 
pcasds_variance_proportion <- function(sds) {
  svals <- sds^2
  return(cumsum(svals)/sum(svals))
}
