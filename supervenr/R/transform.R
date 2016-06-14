#' Reduce number of dimensions
#' @description reduces to a fixed number of dimensions \code{k}, or does nothing if
#' \code{k} is \code{NULL} or larger than the number of columns of \code{m} - 1
#' @param m A matrix
#' @param k Fixed number of dimensions to return
#' @return The first \code{k} columns of \code{m}, or \code{m} if \code{k} is \code{NULL}
#' or larger than the number of columns of \code{m} - 1
reduce_to <- function(m, k=NULL) {
  if (!is.null(k)) {
    dims <- min(k, ncol(m))
    m <- m[,1:dims]
  }    
  return(m)
}

#' Apply PCA transform
#' @description Applies PCA and reduces to \code{k} dimensions; works by calling prcomp with z-scoring first
#' @param m A matrix
#' @param k Fixed number of dimensions (principal components) to return; defaults to
#' no dimensionality reduction if \code{k} is \code{NULL} - reducing by variance explained is not supported 
#' @return m decorrelated by PCA-rotation
#' @export
pca <- function(m, k=NULL) {
  return(reduce_to(prcomp(m, scale=T)$x, k))
}

#' Apply whitening PCA transform
#' @description Applies PCA and reduces to \code{k} dimensions, then divides out the variance
#' @param m A matrix
#' @param k Fixed number of dimensions (principal components) to return; defaults to
#' no dimensionality reduction if \code{k} is \code{NULL} - reducing by variance explained is not supported 
#' @return m whitened by PCA-rotation and z-scoring
#' @export
whiten_pca <- function(m, k=NULL) {
  return(scale(reduce_to(prcomp(m, scale=T)$x, k))[,])
}

#' Apply ZCA transform
#' @description Performs PCA on \code{m}, divides out the variance, and then
#' undoes the original PCA rotation
#' @param m A matrix
#' @param k Fixed number of dimensions (principal components) to return; defaults to
#' no dimensionality reduction if \code{k} is \code{NULL} - reducing by variance explained is not supported 
#' @return m whitened by PCA-rotation and z-scoring, and then re-rotated back to its original dimensions
#' @export
whiten_zca <- function(m, k=NULL) {
  p <- prcomp(m, scale=T)
  return(reduce_to(scale(p$x) %*% t(p$rotation), k))
}

#' Apply z-score transform
#' @description Centers and divides out the variance from \code{m}
#' @param m A matrix
#' @param k Fixed number of dimensions to return; defaults to
#' no dimensionality reduction if \code{k} is \code{NULL}
#' @return z-scored \code{m}
#' @export
zscore <- function(m, k=NULL) {
  return(reduce_to(scale(m), k))
}
