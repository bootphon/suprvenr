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
    m <- m[,1:dims,drop=F]
  }    
  return(m)
}

#' Apply PCA transform
#' @description Applies PCA and reduces to \code{k} dimensions; works by calling prcomp with z-scoring first
#' @param m A matrix
#' @param k Fixed number of dimensions (principal components) to return
#' @param prop_var Select dimension by proportion of explained variance (need to explain
#' at least \code{prop_var}); overrides \code{k} if set
#' @return m decorrelated by PCA-rotation
#' @export
pca <- function(m, k=NULL, prop_var=NULL) {
  pc <- prcomp(m, scale=T)
  if (!is.null(prop_var)) {
    prop_var <- min(prop_var, 1.0)
    prop_vars_all <- pcasds_variance_proportion(pc$sdev) 
    k <- which(prop_vars_all >= prop_var)[1]
  }
  return(reduce_to(pc$x, k))
}

#' Apply z-score transform
#' @description Centers and divides out the variance from \code{m}
#' @param m A matrix
#' @param k Fixed number of dimensions to return; defaults to
#' no dimensionality reduction if \code{k} is \code{NULL}
#' @return z-scored \code{m}
#' @export
zscore <- function(m, k=NULL) {
  return(reduce_to(scale(m)[,,drop=F], k))
}

#' Apply whitening PCA transform
#' @description Applies PCA and reduces to \code{k} dimensions, then divides out the variance
#' @param m A matrix
#' @param k Fixed number of dimensions (principal components) to return
#' @param prop_var Select dimension by proportion of explained variance (need to explain
#' at least \code{prop_var}); overrides \code{k} if set
#' @return m whitened by PCA-rotation and z-scoring
#' @export
whiten_pca <- function(m, k=NULL, prop_var=NULL) {
  pc <- prcomp(m, scale=T)
  if (!is.null(prop_var)) {
    prop_var <- min(prop_var, 1.0)
    prop_vars_all <- pcasds_variance_proportion(pc$sdev) 
    k <- which(prop_vars_all >= prop_var)[1]
  }
  return(zscore(pc$x, k))
}

#' Apply ZCA transform
#' @description Performs PCA on \code{m}, divides out the variance, and then
#' undoes the original PCA rotation
#' @param m A matrix
#' @param k Fixed number of dimensions (principal components) to return
#' @param prop_var Select dimension by proportion of explained variance (need to explain
#' at least \code{prop_var}); overrides \code{k} if set
#' @return m whitened by PCA-rotation and z-scoring, and then re-rotated back to its original dimensions
#' @export
whiten_zca <- function(m, k=NULL) {
  pc <- prcomp(m, scale=T)
  if (!is.null(prop_var)) {
    prop_var <- min(prop_var, 1.0)
    prop_vars_all <- pcasds_variance_proportion(pc$sdev) 
    k <- which(prop_vars_all >= prop_var)[1]
  }
  return(reduce_to(scale(pc$x) %*% t(pc$rotation), k))
}

#' Center
#' @description Centers \code{m}
#' @param m A matrix
#' @param k Fixed number of dimensions to return; defaults to
#' no dimensionality reduction if \code{k} is \code{NULL}
#' @return Centered \code{m}
#' @export
center <- function(m, k=NULL) {
  col_means <- colMeans(m)
  result <- sweep(m, 2, col_means)
  return(reduce_to(result, k))
}
