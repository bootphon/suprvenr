#'Create \code{encoding} object
#'@description Create \code{encoding} object from a matrix
#'with \code{label} corresponding \code{label}, \code{m}
#'corresponding \code{m}, and \code{fname} corresponding to \code{fname}
#'@param m a matrix
#'@param label a string vector
#'@param fname a string vector
#'@param transformation a transformation that will be applied to \code{m}
#'@return See \code{\link{encoding}}
#'@export
encoding.matrix <- function(m, label, fnames, transformation=NULL, ...) {
  if (!is.null(transformation)) {
    m <- transformation(m, ...)
    fnames <- colnames(m)
  }
  result <- list(m=m, label=label, fnames=fnames)
  class(result) <- "encoding"
  return(result)  
}

#'Create \code{encoding} object
#'@description Create \code{encoding} object from \code{\link{data.frame}}
#'with \code{label} corresponding to a column called \code{label} in \code{d}, \code{m}
#'corresponding to the remaining columns, and \code{fname} corresponding to the
#'names of the remaining columns
#'@param d a \code{\link{data.frame}}
#'@param transformation a transformation that will be applied to \code{m}
#'@return See \code{\link{encoding}}
#'@export
encoding.data.frame <- function(d, transformation=NULL, ...) {
  if (!("label" %in% colnames(d))) {
    stop("label column not found in d")
  }
  label_col <- which(colnames(d)=="label")[1]
  return(encoding.matrix(as.matrix(d[,-label_col,drop=F]),
                         as.character(d[["label"]]),
                         colnames(d)[-label_col],
                         transformation, ...)) 
}

#'Create \code{encoding} object
#'@description Create \code{encoding} object from \code{\link{dplyr::tbl}} object
#'with \code{label} corresponding to a column called \code{label} in \code{d}, \code{m}
#'corresponding to the remaining columns, and \code{fname} corresponding to the
#'names of the remaining columns
#'@param d a \code{\link{dplyr::tbl}}
#'@param transformation a transformation that will be applied to \code{m}
#'@return See \code{\link{encoding}}
#'@export
encoding.tbl <- function(d, transformation=NULL, ...) {
  if (!("label" %in% colnames(d))) {
    stop("label column not found in d")
  }
  return(encoding.matrix(as.matrix(select(d, -label)),
                         as.character(d[["label"]]),
                         colnames(select(d, -label)),
                         transformation, ...))
}

#' Create \code{encoding} object
#'@description Generic method to create an \code{encoding} object
#'@return An object of class \code{encoding} with three elements: \code{m},
#'an \code{n} by \code{k} matrix of \code{n} objects coded in \code{k} features;
#'\code{label}, a string vector labelling the objects of \code{m}; \code{fname},
#'a string vector labelling the features of \code{m}
#'@export
encoding <- function(...) {
  UseMethod("encoding")
}

#' Get encoding corresponding to a label
#' @param e An \code{\link{encoding}}
#' @param label A character string containing a label
#' @return All the rows of the encoding matrix that have \code{label} as their
#' label (extra dimensions will be dropped to make it a vector if there is only one row)
#' @export
get.encoding <- function(e, label) {
  if (!(label %in% e$label)) {
    stop(paste0("No such label: ", label))
  }
  return (e$m[e$label==label,])
}