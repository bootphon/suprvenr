image_with_labels <- function(d, labels_x=NULL, labels_y=NULL, ...) {
  m <- as.matrix(d)
  image(m, axes=F)
  if (!is.null(labels_x)) {
    nx <- length(labels_x)
    axis(1, at=(0:(nx-1))/(nx-1), lab=labels_x, ...)
  }
  if (!is.null(labels_y)) {
    ny <- length(labels_y)
    axis(2, at=(0:(ny-1))/(ny-1), lab=labels_y, las=1, ...)
  }
}

