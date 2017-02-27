svm_fit_and_predict <- function(x_tr, y_tr, x_te) {
  nc_cols <- non_constant(x_tr)
  x_tr_ <- x_tr[,nc_cols]
  x_te_ <- x_te[,nc_cols]
  m <- e1071::svm(x_tr_, y_tr, cost=1, kernel="linear")
  return(as.character(predict(m, x_te_)))
}

#' Conduct a class test using SVM
#' @description Conducts a leave-one-out class separation test on a single
#' feature as defined in \code{test_classes_f} using SVM
#' @param encoding an \code{\link{encoding}} object
#' @param test_classes_f a data frame specifying the two classes: it contains a
#' column \code{label} giving the label of the element,
#' and a column \code{value} giving the classification label
#' @return a \code{\link{dplyr::tbl}} containing classification scores,
#' having at least one column,  \code{avg_loo}, giving the average
#' leave-one-out classification score
#' @export
svm_class_test <- function(encoding, test_classes_f, parallel=T) {
  return(generic_test(encoding, test_classes_f, svm_fit_and_predict,
                      parallel=parallel))
}
