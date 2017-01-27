#' @importFrom dplyr %>%
#' @importFrom foreach %dopar%
NULL

#' Conduct a class test
#' @description Conducts a class separation test on a single feature as defined in
#' \code{test_classes_f} 
#' @param encoding an \code{\link{encoding}} object
#' @param test_classes a data frame specifying the two classes: it contains a column
#' column \code{label} giving the label of the element,
#' and a column \code{value} giving the classification label
#' @param fit_and_predict_fn a function of three arguments: \code{x_tr}
#' (an N by d array of training vectors), \code{y_tr} (a vector of training labels),
#' \code{x_te} (a matrix or vector containing a point to classify), returning a
#' predicted label.
#' @return a \code{\link{dplyr::tbl}} containing classification scores,
#' having at least one column,  \code{avg_loo}, giving the average
#' leave-one-out classification score
#' @export
generic_test <- function(encoding, test_classes_f, fit_and_predict_fn) {
  registerDoParallel()
  d <- inner_join(as.tbl(encoding), test_classes_f, by="label")
  d$y <- factor(d$value)
  pred <- foreach(i=1:nrow(d), .combine=c) %dopar%
    (function(j) {
      x_tr <- d[-j,names(d) %in% encoding$fnames]
      y_tr <- d[-j,]$y
      x_te <- d[j,names(d) %in% encoding$fnames]
      return(fit_and_predict_fn(x_tr, y_tr, x_te))
    })(i)
  correct <- pred == as.character(d$y)
  result <- data_frame(avg_loo=mean(correct),
                       predictions=list(data_frame(label=d$label,
                                                   y=d$y,
                                                   pred=pred,
                                                   correct=correct)))
  return(result)
}

#' Conduct class tests for several features
#' @description Conducts class separation tests independently on the features defined in
#' \code{test_classes} 
#' @param encoding an \code{\link{encoding}} object
#' @param test_classes a data frame specifying the classes: it contains a column
#' \code{fname} giving the feature  a column \code{label} giving the label of the element,
#' and a column \code{value} giving the classification label
#' @param class_test_fn a function performing a classification test for a single binary
#' feature, returning a data frame of information about the class separation, containing
#' (at least) a column called \code{avg_loo}, giving the average of the 0-1 classification
#' scores for all points under leave-one-out
#' @return a \code{\link{dplyr::tbl}} containing the results of applying \code{class_test_fn}
#' for all the unique feature names in \code{test_classes$fname}; the \code{tbl} will
#' have the following columns (at least):
#' \itemize{
#'  \itemize{"fname"}{feature name}
#'  \itemize{"avg_loo"}{Average leave-one-out classification score}
#' }
#' @export
all_class_tests <- function(encoding, test_classes, class_test_fn) {
  test_classes %>%
    group_by(fname) %>%
    do(class_test=class_test_fn(encoding, .)) %>%
    ungroup() %>%
    unnest(class_test)
}