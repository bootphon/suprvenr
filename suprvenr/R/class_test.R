#' @importFrom dplyr %>%
NULL

#' Conduct class tests
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