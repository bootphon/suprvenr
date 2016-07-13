#' @importFrom foreach %dopar%
#' @importFrom dplyr %>%
NULL

#'Label pairs
#'@param lab1 Labels of left elements
#'@param lab2 Labels of right elements
#'@param collapse_main Separating string
#'@param collapse_sub String to replace \code{collapse_main} with if it appears
#'in lab1 or lab2 already
#'@return A character vector containing labels for the pairs
#'@export
pair_labels <- function(lab1, lab2, collapse_main="::", collapse_sub="_") {
  lab1 <- sub(collapse_main, collapse_sub, lab1)
  lab2 <- sub(collapse_main, collapse_sub, lab2)
  result <- paste(lab1, lab2, sep=collapse_main)
  return(result)
}

#'@export
encode_pairs <- function(test_pairs, encoding) {
 doParallel::registerDoParallel() 
 test_elems <- unique(c(test_pairs$x1, test_pairs$x2)) 
 test_elem_rows <- sapply(test_elems, function(e) which(encoding$label==e))
 m_pair <- foreach::foreach(row_1=test_elem_rows[test_pairs$x1],
                   row_2=test_elem_rows[test_pairs$x2],
                   .combine=rbind,
                   .final=function(m) matrix(m, nrow(m), ncol(m))) %dopar%
           (encoding$m[row_1,,drop=F] - encoding$m[row_2,,drop=F])
 return(encoding(m_pair, pair_labels(test_pairs$x1, test_pairs$x2),
                 encoding$fnames))
}

#'@export
compare_encoded_pairs <- function(encoding, similarity, similarity_param=NULL) {
  doParallel::registerDoParallel()
  row_pairs <- combn(nrow(encoding$m), 2)
  if (is.null(similarity_param)) {
    s <- foreach::foreach(row_1=row_pairs[1,], row_2=row_pairs[2,],
                .combine=c) %dopar%
        similarity(encoding$m[row_1,,drop=F], encoding$m[row_2,,drop=F])   
  } else if (length(similarity_param) == nrow(encoding$m)) {
    s <- foreach::foreach(row_1=row_pairs[1,], row_2=row_pairs[2,],
                .combine=c) %dopar%
        similarity(encoding$m[row_1,,drop=F], encoding$m[row_2,,drop=F],
                   similarity_param[c(row_1,row_2)])
  } else {
     s <- foreach::foreach(row_1=row_pairs[1,], row_2=row_pairs[2,],
                  .combine=c) %dopar%
          similarity(encoding$m[row_1,,drop=F], encoding$m[row_2,,drop=F],
                     similarity_param)    
  }
  labels1 <- encoding$label[row_pairs[1,]]
  labels2 <- encoding$label[row_pairs[2,]]
  result <- dplyr::data_frame(pair=pair_labels(labels1, labels2), similarity=s)
  return(result)
}

compare_test_pairs <- function(test_pairs) {
  doParallel::registerDoParallel()
  row_pairs <- combn(nrow(test_pairs), 2)
  s <- foreach::foreach(row_1=row_pairs[1,], row_2=row_pairs[2,], .combine=c) %dopar%
  {if (is.na(test_pairs[["fname"]][row_1]) ||
       is.na(test_pairs[["fname"]][row_2]) ||
       test_pairs[["fname"]][row_1] != test_pairs[["fname"]][row_2]) "Different"
    else "Same"}
  labels1 <- pair_labels(test_pairs$x1, test_pairs$x2)[row_pairs[1,]]
  labels2 <- pair_labels(test_pairs$x1, test_pairs$x2)[row_pairs[2,]]
  result <- dplyr::data_frame(pair=pair_labels(labels1, labels2), same_different=s)
  return(result)  
}

fname_test_pairs <- function(test_pairs) {
  doParallel::registerDoParallel()
  row_pairs <- combn(nrow(test_pairs), 2)
  labels1 <- pair_labels(test_pairs$x1, test_pairs$x2)[row_pairs[1,]]
  labels2 <- pair_labels(test_pairs$x1, test_pairs$x2)[row_pairs[2,]]
  fname1 <- test_pairs$fname[row_pairs[1,]]
  fname2 <- test_pairs$fname[row_pairs[2,]]
  result <- dplyr::data_frame(pair=pair_labels(labels1, labels2),
                              fname1=fname1, fname2=fname2)
  return(result)  
}

#' Conduct minimal pair tests
#' @description Conducts minimal pair tests jointly on the features defined in
#' \code{test_pairs} 
#' @param encoding an \code{\link{encoding}} object
#' @param test_pairs a data frame specifying the minimal pair tests: it contains a column
#' \code{fname} giving the feature that each pair is a minimal pair for, a column \code{x1}
#' giving the label of the left element of the pair, and a column \code{x2} giving the label 
#' of the right element of the pair
#' @param similarity a similarity function to apply to encoded pairs
#' @return a \code{\link{dplyr::tbl}} containing the results of minimal pair tests for all the
#' unique feature names in \code{test_pairs$fname} that are not \code{NA}; each feature is
#' tested by comparing all its pairs (all the pairs labelled with that feature name) to
#' each other, on the one hand, and to all the other pairs in \code{test_pairs}
#' (including pairs whose labels are \code{NA}), on the other; encodings of pairs will be compared
#' by \code{similarity}, where the encoding of a pair is \code{x1-x2}; the \code{tbl} will
#' have the following columns:
#' \itemize{
#'  \itemize{"fname"}{feature name}
#'  \itemize{"data"}{a nested \code{tbl} containing all the pairs that were taken in both the "same" (both \code{fname}) and
#'  "different" (only one \code{fname}) groups}
#'  \itemize{"roc"}{a nested \code{tbl} containing a ROC curve (three columns: \code{fpr}, false positive rate;
#'  \code{tpr}, the highest corresponding true positive rate; \code{crit}, the highest corresponding linear decision
#'  boundary)}
#'  \itemize{"auc"}{AUC (integral under \code{roc$tpr ~ roc$fpr})}
#' }
#' @export
joint_mptests <- function(encoding, test_pairs, similarity, similarity_param=NULL) {
  pairs <- encode_pairs(test_pairs, encoding)
  pairs_xy <- compare_encoded_pairs(pairs, similarity, similarity_param) %>%
    dplyr::left_join(compare_test_pairs(test_pairs)) %>%
    dplyr::left_join(fname_test_pairs(test_pairs)) %>%
    dplyr::mutate(direction="Default")
  negative_different_pairs <- pairs_xy %>%
    dplyr::filter(same_different == "Different") %>%
    dplyr::mutate(similarity=-similarity, direction="Reverse")
  pairs_all <- pairs_xy %>%
    dplyr::bind_rows(negative_different_pairs)
  result <- dplyr::data_frame(fname=unique((test_pairs %>% dplyr::filter(!is.na(fname)))[["fname"]]))
  result <- result %>% dplyr::mutate(data=purrr::map(fname, ~ dplyr::filter(pairs_all, fname1==. | fname2==.)))
  result <- result %>% dplyr::mutate(roc=purrr::map(data, ~ rocauc::pred_stats(.$similarity, factor(.$same_different))))
  result <- result %>% dplyr::mutate(auc=purrr::map_dbl(roc, ~ rocauc::auc(.$tpr, .$fpr)))
  return(result) 
}

#' @export
hyp_nodiff_mptest <- function(mptest_data, nreps) {
  doParallel::registerDoParallel()
  unlabeled_data <- dplyr::select(mptest_data, pair, similarity)
  same_diff_orig <- factor(mptest_data$same_different)
  iter_names <- paste0("Iter", 1:nreps)
  result <- foreach::foreach(iter=iter_names, .final=dplyr::bind_rows) %dopar%  {
      d_shuf <- sample_n(unlabeled_data, nrow(unlabeled_data))
      roc <- rocauc::pred_stats(d_shuf$similarity, same_diff_orig)
      auc <- rocauc::auc(roc$tpr, roc$fpr)
      data_frame(iter=iter, auc=auc)
  }
  return(result)
}

#' @export
hyp_nodiff_joint_mptests <- function(mptests, nreps) {
  result <- mptests %>% dplyr::transmute(fname=fname,
                                         auc_real=auc,
                                         hyp_nodiff_mptest=purrr::map(data, ~ hyp_nodiff_mptest(., nreps))) %>%
                        dplyr::mutate(pval=purrr::map2_dbl(auc_real, hyp_nodiff_mptest, ~ mean(abs(.x-0.5) < abs(.y$auc-0.5))))
  return(result)
}
