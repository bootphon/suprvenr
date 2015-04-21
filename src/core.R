library(parallel)

inner <- function(m1, m2, FUN) {
  fun_on_rows <- function(i)  apply(m1, 1, function(v) FUN(v, m2[,i]))
  result <- sapply(1:ncol(m2), fun_on_rows)
  colnames(result) <- colnames(m2)
  return(result)
}

abscor <- function(v1, v2) {
  return(abs(cor(v1, v2)))
}

all_pairs <- function(m) {
  result <- matrix(0, m*(m-1)/2, m)
  n_tot <- 0
  for (i in 1:(m-1)) {
    num_other_items <- m - i
    result[n_tot + 1:num_other_items,i] <- 1
    result[n_tot + 1:num_other_items,i+1:num_other_items] <-
          diag(num_other_items)
    n_tot <- n_tot + num_other_items
  }
  result <- matrix(as.logical(result), dim(result)[1], dim(result)[2])
  return(result)
}

pair_labeler <- function(labels, collapse_main, collapse_sub) {
  function(pair_index_logical) {
    if (sum(pair_index_logical) != 2) {
      stop(paste0("bad pair index: ", toString(pair_index_logical)))
    }
    two_labels_raw <- labels[pair_index_logical]
    two_labels <- sub(collapse_main, collapse_sub, two_labels_raw)
    pair_label <- paste(two_labels, collapse=collapse_main)
    return(c(pair_label, two_labels[1], two_labels[2]))
  }
}

label_all_pairs <- function(pairs_m, collapse_main,
                            collapse_sub) {
  function (named_label_vector) {
    name <- attr(named_label_vector, "name")
    pair_labeler_this <- pair_labeler(named_label_vector,
                                      collapse_main,
                                      collapse_sub)
    label_triplets_m <- apply(pairs_m, 1, pair_labeler_this)
    pair_labels_d <- as.data.frame(t(label_triplets_m))
    names(pair_labels_d) <- paste0(name, c("_pair", "_1", "_2"))
    return(pair_labels_d)
  }
}

to_named_objects <- function(l) {
  return(lapply(names(l), function(n) {
                            attr(l[[n]], "name") <- n
                            return(l[[n]])
                          }
                )
         )
}

pairs_of <- function(m, label_vectors, FUN=base::"-", result_names=NULL, 
                     collapse_main="::", collapse_sub="_",
                     mc.cores=getOption("mc.cores", 2L)) {
  pairs_m <- all_pairs(nrow(m))
  pair_vals <- c()
  label_vectors_named <- to_named_objects(label_vectors)
  pair_label_ds <- lapply(label_vectors_named, label_all_pairs(pairs_m,
                                                               collapse_main,
                                                               collapse_sub))
  pair_label_d <- do.call("cbind", pair_label_ds)
  
  fun_by_row <- function(i) {
    pair <- m[pairs_m[i,],]
    return(FUN(pair[1,], pair[2,]))
  }
  pair_vals <- mcmapply(fun_by_row, 1:nrow(pairs_m), mc.cores=mc.cores)
  if (is.list(pair_vals)) {
    pair_vals_m <- simplify2array(pair_vals)[,,,drop=T]
    pair_vals_d <- as.data.frame(t(pair_vals_m))
  } else if (is.matrix(pair_vals)) {
    pair_vals_d <- as.data.frame(t(pair_vals))
  } else {
    pair_vals_d <- data.frame(pair_vals)
  }
  if (!is.null(result_names)) {
    names(pair_vals_d) <- result_names
  }
  result <- cbind(pair_vals_d, pair_label_d)
  return(result)
}

cos2sim <- function(x, y) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  dot <- x %*% y
  z <- norm(x,type="2")*norm(y,type="2")
  return((dot/z)^2)
}

abscossim <- function(x, y) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  dot <- x %*% y
  z <- norm(x,type="2")*norm(y,type="2")
  return(abs(dot/z))  
}

abscosmagdist <- function(x, y) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  dot <- x %*% y
  normx <- norm(x,type="2")
  normy <- norm(y,type="2")
  z <- normx*normy
  abscos <- max(0, 1. - abs(dot/z))
  absmagdiff <- abs(normx - normy)
  return(abscos*absmagdiff)
}

minusabscosmagsim <- function(x, y) {
  return(-abscosmagdist(x, y))
}

eucldist <- function(x, y, minus=F) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  result <- norm(x - y, type="2")
  if (minus) {
    result <- -result
  }
  return(result)
}

minuseucldist <- function(x, y) {
  return(eucldist(x,y,minus=T))
}

classify_by_sim <- function(sim, class_levels=c("Different", "Same"),
                            decision_bound=0.5) {
  classes_bool <- sim >= decision_bound
  classes_factor <- factor(class_levels[as.numeric(classes_bool) + 1],
                           levels=class_levels)
  return(classes_factor)
}


positive_prediction_stats <- function(classes, predictions,
                                      class_levels=c("Different", "Same")) {
  if (!is.factor(classes)) {
    classes <- factor(classes, levels=class_levels)
  }
  if (!is.factor(predictions)) {
    predictions <- factor(predictions, levels=class_levels)
  }
  classes_bool <- binary_factor_as_logical(classes)
  predictions_bool <- binary_factor_as_logical(predictions)
  all_pos <- sum(classes_bool)
  all_neg <- length(classes_bool) - all_pos
  true_pos <- sum(classes_bool & predictions_bool)
  false_pos <- sum(!classes_bool & predictions_bool)
  if (all_pos == 0) {
    tpr <- 0.0
  } else {
    tpr <- true_pos/all_pos
  }
  if (all_neg== 0) {
    fpr <- 1.0
  } else {
    fpr <- false_pos/all_neg
  }
  return(c(tpr=tpr, fpr=fpr))
}

minimal_diff_feat_rows <- function(m, max_total_remaining_diff=2) {
  result <- rowSums(abs(m) >= 2) == 1 &
            rowSums(abs(m) >= 1) < (2 + max_total_remaining_diff)
  return(rownames(m)[result])
}

to_multi_feature <- function(features) {
  return(paste(as.character(features), collapse=";;"))
}

from_multi_feature <- function(multi_feature) {
  return(strsplit(as.character(multi_feature), ";;"))
}

multi_feature_consistent_with_spec <- function(multi_feature, features) {
  all_in <- sapply(from_multi_feature(multi_feature),
                   function(x) prod(x %in% features))
  return(all_in)
}

multi_features_consistent <- function(multi_feature_1, multi_feature_2) {
  features_1 <- from_multi_feature(multi_feature_1)
  features_2 <- from_multi_feature(multi_feature_2)
  has_intersection <- sapply(1:length(features_1), function(i)
                       length(intersect(features_1[[i]], features_2[[i]])) > 0)
  return(has_intersection)
}

max_features <- function(m, id, id_col_name, first_tie=F) {
  id_col <- c()
  max_feat_val_col <- c()
  max_feat_name_col <- c()
  abs_m <- abs(m)
  for (i in 1:nrow(m)) {
    max_i <- max(abs_m[i,])
    all_max_index <- (1:ncol(m))[abs_m[i,] == max_i]
    if (first_tie) {
      all_max_index <- all_max_index[1]
    }
    all_feat_val <- m[i,all_max_index]
    all_names <- colnames(m)[all_max_index]
    id_col <- c(id_col, as.character(id[i]))
    max_feat_val_col <- c(max_feat_val_col, to_multi_feature(all_feat_val))
    max_feat_name_col <- c(max_feat_name_col, to_multi_feature(all_names))
  }
  result <- data.frame(max_feat_val=max_feat_val_col,
                       max_feat_name=max_feat_name_col)
  result[[id_col_name]] <- id_col
  return(result)
}
 
floateq <- function(x, y) {
  round(x - y, 9) == 0
}

gaus1d_bdr <- function(m1, m2, v1, v2, p1, p2) {
  A <- (1/v1 - 1/v2)
  if (floateq(v1, v2)) { # equal variance
    result <- (m1 + m2)/2 + v1*log(p1/p2)
  } else { 
    B <- -2*(m1/v1 - m2/v2)
    C <- (m1^2)/v1 - (m2^2)/v2 - 2*(log(v2/v1))*log(p1/p2)
    discriminant <- B^2 - 4*A*C
    if (discriminant < 0) {
      # I think this happens for numerical reasons when likelihood
      # is >> in one case
      return(NA)
    }
    result1 <- (-B + sqrt(discriminant))/(2*A)
    result2 <- (-B - sqrt(discriminant))/(2*A)
    if (result1 < min(m1, m2) | result1 > max(m1, m2)) {# too lazy to do algebra
      result <- result2
    } else {
      result <- result1
    }    
  }
  return(result)
}

either_feature_matches <- function(features, left, right) {
  left_f <- multi_feature_consistent_with_spec(left, features)
  right_f <- multi_feature_consistent_with_spec(right, features)
  return(left_f | right_f)
}

match_above_cutoff <- function(features, cutoff, sim, left, right) {
  if (sum(!either_feature_matches(features, left, right)) > 0) {
    stop()
  }  
  minimal <- multi_features_consistent(left, right) &
             (sim > cutoff | floateq(sim, cutoff))  
  result <- factor(ifelse(minimal, "2_Same", "1_Different"))
  return(result)
}

has_principal_contrast <- function(m, eps) {
  sum_feature <- apply(m, 1, function(x) sum(abs(x)))
  m_norm <- sweep(m, 1, sum_feature, FUN="/")
  max_feature <- apply(m_norm, 1, function(x) max(abs(x)))
  sum_other <- 1.0 - max_feature
  result <- sum_other <= eps
  return(result)
}

reduce_phones <- function(phones, labels) {
  result <- phones[as.character(phones$label_std) %in% labels,]
  result$label_std <- as.character(result$label_std)
  result <- result[order(result$label_std),]
  return(result)
}

construct_phone_pairs <- function(phones, sim, sim_col_name="sim") {
  phone_labels <- as.character(phones$label_std)
  m <- feature_matrix(phones, feature_names(phones))
  pairs <- pairs_of(m, list(phone=phone_labels))
  pairs_sim <- pairs_of(m, list(phone=phone_labels), sim, sim_col_name)
  result <- merge(pairs, pairs_sim)  
  return(result)
}

construct_merged_phone_pairs <- function(c_phones, t_phones, c_sim, t_sim) {
  merged_labels <- intersect(as.character(c_phones$label_std),
                             as.character(t_phones$label_std))
  c_phones <- reduce_phones(c_phones, merged_labels)
  t_phones <- reduce_phones(t_phones, merged_labels)
  c_pairs <- construct_phone_pairs(c_phones, c_sim, "c_sim")
  t_pairs <- construct_phone_pairs(t_phones, t_sim, "t_sim")
  result <- merge(c_pairs, t_pairs, by=c("phone_pair", "phone_1", "phone_2"))
  return(result)
}

construct_freps <- function(ct_phone_pairs, c_fnames, eps_other) {
  c_pairs_m <- feature_matrix(ct_phone_pairs, c_fnames)
  principal_contrast <- has_principal_contrast(c_pairs_m, eps_other)
  ct_freps <- ct_phone_pairs[principal_contrast,]
  c_freps_m <- feature_matrix(ct_freps, c_fnames)
  result <- merge(max_features(c_freps_m, ct_freps$phone_pair, "phone_pair"),
                  ct_freps)  
  return(result)
}

construct_frep_pairs <- function(ct_freps, c_fnames, t_fnames, c_sim, t_sim) {
  c_freps_m <- feature_matrix(ct_freps, c_fnames)
  t_freps_m <- feature_matrix(ct_freps, t_fnames)
  frep_pair_by <- with(ct_freps, list(phone_pair=as.character(phone_pair),
                          contr_feature=as.character(max_feat_name)))
  c_frep_pairs <- pairs_of(c_freps_m, frep_pair_by, FUN=c_sim,
                            result_names="c_sim")
  t_frep_pairs <- pairs_of(t_freps_m, frep_pair_by, FUN=t_sim,
                            result_names="t_sim")  
  result <- merge(t_frep_pairs, c_frep_pairs)  
  return(result)
}

relevant_frep_pairs <- function(ct_frep_pairs, class_features) {
  relevant <- either_feature_matches(class_features,
                                     ct_frep_pairs$contr_feature_1,
                                     ct_frep_pairs$contr_feature_2)
  result <- ct_frep_pairs[relevant,]  
  return(result)
}

frep_pairs_same_diff <- function(ct_frep_pairs, class_features, same_thresh) {
  result <- with(ct_frep_pairs,
                 match_above_cutoff(class_features, same_thresh, c_sim,
                                    contr_feature_1, contr_feature_2))
  return(result)
}

phones_to_freps <- function(c_phones, t_phones, c_sim, t_sim, eps_other) {
  merged_labels <- intersect(as.character(c_phones$label_std),
                             as.character(t_phones$label_std))
  c_phones <- reduce_phones(c_phones, merged_labels)
  t_phones <- reduce_phones(t_phones, merged_labels)
  c_phone_pairs <- construct_phone_pairs(c_phones, c_sim)  
  t_phone_pairs <- construct_phone_pairs(t_phones, t_sim)  
  ct_phone_pairs <- merge(c_phone_pairs, t_phone_pairs, by=c("phone_pair",
                                                             "phone_1",
                                                             "phone_2"))
  result <- construct_freps(ct_phone_pairs, feature_names(c_phones),
                            eps_other)
  return(result)
}

phones_to_relevant_frep_pairs <- function(c_phones, t_phones, c_sim, t_sim, 
                                          class_features, eps_other) {
  ct_freps <- phones_to_freps(c_phones, t_phones, c_sim, t_sim, eps_other)
  ct_frep_pairs <- construct_frep_pairs(ct_freps,
                                        feature_names(c_phones),
                                        feature_names(t_phones),
                                        c_sim, t_sim)
  result <- relevant_frep_pairs(ct_frep_pairs, class_features)  
  return(result)
}