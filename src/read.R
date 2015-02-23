convert_binary <- function(d, column_numbers) {
  m <- as.matrix(d[,column_numbers])
  m[,] <- c("-"=-1, "+"=+1, "0"=0)[m]
  m <- apply(m, 2, as.numeric)
  d <- cbind(d[,-column_numbers,drop=F], m)
  return(d)
}

which_feature_names <- function(d) {
  return(grep("^_F", names(d)))
}

feature_names <- function(d) {
  result <- names(d)[which_feature_names(d)]
  return(result)
}

feature_matrix <- function(d, fnames=NULL) {
  if (is.null(fnames)) {
    fnames <- feature_names(d)
  }
  return(as.matrix(d[,fnames]))
}

prefix_feature_names <- function(s, prefix) {
  return(paste0("_F", prefix, "___", s))
}

prefix_feature_names_raw <- function(d, prefix) {
  fcols <- !names(d) %in% c("label", "label_std")
  names(d)[fcols] <- prefix_feature_names(names(d)[fcols], prefix)
  return(d)
}

undo_feature_prefix <- function(x) {
  splits <- strsplit(x, "___")
  prefix <- splits[[1]][1]
  fnames <- sapply(splits, function(v) v[2])
  return(list(prefix, fnames))
}

replace_features <- function(d, f) {
  if (ncol(f) == length(feature_names(d))) {
    d[,feature_names(d)] <- f
  } else {
    d_resid <- d[,!names(d) %in% feature_names(d)]
    feat_prefix <- undo_feature_prefix(feature_names(d))[[1]]
    colnames(f) <- prefix_feature_names(as.character(1:ncol(f)), feat_prefix) 
    d_feat <- as.data.frame(f)
    d <- cbind(d_resid, d_feat)
  }
  return(d)
}

intersect_labels <- function(l) { # DEPRECATED
  label_intersect <- intersect(as.character(l[[1]]$label_std),
                               as.character(l[[2]]$label_std))
  if (length(l) > 2) {
    for (i in 3:length(l)) {
      label_intersect <- intersect(label_intersect,
                                   as.character(l[[i]]$label_std))
    }
  }
  if (!is.null(names(l))) {
    index <- names(l)
  } else {
    index <- 1:length(l)
  }
  for (n in index) {
    l[[n]] <- l[[n]][as.character(l[[n]]$label_std) %in% label_intersect,]
    l[[n]] <- l[[n]][order(l[[n]]$label_std),]
    l[[n]]$label_std <- as.character(l[[n]]$label_std)
  }
  return(l)
}

merge_phones <- function(f1, f2) {
  l <- intersect_labels(list(f1, f2))
  return(merge(l[[1]], l[[2]], by="label_std"))
}

transform_features <- function(d, trans=NULL, normalize=NULL) {
  m <- feature_matrix(d)
  if (!is.null(trans)) {
    m <- feature_matrix(d) %*% trans
  }
  if (identical(normalize, "pca")) {
    m <- prcomp(m, scale=TRUE)$x
  } else if (identical(normalize, "zscore")) {
    m_col_mean <- apply(m, 1, mean)
    m_col_sd <- apply(m, 1, sd)
    m <- sweep(m, 1, m_col_mean, "-")
    m <- sweep(m, 1, m_col_sd, "/")
  }
  result <- replace_features(d, m)
  return(result)
}

read_features <- function(fn, label_map, trans_fn, normalize, feature_prefix) {
  d <- read.csv(fn)
  d <- prefix_feature_names_raw(d, feature_prefix)
  if (!is.null(label_map)) {
    d$label_std <- label_map[as.character(d$label)]
  } else {
    d$label_std <- d$label
  }
  d <- d[!is.na(d$label_std),]
  if (!is.null(trans_fn)) {
    trans_m <- as.matrix(read.csv(trans_fn, header=F))
  } else {
    trans_m <- NULL
  }
  d <- transform_features(d, trans=trans_m, normalize=normalize)
  return(d)
}
