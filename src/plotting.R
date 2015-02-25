# palette from http://jfly.iam.u-tokyo.ac.jp/color/
default_colour_palette <- c(Nasal="#E69F00", Continuant="#56B4E9",
                            Coronal_Dorsal="#009E73", Other="#F0E442",
                            Labial_Dorsal="#0072B2", Voice="#D55E00",
                            Coronal_Labial="#CC79A7")

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

sim_d_to_distance_m <- function(d, row_col, col_col, sim_col, sim_to_dist_f) {
  library(reshape2, quietly=T, warn.conflicts=F)
  sim_m <- acast(d, formula(paste0(row_col, " ~ ", col_col)), value.var=sim_col)
  dist_m <- sim_to_dist_f(sim_m)
  result <- matrix(0., nrow(dist_m)+1, ncol(dist_m)+1)
  result[upper.tri(result)] <- dist_m[upper.tri(dist_m, diag=T)]
  result[lower.tri(result)] <- t(dist_m)[lower.tri(t(dist_m), diag=T)]
  diag(result) <- 0
  rownames(result) <- c(rownames(dist_m), colnames(dist_m)[ncol(dist_m)])
  colnames(result) <- c(rownames(dist_m)[1], colnames(dist_m))
  return(result)
}

mds_plot <- function(dist_m, mds_k=2, label_to_class=function(x) x,
                     palette=default_colour_palette, font_family=NULL) {
  library(MASS, quietly=T, warn.conflicts=F)
  mds_fit <- isoMDS(dist_m, k=mds_k, trace=F)
  mds_d <- as.data.frame(mds_fit$points)
  mds_d$label <- rownames(mds_fit$points)
  mds_d$class <- label_to_class(mds_d$label)
  rownames(mds_d) <- c()
  p <- ggplot(mds_d, aes(x=V1, y=V2, label=label, colour=class))
  if (!is.null(font_family)) {
    p <- p + geom_text(family=font_family)
  } else {
    p <- p + geom_text()
  }
  p <- p + scale_colour_manual(values=palette)
  return(p)
}

pair_label_to_paper_classes <- function(s) {
  PAPER_NASAL <- c("b_m", "d_n", "ɡ_ŋ", "m_b", "n_d", "ŋ_ɡ")
  PAPER_CONTINUANT <- c("b_v", "f_p", "d_z", "s_t", "v_b", "p_f", "z_d", "t_s")
  PAPER_CORDOR <- c("d_ɡ", "k_t", "n_ŋ", "ɡ_d", "t_k", "ŋ_n")
  PAPER_CORLAB <- c("b_d", "p_t", "f_s", "v_z", "m_n", "d_b", "t_p", "s_f",
                    "z_v", "n_m")
  PAPER_LABDOR <- c("b_ɡ", "k_p", "m_ŋ", "ɡ_b", "p_k", "ŋ_m")
  PAPER_VOICE <- c("b_p", "d_t", "ɡ_k", "f_v", "s_z", "ʃ_ʒ", "p_b", "t_d",
                   "k_ɡ", "v_f", "z_s", "ʒ_ʃ")
  if (as.character(s) %in% PAPER_NASAL) { "Nasal" }
  else if (as.character(s) %in% PAPER_CONTINUANT) { "Continuant" }
  else if (as.character(s) %in% PAPER_CORDOR) { "Coronal_Dorsal" }
  else if (as.character(s) %in% PAPER_CORLAB) { "Coronal_Labial" }
  else if (as.character(s) %in% PAPER_LABDOR) { "Labial_Dorsal" }
  else if (as.character(s) %in% PAPER_VOICE) { "Voice" }
  else { "Other" }
}
pair_label_to_paper_classes <- Vectorize(pair_label_to_paper_classes)