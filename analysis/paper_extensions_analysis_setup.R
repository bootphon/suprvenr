source("../data/phone_mappings.R")

t_fns <- c("../data/art_aireggxdist_means.csv",
           "../data/timit_filterbanks_11.csv",
           "../data/buckeye_phones_sg.csv",
           "../data/buckeye_svd.txt",
           "../data/art_plus_timit11.csv")
t_trans_fns <- list(NULL, NULL, NULL, NULL, NULL)
t_normalize <- list("pca", "pca", "pca", "pca", "pca")
t_label_maps <- list(label_map_jm_art, label_map_timit, label_map_timit,
                     label_map_timit, NULL)
t_sim_fns <- list(abscossim, abscossim, abscossim, abscossim, abscossim)
#t_sim_fns <- list(minusabscosmagsim, minusabscosmagsim, minusabscosmagsim, minusabscosmagsim, minusabscosmagsim)

tn <- c("Articulatory", "Acoustic",  "Phonotactic (NN)", "Phonotactic (SVD)",
        "Artic + Acoustic")
names(t_fns) <- tn
names(t_trans_fns) <- tn
names(t_normalize) <- tn
names(t_label_maps) <- tn
names(t_sim_fns) <- tn

c_names <- c("V_Voice", "P_Coronal-Labial", "P_Labial-Dorsal",
             "P_Coronal-Dorsal", "M_Continuant", "M_Nasal")
c_fns <- c("../data/features_for_manual_voice_class.csv",
 "../data/features_for_manual_place_classes__labial_coronal_back_anterior.csv",
 "../data/features_for_manual_place_classes__labial_coronal_back_anterior.csv",
 "../data/features_for_manual_place_classes__labial_coronal_back_anterior.csv",
 "../data/features_for_manual_continuant_class.csv",
 "../data/features_for_manual_nasal_class.csv")
c_trans_fns <- list(NULL, NULL, NULL, NULL, NULL, NULL)
c_normalize <- list(NULL, NULL, NULL, NULL, NULL, NULL)
c_label_maps <- list(NULL, NULL, NULL, NULL, NULL, NULL)
c_sim_fns <- list(abscossim, abscossim, abscossim,
                  abscossim, abscossim, abscossim)
c_eps_others <- c(0.01, 0.67, 0.67, 0.67, 0.01, 0.01)
c_same_threshs <- c(1.0, 1.0, 1.0, 1.0, 1.0, 1.0)
c_class_features <- list("_Fc___voice",
                         c("_Fc___labial", "_Fc___coronal"),
                         c("_Fc___labial", "_Fc___back", "_Fc___anterior"),
                         c("_Fc___coronal", "_Fc___back", "_Fc___anterior"),
                         "_Fc___continuant",
                         "_Fc___nasal")

names(c_fns) <- c_names
names(c_trans_fns) <- c_names
names(c_normalize) <- c_names
names(c_sim_fns) <- c_names
names(c_eps_others) <- c_names
names(c_same_threshs) <- c_names
names(c_class_features) <- c_names
