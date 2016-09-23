library(suprvenr)
library(readr)
library(tidyr)
library(dplyr)
library(purrr)

encodings <- readr::read_csv("encodings.csv")
test_pairs <- readr::read_csv("test_pairs.csv")
encodings <- encodings %>%
  mutate(encoding_value=purrr::map(
    filename,
    ~ encoding(readr::read_csv(.), transformation=supervenr::whiten_pca, k=2)
    ))
mptests <- encodings %>%
  transmute(encoding_name=encoding_name,
            mptests=purrr::map(encoding_value,
                               ~ joint_mptests(., test_pairs,
                                               similarity=cosabscossim,
                                               similarity_param=test_pairs$fname)
    )) 
mptests_summ <- mptests %>% unnest() %>% select(encoding_name, fname, auc)

mptests_h0 <- mptests %>%
  transmute(encoding_name=encoding_name,
            mptests_h0=purrr::map(mptests, ~ hyp_nodiff_joint_mptests(., nreps=500)
  ))
mptests_h0_summ <- mptests_h0 %>% unnest() %>% select(encoding_name, fname, auc_real, pval)
