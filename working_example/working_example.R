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
    ~ encoding(readr::read_csv(.), transformation=supervenr::zscore)
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

p <- ggplot(mptests_h0_summ, aes(fill=encoding_name, x=fname, y=pval)) +
  geom_bar(stat="identity", position="dodge",colour="black") +
  geom_hline(yintercept=0.05) +
  ggtitle("PCA with variance normalization, dimension = 10") +
  ylim(c(0,1)) +
  scale_fill_manual(values=emdplot::emd_palette(mptests_h0_summ$encoding_name)) +
  emdplot::emd_theme()
print(p)
