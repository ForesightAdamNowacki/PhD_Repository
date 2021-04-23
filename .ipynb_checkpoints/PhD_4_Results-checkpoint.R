library(tidyverse)
library(readr)

files_evaluation <- list.files(path = "D:/GitHub/PhD_Repository/Results_4", 
                               pattern = "evaluation",
                               full.names = TRUE); files_evaluation
files <- list()
for (i in 1:length(files_evaluation)){
  files[[i]] <- readr::read_csv(files_evaluation[i])
}

big_grid_evaluation <- files %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(X1 = NULL) %>%
  dplyr::filter(grepl("big_grid", Model_Name))

small_grid_evaluation <- files %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(X1 = NULL) %>%
  dplyr::filter(grepl("small_grid", Model_Name))

big_grid_evaluation %>%
  dplyr::mutate(Dataset = factor(Dataset, levels = c("train", "validation", "test"), ordered = TRUE)) %>%
  dplyr::group_by(Dataset) %>%
  dplyr::summarise(min = min(Accuracy),
                   mean = mean(Accuracy),
                   max = max(Accuracy))

colnames(big_grid_evaluation)
big_grid_evaluation %>%
  dplyr::mutate(Dataset = factor(Dataset, levels = c("train", "validation", "test"), ordered = TRUE)) %>%
  dplyr::group_by(Dataset, structuring_elements_depth)
;
