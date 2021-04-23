# RGB vs LAB
library(tidyverse)
library(readr)

files_evaluation <- list.files(path = "D:/GitHub/PhD_Repository/Results_7", 
                               pattern = "evaluation",
                               full.names = TRUE); files_evaluation
files <- list()
for (i in 1:length(files_evaluation)){
  files[[i]] <- readr::read_csv(files_evaluation[i])
}

rgb_grid_evaluation <- files %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(X1 = NULL) %>%
  dplyr::filter(grepl("RGB", Model_Name))

lab_grid_evaluation <- files %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(X1 = NULL) %>%
  dplyr::filter(grepl("LAB", Model_Name))

results <- rgb_grid_evaluation %>%
  dplyr::rename(Accuracy_RGB = Accuracy) %>%
  dplyr::mutate(Model_Name = NULL) %>%
  dplyr::left_join(lab_grid_evaluation %>%
                     dplyr::mutate(Model_Name = NULL) %>%
                     dplyr::rename(Accuracy_LAB = Accuracy)) %>%
  dplyr::select(Dataset, epochs, start_neurons, dense_neurons, batch_size, augmentation, dplyr::starts_with("Accuracy")) 


results %>%
  group_by(Dataset) %>%
  dplyr::summarise(Min_Accuracy_RGB = min(Accuracy_RGB),
                   Mean_Accuracy_RGB = mean(Accuracy_RGB),
                   Median_Accuracy_RGB = median(Accuracy_RGB),
                   Max_Accuracy_RGB = max(Accuracy_RGB),
                   Min_Accuracy_LAB = min(Accuracy_LAB),
                   Mean_Accuracy_LAB = mean(Accuracy_LAB),
                   Median_Accuracy_LAB = median(Accuracy_LAB),
                   Max_Accuracy_LAB = max(Accuracy_LAB))

# LAB lepsze wyniki ni¿ na RGB
