# LAB
# L - przeksztalcenia morfologiczne
# Dodane A i B do stosu
library(tidyverse)
library(readr)

files_evaluation <- list.files(path = "D:/GitHub/PhD_Repository/Results_6", 
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

# 1.
dplyr::bind_rows(small_grid_evaluation %>%
                   dplyr::mutate(Dataset = factor(Dataset, levels = c("train", "validation", "test"), ordered = TRUE)) %>%
                   dplyr::group_by(Dataset, morphological_transformation_mode) %>%
                   dplyr::summarise(min = min(Accuracy),
                                    mean = mean(Accuracy),
                                    max = max(Accuracy), 
                                    n = n()) %>%
                   dplyr::ungroup() %>%
                   dplyr::mutate(Type = "without morphological transformation"),
                 big_grid_evaluation %>%
                   dplyr::mutate(Dataset = factor(Dataset, levels = c("train", "validation", "test"), ordered = TRUE)) %>%
                   dplyr::group_by(Dataset, morphological_transformation_mode) %>%
                   dplyr::summarise(min = min(Accuracy),
                                    mean = mean(Accuracy),
                                    max = max(Accuracy), 
                                    n = n()) %>%
                   dplyr::ungroup() %>%
                   dplyr::mutate(Type = "with morphological transformation")) %>%
  as.data.frame()

# 2.
dplyr::bind_rows(small_grid_evaluation %>%
                   dplyr::mutate(Dataset = factor(Dataset, levels = c("train", "validation", "test"), ordered = TRUE)) %>%
                   dplyr::group_by(Dataset, structuring_elements_depth) %>%
                   dplyr::summarise(min = min(Accuracy),
                                    mean = mean(Accuracy),
                                    max = max(Accuracy), 
                                    n = n()) %>%
                   dplyr::ungroup() %>%
                   dplyr::mutate(Type = "without morphological transformation"),
                 big_grid_evaluation %>%
                   dplyr::mutate(Dataset = factor(Dataset, levels = c("train", "validation", "test"), ordered = TRUE)) %>%
                   dplyr::group_by(Dataset, structuring_elements_depth) %>%
                   dplyr::summarise(min = min(Accuracy),
                                    mean = mean(Accuracy),
                                    max = max(Accuracy), 
                                    n = n()) %>%
                   dplyr::ungroup() %>%
                   dplyr::mutate(Type = "with morphological transformation")) %>%
  as.data.frame()

# 3.
dplyr::bind_rows(small_grid_evaluation %>%
                   dplyr::mutate(Dataset = factor(Dataset, levels = c("train", "validation", "test"), ordered = TRUE)) %>%
                   dplyr::group_by(Dataset, transormation_type) %>%
                   dplyr::summarise(min = min(Accuracy),
                                    mean = mean(Accuracy),
                                    max = max(Accuracy), 
                                    n = n()) %>%
                   dplyr::ungroup() %>%
                   dplyr::mutate(Type = "without morphological transformation"),
                 big_grid_evaluation %>%
                   dplyr::mutate(Dataset = factor(Dataset, levels = c("train", "validation", "test"), ordered = TRUE)) %>%
                   dplyr::group_by(Dataset, transormation_type) %>%
                   dplyr::summarise(min = min(Accuracy),
                                    mean = mean(Accuracy),
                                    max = max(Accuracy), 
                                    n = n()) %>%
                   dplyr::ungroup() %>%
                   dplyr::mutate(Type = "with morphological transformation")) %>%
  as.data.frame()

# 4.
dplyr::bind_rows(small_grid_evaluation %>%
                   dplyr::mutate(Dataset = factor(Dataset, levels = c("train", "validation", "test"), ordered = TRUE)) %>%
                   dplyr::group_by(Dataset, structuring_elements_type) %>%
                   dplyr::summarise(min = min(Accuracy),
                                    mean = mean(Accuracy),
                                    max = max(Accuracy), 
                                    n = n()) %>%
                   dplyr::ungroup() %>%
                   dplyr::mutate(Type = "without morphological transformation"),
                 big_grid_evaluation %>%
                   dplyr::mutate(Dataset = factor(Dataset, levels = c("train", "validation", "test"), ordered = TRUE)) %>%
                   dplyr::group_by(Dataset, structuring_elements_type) %>%
                   dplyr::summarise(min = min(Accuracy),
                                    mean = mean(Accuracy),
                                    max = max(Accuracy), 
                                    n = n()) %>%
                   dplyr::ungroup() %>%
                   dplyr::mutate(Type = "with morphological transformation")) %>%
  as.data.frame()

# 5.
dplyr::bind_rows(small_grid_evaluation %>%
                   dplyr::mutate(Dataset = factor(Dataset, levels = c("train", "validation", "test"), ordered = TRUE)) %>%
                   dplyr::group_by(Dataset, start_neurons) %>%
                   dplyr::summarise(min = min(Accuracy),
                                    mean = mean(Accuracy),
                                    max = max(Accuracy), 
                                    n = n()) %>%
                   dplyr::ungroup() %>%
                   dplyr::mutate(Type = "without morphological transformation"),
                 big_grid_evaluation %>%
                   dplyr::mutate(Dataset = factor(Dataset, levels = c("train", "validation", "test"), ordered = TRUE)) %>%
                   dplyr::group_by(Dataset, start_neurons) %>%
                   dplyr::summarise(min = min(Accuracy),
                                    mean = mean(Accuracy),
                                    max = max(Accuracy), 
                                    n = n()) %>%
                   dplyr::ungroup() %>%
                   dplyr::mutate(Type = "with morphological transformation")) %>%
  as.data.frame()

# 7.
dplyr::bind_rows(small_grid_evaluation %>%
                   dplyr::mutate(Dataset = factor(Dataset, levels = c("train", "validation", "test"), ordered = TRUE)) %>%
                   dplyr::group_by(Dataset, dense_neurons) %>%
                   dplyr::summarise(min = min(Accuracy),
                                    mean = mean(Accuracy),
                                    max = max(Accuracy), 
                                    n = n()) %>%
                   dplyr::ungroup() %>%
                   dplyr::mutate(Type = "without morphological transformation"),
                 big_grid_evaluation %>%
                   dplyr::mutate(Dataset = factor(Dataset, levels = c("train", "validation", "test"), ordered = TRUE)) %>%
                   dplyr::group_by(Dataset, dense_neurons) %>%
                   dplyr::summarise(min = min(Accuracy),
                                    mean = mean(Accuracy),
                                    max = max(Accuracy), 
                                    n = n()) %>%
                   dplyr::ungroup() %>%
                   dplyr::mutate(Type = "with morphological transformation")) %>%
  as.data.frame()

# ---------------------------------------------------------------------------- #
files_history <- list.files(path = "D:/GitHub/PhD_Repository/Results_6", 
                            pattern = "history",
                            full.names = TRUE); files_history
files <- list()
for (i in 1:length(files_history)){
  files[[i]] <- readr::read_csv(files_history[i])
}

big_grid_history <- files %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(X1 = NULL) %>%
  dplyr::filter(grepl("big_grid", model))

small_grid_history <- files %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(X1 = NULL) %>%
  dplyr::filter(grepl("small_grid", model))

# 1.
dplyr::bind_rows(big_grid_history %>%
                   dplyr::select(epoch, model, accuracy) %>%
                   dplyr::mutate(Type = "with morphological transformation"),
                 small_grid_history %>%
                   dplyr::select(epoch, model, accuracy) %>%
                   dplyr::mutate(Type = "without morphological transformation")) %>%
  ggplot2::ggplot(data = ., mapping = ggplot2::aes(x = epoch, y = accuracy, group = model, color = Type)) +
  ggplot2::geom_line() +
  ggplot2::labs(title = "Train accuracy")

# 2.
dplyr::bind_rows(big_grid_history %>%
                   dplyr::select(epoch, model, loss) %>%
                   dplyr::mutate(Type = "with morphological transformation"),
                 small_grid_history %>%
                   dplyr::select(epoch, model, loss) %>%
                   dplyr::mutate(Type = "without morphological transformation")) %>%
  ggplot2::ggplot(data = ., mapping = ggplot2::aes(x = epoch, y = loss, group = model, color = Type)) +
  ggplot2::geom_line() +
  ggplot2::labs(title = "Train loss")

# 3.
dplyr::bind_rows(big_grid_history %>%
                   dplyr::select(epoch, model, val_accuracy) %>%
                   dplyr::mutate(Type = "with morphological transformation"),
                 small_grid_history %>%
                   dplyr::select(epoch, model, val_accuracy) %>%
                   dplyr::mutate(Type = "without morphological transformation")) %>%
  ggplot2::ggplot(data = ., mapping = ggplot2::aes(x = epoch, y = val_accuracy, group = model, color = Type)) +
  ggplot2::geom_line() +
  ggplot2::labs(title = "Validation accuracy")

# 4.
dplyr::bind_rows(big_grid_history %>%
                   dplyr::select(epoch, model, val_loss) %>%
                   dplyr::mutate(Type = "with morphological transformation"),
                 small_grid_history %>%
                   dplyr::select(epoch, model, val_loss) %>%
                   dplyr::mutate(Type = "without morphological transformation")) %>%
  ggplot2::ggplot(data = ., mapping = ggplot2::aes(x = epoch, y = val_loss, group = model, color = Type)) +
  ggplot2::geom_line() +
  ggplot2::labs(title = "Validation loss")

# ---------------------------------------------------------------------------- #
# 1. Ogólnie wyniki lepsze z LAB pytkim, ni¿ RGB. Im glebsze LAB tym wynik gorszy