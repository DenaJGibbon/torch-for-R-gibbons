library(dplyr)
library(stringr)
library(tidyr)
library(ggpubr)
library(viridis)
library(gibbonNetR)

# Deploy trained models over new test data ----------------------------------------
test.data.path <- 'TestData/MaliauVietnamCombined/test/'

trained_models_dir <-  list.files('results/part3/DataAugmentation_V4/modelruns_repeatsubset_multi/',
                                  full.names = TRUE)

# NOTE: script below is commented out as only needs to run once
# for(a in 1:length(trained_models_dir)){
# 
#   output_dir <- paste('results/part3/TestOutput/multi_all_augmented_testdata_v1/', 
#         basename(trained_models_dir[a]),'/',sep='')
#   
#   dir.create(output_dir)
#   evaluate_trainedmodel_performance_multi(
#   trained_models_dir = trained_models_dir[a],
#   image_data_dir = test.data.path,
#   class_names = list.files(test.data.path),
#   output_dir = output_dir,
#   noise.category = "Noise"
# ) # Label for negative class
# 
# }


# Evaluate performance ---------------------------------------------

PerformanceTestDirs <- list.files('results/part3/TestOutput/multi_all_augmented_testdata_v1/',
                                  full.names = TRUE)

# Initialize empty dataframe to hold all test performance results
CombinedTestPerformanceAll <- data.frame()

# Loop through each performance output directory
for(i in 1:length(PerformanceTestDirs)){
  # Get list of all CSV files in the directory
  PerformanceTestDirsList <- list.files(PerformanceTestDirs[i], full.names = TRUE, recursive = TRUE)
  
  # Read in all CSVs and combine them
  TestPerformanceListCSV <- lapply(unlist(PerformanceTestDirsList), read.csv)
  TestPerformanceListCSVCombined <- bind_rows(TestPerformanceListCSV)
  
  # Subset by gibbon class and assign clearer names for plotting
  CrestedTestPerformanceListCSVCombined <- subset(TestPerformanceListCSVCombined, Class == 'CrestedGibbons')
  GreyTestPerformanceListCSVCombined <- subset(TestPerformanceListCSVCombined, Class == 'GreyGibbons')
  
  CrestedTestPerformanceListCSVCombined$Class <- 'Crested Gibbons \n (multi)'
  GreyTestPerformanceListCSVCombined$Class <- 'Grey Gibbons \n (multi)'
  
  # Combine class-specific results
  CombinedTestPerformance <- rbind.data.frame(CrestedTestPerformanceListCSVCombined,
                                              GreyTestPerformanceListCSVCombined)
  
  # Track source directory for traceability
  CombinedTestPerformance$DirectoryName <- basename(PerformanceTestDirs[i])
  CombinedTestPerformanceAll <- rbind.data.frame(CombinedTestPerformanceAll, CombinedTestPerformance)
}

# Extract and label metadata from directory names
CombinedTestPerformanceAll <- CombinedTestPerformanceAll %>%
  mutate(TrainingDataType = case_when(
    grepl("Noise", DirectoryName) ~ "Noise",
    grepl("Cropping", DirectoryName) ~ "Crop",
    grepl("copy", DirectoryName) ~ "Duplicated",
    TRUE ~ "Original"
  )) %>%
  mutate(jitter = case_when(
    grepl("_nojitter", DirectoryName) ~ "No Jitter",
    grepl("_jitter", DirectoryName) ~ "Jitter"
  ))

# Clean CNN architecture names
CombinedTestPerformanceAll$CNN.Architecture <- str_replace_all(CombinedTestPerformanceAll$CNN.Architecture, 'nojitter_', '')

# Extract number of epochs and architecture name
CombinedTestPerformanceAll$`N.epochs` <- as.factor(str_split_fixed(CombinedTestPerformanceAll$CNN.Architecture, '_', n = 3)[,1])
CombinedTestPerformanceAll$CNN.Architecture <- as.factor(str_split_fixed(CombinedTestPerformanceAll$CNN.Architecture, '_', n = 3)[,2])

# Combine training type and jitter label
CombinedTestPerformanceAll$TrainingDataType <- paste(CombinedTestPerformanceAll$TrainingDataType, '\n', CombinedTestPerformanceAll$jitter)

# Identify best AUC for each training setup
best_auc_per_training_data <- CombinedTestPerformanceAll %>%
  group_by(Class, TrainingDataType, CNN.Architecture, N.epochs) %>%
  filter(AUC == max(AUC, na.rm = TRUE)) %>%
  ungroup()

# Identify best F1 scores
best_F1_per_training_data <- best_auc_per_training_data %>%
  group_by(Class) %>%
  filter(F1 == max(F1, na.rm = TRUE)) %>%
  ungroup()

# Identify best Top-1 Accuracy
best_top1_per_training_data <- best_auc_per_training_data %>%
  group_by(Class) %>%
  filter(Top1Accuracy == max(Top1Accuracy, na.rm = TRUE)) %>%
  ungroup()


# Compare Original and Augmented training types
CompareOriginalandAugment <- subset(CombinedTestPerformanceAll,
                                    TrainingDataType == "Original \n No Jitter" |
                                      TrainingDataType == "Duplicated \n Jitter") %>%
  na.omit()

# Identify best AUC and F1
best_auc_Compare <- CompareOriginalandAugment %>%
  group_by(Class, TrainingDataType) %>%
  filter(AUC == max(AUC, na.rm = TRUE)) %>%
  filter(F1 == max(F1, na.rm = TRUE)) %>%
  slice_max(Threshold, n = 1) %>%
  ungroup()

# Check output
as.data.frame(best_auc_Compare)

# Select columns for reporting
best_auc_Compare[, c("TrainingDataType", "Class", "Precision", "Recall", "F1", "AUC", 
                     "Top1Accuracy", "N.epochs", "CNN.Architecture", "Threshold", "Class")]

# Refine best AUC for grouped comparison
best_auc_per_training_data <- CombinedTestPerformanceAll %>%
  group_by(CNN.Architecture, Class, TrainingDataType) %>%
  filter(AUC == max(AUC, na.rm = TRUE)) %>%
  filter(F1 == max(F1, na.rm = TRUE)) %>%
  slice_max(Threshold, n = 1) %>%
  ungroup()

# Reshape data for plotting
combined_long <- best_auc_per_training_data %>%
  select(CNN.Architecture, TrainingDataType, Class, F1, AUC) %>%
  pivot_longer(cols = c(F1, AUC), names_to = "Metric", values_to = "Value")

# Rename architecture levels
levels(combined_long$CNN.Architecture) <- c("AlexNet", "ResNet50")

# Define order of training types
desired_order <- c("Duplicated \n Jitter", "Original \n Jitter", "Original \n No Jitter",
                   "Duplicated \n No Jitter", "Noise \n No Jitter", "Crop \n No Jitter")
combined_long$TrainingDataType <- factor(combined_long$TrainingDataType, levels = desired_order)

# Final bar plot comparing metrics across setups
ggplot(combined_long, aes(x = Class, y = Value, fill = TrainingDataType)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  facet_grid(CNN.Architecture ~ Metric, scales = "free_y") +
  labs(title = "", x = "", y = "Metric") +
  scale_fill_manual(values = rev(viridis(6))) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        strip.text = element_text(face = "bold")) +
  guides(fill = guide_legend(title = NULL))
