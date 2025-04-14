# Load required packages --------------------------------------------------
library(dplyr)        # For data manipulation
library(tidyr)        # For pivot_longer
library(ggplot2)      # For plotting
library(stringr)      # For string operations (if used elsewhere)
library(viridis)      # For viridis color scales
library(flextable)    # For creating and exporting Word tables
library(readr)        
library(gibbonNetR)

# Deploy trained top model over WA test data ----------------------------------------
test.data.path <- 'data/CombinedImagesWAEvaluation/Danum/'

trained_models_dir <-  'data/CombinedImagesWAEvaluation/topmodels/DanumTopModel/'


output_dir <- paste('data/CombinedImagesWAEvaluation/final_test_WA/', 
                      basename(trained_models_dir),'/',sep='')
  
# dir.create(output_dir,recursive = TRUE)
#  NOTE: This is commented out because it just needs to run once 
# evaluate_trainedmodel_performance_multi(
#     trained_models_dir = trained_models_dir,
#     image_data_dir = test.data.path,
#     class_names = list.files(test.data.path),
#     output_dir = output_dir,
#     noise.category = "Noise"
#   ) # Label for negative class
#   


# Deply trained top model over WA test data ----------------------------------------
test.data.path <- 'data/CombinedImagesWAEvaluation/Jahoo/'

trained_models_dir <-  'data/CombinedImagesWAEvaluation/topmodels/JahooTopModel/'


output_dir <- paste('data/CombinedImagesWAEvaluation/final_test_WA/', 
                    basename(trained_models_dir),'/',sep='')

# dir.create(output_dir,recursive = TRUE)
#  NOTE: This is commented out because it just needs to run once 
# evaluate_trainedmodel_performance_multi(
#   trained_models_dir = trained_models_dir,
#   image_data_dir = test.data.path,
#   class_names = list.files(test.data.path),
#   output_dir = output_dir,
#   noise.category = "Noise"
# ) # Label for negative class
# 


# Combine and plot results ------------------------------------------------
# Load top-performing model results for Grey Gibbons (Danum)
DanumTop <- read.csv('data/CombinedImagesWAEvaluation/final_test_WA/DanumTopModel/performance_tables_multi_trained/Combined copy _jitter_5_resnet50_model_TransferLearningTrainedModel.csv')
DanumTop <- subset(DanumTop, Class == 'GreyGibbons')  # Filter to relevant class
max(na.omit(DanumTop$AUC))  # Check highest AUC
max(na.omit(DanumTop$F1))   # Check highest F1

# Load top-performing model results for Crested Gibbons (Jahoo)
JahooTop <- read.csv('data/CombinedImagesWAEvaluation/final_test_WA/JahooTopModel/performance_tables_multi_trained/Combined copy _jitter_5_resnet50_model_TransferLearningTrainedModel.csv')
JahooTop <- subset(JahooTop, Class == 'CrestedGibbons')  # Filter to relevant class
max(na.omit(JahooTop$AUC))  # Check highest AUC
max(na.omit(JahooTop$F1))   # Check highest F1

# Combine both dataframes
CombinedFinal <- rbind.data.frame(DanumTop, JahooTop)

# Calculate False Positive Rate from specificity
CombinedFinal$FPR <- 1 - CombinedFinal$Specificity

# Identify best performing rows for each class based on different metrics
best_by_f1 <- CombinedFinal %>%
  group_by(Class) %>%
  filter(F1 == max(F1, na.rm = TRUE)) %>%
  slice(1) %>%  # Select first row in case of tie
  mutate(Type = "Best F1")

best_by_precision <- CombinedFinal %>%
  group_by(Class) %>%
  filter(Precision == max(Precision, na.rm = TRUE)) %>%
  slice(1) %>%
  mutate(Type = "Best Precision")

best_by_recall <- CombinedFinal %>%
  group_by(Class) %>%
  filter(Recall == max(Recall, na.rm = TRUE)) %>%
  slice(1) %>%
  mutate(Type = "Best Recall")

# Combine all best rows and format the table
best_all <- bind_rows(best_by_f1, best_by_precision, best_by_recall) %>%
  select(Type, Class, CNN.Architecture, F1, AUC, Threshold, Precision, Recall, FPR) %>%
  mutate(
    F1 = round(F1, 2),
    AUC = round(AUC, 2),
    Threshold = round(Threshold, 2),
    Precision = round(Precision, 2),
    Recall = round(Recall, 2),
    FPR = round(FPR, 3)
  )

# Create and save a formatted Word table
best_all_flextable <- flextable::flextable(best_all)
flextable::save_as_docx(best_all_flextable,
                        path = 'results/Table 6. Final Model Results.docx')

# Reshape CombinedFinal for plotting metric trends across thresholds
CombinedFinal_long <- CombinedFinal %>%
  pivot_longer(cols = c(Precision, Recall, F1), 
               names_to = "Metric", values_to = "Score")

# Plot metric values across threshold values for each class
ggplot(CombinedFinal_long, aes(x = Threshold, y = Score, color = Metric, linetype = Metric)) +
  geom_line(size = 1) +
  facet_wrap(~ Class) +
  labs(x = "Threshold", y = "Score") +
  scale_color_manual(values = viridis(3)) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold"))