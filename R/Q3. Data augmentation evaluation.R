library(dplyr)
library(purrr)
library(readr)
library(flextable)

# Multi Data augmentation -------------------------------------------------------

ListAll <- list.files('/Volumes/DJC Files/MultiSpeciesTransferLearning_R1/Benchmarking_random_variation_bigmodels/modelruns_repeatsubset_multi_updateAUC/',
          recursive = TRUE, full.names = TRUE )

PerformanceList <- 
  ListAll[str_detect(ListAll,'performance_tables_multi')]


PerformanceCombined <- as.data.frame(PerformanceList %>%
  map(read_csv,show_col_types = FALSE) %>%
  bind_rows())

tail(PerformanceCombined)

PerformanceCombined <- subset(PerformanceCombined,`CNN Architecture`=="alexnet"|
         `CNN Architecture`=="resnet50")

# Assuming your dataframe is named 'results_df'
summary_df_aug <- as.data.frame(PerformanceCombined %>%
  group_by(`Training Data`, `CNN Architecture`, `Class`, `N epochs`) %>%
  summarize(
    Max_AUC = max(AUC, na.rm = TRUE),
    Max_F1 = max(F1, na.rm = TRUE),
    SE_AUC = sd(AUC, na.rm = TRUE) / sqrt(sum(!is.na(AUC))),
    SE_F1 = sd(F1, na.rm = TRUE) / sqrt(sum(!is.na(F1)))
  ))

# View results
print(summary_df_aug)

summary_df_aug$`N epochs` <- as.factor(summary_df_aug$`N epochs`)

ggscatter(data=summary_df_aug,x="CNN Architecture",
          y='Max_AUC',facet.by ="Training Data",color ='N epochs',
          shape = 'Class')+scale_color_manual(values=c('purple','orange'))

# library(ggpubr)
# library(ggplot2)
# library(dplyr)
# 
# # Create a combined variable for color and shape
# summary_df_aug <- summary_df_aug %>%
#   mutate(combined_var = paste(Class, `N epochs`, sep = " - "))
# 
# # Plot using the combined variable
# ggscatter(summary_df_aug, 
#           x = "CNN Architecture", 
#           y = "Max_AUC", 
#           facet.by = "Training Data", 
#           color = "combined_var", 
#           shape = "combined_var") +
#   scale_color_manual(values = viridis(n=4)) +  
#   theme(legend.title = element_blank())  # Optional: Remove legend title

ggscatter(data=summary_df_aug,x="CNN Architecture",
          y='Max_F1',facet.by ="Training Data",
          color = 'Class')

# Danum data augmentation -------------------------------------------------------

ListAll <- list.files('/Volumes/DJC Files/MultiSpeciesTransferLearning_R1/DataAugmentation_V3/modelruns_repeatsubset/',
                      recursive = TRUE, full.names = TRUE )

PerformanceList <- 
  ListAll[str_detect(ListAll,'performance_tables')]


PerformanceCombined <- as.data.frame(PerformanceList %>%
                                       map(read_csv,show_col_types = FALSE) %>%
                                       bind_rows())

tail(PerformanceCombined)

# Assuming your dataframe is named 'results_df'
summary_df_grey_aug <- as.data.frame(PerformanceCombined %>%
                                  group_by(`Training Data`, `CNN Architecture`, `Class`, `N epochs`) %>%
                                  summarize(
                                    Max_AUC = max(AUC, na.rm = TRUE),
                                    Max_F1 = max(F1, na.rm = TRUE),
                                    SE_AUC = sd(AUC, na.rm = TRUE) / sqrt(sum(!is.na(AUC))),
                                    SE_F1 = sd(F1, na.rm = TRUE) / sqrt(sum(!is.na(F1)))
                                  ))

# View results
print(summary_df_grey_aug)

ggscatter(data=summary_df_grey_aug,x="CNN Architecture",
          y='Max_AUC',facet.by ="Training Data")+
  ggtitle('Grey gibbons')

ggscatter(data=summary_df_grey_aug,x="CNN Architecture",
          y='Max_F1',facet.by ="Training Data")+
  ggtitle('Grey gibbons')


# Jahoo data augmentation -------------------------------------------------------
  
  ListAll <- list.files('/Volumes/DJC Files/MultiSpeciesTransferLearning_R1/DataAugmentation_V3/modelruns_repeatsubset_Jahoo/',
                        recursive = TRUE, full.names = TRUE )

PerformanceList <- 
  ListAll[str_detect(ListAll,'performance_tables')]


PerformanceCombined <- as.data.frame(PerformanceList %>%
                                       map(read_csv,show_col_types = FALSE) %>%
                                       bind_rows())

tail(PerformanceCombined)

# Assuming your dataframe is named 'results_df'
summary_df_Crested_aug <- as.data.frame(PerformanceCombined %>%
                                       group_by(`Training Data`, `CNN Architecture`, `Class`, `N epochs`) %>%
                                       summarize(
                                         Max_AUC = max(AUC, na.rm = TRUE),
                                         Max_F1 = max(F1, na.rm = TRUE),
                                         SE_AUC = sd(AUC, na.rm = TRUE) / sqrt(sum(!is.na(AUC))),
                                         SE_F1 = sd(F1, na.rm = TRUE) / sqrt(sum(!is.na(F1)))
                                       ))

# View results
print(summary_df_Crested_aug)

ggscatter(data=summary_df_Crested_aug,x="CNN Architecture",
          y='Max_AUC',facet.by ="Training Data")+
  ggtitle('Crested gibbons')

ggscatter(data=summary_df_Crested_aug,x="CNN Architecture",
          y='Max_F1',facet.by ="Training Data")+
  ggtitle('Crested gibbons')

summary_df_aug$Class <- 
  paste(summary_df_aug$Class,'(Multi)')

summary_df_grey_aug$Class <- c('Grey (Binary)')

summary_df_Crested_aug$Class <- 
  c('Crested (Binary)')

CombineDF <- 
  rbind.data.frame(summary_df_aug, summary_df_grey_aug,summary_df_Crested_aug)

# Get the best AUC per class
library(dplyr)

best_auc_f1_per_class <- CombineDF %>%
  group_by(Class) %>%
  slice_max(order_by = Max_AUC, n = 1) %>%  # Selects row with highest AUC
 # slice_max(order_by = Max_F1, n = 1) %>%   # Among those, selects row with highest F1
  select(Class, `Training Data`, `CNN Architecture`,  Max_AUC)

# Print the results
print(best_auc_f1_per_class)

best_auc_f1_per_class$`Training Data` <- 
  plyr::revalue(best_auc_f1_per_class$`Training Data`,
              c("CombinedClipsSorted_AugmentedCropping _nojitter"="Cropped",
                "DanumClipsSorted_AugmentedNoise _nojitter"="Noise",
                "Jahoo copy _nojitter"= "Copy",
                "JahooClipsSorted_AugmentedCropping _nojitter"="Cropped",
                "JahooClipsSorted_AugmentedNoise _nojitter" ="Noise",
                "CombinedClipsSorted_AugmentedCropping"="Cropped"))

# Print the result
print(best_auc_f1_per_class)


best_auc_f1_per_class$Max_AUC <- round(best_auc_f1_per_class$Max_AUC,2)

best_auc_f1_per_class <- 
  best_auc_f1_per_class[-duplicated(best_auc_f1_per_class),]

CombinedMetricsTable <- flextable(best_auc_f1_per_class)
CombinedMetricsTable
#flextable::save_as_docx(CombinedMetricsTable,path='data/Table 2 Data augmentation performance.docx')

# Increase test data -----------------------------------------------

ListAll <- list.files("/Volumes/DJC Files/MultiSpeciesTransferLearning/InitialModelEvaluation/model_output_3/evaluation_WA",
                      recursive = TRUE, full.names = TRUE )

PerformanceList <- 
  ListAll[str_detect(ListAll,'performance_tables_multi_trained')]


PerformanceCombined <- as.data.frame(PerformanceList %>%
                                       map(read_csv,show_col_types = FALSE) %>%
                                       bind_rows())

tail(PerformanceCombined)

# PerformanceCombined <- 
#   subset(PerformanceCombined, `CNN Architecture` == 'alexnet_model' | `CNN Architecture` == 'resnet50_model')

# Assuming your dataframe is named 'results_df'
summary_df_increasetest <- as.data.frame(PerformanceCombined %>%
                              group_by(`Training Data`, `CNN Architecture`, `Class`) %>%
                              summarize(
                                Max_AUC = max(AUC, na.rm = TRUE),
                                Max_F1 = max(F1, na.rm = TRUE),
                                SE_AUC = sd(AUC, na.rm = TRUE) / sqrt(sum(!is.na(AUC))),
                                SE_F1 = sd(F1, na.rm = TRUE) / sqrt(sum(!is.na(F1)))
                              ))

# View results
print(summary_df_increasetest)


ggscatter(data=summary_df_increasetest,x="CNN Architecture",
          y='Max_AUC',facet.by ="Training Data",
          color = 'Class')

ggscatter(data=summary_df_increasetest,x="CNN Architecture",
          y='Max_F1',facet.by ="Training Data",
          color = 'Class')

