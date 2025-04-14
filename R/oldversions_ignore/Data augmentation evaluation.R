library(dplyr)


# Multi Data augmentation -------------------------------------------------------

ListAll <- list.files('/Volumes/DJC Files/MultiSpeciesTransferLearning_R1/DataAugmentation_V3/modelruns_repeatsubset_multi',
          recursive = TRUE, full.names = TRUE )

PerformanceList <- 
  ListAll[str_detect(ListAll,'performance_tables_multi')]


PerformanceCombined <- as.data.frame(PerformanceList %>%
  map(read_csv,show_col_types = FALSE) %>%
  bind_rows())

tail(PerformanceCombined)

# Two values have AUC that is incorrect as there were no postive predictions
PerformanceCombined$AUC <- 
  ifelse(PerformanceCombined$AUC > 0.9, NA, PerformanceCombined$AUC)

# Assuming your dataframe is named 'results_df'
summary_df_aug <- as.data.frame(PerformanceCombined %>%
  group_by(`Training Data`, `CNN Architecture`, `Class`) %>%
  summarize(
    Max_AUC = max(AUC, na.rm = TRUE),
    Max_F1 = max(F1, na.rm = TRUE),
    SE_AUC = sd(AUC, na.rm = TRUE) / sqrt(sum(!is.na(AUC))),
    SE_F1 = sd(F1, na.rm = TRUE) / sqrt(sum(!is.na(F1)))
  ))

# View results
print(summary_df_aug)

ggscatter(data=summary_df_aug,x="CNN Architecture",
          y='Max_AUC',facet.by ="Training Data",
          color = 'Class')

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
                                  group_by(`Training Data`, `CNN Architecture`, `Class`) %>%
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
                                       group_by(`Training Data`, `CNN Architecture`, `Class`) %>%
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


rbind.data.frame(summary_df_aug, summary_df_grey_aug,summary_df_Crested_aug)

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

