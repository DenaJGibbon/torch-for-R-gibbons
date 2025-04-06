devtools::load_all("/Users/denaclink/Desktop/RStudioProjects/gibbonNetR")

# Update model runs for comparison ----------------------------------------
test.data.path <- '/Users/denaclink/Desktop/RStudioProjects/Gibbon-transfer-learning-multispecies/data/testimages/images_combined/test/'

trained_models_dir <-  list.files('/Volumes/DJC Files/MultiSpeciesTransferLearning_R1/DataAugmentation_V4/modelruns_repeatsubset_multi/',
                                  full.names = TRUE)

for(a in 1:length(trained_models_dir)){

  output_dir <- paste('/Volumes/DJC Files/MultiSpeciesTransferLearning_R1/TestOutput/multi_all_augmented_testdata/', 
        basename(trained_models_dir[a]),'/',sep='')
  
  dir.create(output_dir)
  evaluate_trainedmodel_performance_multi(
  trained_models_dir = trained_models_dir[a],
  image_data_dir = test.data.path,
  class_names = list.files(test.data.path),
  output_dir = output_dir,
  noise.category = "Noise"
) # Label for negative class

}


# Evaluate performance ---------------------------------------------

PerformanceTestDirs <- list.files('/Volumes/DJC Files/MultiSpeciesTransferLearning_R1/TestOutput/multi_all_augmented_testdata',
                                  full.names = TRUE)

CombinedTestPerformanceAll <- data.frame()

for(i in 1:length(PerformanceTestDirs)){

PerformanceTestDirsList <- list.files(PerformanceTestDirs[i],full.names = T,recursive = T)
  
TestPerformanceListCSV <-lapply(unlist(PerformanceTestDirsList), read.csv)
TestPerformanceListCSVCombined <-  bind_rows(TestPerformanceListCSV)
CrestedTestPerformanceListCSVCombined <- subset(TestPerformanceListCSVCombined,Class=='CrestedGibbons')
GreyTestPerformanceListCSVCombined  <- subset(TestPerformanceListCSVCombined,Class=='GreyGibbons')


CrestedTestPerformanceListCSVCombined$Class <- 'Crested Gibbons \n (multi)'
GreyTestPerformanceListCSVCombined$Class <- 'Grey Gibbons \n (multi)'

CombinedTestPerformance <- 
  rbind.data.frame(CrestedTestPerformanceListCSVCombined,
                   GreyTestPerformanceListCSVCombined)

CombinedTestPerformance$DirectoryName <- basename(PerformanceTestDirs[i])
CombinedTestPerformanceAll <- rbind.data.frame(CombinedTestPerformanceAll,CombinedTestPerformance)
}

CombinedTestPerformanceAll <- CombinedTestPerformanceAll %>%
  mutate(TrainingDataType = case_when(
    grepl("Noise", DirectoryName) ~ "Noise",
    grepl("Cropping", DirectoryName) ~ "Crop",
    grepl("copy", DirectoryName) ~ "Duplicated",
    TRUE ~ "Original"
  ))

CombinedTestPerformanceAll <- CombinedTestPerformanceAll %>%
  mutate(jitter = case_when(
    grepl("_nojitter", DirectoryName) ~ "No Jitter",
    grepl("_jitter", DirectoryName) ~ "Jitter"
  ))


CombinedTestPerformanceAll$CNN.Architecture <- 
  str_replace_all(CombinedTestPerformanceAll$CNN.Architecture,
                'nojitter_','')

CombinedTestPerformanceAll$`N.epochs`  <- 
  as.factor(str_split_fixed(CombinedTestPerformanceAll$CNN.Architecture,
                            pattern = '_', n=3)[,1])

CombinedTestPerformanceAll$CNN.Architecture <- 
  as.factor(str_split_fixed(CombinedTestPerformanceAll$CNN.Architecture,
                            pattern = '_', n=3)[,2])

CombinedTestPerformanceAll$TrainingDataType <- 
  paste(CombinedTestPerformanceAll$TrainingDataType, '\n', CombinedTestPerformanceAll$jitter)

best_auc_per_training_data <- CombinedTestPerformanceAll %>%
  group_by(Class,TrainingDataType,CNN.Architecture,N.epochs) %>%
  filter(AUC == max(AUC, na.rm = TRUE)) %>%
  ungroup()

# Now combine to check results
ggscatter(data=best_auc_per_training_data,
          x='TrainingDataType',y='AUC',
          facet.by=c('Class','CNN.Architecture'),
          color = 'N.epochs', scales='free', 
)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  xlab('')+scale_color_manual(values = c('yellow','purple'))

best_F1_per_training_data <- best_auc_per_training_data %>%
  group_by(Class) %>%
  filter(F1 == max(F1, na.rm = TRUE)) %>%
  ungroup()



ggscatter(data=best_F1_per_training_data,
          x='TrainingDataType',y='F1',
          facet.by=c('Class','CNN.Architecture'),
          color = 'N.epochs', 
)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  xlab('')+ylim(0,1)+scale_color_manual(values = c('yellow','purple'))


best_top1_per_training_data <- best_auc_per_training_data %>%
  group_by(Class) %>%
  filter(Top1Accuracy == max(Top1Accuracy, na.rm = TRUE)) %>%
  ungroup()


as.data.frame(best_top1_per_training_data)

CompareOriginalandAugment <- 
  subset(CombinedTestPerformanceAll,TrainingDataType=="Original \n No Jitter"|
         TrainingDataType=="Duplicated \n Jitter")

CompareOriginalandAugment <- na.omit(CompareOriginalandAugment)

best_auc_Compare <- CompareOriginalandAugment %>%
  group_by(Class, TrainingDataType) %>%
  filter(AUC == max(AUC, na.rm = TRUE)) %>%
  ungroup()

best_auc_Compare <- best_auc_Compare %>%
  group_by(Class, TrainingDataType) %>%
  filter(F1 == max(F1, na.rm = TRUE)) %>%
  slice_max(Threshold, n = 1) %>%  # or slice_max if you prefer highest threshold
  ungroup()

as.data.frame(best_auc_Compare)

best_auc_Compare[,c("TrainingDataType","Class","Precision", "Recall", "F1", "AUC", "Top1Accuracy",
                    "N.epochs", "CNN.Architecture",  "Threshold", "Class"
                     )]

best_auc_per_training_data <- CombinedTestPerformanceAll %>%
  group_by(Class,TrainingDataType) %>%
  filter(AUC == max(AUC, na.rm = TRUE)) %>%
  ungroup()


combined_long <- best_auc_per_training_data %>%
  select(TrainingDataType, Class, F1, AUC) %>%
  pivot_longer(cols = c(F1, AUC), names_to = "Metric", values_to = "Value")


ggplot(combined_long, aes(x = Class, y = Value, fill = TrainingDataType)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  # geom_text(aes(label = round(Value, 3)), 
  #           position = position_dodge(width = 0.8), 
  #           vjust = -0.5, size = 3.5) +
  facet_wrap(~ Metric, scales = "free_y") +
  labs(title = "",
       x = "", y = "Metric") +
  scale_fill_manual(values = rev(viridis(6))  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        strip.text = element_text(face = "bold"))+ guides(fill = guide_legend(title = NULL))
