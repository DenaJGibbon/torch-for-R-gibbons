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
                                  recursive = TRUE, full.names = TRUE)


TestPerformanceListCSV <-lapply(unlist(PerformanceTestDirs), read.csv)
TestPerformanceListCSVCombined <-  bind_rows(TestPerformanceListCSV)
CrestedTestPerformanceListCSVCombined <- subset(TestPerformanceListCSVCombined,Class=='CrestedGibbons')
GreyTestPerformanceListCSVCombined  <- subset(TestPerformanceListCSVCombined,Class=='GreyGibbons')


CrestedTestPerformanceListCSVCombined$Class <- 'Crested Gibbons \n (multi)'
GreyTestPerformanceListCSVCombined$Class <- 'Grey Gibbons \n (multi)'

CombinedTestPerformance <- 
  rbind.data.frame(CrestedTestPerformanceListCSVCombined,
                   GreyTestPerformanceListCSVCombined)

CombinedTestPerformance <- CombinedTestPerformance %>%
  mutate(TrainingDataType = case_when(
    grepl("Noise", Training.Data) ~ "Noise",
    grepl("Cropping", Training.Data) ~ "Crop",
    grepl("copy", Training.Data) ~ "Duplicated",
    TRUE ~ "Original"
  ))

CombinedTestPerformance <- CombinedTestPerformance %>%
  mutate(jitter = case_when(
    grepl("_jitter", Training.Data) ~ "Jitter",
    grepl("_nojitter", Training.Data) ~ "No Jitter"
  ))

ModelInfo <-as.factor(str_split_fixed(CombinedTestPerformance$Training.Data,
                                      pattern = 'jitter', n=2)[,2])

CombinedTestPerformance$`N.epochs`  <- 
  as.factor(str_split_fixed(ModelInfo,
                            pattern = '_', n=3)[,2])

CombinedTestPerformance$CNN.Architecture <- 
  as.factor(str_split_fixed(ModelInfo,
                            pattern = '_', n=4)[,3])

CombinedTestPerformance$TrainingDataType <- 
  paste(CombinedTestPerformance$TrainingDataType, '\n', CombinedTestPerformance$jitter)

best_auc_per_training_data <- CombinedTestPerformance %>%
  group_by(Class,TrainingDataType,CNN.Architecture,N.epochs) %>%
  filter(AUC == max(AUC, na.rm = TRUE)) %>%
  ungroup()

# Now combine to check results
ggscatter(data=best_auc_per_training_data,
          x='TrainingDataType',y='AUC',
          facet.by=c('Class','CNN.Architecture'),
          color = 'N.epochs' 
)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  xlab('')

best_F1_per_training_data <- best_auc_per_training_data %>%
  group_by(Class) %>%
  filter(F1 == max(F1, na.rm = TRUE)) %>%
  ungroup()


as.data.frame(best_F1_per_training_data)

best_top1_per_training_data <- best_auc_per_training_data %>%
  group_by(Class) %>%
  filter(Top1Accuracy == max(Top1Accuracy, na.rm = TRUE)) %>%
  ungroup()


as.data.frame(best_top1_per_training_data)
