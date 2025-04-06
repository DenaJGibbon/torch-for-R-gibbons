library(stringr)
library(ggpubr)
library(dplyr)
library(tidyr)
#library(gibbonNetR)
devtools::load_all("/Users/denaclink/Desktop/RStudioProjects/gibbonNetR")

# We focus on the smaller test set for computational efficiency
# Best performance using AUC- Grey binary ---------------------------------

GreyGibbonPerformance <- list.files('/Volumes/DJC Files/MultiSpeciesTransferLearning_R1/DataAugmentation_V3/modelruns_repeatsubset',
                                    full.names = T,recursive = T)

GreyGibbonPerformanceSub <- GreyGibbonPerformance[str_detect(GreyGibbonPerformance,'Danum')]

GreyGibbonPerformanceSub <- GreyGibbonPerformanceSub[str_detect(GreyGibbonPerformanceSub,'performance_tables')]

GreyGibbonPerformanceDirs <- unique(dirname(GreyGibbonPerformanceSub))

# Best performance using AUC- Crested binary ---------------------------------

CrestedGibbonPerformance <- list.files('/Volumes/DJC Files/MultiSpeciesTransferLearning_R1/DataAugmentation_V3/modelruns_repeatsubset_Jahoo',
                                    full.names = T,recursive = T)

CrestedGibbonPerformanceSub <- CrestedGibbonPerformance[str_detect(CrestedGibbonPerformance,'Jahoo')]

CrestedGibbonPerformanceSub <- CrestedGibbonPerformanceSub[str_detect(CrestedGibbonPerformanceSub,'performance_tables')]

CrestedGibbonPerformanceDirs <- unique(dirname(CrestedGibbonPerformanceSub))

# Best performance using AUC -Multi ----------------------------------------------
GreyMultiGibbonPerformance <- list.files('/Volumes/DJC Files/MultiSpeciesTransferLearning_R1/DataAugmentation_V4/modelruns_repeatsubset_multi',
                                         full.names = T,recursive = T)

GreyMultiGibbonPerformanceSub <- GreyMultiGibbonPerformance[str_detect(GreyMultiGibbonPerformance,'performance_tables_multi')]

GreyMultiGibbonPerformanceDirs <- unique(dirname(GreyMultiGibbonPerformanceSub))

# Best performance using AUC -Multi ----------------------------------------------
CrestedMultiGibbonPerformance <- list.files('/Volumes/DJC Files/MultiSpeciesTransferLearning_R1/DataAugmentation_V4/modelruns_repeatsubset_multi',
                                         full.names = T,recursive = T)

CrestedMultiGibbonPerformanceSub <- CrestedMultiGibbonPerformance[str_detect(CrestedMultiGibbonPerformance,'performance_tables_multi')]

CrestedMultiGibbonPerformanceDirs <- unique(dirname(CrestedMultiGibbonPerformanceSub))


# Now combine to check results -------------------------------------------------------------------------

GreyGibbonPerformanceList <- lapply(GreyGibbonPerformanceDirs,  list.files, full.names = TRUE)
GreyGibbonPerformanceListCSV <-lapply(unlist(GreyGibbonPerformanceList), read.csv)
GreyGibbonPerformanceCombined <-  bind_rows(GreyGibbonPerformanceListCSV)

CrestedGibbonPerformanceList <- lapply(CrestedGibbonPerformanceDirs,  list.files, full.names = TRUE)
CrestedGibbonPerformanceListCSV <-lapply(unlist(CrestedGibbonPerformanceList), read.csv)
CrestedGibbonPerformanceCombined <-  bind_rows(CrestedGibbonPerformanceListCSV)

CrestedGibbonMultiPerformanceList <- lapply(CrestedMultiGibbonPerformanceDirs,  list.files, full.names = TRUE)
CrestedGibbonMultiPerformanceListCSV <-lapply(unlist(CrestedGibbonMultiPerformanceList), read.csv)
CrestedGibbonMultiPerformanceCombined <-  bind_rows(CrestedGibbonMultiPerformanceListCSV)
CrestedGibbonMultiPerformanceCombined <- subset(CrestedGibbonMultiPerformanceCombined,Class=='CrestedGibbons')

GreyGibbonMultiPerformanceList <- lapply(GreyMultiGibbonPerformanceDirs,  list.files, full.names = TRUE)
GreyGibbonMultiPerformanceListCSV <-lapply(unlist(GreyGibbonMultiPerformanceList), read.csv)
GreyGibbonMultiPerformanceCombined <-  bind_rows(GreyGibbonMultiPerformanceListCSV)
GreyGibbonMultiPerformanceCombined <- subset(GreyGibbonMultiPerformanceCombined,Class=='GreyGibbons')

GreyGibbonPerformanceCombined$Class <- 'Grey Gibbons \n (binary)'
CrestedGibbonPerformanceCombined$Class <- 'Crested Gibbons \n (binary)'
CrestedGibbonMultiPerformanceCombined$Class <- 'Crested Gibbons \n (multi)'
GreyGibbonMultiPerformanceCombined$Class <- 'Grey Gibbons \n (multi)'


CombinedF1all <- 
  rbind.data.frame(GreyGibbonPerformanceCombined,CrestedGibbonPerformanceCombined,
                   CrestedGibbonMultiPerformanceCombined,GreyGibbonMultiPerformanceCombined)


CombinedF1all <- CombinedF1all %>%
  mutate(TrainingDataType = case_when(
    grepl("Noise", Training.Data) ~ "Noise",
    grepl("Cropping", Training.Data) ~ "Crop",
    grepl("copy", Training.Data) ~ "Duplicated",
    TRUE ~ "Original"
  ))

CombinedF1all <- CombinedF1all %>%
  mutate(jitter = case_when(
    grepl("_jitter", Training.Data) ~ "Jitter",
    grepl("_nojitter", Training.Data) ~ "No Jitter"
  ))


CombinedF1all$`N.epochs` <- 
  as.factor(CombinedF1all$`N.epochs`)

CombinedF1all$TrainingDataType <- 
  paste(CombinedF1all$TrainingDataType, '\n', CombinedF1all$jitter)

best_auc_per_training_data <- CombinedF1all %>%
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
  xlab('')+scale_color_manual(values = c('yellow','purple'))

# now try to find best max F1 given high AUC
best_F1_per_training_data <- best_auc_per_training_data %>%
  group_by(Class,TrainingDataType) %>%
  filter(F1 == max(F1, na.rm = TRUE)) %>%
  ungroup()

best_F1_per_training_data

ggscatter(data=best_F1_per_training_data,
          x='TrainingDataType',y='F1',
          facet.by=c('Class','CNN.Architecture'),
          color = 'N.epochs' 
)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  xlab('')+scale_color_manual(values = c('yellow','purple'))


