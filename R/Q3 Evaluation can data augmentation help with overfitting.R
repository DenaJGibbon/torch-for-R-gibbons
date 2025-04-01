library(stringr)
library(ggpubr)
library(dplyr)
library(gibbonNetR)

# We focus on the smaller test set for computational efficiency

# Best performance using AUC- Grey binary ---------------------------------

GreyGibbonPerformance <- list.files('/Volumes/DJC Files/MultiSpeciesTransferLearning_R1/DataAugmentation_3epochs/modelruns_repeatsubset',
                                    full.names = T,recursive = T)

GreyGibbonPerformanceSub <- GreyGibbonPerformance[str_detect(GreyGibbonPerformance,'Danum')]

GreyGibbonPerformanceSub <- GreyGibbonPerformanceSub[str_detect(GreyGibbonPerformanceSub,'performance_tables')]

GreyGibbonPerformanceDirs <- unique(dirname(GreyGibbonPerformanceSub))

# Grey gibbons binary
PerformanceOutputGreyBinaryCombined <- data.frame()

for(a in 1:length(GreyGibbonPerformanceDirs)){
  PerformanceOutputGreyBinary <- gibbonNetR::get_best_performance(performancetables.dir=GreyGibbonPerformanceDirs[a],
                                                                  class='Gibbons',
                                                                  model.type = "binary", Thresh.val = 0)
  
  AUCDF <- as.data.frame(PerformanceOutputGreyBinary$best_auc)
  AUCDF$Category <- 'Best AUC'
  
  F1DF <- as.data.frame(PerformanceOutputGreyBinary$best_f1)
  F1DF$Category <- 'Best F1'
  
  TempRow <- rbind.data.frame(AUCDF,F1DF)
  PerformanceOutputGreyBinaryCombined <- rbind.data.frame(PerformanceOutputGreyBinaryCombined,TempRow)
}

head(PerformanceOutputGreyBinaryCombined)

PerformanceOutputGreyBinaryCombined_AUC <- 
  subset(PerformanceOutputGreyBinaryCombined,Category=='Best AUC')

ggboxplot(data=PerformanceOutputGreyBinaryCombined_AUC,
          color='CNN Architecture', y='AUC',x = 'Frozen',
          facet.by ='Training Data' )

PerformanceOutputGreyBinaryCombined_F1 <- 
  subset(PerformanceOutputGreyBinaryCombined,Category=='Best F1')

ggboxplot(data=PerformanceOutputGreyBinaryCombined_F1,
          color='CNN Architecture', y='F1',x = 'Frozen',
          facet.by ='Training Data' )

# Best performance using AUC- Crested binary ---------------------------------

CrestedGibbonPerformance <- list.files('/Volumes/DJC Files/MultiSpeciesTransferLearning_R1/DataAugmentation_3epochs/modelruns_repeatsubset_Jahoo',
                                    full.names = T,recursive = T)

CrestedGibbonPerformanceSub <- CrestedGibbonPerformance[str_detect(CrestedGibbonPerformance,'Jahoo')]

CrestedGibbonPerformanceSub <- CrestedGibbonPerformanceSub[str_detect(CrestedGibbonPerformanceSub,'performance_tables')]

CrestedGibbonPerformanceDirs <- unique(dirname(CrestedGibbonPerformanceSub))

# Crested gibbons binary
PerformanceOutputCrestedBinaryCombined <- data.frame()

for(a in 1:length(CrestedGibbonPerformanceDirs)){
  PerformanceOutputCrestedBinary <- gibbonNetR::get_best_performance(performancetables.dir=CrestedGibbonPerformanceDirs[a],
                                                                  class='Gibbons',
                                                                  model.type = "binary", Thresh.val = 0)
  
  AUCDF <- as.data.frame(PerformanceOutputCrestedBinary$best_auc)
  AUCDF$Category <- 'Best AUC'
  
  F1DF <- as.data.frame(PerformanceOutputCrestedBinary$best_f1)
  F1DF$Category <- 'Best F1'
  
  TempRow <- rbind.data.frame(AUCDF,F1DF)
  PerformanceOutputCrestedBinaryCombined <- rbind.data.frame(PerformanceOutputCrestedBinaryCombined,TempRow)
}

head(PerformanceOutputCrestedBinaryCombined)

PerformanceOutputCrestedBinaryCombined_AUC <- 
  subset(PerformanceOutputCrestedBinaryCombined,Category=='Best AUC')

ggboxplot(data=PerformanceOutputCrestedBinaryCombined_AUC,
          color='CNN Architecture', y='AUC',x = 'Frozen',
          facet.by ='Training Data' )

PerformanceOutputCrestedBinaryCombined_F1 <- 
  subset(PerformanceOutputCrestedBinaryCombined,Category=='Best F1')

ggboxplot(data=PerformanceOutputCrestedBinaryCombined_F1,
          color='CNN Architecture', y='F1',x = 'Frozen',
          facet.by ='Training Data' )

# Best performance using AUC -Multi ----------------------------------------------
GreyMultiGibbonPerformance <- list.files('/Volumes/DJC Files/MultiSpeciesTransferLearning_R1/DataAugmentation_3epochs/modelruns_repeatsubset_multi',
                                         full.names = T,recursive = T)

GreyMultiGibbonPerformanceSub <- GreyMultiGibbonPerformance[str_detect(GreyMultiGibbonPerformance,'performance_tables_multi')]

GreyMultiGibbonPerformanceDirs <- unique(dirname(GreyMultiGibbonPerformanceSub))

# GreyMulti gibbons 
PerformanceOutputGreyMultiCombined <- data.frame()

for(a in 1:length(GreyMultiGibbonPerformanceDirs)){
  
  PerformanceOutputGreyMulti <- gibbonNetR::get_best_performance(performancetables.dir=GreyMultiGibbonPerformanceDirs[a],
                                                                 class='GreyGibbons',
                                                                 model.type = "multi", Thresh.val = 0)
  
  AUCDF <- as.data.frame(PerformanceOutputGreyMulti$best_auc)
  AUCDF$Category <- 'Best AUC'
  
  F1DF <- as.data.frame(PerformanceOutputGreyMulti$best_f1)
  F1DF$Category <- 'Best F1'
  
  TempRow <- rbind.data.frame(AUCDF,F1DF)
  PerformanceOutputGreyMultiCombined <- rbind.data.frame(PerformanceOutputGreyMultiCombined,TempRow)
}

head(PerformanceOutputGreyMultiCombined)


PerformanceOutputGreyMultiCombined_AUC <- 
  subset(PerformanceOutputGreyMultiCombined,Category=='Best AUC')

ggboxplot(data=PerformanceOutputGreyMultiCombined_AUC,
          color='CNN Architecture', y='AUC',x = 'Frozen',
          facet.by ='Training Data' )

PerformanceOutputGreyMultiCombined_F1 <- 
  subset(PerformanceOutputGreyMultiCombined,Category=='Best F1')

ggboxplot(data=PerformanceOutputGreyMultiCombined_F1,
          color='CNN Architecture', y='F1',x = 'Frozen',
          facet.by ='Training Data' )+ggtitle("Grey Gibbon")

# Best performance using AUC -Multi ----------------------------------------------
CrestedMultiGibbonPerformance <- list.files('/Volumes/DJC Files/MultiSpeciesTransferLearning_R1/DataAugmentation_3epochs/modelruns_repeatsubset_multi',
                                         full.names = T,recursive = T)

CrestedMultiGibbonPerformanceSub <- CrestedMultiGibbonPerformance[str_detect(CrestedMultiGibbonPerformance,'performance_tables_multi')]

CrestedMultiGibbonPerformanceDirs <- unique(dirname(CrestedMultiGibbonPerformanceSub))

# CrestedMulti gibbons 
PerformanceOutputCrestedMultiCombined <- data.frame()

for(a in 1:length(CrestedMultiGibbonPerformanceDirs)){
  
  PerformanceOutputCrestedMulti <- gibbonNetR::get_best_performance(performancetables.dir=CrestedMultiGibbonPerformanceDirs[a],
                                                                 class='CrestedGibbons',
                                                                 model.type = "multi", Thresh.val = 0)
  
  AUCDF <- as.data.frame(PerformanceOutputCrestedMulti$best_auc)
  AUCDF$Category <- 'Best AUC'
  
  F1DF <- as.data.frame(PerformanceOutputCrestedMulti$best_f1)
  F1DF$Category <- 'Best F1'
  
  TempRow <- rbind.data.frame(AUCDF,F1DF)
  PerformanceOutputCrestedMultiCombined <- rbind.data.frame(PerformanceOutputCrestedMultiCombined,TempRow)
}

head(PerformanceOutputCrestedMultiCombined)

PerformanceOutputCrestedMultiCombined_AUC <- 
  subset(PerformanceOutputCrestedMultiCombined,Category=='Best AUC')

ggboxplot(data=PerformanceOutputCrestedMultiCombined_AUC,
          color='CNN Architecture', y='AUC',x = 'Frozen',
          facet.by ='Training Data' )

PerformanceOutputCrestedMultiCombined_F1 <- 
  subset(PerformanceOutputCrestedMultiCombined,Category=='Best F1')

ggscatter(data=PerformanceOutputCrestedMultiCombined_F1,
          color='N epochs',
          shape='CNN Architecture', y='F1',x = 'Frozen',
          facet.by ='Training Data' )+ggtitle("Crested Gibbon")


# Now combine to check results -------------------------------------------------------------------------
library(tidyr)
GreyGibbonPerformanceList <- lapply(GreyGibbonPerformanceDirs,  list.files, full.names = TRUE)
GreyGibbonPerformanceListCSV <-lapply(unlist(GreyGibbonPerformanceList), read.csv)
GreyGibbonPerformanceCombined <-  bind_rows(GreyGibbonPerformanceListCSV)

CrestedGibbonPerformanceList <- lapply(CrestedGibbonPerformanceDirs,  list.files, full.names = TRUE)
CrestedGibbonPerformanceListCSV <-lapply(unlist(CrestedGibbonPerformanceList), read.csv)
CrestedGibbonPerformanceCombined <-  bind_rows(CrestedGibbonPerformanceListCSV)

CrestedGibbonMultiPerformanceList <- lapply(CrestedMultiGibbonPerformanceDirs,  list.files, full.names = TRUE)
CrestedGibbonMultiPerformanceListCSV <-lapply(unlist(CrestedGibbonMultiPerformanceList), read.csv)
CrestedGibbonMultiPerformanceCombined <-  bind_rows(CrestedGibbonMultiPerformanceListCSV)

GreyGibbonMultiPerformanceList <- lapply(GreyMultiGibbonPerformanceDirs,  list.files, full.names = TRUE)
GreyGibbonMultiPerformanceListCSV <-lapply(unlist(GreyGibbonMultiPerformanceList), read.csv)
GreyGibbonMultiPerformanceCombined <-  bind_rows(GreyGibbonMultiPerformanceListCSV)

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

# CombinedF1all is your data frame
best_f1_per_class <- CombinedF1all %>%
  group_by(Class,TrainingDataType,CNN.Architecture,Frozen,N.epochs) %>%  # Group by Class
  filter(F1 == max(F1, na.rm = TRUE),
         AUC == max(AUC, na.rm = TRUE)) %>%  # Select row(s) with max F1
  ungroup()

tail(as.data.frame(best_f1_per_class))

# Reshape your dataframe into long format
long_data <- best_f1_per_class %>%
  pivot_longer(cols = c(AUC, F1), 
               names_to = "Metric", 
               values_to = "Value")

colnames(long_data)

# Now visualize using ggplot
ggplot(long_data, aes(x = Frozen, y = Value, color = Metric)) +
  geom_boxplot() +
  #facet_grid(Class ~ CNN.Architecture,scales = 'free') +
  theme_minimal() +
  labs(title = "Comparison of Model Metrics",
       y = "Score", x = "Class") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

head(long_data)

# Create the boxplot comparing F1 and AUC across architectures, with Frozen as a factor
ggplot(long_data, aes(x = Frozen, y = Value, fill = Metric)) +
  geom_boxplot() +
  facet_grid(Class ~ CNN.Architecture) +
  theme_minimal() +
  labs(title = "Performance of CNN Architectures with and without Frozen Weights", 
       y = "Score (F1/AUC)", x = "CNN Architecture") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

PerformanceOutputGreyBinaryCombined_F1$Class <- 'Grey \n (binary)'
PerformanceOutputCrestedBinaryCombined_F1$Class <- 'Crested \n (binary)'
PerformanceOutputGreyMultiCombined_F1$Class <- 'Grey \n (multi)'
PerformanceOutputCrestedMultiCombined_F1$Class <- 'Crested \n (multi)'

CombinedF1all <- rbind.data.frame(PerformanceOutputGreyBinaryCombined_F1,
                                  PerformanceOutputCrestedBinaryCombined_F1,
                                  PerformanceOutputGreyMultiCombined_F1,
                                  PerformanceOutputCrestedMultiCombined_F1)



CombinedF1all <- CombinedF1all %>%
  mutate(TrainingDataType = case_when(
    grepl("Noise", `Training Data`) ~ "Noise",
    grepl("Cropping", `Training Data`) ~ "Crop",
    grepl("copy", `Training Data`) ~ "Duplicated",
    TRUE ~ "Original"
  ))


ggscatter(data=CombinedF1all,
            color='CNN Architecture', y='F1',x = 'TrainingDataType',
            shape='Frozen',
            facet.by ='Class' )+ggtitle("Best F1")

ggboxplot(data=CombinedF1all,
          color='CNN Architecture', y='F1', x='Frozen',
          facet.by=c('Class', 'TrainingDataType')) + 
  ggtitle("Best F1")

# Now combine to check results


PerformanceOutputGreyBinaryCombined_AUC$Class <- 'Grey \n (binary)'
PerformanceOutputCrestedBinaryCombined_AUC$Class <- 'Crested \n (binary)'
PerformanceOutputGreyMultiCombined_AUC$Class <- 'Grey \n (multi)'
PerformanceOutputCrestedMultiCombined_AUC$Class <- 'Crested \n (multi)'

CombinedAUCall <- rbind.data.frame(PerformanceOutputGreyBinaryCombined_AUC,
                                   PerformanceOutputCrestedBinaryCombined_AUC,
                                   PerformanceOutputGreyMultiCombined_AUC,
                                   PerformanceOutputCrestedMultiCombined_AUC)



CombinedAUCall <- CombinedAUCall %>%
  mutate(TrainingDataType = case_when(
    grepl("Noise", `Training Data`) ~ "Noise",
    grepl("Cropping", `Training Data`) ~ "Crop",
    grepl("copy", `Training Data`) ~ "Duplicate",
    TRUE ~ "Original"
  ))


ggscatter(data=CombinedAUCall,
          color='CNN Architecture', y='AUC',x = 'TrainingDataType',
          shape='Frozen',
          facet.by ='Class' )+ggtitle("Best AUC")

ggscatter(data=CombinedF1all,scales='free',
          x='CNN Architecture', y='F1', color='Frozen',
          facet.by=c('Class','TrainingDataType')) + 
  ggtitle("")+scale_color_manual(values = c('yellow','purple'))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab('Max F1')

ggscatter(data=CombinedAUCall,#scales='free',
          x='CNN Architecture', y='AUC', color='Frozen',
          facet.by=c('Class','TrainingDataType')) + 
  ggtitle("")+scale_color_manual(values = c('yellow','purple'))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab('AUC')




# CombinedF1all is your data frame
best_f1_per_class <- CombinedF1all %>%
  group_by(Class) %>%  # Group by Class
  filter(F1 == max(F1, na.rm = TRUE)) %>%  # Select row(s) with max F1
  ungroup()

# View the result
best_F1 <- as.data.frame(best_f1_per_class[,c("TrainingDataType","N epochs", "CNN Architecture", "Threshold", 
                                              "F1", "Frozen", "Class")])

# CombinedF1all is your data frame
best_AUC_per_class <- CombinedAUCall %>%
  group_by(Class) %>%  # Group by Class
  filter(AUC == max(AUC, na.rm = TRUE)) %>%  # Select row(s) with max AUC
  ungroup()

# View the result
best_AUC <- as.data.frame(best_AUC_per_class[,c("TrainingDataType","N epochs", "CNN Architecture", #"Threshold", 
                                                "AUC", "Frozen", "Class")])

best_F1
best_AUC


# What are we trying to show? ---------------------------------------------

# DO we see 
