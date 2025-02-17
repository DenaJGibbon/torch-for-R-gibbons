library(stringr)
library(ggpubr)
library(dplyr)

# Question: Which data augmentation + fine-tuning approach works best?
# Best performance using AUC- Grey binary ---------------------------------

GreyGibbonPerformance <- list.files('/Volumes/DJC Files/MultiSpeciesTransferLearning_R1/Benchmarking_random_variation/modelruns_repeatsubset',
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

CrestedGibbonPerformance <- list.files('/Volumes/DJC Files/MultiSpeciesTransferLearning_R1/Benchmarking_random_variation/modelruns_repeatsubset_Jahoo',
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
GreyMultiGibbonPerformance <- list.files('/Volumes/DJC Files/MultiSpeciesTransferLearning_R1/Benchmarking_random_variation/modelruns_repeatsubset_multi',
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
CrestedMultiGibbonPerformance <- list.files('/Volumes/DJC Files/MultiSpeciesTransferLearning_R1/Benchmarking_random_variation/modelruns_repeatsubset_multi',
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

ggboxplot(data=PerformanceOutputCrestedMultiCombined_F1,
          color='CNN Architecture', y='F1',x = 'Frozen',
          facet.by ='Training Data' )+ggtitle("Crested Gibbon")

