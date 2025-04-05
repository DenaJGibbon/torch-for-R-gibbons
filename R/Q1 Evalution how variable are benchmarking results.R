library(stringr)
library(ggpubr)
library(dplyr)

# Question: Which data augmentation + fine-tuning approach works best?
# Best performance using AUC- Grey binary ---------------------------------

GreyGibbonPerformance <- list.files('/Volumes/DJC Files/MultiSpeciesTransferLearning_R1/Benchmarking_random_variation_combined/modelruns_repeatsubset',
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
  AUCDF$Folder <- GreyGibbonPerformanceDirs[a]
  
  F1DF <- as.data.frame(PerformanceOutputGreyBinary$best_f1)
  F1DF$Category <- 'Best F1'
  F1DF$Folder <- GreyGibbonPerformanceDirs[a]
  
  TempRow <- rbind.data.frame(AUCDF,F1DF)
  PerformanceOutputGreyBinaryCombined <- rbind.data.frame(PerformanceOutputGreyBinaryCombined,TempRow)
}

head(PerformanceOutputGreyBinaryCombined)
nrow(PerformanceOutputGreyBinaryCombined)

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


PerformanceOutputGreyBinaryCombined_F1[c('F1','CNN Architecture','Frozen')]
PerformanceOutputGreyBinaryCombined_AUC[c('AUC','CNN Architecture','Frozen')]

# Best performance using AUC- Crested binary ---------------------------------

CrestedGibbonPerformance <- list.files('/Volumes/DJC Files/MultiSpeciesTransferLearning_R1/Benchmarking_random_variation_combined/modelruns_repeatsubset_Jahoo',
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
  AUCDF$Folder <- CrestedGibbonPerformanceDirs[a]
  
  F1DF <- as.data.frame(PerformanceOutputCrestedBinary$best_f1)
  F1DF$Category <- 'Best F1'
  F1DF$Folder <- CrestedGibbonPerformanceDirs[a]
  
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

PerformanceOutputCrestedBinaryCombined_F1[c('F1','CNN Architecture','Frozen')]
PerformanceOutputCrestedBinaryCombined_AUC[c('AUC','CNN Architecture','Frozen')]

# Best performance using AUC - Grey Multi ----------------------------------------------
GreyMultiGibbonPerformance <- list.files('/Volumes/DJC Files/MultiSpeciesTransferLearning_R1/Benchmarking_random_variation_bigmodels/modelruns_repeatsubset_multi_updateAUC_1epoch/',
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
  AUCDF$Folder <- GreyMultiGibbonPerformanceDirs[a]
  
  F1DF <- as.data.frame(PerformanceOutputGreyMulti$best_f1)
  F1DF$Category <- 'Best F1'
  F1DF$Folder <- GreyMultiGibbonPerformanceDirs[a]
  
  
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

PerformanceOutputGreyMultiCombined_F1[c('F1','CNN Architecture','Frozen','Threshold')]
PerformanceOutputGreyMultiCombined_AUC[c('AUC','CNN Architecture','Frozen')]

# Best performance using AUC - Multi Crested ----------------------------------------------
CrestedMultiGibbonPerformance <- list.files('/Volumes/DJC Files/MultiSpeciesTransferLearning_R1/Benchmarking_random_variation_bigmodels/modelruns_repeatsubset_multi_updateAUC_1epoch/',
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
  AUCDF$Folder <- CrestedMultiGibbonPerformanceDirs[a]
  
  F1DF <- as.data.frame(PerformanceOutputCrestedMulti$best_f1)
  F1DF$Category <- 'Best F1'
  F1DF$Folder <- CrestedMultiGibbonPerformanceDirs[a]
  
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

PerformanceOutputCrestedMultiCombined_F1[c('F1','CNN Architecture','Frozen','Threshold')]
PerformanceOutputCrestedMultiCombined_AUC[c('AUC','CNN Architecture','Frozen')]

# Now combine to check results -------------------------------------------------------------------------
library(tidyr)
library(dplyr)
library(tidyr)
library(stringr)

# Function to read CSVs and include folder name
read_and_label <- function(file_path) {
  df <- read.csv(file_path)
  df$Folder <- dirname(file_path) # Extracts folder name
  return(df)
}

# Process Grey Gibbon Performance
GreyGibbonPerformanceList <- lapply(GreyGibbonPerformanceDirs, list.files, full.names = TRUE)
GreyGibbonPerformanceListCSV <- lapply(unlist(GreyGibbonPerformanceList), read_and_label)
GreyGibbonPerformanceCombined <- bind_rows(GreyGibbonPerformanceListCSV)  # Efficient rbind

# Process Crested Gibbon Performance
CrestedGibbonPerformanceList <- lapply(CrestedGibbonPerformanceDirs, list.files, full.names = TRUE)
CrestedGibbonPerformanceListCSV <- lapply(unlist(CrestedGibbonPerformanceList), read_and_label)
CrestedGibbonPerformanceCombined <- bind_rows(CrestedGibbonPerformanceListCSV)

# Process Crested Gibbon Multi Performance
CrestedGibbonMultiPerformanceList <- lapply(CrestedMultiGibbonPerformanceDirs, list.files, full.names = TRUE)
CrestedGibbonMultiPerformanceListCSV <- lapply(unlist(CrestedGibbonMultiPerformanceList), read_and_label)
CrestedGibbonMultiPerformanceCombined <- bind_rows(CrestedGibbonMultiPerformanceListCSV)
CrestedGibbonMultiPerformanceCombined <- subset(CrestedGibbonMultiPerformanceCombined,Class=='CrestedGibbons')

# Process Grey Gibbon Multi Performance
GreyGibbonMultiPerformanceList <- lapply(GreyMultiGibbonPerformanceDirs, list.files, full.names = TRUE)
GreyGibbonMultiPerformanceListCSV <- lapply(unlist(GreyGibbonMultiPerformanceList), read_and_label)
GreyGibbonMultiPerformanceCombined <- bind_rows(GreyGibbonMultiPerformanceListCSV)
GreyGibbonMultiPerformanceCombined <- subset(GreyGibbonMultiPerformanceCombined,Class=='GreyGibbons')

GreyGibbonPerformanceCombined$Class <- 'Grey Gibbons \n (binary)'
CrestedGibbonPerformanceCombined$Class <- 'Crested Gibbons \n (binary)'
CrestedGibbonMultiPerformanceCombined$Class <- 'Crested Gibbons \n (multi)'
GreyGibbonMultiPerformanceCombined$Class <- 'Grey Gibbons \n (multi)'

CombinedF1all <-
  rbind.data.frame(GreyGibbonPerformanceCombined,CrestedGibbonPerformanceCombined,
                   CrestedGibbonMultiPerformanceCombined,GreyGibbonMultiPerformanceCombined)

# 
# PerformanceOutputGreyBinaryCombined_F1$Class <- 'Grey Gibbons \n (binary)'
# PerformanceOutputCrestedBinaryCombined_F1$Class <- 'Crested Gibbons \n (binary)'
# PerformanceOutputCrestedMultiCombined_F1$Class <- 'Crested Gibbons \n (multi)'
# PerformanceOutputGreyMultiCombined_F1$Class <- 'Grey Gibbons \n (multi)'

# CombinedF1all <- 
#   rbind.data.frame(PerformanceOutputGreyBinaryCombined_F1,PerformanceOutputCrestedBinaryCombined_F1,
#                    PerformanceOutputCrestedMultiCombined_F1,PerformanceOutputGreyMultiCombined_F1)

Randomization <- str_split_fixed(CombinedF1all$Folder,pattern = '/', n=10)[,7]

CombinedF1all$Randomization <-str_split_fixed(Randomization,pattern = '_', n=3)[,2]

TempRandomMissing <- subset(CombinedF1all,Randomization == "")
TempRandom <- str_split_fixed(TempRandomMissing$Folder,pattern = '/', n=10)[,8]
TempRandomMissing$Randomization <- TempRandom

CombinedF1all <- subset(CombinedF1all,Randomization != "")
CombinedF1all <- rbind.data.frame(CombinedF1all,TempRandomMissing)

# CombinedF1all is your data frame
best_f1_per_class <- CombinedF1all %>%
  group_by(Class, Randomization, CNN.Architecture, Frozen) %>%  
  summarise(
    F1 = max(F1, na.rm = TRUE),
    AUC = max(AUC, na.rm = TRUE),
    .groups = "drop"  # Drop grouping after summarization
  )

best_f1_per_class <- as.data.frame(best_f1_per_class)

ggboxplot(data=best_f1_per_class,
            x='Frozen',
            y='F1',
          color='CNN.Architecture',
          facet.by ='Class',scales='free' )+ ylab('Max F1')

ggboxplot(data=best_f1_per_class,
            x='Frozen',
            y='AUC',
            color='CNN.Architecture',
            facet.by ='Class',scales='free'  )

ggplot(data = best_f1_per_class, aes(x = Frozen, y = F1, color = CNN.Architecture)) +
  geom_jitter(width = 0.1, height = 0) + 
  facet_wrap(~ Class, scales = "free") +
  theme_minimal() + 
  ylab('Max F1') + 
  labs(title = "Max F1 by Class and CNN Architecture")

ggplot(data = best_f1_per_class, aes(x = Frozen, y = AUC, color = CNN.Architecture)) +
  geom_jitter(width = 0.1, height = 0) + 
  facet_wrap(~ Class, scales = "free") +
  theme_minimal() + 
  ylab('Max AUC') + 
  labs(title = "Max AUC by Class and CNN Architecture")


# Compute the mean and standard error for F1 and AUC
maxSEF1AUC <- best_f1_per_class %>%
  group_by(Class, CNN.Architecture, Frozen) %>%  
  summarise(
    max_F1 = round(mean(F1, na.rm = TRUE), 2),
    se_F1 = round(sd(F1, na.rm = TRUE) / sqrt(n()), 2),  # Standard Error for F1 rounded to 2 decimals
    max_AUC = round(mean(AUC, na.rm = TRUE), 3),
    se_AUC = round(sd(AUC, na.rm = TRUE) / sqrt(n()), 3),  # Standard Error for AUC rounded to 2 decimals
    .groups = "drop"  # Drop grouping after summarization
  )

# Convert to data frame
maxSEF1AUC <- as.data.frame(maxSEF1AUC)

# View the table
print(maxSEF1AUC)

# Create a line plot to visualize trends
F1plot <- ggplot(maxSEF1AUC, aes(x = CNN.Architecture, y = max_F1, color = factor(Frozen), group = factor(Frozen))) +
  #geom_line(linewidth = 1) +  # Line plot for mean F1 across architectures
  geom_point(size = 1) +  # Add points to the lines
  geom_errorbar(aes(ymin = max_F1 - se_F1, ymax = max_F1 + se_F1), 
                width = 0.2) +  # Error bars
  facet_wrap(~ Class) +  # Facet by Class
  theme_minimal() +
  scale_color_manual(values = c("orange", "purple")) +  # Custom colors for 'Frozen' status
  labs(
    title = "",
    x = "CNN Architecture",
    y = "Max F1 Score",
    color = "Fine-tuning"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    legend.position = "top"  # Move legend to the top
  )

F1plot

AUCPlot <- ggplot(maxSEF1AUC, aes(x = CNN.Architecture, y = max_AUC, color = factor(Frozen), group = factor(Frozen))) +
  #geom_line(linewidth = 1) +  # Line plot for mean F1 across architectures
  geom_point(size = 1, width = 0.2, height = 0) +  # Add points 
  geom_errorbar(aes(ymin = max_AUC - se_AUC, ymax = max_AUC + se_AUC), 
                width = 0.2) +  # Error bars
  facet_wrap(~ Class, scales = 'free') +  # Facet by Class
  theme_minimal() +
  scale_color_manual(values = c("orange", "purple")) +  # Custom colors for 'Frozen' status
  labs(
    title = "",
    x = "CNN Architecture",
    y = "AUC",
    color = "Fine-tuning"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    legend.position = "top"  # Move legend to the top
  )+ylab('AUC-ROC')

AUCPlot

cowplot::plot_grid(F1plot,AUCPlot,
                   labels = c('A)','B)'),
                   label_x = 0.9)
