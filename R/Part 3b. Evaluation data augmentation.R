library(stringr)
library(ggpubr)
library(dplyr)
library(tidyr)
#library(gibbonNetR)

# Best performance using AUC- Grey binary ---------------------------------

GreyGibbonPerformance <- list.files('results/part3/DataAugmentation_V4/modelruns_repeatsubset',
                                    full.names = T,recursive = T)

GreyGibbonPerformanceSub <- GreyGibbonPerformance[str_detect(GreyGibbonPerformance,'Danum')]

GreyGibbonPerformanceSub <- GreyGibbonPerformanceSub[str_detect(GreyGibbonPerformanceSub,'performance_tables')]

GreyGibbonPerformanceDirs <- unique(dirname(GreyGibbonPerformanceSub))

# Best performance using AUC- Crested binary ---------------------------------

CrestedGibbonPerformance <- list.files('results/part3/DataAugmentation_V4/modelruns_repeatsubset_Jahoo',
                                    full.names = T,recursive = T)

CrestedGibbonPerformanceSub <- CrestedGibbonPerformance[str_detect(CrestedGibbonPerformance,'Jahoo')]

CrestedGibbonPerformanceSub <- CrestedGibbonPerformanceSub[str_detect(CrestedGibbonPerformanceSub,'performance_tables')]

CrestedGibbonPerformanceDirs <- unique(dirname(CrestedGibbonPerformanceSub))

# Best performance using AUC -Multi ----------------------------------------------
GreyMultiGibbonPerformance <- list.files('results/part3/DataAugmentation_V4//modelruns_repeatsubset_multi',
                                         full.names = T,recursive = T)

GreyMultiGibbonPerformanceSub <- GreyMultiGibbonPerformance[str_detect(GreyMultiGibbonPerformance,'performance_tables_multi')]

GreyMultiGibbonPerformanceDirs <- unique(dirname(GreyMultiGibbonPerformanceSub))

# Best performance using AUC -Multi ----------------------------------------------
CrestedMultiGibbonPerformance <- list.files('results/part3/DataAugmentation_V4//modelruns_repeatsubset_multi',
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

GreyGibbonPerformanceCombined$Class <- 'Grey  \n (binary)'
CrestedGibbonPerformanceCombined$Class <- 'Crested  \n (binary)'
CrestedGibbonMultiPerformanceCombined$Class <- 'Crested  \n (multi)'
GreyGibbonMultiPerformanceCombined$Class <- 'Grey  \n (multi)'


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
  slice_max(Threshold, n = 1) %>%  # or slice_max if you prefer highest threshold
  ungroup()


# Create pdf to save for online supporting material -----------------------
pdf(file = 'results/tablesandfigures/Online Supporting Material Figure 1.pdf', width = 10, height = 10)

# Create the plot
p <- ggscatter(data = best_auc_per_training_data,
               x = 'TrainingDataType', 
               y = 'AUC',
               facet.by = c('Class', 'CNN.Architecture'),
               color = 'N.epochs',
               scales = 'free') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab('') +
  scale_color_manual(values = c('yellow', 'purple'))

# Print the plot
print(p)

# Add figure caption
grid::grid.text("OSM Figure 1. AUC-ROC scores across training data augmentation types and architectures. \n AUC-ROC was calculated for the original test data split.",
                x = 0.5, y = 0.02, gp = grid::gpar(fontsize = 12))

dev.off()
