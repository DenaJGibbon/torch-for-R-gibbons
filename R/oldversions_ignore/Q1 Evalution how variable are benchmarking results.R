library(stringr)
library(ggpubr)
library(dplyr)
library(tidyr)

# Question: Which data augmentation + fine-tuning approach works best?
# Best performance using AUC- Grey binary ---------------------------------

GreyGibbonPerformance <- list.files('/Volumes/DJC Files/MultiSpeciesTransferLearning_R1/Benchmarking_random_variation_combined/modelruns_repeatsubset',
                                    full.names = T,recursive = T)

GreyGibbonPerformanceSub <- GreyGibbonPerformance[str_detect(GreyGibbonPerformance,'Danum')]

GreyGibbonPerformanceSub <- GreyGibbonPerformanceSub[str_detect(GreyGibbonPerformanceSub,'performance_tables')]

GreyGibbonPerformanceDirs <- unique(dirname(GreyGibbonPerformanceSub))

# Best performance using AUC- Crested binary ---------------------------------

CrestedGibbonPerformance <- list.files('/Volumes/DJC Files/MultiSpeciesTransferLearning_R1/Benchmarking_random_variation_combined/modelruns_repeatsubset_Jahoo',
                                    full.names = T,recursive = T)

CrestedGibbonPerformanceSub <- CrestedGibbonPerformance[str_detect(CrestedGibbonPerformance,'Jahoo')]

CrestedGibbonPerformanceSub <- CrestedGibbonPerformanceSub[str_detect(CrestedGibbonPerformanceSub,'performance_tables')]

CrestedGibbonPerformanceDirs <- unique(dirname(CrestedGibbonPerformanceSub))

# Best performance using AUC - Grey Multi ----------------------------------------------
GreyMultiGibbonPerformance <- list.files('/Volumes/DJC Files/MultiSpeciesTransferLearning_R1/Benchmarking_random_variation_bigmodels/modelruns_repeatsubset_multi_updateAUC_1epoch',
                                         full.names = T,recursive = T)

GreyMultiGibbonPerformanceSub <- GreyMultiGibbonPerformance[str_detect(GreyMultiGibbonPerformance,'performance_tables_multi')]

GreyMultiGibbonPerformanceDirs <- unique(dirname(GreyMultiGibbonPerformanceSub))


# Best performance using AUC - Multi Crested ----------------------------------------------
CrestedMultiGibbonPerformance <- list.files('/Volumes/DJC Files/MultiSpeciesTransferLearning_R1/Benchmarking_random_variation_bigmodels/modelruns_repeatsubset_multi_updateAUC_1epoch',
                                         full.names = T,recursive = T)

CrestedMultiGibbonPerformanceSub <- CrestedMultiGibbonPerformance[str_detect(CrestedMultiGibbonPerformance,'performance_tables_multi')]

CrestedMultiGibbonPerformanceDirs <- unique(dirname(CrestedMultiGibbonPerformanceSub))

# Now combine to check results -------------------------------------------------------------------------

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


Randomization <- str_split_fixed(CombinedF1all$Folder,pattern = '/', n=10)[,7]

CombinedF1all$Randomization <-str_split_fixed(Randomization,pattern = '_', n=3)[,2]


# CombinedF1all is your data frame
best_f1_per_class <- CombinedF1all %>%
  group_by(Class, Randomization, CNN.Architecture, Frozen) %>%  
  summarise(
    F1 = max(F1, na.rm = TRUE),
    AUC = max(AUC, na.rm = TRUE),
    .groups = "drop"  # Drop grouping after summarization
  )

best_f1_per_class <- as.data.frame(best_f1_per_class)

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
  facet_wrap(~ Class,scales = 'free') +  # Facet by Class
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
  facet_wrap(~ Class,scales = 'free') +  # Facet by Class
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
