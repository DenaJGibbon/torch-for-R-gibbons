# Load required packages ---------------------------------------------------
library(gibbonNetR)
library(flextable)
# Optional: Load local dev version
# devtools::load_all("/Users/denaclink/Desktop/RStudioProjects/gibbonNetR")

# -------------------- Evaluate Binary Model Performance -------------------

# Crested Gibbon (Binary Model)
performancetables.dir.crested <- '/Volumes/DJC Files/MultiSpeciesTransferLearning_R1/InitialModelEvaluation/model_output_1/_imagescambodia_binary_unfrozen_TRUE_/performance_tables/'
PerformanceOutputCrestedBinary <- gibbonNetR::get_best_performance(
  performancetables.dir = performancetables.dir.crested,
  class = 'Gibbons',
  model.type = "binary", Thresh.val = 0)

# Grey Gibbon (Binary Model)
performancetables.dir.grey <- '/Volumes/DJC Files/MultiSpeciesTransferLearning_R1/InitialModelEvaluation/model_output_1/_imagesmalaysia_binary_unfrozen_TRUE_/performance_tables/'
PerformanceOutputGreyBinary <- gibbonNetR::get_best_performance(
  performancetables.dir = performancetables.dir.grey,
  class = 'Gibbons',
  model.type = "binary", Thresh.val = 0)

# -------------------- Evaluate Multi-Class Model Performance ----------------

performancetables.dir.multi <- '/Volumes/DJC Files/MultiSpeciesTransferLearning_R1/InitialModelEvaluation/model_output_1/_imagesmulti_multi_unfrozen_TRUE_/performance_tables_multi/'

# Crested Gibbon (Multi-Class Model)
PerformanceOutputMultiCrested <- gibbonNetR::get_best_performance(
  performancetables.dir = performancetables.dir.multi,
  class = 'CrestedGibbons',
  model.type = "multi", Thresh.val = 0)

# Grey Gibbon (Multi-Class Model)
PerformanceOutputMultiGrey <- gibbonNetR::get_best_performance(
  performancetables.dir = performancetables.dir.multi,
  class = 'GreyGibbons',
  model.type = "multi", Thresh.val = 0)

# -------------------- Format and Combine F1 Scores -------------------------

# Add species label for clarity
PerformanceOutputCrestedBinary$best_f1$Species <- 'Crested Gibbon'
PerformanceOutputGreyBinary$best_f1$Species <- 'Grey Gibbon'
PerformanceOutputMultiCrested$best_f1$Species <- 'Crested Gibbon'
PerformanceOutputMultiGrey$best_f1$Species <- 'Grey Gibbon'

# Combine all F1 results into one dataframe
CombinedDF <- as.data.frame(rbind.data.frame(
  PerformanceOutputCrestedBinary$best_f1[,c("Species", "Training Data","N epochs","CNN Architecture","Threshold","Precision","Recall","F1","AUC")],
  PerformanceOutputGreyBinary$best_f1[,c("Species", "Training Data","N epochs","CNN Architecture","Threshold","Precision","Recall","F1","AUC")],
  PerformanceOutputMultiCrested$best_f1[,c("Species", "Training Data","N epochs","CNN Architecture","Threshold","Precision","Recall","F1","AUC")],
  PerformanceOutputMultiGrey$best_f1[,c("Species", "Training Data","N epochs","CNN Architecture","Threshold","Precision","Recall","F1","AUC")]
))

# Round metrics and rename training types
CombinedDFSubset <- CombinedDF
CombinedDFSubset$Precision <- round(CombinedDFSubset$Precision, 2)
CombinedDFSubset$Recall <- round(CombinedDFSubset$Recall, 2)
CombinedDFSubset$F1 <- round(CombinedDFSubset$F1, 2)
CombinedDFSubset$AUC <- round(CombinedDFSubset$AUC, 2)
CombinedDFSubset$`Training Data` <- ifelse(CombinedDFSubset$`Training Data` %in% c('imagescambodia', 'imagesmalaysia'), 'Binary', 'MultiClass')
CombinedDFSubset$`Training Data` <- paste(CombinedDFSubset$Species, CombinedDFSubset$`Training Data`)
CombinedDFSubset <- CombinedDFSubset[,-1]  # drop species column (already in Training Data)

# Create unique ID to collapse identical results across epochs/thresholds
CombinedDFSubset$`N epochs` <- as.factor(CombinedDFSubset$`N epochs`)
CombinedDFSubset$Threshold <- as.factor(CombinedDFSubset$Threshold)
CombinedDFSubset$UniqueID <- as.factor(paste(CombinedDFSubset$`Training Data`,
                                             CombinedDFSubset$`CNN Architecture`,
                                             CombinedDFSubset$Precision,
                                             CombinedDFSubset$Recall,
                                             CombinedDFSubset$F1,
                                             CombinedDFSubset$AUC, sep='_'))

# Collapse identical rows differing only in epoch/threshold
UniqueID_single <- unique(CombinedDFSubset$UniqueID)
ForFlextableCollapsed <- data.frame()

for (a in 1:length(UniqueID_single)) {
  TempSubset <- subset(CombinedDFSubset, UniqueID == UniqueID_single[a])
  UniqueEpoch <- droplevels(as.factor(dput(unique(TempSubset$`N epochs`))))
  UniqueThreshold <- droplevels(as.factor(dput(unique(TempSubset$Threshold))))
  TempRow <- TempSubset[1,]
  TempRow$`N epochs` <- paste(levels(UniqueEpoch), collapse = ", ")
  TempRow$Threshold <- paste(levels(UniqueThreshold), collapse = ", ")
  print(TempRow)
  ForFlextableCollapsed <- rbind.data.frame(ForFlextableCollapsed, TempRow)
}

# Drop UniqueID column before displaying
ForFlextableCollapsed <- ForFlextableCollapsed[,-which(names(ForFlextableCollapsed) == "UniqueID")]

# Create and format flextable
CombinedDFSubsetFlextable <- flextable(ForFlextableCollapsed)
CombinedDFSubsetFlextable <- merge_v(CombinedDFSubsetFlextable, j = c("Training Data"))
CombinedDFSubsetFlextable <- valign(CombinedDFSubsetFlextable, valign = "top")
CombinedDFSubsetFlextable

# Save F1 results
flextable::save_as_docx(CombinedDFSubsetFlextable,
                        path = 'Online Supporting Material Table 1. Best F1 Performance on training split.docx')

# -------------------- Format and Combine AUC Scores ------------------------

# Add species labels
PerformanceOutputCrestedBinary$best_auc$Species <- 'Crested Gibbon'
PerformanceOutputGreyBinary$best_auc$Species <- 'Grey Gibbon'
PerformanceOutputMultiCrested$best_auc$Species <- 'Crested Gibbon'
PerformanceOutputMultiGrey$best_auc$Species <- 'Grey Gibbon'

# Combine AUC results
CombinedDFAUC <- as.data.frame(rbind.data.frame(
  PerformanceOutputCrestedBinary$best_auc[,c("Species", "Training Data","N epochs","CNN Architecture","Precision","Recall","F1","AUC")],
  PerformanceOutputGreyBinary$best_auc[,c("Species", "Training Data","N epochs","CNN Architecture","Precision","Recall","F1","AUC")],
  PerformanceOutputMultiCrested$best_auc[,c("Species", "Training Data","N epochs","CNN Architecture","Precision","Recall","F1","AUC")],
  PerformanceOutputMultiGrey$best_auc[,c("Species", "Training Data","N epochs","CNN Architecture","Precision","Recall","F1","AUC")]
))

# Clean formatting
CombinedDFAUCSubset <- CombinedDFAUC
CombinedDFAUCSubset$Precision <- round(CombinedDFAUCSubset$Precision, 2)
CombinedDFAUCSubset$Recall <- round(CombinedDFAUCSubset$Recall, 2)
CombinedDFAUCSubset$F1 <- round(CombinedDFAUCSubset$F1, 2)
CombinedDFAUCSubset$AUC <- round(CombinedDFAUCSubset$AUC, 2)
CombinedDFAUCSubset$`Training Data` <- ifelse(CombinedDFAUCSubset$`Training Data` %in% c('imagescambodia', 'imagesmalaysia'), 'Binary', 'MultiClass')
CombinedDFAUCSubset$`Training Data` <- paste(CombinedDFAUCSubset$Species, CombinedDFAUCSubset$`Training Data`)
CombinedDFAUCSubset <- CombinedDFAUCSubset[,-1]

# Unique collapse
CombinedDFAUCSubset$`N epochs` <- as.factor(CombinedDFAUCSubset$`N epochs`)
CombinedDFAUCSubset$UniqueID <- as.factor(paste(CombinedDFAUCSubset$`Training Data`,
                                                CombinedDFAUCSubset$`CNN Architecture`,
                                                CombinedDFAUCSubset$Precision,
                                                CombinedDFAUCSubset$Recall,
                                                CombinedDFAUCSubset$F1,
                                                CombinedDFAUCSubset$AUC, sep='_'))

UniqueID_single <- unique(CombinedDFAUCSubset$UniqueID)
ForFlextableCollapsed <- data.frame()

for (a in 1:length(UniqueID_single)) {
  TempSubset <- subset(CombinedDFAUCSubset, UniqueID == UniqueID_single[a])
  UniqueEpoch <- droplevels(as.factor(dput(unique(TempSubset$`N epochs`))))
  UniqueThreshold <- droplevels(as.factor(dput(unique(TempSubset$Threshold))))
  TempRow <- TempSubset[1,]
  TempRow$`N epochs` <- paste(levels(UniqueEpoch), collapse = ", ")
  TempRow$Threshold <- paste(levels(UniqueThreshold), collapse = ", ")
  print(TempRow)
  ForFlextableCollapsed <- rbind.data.frame(ForFlextableCollapsed, TempRow)
}

# Keep only final AUC table columns
ForFlextableCollapsed <- ForFlextableCollapsed[,c("Training Data", "N epochs", "CNN Architecture", "AUC")]
ForFlextableCollapsed <- ForFlextableCollapsed[-which(duplicated(ForFlextableCollapsed)),]

# Create and save AUC flextable
CombinedDFAUCSubsetFlextableAUC <- flextable(ForFlextableCollapsed)
CombinedDFAUCSubsetFlextableAUC

flextable::save_as_docx(CombinedDFAUCSubsetFlextableAUC,
                        path = 'Online Supporting Material Table 2. Best AUC Performance on training split.docx')


# Does larger test dataset change AUC? -----------------------------------
performancetables.dir.multi <- '/Volumes/DJC Files/MultiSpeciesTransferLearning_R1/InitialModelEvaluation/model_output_1/evaluation_WA/performance_tables_multi_trained/'

# Crested Gibbon (Multi-Class Model)
PerformanceOutputMultiCrested <- gibbonNetR::get_best_performance(
  performancetables.dir = performancetables.dir.multi,
  class = 'CrestedGibbons',
  model.type = "multi", Thresh.val = 0)

PerformanceOutputMultiCrested$best_auc$AUC
PerformanceOutputMultiCrested$best_f1$F1
PerformanceOutputMultiCrested$pr_plot

# Grey Gibbon (Multi-Class Model)
PerformanceOutputMultiGrey <- gibbonNetR::get_best_performance(
  performancetables.dir = performancetables.dir.multi,
  class = 'GreyGibbons',
  model.type = "multi", Thresh.val = 0)

PerformanceOutputMultiGrey$best_auc$AUC
PerformanceOutputMultiGrey$best_f1$F1
as.data.frame(PerformanceOutputMultiGrey$best_auc)
as.data.frame(PerformanceOutputMultiGrey$best_f1)
PerformanceOutputMultiGrey$pr_plot

