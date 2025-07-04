# Load required packages ---------------------------------------------------
library(gibbonNetR)
library(flextable)
library(gibbonNetR)
# -------------------- Evaluate Binary Model Performance -------------------

# Crested Gibbon (Binary Model)
performancetables.dir.crested <- 'results/part2/InitialModelEvaluation/model_output_1/_imagescambodia_binary_unfrozen_TRUE_/performance_tables/'
PerformanceOutputCrestedBinary <- gibbonNetR::get_best_performance(
  performancetables.dir = performancetables.dir.crested,
  class = 'Gibbons',
  model.type = "binary", Thresh.val = 0)

# Grey Gibbon (Binary Model)
performancetables.dir.grey <- 'results/part2/InitialModelEvaluation/model_output_1/_imagesmalaysia_binary_unfrozen_TRUE_/performance_tables/'
PerformanceOutputGreyBinary <- gibbonNetR::get_best_performance(
  performancetables.dir = performancetables.dir.grey,
  class = 'Gibbons',
  model.type = "binary", Thresh.val = 0)

# -------------------- Evaluate Multi-Class Model Performance ----------------

performancetables.dir.multi <- 'results/part2/InitialModelEvaluation/model_output_1/rerun_updated_AUC/_imagesmulti_multi_unfrozen_TRUE_/performance_tables_multi_trained/'

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
PerformanceOutputCrestedBinary$best_f1$Species <- 'Crested Gibbon \n binary'
PerformanceOutputGreyBinary$best_f1$Species <- 'Grey Gibbon \n binary'
PerformanceOutputMultiCrested$best_f1$Species <- 'Crested Gibbon \n multi'
PerformanceOutputMultiGrey$best_f1$Species <- 'Grey Gibbon \n multi'

# Combine all F1 results into one dataframe
CombinedDF <- as.data.frame(rbind.data.frame(
  PerformanceOutputCrestedBinary$best_f1[,c("Species","N epochs","CNN Architecture","Threshold","Precision","Recall","F1","AUC")],
  PerformanceOutputGreyBinary$best_f1[,c("Species", "N epochs","CNN Architecture","Threshold","Precision","Recall","F1","AUC")],
  PerformanceOutputMultiCrested$best_f1[,c("Species", "N epochs","CNN Architecture","Threshold","Precision","Recall","F1","AUC")],
  PerformanceOutputMultiGrey$best_f1[,c("Species", "N epochs","CNN Architecture","Threshold","Precision","Recall","F1","AUC")]
))

CombinedDF$`CNN Architecture` <- 
  str_split_fixed(CombinedDF$`CNN Architecture`,
                pattern = '_', n=2)[,1]

# Round metrics and rename training types
CombinedDFSubset <- CombinedDF
CombinedDFSubset$Precision <- round(CombinedDFSubset$Precision, 2)
CombinedDFSubset$Recall <- round(CombinedDFSubset$Recall, 2)
CombinedDFSubset$F1 <- round(CombinedDFSubset$F1, 2)
CombinedDFSubset$AUC <- round(CombinedDFSubset$AUC, 2)

# Create unique ID to collapse identical results across epochs/thresholds
CombinedDFSubset$`N epochs` <- as.factor(CombinedDFSubset$`N epochs`)
CombinedDFSubset$Threshold <- as.factor(CombinedDFSubset$Threshold)
CombinedDFSubset$UniqueID <- as.factor(paste(CombinedDFSubset$Species,
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
CombinedDFSubsetFlextable <- merge_v(CombinedDFSubsetFlextable, j = c("Species"))
CombinedDFSubsetFlextable <- valign(CombinedDFSubsetFlextable, valign = "top")
CombinedDFSubsetFlextable

# Save F1 results
flextable::save_as_docx(CombinedDFSubsetFlextable,
                        path = 'results/tablesandfigures/Online Supporting Material Table 1. Best F1 Performance on training split.docx')

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
ForFlextableCollapsed <- ForFlextableCollapsed[,c("Training Data",  "CNN Architecture", "AUC")]
ForFlextableCollapsed <- ForFlextableCollapsed[-which(duplicated(ForFlextableCollapsed)),]

# Create and save AUC flextable
CombinedDFAUCSubsetFlextableAUC <- flextable(ForFlextableCollapsed)
CombinedDFAUCSubsetFlextableAUC

flextable::save_as_docx(CombinedDFAUCSubsetFlextableAUC,
                        path = 'results/tablesandfigures/Online Supporting Material Table 2. Best AUC Performance on training split.docx')
