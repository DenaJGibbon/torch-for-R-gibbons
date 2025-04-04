# Load libraries -------------------------------------------------------------------------
library(dplyr)
library(flextable)
library(dplyr)
library(gibbonNetR)

# Optional setwd
setwd("/Users/denaclink/Desktop/RStudioProjects/Gibbon-transfer-learning-multispecies")

# Crested gibbon data preparation ----------

# Check for data leakage among training, valid and test datasets
# Function to extract the relevant identifier from the filename
extract_file_identifier <- function(filename) {
  components <- str_split_fixed(filename, "_", n = 6)
  identifier <- paste(components[, 3], components[, 4],components[, 5],sep = "_")
  return(identifier)
}

# Function to check for data leakage
check_data_leakage <- function(rootDir) {
  trainingDir <- file.path(rootDir, 'train')
  validationDir <- file.path(rootDir, 'valid')
  testDir <- file.path(rootDir, 'test')
  
  trainFiles <- list.files(trainingDir, pattern = "\\.jpg$", full.names = FALSE, recursive = TRUE)
  validationFiles <- list.files(validationDir, pattern = "\\.jpg$", full.names = FALSE, recursive = TRUE)
  testFiles <- list.files(testDir, pattern = "\\.jpg$", full.names = FALSE, recursive = TRUE)
  
  trainIds <- sapply(trainFiles, extract_file_identifier)
  validationIds <- sapply(validationFiles, extract_file_identifier)
  testIds <- sapply(testFiles, extract_file_identifier)
  
  trainValidationOverlap <- trainIds[which(trainIds %in% validationIds)]
  trainTestOverlap <- trainIds[which(trainIds %in% testIds)]
  validationTestOverlap <- testIds[which(testIds %in% validationIds)]
  
  if (length(trainValidationOverlap) == 0 & length(trainTestOverlap) == 0 & length(validationTestOverlap) == 0) {
    cat("No data leakage detected among the datasets.\n")
  } else {
    cat("Data leakage detected!\n")
    if (length(trainValidationOverlap) > 0) {
      cat("Overlap between training and validation datasets:\n", trainValidationOverlap, "\n")
    }
    if (length(trainTestOverlap) > 0) {
      cat("Overlap between training and test datasets:\n", trainTestOverlap, "\n")
    }
    if (length(validationTestOverlap) > 0) {
      cat("Overlap between validation and test datasets:\n", validationTestOverlap, "\n")
    }
  }
}

# Check for leakage in different datasets
check_data_leakage('data/training_images_sorted/Jahoo')

# Cambodia Binary Model Training ---------------------------------------------------------

# Location of spectrogram images for training
input.data.path <-  'data/training_images_sorted/Jahoo'

# Location of spectrogram images for testing
test.data.path <- 'data/training_images_sorted/Jahoo/test/'

# Training data folder short
trainingfolder.short <- 'imagescambodia'

# Whether to unfreeze.param the layers
unfreeze.param.param <- TRUE # FALSE means the features are frozen; TRUE unfrozen

# Number of epochs to include
epoch.iterations <- c(1,2,3,4,5,20)

# Train the models specifying different architectures
gibbonNetR::train_CNN_binary(input.data.path=input.data.path,
                             architecture ='alexnet',
                             save.model= TRUE,
                             learning_rate = 0.001,
                             test.data=test.data.path,
                             unfreeze.param = TRUE,
                             epoch.iterations=epoch.iterations,
                             early.stop = "yes",
                             output.base.path = "/Volumes/DJC Files/MultiSpeciesTransferLearning_R1/InitialModelEvaluation/model_output_1/",
                             trainingfolder=trainingfolder.short,
                             positive.class="Gibbons",
                             negative.class="Noise")


gibbonNetR::train_CNN_binary(input.data.path=input.data.path,
                             architecture ='vgg16',
                             save.model= TRUE,
                             learning_rate = 0.001,
                             test.data=test.data.path,
                             unfreeze.param = TRUE,
                             epoch.iterations=epoch.iterations,
                             early.stop = "yes",
                             output.base.path = "/Volumes/DJC Files/MultiSpeciesTransferLearning_R1/InitialModelEvaluation/model_output_1/",
                             trainingfolder=trainingfolder.short,
                             positive.class="Gibbons",
                             negative.class="Noise")

gibbonNetR::train_CNN_binary(input.data.path=input.data.path,
                             architecture ='vgg19',
                             save.model= TRUE,
                             learning_rate = 0.001,
                             test.data=test.data.path,
                             unfreeze.param = TRUE,
                             epoch.iterations=epoch.iterations,
                             early.stop = "yes",
                            output.base.path = "/Volumes/DJC Files/MultiSpeciesTransferLearning_R1/InitialModelEvaluation/model_output_1/",
                             trainingfolder=trainingfolder.short,
                             positive.class="Gibbons",
                             negative.class="Noise")

gibbonNetR::train_CNN_binary(input.data.path=input.data.path,
                             architecture ='resnet18',
                             save.model= TRUE,
                             learning_rate = 0.001,
                             test.data=test.data.path,
                             unfreeze.param = TRUE,
                             epoch.iterations=epoch.iterations,
                             early.stop = "yes",
                            output.base.path = "/Volumes/DJC Files/MultiSpeciesTransferLearning_R1/InitialModelEvaluation/model_output_1/",
                             trainingfolder=trainingfolder.short,
                             positive.class="Gibbons",
                             negative.class="Noise")

gibbonNetR::train_CNN_binary(input.data.path=input.data.path,
                             architecture ='resnet50',
                             save.model= TRUE,
                             learning_rate = 0.001,
                             test.data=test.data.path,
                             unfreeze.param = TRUE,
                             epoch.iterations=epoch.iterations,
                             early.stop = "yes",
                             output.base.path = "/Volumes/DJC Files/MultiSpeciesTransferLearning_R1/InitialModelEvaluation/model_output_1/",
                             trainingfolder=trainingfolder.short,
                             positive.class="Gibbons",
                             negative.class="Noise")

gibbonNetR::train_CNN_binary(input.data.path=input.data.path,
                             architecture ='resnet152',
                             save.model= TRUE,
                             learning_rate = 0.001,
                             test.data=test.data.path,
                             unfreeze.param = TRUE,
                             epoch.iterations=epoch.iterations,
                             early.stop = "yes",
                            output.base.path = "/Volumes/DJC Files/MultiSpeciesTransferLearning_R1/InitialModelEvaluation/model_output_1/",
                             trainingfolder=trainingfolder.short,
                             positive.class="Gibbons",
                             negative.class="Noise")


# Cambodia Binary Model Evaluation ----------------------------------------

performancetables.dir <- '/Users/denaclink/Desktop/RStudioProjects//Volumes/DJC Files/MultiSpeciesTransferLearning_R1/InitialModelEvaluation/model_output_1/_imagescambodia_binary_unfrozen_TRUE_/performance_tables/'

PerformanceOutput <- gibbonNetR::get_best_performance(performancetables.dir=performancetables.dir,
                                                      class='Gibbons',
                                                      model.type = "binary",
                                                      Thresh.val = 0)


PerformanceOutput$f1_plot


# Grey Gibbon Data Prep ---------------------------------------------------------
# Function to extract the relevant identifier from the filename
extract_file_identifier <- function(filename) {
  components <- str_split_fixed(filename, "_", n = 6)
  identifier <- paste(components[, 2], components[, 3],components[, 4],sep = "_")
  return(identifier)
}

# Function to check for data leakage
check_data_leakage <- function(rootDir) {
  trainingDir <- file.path(rootDir, 'train')
  validationDir <- file.path(rootDir, 'valid')
  testDir <- file.path(rootDir, 'test')
  
  
  trainFiles <- list.files(trainingDir, pattern = "\\.jpg$", full.names = FALSE, recursive = TRUE)
  validationFiles <- list.files(validationDir, pattern = "\\.jpg$", full.names = FALSE, recursive = TRUE)
  testFiles <- list.files(testDir, pattern = "\\.jpg$", full.names = FALSE, recursive = TRUE)
  
  trainIds <- sapply(trainFiles, extract_file_identifier)
  validationIds <- sapply(validationFiles, extract_file_identifier)
  testIds <- sapply(testFiles, extract_file_identifier)
  
  trainValidationOverlap <- trainIds[which(trainIds %in% validationIds)]
  trainTestOverlap <- trainIds[which(trainIds %in% testIds)]
  validationTestOverlap <- testIds[which(testIds %in% validationIds)]
  
  if (length(trainValidationOverlap) == 0 & length(trainTestOverlap) == 0 & length(validationTestOverlap) == 0) {
    cat("No data leakage detected among the datasets.\n")
  } else {
    cat("Data leakage detected!\n")
    if (length(trainValidationOverlap) > 0) {
      cat("Overlap between training and validation datasets:\n", trainValidationOverlap, "\n")
    }
    if (length(trainTestOverlap) > 0) {
      cat("Overlap between training and test datasets:\n", trainTestOverlap, "\n")
    }
    if (length(validationTestOverlap) > 0) {
      cat("Overlap between validation and test datasets:\n", validationTestOverlap, "\n")
    }
  }
}

# Check for leakage in different datasets
check_data_leakage('data/training_images_sorted/Danum/')


# Grey Gibbon Binary Model Training ---------------------------------------

# Location of spectrogram images for training
input.data.path <-  'data/training_images_sorted/Danum/'

# Location of spectrogram images for testing
test.data.path <- "data/training_images_sorted/Danum/test/"

# Training data folder short
trainingfolder.short <- 'imagesmalaysia'

# Whether to unfreeze.param the layers
unfreeze.param.param <- TRUE # FALSE means the features are frozen; TRUE unfrozen

# Number of epochs to include
epoch.iterations <- c(1,2,3,4,5,20)

# Train the models specifying different architectures
gibbonNetR::train_CNN_binary(input.data.path=input.data.path,
                             architecture ='alexnet',
                             save.model= TRUE,
                             learning_rate = 0.001,
                             test.data=test.data.path,
                             unfreeze.param = TRUE,
                             epoch.iterations=epoch.iterations,
                             early.stop = "yes",
                            output.base.path = "/Volumes/DJC Files/MultiSpeciesTransferLearning_R1/InitialModelEvaluation/model_output_1/",
                             trainingfolder=trainingfolder.short,
                             positive.class="Gibbons",
                             negative.class="Noise")


gibbonNetR::train_CNN_binary(input.data.path=input.data.path,
                             architecture ='vgg16',
                             save.model= TRUE,
                             learning_rate = 0.001,
                             test.data=test.data.path,
                             unfreeze.param = TRUE,
                             epoch.iterations=epoch.iterations,
                             early.stop = "yes",
                            output.base.path = "/Volumes/DJC Files/MultiSpeciesTransferLearning_R1/InitialModelEvaluation/model_output_1/",
                             trainingfolder=trainingfolder.short,
                             positive.class="Gibbons",
                             negative.class="Noise")

gibbonNetR::train_CNN_binary(input.data.path=input.data.path,
                             architecture ='vgg19',
                             save.model= TRUE,
                             learning_rate = 0.001,
                             test.data=test.data.path,
                             unfreeze.param = TRUE,
                             epoch.iterations=epoch.iterations,
                             early.stop = "yes",
                            output.base.path = "/Volumes/DJC Files/MultiSpeciesTransferLearning_R1/InitialModelEvaluation/model_output_1/",
                             trainingfolder=trainingfolder.short,
                             positive.class="Gibbons",
                             negative.class="Noise")

gibbonNetR::train_CNN_binary(input.data.path=input.data.path,
                             architecture ='resnet18',
                             save.model= TRUE,
                             learning_rate = 0.001,
                             test.data=test.data.path,
                             unfreeze.param = TRUE,
                             epoch.iterations=epoch.iterations,
                             early.stop = "yes",
                            output.base.path = "/Volumes/DJC Files/MultiSpeciesTransferLearning_R1/InitialModelEvaluation/model_output_1/",
                             trainingfolder=trainingfolder.short,
                             positive.class="Gibbons",
                             negative.class="Noise")

gibbonNetR::train_CNN_binary(input.data.path=input.data.path,
                             architecture ='resnet50',
                             save.model= TRUE,
                             learning_rate = 0.001,
                             test.data=test.data.path,
                             unfreeze.param = TRUE,
                             epoch.iterations=epoch.iterations,
                             early.stop = "yes",
                            output.base.path = "/Volumes/DJC Files/MultiSpeciesTransferLearning_R1/InitialModelEvaluation/model_output_1/",
                             trainingfolder=trainingfolder.short,
                             positive.class="Gibbons",
                             negative.class="Noise")

gibbonNetR::train_CNN_binary(input.data.path=input.data.path,
                             architecture ='resnet152',
                             save.model= TRUE,
                             learning_rate = 0.001,
                             test.data=test.data.path,
                             unfreeze.param = TRUE,
                             epoch.iterations=epoch.iterations,
                             early.stop = "yes",
                            output.base.path = "/Volumes/DJC Files/MultiSpeciesTransferLearning_R1/InitialModelEvaluation/model_output_1/",
                             trainingfolder=trainingfolder.short,
                             positive.class="Gibbons",
                             negative.class="Noise")

# Evaluate model performance
performancetables.dir <- "/Users/denaclink/Desktop/RStudioProjects/Gibbon-transfer-learning-multispecies//Volumes/DJC Files/MultiSpeciesTransferLearning_R1/InitialModelEvaluation/model_output_1/_imagesmalaysia_binary_unfrozen_TRUE_/performance_tables/"

PerformanceOutput <- gibbonNetR::get_best_performance(performancetables.dir=performancetables.dir,
                                                      class='Gibbons',
                                                      model.type = "binary")

PerformanceOutput$f1_plot
PerformanceOutput$best_f1$F1
PerformanceOutput$best_auc$AUC


# Multi Species Model Training --------------------------------------------
setwd("/Users/denaclink/Desktop/RStudioProjects/Gibbon-transfer-learning-multispecies")

# Location of spectrogram images for training
input.data.path <- '/Users/denaclink/Desktop/RStudioProjects/Gibbon-transfer-learning-multispecies/data/training_images_sorted/Combined/'

# Location of spectrogram images for testing
test.data.path <- '/Users/denaclink/Desktop/RStudioProjects/Gibbon-transfer-learning-multispecies/data/training_images_sorted/Combined/test/'

# Training data folder short
trainingfolder.short <- 'imagesmulti'

# Whether to unfreeze.param the layers
unfreeze.param.param <- TRUE # FALSE means the features are frozen; TRUE unfrozen

# Number of epochs to include
epoch.iterations <- c(1,2,3,4,5,20)

# Allow early stopping?
early.stop <- 'yes'

#source("/Users/denaclink/Desktop/RStudioProjects/gibbonNetR/R/train_CNN_multi.R")
# Train models using different architectures
gibbonNetR::train_CNN_multi(input.data.path=input.data.path,
                            architecture ='alexnet',
                            learning_rate = 0.001,
                            test.data=test.data.path,
                            unfreeze.param = TRUE,
                            epoch.iterations=epoch.iterations,
                            save.model= TRUE,
                            early.stop = "yes",
                            output.base.path = "/Volumes/DJC Files/MultiSpeciesTransferLearning_R1/InitialModelEvaluation/model_output_1/",
                            trainingfolder=trainingfolder.short,
                            noise.category = "Noise")


gibbonNetR::train_CNN_multi(input.data.path=input.data.path,
                            architecture ='vgg16',
                            learning_rate = 0.001,
                            test.data=test.data.path,
                            unfreeze.param = TRUE,
                            epoch.iterations=epoch.iterations,
                            save.model= TRUE,
                            early.stop = "yes",
                           output.base.path = "/Volumes/DJC Files/MultiSpeciesTransferLearning_R1/InitialModelEvaluation/model_output_1/",
                            trainingfolder=trainingfolder.short,
                            noise.category = "Noise")

gibbonNetR::train_CNN_multi(input.data.path=input.data.path,
                            architecture ='vgg19',
                            learning_rate = 0.001,
                            test.data=test.data.path,
                            unfreeze.param = TRUE,
                            epoch.iterations=epoch.iterations,
                            save.model= TRUE,
                            early.stop = "yes",
                           output.base.path = "/Volumes/DJC Files/MultiSpeciesTransferLearning_R1/InitialModelEvaluation/model_output_1/",
                            trainingfolder=trainingfolder.short,
                            noise.category = "Noise")

gibbonNetR::train_CNN_multi(input.data.path=input.data.path,
                            architecture ='resnet18',
                            learning_rate = 0.001,
                            test.data=test.data.path,
                            unfreeze.param = TRUE,
                            epoch.iterations=epoch.iterations,
                            save.model= TRUE,
                            early.stop = "yes",
                           output.base.path = "/Volumes/DJC Files/MultiSpeciesTransferLearning_R1/InitialModelEvaluation/model_output_1/",
                            trainingfolder=trainingfolder.short,
                            noise.category = "Noise")

gibbonNetR::train_CNN_multi(input.data.path=input.data.path,
                            architecture ='resnet50',
                            learning_rate = 0.001,
                            test.data=test.data.path,
                            unfreeze.param = TRUE,
                            epoch.iterations=epoch.iterations,
                            save.model= TRUE,
                            early.stop = "yes",
                           output.base.path = "/Volumes/DJC Files/MultiSpeciesTransferLearning_R1/InitialModelEvaluation/model_output_1/",
                            trainingfolder=trainingfolder.short,
                            noise.category = "Noise")

gibbonNetR::train_CNN_multi(input.data.path=input.data.path,
                            architecture ='resnet152',
                            learning_rate = 0.001,
                            test.data=test.data.path,
                            unfreeze.param = TRUE,
                            epoch.iterations=epoch.iterations,
                            save.model= TRUE,
                            early.stop = "yes",
                           output.base.path = "/Volumes/DJC Files/MultiSpeciesTransferLearning_R1/InitialModelEvaluation/model_output_1/",
                            trainingfolder=trainingfolder.short,
                            noise.category = "Noise")



# Evaluate performance of all models ----------------------------------

# Crested binary
performancetables.dir.crested <- '/Volumes/DJC Files/MultiSpeciesTransferLearning_R1/InitialModelEvaluation/model_output_1/_imagescambodia_binary_unfrozen_TRUE_/performance_tables/'
PerformanceOutputCrestedBinary <- gibbonNetR::get_best_performance(performancetables.dir=performancetables.dir.crested,
                                                      class='Gibbons',
                                                      model.type = "binary", Thresh.val = 0)
as.data.frame(PerformanceOutputCrestedBinary$best_f1)
as.data.frame(PerformanceOutputCrestedBinary$best_auc)

# Grey gibbons binary
performancetables.dir.grey <- '/Volumes/DJC Files/MultiSpeciesTransferLearning_R1/InitialModelEvaluation/model_output_1/_imagesmalaysia_binary_unfrozen_TRUE_/performance_tables/'
PerformanceOutputGreyBinary <- gibbonNetR::get_best_performance(performancetables.dir=performancetables.dir.grey,
                                                                   class='Gibbons',
                                                                   model.type = "binary", Thresh.val = 0)

as.data.frame(PerformanceOutputGreyBinary$best_f1)
as.data.frame(PerformanceOutputGreyBinary$best_auc)


performancetables.dir.multi <- '/Volumes/DJC Files/MultiSpeciesTransferLearning_R1/InitialModelEvaluation/model_output_1/_imagesmulti_multi_unfrozen_TRUE_/performance_tables_multi/'

PerformanceOutputMultiCrested <- gibbonNetR::get_best_performance(performancetables.dir=performancetables.dir.multi,
                                                           class='CrestedGibbons',
                                                           model.type = "multi",  Thresh.val = 0)

PerformanceOutputMultiCrested$f1_plot
PerformanceOutputMultiCrested$pr_plot
PerformanceOutputMultiCrested$best_f1$F1
PerformanceOutputMultiCrested$best_auc$AUC

PerformanceOutputMultiGrey <- gibbonNetR::get_best_performance(performancetables.dir=performancetables.dir.multi,
                                                           class='GreyGibbons',
                                                           model.type = "multi", Thresh.val = 0)

PerformanceOutputMultiGrey$f1_plot
PerformanceOutputMultiGrey$pr_plot
PerformanceOutputMultiGrey$best_f1$F1
PerformanceOutputMultiGrey$best_auc$AUC

# Create table to report model performance --------------------------------

# F1 ----------------------------------------------------------------------
PerformanceOutputCrestedBinary$best_f1$Species <- 'Crested Gibbon'
PerformanceOutputGreyBinary$best_f1$Species <- 'Grey Gibbon'
PerformanceOutputMultiCrested$best_f1$Species <- 'Crested Gibbon'
PerformanceOutputMultiGrey$best_f1$Species  <- 'Grey Gibbon'

CombinedDF <- as.data.frame(rbind.data.frame(PerformanceOutputCrestedBinary$best_f1[,c("Species", "Training Data","N epochs","CNN Architecture","Threshold","Precision","Recall","F1","AUC")], PerformanceOutputGreyBinary$best_f1[,c("Species", "Training Data","N epochs","CNN Architecture","Threshold","Precision","Recall","F1","AUC")],
                 PerformanceOutputMultiCrested$best_f1[,c("Species", "Training Data","N epochs","CNN Architecture","Threshold","Precision","Recall","F1","AUC")], PerformanceOutputMultiGrey$best_f1[,c("Species", "Training Data","N epochs","CNN Architecture","Threshold","Precision","Recall","F1","AUC")]))


CombinedDF <- CombinedDF[,c("Species", "Training Data","N epochs","CNN Architecture","Threshold","Precision","Recall","F1","AUC")]
CombinedDFSubset <-CombinedDF #subset(CombinedDF,Threshold >= 0.5)
CombinedDFSubset$Precision <- round(CombinedDFSubset$Precision,2)
CombinedDFSubset$Recall <- round(CombinedDFSubset$Recall,2)
CombinedDFSubset$F1 <- round(CombinedDFSubset$F1,2)
CombinedDFSubset$AUC <- round(CombinedDFSubset$AUC,2)

CombinedDFSubset$`Training Data` <- 
  ifelse(CombinedDFSubset$`Training Data` == 'imagescambodia' | CombinedDFSubset$`Training Data` == 'imagesmalaysia', 'Binary', 'MultiClass')

CombinedDFSubset$`Training Data` <- paste(CombinedDFSubset$Species, CombinedDFSubset$`Training Data`)
CombinedDFSubset <-CombinedDFSubset[,-c(1)]

head(CombinedDFSubset)
nrow(CombinedDFSubset)

CombinedDFSubset$`N epochs` <- as.factor(CombinedDFSubset$`N epochs`)
CombinedDFSubset$Threshold <- as.factor(CombinedDFSubset$Threshold)

CombinedDFSubset$UniqueID <- as.factor(paste(CombinedDFSubset$`Training Data`,
                                   CombinedDFSubset$`CNN Architecture`,
                                   CombinedDFSubset$Precision,
                                   CombinedDFSubset$Recall,
                                   CombinedDFSubset$F1,
                                   CombinedDFSubset$AUC,
                                   sep='_'))

UniqueID_single <- unique(CombinedDFSubset$UniqueID)

ForFlextableCollapsed <- data.frame()

for(a in 1:length(UniqueID_single)){
  TempSubset <-  subset(CombinedDFSubset,UniqueID==UniqueID_single[a])
  UniqueEpoch <- droplevels(as.factor(dput(unique(TempSubset$`N epochs`))))
  UniqueThreshold <- droplevels(as.factor(dput(unique(TempSubset$Threshold))))
  TempRow <- TempSubset[1,]
  TempRow$`N epochs` <- paste(levels(UniqueEpoch),collapse = ", ")
  TempRow$Threshold <- paste(levels(UniqueThreshold),collapse = ", ")
  print(TempRow)
  ForFlextableCollapsed <- rbind.data.frame(ForFlextableCollapsed,
                                            TempRow)
}

table(ForFlextableCollapsed$`CNN Architecture`,
      ForFlextableCollapsed$`Training Data`)

ForFlextableCollapsed <- as.data.frame(ForFlextableCollapsed)
ForFlextableCollapsed <- ForFlextableCollapsed[,-c(9)]

CombinedDFSubsetFlextable <- flextable(ForFlextableCollapsed)
CombinedDFSubsetFlextable

CombinedDFSubsetFlextable <- merge_v(CombinedDFSubsetFlextable, j = c("Training Data"))
CombinedDFSubsetFlextable <- valign(CombinedDFSubsetFlextable,valign = "top")                        
CombinedDFSubsetFlextable

unique(ForFlextableCollapsed$`CNN Architecture`)

# AUC ---------------------------------------------------------------------
PerformanceOutputCrestedBinary$best_auc$Species <- 'Crested Gibbon'
PerformanceOutputGreyBinary$best_auc$Species <- 'Grey Gibbon'
PerformanceOutputMultiCrested$best_auc$Species <- 'Crested Gibbon'
PerformanceOutputMultiGrey$best_auc$Species  <- 'Grey Gibbon'

CombinedDFAUC <- as.data.frame(rbind.data.frame(PerformanceOutputCrestedBinary$best_auc[,c("Species", "Training Data","N epochs","CNN Architecture","Precision","Recall","F1","AUC")], PerformanceOutputGreyBinary$best_auc[,c("Species", "Training Data","N epochs","CNN Architecture","Precision","Recall","F1","AUC")],
                                             PerformanceOutputMultiCrested$best_auc[,c("Species", "Training Data","N epochs","CNN Architecture","Precision","Recall","F1","AUC")], PerformanceOutputMultiGrey$best_auc[,c("Species", "Training Data","N epochs","CNN Architecture","Precision","Recall","F1","AUC")]))

CombinedDFAUC <- CombinedDFAUC[,c("Species", "Training Data","N epochs","CNN Architecture","Precision","Recall","F1","AUC")]
CombinedDFAUCSubset <-CombinedDFAUC #subset(CombinedDFAUC,Threshold >= 0.5)
CombinedDFAUCSubset$Precision <- round(CombinedDFAUCSubset$Precision,2)
CombinedDFAUCSubset$Recall <- round(CombinedDFAUCSubset$Recall,2)
CombinedDFAUCSubset$F1 <- round(CombinedDFAUCSubset$F1,2)
CombinedDFAUCSubset$AUC <- round(CombinedDFAUCSubset$AUC,2)

CombinedDFAUCSubset$`Training Data` <- 
  ifelse(CombinedDFAUCSubset$`Training Data` == 'imagescambodia' | CombinedDFAUCSubset$`Training Data` == 'imagesmalaysia', 'Binary', 'MultiClass')

CombinedDFAUCSubset$`Training Data` <- paste(CombinedDFAUCSubset$Species, CombinedDFAUCSubset$`Training Data`)

table(CombinedDFAUCSubset$`CNN Architecture`,
      CombinedDFAUCSubset$`N epochs`,
      CombinedDFAUCSubset$`Training Data`)

CombinedDFAUCSubset <- CombinedDFAUCSubset[,c("Species", "Training Data", "N epochs", "CNN Architecture", 
                        "AUC")]


CombinedDFAUCSubsetFlextableAUC <- flextable(CombinedDFAUCSubset)
CombinedDFAUCSubsetFlextableAUC

flextable::save_as_docx(CombinedDFAUCSubsetFlextable,
                         path='Online Supporting Material Table 1. Performance on training split.docx')



# Evaluate performance over separate test set -----------------------------
  library(gibbonNetR)
  trained_models_dir <- '/Volumes/DJC Files/MultiSpeciesTransferLearning_R1/InitialModelEvaluation/model_output_1/_imagesmulti_multi_unfrozen_TRUE_'
  
  image_data_dir <- '/Volumes/DJC Files/MultiSpeciesTransferLearning/DataAugmentation/images/Combined//CombinedTest/test'

  class_names <-  dput(list.files(image_data_dir))
  
  output_dir <- '/Volumes/DJC Files/MultiSpeciesTransferLearning_R1/InitialModelEvaluation/model_output_1/evaluation_WA/'
  
  # Evaluate the performance of the trained models using the test images
  evaluate_trainedmodel_performance_multi(trained_models_dir=trained_models_dir,
                                          class_names=class_names,
                                          image_data_dir=image_data_dir,
                                          output_dir= output_dir,
                                          noise.category = "Noise")
