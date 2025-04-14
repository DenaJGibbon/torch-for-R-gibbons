# Load libraries -------------------------------------------------------------------------
library(dplyr)
library(flextable)

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
                             output.base.path = "results/part2/InitialModelEvaluation/model_output_1/",
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
                             output.base.path = "results/part2/InitialModelEvaluation/model_output_1/",
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
                            output.base.path = "results/part2/InitialModelEvaluation/model_output_1/",
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
                            output.base.path = "results/part2/InitialModelEvaluation/model_output_1/",
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
                             output.base.path = "results/part2/InitialModelEvaluation/model_output_1/",
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
                            output.base.path = "results/part2/InitialModelEvaluation/model_output_1/",
                             trainingfolder=trainingfolder.short,
                             positive.class="Gibbons",
                             negative.class="Noise")


# Cambodia Binary Model Evaluation ----------------------------------------

performancetables.dir <- 'results/part2/InitialModelEvaluation/model_output_1/_imagescambodia_binary_unfrozen_TRUE_/performance_tables/'

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
                            output.base.path = "results/part2/InitialModelEvaluation/model_output_1/",
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
                            output.base.path = "results/part2/InitialModelEvaluation/model_output_1/",
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
                            output.base.path = "results/part2/InitialModelEvaluation/model_output_1/",
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
                            output.base.path = "results/part2/InitialModelEvaluation/model_output_1/",
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
                            output.base.path = "results/part2/InitialModelEvaluation/model_output_1/",
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
                            output.base.path = "results/part2/InitialModelEvaluation/model_output_1/",
                             trainingfolder=trainingfolder.short,
                             positive.class="Gibbons",
                             negative.class="Noise")

# Evaluate model performance
performancetables.dir <- "results/part2/InitialModelEvaluation/model_output_1/_imagesmalaysia_binary_unfrozen_TRUE_/performance_tables/"

PerformanceOutput <- gibbonNetR::get_best_performance(performancetables.dir=performancetables.dir,
                                                      class='Gibbons',
                                                      model.type = "binary")

PerformanceOutput$f1_plot
PerformanceOutput$best_f1$F1
PerformanceOutput$best_auc$AUC


# Multi Species Model Training --------------------------------------------
# Location of spectrogram images for training
input.data.path <- 'data/training_images_sorted/Combined/'

# Location of spectrogram images for testing
test.data.path <- 'data/training_images_sorted/Combined/test/'

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
                            output.base.path = "results/part2/InitialModelEvaluation/model_output_1/",
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
                           output.base.path = "results/part2/InitialModelEvaluation/model_output_1/",
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
                           output.base.path = "results/part2/InitialModelEvaluation/model_output_1/",
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
                           output.base.path = "results/part2/InitialModelEvaluation/model_output_1/",
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
                           output.base.path = "results/part2/InitialModelEvaluation/model_output_1/",
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
                           output.base.path = "results/part2/InitialModelEvaluation/model_output_1/",
                            trainingfolder=trainingfolder.short,
                            noise.category = "Noise")
