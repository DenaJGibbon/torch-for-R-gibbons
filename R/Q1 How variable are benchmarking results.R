# Load libraries -------------------------------------------------------------------------
library(dplyr)
library(flextable)
library(dplyr)
#library(gibbonNetR)

# Goal is test how variable results are
# Will use the small test set, and use the large one for final reporting

# Set training parameters -------------------------------------------------
# Number of epochs to include
epoch.iterations <- c(1)

# Train the models specifying different architectures
architectures <-  c('alexnet', 'resnet18', 'resnet50', 'vgg16', 'vgg19', 'resnet152')

# Whether to fine-tune or use as feature extractor
freeze.param <- c(FALSE,TRUE)

# Grey Gibbon Binary Model Training ---------------------------------------

TrainingFolders <- list.files('/Volumes/DJC Files/MultiSpeciesTransferLearning/DataAugmentation/images/Danum Images/',
                              full.name=T)

# Location of spectrogram images for testing
test.data.path <-"/Users/denaclink/Desktop/RStudioProjects/Gibbon-transfer-learning-multispecies/data/training_images_sorted/Danum/test/"

output.dir <-'/Volumes/DJC Files/MultiSpeciesTransferLearning_R1/Benchmarking_random_variation_bigmodels/modelruns_repeatsubset/'

for(d in 1:3){
  for (a in 1:length(architectures)) {
    for (b in 1:length(freeze.param)) {
      for(c in 1:1){
        
        input.data.path <-  TrainingFolders[c]
        
        trainingfolder.short <-  basename(TrainingFolders[c])
        
        gibbonNetR::train_CNN_binary(
          input.data.path = input.data.path,
          noise.weight = 0.25,
          architecture = architectures[a],
          save.model = FALSE,
          learning_rate = 0.001,
          test.data = test.data.path,
          batch_size = 32,
          unfreeze.param = freeze.param[b],
          # FALSE means the features are frozen
          epoch.iterations = epoch.iterations,
          list.thresholds = seq(0, 1, .1),
          early.stop = "yes",
          output.base.path = paste(output.dir,trainingfolder.short,'_',d,sep=''),
          trainingfolder = trainingfolder.short,
          positive.class = "Gibbons",
          negative.class = "Noise"
        )
        
      }
    }
  }
}
gc()

# Crested Gibbon Binary Model Training ---------------------------------------

# TrainingFolders <- list.files('/Volumes/DJC Files/MultiSpeciesTransferLearning/DataAugmentation/images/Jahoo Images/',
#                               full.name=T)

TrainingFolders <- list.files('/Volumes/DJC Files/MultiSpeciesTransferLearning/DataAugmentation/images/Jahoo Images/',
                              full.name=T)

# Location of spectrogram images for testing
test.data.path <-'/Users/denaclink/Desktop/RStudioProjects/Gibbon-transfer-learning-multispecies/data/training_images_sorted/Jahoo/test/'

output.dir <-'/Volumes/DJC Files/MultiSpeciesTransferLearning_R1/Benchmarking_random_variation_bigmodels/modelruns_repeatsubset_Jahoo/'

for(d in 1:3){
  for (a in 1:length(architectures)) {
    for (b in 1:length(freeze.param)) {
      for(c in 1:1){
        
        input.data.path <-  TrainingFolders[c]
        
        trainingfolder.short <-  basename(TrainingFolders[c])
        
        gibbonNetR::train_CNN_binary(
          input.data.path = input.data.path,
          noise.weight = 0.25,
          architecture = architectures[a],
          save.model = FALSE,
          learning_rate = 0.001,
          test.data = test.data.path,
          batch_size = 32,
          unfreeze.param = freeze.param[b],
          # FALSE means the features are frozen
          epoch.iterations = epoch.iterations,
          list.thresholds = seq(0, 1, .1),
          early.stop = "yes",
          output.base.path = paste(output.dir,trainingfolder.short,'_',d,sep=''),
          trainingfolder = trainingfolder.short,
          positive.class = "Gibbons",
          negative.class = "Noise"
        )
        
        
        
        
      }
    }
  }
}
gc()

# Multi-class Model Training ---------------------------------------
TrainingFolders <- list.files('/Volumes/DJC Files/MultiSpeciesTransferLearning/DataAugmentation/images/Combined/',
                              full.name=T)

TrainingFolders <- TrainingFolders[1]

# Location of spectrogram images for testing
test.data.path <- '/Users/denaclink/Desktop/RStudioProjects/Gibbon-transfer-learning-multispecies/data/training_images_sorted/Combined/test/'
#test.data.path <-'/Volumes/DJC Files/MultiSpeciesTransferLearning/DataAugmentation/images/Combined//CombinedTest/test'

output.dir <-'/Volumes/DJC Files/MultiSpeciesTransferLearning_R1/Benchmarking_random_variation_bigmodels/modelruns_repeatsubset_multi_updateAUC_1epoch/'

for(d in 2:3){
  for (a in 1:length(architectures)) {
    for (b in 1:length(freeze.param)) {
      for(c in 1:length(TrainingFolders)){
        
        input.data.path <-  TrainingFolders[c]
        
        trainingfolder.short <-  basename(TrainingFolders[c])
        
        gibbonNetR::train_CNN_multi(input.data.path=input.data.path,
                                    architecture =architectures[a],
                                    learning_rate = 0.001,
                                    test.data=test.data.path,
                                    unfreeze.param = freeze.param[b],
                                    epoch.iterations=epoch.iterations,
                                    #list.thresholds = seq(0, 1, .1),
                                    save.model= FALSE,
                                    early.stop = "yes",
                                    output.base.path = paste(output.dir,trainingfolder.short,'_',d,sep=''),
                                    trainingfolder=trainingfolder.short,
                                    noise.category = "Noise")
        
      }
    }
  }
}
gc()

