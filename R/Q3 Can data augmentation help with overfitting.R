# Load libraries -------------------------------------------------------------------------
library(dplyr)
library(flextable)
library(dplyr)
#library(gibbonNetR)
devtools::load_all("/Users/denaclink/Desktop/RStudioProjects/gibbonNetR")

# Goal is to find the best performing model combination with data augmentation
# Will use the small test set, and use the large one for final reporting
# We focus on the smaller test set for computational efficiency

# Grey Gibbon Binary Model Training ---------------------------------------

TrainingFolders <- list.files('/Volumes/DJC Files/MultiSpeciesTransferLearning/DataAugmentation/images/Danum Images/',
                              full.name=T)

# Location of spectrogram images for testing
test.data.path <-"/Users/denaclink/Desktop/RStudioProjects/Gibbon-transfer-learning-multispecies/data/training_images_sorted/Danum/test/"

output.dir <-'/Volumes/DJC Files/MultiSpeciesTransferLearning_R1/DataAugmentation_V4/modelruns_repeatsubset/'

# Number of epochs to include
epoch.iterations <- c(1,5)

# Train the models specifying different architectures
architectures <-  c('alexnet', 'resnet50')

freeze.param <- c(TRUE)

for(d in 1:1){
  for (a in 1:length(architectures)) {
    for (b in 1:length(freeze.param)) {
      for(c in 1:2){
        
        input.data.path <-  TrainingFolders[c]
        
        trainingfolder.short <- paste( basename(TrainingFolders[c]),'_jitter')
        
        gibbonNetR::train_CNN_binary(
          input.data.path = input.data.path,
          noise.weight = 0.25,
          architecture = architectures[a],
          brightness = 1,
          contrast = 1 ,
          saturation = 1,
          save.model = TRUE,
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

for(d in 1:1){
  for (a in 1:length(architectures)) {
    for (b in 1:length(freeze.param)) {
      for(c in 2:length(TrainingFolders)){

        input.data.path <-  TrainingFolders[c]

        trainingfolder.short <- paste( basename(TrainingFolders[c]),'_nojitter')

        gibbonNetR::train_CNN_binary(
          input.data.path = input.data.path,
          noise.weight = 0.25,
          architecture = architectures[a],
          brightness = 0,
          contrast = 0 ,
          saturation = 0,
          save.model = TRUE,
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

# Crested Gibbon Binary Model Training ---------------------------------------

# TrainingFolders <- list.files('/Volumes/DJC Files/MultiSpeciesTransferLearning/DataAugmentation/images/Jahoo Images/',
#                               full.name=T)

TrainingFolders <- list.files('/Volumes/DJC Files/MultiSpeciesTransferLearning/DataAugmentation/images/Jahoo Images/',
                              full.name=T)

# Location of spectrogram images for testing
test.data.path <-'/Users/denaclink/Desktop/RStudioProjects/Gibbon-transfer-learning-multispecies/data/training_images_sorted/Jahoo/test/'

output.dir <-'/Volumes/DJC Files/MultiSpeciesTransferLearning_R1/DataAugmentation_V4/modelruns_repeatsubset_Jahoo/'

for(d in 1:1){
  for (a in 1:length(architectures)) {
    for (b in 1:length(freeze.param)) {
      for(c in 1:2){
        
        input.data.path <-  TrainingFolders[c]
        
        trainingfolder.short <-  paste( basename(TrainingFolders[c]),'_jitter')
        
        gibbonNetR::train_CNN_binary(
          input.data.path = input.data.path,
          noise.weight = 0.25,
          architecture = architectures[a],
          save.model = TRUE,
          learning_rate = 0.001,
          test.data = test.data.path,
          brightness = 1,
          contrast = 1 ,
          saturation = 1,
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

for(d in 1:1){
  for (a in 1:length(architectures)) {
    for (b in 1:length(freeze.param)) {
      for(c in 1:length(TrainingFolders)){

        input.data.path <-  TrainingFolders[c]

        trainingfolder.short <-  paste( basename(TrainingFolders[c]),'_nojitter')

        gibbonNetR::train_CNN_binary(
          input.data.path = input.data.path,
          noise.weight = 0.25,
          architecture = architectures[a],
          save.model = TRUE,
          learning_rate = 0.001,
          test.data = test.data.path,
          brightness = 0,
          contrast = 0 ,
          saturation = 0,
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

# Multi-class Model Training ---------------------------------------
library(dplyr)
library(flextable)
library(dplyr)
#library(gibbonNetR)
devtools::load_all("/Users/denaclink/Desktop/RStudioProjects/gibbonNetR")

# Number of epochs to include
epoch.iterations <- c(1,5)

# Train the models specifying different architectures
architectures <-  c('alexnet', 'resnet50')

freeze.param <- c(TRUE)


TrainingFolders <- list.files('/Volumes/DJC Files/MultiSpeciesTransferLearning/DataAugmentation/images/Combined/',
                              full.name=T)

TrainingFolders <- TrainingFolders[1:4]

# Location of spectrogram images for testing
test.data.path <- '/Users/denaclink/Desktop/RStudioProjects/Gibbon-transfer-learning-multispecies/data/training_images_sorted/Combined/test/'
#test.data.path <-'/Volumes/DJC Files/MultiSpeciesTransferLearning/DataAugmentation/images/Combined//CombinedTest/test'

output.dir <-'/Volumes/DJC Files/MultiSpeciesTransferLearning_R1/DataAugmentation_V4/modelruns_repeatsubset_multi/'

for(d in 1:1){
  for (a in 1:length(architectures)) {
    for (b in 1:length(freeze.param)) {
      for(c in 1:2){
        
        
        input.data.path <-  TrainingFolders[c]
        
        trainingfolder.short <-  paste( basename(TrainingFolders[c]),'_jitter')
        
        gibbonNetR::train_CNN_multi(input.data.path=input.data.path,
                                    architecture =architectures[a],
                                    learning_rate = 0.001,
                                    test.data=test.data.path,
                                    unfreeze.param = freeze.param[b],
                                    epoch.iterations=epoch.iterations,
                                    brightness = 1,
                                    contrast = 1 ,
                                    saturation = 1,
                                    #list.thresholds = seq(0, 1, .1),
                                    save.model = TRUE,
                                    early.stop = "yes",
                                    output.base.path = paste(output.dir,trainingfolder.short,'_',d,sep=''),
                                    trainingfolder=trainingfolder.short,
                                    noise.category = "Noise")
        
      }
    }
  }
}

for(d in 1:1){
  for (a in 1:length(architectures)) {
    for (b in 1:length(freeze.param)) {
      for(c in 1:length(TrainingFolders)){
        
        
        input.data.path <-  TrainingFolders[c]
        
        trainingfolder.short <-  paste( basename(TrainingFolders[c]),'_nojitter')
        
        gibbonNetR::train_CNN_multi(input.data.path=input.data.path,
                                    architecture =architectures[a],
                                    learning_rate = 0.001,
                                    test.data=test.data.path,
                                    unfreeze.param = freeze.param[b],
                                    epoch.iterations=epoch.iterations,
                                    brightness = 0,
                                    contrast = 0 ,
                                    saturation = 0,
                                    #list.thresholds = seq(0, 1, .1),
                                    save.model = TRUE,
                                    early.stop = "yes",
                                    output.base.path = paste(output.dir,trainingfolder.short,'_',d,sep=''),
                                    trainingfolder=trainingfolder.short,
                                    noise.category = "Noise")
        
      }
    }
  }
}



