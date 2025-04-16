# Load libraries -------------------------------------------------------------------------
library(dplyr)
library(flextable)
library(dplyr)
library(gibbonNetR)

# Set training parameters -------------------------------------------------
# Number of epochs to include
epoch.iterations <- c(1)

# Train the models specifying different architectures
architectures <-  c('alexnet', 'resnet18', 'resnet50', 'vgg16', 'vgg19', 'resnet152')

# Whether to fine-tune or use as feature extractor
freeze.param <- c(FALSE,TRUE)

# Grey Gibbon Binary Model Training ---------------------------------------
TrainingFolders <- 'data/training_images_sorted/Danum'

# Location of spectrogram images for testing
test.data.path <-'data/training_images_sorted/Danum/test/'

# Location of output directory
output.dir <-'results/part1/modelruns_repeatsubset/'

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
          save.model= TRUE,
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

TrainingFolders <- 'data/training_images_sorted/Jahoo'

# Location of spectrogram images for testing
test.data.path <-'data/training_images_sorted/Jahoo/test/'

output.dir <-'results/part1/modelruns_repeatsubset_Jahoo/'

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
          save.model= TRUE,
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

TrainingFolders <- 'data/training_images_sorted/Combined'

# Location of spectrogram images for testing
test.data.path <- 'data/training_images_sorted/Combined/test/'

output.dir <-'results/part1/modelruns_repeatsubset_multi_updateAUC_1epoch/'

for(d in 1:3){
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
                                    save.model= TRUE,
                                    early.stop = "yes",
                                    output.base.path = paste(output.dir,trainingfolder.short,'_',d,sep=''),
                                    trainingfolder=trainingfolder.short,
                                    noise.category = "Noise")
        
      }
    }
  }
}
gc()

