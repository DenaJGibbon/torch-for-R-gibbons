# Load libraries -------------------------------------------------------------------------
library(dplyr)
library(flextable)
library(dplyr)
library(gibbonNetR)

# Grey Gibbon Binary Model Training ---------------------------------------

TrainingFolders <- list.files('data/DataAugmentation/images/Danum Images/',
                              full.name=T)

# Location of spectrogram images for testing
test.data.path <-"data/training_images_sorted/Danum/test/"

output.dir <-'results/part3/DataAugmentation_V4/modelruns_repeatsubset/'

# Number of epochs to include
epoch.iterations <- c(1,5)

# Train the models specifying different architectures
architectures <-  c('alexnet', 'resnet50')

freeze.param <- c(TRUE)


# Grey Gibbon Binary Models with jitter -----------------------------------

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


# Grey Gibbon Binary Models without jitter --------------------------------
for(d in 1:1){
  for (a in 1:length(architectures)) {
    for (b in 1:length(freeze.param)) {
      for(c in 1:length(TrainingFolders)){

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
TrainingFolders <- list.files('data/DataAugmentation/images/Jahoo Images/',
                              full.name=T)

# Location of spectrogram images for testing
test.data.path <-'data/training_images_sorted/Jahoo/test/'

output.dir <-'results/part3/DataAugmentation_V4/modelruns_repeatsubset_Jahoo/'


# Crested Gibbon Binary Models with jitter --------------------------------
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

# Crested Gibbon Binary Models without jitter --------------------------------

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
TrainingFolders <- list.files('data/DataAugmentation/images/Combined/',
                              full.name=T)

TrainingFolders <- TrainingFolders[1:4]

# Location of spectrogram images for testing
test.data.path <- 'data/training_images_sorted/Combined/test/'

# Output directory for results
output.dir <-'results/part3/DataAugmentation_V4/modelruns_repeatsubset_multi/'


# Multiclass Models with jitter -------------------------------------------

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

# Multiclass Models without jitter -------------------------------------------

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



