library(ggpubr)
library(plyr)
library(caret)
library(dplyr)
library(purrr)
library(tidyr)
library(readr)
library(cowplot)
library(flextable)

# Crested gibbon binary BirdNET ---------------------------------------------------

ClipDetections <- list.files('results/part4/BirdNETComparison/Jahoo/BirdNETOutput',
                             recursive = T,full.names = T)

ClipDetectionsShort <- list.files('results/part4/BirdNETComparison/Jahoo/BirdNETOutput',
                                  recursive = T,full.names = F)

BirdNETCrestedBinaryPerformanceDF <- data.frame()

for(a in 1: length(ClipDetections)){
  
  TempDF <- read.delim(ClipDetections[a])
  
  TempDF <-  subset(TempDF,Common.Name=='NG')
  
  TempDF$Common.Name <- revalue(TempDF$Common.Name,
          c('NG'= 'Gibbon'))
  # Find the highest confidence for each clip
  if(nrow(TempDF) > 0){
    Confidence <- max(TempDF$Confidence)
    TempDF <- TempDF[which.max(TempDF$Confidence),]
    ActualLabel <- dirname(ClipDetectionsShort[a])
  } else{
    
    Confidence <- 0
    
    ActualLabel <- dirname(ClipDetectionsShort[a])
  }
  
  TempRow <- cbind.data.frame(Confidence, ActualLabel)
  TempRow$FileName <-ClipDetectionsShort[a]
  TempRow$BirdNETCrestedBinaryBinary <- ifelse(TempRow$Confidence <= 0.5, 'Noise','Gibbon')
  BirdNETCrestedBinaryPerformanceDF <- rbind.data.frame(BirdNETCrestedBinaryPerformanceDF,TempRow)
}


caretConf <- caret::confusionMatrix(
  as.factor(BirdNETCrestedBinaryPerformanceDF$BirdNETCrestedBinaryBinary),
  as.factor(BirdNETCrestedBinaryPerformanceDF$ActualLabel),
  mode = 'everything')
caretConf

# Define a vector of confidence Thresholds
Thresholds <- seq(0.1,1,0.1)

# Create an empty data frame to store results
BestF1data.frameCrestedGibbonBirdNETBinary <- data.frame()

# Loop through each threshold value
for(a in 1:length(Thresholds)){
  
  # Filter the subset based on the confidence threshold
  TopModelDetectionDF_single <-BirdNETCrestedBinaryPerformanceDF
  
  TopModelDetectionDF_single$PredictedClass <-
    ifelse(TopModelDetectionDF_single$Confidence  <=Thresholds[a], 'Noise','Gibbon')
  
  # Calculate confusion matrix using caret package
  caretConf <- caret::confusionMatrix(
    as.factor(TopModelDetectionDF_single$PredictedClass),
    as.factor(TopModelDetectionDF_single$ActualLabel),
    mode = 'everything')
  
  
  # Extract F1 score, Precision, and Recall from the confusion matrix
  F1 <- caretConf$byClass[7]
  Precision <- caretConf$byClass[5]
  Recall <- caretConf$byClass[6]
  FP <- caretConf$table[1,2]
  # TN <- caretConf$table[2,2]+JahooAdj
  # FPR <-  FP / (FP + TN)
  # # Create a row for the result and add it to the BestF1data.frameGreyGibbon
  #TrainingData <- training_data_type
  TempF1Row <- cbind.data.frame(F1, Precision, Recall)#,FPR
  TempF1Row$Thresholds <- Thresholds[a]
  BestF1data.frameCrestedGibbonBirdNETBinary <- rbind.data.frame(BestF1data.frameCrestedGibbonBirdNETBinary, TempF1Row)
}

BestF1data.frameCrestedGibbonBirdNETBinary
max(na.omit(BestF1data.frameCrestedGibbonBirdNETBinary$F1))
BestF1data.frameCrestedGibbonBirdNETBinary[which.max(na.omit(BestF1data.frameCrestedGibbonBirdNETBinary$F1)),]

BirdNETCrestedBinaryPerformanceDF$BinaryLabel <- 
  ifelse(BirdNETCrestedBinaryPerformanceDF$ActualLabel == "Gibbon",1,0)

ROCRpred <- ROCR::prediction(predictions = as.numeric(BirdNETCrestedBinaryPerformanceDF$Confidence), labels = BirdNETCrestedBinaryPerformanceDF$BinaryLabel)
AUCvalCrestedBinary <- ROCR::performance(ROCRpred, "auc")
AUCvalCrestedBinary@y.values[[1]]

MaxF1BirdNETBinary <- round(max(na.omit(BestF1data.frameCrestedGibbonBirdNETBinary$F1)),2)
MaxAUCBirdNETBinary <- round(AUCvalCrestedBinary@y.values[[1]],2)

# Metric plot
CrestedGibbonBirdNETBinaryPlot <- ggplot(data = BestF1data.frameCrestedGibbonBirdNETBinary, aes(x = Thresholds)) +
  geom_line(aes(y = F1, color = "F1", linetype = "F1")) +
  geom_line(aes(y = Precision, color = "Precision", linetype = "Precision")) +
  geom_line(aes(y = Recall, color = "Recall", linetype = "Recall")) +
  labs(title = paste("Crested Gibbons (BirdNET Binary) \n",'Max F1=', MaxF1BirdNETBinary,
                     'Max AUC=',MaxAUCBirdNETBinary),
       x = "Confidence",
       y = "Values") +
  scale_color_manual(values = c("F1" = "blue", "Precision" = "red", "Recall" = "green"),
                     labels = c("F1", "Precision", "Recall")) +
  scale_linetype_manual(values = c("F1" = "dashed", "Precision" = "dotted", "Recall" = "solid")) +
  theme_minimal() +
  theme(legend.title = element_blank())+
  labs(color  = "Guide name", linetype = "Guide name", shape = "Guide name")+ylim(0,1)

CrestedGibbonBirdNETBinaryPlot


# Grey gibbon binary BirdNET------------------------------------------------------

ClipDetections <- list.files('results/part4/BirdNETComparison/Danum/BirdNETOutput',
                             recursive = T,full.names = T)

ClipDetectionsShort <- list.files('results/part4/BirdNETComparison/Danum/BirdNETOutput',
                                  recursive = T,full.names = F)

BirdNETGreyBinaryPerformanceDF <- data.frame()

for(a in 1: length(ClipDetections)){
  
  TempDF <- read.delim(ClipDetections[a])
  
  TempDF <-  subset(TempDF,Common.Name=='Gibbons')
  
  TempDF$Common.Name <- revalue(TempDF$Common.Name,
                                c('Gibbons'= 'Gibbon'))
  # Find the highest confidence for each clip
  if(nrow(TempDF) > 0){
    Confidence <- max(TempDF$Confidence)
    TempDF <- TempDF[which.max(TempDF$Confidence),]
    ActualLabel <- dirname(ClipDetectionsShort[a])
  } else{
    
    Confidence <- 0
    
    ActualLabel <- dirname(ClipDetectionsShort[a])
  }
  
  TempRow <- cbind.data.frame(Confidence, ActualLabel)
  TempRow$FileName <-ClipDetectionsShort[a]
  TempRow$BirdNETGreyBinaryBinary <- ifelse(TempRow$Confidence <= 0.5, 'Noise','Gibbon')
  BirdNETGreyBinaryPerformanceDF <- rbind.data.frame(BirdNETGreyBinaryPerformanceDF,TempRow)
}


caretConf <- caret::confusionMatrix(
  as.factor(BirdNETGreyBinaryPerformanceDF$BirdNETGreyBinaryBinary),
  as.factor(BirdNETGreyBinaryPerformanceDF$ActualLabel),
  mode = 'everything')
caretConf

# Define a vector of confidence Thresholds
Thresholds <- seq(0.1,1,0.1)

# Create an empty data frame to store results
BestF1data.frameGreyGibbonBirdNETBinary <- data.frame()

# Loop through each threshold value
for(a in 1:length(Thresholds)){
  
  # Filter the subset based on the confidence threshold
  TopModelDetectionDF_single <-BirdNETGreyBinaryPerformanceDF
  
  TopModelDetectionDF_single$PredictedClass <-
    ifelse(TopModelDetectionDF_single$Confidence  <=Thresholds[a], 'Noise','Gibbon')
  
  # Calculate confusion matrix using caret package
  caretConf <- caret::confusionMatrix(
    as.factor(TopModelDetectionDF_single$PredictedClass),
    as.factor(TopModelDetectionDF_single$ActualLabel),
    mode = 'everything')
  
  
  # Extract F1 score, Precision, and Recall from the confusion matrix
  F1 <- caretConf$byClass[7]
  Precision <- caretConf$byClass[5]
  Recall <- caretConf$byClass[6]
  FP <- caretConf$table[1,2]
  # TN <- caretConf$table[2,2]+JahooAdj
  # FPR <-  FP / (FP + TN)
  # # Create a row for the result and add it to the BestF1data.frameGreyGibbon
  #TrainingData <- training_data_type
  TempF1Row <- cbind.data.frame(F1, Precision, Recall)#,FPR
  TempF1Row$Thresholds <- Thresholds[a]
  BestF1data.frameGreyGibbonBirdNETBinary <- rbind.data.frame(BestF1data.frameGreyGibbonBirdNETBinary, TempF1Row)
}

BestF1data.frameGreyGibbonBirdNETBinary
max(na.omit(BestF1data.frameGreyGibbonBirdNETBinary$F1))
BestF1data.frameGreyGibbonBirdNETBinary[which.max(na.omit(BestF1data.frameGreyGibbonBirdNETBinary$F1)),]

BirdNETGreyBinaryPerformanceDF$BinaryLabel <- 
  ifelse(BirdNETGreyBinaryPerformanceDF$ActualLabel == "Gibbon",1,0)

ROCRpred <- ROCR::prediction(predictions = as.numeric(BirdNETGreyBinaryPerformanceDF$Confidence), labels = BirdNETGreyBinaryPerformanceDF$BinaryLabel)
AUCvalGreyBinary <- ROCR::performance(ROCRpred, "auc")
AUCvalGreyBinary@y.values[[1]]

MaxF1BirdNETBinary <- round(max(na.omit(BestF1data.frameGreyGibbonBirdNETBinary$F1)),2)
MaxAUCBirdNETBinary <- round(AUCvalGreyBinary@y.values[[1]],2)

# Metric plot
GreyGibbonBirdNETBinaryPlot <- ggplot(data = BestF1data.frameGreyGibbonBirdNETBinary, aes(x = Thresholds)) +
  geom_line(aes(y = F1, color = "F1", linetype = "F1")) +
  geom_line(aes(y = Precision, color = "Precision", linetype = "Precision")) +
  geom_line(aes(y = Recall, color = "Recall", linetype = "Recall")) +
  labs(title = paste("Grey Gibbons (BirdNET Binary) \n",'Max F1=', MaxF1BirdNETBinary,
                     'Max AUC=',MaxAUCBirdNETBinary),
       x = "Confidence",
       y = "Values") +
  scale_color_manual(values = c("F1" = "blue", "Precision" = "red", "Recall" = "green"),
                     labels = c("F1", "Precision", "Recall")) +
  scale_linetype_manual(values = c("F1" = "dashed", "Precision" = "dotted", "Recall" = "solid")) +
  theme_minimal() +
  theme(legend.title = element_blank())+
  labs(color  = "Guide name", linetype = "Guide name", shape = "Guide name")+ylim(0,1)

GreyGibbonBirdNETBinaryPlot








# Grey gibbon multi BirdNET -------------------------------------------------------
ClipDetections <- list.files('results/part4/BirdNETComparison/MultiClass/BirdNETOutput',
                             recursive = T,full.names = T)

ClipDetectionsShort <- list.files('results/part4/BirdNETComparison/MultiClass/BirdNETOutput',
                                  recursive = T,full.names = F)

BirdNETGreyMultiPerformanceDF <- data.frame()

for(a in 1: length(ClipDetections)){
  
  TempDF <- read.delim(ClipDetections[a])
  
  TempDF <-  subset(TempDF,Common.Name=='GreyGibbons')
  
 # Find the highest confidence for each clip
  if(nrow(TempDF) > 0){
    Confidence <- max(TempDF$Confidence)
    TempDF <- TempDF[which.max(TempDF$Confidence),]
    ActualLabel <- dirname(ClipDetectionsShort[a])
  } else{
    
    Confidence <- 0
    
    ActualLabel <- dirname(ClipDetectionsShort[a])
  }
  
  TempRow <- cbind.data.frame(Confidence, ActualLabel)
  TempRow$FileName <-ClipDetectionsShort[a]
  TempRow$BirdNETGreyMulti <- ifelse(TempRow$Confidence <= 0.5, 'Noise','GreyGibbons')
  BirdNETGreyMultiPerformanceDF <- rbind.data.frame(BirdNETGreyMultiPerformanceDF,TempRow)
}

BirdNETGreyMultiPerformanceDF <- 
  subset(BirdNETGreyMultiPerformanceDF,ActualLabel !='CrestedGibbons' )


caretConf <- caret::confusionMatrix(
  as.factor(BirdNETGreyMultiPerformanceDF$BirdNETGreyMulti),
  as.factor(BirdNETGreyMultiPerformanceDF$ActualLabel),
  mode = 'everything')
caretConf

# Define a vector of confidence Thresholds
Thresholds <- seq(0.1,1,0.1)

# Create an empty data frame to store results
BestF1data.frameGreyGibbonBirdNETMulti <- data.frame()

# Loop through each threshold value
for(a in 1:length(Thresholds)){
  
  # Filter the subset based on the confidence threshold
  TopModelDetectionDF_single <-BirdNETGreyMultiPerformanceDF
  
  TopModelDetectionDF_single$PredictedClass <-
    ifelse(TopModelDetectionDF_single$Confidence  <=Thresholds[a], 'Noise','GreyGibbons')
  
  # Calculate confusion matrix using caret package
  caretConf <- caret::confusionMatrix(
    as.factor(TopModelDetectionDF_single$PredictedClass),
    as.factor(TopModelDetectionDF_single$ActualLabel),
    mode = 'everything')
  
  
  # Extract F1 score, Precision, and Recall from the confusion matrix
  F1 <- caretConf$byClass[7]
  Precision <- caretConf$byClass[5]
  Recall <- caretConf$byClass[6]
  FP <- caretConf$table[1,2]
  # TN <- caretConf$table[2,2]+JahooAdj
  # FPR <-  FP / (FP + TN)
  # # Create a row for the result and add it to the BestF1data.frameGreyGibbon
  #TrainingData <- training_data_type
  TempF1Row <- cbind.data.frame(F1, Precision, Recall)#,FPR
  TempF1Row$Thresholds <- Thresholds[a]
  BestF1data.frameGreyGibbonBirdNETMulti <- rbind.data.frame(BestF1data.frameGreyGibbonBirdNETMulti, TempF1Row)
}

BestF1data.frameGreyGibbonBirdNETMulti
max(na.omit(BestF1data.frameGreyGibbonBirdNETMulti$F1))
BestF1data.frameGreyGibbonBirdNETMulti[which.max(na.omit(BestF1data.frameGreyGibbonBirdNETMulti$F1)),]

BirdNETGreyMultiPerformanceDF$MultiLabel <- 
  ifelse(BirdNETGreyMultiPerformanceDF$ActualLabel == "GreyGibbons",1,0)

ROCRpred <- ROCR::prediction(predictions = as.numeric(BirdNETGreyMultiPerformanceDF$Confidence), labels = BirdNETGreyMultiPerformanceDF$MultiLabel)
AUCvalGreyMulti <- ROCR::performance(ROCRpred, "auc")
AUCvalGreyMulti@y.values[[1]]

MaxF1BirdNETMulti <- round(max(na.omit(BestF1data.frameGreyGibbonBirdNETMulti$F1)),2)
MaxAUCBirdNETMulti <- round(AUCvalGreyMulti@y.values[[1]],2)

# Metric plot
GreyGibbonBirdNETMultiPlot <- ggplot(data = BestF1data.frameGreyGibbonBirdNETMulti, aes(x = Thresholds)) +
  geom_line(aes(y = F1, color = "F1", linetype = "F1")) +
  geom_line(aes(y = Precision, color = "Precision", linetype = "Precision")) +
  geom_line(aes(y = Recall, color = "Recall", linetype = "Recall")) +
  labs(title = paste("Grey Gibbons (BirdNET Multi) \n",'Max F1=', MaxF1BirdNETMulti,
                     'Max AUC=',MaxAUCBirdNETMulti),
       x = "Confidence",
       y = "Values") +
  scale_color_manual(values = c("F1" = "blue", "Precision" = "red", "Recall" = "green"),
                     labels = c("F1", "Precision", "Recall")) +
  scale_linetype_manual(values = c("F1" = "dashed", "Precision" = "dotted", "Recall" = "solid")) +
  theme_minimal() +
  theme(legend.title = element_blank())+
  labs(color  = "Guide name", linetype = "Guide name", shape = "Guide name")+ylim(0,1)

GreyGibbonBirdNETMultiPlot



# Crested multi BirdNET-----------------------------------------------------------
ClipDetections <- list.files('results/part4/BirdNETComparison/MultiClass/BirdNETOutput3000hz',
                             recursive = T,full.names = T)

ClipDetectionsShort <- list.files('results/part4/BirdNETComparison/MultiClass/BirdNETOutput3000hz',
                                  recursive = T,full.names = F)

BirdNETCrestedMultiPerformanceDF <- data.frame()

for(a in 1: length(ClipDetections)){
  
  TempDF <- read.delim(ClipDetections[a])
  
  TempDF <-  subset(TempDF,Common.Name=='CrestedGibbons')
  
  # Find the highest confidence for each clip
  if(nrow(TempDF) > 0){
    Confidence <- max(TempDF$Confidence)
    TempDF <- TempDF[which.max(TempDF$Confidence),]
    ActualLabel <- dirname(ClipDetectionsShort[a])
  } else{
    
    Confidence <- 0
    
    ActualLabel <- dirname(ClipDetectionsShort[a])
  }
  
  TempRow <- cbind.data.frame(Confidence, ActualLabel)
  TempRow$FileName <-ClipDetectionsShort[a]
  TempRow$BirdNETCrestedMulti <- ifelse(TempRow$Confidence <= 0.5, 'Noise','CrestedGibbons')
  BirdNETCrestedMultiPerformanceDF <- rbind.data.frame(BirdNETCrestedMultiPerformanceDF,TempRow)
}

BirdNETCrestedMultiPerformanceDF <- 
  subset(BirdNETCrestedMultiPerformanceDF,ActualLabel!='GreyGibbons')

caretConf <- caret::confusionMatrix(
  as.factor(BirdNETCrestedMultiPerformanceDF$BirdNETCrestedMulti),
  as.factor(BirdNETCrestedMultiPerformanceDF$ActualLabel),
  mode = 'everything')
caretConf

# Define a vector of confidence Thresholds
Thresholds <- seq(0.1,1,0.1)

# Create an empty data frame to store results
BestF1data.frameCrestedGibbonBirdNETMulti <- data.frame()

# Loop through each threshold value
for(a in 1:length(Thresholds)){
  
  # Filter the subset based on the confidence threshold
  TopModelDetectionDF_single <-BirdNETCrestedMultiPerformanceDF
  
  TopModelDetectionDF_single$PredictedClass <-
    ifelse(TopModelDetectionDF_single$Confidence  <=Thresholds[a], 'Noise','CrestedGibbons')
  
  # Calculate confusion matrix using caret package
  caretConf <- caret::confusionMatrix(
    as.factor(TopModelDetectionDF_single$PredictedClass),
    as.factor(TopModelDetectionDF_single$ActualLabel),
    mode = 'everything')
  
  
  # Extract F1 score, Precision, and Recall from the confusion matrix
  F1 <- caretConf$byClass[7]
  Precision <- caretConf$byClass[5]
  Recall <- caretConf$byClass[6]
  FP <- caretConf$table[1,2]
  # TN <- caretConf$table[2,2]+JahooAdj
  # FPR <-  FP / (FP + TN)
  # # Create a row for the result and add it to the BestF1data.frameCrestedGibbon
  #TrainingData <- training_data_type
  TempF1Row <- cbind.data.frame(F1, Precision, Recall)#,FPR
  TempF1Row$Thresholds <- Thresholds[a]
  BestF1data.frameCrestedGibbonBirdNETMulti <- rbind.data.frame(BestF1data.frameCrestedGibbonBirdNETMulti, TempF1Row)
}

BestF1data.frameCrestedGibbonBirdNETMulti
max(na.omit(BestF1data.frameCrestedGibbonBirdNETMulti$F1))
BestF1data.frameCrestedGibbonBirdNETMulti[which.max(na.omit(BestF1data.frameCrestedGibbonBirdNETMulti$F1)),]

BirdNETCrestedMultiPerformanceDF$MultiLabel <- 
  ifelse(BirdNETCrestedMultiPerformanceDF$ActualLabel == "CrestedGibbons",1,0)

ROCRpred <- ROCR::prediction(predictions = as.numeric(BirdNETCrestedMultiPerformanceDF$Confidence), labels = BirdNETCrestedMultiPerformanceDF$MultiLabel)
AUCvalCrestedMulti <- ROCR::performance(ROCRpred, "auc")
AUCvalCrestedMulti@y.values[[1]]

MaxF1BirdNETMulti <- round(max(na.omit(BestF1data.frameCrestedGibbonBirdNETMulti$F1)),2)
MaxAUCBirdNETMulti <- round(AUCvalCrestedMulti@y.values[[1]],2)

# Metric plot
CrestedGibbonBirdNETMultiPlot <- ggplot(data = BestF1data.frameCrestedGibbonBirdNETMulti, aes(x = Thresholds)) +
  geom_line(aes(y = F1, color = "F1", linetype = "F1")) +
  geom_line(aes(y = Precision, color = "Precision", linetype = "Precision")) +
  geom_line(aes(y = Recall, color = "Recall", linetype = "Recall")) +
  labs(title = paste("Crested Gibbons (BirdNET Multi) \n",'Max F1=', MaxF1BirdNETMulti,
                     'Max AUC=',MaxAUCBirdNETMulti),
       x = "Confidence",
       y = "Values") +
  scale_color_manual(values = c("F1" = "blue", "Precision" = "red", "Recall" = "green"),
                     labels = c("F1", "Precision", "Recall")) +
  scale_linetype_manual(values = c("F1" = "dashed", "Precision" = "dotted", "Recall" = "solid")) +
  theme_minimal() +
  theme(legend.title = element_blank())+
  labs(color  = "Guide name", linetype = "Guide name", shape = "Guide name")+ylim(0,1)

CrestedGibbonBirdNETMultiPlot

# Multi Grey gibbon --------------------------------------------------

DanumFiles <- list.files('results/part3/TestOutput/multi_all_augmented_testdata_v1/Combined copy _jitter_1_Combined copy _jitter_multi_unfrozen_TRUE_/',
                         recursive = TRUE,full.names = TRUE)

DanumFilesCombined <- DanumFiles %>%
  map(~read_csv(.x, show_col_types = FALSE) %>%
        mutate(`N epochs` = as.character(`N epochs`))) %>%
  bind_rows()

DanumFilesCombinedSubset <- 
  as.data.frame(subset(DanumFilesCombined, Class=='GreyGibbons'))

MaxAUC <-max(DanumFilesCombinedSubset$AUC)

DanumFilesCombinedSubset <- 
  as.data.frame(subset(DanumFilesCombinedSubset,AUC==MaxAUC ))

unique(DanumFilesCombinedSubset$`Training Data`)

MaxF1GreyGibbonCNNMulti <- round(max(na.omit(DanumFilesCombinedSubset$F1)),2)
MaxAUCGreyGibbonCNNMulti <- round(max(na.omit(DanumFilesCombinedSubset$AUC)),2)

GreyGibbonCNNMultiPlot <- ggplot(data = DanumFilesCombinedSubset, aes(x = Threshold)) +
  geom_line(aes(y = F1, color = "F1", linetype = "F1")) +
  geom_line(aes(y = Precision, color = "Precision", linetype = "Precision")) +
  geom_line(aes(y = Recall, color = "Recall", linetype = "Recall")) +
  labs(title = paste("Grey Gibbons (ResNet50 Multi) \n",'Max F1=', MaxF1GreyGibbonCNNMulti,
                     'Max AUC=',MaxAUCGreyGibbonCNNMulti),
       x = "Confidence",
       y = "Values") +
  scale_color_manual(values = c("F1" = "blue", "Precision" = "red", "Recall" = "green"),
                     labels = c("F1", "Precision", "Recall")) +
  scale_linetype_manual(values = c("F1" = "dashed", "Precision" = "dotted", "Recall" = "solid")) +
  theme_minimal() +
  theme(legend.title = element_blank())+
  labs(color  = "Guide name", linetype = "Guide name", shape = "Guide name")+ylim(0,1)

GreyGibbonCNNMultiPlot


# Multi Crested CNN  ------------------------------------------------------
JahooFiles <- list.files('results/part3/TestOutput/multi_all_augmented_testdata_v1/Combined copy _jitter_1_Combined copy _jitter_multi_unfrozen_TRUE_/',
                         recursive = TRUE,full.names = TRUE)

JahooFilesCombined <- JahooFiles %>%
  map(~read_csv(.x, show_col_types = FALSE) %>%
        mutate(`N epochs` = as.character(`N epochs`))) %>%
  bind_rows()


JahooFilesCombinedSubset <- 
  as.data.frame(subset(JahooFilesCombined,Class=='CrestedGibbons' ))

MaxAUC <-max(JahooFilesCombinedSubset$AUC)

JahooFilesCombinedSubset <- 
  as.data.frame(subset(JahooFilesCombinedSubset,AUC==MaxAUC ))

unique(JahooFilesCombinedSubset$`Training Data`)

MaxF1CrestedGibbonCNNMulti <- round(max(na.omit(JahooFilesCombinedSubset$F1)),2)
MaxAUCCrestedGibbonCNNMulti <- round(max(na.omit(JahooFilesCombinedSubset$AUC)),2)

CrestedGibbonCNNMultiPlot <- ggplot(data = JahooFilesCombinedSubset, aes(x = Threshold)) +
  geom_line(aes(y = F1, color = "F1", linetype = "F1")) +
  geom_line(aes(y = Precision, color = "Precision", linetype = "Precision")) +
  geom_line(aes(y = Recall, color = "Recall", linetype = "Recall")) +
  labs(title = paste("Crested Gibbons (ResNet50 Multi) \n",'Max F1=', MaxF1CrestedGibbonCNNMulti,
                     'Max AUC=',MaxAUCCrestedGibbonCNNMulti),
       x = "Confidence",
       y = "Values") +
  scale_color_manual(values = c("F1" = "blue", "Precision" = "red", "Recall" = "green"),
                     labels = c("F1", "Precision", "Recall")) +
  scale_linetype_manual(values = c("F1" = "dashed", "Precision" = "dotted", "Recall" = "solid")) +
  theme_minimal() +
  theme(legend.title = element_blank())+
  labs(color  = "Guide name", linetype = "Guide name", shape = "Guide name")+ylim(0,1)

CrestedGibbonCNNMultiPlot




pdf('results/Online Supporting Material Figure 2. BirdNETComparison.pdf', height = 7, width = 9)

# Combine all plots into a single panel
combined_plot <- plot_grid(
  CrestedGibbonBirdNETBinaryPlot, GreyGibbonBirdNETBinaryPlot,
  CrestedGibbonBirdNETMultiPlot, GreyGibbonBirdNETMultiPlot,
  CrestedGibbonCNNMultiPlot, GreyGibbonCNNMultiPlot,
  nrow = 3,
  labels = c('A)', 'B)', 'C)', 'D)', 'E)', 'F)')
)

# Add caption below the combined plot
captioned_plot <- ggdraw() +
  draw_plot(combined_plot, 0, 0.05, 1, 0.95) +
  draw_label("OSM Figure 2. Performance comparison of BirdNET and CNN models for binary and multi-class classification of gibbon calls.",
             x = 0.5, y = 0.01, hjust = 0.5, size = 10)

# Print to the PDF device
print(captioned_plot)

dev.off()


# Create Comparision Table  ------------------------------------------------------------------
# Crested gibbon results
crested_max_index <- which.max(BestF1data.frameCrestedGibbonBirdNETBinary$F1)
crested_best_row <- BestF1data.frameCrestedGibbonBirdNETBinary[crested_max_index, ]
crested_auc <- round(AUCvalCrestedBinary@y.values[[1]], 2)

# Grey gibbon results
grey_max_index <- which.max(BestF1data.frameGreyGibbonBirdNETBinary$F1)
grey_best_row <- BestF1data.frameGreyGibbonBirdNETBinary[grey_max_index, ]
grey_auc <- round(AUCvalGreyBinary@y.values[[1]], 2)

# Combine into a summary table
BirdNETBinarySummaryTable <- data.frame(
  Species = c("Crested Gibbon", "Grey Gibbon"),
  Model = c("BirdNET Binary", "BirdNET Binary"),
  Max_F1 = round(c(crested_best_row$F1, grey_best_row$F1), 2),
  Precision = round(c(crested_best_row$Precision, grey_best_row$Precision), 2),
  Recall = round(c(crested_best_row$Recall, grey_best_row$Recall), 2),
  Threshold = c(crested_best_row$Thresholds, grey_best_row$Thresholds),
  AUC = c(crested_auc, grey_auc)
)

print(BirdNETBinarySummaryTable)

# Extract best performing row (max F1) for each species
best_row_grey <- DanumFilesCombinedSubset[which.max(DanumFilesCombinedSubset$F1), ]
best_row_crest <- JahooFilesCombinedSubset[which.max(JahooFilesCombinedSubset$F1), ]

# Create summary table
MultiCNN_SummaryTable <- data.frame(
  Species = c("Grey Gibbon", "Crested Gibbon"),
  Model = c("AlexNet Multi", "ResNet50 Multi"),
  Max_F1 = round(c(best_row_grey$F1, best_row_crest$F1), 2),
  Precision = round(c(best_row_grey$Precision, best_row_crest$Precision), 2),
  Recall = round(c(best_row_grey$Recall, best_row_crest$Recall), 2),
  Threshold = round(c(best_row_grey$Threshold, best_row_crest$Threshold), 2),
  AUC = round(c(best_row_grey$AUC, best_row_crest$AUC), 2)
)

print(MultiCNN_SummaryTable)

# --- For BirdNET Multi ---
# For Grey Gibbon (BirdNET Multi)
best_row_birdnet_grey <- BestF1data.frameGreyGibbonBirdNETMulti[which.max(BestF1data.frameGreyGibbonBirdNETMulti$F1), ]
birdnet_grey_auc <- round(AUCvalGreyMulti@y.values[[1]], 2)

# For Crested Gibbon (BirdNET Multi)
best_row_birdnet_crest <- BestF1data.frameCrestedGibbonBirdNETMulti[which.max(BestF1data.frameCrestedGibbonBirdNETMulti$F1), ]
birdnet_crest_auc <- round(AUCvalCrestedMulti@y.values[[1]], 2)

BirdNETMulti_SummaryTable <- data.frame(
  Species = c("Grey Gibbon", "Crested Gibbon"),
  Model = c("BirdNET Multi", "BirdNET Multi"),
  Max_F1 = round(c(best_row_birdnet_grey$F1, best_row_birdnet_crest$F1), 2),
  Precision = round(c(best_row_birdnet_grey$Precision, best_row_birdnet_crest$Precision), 2),
  Recall = round(c(best_row_birdnet_grey$Recall, best_row_birdnet_crest$Recall), 2),
  Threshold = round(c(best_row_birdnet_grey$Thresholds, best_row_birdnet_crest$Thresholds), 2),
  AUC = c(birdnet_grey_auc, birdnet_crest_auc)
)


CombinedPerformanceTable <- 
  rbind.data.frame(BirdNETBinarySummaryTable,BirdNETMulti_SummaryTable,MultiCNN_SummaryTable)

head(CombinedPerformanceTable)

# Sort by Species, then descending AUC
CombinedPerformanceTable <- CombinedPerformanceTable %>%
  arrange(Species, desc(AUC))


CombinedPerformanceTableFT <- 
  flextable(CombinedPerformanceTable)


# flextable::save_as_docx(CombinedPerformanceTableFT,
#                         path='BirdNETComparisonTable.docx')

