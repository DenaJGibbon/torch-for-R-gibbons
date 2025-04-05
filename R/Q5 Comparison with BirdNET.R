library(ggpubr)
library(plyr)

# Crested gibbon binary ---------------------------------------------------

ClipDetections <- list.files('/Volumes/DJC Files/MultiSpeciesTransferLearning_R1/BirdNETComparison/Jahoo/BirdNETOutput',
                             recursive = T,full.names = T)

ClipDetectionsShort <- list.files('/Volumes/DJC Files/MultiSpeciesTransferLearning_R1/BirdNETComparison/Jahoo/BirdNETOutput/',
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


# Grey gibbon binary ------------------------------------------------------

ClipDetections <- list.files('/Volumes/DJC Files/MultiSpeciesTransferLearning_R1/BirdNETComparison/Danum/BirdNETOutput',
                             recursive = T,full.names = T)

ClipDetectionsShort <- list.files('/Volumes/DJC Files/MultiSpeciesTransferLearning_R1/BirdNETComparison/Danum/BirdNETOutput/',
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








# Grey gibbon multi -------------------------------------------------------
ClipDetections <- list.files('/Volumes/DJC Files/MultiSpeciesTransferLearning_R1/BirdNETComparison/MultiClass/BirdNETOutput',
                             recursive = T,full.names = T)

ClipDetectionsShort <- list.files('/Volumes/DJC Files/MultiSpeciesTransferLearning_R1/BirdNETComparison/MultiClass/BirdNETOutput/',
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

BirdNETGreyMultiPerformanceDF$ActualLabel <- revalue(BirdNETGreyMultiPerformanceDF$ActualLabel,
        c('CrestedGibbons'='Noise'))

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



# Crested multi -----------------------------------------------------------
ClipDetections <- list.files('/Volumes/DJC Files/MultiSpeciesTransferLearning_R1/BirdNETComparison/MultiClass/BirdNETOutput3000hz',
                             recursive = T,full.names = T)

ClipDetectionsShort <- list.files('/Volumes/DJC Files/MultiSpeciesTransferLearning_R1/BirdNETComparison/MultiClass/BirdNETOutput3000hz/',
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

BirdNETCrestedMultiPerformanceDF$ActualLabel <- revalue(BirdNETCrestedMultiPerformanceDF$ActualLabel,
                                                     c('GreyGibbons'='Noise'))

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


# gibbonNetR Grey gibbon --------------------------------------------------

DanumFiles <- list.files('/Volumes/DJC Files/MultiSpeciesTransferLearning_R1/TestOutput/modelruns_repeatsubset',
           recursive = TRUE,full.names = TRUE)

DanumFiles <- DanumFiles[which(str_detect(DanumFiles,pattern = 'performance_tables'))]

DanumFilesCombined <- DanumFiles %>%
  map(read_csv,show_col_types = FALSE) %>%
  bind_rows()

MaxAUC <-max(DanumFilesCombined$AUC)

DanumFilesCombinedSubset <- 
  as.data.frame(subset(DanumFilesCombined,AUC==MaxAUC))

unique(DanumFilesCombinedSubset$`Training Data`)

MaxF1GreyGibbonCNNBinary <- round(max(na.omit(DanumFilesCombinedSubset$F1)),2)
MaxAUCGreyGibbonCNNBinary <- round(max(na.omit(DanumFilesCombinedSubset$AUC)),2)

GreyGibbonCNNBinaryPlot <- ggplot(data = DanumFilesCombinedSubset, aes(x = Threshold)) +
  geom_line(aes(y = F1, color = "F1", linetype = "F1")) +
  geom_line(aes(y = Precision, color = "Precision", linetype = "Precision")) +
  geom_line(aes(y = Recall, color = "Recall", linetype = "Recall")) +
  labs(title = paste("Grey Gibbons (ResNet50 Binary) \n",'Max F1=', MaxF1GreyGibbonCNNBinary,
                     'Max AUC=',MaxAUCGreyGibbonCNNBinary),
       x = "Confidence",
       y = "Values") +
  scale_color_manual(values = c("F1" = "blue", "Precision" = "red", "Recall" = "green"),
                     labels = c("F1", "Precision", "Recall")) +
  scale_linetype_manual(values = c("F1" = "dashed", "Precision" = "dotted", "Recall" = "solid")) +
  theme_minimal() +
  theme(legend.title = element_blank())+
  labs(color  = "Guide name", linetype = "Guide name", shape = "Guide name")+ylim(0,1)

GreyGibbonCNNBinaryPlot


# Crested CNN Binary ------------------------------------------------------
JahooFiles <- list.files('/Volumes/DJC Files/MultiSpeciesTransferLearning_R1/TestOutput/modelruns_repeatsubset_Jahoo',
                         recursive = TRUE,full.names = TRUE)

JahooFiles <- JahooFiles[which(str_detect(JahooFiles,pattern = 'performance_tables'))]

JahooFilesCombined <- JahooFiles %>%
  map(read_csv,show_col_types = FALSE) %>%
  bind_rows()

MaxAUC <-max(JahooFilesCombined$AUC)

JahooFilesCombinedSubset <- 
  as.data.frame(subset(JahooFilesCombined,AUC==MaxAUC))

unique(JahooFilesCombinedSubset$`Training Data`)

MaxF1CrestedGibbonCNNBinary <- round(max(na.omit(JahooFilesCombinedSubset$F1)),2)
MaxAUCCrestedGibbonCNNBinary <- round(max(na.omit(JahooFilesCombinedSubset$AUC)),2)

CrestedGibbonCNNBinaryPlot <- ggplot(data = JahooFilesCombinedSubset, aes(x = Threshold)) +
  geom_line(aes(y = F1, color = "F1", linetype = "F1")) +
  geom_line(aes(y = Precision, color = "Precision", linetype = "Precision")) +
  geom_line(aes(y = Recall, color = "Recall", linetype = "Recall")) +
  labs(title = paste("Crested Gibbons (ResNet50 Binary) \n",'Max F1=', MaxF1CrestedGibbonCNNBinary,
                     'Max AUC=',MaxAUCCrestedGibbonCNNBinary),
       x = "Confidence",
       y = "Values") +
  scale_color_manual(values = c("F1" = "blue", "Precision" = "red", "Recall" = "green"),
                     labels = c("F1", "Precision", "Recall")) +
  scale_linetype_manual(values = c("F1" = "dashed", "Precision" = "dotted", "Recall" = "solid")) +
  theme_minimal() +
  theme(legend.title = element_blank())+
  labs(color  = "Guide name", linetype = "Guide name", shape = "Guide name")+ylim(0,1)

CrestedGibbonCNNBinaryPlot



cowplot::plot_grid(CrestedGibbonBirdNETBinaryPlot, CrestedGibbonCNNBinaryPlot,
                   GreyGibbonBirdNETBinaryPlot, GreyGibbonCNNBinaryPlot,
                   CrestedGibbonBirdNETMultiPlot,GreyGibbonBirdNETMultiPlot,
                   nrow=3, 
                   labels=c('A)','B)','C)','D)','E)','F)','G)','H)')
                   )

