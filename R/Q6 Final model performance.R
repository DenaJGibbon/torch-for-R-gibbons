devtools::load_all("/Users/denaclink/Desktop/RStudioProjects/gibbonNetR")

# Deply trained top model over WA test data ----------------------------------------
test.data.path <- '/Volumes/DJC Files/MultiSpeciesTransferLearning_R1/CombinedImagesWAEvaluation/Danum/'

trained_models_dir <-  '/Volumes/DJC Files/MultiSpeciesTransferLearning_R1/CombinedImagesWAEvaluation/topmodels/DanumTopModel/'


output_dir <- paste('/Volumes/DJC Files/MultiSpeciesTransferLearning_R1/CombinedImagesWAEvaluation/final_test_WA/', 
                      basename(trained_models_dir),'/',sep='')
  
dir.create(output_dir,recursive = TRUE)
  
evaluate_trainedmodel_performance_multi(
    trained_models_dir = trained_models_dir,
    image_data_dir = test.data.path,
    class_names = list.files(test.data.path),
    output_dir = output_dir,
    noise.category = "Noise"
  ) # Label for negative class
  


# Deply trained top model over WA test data ----------------------------------------
test.data.path <- '/Volumes/DJC Files/MultiSpeciesTransferLearning_R1/CombinedImagesWAEvaluation/Jahoo/'

trained_models_dir <-  '/Volumes/DJC Files/MultiSpeciesTransferLearning_R1/CombinedImagesWAEvaluation/topmodels/JahooTopModel/'


output_dir <- paste('/Volumes/DJC Files/MultiSpeciesTransferLearning_R1/CombinedImagesWAEvaluation/final_test_WA/', 
                    basename(trained_models_dir),'/',sep='')

dir.create(output_dir,recursive = TRUE)

evaluate_trainedmodel_performance_multi(
  trained_models_dir = trained_models_dir,
  image_data_dir = test.data.path,
  class_names = list.files(test.data.path),
  output_dir = output_dir,
  noise.category = "Noise"
) # Label for negative class



# Combine and plot results ------------------------------------------------


DanumTop <- 
  read.csv('/Volumes/DJC Files/MultiSpeciesTransferLearning_R1/CombinedImagesWAEvaluation/final_test_WA/DanumTopModel/performance_tables_multi_trained/Combined copy _jitter_5_resnet50_model_TransferLearningTrainedModel.csv')

DanumTop <- subset(DanumTop,Class =='GreyGibbons')

max(na.omit(DanumTop$AUC))
max(na.omit(DanumTop$F1))

JahooTop <- 
  read.csv('/Volumes/DJC Files/MultiSpeciesTransferLearning_R1/CombinedImagesWAEvaluation/final_test_WA/JahooTopModel/performance_tables_multi_trained/Combined copy _jitter_5_resnet50_model_TransferLearningTrainedModel.csv')

JahooTop <- subset(JahooTop,Class =='CrestedGibbons')

max(na.omit(JahooTop$AUC))
max(na.omit(JahooTop$F1))  


CombinedFinal<- 
  rbind.data.frame(DanumTop,JahooTop)

CombinedFinal$FPR <- 1 - CombinedFinal$Specificity

best_by_f1 <- CombinedFinal %>%
  group_by(Class) %>%
  filter(F1 == max(F1, na.rm = TRUE)) %>%
  slice(1) %>%  # in case of ties
  mutate(Type = "Best F1")

best_by_precision <- CombinedFinal %>%
  group_by(Class) %>%
  filter(Precision == max(Precision, na.rm = TRUE)) %>%
  slice(1) %>%
  mutate(Type = "Best Precision")

best_by_recall <- CombinedFinal %>%
  group_by(Class) %>%
  filter(Recall == max(Recall, na.rm = TRUE)) %>%
  slice(1) %>%
  mutate(Type = "Best Recall")

best_all <- bind_rows(best_by_f1, best_by_precision, best_by_recall) %>%
  select(Type, Class, CNN.Architecture, F1, AUC, Threshold, Precision, Recall, FPR) %>%
  mutate(
    F1 = round(F1, 2),
    AUC = round(AUC, 2),
    Threshold = round(Threshold, 2),
    Precision = round(Precision, 2),
    Recall = round(Recall, 2),
    FPR = round(FPR, 3)
  )

best_all_flextable <-
flextable::flextable(best_all)

flextable::save_as_docx(best_all_flextable,
                        path='FinalResults.docx')

# Pivot longer to gaAUC# Pivot longer to gather the metrics for plotting
CombinedFinal_long <- CombinedFinal %>%
  pivot_longer(cols = c(Precision, Recall, F1), 
               names_to = "Metric", values_to = "Score")

# Plot
ggplot(CombinedFinal_long, aes(x = Threshold, y = Score, color = Metric, linetype = Metric)) +
  geom_line(size = 1) +
  #geom_point(size = 2) +
  facet_wrap(~ Class) +
  labs(
       x = "Threshold", y = "Score") +
  scale_color_manual(values = viridis(3)) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold"))

