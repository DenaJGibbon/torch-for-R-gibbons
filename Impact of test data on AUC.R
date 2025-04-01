# Is low AUC of multi due to test data?

output.dir <- "/Volumes/DJC Files/MultiSpeciesTransferLearning_R1/ImpactOfTestDataAUC/"


# Location of spectrogram images for testing
test.data.path <- '/Volumes/DJC Files/MultiSpeciesTransferLearning/DataAugmentation/images/Combined/CombinedTest/test/'

trained_models_dir <- '/Volumes/DJC Files/MultiSpeciesTransferLearning/InitialModelEvaluation/model_output_2/_imagesmulti_multi_unfrozen_TRUE_'


class_names <-  dput(list.files(test.data.path))

output_dir <- '/Volumes/DJC Files/MultiSpeciesTransferLearning/InitialModelEvaluation/model_output_2/evaluation_WA/'

# Evaluate the performance of the trained models using the test images
evaluate_trainedmodel_performance_multi(trained_models_dir=trained_models_dir,
                                        class_names=class_names,
                                        image_data_dir=test.data.path,
                                        output_dir= output_dir,
                                        noise.category = "Noise")

