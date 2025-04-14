library(gibbonNetR)

# NOTE this is just an example as the full dataset is not publically available
# For detailed usage instructions see: https://denajgibbon.github.io/gibbonNetR/


# Specify location of files -----------------------------------------------
JahooSoundFiles <- 'data/WideArrayEvaluation/Jahoo/SoundFiles/'

TopModelMulti <-  'results/part3/DataAugmentation_V4/modelruns_repeatsubset_multi/Combined _jitter_1_Combined _jitter_multi_unfrozen_TRUE_/_Combined _jitter_5_resnet50_model.pt'

# Deploy over directory of files ------------------------------------------
deploy_CNN_multi(
  clip_duration = 12,
  max_freq_khz = 3,
  architecture='resnet',
  output_folder = 'results/part6/DeploymentOutputV1_R1/KSWS/Images/',
  output_folder_selections = 'results/part6/DeploymentOutputV1_R1/KSWS/Selections/',
  output_folder_wav = 'results/part6/DeploymentOutputV1_R1/KSWS/Wavs/',
  #detect_pattern= c('_050','_060'),
  top_model_path = TopModelMulti,
  path_to_files = JahooSoundFiles,
  class_names = c('CrestedGibbon','GreyGibbon','Noise'),
  noise_category = 'Noise',
  single_class = TRUE,
  single_class_category='CrestedGibbon',
  save_wav = TRUE,
  threshold = .9
)

