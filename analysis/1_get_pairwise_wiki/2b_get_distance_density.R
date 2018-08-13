# Calculate word pairwise distance densisty for each langauge

library(tidyverse)
library(feather)

#### PARAMETERS ####
OUTPUT_PATH <- "language_CV_measures.csv"
LANGS <- c( "el", "gu", "hi", "ig", "id", "it", "ja", "kn", "ko", "ar", "bn", "bg", "zh", "nl",  "fr", "de",
          "ml", "mr", "ne", "pa", "pl", "pt", "ro", "ru", "es", "tl", "ta", "te", "th", "tr", "ur", "vi", "yo","fa", "en") 

#### FUNCTIONS ####
# calculates coefficient of varaiance and entropy measure
get_density_measure <- function(current_lang, outputfile){
  
  print(paste0("====== ", current_lang, " ======"))
  
  inpath <- paste0("/Volumes/wilbur_the_great/concreteness_distances_for_paper/pairwise_word_10x1000_", current_lang, "_wiki.feather")
  df <- read_feather(inpath)

   lang_values <- df %>%
    group_by(word1) %>%
    summarize(sd_word = sd(cos_dist),
              mean_word = mean(cos_dist),
              cv_word = sd_word/mean_word) %>%
    ungroup() %>%
    summarize(lang = current_lang,
              lang_cv = mean(cv_word),
              lang_mean = mean(mean_word),
              lang_sd = mean(sd_word),
              n_words = length(unique(df$word1)))
  
  write_csv(lang_values, outputfile, append = T)
}

#### DO THE THING ####
walk(LANGS, get_density_measure, OUTPUT_PATH)

cvs <- read_csv(OUTPUT_PATH)
mod <- lm(lang_cv ~ n_words, cvs)
mod2 <- lm(lang_mean ~ n_words, cvs)
cvs_new <- modelr::add_residuals(cvs, mod, var = "lang_cv_n_resid")
cvs_new_new <- modelr::add_residuals(cvs_new, mod2, "lang_mean_n_resid")
cvs_clean <- cvs_new_new %>% 
  mutate(lang_cv2 = lang_mean_n_resid/lang_sd)
  
  

write_csv(cvs_clean, "language_CV_measures2.csv")