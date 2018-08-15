# Get pairwise word distances for words in a language, given a language model
# takes in one model per language and outputs a single csv for all the pairwise 
# word distances for each langauge

library(tidyverse)
library(data.table)

#### PARAMETERS ####
PREFIX_PATH <- "file_prefixes.csv"
OUTPUT_PATH <- "/Volumes/wilbur_the_great/density_models/europarl_dists/" # path to csv for writing distances
MODEL_PATH <- "/Volumes/wilbur_the_great/fasttext_models/wiki."
NSAMPLES <- 5000

#### FUNCTIONS ###
get_distances_for_crit_words <- function(language, model_path, out_path, nsamples, critical_words_df){
  print(language)
  full_model_path <- paste0(model_path, language, ".vec")
  
  model <- fread( # this reads in the wiki fasttext models....you'll have to change this depending on how your models are saved
    full_model_path,
    header = FALSE,
    skip = 1,
    quote = "",
    encoding = "UTF-8",
    data.table = TRUE,
    col.names = c("target_word", 
                  unlist(lapply(2:301, function(x) paste0("V", x)))))
  
  these_crit_words <- critical_words_df %>%
    filter(wiki_lang_code == language)
  
  model_filtered <- model[target_word %in% these_crit_words$words] 
  if (dim(model_filtered)[1] > nsamples) {
    model_filtered  <- sample_n(model_filtered, nsamples)
  }
 
  word_word_dists <- coop::cosine(t(model_filtered[,-1]))  %>%
    as_data_frame() %>%
    mutate(language = language)
  
  write_csv(word_word_dists, paste0(out_path, "5000_words_", language, ".csv"))
}

### DO THE THING  ####
target_langs  <- read_csv(PREFIX_PATH) %>%
  filter(!is.na(wiki_lang_code))  %>%
  filter(wiki_lang_code != "en")

critical_words <- map_df(target_langs$prefixes, ~ read_csv(paste0("/Volumes/wilbur_the_great/density_models/europarl_words/density_model_words_", ., "csv"))) %>%
  left_join(target_langs, by = c("file" = "prefixes")) %>%
  select(-file) 

wiki_lang_codes <- pull(target_langs, wiki_lang_code)

# loops over languages and saves pairiwse word distances to a single csv
walk(wiki_lang_codes, 
     get_distances_for_crit_words, 
     MODEL_PATH, 
     OUTPUT_PATH,
     NSAMPLES,
     critical_words)
