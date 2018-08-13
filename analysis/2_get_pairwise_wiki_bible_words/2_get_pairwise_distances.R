# Get pairwise word distances for words in a language, given a language model
# takes in one model per language and outputs a single csv for all the pairwise 
# word distances for each langauge

library(tidyverse)
library(data.table)

#### PARAMETERS ####
PREFIX_PATH <- "file_prefixes.csv"
OUTPUT_PATH <- "/Volumes/wilbur_the_great/density_models/bible_dists/" # path to csv for writing distances
MODEL_PATH <- "/Volumes/wilbur_the_great/fasttext_models/wiki."
NSAMPLES <- 1000

#### FUNCTIONS ###
get_pairwise_dist_beween_words <- function(d){
  model_matrix <- d %>%
    select(-1) %>%
    as.matrix()
  
  words <- d$target_word
  word_word_dists <- philentropy::distance(model_matrix, 
                                           method = "cosine") %>%
    as.data.frame()  %>%
    mutate(w1 = words)
  
  colnames(word_word_dists) = c(words, "w1") 
  
  long_word_word_dists <- gather(word_word_dists, "w2", "cos_dist", -w1)
  
  # this is a data.table way of getting unique pairs that seems fast
  long_word_word_dists_f <-  unique(as.data.table(long_word_word_dists)[, c("w1", "w2") := list(pmin(w1, w2),pmax(w1, w2))], by = c("w1", "w2")) %>%
    filter(w1 != w2)
  
  long_word_word_dists_f
}

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
    filter(bible_wiki_lang_code == language)
  
  model_filtered <- model[target_word %in% these_crit_words$words]  %>%
    sample_n(nsamples)
  
  dists <- get_pairwise_dist_beween_words(model_filtered) %>%
    mutate(language = language)
  
  write_csv(dists, paste0(out_path, "_", language, ".csv"))
}

### DO THE THING  ####
target_langs  <- read_csv(PREFIX_PATH) %>%
  filter(!is.na(bible_wiki_lang_code)) 

critical_words <- map_df(target_langs$prefixes, ~ read_csv(paste0("/Volumes/wilbur_the_great/density_models/bible_words/density_model_words_", ., "csv"))) %>%
  left_join(target_langs, by = c("file" = "prefixes")) %>%
  select(-file) 

wiki_lang_codes <- pull(target_langs, bible_wiki_lang_code)

# loops over languages and saves pairiwse word distances to a single csv
walk(wiki_lang_codes[32:34], 
     get_distances_for_crit_words, 
     MODEL_PATH, 
     OUTPUT_PATH,
     NSAMPLES,
     critical_words)
