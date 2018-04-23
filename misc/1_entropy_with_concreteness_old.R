# Get pairwise word distances for words in a language, given a language model
# takes in one model per language and outputs a single csv for all the pairwise 
# word distances for each langauge

# Look at entropy across word distances using existing pairwise concreteness

library(tidyverse)
library(data.table)
library(feather)

#### PARAMETERS ####

INPUT_PATH <- "/Users/mollylewis/Documents/research/Projects/VOCAB_SEEDS/analyses/1_exploration/wiki." # path to the embedding models
OUTPUT_PATH <- "word_dists_by_language.csv" # path to csv for writiing distances
GOOGLE_LANGS <- c("en", "en") # list of languages
CRITICAL_WORDS <- c("apple", "dog", "banana") # list of words comparing across languages 

## functions for getting getting pairiwse words distances
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

get_distances_for_crit_words <- function(language, model_path, out_path, critical_words){
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
  
  model_filtered <- model[target_word %in% critical_words]
  
  dists <- get_pairwise_dist_beween_words(model_filtered) %>%
    mutate(language = language)
  
  write_csv(dists, out_path, append = T)
}

### DO THE THING
# loops over languages and saves pairiwse word distances to a single csv
walk(GOOGLE_LANGS, 
     get_distances_for_crit_words, 
     INPUT_PATH, 
     OUTPUT_PATH,
     CRITICAL_WORDS)

files <- list.files("/Users/mollylewis/Documents/research/Projects/SEMANTIC_DENSITY/data/quintile_anchored/", 
                    full.names = T)[1:34]

get_sd <- function(x){
  language <- pluck(str_split(x , "_"), 1, 6)
  print(language)
  
  dists <- read_feather(x) %>%
    mutate(lang = language) 
  
  # mean_sd <- dists %>%
  #   group_by(w1) %>%
  #   summarize(sd_dist = sd(cos_dist),
  #             mean_dist = mean(cos_dist)) %>%
  #   ungroup() %>%
  #   summarize(language = language, 
  #             n = n(), 
  #             overall_sd_dist = mean(sd_dist),
  #             overall_mean_dist = mean(mean_dist),
  #             cv = overall_sd_dist/overall_mean_dist)
  
  mean_sd_word <- dists %>% 
    nest(-w1) %>% 
    sample_n(500, replace = T) %>%
    unnest() %>%
    group_by(w1) %>%
    sample_n(500, replace = T)
  
  mean_sd_word %>%
    group_by(w1) %>%
    summarize(sd_word = sd(cos_dist),
              mean_word = mean(cos_dist),
              cv_word = sd_word/mean_word) %>%
   ungroup() %>%
   summarize(language = language, 
             cv = mean(cv_word))
    

  #mean_sd <- dists %>%
    #sample_n(50000, replace = T) %>%
    #summarize(language = language, 
    #          n = n(), 
    #          overall_sd_dist = sd(cos_dist),
    #          overall_mean_dist = mean(cos_dist)) %>%
    #mutate(cv = overall_sd_dist/overall_mean_dist)

  #mean_sd
}

#KL_divergence or MI
sds <- map_df(files, get_sd)

write_csv(sds, "semantic_density_cv2.csv")

sds1 <- read_csv("semantic_density_cv.csv")

m = left_join(sds1, sds, by = "language") 

cor.test(m$cv.x, m$cv.y)

ggplot(m, aes(cv.x, cv.y))  +
  geom_text(aes(label = language)) +
  geom_smooth(method = "lm")

get_entropy <- function(x){
  language <- pluck(str_split(x , "_"), 1, 6)
  
  print(language)
  
  dists <- read_feather(x) %>%
    mutate(lang = language) 
 
   word_samples <- dists %>% 
    nest(-w1) %>% 
    sample_n(500, replace = T) %>%
    unnest() %>%
    group_by(w1) %>%
    sample_n(500, replace = T)
  
  word_samples %>%
    group_by(w1) %>%
    summarize(H_word = entropy(discretize(cos_dist, 1000))) %>%
    ungroup() %>%
    summarize(language = language, 
              H = mean(H_word))
}

entropies <- map_df(files, get_entropy)


write_csv(entropies, "semantic_density_entropy.csv")



m = left_join(entropies, sds, by = "language") 

cor.test(m$cv, m$H)

ggplot(m, aes(cv, H))  +
  geom_text(aes(label = language)) +
  geom_smooth(method = "lm")
