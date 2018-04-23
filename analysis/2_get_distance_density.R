# Calculate word pairwise distance densisty for each langauge

library(tidyverse)
library(entropy)

#### PARAMETERS ####
INPUT_PATH <- "word_dists_by_language.csv" 
OUTPUT_PATH <- "language_density_measures.csv"
NWORDS <- 50

dists <- read_csv(INPUT_PATH,
                  col_names = c("w1", "w2", "cos_dist", "lang"))

#### FUNCTIONS ####
# calculates coefficient of varaiance and entropy measure
get_density_measure <- function(x, n_words_to_sample){

  # group by each word, sample n words to calcualte pairwise distances for that word
  # sample n of those words
  mean_sd_word <- x %>% 
    nest(-w1) %>% 
    sample_n(n_words_to_sample, replace = T) %>%
    unnest() %>%
    group_by(w1) %>%
    sample_n(n_words_to_sample, replace = T)
  
  mean_sd_word %>%
    group_by(w1) %>%
    summarize(sd_word = sd(cos_dist),
              mean_word = mean(cos_dist),
              cv_word = sd_word/mean_word,
              H_word =  entropy(discretize(cos_dist, 1))) %>%
   ungroup() %>%
   summarize(cv = mean(cv_word),
             H = mean(H_word)) 
}

#### DO THE THING ####
densities <- dists %>%
  group_by(lang) %>%
  do(cv = get_sd(., NWORDS)) %>%
  unnest()

write_csv(densities, OUTPUT_PATH)
