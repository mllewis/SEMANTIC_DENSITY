# get words from Pete's density model so can do pairwise word comaprions
# downloads models from google drive

library(tidyverse)
library(reticulate)
library(googledrive)

PATH_TO_MODELS <- "/Volumes/wilbur_the_great/density_models/"
OUTPATH <- "/Volumes/wilbur_the_great/density_models/density_model_words"

# reticulate stuff
use_python("/usr/local/bin/python") 
gensim <- import("gensim")

# get file prefixes
bible_files <- drive_find(pattern = "bible_")
europarl_files <- drive_find(pattern = "europarl_")
all_files <- bind_rows(bible_files, europarl_files )
file_prefixes <- unique(map(str_split(all_files$name, ".txt"), ~.[[1]]))

# function for downloading file (overwrites exsiting file), then writing words to file
get_words_from_model <- function(file_prefix, model_path, out_path){
  print(file_prefix)
  file1 <- paste0(file_prefix, ".txt.model")
  file1_outpath = paste0(model_path, "current_model")
  drive_download(file1, overwrite = T, path = file1_outpath)
  
  try( {
    file2_outpath <- paste0(model_path,"current_model.trainables.syn1neg.npy")
    file2 <- paste0(file_prefix, ".txt.model.trainables.syn1neg.npy")
    drive_download(file2, overwrite = T, path = file2_outpath )
    
    file3_outpath <- paste0(model_path,"current_model.wv.vectors.npy")
    file3 <- paste0(file_prefix, ".txt.model.wv.vectors.npy")
    drive_download(file3, overwrite = T, path = file3_outpath)
  })
  
  this_model <- gensim$models$Word2Vec$load(file1_outpath)
  words <- this_model$wv$index2word
  these_words <- data.frame(file = file_prefix, 
                            words = words)
  
  #write.csv(these_words, out_path, append = T)
  
  write_csv(these_words, paste0(out_path, "_", file_prefix, "csv"))
  
  
  file.remove(file1_outpath)
  
  try({
    file.remove(file2_outpath)
    file.remove(file3_outpath)
  
  })
}

bible_prefixes <- read_csv("file_prefixes.csv") %>%
  select(prefixes, bible_wiki_lang_code) %>%
  filter(!is.na(bible_wiki_lang_code)) %>%
  pull(prefixes)

#europarl_prefixes <-unlist(file_prefixes[440:479])[!grepl("english", unlist(file_prefixes[440:479]))]

walk(bible_prefixes, get_words_from_model, PATH_TO_MODELS, OUTPATH)

#prefix_df <- data_frame(prefixes = unlist(file_prefixes))

#write_csv(prefix_df, "file_prefixes.csv")



