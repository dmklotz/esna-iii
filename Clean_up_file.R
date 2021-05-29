z <- list.files(pattern = "Esna_[0-9]+.Rmd")


library(tidyverse)
library(tidytext)

for(files in z){
  
  tmp_text <- read_file(files)
  
  if(stringi::stri_detect_fixed(tmp_text,"|")){
    
    out_text <- tmp_text %>% 
      stringi::stri_replace_all_fixed("| ", "") %>%
      stringi::stri_replace_all_fixed("\r", "  \r") 
    
    write_file(out_text, files, append = F)
    
    }
  
  
}

for(files in z){
  
  tmp_text <- read_file(files)
  
  if(stringi::stri_detect_fixed(tmp_text,"|")){
    
    out_text <- tmp_text %>% 
      stringi::stri_replace_all_fixed("|", "") 
    
    write_file(out_text, files, append = F)
    
  }
  
  
}

read_file("073-Esna_073.Rmd") %>% 
  str_split("\r\n") %>% 
  unlist %>% 
  trimws %>% 
  enframe %>% 
  filter(substr(value,1,1) %in% c("*", "^")) %>% 
  filter(str_sub(value, -1, -1) %in% c("*") |
           str_sub(value, -2, -1) %in% c(".]")) %>% 
  filter(stringi::stri_count_fixed(value, "*") >= 2) %>% 
  pull(value) %>% paste0(., collapse = " ") %>% 
  stringi::stri_replace_all_fixed("*", "")  %>% 
  stringi::stri_replace_all_fixed("[...]", "") %>% 
  str_split(pattern = "\\^(?=[0-9])")  %>% unlist %>% 
  enframe %>% 
  filter(nchar(value) > 1) %>% select(-name) %>% 
  separate(value, into = c("row", "text"), sep = "\\^") %>% 
  mutate(text = str_remove_all(text, "\\((.)*\\)\\|")) %>% 
  mutate(text = str_remove_all(text, "[") %>% str_remove_all("]")) %>% 
  unnest_tokens(input = text, output = "word", token = "ptb") %>% 
  filter(nchar(word) > 1) %>% 
  filter(!word %in% c("sn", "tn", "pn", "pw", ".t", "tw")) %>% 
  filter(!str_detect(word, "=")) %>% 
  mutate(first = substr(word, 1,1), second = substr(word, 2,2),
         third = substr(word, 3,3)) %>% 
  filter(first != ".") %>% 
  mutate_at(vars(first, second, third), ~factor(., levels = trans_levels)) %>% 
  arrange(first, second, third)
  count(first)

trans_levels = c("ȝ", "ỉ", "ʿ", "w", "b", "p", "f", "m", "n", "r", "h", "ḥ", "ḫ","ẖ","z","s","š", "q", "k","g", "t", "ṯ", "d","ḏ", ".t", ".w", "-" )


out_text <- read_file(z) %>% 
  stringi::stri_replace_all_fixed("| ", "") %>%
  stringi::stri_replace_all_fixed("\r", "  \r") 


write_file(out_text, "046-Esna_046b.Rmd")
out_text <- str_remove_all(text, "|")

final_tbl <- tibble()

for(files in z){
  
  tmp_text <- read_file(files)
  tmp_ind <- substr(files, 1,3) %>% as.integer()
  
  tmp_out <- tmp_text %>% 
    str_split("\r\n") %>% 
    unlist %>% 
    trimws %>% 
    enframe %>% 
    filter(substr(value,1,1) %in% c("*", "^")) %>% 
    filter(str_sub(value, -1, -1) %in% c("*") |
             str_sub(value, -2, -1) %in% c(".]")) %>% 
    filter(stringi::stri_count_fixed(value, "*") >= 2) 
  
  if(nrow(tmp_out) > 0){
  
    tmp_out <- tmp_out %>% 
    pull(value) %>% paste0(., collapse = " ") %>% 
    stringi::stri_replace_all_fixed("*", "")  %>% 
    stringi::stri_replace_all_fixed("[...]", "") %>% 
    str_split(pattern = "\\^(?=[0-9AB])")  %>% unlist %>% 
    enframe %>% 
    filter(nchar(value) > 1) %>% select(-name) %>% 
    separate(value, into = c("row", "text"), sep = "\\^") %>% 
    mutate(text = str_remove_all(text, "\\((.)*\\)\\|")) %>% 
    mutate(text = str_remove_all(text, "=sn")) %>%
    mutate(text = str_remove_all(text, "=tn")) %>%
    mutate(text = str_remove_all(text, "=ṯn")) %>%
    mutate(text = str_remove_all(text, "=[fstṯnỉj]")) %>%
    mutate(text = stringi::stri_replace_all_fixed(text, ".tw","")) %>% 
    mutate(text = stringi::stri_replace_all_fixed(text, ".n","")) %>% 
    mutate(text = stringi::stri_replace_all_fixed(text, "[","") %>% stringi::stri_replace_all_fixed("]","")) %>% 
    unnest_tokens(input = text, output = "word", token = "ptb") %>% 
    filter(nchar(word) > 1) %>% 
    filter(!word %in% c("sn", "tn", "pn", "pw", ".t", "tw", "ṯn")) %>% 
    filter(!str_detect(word, "=")) %>% 
    mutate(first = substr(word, 1,1), second = substr(word, 2,2),
           third = substr(word, 3,3)) %>% 
    filter(first != ".") %>% 
    mutate(text = tmp_ind)
  
  final_tbl <- 
    bind_rows(final_tbl, tmp_out)
  }
}

### Naive glossary

final_tbl %>% mutate_at(vars(word, first, second, third), ~str_replace_all(., "i", "ỉ") %>% str_replace_all("j", "ỉ")) %>% 
     
     mutate_at(vars(first, second, third), 
                           ~factor(., levels = trans_levels)) %>% 
     arrange(first, second, third, word, text, row) %>% mutate(row = as.integer(row)) %>% filter(!is.na(row)) %>% 
  mutate(out_text = paste0("[Esna ", text, "], ", row)) %>% 
  rowid_to_column("word_order") %>% 
  group_by(word) %>% 
  summarize(out_text = paste0(out_text, collapse = "; "), word_order = min(word_order)) %>% 
  ungroup %>% 
  arrange(word_order) %>% 
  mutate(word = paste0("*", word, "*")) %>% 
  unite(final_text, c(word, out_text), sep = "\t\t") %>% 
  pull(final_text) %>% 
  paste(collapse = "   \n   \n") %>% 
  write_file("800-Naive-Dictionary.Rmd")
  
  
  
     write.xlsx("dictionary2.xlsx")


  if(stringi::stri_detect_fixed(tmp_text,"|")){
    
    out_text <- tmp_text %>% 
      stringi::stri_replace_all_fixed("| ", "") %>%
      stringi::stri_replace_all_fixed("\r", "  \r") 
    
    write_file(out_text, files, append = F)
    
  }
  
  
}

