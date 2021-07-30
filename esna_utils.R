library(dplyr)
library(stringr)
library(textclean)
library(translit)

# replaceTranslit = function() {
#   
#   context <- rstudioapi::getActiveDocumentContext()
#   
#   for (sel in context$selection) {
#     
#     txt  <- sel$text
#     
#     newtxt <- translit(txt)
#     
#     rstudioapi::modifyRange(sel$range, as.character(newtxt), context$id)
#     
#   }
# }

##  remaining files:
list.files(pattern = "Esna_") %>% substr(1,3) %>% as.integer() %>% setdiff(194:398, .) %>% length
list.files(pattern = "Esna_") %>% substr(1,3) %>% as.integer() %>% setdiff(194:398, .) %>% paste0(collapse = "; ")

bookdown::render_book(input = ".")
bookdown::publish_book(name = "Esna3", account = "shemanefer", server = "bookdown.org")
rmarkdown::find_pandoc(dir = "c://users/klotzd/AppData/local/pandoc/", version = "2.13.0")

fix_translit <- function(x) {
  
  textclean::mgsub_fixed(x, pattern = c("a", "A", "i","I", "H","@", "x","#", "X","S", "$","%", "D", "T", "_", "+"), 
                                       c("ʿ","ȝ", "ỉ","Ỉ", "ḥ", "Ḥ","ḫ","Ḫ", "ẖ","š","S","Š", "ḏ","ṯ","Ṯ", "Ḏ"))
  
  

}

  tmp_theme <- function() bookdown::bs4_book(css = "style.css", theme = bookdown::bs4_book_theme(font_scale = .5, 
                                                                              primary = "#9B110E"))
  translate <- function(x){
   
      stringr::str_split(x, "\\n") %>% 
      unlist %>% 
      #paste0("| ", .) %>% 
      paste(collapse = "  \n") %>% 
      cat
      
  }
  translit <- function(x){
    
    fix_translit(x) %>% 
      stringr::str_split(., "\\n") %>% 
      unlist %>% 
      paste0("*", ., "*  ") %>% 
      stringi::stri_replace_all_fixed("*  ", "  *") %>% 
      stringi::stri_replace_all_fixed("*  ", "  *") %>% 
      stringi::stri_replace_all_fixed("*  ", "  *") %>% 
      stringi::stri_replace_all_fixed("*  ", "  *") %>% 
      str_remove_all("  ") %>% 
      paste(collapse = "  \n") %>% 
      stringi::stri_replace_all_fixed("**", " ") %>% 
      stringi::stri_replace_all_fixed("*^", "^") %>% 
      stringi::stri_replace_all_fixed("^ ", "^ *") %>% 
      stringi::stri_replace_all_fixed(" ^", "* ^")
    
    
  }

  