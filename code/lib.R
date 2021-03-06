theme_report <- function(base_size = 11,
                         strip_text_size = 12,
                         strip_text_margin = 5,
                         subtitle_size = 13,
                         subtitle_margin = 10,
                         plot_title_size = 16,
                         plot_title_margin = 10,
                         ...) {
    ret <- ggplot2::theme_minimal(base_family = "Roboto-Regular",
                                  base_size = base_size, ...)
    ret$strip.text <- ggplot2::element_text(hjust = 0, size=strip_text_size,
                                            margin=margin(b=strip_text_margin),
                                            family="Roboto-Bold")
    ret$plot.subtitle <- ggplot2::element_text(hjust = 0, size=subtitle_size,
                                               margin=margin(b=subtitle_margin),
                                               family="PT Sans")
    ret$plot.title <- ggplot2::element_text(hjust = 0, size = plot_title_size,
                                             margin=margin(b=plot_title_margin),
                                            family="Oswald")
    ret
}

import_data <- function(){
    library(tidyverse)
    importados = readr::read_delim(here::here("data/dados-74.tsv"), delim = " ", 
                      col_types = cols(.default = col_double(), 
                                       Situation = col_character())) 
    
    importados %>% 
        readr::write_csv(here::here("data/dados-74.csv"))
    
    importados %>% 
        nest(-Situation) %>% 
        jsonlite::write_json(here::here("data/dados-74.json"))
    
    importados %>% 
        gather(key = "Acao", value = "Apropriado", -Situation) %>% 
        readr::write_csv(here::here("data/dados-74-long.csv"))
}

read_projectdata <- function(){
    readr::read_csv(here::here("data/dados-74.csv"), 
                      col_types = cols(.default = col_double(), 
                                       Situation = col_character()))
}
