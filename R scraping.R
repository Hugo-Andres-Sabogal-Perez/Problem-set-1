rm(list = ls())

require(pacman) 

p_load(tidyverse, # contiene las librer?as ggplot, dplyr...
       rvest) # web-scraping


my_link = "https://ignaciomsarmiento.github.io/GEIH2018_sample/"

my_html = read_html(my_link) 



div<- my_html %>%  html_elements(".col-md-9")  %>%
                    html_elements("ul") %>%  html_elements("a")


