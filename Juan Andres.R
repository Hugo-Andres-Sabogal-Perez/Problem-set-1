require(pacman)
require(tidyverse)
require(rvest)

#Importar la base de datos:
my_url = "https://ignaciomsarmiento.github.io/GEIH2018_sample/"
browseURL(my_url)
my_html = read_html(my_url)
