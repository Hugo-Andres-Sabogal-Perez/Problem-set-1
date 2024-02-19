
rm(list = ls())


require(pacman)
require(tidyverse)
require(rvest)



# 1. Importar la base de datos:
html1 = read_html('https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_1.html')
html2 = read_html('https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_2.html')
html3 = read_html('https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_3.html')
html4 = read_html('https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_4.html')
html5 = read_html('https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_5.html')
html6 = read_html('https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_6.html')
html7 = read_html('https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_7.html')
html8 = read_html('https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_8.html')
html9 = read_html('https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_9.html')
html10 = read_html('https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_10.html')

C1 = html1 %>% html_table()
C2 = html2 %>% html_table()
C3 = html3 %>% html_table()
C4 = html4 %>% html_table()
C5 = html5 %>% html_table()
C6 = html6 %>% html_table()
C7 = html7 %>% html_table()
C8 = html8 %>% html_table()
C9 = html9 %>% html_table()
C10 = html10 %>% html_table()

Df1 = as.data.frame(C1[1])
Df2 = as.data.frame(C2[1])
Df3 = as.data.frame(C3[1])
Df4 = as.data.frame(C4[1])
Df5 = as.data.frame(C5[1])
Df6 = as.data.frame(C6[1])
Df7 = as.data.frame(C7[1])
Df8 = as.data.frame(C8[1])
Df9 = as.data.frame(C9[1])
Df10 = as.data.frame(C10[1])

# Juntar bases de datos:
DF = rbind(Df1, Df2, Df3, Df4, Df5, Df6, Df7, Df8, Df9, Df10)

# Drop variables inecasarias:
rm(html1, html2, html3, html4, html5, html6, html7, html8, html9, html10, C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, 
   Df1, Df2, Df3, Df4, Df5, Df6, Df7, Df8, Df9, Df10)

# Seleccion de la muetra de interes: edad >= 18 y empleado (dsi).
DF = DF[DF$age >= 18 & DF$dsi != 1,]
DF = DF[,-1]

# 2. Estadisticas descriptivas:
#salario real o nominal?
tabla= DF %>% select(age,clase,depto,formal,maxEducLevel,orden,p6426,p7040,sex,sizeFirm,y_ingLab_m_ha)
stargazer(tabla, type= "text", summary=T, title = "Estadisticas Descriptivas",out = "Views/esta_des.txt")


