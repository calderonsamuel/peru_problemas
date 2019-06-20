library(tidyverse)
library(haven)
library(lubridate)
library(labelled)

files <- paste0("tablas/", dir("tablas"))

tabular <- function(file) {
      read_spss(file) %>%
            select(
                  AÑO,
                  MES,
                  CONGLOME,
                  VIVIENDA,
                  HOGAR,
                  DOMINIO,
                  ESTRATO,
                  starts_with("FA"),
                  #starts_with("P207"), starts_with("P208"),
                  #starts_with("P209"), starts_with("P301"),
                  matches("^P3_.*1$"),
                  matches("^P2_1\\$")
            )
}

tablas <- map(files, tabular)
names(tablas) <- as.character(c(2004:2018))

#no sirvió hacerlo con map
#tablas2 <- map(tablas, function(x) colnames(x) <- var_label(x, T))

for (i in seq_along(tablas)) {
      colnames(tablas[[i]]) <- var_label(tablas[[i]], T)
}
rm(i)

tidy_tabla <- function(df) {
      dataframe <- df %>%
            gather(key = "problema", value = "valor",-c(1:8)) %>%
            filter(!is.na(valor)) %>%
            mutate(valor = if_else(valor > 0, 1, 0))
      
      names(dataframe) <-
            c(
                  "year",
                  "month",
                  "conglome",
                  "vivienda",
                  "hogar",
                  "dominio",
                  "estrato",
                  "factor",
                  "problema",
                  "valor"
            )
      dataframe
}

datos <- list()
for (i in seq_along(tablas)) {
      datos[[i]] <- tidy_tabla(tablas[[i]])
}
rm(tablas, i)

names(datos) <- as.character(c(2004:2018))
# datos <- map(tablas, tidy_tabla)

#sumaria otorga el ranking de los 5 principales problemas
sumaria_simple <- function(df) {
      df %>%
            mutate(month = as.integer(month)) %>%
            #filter(month > 6) %>%
            group_by(year, month, problema) %>%
            summarise(perc = mean(valor)) %>%
            arrange(month, desc(perc)) %>%
            ungroup() %>%
            group_by(year, month) %>%
            mutate(perc = round(perc * 100, 1), rank = row_number(desc(perc))) %>%
            filter(rank <= 5)
}

#sumaria otorga el ranking de los 5 principales problemas, esta pondera segun factor de expansion
sumaria_ponderada <- function(df) {
      df %>%
            mutate(month = as.integer(month)) %>%
            #filter(month > 6) %>%
            group_by(year, month, problema) %>%
            summarise(perc = sum(valor * factor, na.rm = T) / sum(factor, na.rm = T)) %>%
            arrange(month, desc(perc)) %>%
            ungroup() %>%
            group_by(year, month) %>%
            mutate(perc = round(perc * 100, 1), rank = row_number(desc(perc))) %>%
            filter(rank <= 5)
}

#deja listo para el gráfico
limpiar <- function(list) {
      #Puede generar problemas porque el locale se define en linux
      problemas <- list %>%
            tibble() %>%
            unnest() %>%
            filter(!(year == 2004 & month < 7)) %>%
            mutate(month = month(month, 
                                 #locale = "es_PE.utf8",
                                 label = T),
                                 problema = tolower(problema))
                   
                   parte1 <- problemas %>%
                         filter(year < 2018) %>%
                         mutate(
                               problema = str_extract(problema, "-+[[:alpha:]\\s]+"),
                               problema = str_sub(problema, 2),
                               problema = str_trim(problema)
                         )
                   
                   parte2 <- problemas %>%
                         filter(year == 2018) %>%
                         mutate(
                               problema = str_extract(problema, "\\?.*"),
                               problema = str_sub(problema, 2),
                               problema = str_trim(problema)
                         )
                   
                   data <- bind_rows(parte1, parte2) %>%
                         mutate(
                               problema = str_remove(problema, "^la "),
                               problema = str_to_sentence(problema),
                               fecha = paste0(as.character(month), " ", year),
                               fecha = as_factor(fecha),
                               str_perc = paste(as.character(perc), "%")
                         ) %>%
                         select(-(year:month))
                   data
}

datos_simple <- list()
for (i in seq_along(datos)) {
      datos_simple[[i]] <- sumaria_simple(datos[[i]])
}

data_simple <- limpiar(datos_simple)
write_csv(data_simple, "data_simple.csv")
rm(i, data_simple, datos_simple)


datos_ponderada <- list()
for (i in seq_along(datos)) {
      datos_ponderada[[i]] <- sumaria_ponderada(datos[[i]])
}


data_ponderada <- limpiar(datos_ponderada)
write_csv(data_ponderada, "data_ponderada.csv")

rm(i, data_ponderada, datos, datos_ponderada)    
