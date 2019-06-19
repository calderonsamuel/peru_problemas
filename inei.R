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

for (i in seq_along(tablas)){
      colnames(tablas[[i]]) <- var_label(tablas[[i]], T)
}
rm(i)

tidy_tabla <- function(df){
      
      dataframe <- df %>%
            gather(key = "problema", value = "valor", -c(1:8)) %>%
            filter(!is.na(valor)) %>%
            mutate(valor = if_else(valor > 0, 1, 0))
      
      names(dataframe) <- c("year", "month", "conglome", "vivienda", "hogar", 
                     "dominio", "estrato", "factor", "problema", "valor")
      dataframe
}

datos <- list()
for (i in seq_along(tablas)){
      datos[[i]] <- tidy_tabla(tablas[[i]])
}
rm(tablas, i)

names(datos) <- as.character(c(2004:2018))
# datos <- map(tablas, tidy_tabla)

#sumaria otorga el ranking de los 5 principales problemas
sumaria <- function(df) {
      df %>%
            mutate(month = as.integer(month)) %>%
            #filter(month > 6) %>%
            group_by(year, month, problema) %>%
            summarise(perc = mean(valor)) %>%
            arrange(month, desc(perc)) %>%
            ungroup() %>%
            group_by(year, month) %>%
            mutate(perc = round(perc * 100, 1), r = row_number(desc(perc))) %>%
            filter(r <= 5)
}

datos2 <- list()
for(i in seq_along(datos)){
      datos2[[i]] <- sumaria(datos[[i]])
}
rm(i)

#Puede generar problemas porque el locale se define en linux
problemas <- datos2 %>%
      tibble() %>%
      unnest() %>%
      filter(!(year == 2004 & month < 7)) %>%
      mutate(month = month(month, label = T
                           #, locale = "es_PE.utf8"
                           ),
             problema = tolower(problema))

parte1 <- problemas %>%
      filter(year < 2018)%>%
      mutate(problema = str_extract(problema, "-+[[:alpha:]\\s]+"),
             problema = str_sub(problema, 2),
             problema = str_trim(problema))

parte2 <- problemas %>%
      filter(year == 2018) %>% 
      mutate(problema = str_extract(problema, "\\?.*"),
             problema = str_sub(problema, 2),
             problema = str_trim(problema))

data <- bind_rows(parte1, parte2) %>%
      mutate(problema = str_remove(problema, "^la "),
             problema = str_to_sentence(problema))

rm(datos, datos2, parte1, parte2, problemas)      

# 
# data <- data %>%
#       mutate(problema = str_remove(problema, "^la "),
#              problema = str_to_sentence(problema))

# unique(data$problema) %>%
#       str_remove("^la ")%>% 
#       unique() %>%
#       str_to_sentence()

##Este será el grafico
data %>%
      filter(year == 2016) %>%
      ggplot(aes(x= factor(problema), y = desc(perc), fill = factor(problema)))+
      geom_bar(stat = "identity") +
      facet_wrap(~month)+
      coord_flip()+
      theme(legend.position = "none")
