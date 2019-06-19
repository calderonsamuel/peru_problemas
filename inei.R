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

names(datos) <- as.character(c(2004:2018))
# datos <- map(tablas, tidy_tabla)
# sum(map_int(tablas, ~ dim(.)[1]))

datos1 <- datos[[1]]

#esto debe convertirse en una función
datos1 %>%
      mutate(month = as.integer(month)) %>%
      filter(month > 6) %>%
      group_by(month, problema)%>%
      summarise(perc = mean(valor)) %>%
      arrange(month, desc(perc)) %>%
      ungroup() %>%
      group_by(month) %>%
      mutate(r = dense_rank(desc(perc)))%>%
      filter(r <= 5)

