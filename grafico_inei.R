library(tidyverse)
library(gganimate)
library(paletteer)

data <- read.csv("datalista.csv", stringsAsFactors = FALSE) %>%
      mutate(str_perc = paste(str_perc, "%")) %>%
      # filter(year < 2008) %>%
      select(-(year:month))


# se crea el gráfico de barras
## las posiciones de las barras representan el ranking, su tamaño el porcentaje y su color el problema
p <- data %>%
      ggplot(aes(
            x = r,
            y = perc,
            color = factor(problema),
            fill = factor(problema)
      )) +
      geom_bar(stat = "identity") +
      scale_y_continuous(limits = c(0, 120)) +
      # etiqueta de problema
      geom_text(aes(y = 120, label = problema), hjust = 1, size = 7) +
      #etiqueta de porcentaje
      geom_text(aes(y = perc + 1, label = str_perc),
                size = 6,
                hjust = 0) +
      #se transponen los ejes y se invierte la escala de x para tener la barra mas grande arriba
      coord_flip(clip = "off") +
      scale_x_reverse() +
      #elementos estéticos. se quita leyenda y se cambia el color del plano
      theme(
            legend.position = "none",
            axis.text.y = element_blank(),
            text = element_text(size = 18),
            panel.background = element_rect(
                  fill = "black",
                  colour = "grey20",
                  size = 2,
                  linetype = "solid"
            ),
            panel.grid = element_line(
                  size = 0.5,
                  linetype = 'solid',
                  colour = "black"
            )
      ) +
      #Paleta de color para las barras
      scale_fill_paletteer_d(package = "dutchmasters", palette = "milkmaid") +
      scale_color_paletteer_d(package = "dutchmasters", palette = "milkmaid")

# Se anima el gráfico para que avance según el tiempo
anp <- p +
      transition_states(as_factor(fecha), transition_length = 1, state_length = 1) +
      labs(
            subtitle = "Mes: {closest_state}",
            title = "En su opinión ,¿Cuáles son los principales problemas del Perú?",
            x = "Problema",
            y = "Porcentaje"
      ) +
      enter_fade() +
      exit_fade()+
      ease_aes('cubic-in-out')

# se guarda la animación como objeto, se puede ajustar su tamaño, fps y duración
anim <-
      animate(
            plot = anp,
            fps = 20,
            duration = 162,
            height = 500,
            width = 750
      )
anim
anim_save("gif2.gif")

