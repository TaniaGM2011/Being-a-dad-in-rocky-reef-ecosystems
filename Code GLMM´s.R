library(patchwork)
library(ggplot2)
library(tidyverse)     ## 
library(colorspace)    ## 
library(rcartocolor)   ## 
library(ggforce)       ## 
library(ggdist)        ## 
library(ggridges)      ## 
library(ggbeeswarm)    ## 
library(gghalves)      ## 
library(ggpubr)
library(systemfonts)   ## 
library(effects)
library(jtools)
library(sjPlot)
library(dotwhisker)
library(dplyr)
library(performance)
library(lme4)
library(lsmeans)
library(DHARMa)
library(ggeffects)
library(car)
library(sjmisc)
library(merTools)
library(glmmTMB)
library(MASS)
library(RColorBrewer)
library(lmerTest)
library(stargazer)
library(broom)
library(modelsummary)
library (arm)
library (rstanarm)
library(crplots)
library(ggpredict)library(patchwork)
library(ggplot2)
library(tidyverse)     ## 
library(colorspace)    ## 
library(rcartocolor)   ## 
library(ggforce)       ## 
library(ggdist)        ## 
library(ggridges)      ## 
library(ggbeeswarm)    ## 
library(gghalves)      ## 
library(ggpubr)
library(systemfonts)   ## 
library(effects)
library(jtools)
library(sjPlot)
library(dotwhisker)
library(dplyr)
library(performance)
library(lme4)
library(lsmeans)
library(DHARMa)
library(ggeffects)
library(car)
library(sjmisc)
library(merTools)
library(glmmTMB)
library(MASS)
library(RColorBrewer)
library(lmerTest)
library(stargazer)
library(broom)
library(modelsummary)
library (arm)
library (rstanarm)
library(crplots)
library(ggpredict)

##########GLMM´s###################
Poma$especie<- as.factor(Poma$especie)#SI
Poma$ubicacion<- as.factor(Poma$ubicacion)##SI
Poma$nido<- as.factor(Poma$nido)##SI
Poma<-within(Poma, tranmes <- factor(ubicacion:nido))
head(Poma)
names(Poma)
str(Poma)
plot(Poma)

levels(Poma$ubicacion)
Poma$ubicacion <- factor(Poma$ubicacion, levels = c ("border", "center"))


hist(Poma$longitud)
hist(Poma$Area)
plot(Poma$Area~Poma$ubicacion)
plot(Poma$longitud~Poma$ubicacion)

########## LT-Location #######

# Modelo lineal mixto
mix.tan2 <- lmer(longitud ~ especie + ubicacion + (1 | nido), data = Poma, REML = FALSE)

# Calcular los efectos
effects_tan2 <- allEffects(mix.tan2)
summary(mix.tan2)
lsmeans(mix.tan2,pairwise ~ ubicacion:especie) 
simulateResiduals(mix.tan2, n=1000, plot= T)

# Convertir el efecto de 'especie' a data frame
df_especie <- as.data.frame(effects_tan2[["especie"]])

# Verificar la estructura del data frame
str(df_especie)

# Definir colores para cada especie
colores <- c("A. troschelii" = '#00CD66', "S. acapulcoensis" = '#FF8C00', "M. dorsalis" = '#1874CD')

# Graficar usando ggplot2 sin el cuadro de leyenda
Fig1 <- ggplot(df_especie, aes(x = factor(especie), y = fit)) +
  geom_point(aes(color = especie), size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper, color = especie), width = 0.2, size = 1.2) +
  geom_line(aes(group = 1), color = "black", size = 1) + # Aumentar el grosor de la línea de unión
  scale_color_manual(values = colores) +
  labs(y = "Total length (cm)", x = "Species") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(face = "italic", family = "Arial", size = 10, color = "black"),
    axis.text.y = element_text(family = "Arial", size = 10, color = "black"),
    axis.title.x = element_text(family = "Arial", size = 10, color = "black"),
    axis.title.y = element_text(family = "Arial", size = 10, color = "black"),
    panel.grid.major = element_blank(),  # Eliminar la cuadrícula mayor
    panel.grid.minor = element_blank(),  # Eliminar la cuadrícula menor
    axis.line = element_line(color = "black"),  # Agregar líneas a los ejes
    axis.ticks.x = element_line(color = "black"),  # Agregar ticks en el eje x
    axis.ticks.y = element_line(color = "black"),  # Agregar ticks en el eje y
    legend.position = "none"  # eliminar la leyenda
  ) +
  theme(
    axis.line.x.top = element_line(color = "black", size = 0.5),  # Agregar la línea superior
    axis.line.y.right = element_line(color = "black", size = 0.5),  # Agregar la línea derecha
    panel.border = element_rect(color = "black", fill = NA, size = 0.5)  # Agregar la caja alrededor del gráfico
  ) +
  annotate(
    "text", x = Inf, y = Inf, hjust = 1.8, vjust = 1.8,
    label = "a", fontsize = 12, fontfamily = "Arial"
  ) +
  ggtitle("")

# Mostrar el gráfico
print(Fig1)


# Convertir el efecto de 'ubicacion' a data frame
df_ubicacion <- as.data.frame(effects_tan2[["ubicacion"]])

# Verificar la estructura del data frame
str(df_ubicacion)

# Definir colores para cada ubicación
colores_ubicacion <- c("border" = "#68228b" ,"center" = "#8b0a50")

# Graficar usando ggplot2
Fig2 <- ggplot(df_ubicacion, aes(x = factor(ubicacion), y = fit)) +
  geom_point(aes(color = ubicacion), size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper, color = ubicacion), width = 0.2, size = 1.2) +
  geom_line(aes(group = 1), color = "black", size = 1) + # Aumentar el grosor de la línea de unión
  scale_color_manual(values = colores_ubicacion) +
  labs(y = "Total length (cm)", x = "Location") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(family = "Arial", size = 10, color = "black"),
    axis.text.y = element_text(family = "Arial", size = 10, color = "black"),
    axis.title.x = element_text(family = "Arial", size = 10, color = "black"),
    axis.title.y = element_text(family = "Arial", size = 10, color = "black"),
    panel.grid.major = element_blank(),  # Eliminar la cuadrícula mayor
    panel.grid.minor = element_blank(),  # Eliminar la cuadrícula menor
    axis.line = element_line(color = "black"),  # Agregar líneas a los ejes
    axis.ticks.x = element_line(color = "black"),  # Agregar ticks en el eje x
    axis.ticks.y = element_line(color = "black"),  # Agregar ticks en el eje y
    legend.position = "none"  # eliminar la leyenda
  ) +
  theme(
    axis.line.x.top = element_line(color = "black", size = 0.5),  # Agregar la línea superior
    axis.line.y.right = element_line(color = "black", size = 0.5),  # Agregar la línea derecha
    panel.border = element_rect(color = "black", fill = NA, size = 0.5)  # Agregar la caja alrededor del gráfico
  ) +
  annotate(
    "text", x = Inf, y = Inf, hjust = 1.8, vjust = 1.8,
    label = "b", fontsize = 12, fontfamily = "Arial"
  ) +
  ggtitle("")

# Mostrar el gráfico
print(Fig2)

# Generar el gráfico base
tan_plot4 <- ggpredict(mix.tan2, terms = c("especie", "ubicacion"), type = "fe") %>% 
  plot(connect.lines = FALSE, add.data = TRUE, line.size = 1, dot.size = 3, dot.alpha = 0.2) +
  
  # Ajustes de tema y colores
  theme_classic(base_size = 20) +
  scale_colour_manual(values = c( "#68228b", "#8b0a50")) +
  scale_fill_manual(values = c( "#68228b", "#8b0a50")) +
  
  # Etiquetas de ejes y títulos
  labs(y = "Total length (cm)", x = "Species", title = NULL) +  # Eliminar el título del gráfico
  
  theme(
    axis.text.x = element_text(face = "italic", family = "Arial", color = "black", size = 10),
    axis.text.y = element_text(family = "Arial", color = "black", size = 11),
    axis.title.x = element_text(family = "Arial", size = 10),
    axis.title.y = element_text(family = "Arial", size = 10),
    plot.title = element_text(size = 12, hjust = 0.9, vjust = 0.2),
    axis.line = element_line(size = 0.5),  # Reducir el grosor de los ejes x e y
    axis.ticks = element_line(size = 0.5),  # Reducir el grosor de los ticks
    panel.border = element_rect(color = "black", size = 0.5, fill = NA),  # Agregar líneas superior y derecha del gráfico
    legend.position = "none"  # eliminar la leyenda
  ) +
  
  # Título personalizado dentro del gráfico
  annotation_custom(
    grob = grid::textGrob("c", x = 0.95, y = 0.95, just = c("right", "top"), gp = grid::gpar(fontsize = 12, fontfamily = "Arial")),
    xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
  )

# Mostrar el gráfico
tan_plot4

######### Area-Location###########

# Modelo mixto para área
mix.tan <- lmer(Area ~ especie + ubicacion + (1 | nido), data = Poma, REML = FALSE)

# Calcular los efectos
effects_tan <- allEffects(mix.tan)
summary(mix.tan)
lsmeans(mix.tan,pairwise ~ ubicacion:especie) 
simulateResiduals(mix.tan, n=1000, plot= T)

# Convertir el efecto de 'especie' a data frame
df_especie <- as.data.frame(effects_tan[["especie"]])

# Verificar la estructura del data frame
str(df_especie)

# Definir colores para cada especie
colores <- c("A. troschelii" = '#00CD66', "S. acapulcoensis" = '#FF8C00', "M. dorsalis" = '#1874CD')

# Graficar usando ggplot2
Fig3 <- ggplot(df_especie, aes(x = factor(especie), y = fit)) +
  geom_point(aes(color = especie), size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper, color = especie), width = 0.2, size = 1.2) +
  geom_line(aes(group = 1), color = "black", size = 1) + # Aumentar el grosor de la línea de unión
  scale_color_manual(values = colores) +
  labs(y = expression("Nest size (cm"^2*")"), x = "Species") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(face = "italic", family = "Arial", size = 10, color = "black"),
    axis.text.y = element_text(family = "Arial", size = 10, color = "black"),
    axis.title.x = element_text(family = "Arial", size = 10, color = "black"),
    axis.title.y = element_text(family = "Arial", size = 10, color = "black"),
    panel.grid.major = element_blank(),  # Eliminar la cuadrícula mayor
    panel.grid.minor = element_blank(),  # Eliminar la cuadrícula menor
    axis.line = element_line(color = "black"),  # Agregar líneas a los ejes
    axis.ticks.x = element_line(color = "black"),  # Agregar ticks en el eje x
    axis.ticks.y = element_line(color = "black"),  # Agregar ticks en el eje y
    legend.position = "none"  # eliminar la leyenda
  ) +
  theme(
    axis.line.x.top = element_line(color = "black", size = 0.5),  # Agregar la línea superior
    axis.line.y.right = element_line(color = "black", size = 0.5),  # Agregar la línea derecha
    panel.border = element_rect(color = "black", fill = NA, size = 0.5)  # Agregar la caja alrededor del gráfico
  ) +
  annotate(
    "text", x = Inf, y = Inf, hjust = 1.8, vjust = 1.8,
    label = "b", fontsize = 12, fontfamily = "Arial"
  ) +
  ggtitle("")

# Mostrar el gráfico
print(Fig3)

# Convertir el efecto de 'ubicacion' a data frame
df_ubicacion <- as.data.frame(effects_tan[["ubicacion"]])

# Verificar la estructura del data frame
str(df_ubicacion)

# Definir colores para cada ubicación
colores_ubicacion <- c("border" = "#68228b" ,"center" = "#8b0a50")

# Graficar usando ggplot2
Fig4 <- ggplot(df_ubicacion, aes(x = factor(ubicacion), y = fit)) +
  geom_point(aes(color = ubicacion), size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper, color = ubicacion), width = 0.2, size = 1.2) +
  geom_line(aes(group = 1), color = "black", size = 1) + # Aumentar el grosor de la línea de unión
  scale_color_manual(values = colores_ubicacion) +
  labs(y = expression("Nest size (cm"^2*")"), x = "Location") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(family = "Arial", size = 10, color = "black"),
    axis.text.y = element_text(family = "Arial", size = 10, color = "black"),
    axis.title.x = element_text(family = "Arial", size = 10, color = "black"),
    axis.title.y = element_text(family = "Arial", size = 10, color = "black"),
    panel.grid.major = element_blank(),  # Eliminar la cuadrícula mayor
    panel.grid.minor = element_blank(),  # Eliminar la cuadrícula menor
    axis.line = element_line(color = "black"),  # Agregar líneas a los ejes
    axis.ticks.x = element_line(color = "black"),  # Agregar ticks en el eje x
    axis.ticks.y = element_line(color = "black"),  # Agregar ticks en el eje y
    legend.position = "none"  # eliminar la leyenda
  ) +
  theme(
    axis.line.x.top = element_line(color = "black", size = 0.5),  # Agregar la línea superior
    axis.line.y.right = element_line(color = "black", size = 0.5),  # Agregar la línea derecha
    panel.border = element_rect(color = "black", fill = NA, size = 0.5)  # Agregar la caja alrededor del gráfico
  ) +
  annotate(
    "text", x = Inf, y = Inf, hjust = 1.8, vjust = 1.8,
    label = "d", fontsize = 12, fontfamily = "Arial"
  ) +
  ggtitle("")

# Mostrar el gráfico
print(Fig4)

# Generar el gráfico base
tan_plot42 <- ggpredict(mix.tan, terms = c("especie", "ubicacion"), type = "fe") %>% 
  plot(connect.lines = FALSE, add.data = TRUE, line.size = 1, dot.size = 3, dot.alpha = 0.2) +
  
  # Ajustes de tema y colores
  theme_classic(base_size = 20) +
  scale_colour_manual(values = c( "#68228b", "#8b0a50")) +
  scale_fill_manual(values = c( "#68228b", "#8b0a50")) +
  
  # Etiquetas de ejes y títulos
  labs(y = expression("Nest size (cm"^2*")"), x = "Species", title = NULL) +  # Eliminar el título del gráfico
  
  theme(
    axis.text.x = element_text(face = "italic", family = "Arial", color = "black", size = 10),
    axis.text.y = element_text(family = "Arial", color = "black", size = 10),
    axis.title.x = element_text(family = "Arial", size = 10),
    axis.title.y = element_text(family = "Arial", size = 10),
    plot.title = element_text(size = 12, hjust = 0.9, vjust = 0.2),
    axis.line = element_line(size = 0.5),  # Reducir el grosor de los ejes x e y
    axis.ticks = element_line(size = 0.5),  # Reducir el grosor de los ticks
    panel.border = element_rect(color = "black", size = 0.5, fill = NA),  # Agregar líneas superior y derecha del gráfico
    legend.position = "none"  # eliminar la leyenda
  ) +
  
  # Título personalizado dentro del gráfico
  annotation_custom(
    grob = grid::textGrob("d", x = 0.95, y = 0.95, just = c("right", "top"), gp = grid::gpar(fontsize = 12, fontfamily = "Arial")),
    xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
  )

# Mostrar el gráfico
tan_plot42

##### Frequency of interactions VS Location, LT, Area ######

attach(Poma2)

Poma2$especie<- as.factor(Poma2$especie)#SI
Poma2$nido<- as.factor(Poma2$nido)##SI
Poma2$ubicacion<- as.factor(Poma2$ubicacion)##SI
Poma2$Area<- as.factor(Poma2$Area)##SI
Poma2$longitud<- as.factor(Poma2$longitud)##SI
Poma2<-within(Poma2, tranmes <- factor(ubicacion:nido))
head(Poma2)
names(Poma2)
str(Poma2)
plot(Poma2)


levels(Poma2$ubicacion)
Poma2$ubicacion <- factor(Poma2$ubicacion, levels = c ("border", "center"))

#Ajustar el modelo
mix.mod1 <- lmer(interacciones ~ longitud + Area + ubicacion + (1 | nido), data = Poma2, REML = FALSE)

summary(mix.mod1)
plot(allEffects(mix.mod1))###lmer


