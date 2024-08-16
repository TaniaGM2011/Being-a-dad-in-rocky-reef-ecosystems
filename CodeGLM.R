library(ggplot2)

##### A. troschelii ######
# Definir los datos
LT <- c(23.57, 25.04, 24.2, 24.58, 25.8, 23.47, 15.51, 23.06, 19.48, 24.69, 
        17.92, 19.29, 12.47, 18.26, 18.1, 17.20, 14.94, 16.04, 18.5, 18.78, 
        15.62, 21.85, 23.10, 24.17, 15.20, 23.98, 13.48, 25.42, 15.58, 24.69)
Area <- c(92.842, 375.7, 172, 235.1, 296, 245.9, 738.3, 311.4, 443.5, 324.5, 
          619.8, 48.9, 145.9, 902.9, 180.3, 371, 531.3, 318.3, 497.8, 580.3, 
          502.7, 265.2, 474.4, 258.9, 102.5, 185.5, 66.8, 132.8, 212, 102)

# Crear un data frame
data <- data.frame(LT, Area)

# Ajustar el modelo GLM con distribución Gaussiana
glm_model <- glm(Area ~ LT, data = data, family = gaussian())

# Mostrar el resumen del modelo
summary(glm_model)


# Graficar los datos y la línea de regresión
ggplot1 <-ggplot(data = data, mapping = aes(x = LT, y = Area)) +
  geom_point(color = "black", size = 2) +
  geom_smooth(method = "glm",method.args = list(family = "gaussian"), se = TRUE, color = "black", fill = '#00CD66', alpha = 0.3) +
  labs(x = "Total length of guarding males (cm)", y = expression(Nest~size~(cm^2))) +
  theme_bw() +
  theme(axis.text = element_text(family = "Arial", size = 10, color = "black"),
        axis.title = element_text(family = "Arial", size = 11, color = "black"))

# Mostrar el gráfico
print(ggplot1)

################### S. aca ####################
# Definir los datos
LT <- c(20.96,18.36,14.11,18.37,16.89,12.98,17.99,17.39,18.05,14.44,13.96,15.39,
        13.72,18.65,13.6,19.68,20.16,14.67,12.19,15.45,15.22,18.18,13.44,19.12,
        12.66,20.41,12.65,13.54,17.03,17.23)
Area <- c(284.9,181.9,135.1,26.5,25,72.2,386.2,172.4,543.2,460.3,21.4,281.9,185.8,
          179.4,250.5,118.4,308.1,111.9,128.5,65,232.3,186.9,47.2,58.2,91.6,202.1,
          57.3,178.8,288.5,450)

# Crear un data frame
data <- data.frame(LT, Area)

# Ajustar el modelo GLM con distribución Gaussiana
glm_model2 <- glm(Area ~ LT, data = data, family = gaussian())

# Mostrar el resumen del modelo
summary(glm_model2)


# Graficar los datos y la línea de regresión
ggplot2 <-ggplot(data = data, mapping = aes(x = LT, y = Area)) +
  geom_point(color = "black", size = 2) +
  geom_smooth(method = "glm",method.args = list(family = "gaussian"), se = TRUE, color = "black", fill = '#FF8C00', alpha = 0.3) +
  labs(x = "Total length of guarding males (cm)", y = expression(Nest~size~(cm^2))) +
  theme_bw() +
  theme(axis.text = element_text(family = "Arial", size = 10, color = "black"),
        axis.title = element_text(family = "Arial", size = 11, color = "black"))

# Mostrar el gráfico
print(ggplot2)


###### M. dorsalis ######
# Definir los datos
LT <- c(29.14,33.44,28.84,25.79,26.58,30.15,31.4,23.95,29.97,28.94,31.10,35.66,
        29.66,32.07,33.27,31.67,27.19,26.62,27.41,31.56)
Area <- c(1268.126,954.217,535.6,1290.5,190.1,835,1873.2,542.3,488.3,409.6,600.5,
          722.4,548.9,480.2,400.6,1784.5,1440.5,855.2,760.3,1488.1)

# Crear un data frame
data <- data.frame(LT, Area)

# Ajustar el modelo GLM con distribución Gaussiana
glm_model3 <- glm(Area ~ LT, data = data, family = gaussian())

# Mostrar el resumen del modelo
summary(glm_model3)


# Graficar los datos y la línea de regresión
ggplot3 <-ggplot(data = data, mapping = aes(x = LT, y = Area)) +
  geom_point(color = "black", size = 2) +
  geom_smooth(method = "glm",method.args = list(family = "gaussian"), se = TRUE, color = "black", fill = '#1874CD', alpha = 0.3) +
  labs(x = "Total length of guarding males (cm)", y = expression(Nest~size~(cm^2))) +
  theme_bw() +
  theme(axis.text = element_text(family = "Arial", size = 10, color = "black"),
        axis.title = element_text(family = "Arial", size = 11, color = "black"))

# Mostrar el gráfico
print(ggplot3)