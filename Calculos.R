# UNIVERSIDAD DE SAN CARLOS DE GUATEMALA
# ESCUELA DE ESTUDIOS DE POSTGRADO FACULTAD DE INGENIERIA
# Ludwing Isaí Marroquín Jiménez
# DPI: 3058 88165 0301

# Enlace Github Proyecto:
# https://github.com/Ludwing-MJ/Prueba_Diagnostica

################################################################################
# PRUEBA DIAGNOSTICA
################################################################################
# Instalación y carga de paquetes necesarios

## Para manipulación y visualización de datos
if (!require(tidyverse)) install.packages("tidyverse")

## Para importar archivos en excel
if (!require(readxl)) install.packages("readxl")
################################################################################
# Importar datos ejercicio 1
ej1 <- read_excel("Datos_prueba_diagnostica.xlsx", sheet = 2)

# Importar datos ejercicio 2
ej2 <- read_excel("Datos_prueba_diagnostica.xlsx", sheet = 3)

################################################################################
# TEMA 1: ESTADÍSTICA DESCRIPITIVA PARA DATOS AGRUPADOS
# Calculos exploratorios
datosej1 <- calcular_parametros_desde_tabla(ej1)
datosej1$amplitud <- 10 

# Histograma
histograma_ej1 <- ggplot(ej1, aes(x = Marca_Clase, y = Frecuencia_Relativa)) +
  geom_col(width = datosej1$amplitud, 
           fill = "lightblue", 
           color = "black") +
  scale_x_continuous(
    breaks = ej1$Marca_Clase,        
    labels = ej1$Marca_Clase            
  ) +
  labs(title = "Histograma del Tiempo de Atención al cliente",
       subtitle = "Distribución de frecuencias del tiempo (en segundos) que los 
cajeros de un almacén necesitaron para servir a una muestra de clientes",
       x = "Tiempo de atención a los clientes (segundos)",
       y = "Frecuencia relativa") +
  theme_minimal()
ggsave("histograma_ej1.jpg")
histograma_ej1

# Calculo de la media
mtc1 <- calcular_tendencia_central(ej1, datosej1)
mtc1

# Calculo del cv
md1 <- calcular_dispersion(ej1, datosej1, mtc1$media)
md1

# Calculo del coeficiente de asimetria
Q3 <- calcular_posicion_relativa(ej1, datosej1, 3, tipo = "cuartil"); Q3
Q2 <- calcular_posicion_relativa(ej1, datosej1, 2, tipo = "cuartil"); Q2 
Q1 <- calcular_posicion_relativa(ej1, datosej1, 1, tipo = "cuartil"); Q1
Me <- Q2 # La mediana equivale al cuartil 2 (Facilidad operativa de la formula)

SK3 <- ((Q3-Me)- (Me-Q1))/(Q3-Q1) ; SK3
################################################################################
# TEMA 2: REGRESIÓN LINEAL SIMPLE

# Diagrama de dispersión + Linea de tendencia
dispersion_ej2 <- ggplot(ej2, aes(x = H_x, y = TS_y)) +
  geom_point(color = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Diagrama de Dispersión: Dureza (H) vs Resisitencia a la tension (TS)",
       x = "Dureza (H)",
       y = "Resistencia a la tensión (TS)") +
  theme_minimal()
ggsave("dispersion_ej2.jpg")

# Calculo de la ecuación de la recta
regresion_ej2 <- lm(TS_y ~ H_x, data = ej2)
summary(regresion_ej2)

# Calculos de correlacion
cor.test(ej2$H_x, ej2$TS_y)

# Estimar la tensión cuando la dureza es 60 
TS60 <- 174.6900 + 2.25*(60); TS60

################################################################################
# TEMA 3: Distribución normal
ej3_media <- 240
ej3_sd <- 20
ej3_n <- 10000

# a.	¿cuántos trabajadores tienen una producción de más de 250 unidades por hora?
p_ej3_a <- pnorm(250, ej3_media, ej3_sd, F) ; p_ej3_a

t_250 <- p_ej3_a*ej3_n ; t_250 

# b.	Si cualquier trabajador que produzca menos de 200 unidades por hora debe 
# recibir entrenamiento posterior, ¿cuántos recibirán entrenamiento?
p_ej3_b <- pnorm(200, ej3_media, ej3_sd, T) ; p_ej3_b
t_200 <- p_ej3_b * ej3_n ; t_200
