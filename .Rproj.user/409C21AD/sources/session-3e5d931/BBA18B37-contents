# UNIVERSIDAD DE SAN CARLOS DE GUATEMALA
# FACULTAD DE AGRONOMÍA
# CENTRO DE TELEMÁTICA -CETE-
# FUNCIONES PARA ESTADISTICA DESCRIPTIVA PARA DATOS AGRUPADOS
# P. Agr. Ludwing Isaí Marroquín Jiménez

# Enlace Github Ejemplo:
# https://ludwing-mj.github.io/R-para-el-analisis-estadistico-de-datos-

# Repositorio Github Script: 
# https://github.com/Ludwing-MJ/MTCDPR_datos_agrupados.git

###############################################################################
# Función para calcular parámetros de agrupamiento
###############################################################################

calcular_parametros_agrupamiento <- function(datos) {
  n <- length(datos)
  x_min <- min(datos)
  x_max <- max(datos)
  rango <- x_max - x_min
  
  # Regla de Sturges para número de clases
  k <- round(1 + 3.322 * log10(n))
  
  # Amplitud de clase
  amplitud <- rango / k
  
  return(list(
    n = n,
    x_min = x_min,
    x_max = x_max,
    rango = rango,
    k = k,
    amplitud = amplitud
  ))
}
###############################################################################
# Función corregida para construir tabla de frecuencias
construir_tabla_frecuencias <- function(datos, parametros) {
  
  # Crear breaks (puntos de corte) para las clases
  # Esto garantiza exactamente k clases
  breaks <- seq(parametros$x_min, parametros$x_max, length.out = parametros$k + 1)
  
  # Crear límites de clase a partir de los breaks
  limite_inferior <- breaks[-length(breaks)]  # Todos excepto el último
  limite_superior <- breaks[-1]               # Todos excepto el primero
  
  # Calcular marcas de clase
  marca_clase <- (limite_inferior + limite_superior) / 2
  
  # Calcular frecuencias absolutas usando cut()
  intervalos <- cut(datos, 
                    breaks = breaks,
                    include.lowest = TRUE,
                    right = FALSE,
                    labels = FALSE)  # Usar números en lugar de etiquetas
  
  # Contar frecuencias por clase
  frecuencia_absoluta <- as.numeric(table(factor(intervalos, levels = 1:parametros$k)))
  
  # Reemplazar NA por 0 si alguna clase queda vacía
  frecuencia_absoluta[is.na(frecuencia_absoluta)] <- 0
  
  # Calcular frecuencias derivadas
  frecuencia_relativa <- frecuencia_absoluta / parametros$n
  frecuencia_acumulada <- cumsum(frecuencia_absoluta)
  fi_xi <- frecuencia_absoluta * marca_clase
  fi_xi2 <- frecuencia_absoluta * (marca_clase^2)
  
  # Crear tabla
  tabla <- data.frame(
    Clase = 1:parametros$k,
    Limite_Inferior = round(limite_inferior, 3),
    Limite_Superior = round(limite_superior, 3),
    Marca_Clase = round(marca_clase, 3),
    Frecuencia_Absoluta = frecuencia_absoluta,
    Frecuencia_Relativa = round(frecuencia_relativa, 4),
    Frecuencia_Acumulada = frecuencia_acumulada,
    fi_xi = round(fi_xi, 3),
    fi_xi2 = round(fi_xi2, 3)
  )
  
  return(tabla)
}
###############################################################################
# Función CORREGIDA para calcular medidas de tendencia central
###############################################################################
calcular_tendencia_central <- function(tabla, parametros) {
  # Media aritmética
  media <- sum(tabla$fi_xi) / parametros$n
  
  # Mediana
  n <- parametros$n
  posicion_mediana <- n / 2
  clase_mediana <- which(tabla$Frecuencia_Acumulada >= posicion_mediana)[1]
  L <- tabla$Limite_Inferior[clase_mediana]
  F_anterior <- ifelse(clase_mediana == 1, 0, tabla$Frecuencia_Acumulada[clase_mediana - 1])
  f_m <- tabla$Frecuencia_Absoluta[clase_mediana]
  A <- tabla$Limite_Superior[clase_mediana] - tabla$Limite_Inferior[clase_mediana]
  mediana <- L + ((posicion_mediana - F_anterior) / f_m) * A
  
  # Moda
  clase_modal <- which.max(tabla$Frecuencia_Absoluta)
  fa_ant <- ifelse(clase_modal == 1, 0, tabla$Frecuencia_Absoluta[clase_modal - 1])
  fa_sig <- ifelse(clase_modal == parametros$k, 0, tabla$Frecuencia_Absoluta[clase_modal + 1])
  d1 <- tabla$Frecuencia_Absoluta[clase_modal] - fa_ant
  d2 <- tabla$Frecuencia_Absoluta[clase_modal] - fa_sig
  if ((d1 + d2) == 0) {
    moda <- NA
  } else {
    moda <- tabla$Limite_Inferior[clase_modal] + (d1 / (d1 + d2)) * A
  }
  
  return(list(media = media, mediana = mediana, moda = moda))
}
###############################################################################
# Función para calcular medidas de dispersión
###############################################################################

  calcular_dispersion <- function(tabla, parametros, media) {
    # Rango aproximado
    rango_aprox <- tabla$Limite_Superior[parametros$k] - 
      tabla$Limite_Inferior[1]
    
    # Varianza
    varianza <- (sum(tabla$fi_xi2) - (sum(tabla$fi_xi)^2 / parametros$n)) / 
      (parametros$n - 1)
    
    # Desviación estándar
    desviacion_std <- sqrt(varianza)
    
    # Coeficiente de variación
    cv <- (desviacion_std / media) * 100
    
    return(list(
      rango = rango_aprox,
      varianza = varianza,
      desviacion_std = desviacion_std,
      cv = cv
    ))
  }

###############################################################################
# Función para calcular cuartiles y percentiles
###############################################################################

calcular_posicion_relativa <- function(tabla,
                                       parametros, posicion, 
                                       tipo = "cuartil") {
  if (tipo == "cuartil") {
    pos_valor <- posicion * parametros$n / 4
  } else if (tipo == "percentil") {
    pos_valor <- posicion * parametros$n / 100
  }
  
  clase_objetivo <- which(tabla$Frecuencia_Acumulada >= pos_valor)[1]
  fa_anterior <- ifelse(clase_objetivo == 1, 0, 
                        tabla$Frecuencia_Acumulada[clase_objetivo - 1])
  
  valor <- tabla$Limite_Inferior[clase_objetivo] + 
    ((pos_valor - fa_anterior) / 
       tabla$Frecuencia_Absoluta[clase_objetivo]) * parametros$amplitud
  
  return(valor)
}

###############################################################################
# Funcion personalizada para calcular los parametros a partir de una tabla de frecuencias
###############################################################################

calcular_parametros_desde_tabla <- function(tabla) {
  n <- sum(tabla$Frecuencia_Absoluta)
  x_min <- min(tabla$Limite_Inferior)
  x_max <- max(tabla$Limite_Superior)
  rango <- x_max - x_min
  k <- nrow(tabla)
  amplitud <- (tabla$Limite_Superior[1] - tabla$Limite_Inferior[1])
  
  return(list(
    n = n,
    x_min = x_min,
    x_max = x_max,
    rango = rango,
    k = k,
    amplitud = amplitud
  ))
}