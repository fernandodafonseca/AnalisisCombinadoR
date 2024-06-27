#' Analisis combinado de varianza para un diseño de bloques completamente aleatorizado (DBCA).
#'
#' Esta funcion realiza un analisis combinado de varianza para un diseño de bloques completamente aleatorizado (DBCA),
#' permitiendo especificar la estructura de varianza y el efecto del bloque en el modelo.
#'
#' @param datos Un dataframe que contiene los datos del experimento.
#' @param nombre_bloque El nombre, entre comillas(""), de la columna que contiene la variable de bloque.
#' @param nombre_var_resp El nombre, entre comillas(""), de la columna que contiene la variable respuesta.
#' @param nombre_tratamiento El nombre, entre comillas(""), de la columna que contiene la variable de tratamiento.
#' @param nombre_entornos El nombre, entre comillas(""), de la columna que contiene la variable de entorno.
#' @param tipo_varianza Un numero entero que indica el tipo de estructura de varianza. 1 para homogeneidad y 2 para heterogeneidad.
#' @param efecto_bloque Un numero entero que indica el tipo de efecto del bloque en el modelo. 1 para aleatorio y 2 para fijo.
#'
#' @return Un dataframe con los resultados del analisis combinado de varianza.
#'
#' @examples
#' # Ejemplo de uso
#'
#' #data <- read.csv("datos.csv")
#' #resultado <- analisis_combinado_DBCA(data, "bloque", "var_resp", "tratamiento", "entorno", 1, 1)
#'
#' @importFrom nlme lme
#' @importFrom nlme gls
#' @importFrom nlme varIdent
#'
#' @export

analisis_combinado_DBCA <- function(datos, nombre_bloque, nombre_var_resp, nombre_tratamiento, nombre_entornos, tipo_varianza, efecto_bloque) {

  # Obtener los nombres originales de las columnas
  nombres_originales <- names(datos)

  # Verificar si los nombres de las columnas solicitadas estan presentes en los datos
  if (!(nombre_bloque %in% nombres_originales)) {
    stop("El nombre del bloque no se encontro en los datos.")
  }

  if (!(nombre_var_resp %in% nombres_originales)) {
    stop("El nombre de la variable respuesta no se encontro en los datos.")
  }

  if (!(nombre_tratamiento %in% nombres_originales)) {
    stop("El nombre del tratamiento no se encontro en los datos.")
  }

  if (!(nombre_entornos %in% nombres_originales)) {
    stop("El nombre del entorno no se encontro en los datos.")
  }

  # Renombrar las columnas
  names(datos)[names(datos) == nombre_bloque] <- "bloque"
  names(datos)[names(datos) == nombre_var_resp] <- "var_resp"
  names(datos)[names(datos) == nombre_tratamiento] <- "tratamiento"
  names(datos)[names(datos) == nombre_entornos] <- "entorno"

  # Transformar a factor
  datos$bloque <- factor(datos$bloque)
  datos$tratamiento <- factor(datos$tratamiento)
  datos$entorno <- factor(datos$entorno)

  datos$BxS <- paste(datos$bloque, datos$entorno, sep = "_")

  # Ajustar el modelo lineal mixto
  if (tipo_varianza == 1) {     # ASUME HOMOGENIDAD DE VARIANZAS

    if (efecto_bloque == 1) {   # ASUME BLOQUES ALEATORIOS

      modelo <- lme(var_resp ~ tratamiento + entorno + entorno:tratamiento, random = ~ 1|BxS, data = datos)
      anova_resultados <- anova(modelo)

      nombre_interaccion <- paste(nombre_tratamiento,nombre_entornos, sep=":")
      row.names(anova_resultados) <- c("",nombre_tratamiento,nombre_entornos,nombre_interaccion)
      anova_resultados <-anova_resultados[-1, ]

    } else if (efecto_bloque == 2) {  # ASUME BLOQUES FIJOS

      modelo <- lm(var_resp ~ tratamiento + entorno + bloque%in%entorno + entorno:tratamiento, data = datos)
      anova_resultados <- anova(modelo)

      nombre_interaccion <- paste(nombre_tratamiento,nombre_entornos, sep=":")
      nombre_bloqueINentorno <- paste(nombre_entornos,nombre_bloque, sep=":")
      row.names(anova_resultados) <- c(nombre_tratamiento,nombre_entornos,nombre_bloqueINentorno,nombre_interaccion,"Residuals")

      anova_resultados[2,4] <- anova_resultados[2,3]/anova_resultados[3,3]
      anova_resultados[2,5] <- pf(anova_resultados[2,3]/anova_resultados[3,3],df1 = anova_resultados[2,1], df2 = anova_resultados[3,1],lower.tail=FALSE)

    }else {
      stop("El tipo de efecto para el bloque no es valido. Debe ser 1 (aleatorio) o 2 (fijo).")
    }
  } else if (tipo_varianza == 2) {    # ASUME VARIANZAS HETEROGENEAS

    if (efecto_bloque == 1) {         # ASUME BLOQUES ALEATORIOS

      modelo <- lme(var_resp ~ tratamiento + entorno + entorno:tratamiento, random = ~ 1|BxS, data = datos, weights = varIdent(form = ~1|entorno))
      anova_resultados <- anova(modelo)

      nombre_interaccion <- paste(nombre_tratamiento,nombre_entornos, sep=":")
      row.names(anova_resultados) <- c("",nombre_tratamiento,nombre_entornos,nombre_interaccion)
      anova_resultados <-anova_resultados[-1, ]

    } else if (efecto_bloque == 2) {    # ASUME BLOQUES FIJOS

      modelo <- gls(var_resp ~ tratamiento + entorno + bloque%in%entorno + entorno:tratamiento, data = datos, weights = varIdent(form = ~1|entorno))
      anova_resultados <- anova(modelo)
      anova_resultados <- anova_resultados[-1,]

      #nombre_interaccion <- paste(nombre_tratamiento,nombre_entornos, sep=":")
      #nombre_bloqueINentorno <- paste(nombre_entornos,nombre_bloque, sep=":")
      #row.names(anova_resultados) <- c(nombre_tratamiento,nombre_entornos,nombre_bloqueINentorno,nombre_interaccion,"Residuals")

    }else {
      stop("El tipo de efecto para el bloque no es valido. Debe ser 1 (aleatorio) o 2 (fijo).")
    }
  } else {
    stop("El tipo de analisis especificado no es valido. Debe ser 1 o 2.")
  }

  # Devolver los resultados del analisis combinado
  print(anova_resultados)
  return(invisible(modelo))
}
