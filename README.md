
<!-- README.md is generated from README.Rmd. Please edit that file -->

# AnalisisCombinadoR

<!-- badges: start -->
<!-- badges: end -->

Este paquete proporciona varias funciones para análisis estadísticos,
incluyendo:

## Funciones

### ANOVA para múltiples ensayos analizados de forma individual.

- `ANOVA.individuales.DBCA(datos, bloque, var_resp, tratamiento, entorno)`:
  Realiza análisis de varianza (ANOVA) para DBCA de forma individual
  para cada entorno en un conjunto de datos dado.

- `ANOVA.individuales.DCA(datos, var_resp, tratamiento, entorno)`:
  Realiza análisis de varianza (ANOVA) para DCA de forma individual para
  cada entorno en un conjunto de datos dado.

### Análisis Combinado

- `analisis_combinado_DBCA (datos, nombre_bloque, nombre_var_resp, nombre_tratamiento, nombre_entornos, tipo_varianza, efecto_bloque)`:

Esta funcion realiza un analisis combinado de varianza para un diseño de
bloques completamente aleatorizado (DBCA). Permitiendo especificar la
estructura de varianza y el efecto del bloque en el modelo.

- `analisis_combinado_DCA (datos, nombre_var_resp, nombre_tratamiento, nombre_entornos, tipo_varianza)`:

Esta función realiza un análisis combinado de datos para un Diseño
Completamente Aleatorizado (DCA) utilizando modelos lineales mixtos.
Permite ajustar modelos con o sin estructura de varianza heterogénea
entre los entornos.

### Comprobación de Supuestos

- `graficos_residuales_homogenidad (ANOVA_ind, tipo_de_graficos)`:
  Genera diferentes tipos de gráficos útiles para verificar el supuesto
  de homogeneidad de varianzas de los residuos.

- `graficos_residuales_qqnorm(ANOVA_ind, tipo_visualizacion)`: Genera
  gráficos útiles para verificar el supuesto de normalidad de los
  residuos.

### Funciones Teóricas

- `modificar_var_resp(datos, nombre_bloque, nombre_var_resp, nombre_tratamiento, nombre_entornos)`:

Esta funcion ajusta modelos lineales para cada entorno unico en los
datos, calcula los residuos, y modifica la variable respuesta dividiendo
cada valor por la raiz cuadrada del error cuadratico medio de los
residuos. Luego, agrega esta variable respuesta modificada al conjunto
de datos original.

Basado en: Gupta et al. (2016)

- `efectos_fijos(datos,varresp,bloques,entornos,tratamiento)`:

Función teórica para realizar un análisis combinado a efectos fijos.

Basado en: McIntosh, M. S. (1983). Analysis of combined experiments.
Agronomy Journal, 75(1), 153-155.

## Instalacion:

Puedes instalar la versión en desarrollo de AnalisisCombinadoR de la
siguiente manera:

``` r
remotes::install_github("fernandodafonseca/AnalisisCombinadoR")
```

## Ejemplos:

Este paquete se encuentra actualmente en desarrollo y esta sección se
completará próximamente.
