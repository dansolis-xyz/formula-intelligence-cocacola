# ANÁLISIS DE SUSTANCIAS AÑADIDAS A ALIMENTOS - BASE DE DATOS FDA (EAFUS)
# Fuente: FDA Substances Added to Food (formerly EAFUS)
# URL: http://www.hfpappexternal.fda.gov/scripts/fdcc/?set=FoodSubstances
# Última actualización de la fuente: 04/21/2026
# Análisis realizado por: Danaeé Solís
# Contexto: Este análisis simula el flujo de trabajo de un Formula Intelligence
#            Scientist — exploración y visualización de datos regulatorios de 
#            ingredientes para apoyar decisiones de aprobación.



# Primer paso: Cargar librerias 

if (!require(ggplot2)) install.packages("ggplot2") 
if (!require(dplyr))   install.packages("dplyr")
if (!require(stringr)) install.packages("stringr")

library(ggplot2)
library(dplyr)
library(stringr)

# Paso 2: Cargar datos
# El archivo "FoodSubstances.csv" proviene directamente de la FDA.
# Contiene 3 líneas de encabezado informativo antes de los datos reales,
# por eso se usa skip = 3 para comenzar a leer desde la fila correcta.
# Cada fila representa una sustancia con su número CAS, nombre, usos técnicos
# autorizados y referencias regulatorias aplicables.


fda_sustancias <- read.csv("FoodSubstances.csv", skip = 3, header = TRUE)


# Paso 3:Exploración inicial del dataset
# Antes de cualquier análisis, es necesario entender la estructura del dato:
# (cuántos registros hay, qué columnas existen y qué tipo de información contienen)

# Primeras 6 filas para verificar que la carga fue correcta
head(fda_sustancias)

# Total de sustancias registradas en la base de datos
nrow(fda_sustancias)

# Nombres de todas las columnas disponibles
colnames(fda_sustancias)

# Estructura del dataframe: tipo de dato por columna
str(fda_sustancias)

# Resumen estadístico de todas las columnas
summary(fda_sustancias)


# Paso 4: Análisis de distribución 
# La columna "Used.for..Technical.Effect." indica la función tecnológica de 
# cada sustancia en alimentos (ej. conservador, edulcorante, colorante, etc.)
# Este análisis permite identificar cuáles categorías concentran el mayor
# número de ingredientes aprobados.
# AGREGADO: limpiar etiquetas HTML y quedarse con el primer uso de cada sustancia
fda_sustancias <- fda_sustancias %>%
  mutate(
    uso_limpio = str_remove_all(Used.for..Technical.Effect., "<br\\s*/?>"),
    uso_limpio = str_squish(uso_limpio),
    uso_limpio = str_trim(str_extract(uso_limpio, "^[^,]+"))
  )

resumen <- fda_sustancias %>%
  filter(!is.na(uso_limpio) & uso_limpio != "") %>%
  group_by(uso_limpio) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

print(resumen)

# Paso 5: Visualizar los datos
# Se seleccionan las 10 categorías con mayor número de sustancias aprobadas.
# (Esta visualización facilita comunicar al equipo técnico y regulatorio 
# cuáles son los tipos de ingredientes más representados en la base de datos,
# lo que orienta la priorización en procesos de gobernanza de fórmulas).


# Seleccionar únicamente las 10 categorías más frecuentes
top10 <- resumen %>%
  slice_max(n, n = 10)

# Paso 6:visualización de datos

ggplot(top10, aes(x = reorder(uso_limpio, n), y = n)) +
  geom_bar(stat = "identity", fill = "#E61A27") + # Rojo por coca cola :D
  geom_text(aes(label = n), hjust = -0.2, size = 3.5, color = "gray30") +
  coord_flip(clip = "off") +
  scale_y_log10() +
  theme_minimal() +
  labs(
    title = "Top 10 usos técnicos de sustancias aprobadas por la FDA",
    subtitle = "Fuente: FDA Substances Added to Food (EAFUS) — Descargado 06/2026",
    x = "Uso técnico del ingrediente",
    y = "Número de sustancias registradas",
    caption = "Análisis: Danaeé Solís | Aplicación: Formula Intelligence — Coca-Cola"
  ) +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10, color = "gray50"),
    plot.caption  = element_text(size = 8,  color = "gray60"),
    plot.margin   = margin(t = 10, r = 40, b = 10, l = 10),  # evita que se corten
    axis.text.y   = element_text(size = 9)
  )


# Paso 6: Guardar la gráfica
ggsave(
  filename = "analisis_usos_tecnicos_FDA.png",
  width    = 14,
  height   = 6,
  dpi      = 300
)
