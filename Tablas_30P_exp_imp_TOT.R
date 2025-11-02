library(readr)
library(dplyr)
library(openxlsx)


ruta <- "C:/Users/Santi/Documents/BACI_HS02_V202501"
col_code <- 170

# Lista los archivos
file_list <- list.files(path = ruta, pattern = "BACI_HS02_Y\\d{4}_V202501\\.csv$", full.names = TRUE)

# Creamos un dataframe vacío donde se irán guardando los resultados resumidos
resultados <- data.frame()

for (archivo in file_list) {
  # Leer un año
  datos <- read_csv(archivo, show_col_types = FALSE)
  
  # Filtrar solo Colombia como exportador o importador
  datos_col <- datos %>% filter(i == col_code | j == col_code)
  
  # Agregar (sumar valores por año y socio comercial)
  resumen <- datos_col %>%
    mutate(tipo = ifelse(i == col_code, "exportación", "importación"),
           socio = ifelse(i == col_code, j, i)) %>%
    group_by(t, socio, tipo) %>%
    summarise(valor = sum(v, na.rm = TRUE), .groups = "drop")
  
  # Guardar los resultados parciales
  resultados <- bind_rows(resultados, resumen)
  
  # Liberar memoria
  rm(datos, datos_col, resumen)
  gc()
}

# Exportar el resultado final
write_csv(resultados, "C:/Users/Santi/Documents/comercio_colombia_2002_2020.csv")

# --- 2. Tabla de países y sus códigos CEPII ---
codigos <- tibble(
  socio = c(32, 56, 68, 76, 124, 152, 156, 188, 214, 218,
            251, 276, 320, 699, 376, 380, 392, 484, 528, 591,
            604, 410, 643, 702, 724, 757, 784, 826, 842, 704),
  pais = c("Argentina", "Belgium", "Bolivia", "Brazil", "Canada",
           "Chile", "China", "Costa Rica", "Dominican Rep.", "Ecuador",
           "France", "Germany", "Guatemala", "India", "Israel", "Italy",
           "Japan", "Mexico", "Netherlands", "Panama", "Peru", "Korea, Republic of",
           "Russian Federation", "Singapore", "Spain", "Switzerland",
           "United Arab Emirates", "United Kingdom", "United States of America", "Viet Nam")
)

# --- 3. Agregar los nombres de los países a tu base ---
resultados_named <- resultados %>%
  left_join(codigos, by = "socio")

# --- 4. Calcular exportaciones e importaciones totales por año ---
totales <- resultados %>%
  group_by(t, tipo) %>%
  summarise(valor_total = sum(valor, na.rm = TRUE), .groups = "drop")

# --- 5. Filtrar solo los 30 países ---
resultados_30 <- resultados_named %>%
  filter(socio %in% codigos$socio)

# --- 6. Tablas separadas por tipo de comercio ---
exportaciones_30 <- resultados_30 %>%
  filter(tipo == "exportación") %>%
  select(t, pais, valor) %>%
  arrange(t, desc(valor))

importaciones_30 <- resultados_30 %>%
  filter(tipo == "importación") %>%
  select(t, pais, valor) %>%
  arrange(t, desc(valor))

# --- 7. Guardar resultados ---
write.xlsx(totales, "C:/Users/Santi/Documents/totales_colombia_2002_2020.xlsx")
write.xlsx(exportaciones_30, "C:/Users/Santi/Documents/exportaciones_top30.xlsx")
write.xlsx(importaciones_30, "C:/Users/Santi/Documents/importaciones_top30.xlsx")


