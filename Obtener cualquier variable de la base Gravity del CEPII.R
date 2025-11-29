library(dplyr)
library(readr)
library(readxl)
library(openxlsx)

Gravity <- read_csv("Gravity.csv") ### Aquí se agrega la base Gravity del CEPII
vars_grav <- Gravity %>%
  select(iso3num_o, iso3num_d, year, fta_wto)

mapa_paises_iso <- tribble(
  ~socio, ~iso3num_d, ~iso3_d,
  "Argentina", 32, "ARG",
  "Belgium", 56, "BEL",
  "Bolivia (Plurinational State of)", 68, "BOL",
  "Brazil", 76, "BRA",
  "Canada", 124, "CAN",
  "Chile", 152, "CHL",
  "China", 156, "CHN",
  "Costa Rica", 188, "CRI",
  "Dominican Rep.", 214, "DOM",
  "Ecuador", 218, "ECU",
  "France", 250, "FRA",
  "Germany", 276, "DEU",
  "Guatemala", 320, "GTM",
  "India", 356, "IND",
  "Israel", 376, "ISR",
  "Italy", 380, "ITA",
  "Japan", 392, "JPN",
  "Mexico", 484, "MEX",
  "Netherlands", 528, "NLD",
  "Panama", 591, "PAN",
  "Peru", 604, "PER",
  "Rep. of Korea", 410, "KOR",
  "Russian Federation", 643, "RUS",
  "Singapore", 702, "SGP",
  "Spain", 724, "ESP",
  "Switzerland", 756, "CHE",
  "USA", 840, "USA",
  "United Arab Emirates", 784, "ARE",
  "United Kingdom", 826, "GBR",
  "Viet Nam", 704, "VNM"
)

# Código de Colombia (origen)
iso3num_colombia <- 170  # Código numérico de Colombia
iso3_colombia <- "COL"   # Código alfabético de Colombia

# 2. Filtrar Gravity para obtener solo los datos específicos
gravity_filtrada <- vars_grav %>%
  filter(
    iso3num_o == iso3num_colombia,      # Solo Colombia como origen
    iso3num_d %in% mapa_paises_iso$iso3num_d,  # Solo los 30 países destino
    year >= 2002 & year <= 2020         # Solo años 2002-2020
  ) %>%
  select(iso3num_o, iso3num_d, year, fta_wto) %>%
  left_join(mapa_paises_iso, by = "iso3num_d") %>%  # Agregar nombres de países
  mutate(iso3_o = iso3_colombia) %>%    # Agregar código alfabético de Colombia
  select(iso3_o, iso3_d, socio, year, fta_wto)

##### PARA AGREGAR LAS VARIABLES NUEVAS A "DATA30P" (la base de datos de la
##### investigación).

data <- read_excel("DATA_30P.xlsx")

data_con_acuerdos <- data %>%
  left_join(gravity_filtrada, by = c("socio" = "socio", "año" = "year"))

# Guardar el data frame + la nueva variable (FTA)
write.xlsx(data_con_acuerdos, "DATA_30P_2E+FTA.xlsx")
