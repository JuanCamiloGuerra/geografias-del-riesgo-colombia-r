# Geografías del riesgo: análisis revisado
# Requiere los tres microdatos descritos en data/raw/README.md.

library(tidyverse)
library(readxl)
library(janitor)

raw_dir <- file.path("data", "raw")
required <- file.path(raw_dir, c(
  "BD.csv",
  "Cadena_Productiva 3.xlsx",
  "Cultivos Ilicitos 2015-2019 3.xlsx"
))

if (!all(file.exists(required))) {
  stop(
    "Faltan microdatos. Consulta data/raw/README.md. Archivos ausentes: ",
    paste(basename(required[!file.exists(required)]), collapse = ", ")
  )
}

delitos <- read.csv(required[1], check.names = FALSE) |>
  as_tibble()

# Hurtos = hurto a personas + hurto a residencias, siguiendo el informe original.
hurtos_municipio <- delitos |>
  filter(TEMÁTICA %in% c("HURTO A PERSONAS", "HURTO A RESIDENCIAS")) |>
  count(MUNICIPIO, name = "hurtos")

homicidios_municipio <- delitos |>
  filter(TEMÁTICA == "HOMICIDIOS") |>
  count(MUNICIPIO, name = "homicidios")

# Full join: conserva municipios presentes en una sola temática y asigna cero.
relacion_municipal <- full_join(hurtos_municipio, homicidios_municipio, by = "MUNICIPIO") |>
  mutate(across(c(hurtos, homicidios), ~replace_na(.x, 0)))

correlacion <- cor(relacion_municipal$hurtos, relacion_municipal$homicidios)

# Corrección frente al script histórico: aquí sí se filtran homicidios antes de
# calcular la razón por sexo.
razon_homicidios <- delitos |>
  filter(TEMÁTICA == "HOMICIDIOS", SEXO %in% c("FEMENINO", "MASCULINO")) |>
  count(MUNICIPIO, SEXO) |>
  pivot_wider(names_from = SEXO, values_from = n, values_fill = 0) |>
  mutate(razon_hombre_mujer = if_else(FEMENINO > 0, MASCULINO / FEMENINO, NA_real_))

bogota <- delitos |>
  filter(MUNICIPIO == "BOGOTÁ D.C. (CT)") |>
  mutate(
    weapon_type = case_when(
      ARMA.EMPLEADA == "SIN EMPLEO DE ARMAS" ~ "Sin empleo de armas",
      ARMA.EMPLEADA == "ARMA DE FUEGO" ~ "Arma de fuego",
      ARMA.EMPLEADA %in% c("ARMA BLANCA", "ARMA BLANCA / CORTOPUNZANTE", "PUNZANTES", "CORTANTES", "CUCHILLA", "CINTAS/CINTURON", "CORTOPUNZANTES") ~ "Arma blanca",
      TRUE ~ "Otra"
    )
  )

bogota_weapons <- bogota |>
  count(weapon_type) |>
  mutate(share = n / sum(n))

write_csv(bogota_weapons, file.path("data", "derived", "bogota_weapons_recomputed.csv"))
write_csv(relacion_municipal, file.path("data", "derived", "municipal_crime_recomputed.csv"))
write_csv(razon_homicidios, file.path("data", "derived", "homicide_ratio_recomputed.csv"))

message("Correlación municipal revisada: ", round(correlacion, 4))

