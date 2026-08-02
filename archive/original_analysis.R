### Ejercico 1

### Cargar base de datos

rm(list = ls())

### Confirgurar la ruta de busqueda
setwd("C:/Users/algov/OneDrive - Universidad de los Andes/MINE 006 - Maestría Inteligencia de Negocios/2. Aprendizaje estadístico/Taller 1")
getwd()
# setwd("C:/Users/alejandrogomez/Documents/R/Maestría")

# Listar los archivos y carpetas en el directorio actual
objetos_en_directorio <- list.files()
objetos_en_directorio


# Cargar Base de datos 
BD <- read.csv("BD.csv")

#Visualizar base de datos 
head(BD)

# variables
names(BD)

# numero de registros
n = nrow(BD)
n

# numero de variables
p <- ncol(BD)
p

# hay datos faltantes?
any(is.na(BD)) ## FALSE

### agrupar por municipios

# install.packages("dplyr")
# install.packages("gapminder")
library(dplyr)      # manipulaci?n de datos
library(magrittr)   # Operador "pipe" %>%
library(gapminder)  # base de datos

View(BD)

BD_municipios <- BD %>%
  group_by(MUNICIPIO, TEMÁTICA) %>%
  summarise(Cantidad_Delitos = n())

# Calcular el porcentaje de mujeres víctimas por municipio y temática
BD_mujeres <- BD %>%
  group_by(MUNICIPIO, TEMÁTICA) %>%
  summarise(Porcentaje_Mujeres_Victimas = sum(SEXO == "FEMENINO") / n() * 100)

# Filtrar por zona rural y calcular el porcentaje de delitos en zonas rurales
# BD_rural <- BD %>%
#   group_by(MUNICIPIO, TEMÁTICA, ZONA) %>%
#   filter()

(BD_Rural <- BD %>%  
    filter(ZONA == "RURAL") %>%    
    group_by(MUNICIPIO, TEMÁTICA) %>%
    summarise(Total_Rural = n()))

BD_Porc_Rural <- merge(BD_municipios, BD_Rural, 
                       by = c("MUNICIPIO", "TEMÁTICA"), all = TRUE)

(BD_Porc_Rural <- BD_Porc_Rural %>%
  mutate(Porc_RURAL = round(Total_Rural/Cantidad_Delitos * 100, 2)))

# Combinar los resultados en una única base de datos
BD_resultados <- merge(BD_mujeres, BD_Porc_Rural, by = c("MUNICIPIO", "TEMÁTICA"))

View(BD_resultados)

################################################################################

# 1. Explique si la cantidad de homicidios tiene una correlación alta con la 
# cantidad de robos.

# Obtenemos las categorías de crímenes para saber cuáles se consideran robos
unique(BD$TEMÁTICA)

# Se filtran dos categorías que se consideran robos: "HURTO A RESIDENCIAS" y 
# "HURTO A PERSONAS":
(BD_ROBOS <- BD %>%  
    filter(TEMÁTICA == "HURTO A RESIDENCIAS" | TEMÁTICA == "HURTO A PERSONAS") %>%
    group_by(MUNICIPIO) %>%
    summarise(Total_ROBOS = n()))
dim(BD_ROBOS)

# Se filtra otro dataframe con el total de homicidios por municipio 
(BD_HOMICIDIOS <- BD %>%  
  filter(TEMÁTICA == "HOMICIDIOS") %>%
  group_by(MUNICIPIO) %>%
  summarise(Total_HOMICIDIOS = n()))
dim(BD_HOMICIDIOS)

# Se realiza un merge entre ambas bases, debido a que no tienen la misma 
# longitud, y por tanto el comando mutate() de dplyr no funciona
BD_1 <- merge(BD_ROBOS, BD_HOMICIDIOS, by = c("MUNICIPIO"))
names(BD_1)

# Sustituir NA por 0 en las columnas especificadas
BD_1$Total_ROBOS <- replace(BD_1$Total_ROBOS, is.na(BD_1$Total_ROBOS), 0)
BD_1$Total_HOMICIDIOS <- replace(BD_1$Total_HOMICIDIOS, is.na(BD_1$Total_HOMICIDIOS), 0)

(correlacion <- cor(BD_1$Total_ROBOS, BD_1$Total_HOMICIDIOS))

## En este caso , tienemos una correlación de  0.8386416 lo cual  sugiere
## una fuerte correlación positiva entre la cantidad de robos y la cantidad de homicidios
## Esto significa que, en general, los municipios con un alto número de robos tienden a 
## tener también un alto número de homicidios y viceversa.


################################################################################

# 2. Realice diagrama de barras de los diez municipios con mayor cantidad de 
# delitos por temática y explique.

(BD_Crimenes <- BD %>%
  group_by(MUNICIPIO, TEMÁTICA) %>%
  summarise(Total_Crimenes = n()))

BD_Crimenes <- BD_Crimenes[order(BD_Crimenes$Total_Crimenes, decreasing = TRUE), ]

top_10_crimenes <- split(BD_Crimenes, BD_Crimenes$TEMÁTICA)
top_10_crimenes <- lapply(X = top_10_crimenes, FUN = head, 10)
View(top_10_crimenes)

library(ggplot2)

# Crear una función para graficar cada dataframe en la lista
graficar_tabla <- function(df, tema) {
  df$MUNICIPIO <- factor(df$MUNICIPIO,
                    levels = df$MUNICIPIO[order(df$Total_Crimenes, decreasing = TRUE)])
  ggplot(df, aes(x = MUNICIPIO, y = Total_Crimenes)) +
    geom_bar(stat = "identity", fill = "lightblue") +
    labs(title = paste("Top 10 Municipios -", tema),
         x = "Municipio",
         y = "Total de Crímenes") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

for (tema in names(top_10_crimenes)) {
  # Abrir una nueva ventana gráfica
  dev.new()
  
  # Crear el gráfico y mostrarlo
  grafico <- graficar_tabla(top_10_crimenes[[tema]], tema)
  print(grafico)
}

################################################################################

# 3. Realice histogramas de la razón de homicidios de hombres sobre homicidios 
# de mujeres.

# agrupar por homicios 

names(BD)
unique(BD$SEXO)

(BD_HOMICIDIOS_FEMENINOS <- BD %>%  
    filter(SEXO == "FEMENINO") %>%    
    group_by(MUNICIPIO) %>%
    summarise(Total_HOMICIDIOS_FEMENINOS = n()))

(BD_HOMICIDIOS_MASCULINO <- BD %>%  
    filter(SEXO == "MASCULINO") %>%    
    group_by(MUNICIPIO) %>%
    summarise(Total_HOMICIDIOS_MASCULINOS = n()))

(BD_HOMICIDIOS_REGITRADOS <- BD %>%  
    filter(SEXO == "FEMENINO" | SEXO == "MASCULINO") %>%    
    group_by(MUNICIPIO) %>%
    summarise(Total_HOMICIDIOS_REGISTRADOS = n()))

# Fusionar los marcos de datos
(BD_3 <- merge(merge(BD_HOMICIDIOS_FEMENINOS, BD_HOMICIDIOS_MASCULINO, by = "MUNICIPIO", all = TRUE), 
              BD_HOMICIDIOS_REGITRADOS, by = "MUNICIPIO", all = TRUE))

BD_3$razon = round(BD_3$Total_HOMICIDIOS_MASCULINOS/BD_3$Total_HOMICIDIOS_FEMENINOS,2)

dev.new()
hist(BD_3$razon, prob = TRUE, border = "lightblue", col = "darkgreen",
     xlab = "", ylab = "Densidad", main = "Histograma de la razón de homicidios de hombres respecto a mujeres")

################################################################################
################################################################################

# • Ahora debemos obtener una base de datos para Bogotá, para lograr el 
# proposito:

# 1. Reclasifique el tipo de arma empleada en las siguientes categorías: Sin 
# empleo de armas, arma de fuego, arma blanca y otra.

(BD_BOGOTA <- BD %>%  
   filter(MUNICIPIO == "BOGOTÁ D.C. (CT)"))

BD_BOGOTA <- mutate(BD_BOGOTA, 
                    Tipo_Arma_Reclasificada = case_when(
                      ARMA.EMPLEADA %in% c("SIN EMPLEO DE ARMAS") ~ "Sin empleo de armas",
                      ARMA.EMPLEADA %in% c("ARMA DE FUEGO") ~ "Arma de fuego",
                      ARMA.EMPLEADA %in% c("ARMA BLANCA", 
                                           "ARMA BLANCA / CORTOPUNZANTE", 
                                           "PUNZANTES", 
                                           "CORTANTES", 
                                           "CUCHILLA", 
                                           "CINTAS/CINTURON", 
                                           "CORTOPUNZANTES") ~ "Arma blanca",
                      TRUE ~ "Otra"
                    ))

# 2. Calcule la proporción de delitos cometidos en Bogotá, por cada tipo de arma
# empleada. Para eso, utilice la clasificación obtenida en el punto anterior.

(BD_tipo_Arma_BOG <- BD_BOGOTA %>%
    group_by(Tipo_Arma_Reclasificada) %>%
    summarise(Total_tipo_arma = n()))

(total_tipo_arma_BOG<-sum(BD_tipo_Arma_BOG$Total_tipo_arma))

BD_tipo_Arma_BOG$Proporcion <- round(BD_tipo_Arma_BOG$Total_tipo_arma/total_tipo_arma_BOG,2)
View(BD_tipo_Arma_BOG)

# 1 Arma blanca                       17180       0.16
# 2 Arma de fuego                     11840       0.11
# 3 Otra                              40123       0.38
# 4 Sin empleo de armas               35322       0.34

# 3. Realice una tabla cruzada que contemple las variables sexo y el tipo de 
# delito. ¿Cuál es la proporción de víctimas por tipo para cada sexo?

# Primero debemos recategorizar "SEXO" 

names(BD_BOGOTA)
unique(BD_BOGOTA$SEXO)
BD_BOGOTA <- mutate(BD_BOGOTA, 
                    sexo_reclasificado = case_when(
                      SEXO %in% c("MASCULINO") ~ "MASCULINO",
                      SEXO %in% c("FEMENINO") ~ "FEMENINO",
                      SEXO %in% c("NO REPORTADA", "NO REPORTA", 
                                  "NO REPORTADO","-") ~ NA_character_))
library(tidyr)
# Tabla de frecuencia absoluta
tabla_cruzada <- xtabs(~sexo_reclasificado+TEMÁTICA, data=BD_BOGOTA)

# Tabla de frecuencia relativa
tabla_relativa <- round(prop.table(table(BD_BOGOTA$sexo_reclasificado, 
                                     BD_BOGOTA$TEMÁTICA), margin = 2),2)
df_relativo <- as.data.frame(tabla_relativa)

dev.new()
ggplot(df_relativo, aes(x = Var2, y = Freq, fill = Var1)) +
  geom_col(position = "stack") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Proporción de género de víctimas por tipo de crimen",
       x = "Tipo de crimen",
       y = "Proporción de género")

# 4. Realice diagramas de caja donde se evidencie la comparación de la 
# distribución de las edades y el tipo de arma empleada en los delitos.

# Dividir el dataframe en una lista de dataframes por delito
## Crear la columna "EDAD_CORREGIDA": Todos los valores que contengan caracteres
## que no sean números, se clasifican como missing values:
BD_BOGOTA$EDAD_CORREGIDA <- as.integer(gsub("[^0-9]+", "N/A", BD_BOGOTA$EDAD))
unique(BD_BOGOTA$EDAD_CORREGIDA)

# Eliminamos los datos que contengan valores nulos
BD_BOGOTA_SIN_NA <- BD_BOGOTA[!is.na(BD_BOGOTA$sexo_reclasificado), ]

# Crear boxplots con ggplot2
dev.new()
ggplot(BD_BOGOTA_SIN_NA, aes(x = sexo_reclasificado, y = EDAD_CORREGIDA, fill = TEMÁTICA)) +
  geom_boxplot() +
  labs(title = "Boxplots de Edad por Género y Tipo de Delito",
       x = "Género",
       y = "Edad") +
  theme_minimal()

###### OJO JUANCA!!!! OPCIÖN 2: Me parece más visual esta...
dev.new()
ggplot(BD_BOGOTA_SIN_NA, aes(x = TEMÁTICA, y = EDAD_CORREGIDA, fill = sexo_reclasificado)) +
  geom_boxplot() +
  labs(title = "Boxplots de Edad por Género y Tipo de Delito",
       x = "Género",
       y = "Edad") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################

##### Ejercicio 2

# ¿Qué vamos a hacer?

# Tome las bases de Cadena Productiva y Cultivos ilícitos, reestructure estos 
# conjuntos de datos para:
# • Obtener una base de datos por municipios que discrimine la producción, en 
# términos de área sembrada y cosechada, por grupos de cultivos para los años 
# 2007-2015. Plantee gráficos de línea para evaluar el crecimiento o 
# decrecimiento de cada uno de los grupos.

BD_CULTIVOS <- readxl::read_excel("Cultivos Ilicitos 2015-2019 3.xlsx")
BD_CADENA <- readxl::read_excel("Cadena_Productiva 3.xlsx", sheet = "in")

(BD_CADENA_2 <- BD_CADENA %>%  
    filter(PERIODO2 >= 2007) %>%
  group_by(MUNICIPIO, `GRUPO DE CULTIVO`, PERIODO2, `Codigo de Municipio`) %>%
  summarise(Prod = sum(`Produccion`),
            area_semb = sum(`Area Sembrada(ha)`),
            area_cosech = sum(`Area Cosechada(ha)`)))

BD_CADENA_2$MUNICIPIO <- as.factor(BD_CADENA_2$MUNICIPIO)
BD_CADENA_2$`GRUPO DE CULTIVO` <- as.factor(BD_CADENA_2$`GRUPO DE CULTIVO`)

# Debemos agrupar por la suma total del cultivo entre todos los municipios por año 
grupo_cultivo_TOTAL <- BD_CADENA_2 %>% 
  group_by(PERIODO2, `GRUPO DE CULTIVO`) %>% 
  summarize(Producción = sum(Prod),
            `Área sembrada` = sum(area_semb),
            `Área cosechada` = sum(area_cosech))

# 3 Gráficos: Producción, área sembrada y área cosechada:
dev.new()
ggplot(data = grupo_cultivo_TOTAL, aes(x = PERIODO2, y = Producción, 
                                          color = `GRUPO DE CULTIVO`)) +
  geom_line(size = 1) +
  labs(title = "Evolución de la producción por grupo de cultivo")

dev.new()
ggplot(data = grupo_cultivo_TOTAL, aes(x = PERIODO2, y = `Área sembrada`, 
                                       color = `GRUPO DE CULTIVO`)) +
  geom_line(size = 1) +
  labs(title = "Evolución del área sembrada por grupo de cultivo")

dev.new()
ggplot(data = grupo_cultivo_TOTAL, aes(x = PERIODO2, y = `Área cosechada`, 
                                       color = `GRUPO DE CULTIVO`)) +
  geom_line(size = 1) +
  labs(title = "Evolución del área cosechada por grupo de cultivo")

# # Opción de gráfico unificando áreas sembradas y cosechadas:
# dev.new()
# ggplot(data = grupo_cultivo_TOTAL, aes(x = PERIODO2, color = `GRUPO DE CULTIVO`)) +
# 
#   # Segunda variable con línea punteada y ancho 1.5
#   geom_line(aes(y = `Área sembrada`, linetype = "Área sembrada"), size = 1) +
# 
#   # Tercera variable con línea gruesa y ancho 2
#   geom_line(aes(y = `Área cosechada`, linetype = "Área cosechada"), size = 1) +
# 
#   labs(title = "Evolución de variables por grupo de cultivo",
#        y = "Área total",  # Cambia la etiqueta del eje y según tus necesidades
#        color = "Grupo de cultivo") +  
# 
#   # Añade la escala de tipos de líneas manualmente
#   scale_linetype_manual(name = "Tipo de área",
#                         values = c("dashed", "solid"),
#                         labels = c("Área sembrada", "Área cosechada")) +
# 
#   theme_minimal()


################################################################################

#  • Haga un cruce de las bases de datos (join o merge). Una de ellas es la 
# asociada a los Cultivos ilícitos y otra que fue obtenida en el punto anterior 
# y calcule el porcentaje de cultivos ilícitos para cada municipio en 2015
# Total Cultivos ilícitos

# Base asociada a los cultivos ilícitos: Convertimos a numérico el código del
# municipio para poder hacer el merge de manera correcta. Luego nos quedamos 
# con las variables de interés
BD_CULTIVOS$CODMPIO <- as.numeric(BD_CULTIVOS$CODMPIO)

BD_CULTIVOS <- BD_CULTIVOS %>%
  select(CODMPIO, MUNICIPIO, `2015`) %>%
  rename(`Cultivos ilícitos 2015` = `2015`)

# Base del punto anterior. A esta base debemos filtrar los valores para 2015 y
# luego consolidar los totales de áreas sembradas por municipio y su código de
# municipio respectivo
BD_CADENA_2_2015 <- BD_CADENA_2 %>%
  filter(PERIODO2 == 2015)

(municipio_2015 <- BD_CADENA_2_2015 %>% 
  group_by(MUNICIPIO, `Codigo de Municipio`) %>% 
  summarize(Producción = sum(Prod),
            `Área sembrada` = sum(area_semb),
            `Área cosechada` = sum(area_cosech)) %>%
  select(c(-"Producción", -"Área cosechada"))) 

municipio_2015 <- municipio_2015 %>%
  rename(`Cultivos lícitos 2015` = `Área sembrada`)

# Procedemos a crear la base unificada
total_cultivos_2015 <- merge(BD_CULTIVOS, municipio_2015, by.x = "CODMPIO", 
                             by.y = "Codigo de Municipio", all.x =TRUE)

total_cultivos_2015 <- total_cultivos_2015 %>%
  filter(!is.na(`Cultivos ilícitos 2015`), !is.na(`Cultivos lícitos 2015`))  %>%
  select(-"MUNICIPIO.y")

total_cultivos_2015 <- total_cultivos_2015 %>%
  mutate(Porc_Cultivos_Ilícitos = (`Cultivos ilícitos 2015` / (`Cultivos lícitos 2015` + `Cultivos ilícitos 2015`)) * 100)

total_cultivos_2015 <- total_cultivos_2015 %>%
  arrange(desc(Porc_Cultivos_Ilícitos))

# Gráfico de barras:
dev.new()
ggplot(total_cultivos_2015[1:20,], aes(x = reorder(MUNICIPIO.x, -Porc_Cultivos_Ilícitos), 
                                       y = Porc_Cultivos_Ilícitos)) +
  geom_bar(stat = "identity", fill = "pink") +
  labs(title = "Top 20 Municipios con mayor proporción de cultivos ilícitos",
       x = "Municipio",
       y = "Proporción") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
