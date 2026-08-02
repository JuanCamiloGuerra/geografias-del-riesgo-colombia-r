# Geografías del riesgo · Colombia

[![R](https://img.shields.io/badge/R-4.3%2B-276DC3?logo=r&logoColor=white)](https://www.r-project.org/)
[![Artículo](https://img.shields.io/badge/Leer-artículo-DF6C4F?logo=readthedocs&logoColor=white)](https://juancamiloguerra.github.io/geografias-del-riesgo-colombia-r/article.html)
[![Dashboard](https://img.shields.io/badge/Abrir-dashboard-11A683?logo=githubpages&logoColor=white)](https://juancamiloguerra.github.io/geografias-del-riesgo-colombia-r/dashboard.html)
[![Licencia MIT](https://img.shields.io/badge/Licencia-MIT-D5A93B)](LICENSE)

Investigación reproducible en R sobre patrones municipales de delitos, victimización en Bogotá y estructura agrícola asociada con cultivos ilícitos en Colombia.

## Accesos directos

- **[Artículo científico web](https://juancamiloguerra.github.io/geografias-del-riesgo-colombia-r/article.html)**
- **[Dashboard interactivo](https://juancamiloguerra.github.io/geografias-del-riesgo-colombia-r/dashboard.html)**
- **[Informe académico original en PDF](https://juancamiloguerra.github.io/geografias-del-riesgo-colombia-r/docs/articulo-original.pdf)**

## Preguntas de investigación

1. ¿Qué asociación existe entre hurtos y homicidios a escala municipal?
2. ¿Cómo se distribuyen los delitos por municipio, género, edad y arma empleada?
3. ¿Qué patrones territoriales aparecen en la proporción de cultivos ilícitos?
4. ¿Cómo evolucionaron la producción y las áreas sembradas y cosechadas entre 2007 y 2015?

## Hallazgos destacados

- La correlación municipal reportada entre hurtos y homicidios fue `r = 0,839`; es una asociación descriptiva, no una relación causal.
- Bogotá concentró 104.465 registros en el análisis de armas: 38% fueron clasificadas como “otra”, 34% sin empleo de armas, 16% arma blanca y 11% arma de fuego.
- La composición por sexo cambia sustancialmente según la temática: homicidios y hurtos muestran mayoría masculina, mientras delitos sexuales y violencia intrafamiliar registran mayoría femenina.
- Los municipios con mayor participación de cultivos ilícitos se concentraron en el sur y el occidente del país.

## Estructura

```text
├── R/                         # análisis revisado y fuente del dashboard
├── data/derived/              # tablas resumidas auditables
├── data/raw/README.md         # archivos necesarios para reproducción completa
├── article.Rmd                # manuscrito en R Markdown
├── dashboard.Rmd              # dashboard fuente en flexdashboard
├── article.html               # artículo web publicado
├── dashboard.html             # dashboard interactivo publicado
├── docs/articulo-original.pdf # informe académico de 2023
└── archive/                   # código y materiales originales
```

## Reproducibilidad

La presentación puede regenerarse con los datos derivados incluidos:

```r
install.packages(c("rmarkdown", "flexdashboard", "tidyverse", "plotly", "DT", "scales"))
rmarkdown::render("article.Rmd")
rmarkdown::render("dashboard.Rmd")
```

Para rehacer los cálculos desde microdatos se necesitan `BD.csv`, `Cadena_Productiva 3.xlsx` y `Cultivos Ilicitos 2015-2019 3.xlsx`. Esos archivos no venían adjuntos y no se redistribuyen en este repositorio. Consulta [`data/raw/README.md`](data/raw/README.md).

## Nota metodológica

El análisis describe registros administrativos y agregados municipales. Las cifras absolutas dependen de denuncia, registro y cobertura; no deben interpretarse directamente como tasas de riesgo. Una comparación territorial rigurosa requiere denominadores poblacionales, periodo de observación homogéneo y evaluación de calidad del registro.

## Autoría

Trabajo original de Juan Camilo Guerra y Alejandro Gómez (2023), reorganizado como proyecto científico y portafolio BI en R (2026).

