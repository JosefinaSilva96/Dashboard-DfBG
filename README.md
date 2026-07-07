# DfBG Country Dashboard

Dashboard Shiny para el **AI & Data for Better Governance Survey** (World Bank – DECPR).
Permite elegir un país, explorar los gráficos por pregunta del cuestionario Agency
(con vista *país solo* o *país vs. promedio de su income group*) y **descargar un
country brief automatizado en Word**, replicando el formato del brief de Austria.

---

## Estructura

```
dfbg_dashboard/
├── app.R                       # UI + server
├── R/
│   ├── question_dictionary.R   # ★ especificación de cada pregunta (núcleo)
│   ├── data_load.R             # carga de dfbg_*.rds + income groups
│   ├── plots.R                 # gráficos por pregunta (country / compare)
│   └── build_brief.R           # generación del .docx con officer
├── data/                       # ← acá van tus datos (ver abajo)
└── prepare_data.R              # helper opcional para generar los .rds
```

## 1. Preparar los datos

El dashboard consume directamente la salida de `build_dfbg_database_EN.R`.
Copiá a la carpeta `data/`:

- `dfbg_agency.rds`
- `dfbg_managers.rds`
- `dfbg_systems.rds`
- *(opcional)* `CLASS_2025_10_07.xlsx` → activa los income groups y la
  comparación país vs. promedio del grupo de ingreso.

> Si solo tenés los `.csv`, también funcionan: la app busca primero `.rds` y
> luego `.csv` con el mismo nombre.

Para regenerarlos, corré tu `build_dfbg_database_EN.R` (ya los exporta en
`OUT_DIR`) y copiá los tres archivos a `data/`.

## 2. Correr en local (RStudio)

```r
# instalar dependencias una vez
install.packages(c("shiny","bslib","tidyverse","officer","flextable",
                   "janitor","readxl","ggplot2","forcats","scales"))

shiny::runApp()   # desde la carpeta del proyecto
```

## 3. Desplegar en shinyapps.io / Posit Connect

```r
install.packages("rsconnect")
rsconnect::setAccountInfo(name = "...", token = "...", secret = "...")
rsconnect::deployApp(appDir = ".")
```

⚠️ Para el deploy, los `.rds` deben estar dentro de `data/` (rutas relativas).
No uses rutas absolutas de OneDrive.

---

## Cómo agregar / editar preguntas

Toda la lógica de qué se grafica vive en **`R/question_dictionary.R`**.
Cada pregunta es una entrada de la lista `AGENCY_QUESTIONS` con esta forma:

```r
q8 = list(
  id = "q8", section = "AI adoption",
  title = "Government use of AI for citizen-facing public services",
  short = "Citizen-facing AI usage",
  type = "single",            # "single" | "multi" | "barrier" | "text"
  cols = "q8",                # columna(s) de la base
  labels = c(builder = "Builder", ...),   # código -> etiqueta
  levels = c("Builder", "Adapter", ...),  # orden de categorías
  palette = PAL_ADOPTION,
  in_brief = TRUE             # ¿entra al country brief?
)
```

Agregar una pregunta nueva = agregar una entrada. El gráfico en el Shiny y la
fila/sección del brief se generan solos. No hay que tocar `app.R` ni `plots.R`.

### Tipos de pregunta

| type      | qué hace en el Shiny                          | en el brief |
|-----------|-----------------------------------------------|-------------|
| `single`  | barras de la distribución (país vs grupo)     | fila en la tabla de indicadores |
| `multi`   | Yes/No/Not sure por opción                    | gráfico |
| `barrier` | heatmap / barras apiladas de severidad        | heatmap |
| `text`    | tabla con las respuestas abiertas             | viñetas |

---

## Notas

- La ponderación del promedio del income group es **1/n por país**, igual que en
  los R Markdown originales (cada país pesa lo mismo).
- La narrativa de la *Section 2 — AI analysis* del brief es un **borrador
  automático basado en reglas**; está pensada para editarse, no para reemplazar
  el análisis cualitativo fino.
- Los heatmaps de Systems/Managers detectan las columnas de barreras por
  heurística (`barrier`, `data_quality`, etc.). Si tus columnas tienen otros
  nombres, ajustá el patrón en `add_barriers_heatmap()` dentro de
  `R/build_brief.R`.
