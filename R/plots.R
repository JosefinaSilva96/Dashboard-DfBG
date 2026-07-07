# =============================================================================
# plots.R
# -----------------------------------------------------------------------------
# Genera, a partir de la especificación de una pregunta (ver
# question_dictionary.R), el gráfico correspondiente para:
#   scope = "country" : la respuesta del país elegido
#   scope = "compare" : país vs. promedio (ponderado) de su income group
#
# Reusa la lógica de los .Rmd originales:
#   - ponderación 1/n por país (cada país pesa lo mismo en el promedio)
#   - mismas paletas y orden de categorías
#
# Devuelve siempre un objeto ggplot. Para preguntas "text" devuelve NULL (esas
# se muestran como tabla, no como gráfico).
# =============================================================================

library(ggplot2)

theme_dfbg <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title      = element_text(face = "bold", size = base_size + 1, hjust = 0,
                                     margin = margin(b = 4), lineheight = 1.05),
      plot.subtitle   = element_text(size = base_size - 2, hjust = 0, color = "grey30",
                                     margin = margin(b = 8)),
      axis.title.x    = element_text(face = "bold", size = base_size - 2),
      axis.text.y     = element_text(color = "black", size = base_size - 2, lineheight = 0.9),
      axis.text.x     = element_text(color = "black", size = base_size - 2),
      legend.position = "bottom",
      legend.title    = element_blank(),
      legend.text     = element_text(size = base_size - 3),
      panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_blank(),
      plot.caption    = element_text(size = base_size - 4, color = "grey45", hjust = 0),
      plot.margin     = margin(t = 10, r = 16, b = 8, l = 8)
    )
}

WB_BLUE <- "#002245"
CAP_TXT <- "Own elaboration based on DfBG Agency Questionnaire"

# --- Utilidad: respuestas decodificadas de una pregunta single ---------------
# Aplica el recode del diccionario y fija el factor en el orden definido.
prep_single <- function(df, q) {
  col <- q$cols[1]
  v <- df[[col]]
  if (!is.null(q$recoder))      v <- q$recoder(v)        # matcher por patrón (q7/q8/...)
  else if (!is.null(q$labels))  v <- recode_keep(v, q$labels)
  v <- factor(v, levels = q$levels)
  v
}

# =============================================================================
# 1. SINGLE — barras de la distribución (país vs income group)
# =============================================================================

plot_single <- function(data, q, country_name, scope = "compare", base = "agency") {
  ag <- data[[base]]
  col <- q$cols[1]

  # --- valor del país ---
  row_c <- dplyr::filter(ag, country == country_name)
  val_c <- if (nrow(row_c)) prep_single(row_c, q)[1] else NA

  if (scope == "country") {
    df <- tibble::tibble(category = factor(q$levels, levels = q$levels)) |>
      dplyr::mutate(value = dplyr::if_else(category == as.character(val_c), 1, 0))
    p <- ggplot(df, aes(category, value, fill = category)) +
      geom_col(width = 0.7) +
      scale_fill_manual(values = q$palette, drop = FALSE, guide = "none") +
      labs(title = q$title,
           subtitle = paste0(country_name, " — selected response highlighted"),
           x = NULL, y = NULL, caption = CAP_TXT) +
      coord_flip() + theme_dfbg() +
      theme(axis.text.x = element_blank())
    return(p)
  }

  # --- promedio income group (ponderado 1/n por país) ---
  ig <- row_c$income_group[1]
  peers <- ag |>
    dplyr::filter(!is.na(.data[[col]]))
  if (!is.na(ig)) peers <- dplyr::filter(peers, income_group == ig)

  peers <- peers |>
    dplyr::mutate(.cat = if (!is.null(q$recoder)) q$recoder(.data[[col]])
                         else recode_keep(.data[[col]], q$labels %||% setNames(q$levels, q$levels))) |>
    dplyr::filter(.cat %in% q$levels) |>
    dplyr::group_by(country) |>
    dplyr::mutate(w = 1 / dplyr::n()) |>
    dplyr::ungroup()

  grp <- peers |>
    dplyr::group_by(.cat) |>
    dplyr::summarise(n = sum(w), .groups = "drop") |>
    dplyr::mutate(pct = 100 * n / sum(n)) |>
    dplyr::rename(category = .cat)

  ig_lbl <- if (is.na(ig)) "all countries" else ig

  df <- tibble::tibble(category = factor(q$levels, levels = q$levels)) |>
    dplyr::left_join(grp, by = "category") |>
    dplyr::mutate(
      pct = tidyr::replace_na(pct, 0),
      is_country = category == as.character(val_c),
      lbl = paste0(round(pct), "%")
    )

  df_c <- dplyr::filter(df, is_country)
  # posición fija para las etiquetas: a la derecha de la barra más larga,
  # así nunca se solapan con el diamante ni con las barras
  x_lab <- max(df$pct, na.rm = TRUE) * 1.08

  p <- ggplot(df, aes(pct, category, fill = category)) +
    geom_col(width = 0.7, alpha = 0.55) +
    geom_col(data = df_c, aes(pct, category, fill = category), width = 0.7) +
    # diamante del país (se dibuja antes que la etiqueta)
    geom_point(data = df_c, aes(x = pct, y = category), shape = 23, size = 4,
               fill = "white", color = "black", stroke = 1.1) +
    # etiqueta de % alineada a la derecha, en columna fija (incluye None = 0%)
    geom_text(aes(x = x_lab, y = category, label = lbl),
              hjust = 0, size = 4, color = "grey20") +
    scale_fill_manual(values = q$palette, drop = FALSE, guide = "none") +
    scale_x_continuous(labels = function(x) paste0(round(x), "%"),
                       expand = expansion(mult = c(0, 0.20))) +
    labs(title = q$title,
         subtitle = paste0("Bars: ", ig_lbl, " average  \u25c7  Diamond + solid bar: ",
                           country_name, "'s response"),
         x = "Share of countries (weighted)", y = NULL, caption = CAP_TXT) +
    theme_dfbg()
  p
}

# =============================================================================
# 2. MULTI — Yes/No/Not sure por opción (país vs income group)
# =============================================================================

plot_multi <- function(data, q, country_name, scope = "compare", base = "agency") {
  ag   <- data[[base]]
  cols <- resolve_cols(q, ag)
  # excluir columnas "not sure" / "900" / texto libre "_1" del select_multiple
  cols <- cols[!grepl("(_900|_not_sure|_1)$", cols)]
  if (length(cols) == 0) return(empty_plot(q$title, "No data columns found"))

  # En esta encuesta cada columna de un select_multiple contiene la ETIQUETA de
  # la opción cuando fue seleccionada, o NA si no. "Seleccionada" = celda no
  # vacía y distinta de códigos negativos ("no", "none", "not sure", "0").
  is_selected <- function(x) {
    xl <- tolower(trimws(as.character(x)))
    !is.na(x) & xl != "" & !xl %in% c("0","no","none","not sure","not_sure","n/a","na")
  }

  # Envuelve etiquetas largas a max 2 lineas (3+ se trunca con "...") y quita
  # los sufijos "(e.g., ...)". Mantiene altura de fila predecible.
  clean_lab <- function(s, width = 38, max_lines = 2) {
    s <- gsub("\\s*\\(e\\.g\\.,?[^)]*\\)", "", s)
    s <- gsub("\\s*\\([^)]*\\)", "", s)
    s <- trimws(s)
    parts <- strwrap(s, width = width)
    if (length(parts) > max_lines) {
      parts <- parts[seq_len(max_lines)]
      last <- parts[max_lines]
      if (nchar(last) > width - 1)
        parts[max_lines] <- paste0(substr(last, 1, width - 1), "\u2026")
      else
        parts[max_lines] <- paste0(last, "\u2026")
    }
    paste(parts, collapse = "\n")
  }
  derive_label <- function(cc) {
    vals <- as.character(ag[[cc]])
    vals <- vals[is_selected(vals)]
    if (length(vals) == 0) return(prettify_code(cc))
    tb <- sort(table(trimws(vals)), decreasing = TRUE)
    top <- names(tb)[1]
    # Si la "etiqueta" es solo "1"/"yes"/"true" (dummies numéricas), usamos el
    # nombre de columna como fallback.
    if (tolower(top) %in% c("1","yes","true")) return(prettify_code(cc))
    top
  }
  # Convierte un código de columna (q17_central_unit) en etiqueta legible
  # ("Central unit") para casos donde no hay texto real en la celda.
  prettify_code <- function(cc) {
    s <- sub(paste0("^", q$id, "_?"), "", cc)
    s <- gsub("_", " ", s)
    s <- trimws(s)
    if (s == "") return(cc)
    paste(toupper(substr(s,1,1)), substr(s,2,nchar(s)), sep = "")
  }
  opt_lab <- q$options
  if (is.null(opt_lab)) {
    opt_lab <- setNames(vapply(cols, derive_label, character(1)), cols)
  }
  # aplica wrap/limpieza a todas las etiquetas (preservando los nombres=codes)
  opt_lab <- setNames(vapply(opt_lab, clean_lab, character(1)), names(opt_lab))

  to_long <- function(df) {
    df |>
      dplyr::select(country, dplyr::all_of(cols)) |>
      tidyr::pivot_longer(dplyr::all_of(cols), names_to = "code", values_to = "resp") |>
      dplyr::mutate(
        option = ifelse(!is.na(opt_lab[code]), opt_lab[code], code),
        sel    = is_selected(resp)
      )
  }

  # opciones que el país eligió (para marca ✓)
  country_sel <- to_long(dplyr::filter(ag, country == country_name)) |>
    dplyr::filter(sel) |> dplyr::pull(option) |> unique()

  if (scope == "country") {
    df <- to_long(dplyr::filter(ag, country == country_name)) |>
      dplyr::group_by(option) |>
      dplyr::summarise(sel = any(sel), .groups = "drop") |>
      dplyr::mutate(pct = ifelse(sel, 100, 0), lbl = ifelse(sel, "Selected", "\u2014"))
    p <- ggplot(df, aes(pct, forcats::fct_rev(factor(option)),
                        fill = sel)) +
      geom_col(width = 0.75) +
      geom_text(aes(x = 2, label = lbl), hjust = 0, size = 3.6, color = "grey20") +
      scale_fill_manual(values = c(`TRUE` = "#2E8B57", `FALSE` = "#E0E0E0"), guide = "none") +
      scale_x_continuous(limits = c(0, 100), expand = c(0, 0)) +
      labs(title = q$title, subtitle = paste0(country_name, " — selected options"),
           x = NULL, y = NULL, caption = CAP_TXT) +
      theme_dfbg() + theme(axis.text.x = element_blank())
    return(p)
  }

  # compare: % de PAÍSES (del income group) que seleccionan cada opción
  ig <- ag$income_group[ag$country == country_name][1]
  peers <- ag
  if (!is.na(ig)) peers <- dplyr::filter(peers, income_group == ig)

  grp <- to_long(peers) |>
    dplyr::group_by(country, option) |>
    dplyr::summarise(sel = any(sel), .groups = "drop") |>
    dplyr::group_by(option) |>
    dplyr::summarise(pct = 100 * mean(sel), .groups = "drop") |>
    dplyr::mutate(
      option_lbl = ifelse(option %in% country_sel, paste0("\u2713 ", option), option),
      lbl = paste0(round(pct), "%")
    ) |>
    dplyr::arrange(pct)

  ig_lbl <- if (is.na(ig)) "all countries" else ig
  x_lab  <- max(grp$pct, na.rm = TRUE) * 1.04

  p <- ggplot(grp, aes(pct, factor(option_lbl, levels = option_lbl))) +
    geom_col(width = 0.72, fill = "#1F77B4", alpha = 0.85) +
    geom_text(aes(x = x_lab, label = lbl), hjust = 0, size = 3.6, color = "grey20") +
    scale_x_continuous(labels = function(x) paste0(round(x), "%"),
                       breaks = seq(0, 100, 25),
                       expand = expansion(mult = c(0, 0.12))) +
    labs(title = paste(strwrap(q$title, width = 60), collapse = "\n"),
         subtitle = paste0("% of ", ig_lbl, " selecting each option  \u2014  \u2713 = ",
                          country_name),
         x = "Share of countries", y = NULL, caption = CAP_TXT) +
    theme_dfbg() +
    theme(axis.text.y = element_text(size = 10, lineheight = 0.95))
  attr(p, "n_items") <- nrow(grp)
  p
}

# =============================================================================
# 3. BARRIER — Likert apilado por constraint (país vs income group)
# =============================================================================

plot_barrier <- function(data, q, country_name, scope = "compare", base = "agency") {
  ag   <- data[[base]]
  cols <- resolve_cols(q, ag)
  if (length(cols) == 0) return(empty_plot(q$title, "No barrier columns found"))

  # Etiquetas de barreras: wrap a 45 chars, max 2 lineas (3+ se trunca con
  # "...") para que no choquen entre filas. Esto da una altura predecible
  # que add_plot puede convertir a alto del PNG.
  wrap_label <- function(s, width = 45, max_lines = 2) {
    parts <- strwrap(s, width = width)
    if (length(parts) > max_lines) {
      parts <- parts[seq_len(max_lines)]
      last <- parts[max_lines]
      if (nchar(last) > width - 1)
        parts[max_lines] <- paste0(substr(last, 1, width - 1), "\u2026")
      else
        parts[max_lines] <- paste0(last, "\u2026")
    }
    paste(parts, collapse = "\n")
  }
  item_lab <- q$items
  if (is.null(item_lab)) item_lab <- setNames(cols, cols)
  item_lab <- setNames(vapply(item_lab, wrap_label, character(1)),
                       names(item_lab))

  to_long <- function(df) {
    df |>
      dplyr::select(country, dplyr::all_of(intersect(cols, names(df)))) |>
      tidyr::pivot_longer(-country, names_to = "code", values_to = "lvl") |>
      dplyr::mutate(
        constraint = ifelse(!is.na(item_lab[code]), item_lab[code], code),
        lvl = normalize_barrier(lvl)
      ) |>
      dplyr::filter(!is.na(lvl))
  }

  if (scope == "country") {
    df <- to_long(dplyr::filter(ag, country == country_name)) |>
      dplyr::mutate(lvl = factor(lvl, levels = q$levels))
    p <- ggplot(df, aes(x = 1, y = forcats::fct_rev(factor(constraint)), fill = lvl)) +
      geom_tile(color = "white", linewidth = 1.2) +
      scale_fill_manual(values = q$palette, drop = FALSE) +
      labs(title = paste(strwrap(q$title, width = 55), collapse = "\n"),
           subtitle = paste0(country_name, " \u2014 barrier severity"),
           x = NULL, y = NULL, caption = CAP_TXT) +
      theme_dfbg() +
      theme(axis.text.x = element_blank(), panel.grid = element_blank(),
            axis.text.y = element_text(size = 10, lineheight = 0.95))
    attr(p, "n_items") <- length(unique(df$constraint))
    return(p)
  }

  ig <- ag$income_group[ag$country == country_name][1]
  peers <- ag
  if (!is.na(ig)) peers <- dplyr::filter(peers, income_group == ig)

  grp <- to_long(peers) |>
    dplyr::group_by(constraint, lvl) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop_last") |>
    dplyr::mutate(pct = 100 * n / sum(n)) |>
    dplyr::ungroup() |>
    dplyr::mutate(lvl = factor(lvl, levels = q$levels))

  ig_lbl <- if (is.na(ig)) "all countries" else ig
  grp <- grp |>
    dplyr::mutate(lbl = ifelse(pct >= 10, paste0(round(pct), "%"), ""))
  p <- ggplot(grp, aes(pct, forcats::fct_rev(factor(constraint)), fill = lvl)) +
    geom_col(width = 0.75) +
    geom_text(aes(label = lbl), position = position_stack(vjust = 0.5),
              size = 3.0, color = "white", fontface = "bold") +
    scale_fill_manual(values = q$palette, drop = FALSE) +
    scale_x_continuous(labels = function(x) paste0(round(x), "%"),
                       breaks = seq(0, 100, 25), expand = c(0, 0)) +
    labs(title = paste(strwrap(q$title, width = 55), collapse = "\n"),
         subtitle = paste0(ig_lbl, " barrier profile (stacked)"),
         x = "Share of countries", y = NULL, caption = CAP_TXT) +
    theme_dfbg() +
    theme(axis.text.y = element_text(size = 10, lineheight = 0.95))
  attr(p, "n_items") <- length(unique(grp$constraint))
  p
}

# =============================================================================
# Dispatcher
# =============================================================================

make_plot <- function(data, q, country_name, scope = "compare", base = "agency") {
  if (q$type == "text") return(NULL)

  # Para Managers/Systems, la comparacion es ENTRE SECTORES del mismo pais
  # (no contra income group). Para Agency sigue la logica original.
  use_sectoral <- base %in% c("manager", "systems") && scope == "compare"

  # Para preguntas single sectoriales devolvemos NULL: el server usara
  # make_table_ui() para mostrar la tabla badge en sq_text.
  if (use_sectoral && q$type == "single") return(NULL)

  out <- tryCatch(
    if (use_sectoral) {
      switch(q$type,
        multi   = plot_multi_sectors(data, q, country_name, base = base),
        barrier = plot_barrier_sectors(data, q, country_name, base = base),
        empty_plot(q$title, "Unsupported question type")
      )
    } else {
      switch(q$type,
        single  = plot_single(data, q, country_name, scope, base = base),
        multi   = plot_multi(data, q, country_name, scope, base = base),
        barrier = plot_barrier(data, q, country_name, scope, base = base),
        empty_plot(q$title, "Unsupported question type")
      )
    },
    error = function(e) empty_plot(q$title, paste("Error:", conditionMessage(e)))
  )
  out
}

# Devuelve el UI de tabla badge para preguntas single sectoriales,
# o NULL si no aplica (el server lo muestra en sq_text).
make_table_ui <- function(data, q, country_name, scope = "compare", base = "agency") {
  use_sectoral <- base %in% c("manager", "systems") && scope == "compare"
  if (!use_sectoral || q$type != "single") return(NULL)
  tryCatch(
    table_single_sectors(data, q, country_name, base = base),
    error = function(e)
      shiny::p(style = "color:#c00;",
               paste("Error generating table:", conditionMessage(e)))
  )
}

# =============================================================================
# COMPARACIONES SECTORIALES (Managers/Systems)
# -----------------------------------------------------------------------------
# Para un país dado, comparamos los distintos sectores (Health, Tax, Procurement,
# Education, CivilService, PublicFinance) en lugar de comparar contra el
# promedio de su income group. Cada sector aporta una respuesta.
# =============================================================================

# Helper: agrega columna `sector` a una base Managers/Systems
add_sector_col <- function(df) {
  df |> dplyr::mutate(
    sector = stringr::str_remove(questionnaire, "^(Systems|Manager)_")
  )
}

# -- SINGLE: tabla badge por sector (reemplaza barras 100% de un color) --------
# Devuelve NULL para señalizar al dispatcher que use table_single_sectors().
plot_single_sectors <- function(data, q, country_name, base = "manager") {
  NULL  # handled by table_single_sectors() / make_table_ui()
}

# Genera un objeto HTML (shiny::tagList) con una tabla badge compacta:
#   - Cada fila = un sector
#   - Columnas: Sector | Manager response | Systems response (si existen ambos)
#   - Badge de color según la paleta del diccionario de preguntas
#
# También sirve para mostrar ambas fuentes (manager + systems) juntas cuando
# la pregunta existe en los dos cuestionarios.
table_single_sectors <- function(data, q, country_name,
                                  base = "manager",
                                  q_sys = NULL,   # especificación paralela en Systems
                                  base_sys = "systems") {
  # --- helper: extrae sector -> categoría para una base dada ----------------
  extract_sectors <- function(db, qq) {
    ag  <- add_sector_col(dplyr::filter(db[[base_fam_of(db, qq, base)]], country == country_name))
    col <- qq$cols[1]
    if (is.null(col) || !col %in% names(ag) || nrow(ag) == 0) return(NULL)
    ag |>
      dplyr::mutate(.cat = if (!is.null(qq$recoder)) qq$recoder(.data[[col]])
                           else recode_keep(.data[[col]], qq$labels %||% character(0))) |>
      dplyr::filter(!is.na(.cat), .cat != "") |>
      dplyr::select(sector, category = .cat)
  }

  # helper: nombre de slot correcto según base
  base_fam_of <- function(db, qq, fallback) {
    # si la base pedida existe en db la usamos, si no usamos el fallback
    if (fallback %in% names(db)) fallback else names(db)[1]
  }

  df_m <- tryCatch(extract_sectors(data, q), error = function(e) NULL)
  df_s <- if (!is.null(q_sys)) {
    tryCatch({
      ag_s <- add_sector_col(dplyr::filter(data[[base_sys]], country == country_name))
      col_s <- q_sys$cols[1]
      if (!is.null(col_s) && col_s %in% names(ag_s) && nrow(ag_s) > 0) {
        ag_s |>
          dplyr::mutate(.cat = if (!is.null(q_sys$recoder)) q_sys$recoder(.data[[col_s]])
                               else recode_keep(.data[[col_s]], q_sys$labels %||% character(0))) |>
          dplyr::filter(!is.na(.cat), .cat != "") |>
          dplyr::select(sector, category = .cat)
      } else NULL
    }, error = function(e) NULL)
  } else NULL

  if (is.null(df_m) && is.null(df_s))
    return(shiny::p(style = "color:#888;", "No data for this country."))

  # --- todos los sectores presentes (unión) ----------------------------------
  all_sectors <- sort(unique(c(df_m$sector, df_s$sector)))

  # --- función para crear un badge de color ----------------------------------
  make_badge <- function(cat_val, palette) {
    if (is.na(cat_val) || cat_val == "")
      return(shiny::tags$span(style = "color:#bbb; font-style:italic;", "—"))
    col <- palette[cat_val]
    if (is.na(col) || is.null(col)) col <- "#888"
    # text color: blanco si el color es oscuro, negro si claro
    # heurística rápida por luminancia del hex
    rgb_v <- tryCatch({
      r <- strtoi(substr(col, 2, 3), 16L)
      g <- strtoi(substr(col, 4, 5), 16L)
      b <- strtoi(substr(col, 6, 7), 16L)
      0.299 * r + 0.587 * g + 0.114 * b
    }, error = function(e) 128)
    txt_col <- if (rgb_v < 140) "#ffffff" else "#1a1a1a"
    shiny::tags$span(
      style = paste0(
        "display:inline-block; padding:3px 11px; border-radius:20px; ",
        "background:", col, "; color:", txt_col, "; ",
        "font-size:12px; font-weight:600; white-space:nowrap;"
      ),
      cat_val
    )
  }

  # --- prettify sector names ------------------------------------------------
  pretty_sector <- function(s) gsub("([A-Z])", " \\1", s) |> trimws()

  # --- construir filas de la tabla ------------------------------------------
  has_sys <- !is.null(df_s) && nrow(df_s) > 0
  pal_m   <- q$palette   %||% list()
  pal_s   <- if (!is.null(q_sys)) q_sys$palette %||% list() else pal_m

  rows <- lapply(all_sectors, function(sec) {
    cat_m <- df_m$category[df_m$sector == sec][1] %||% NA_character_
    cat_s <- if (has_sys) df_s$category[df_s$sector == sec][1] %||% NA_character_ else NULL

    cells <- list(
      shiny::tags$td(
        style = "padding:8px 14px; font-size:13px; font-weight:500; color:#333; white-space:nowrap; border-bottom:1px solid #f0f0f0;",
        pretty_sector(sec)
      ),
      shiny::tags$td(
        style = "padding:8px 14px; border-bottom:1px solid #f0f0f0;",
        make_badge(cat_m, pal_m)
      )
    )
    if (has_sys) {
      cells <- c(cells, list(
        shiny::tags$td(
          style = "padding:8px 14px; border-bottom:1px solid #f0f0f0;",
          make_badge(cat_s, pal_s)
        )
      ))
    }
    shiny::tags$tr(cells)
  })

  # --- cabecera -------------------------------------------------------------
  col_headers <- list(
    shiny::tags$th(style = "padding:8px 14px; font-size:12px; font-weight:700; color:#555; text-align:left; border-bottom:2px solid #ddd; background:#f7f7f7;", "Sector"),
    shiny::tags$th(style = "padding:8px 14px; font-size:12px; font-weight:700; color:#555; text-align:left; border-bottom:2px solid #ddd; background:#f7f7f7;", "Response")
  )
  if (has_sys) {
    col_headers <- c(col_headers, list(
      shiny::tags$th(style = "padding:8px 14px; font-size:12px; font-weight:700; color:#555; text-align:left; border-bottom:2px solid #ddd; background:#f7f7f7;", "Systems response")
    ))
  }

  # --- leyenda de colores ---------------------------------------------------
  lvls <- q$levels %||% names(pal_m)
  legend_items <- lapply(lvls, function(lv) {
    col <- pal_m[lv]; if (is.na(col) || is.null(col)) return(NULL)
    shiny::div(
      style = "display:flex; align-items:center; gap:6px;",
      shiny::tags$span(style = paste0("width:14px; height:14px; border-radius:3px; background:", col, "; flex-shrink:0;")),
      shiny::tags$span(style = "font-size:12px; color:#555;", lv)
    )
  })

  shiny::tagList(
    # título
    shiny::div(
      style = "margin-bottom:10px;",
      shiny::tags$span(style = "font-size:15px; font-weight:600; color:#002245;", q$title),
      shiny::tags$br(),
      shiny::tags$span(style = "font-size:12px; color:#888;",
                       paste0(country_name, " \u2014 response by sector"))
    ),
    # tabla
    shiny::div(
      style = "overflow-x:auto;",
      shiny::tags$table(
        style = paste0(
          "border-collapse:collapse; width:100%; ",
          "border:1px solid #e8e8e8; border-radius:8px; overflow:hidden; ",
          "box-shadow:0 1px 4px rgba(0,0,0,0.06);"
        ),
        shiny::tags$thead(shiny::tags$tr(col_headers)),
        shiny::tags$tbody(rows)
      )
    ),
    # leyenda
    shiny::div(
      style = "display:flex; flex-wrap:wrap; gap:10px 18px; margin-top:12px; padding:8px 12px; background:#f9f9f9; border-radius:6px;",
      legend_items
    ),
    # caption
    shiny::div(
      style = "font-size:11px; color:#aaa; margin-top:8px;",
      CAP_TXT
    )
  )
}

# -- MULTI: 1 barra por opción, segmentos por sector que la seleccionó --------
# Muestra para cada opción cuántos sectores la marcaron, con cada sector como
# un segmento coloreado. Una sola gráfica con todos los sectores integrados.
plot_multi_sectors <- function(data, q, country_name, base = "manager") {
  ag  <- add_sector_col(dplyr::filter(data[[base]], country == country_name))
  cols <- resolve_cols(q, ag)
  cols <- cols[!grepl("(_900|_not_sure|_700|_1)$", cols)]
  if (length(cols) == 0 || nrow(ag) == 0)
    return(empty_plot(q$title, "No data for this country"))

  is_selected <- function(x) {
    xl <- tolower(trimws(as.character(x)))
    !is.na(x) & xl != "" & !xl %in% c("0","no","none","not sure","not_sure","n/a","na","-","1")
  }

  clean_lab <- function(s) {
    s <- gsub("\\s*\\(e\\.g\\.,?[^)]*\\)", "", s)
    s <- gsub("\\s*\\([^)]*\\)", "", s)
    s <- trimws(s)
    paste(strwrap(s, width = 30), collapse = "\n")
  }
  derive_label <- function(cc) {
    v <- as.character(ag[[cc]]); v <- v[is_selected(v)]
    if (length(v) == 0) return(cc)
    names(sort(table(trimws(v)), decreasing = TRUE))[1]
  }
  opt_lab <- q$options
  if (is.null(opt_lab)) {
    opt_lab <- setNames(vapply(cols, derive_label, character(1)), cols)
  }
  opt_lab <- setNames(vapply(opt_lab, clean_lab, character(1)), names(opt_lab))

  long <- ag |>
    dplyr::select(sector, dplyr::all_of(cols)) |>
    tidyr::pivot_longer(dplyr::all_of(cols), names_to = "code", values_to = "resp") |>
    dplyr::mutate(option = ifelse(!is.na(opt_lab[code]), opt_lab[code], code),
                  sel    = is_selected(resp)) |>
    dplyr::filter(sel)

  # cuenta sectores por opción
  totals <- long |>
    dplyr::group_by(option) |>
    dplyr::summarise(n_sectors = dplyr::n(), .groups = "drop")

  # ordena opciones por número de sectores
  opt_order <- totals |> dplyr::arrange(n_sectors) |> dplyr::pull(option)
  long <- long |> dplyr::mutate(option = factor(option, levels = opt_order),
                                .seg = 1L)
  totals <- totals |> dplyr::mutate(option = factor(option, levels = opt_order),
                                    lbl = paste0(n_sectors, "/", dplyr::n_distinct(long$sector)))

  ggplot(long, aes(x = .seg, y = option, fill = sector)) +
    geom_col(width = 0.72, color = "white", linewidth = 0.4) +
    geom_text(data = totals, aes(x = n_sectors, y = option, label = lbl),
              hjust = -0.2, size = 3.5, color = "grey20", inherit.aes = FALSE) +
    scale_fill_brewer(palette = "Set2") +
    scale_x_continuous(breaks = seq(0, 6, 1),
                       expand = expansion(mult = c(0, 0.15))) +
    labs(title = paste(strwrap(q$title, width = 60), collapse = "\n"),
         subtitle = paste0(country_name,
                          " \u2014 number of sectors selecting each option"),
         x = "Number of sectors", y = NULL, caption = CAP_TXT) +
    theme_dfbg() +
    theme(legend.position = "bottom")
}

# -- BARRIER: diverging bar chart por sector ----------------------------------
# Reemplaza las barras apiladas al 100% por un diverging chart centrado en
# "Not barrier" (izquierda, verde) vs barreras (derecha, naranja/rojo).
# Para q23 (impacto AI) el centro es "No effect".
#
# Lógica de polaridad:
#   - PAL_BARRIER:  "Not barrier" y "Not sure" -> izquierda; resto -> derecha
#   - PAL_EFFECT:   "Strongly decreased"/"Decreased" -> izquierda;
#                   "No effect" -> mitad izquierda; resto -> derecha
# =============================================================================

plot_barrier_sectors <- function(data, q, country_name, base = "manager") {
  ag <- add_sector_col(dplyr::filter(data[[base]], country == country_name))
  cols <- resolve_cols(q, ag)
  if (length(cols) == 0 || nrow(ag) == 0)
    return(empty_plot(q$title, "No barrier data for this country"))

  wrap30 <- function(s) paste(strwrap(s, width = 30), collapse = "\n")
  item_lab <- q$items %||% setNames(cols, cols)
  item_lab <- setNames(vapply(item_lab, wrap30, character(1)), names(item_lab))

  long <- ag |>
    dplyr::select(sector, dplyr::all_of(intersect(cols, names(ag)))) |>
    tidyr::pivot_longer(-sector, names_to = "code", values_to = "lvl") |>
    dplyr::mutate(
      constraint = ifelse(!is.na(item_lab[code]), item_lab[code], code),
      lvl = normalize_barrier(lvl)
    ) |>
    dplyr::filter(!is.na(lvl)) |>
    dplyr::mutate(lvl = factor(lvl, levels = q$levels %||% names(PAL_BARRIER)))

  grp <- long |>
    dplyr::group_by(sector, constraint, lvl) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop_last") |>
    dplyr::mutate(pct = 100 * n / sum(n)) |>
    dplyr::ungroup()

  # Detectar si es una escala de "efecto" (q23) o de "barrera" (q19)
  is_effect_scale <- all(
    c("No effect", "Increased", "Strongly increased") %in% levels(grp$lvl) |
    c("No effect", "Increased", "Strongly increased") %in% unique(grp$lvl)
  )

  if (is_effect_scale) {
    # q23: decreased = izquierda, no effect = neutro (split), increased = derecha
    left_cats    <- c("Strongly decreased", "Decreased")
    neutral_cats <- c("No effect")
    right_cats   <- c("Increased", "Strongly increased", "Not sure")
    subtitle_txt <- paste0(country_name,
      " \u2014 impact by sector  |  \u25c4 decreased   no effect   increased \u25ba")
  } else {
    # q19: not barrier = izquierda, barriers = derecha
    left_cats    <- c("Not barrier")
    neutral_cats <- character(0)
    right_cats   <- c("Minor", "Moderate", "Major", "Not sure")
    subtitle_txt <- paste0(country_name,
      " \u2014 barrier severity by sector  |  \u25c4 not a barrier   barrier \u25ba")
  }

  grp <- grp |>
    dplyr::mutate(
      direction = dplyr::case_when(
        as.character(lvl) %in% left_cats    ~ -1,
        as.character(lvl) %in% neutral_cats ~ -0.5,   # mitad izquierda del centro
        TRUE                                ~ 1
      ),
      pct_dir = pct * direction
    )

  # Ordenar sectores por "net positivo" (más a la derecha = más barrera / más aumento)
  sector_order <- grp |>
    dplyr::group_by(constraint, sector) |>
    dplyr::summarise(net = sum(pct_dir), .groups = "drop") |>
    dplyr::group_by(sector) |>
    dplyr::summarise(avg_net = mean(net), .groups = "drop") |>
    dplyr::arrange(avg_net) |>
    dplyr::pull(sector)

  grp <- grp |>
    dplyr::mutate(sector = factor(sector, levels = sector_order))

  ggplot(grp, aes(x = pct_dir, y = sector, fill = lvl)) +
    geom_col(width = 0.75, color = "white", linewidth = 0.3) +
    geom_vline(xintercept = 0, color = "grey40", linewidth = 0.5) +
    scale_fill_manual(values = q$palette %||% PAL_BARRIER, drop = FALSE,
                      guide = guide_legend(nrow = 1)) +
    scale_x_continuous(
      labels = function(x) paste0(abs(round(x)), "%"),
      expand = expansion(mult = c(0.05, 0.05))
    ) +
    facet_wrap(~ constraint, ncol = 2) +
    labs(
      title    = paste(strwrap(q$title, width = 60), collapse = "\n"),
      subtitle = subtitle_txt,
      x        = "Share of responses",
      y        = NULL,
      caption  = CAP_TXT
    ) +
    theme_dfbg() +
    theme(
      strip.text      = element_text(size = 9, face = "bold", lineheight = 0.95),
      legend.position = "bottom"
    )
}

# Detecta las columnas de texto de una pregunta: usa q$cols si existen, y si no
# (o además) busca por prefijo del id (q11 -> q11, q11_1, q11_2, ...), excluyendo
# sufijos que son códigos de subpreguntas ya cubiertos por otras q.
text_cols <- function(q, df) {
  cols <- intersect(q$cols %||% character(0), names(df))
  if (length(cols) == 0 && !is.null(q$id)) {
    pat  <- paste0("^", q$id, "(_|$|[0-9])")
    cols <- grep(pat, names(df), value = TRUE)
  }
  cols
}

# Texto abierto -> tabla (para el Shiny y para el brief)
#
# OJO: Managers/Systems tienen UNA FILA POR SECTOR para un mismo país (p.ej.
# Health, Education, Taxation...). Antes esta función solo miraba la primera
# fila (ag[1, cols]), así que si el país tenía más de un cuestionario de
# Managers, las respuestas de texto de los demás sectores nunca se mostraban.
# Ahora recorremos TODAS las filas del país y, si hay más de una, etiquetamos
# cada respuesta con su sector para que quede claro de dónde viene.
make_text <- function(data, q, country_name, base = "agency") {
  ag <- dplyr::filter(data[[base]], country == country_name)
  cols <- text_cols(q, ag)
  if (length(cols) == 0 || nrow(ag) == 0) return(tibble::tibble(Response = character()))

  multi_row <- nrow(ag) > 1 && "questionnaire" %in% names(ag)
  out <- list()
  for (i in seq_len(nrow(ag))) {
    vals <- unlist(ag[i, cols], use.names = FALSE)
    vals <- vals[!is.na(vals) & trimws(vals) != ""]
    vals <- vals[!tolower(trimws(vals)) %in% c("na","n/a","none","not sure","-")]
    if (length(vals) == 0) next
    vals <- translate_to_english(vals)
    if (multi_row) {
      sector <- gsub("_", " ", stringr::str_remove(ag$questionnaire[i], "^(Systems|Manager)_"))
      vals <- paste0("[", sector, "] ", vals)
    }
    out[[length(out) + 1]] <- vals
  }
  if (length(out) == 0) return(tibble::tibble(Response = character()))
  tibble::tibble(Response = as.character(unlist(out, use.names = FALSE)))
}

empty_plot <- function(title, msg) {
  ggplot() +
    annotate("text", x = 0, y = 0, label = msg, size = 5, color = "grey40") +
    labs(title = title) + theme_void() +
    theme(plot.title = element_text(face = "bold", hjust = 0))
}

# =============================================================================
# Tarjetas de texto (estilo slide de categorías) para la pestaña "Text responses"
# -----------------------------------------------------------------------------
# Genera un grid de tarjetas con shiny::tags: una tarjeta por pregunta de texto,
# con header de color y las respuestas del país como viñetas.
# =============================================================================

# color de header por pregunta (mismo criterio que la slide de ejemplo)
# OJO: tiene que ser list(), no c(). Managers tiene preguntas de texto sin
# color asignado (q10, q15); con c() (vector atómico), TEXT_CARD_COLORS[[qid]]
# tira "subscript out of bounds" si el id no está. Con list(), devuelve NULL
# y el fallback gris de mas abajo funciona como estaba pensado.
TEXT_CARD_COLORS <- list(
  q11 = "#185FA5",  # azul  — Agency/Managers: aplicaciones AI
  q13 = "#185FA5",  # azul  — Systems: aplicaciones AI/ML
  q28 = "#A32D2D",  # rojo  — Agency: barreras
  q30 = "#BA7517",  # ámbar — Agency: iniciativas
  q34 = "#0F6E56"   # verde — Agency: acciones prioritarias
)

text_cards_ui <- function(data, country_name, qlist = AGENCY_QUESTIONS,
                          base = "agency", family_label = "Agency") {
  text_qids <- names(qlist)[vapply(qlist, function(q) identical(q$type, "text"),
                                   logical(1))]
  # filtrar a las que efectivamente tienen columnas en la base actual
  text_qids <- text_qids[vapply(text_qids,
                                function(qid) length(text_cols(qlist[[qid]], data[[base]])) > 0,
                                logical(1))]
  if (length(text_qids) == 0) {
    return(shiny::tags$p(style = "color:#888; font-style:italic;",
                         "No open-text questions for this questionnaire."))
  }

  cards <- lapply(text_qids, function(qid) {
    q   <- qlist[[qid]]
    tbl <- make_text(data, q, country_name, base = base)
    col <- TEXT_CARD_COLORS[[qid]]
    if (is.null(col) || is.na(col)) col <- "#444441"

    body <- if (nrow(tbl) == 0) {
      shiny::tags$p(style = "margin:12px 14px; color:#888; font-style:italic; font-size:14px;",
                    "No response for this country.")
    } else {
      shiny::tags$ul(
        style = "margin:0; padding:12px 14px 14px 30px; font-size:14px; line-height:1.6;",
        lapply(tbl$Response, function(t) shiny::tags$li(t))
      )
    }

    shiny::div(
      style = paste0(
        "background:#fff; border:0.5px solid #e3e3e0; border-radius:12px; ",
        "overflow:hidden;"),
      shiny::div(
        style = paste0("background:", col, "; padding:8px 14px; font-size:13px; ",
                       "font-weight:600; color:#fff;"),
        paste0(toupper(qid), " \u00b7 ", q$short)
      ),
      body
    )
  })

  shiny::tagList(
    shiny::div(
      style = "display:flex; align-items:baseline; gap:10px; margin-bottom:1rem;",
      shiny::tags$span(style = "font-size:18px; font-weight:500;", country_name),
      shiny::tags$span(style = "font-size:13px; color:#888;",
                       paste0("open-text responses \u00b7 ", family_label, " questionnaire"))
    ),
    shiny::div(
      style = paste0("display:grid; grid-template-columns:",
                     "repeat(auto-fit, minmax(300px, 1fr)); gap:14px;"),
      cards
    )
  )
}
