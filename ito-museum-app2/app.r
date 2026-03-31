library(shiny)
library(leaflet)
library(dplyr)
library(readr)
library(htmltools)
library(RColorBrewer)

`%||%` <- function(a, b) if (!is.null(a)) a else b

# UTF-8 で失敗したら CP932（path は存在する前提）
read_sites_csv <- function(path) {
  tryCatch(
    read_csv(path, locale = locale(encoding = "UTF-8"), show_col_types = FALSE),
    error = function(e) {
      read_csv(path, locale = locale(encoding = "CP932"), show_col_types = FALSE)
    }
  )
}

strip_bom_names <- function(df) {
  nms <- names(df)
  nms <- sub("^\uFEFF", "", nms)
  names(df) <- nms
  df
}

break_before_labels <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("([^\n\r])(種類\\s*[：:])", "\\1\n\\2", x, perl = TRUE)
  x <- gsub("([^\n\r])(時代\\s*[：:])", "\\1\n\\2", x, perl = TRUE)
  x <- gsub("([^\n\r])(年代\\s*[：:])", "\\1\n\\2", x, perl = TRUE)
  x
}

desc_as_html <- function(x) {
  x <- break_before_labels(x)
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- htmlEscape(x)
  gsub("\r\n|\r|\n", "<br/>", x)
}

esc_field <- function(x) {
  x <- as.character(x)[1]
  if (length(x) != 1L || is.na(x)) {
    return("")
  }
  htmlEscape(trimws(x))
}

# アプリフォルダ付近の既定 CSV を探す
find_default_csv <- function(name = "ito_sites_clean.csv") {
  dirs <- c(
    Sys.getenv("SHINY_APP_DIR", NA_character_),
    getwd()
  )
  dirs <- unique(dirs[!is.na(dirs) & nzchar(dirs)])
  for (d in dirs) {
    p <- file.path(d, name)
    if (file.exists(p)) {
      return(normalizePath(p, winslash = "/", mustWork = TRUE))
    }
  }
  NA_character_
}

# 生の data.frame から地図用データ一式を作る
prepare_sites <- function(raw) {
  data <- raw %>% strip_bom_names()
  data <- data %>%
    rename(
      name   = any_of(c("name", "site_name", "遺跡名", "サイト名")),
      type   = any_of(c("type", "種類", "種別")),
      period = any_of(c("period", "年代", "時代")),
      desc   = any_of(c("desc", "description", "説明", "概要", "備考", "詳細", "テキスト")),
      lng    = any_of(c("lng", "lon", "longitude", "経度", "LON", "Lng")),
      lat    = any_of(c("lat", "latitude", "緯度", "LAT", "Lat"))
    )

  for (col in c("type", "period", "desc")) {
    if (!col %in% names(data)) {
      data[[col]] <- NA_character_
    }
  }

  if (all(is.na(data$desc) | trimws(as.character(data$desc)) == "")) {
    skip <- c("name", "lng", "lat", "type", "period", "desc")
    cand <- setdiff(names(data), skip)
    best_col <- NULL
    best_mean <- 0
    for (cn in cand) {
      x <- data[[cn]]
      if (!is.character(x) && !is.factor(x)) {
        next
      }
      v <- as.character(x)
      m <- suppressWarnings(mean(nchar(v, "chars", allowNA = TRUE), na.rm = TRUE))
      if (is.finite(m) && m > best_mean && m >= 8) {
        best_mean <- m
        best_col <- cn
      }
    }
    if (!is.null(best_col)) {
      message("説明列を自動推定: ", best_col)
      data$desc <- as.character(data[[best_col]])
    }
  }

  need <- c("name", "lng", "lat")
  missing <- setdiff(need, names(data))
  if (length(missing)) {
    stop(
      "必須列がありません: ", paste(missing, collapse = ", "),
      "。実際の列名: ", paste(names(data), collapse = ", ")
    )
  }

  data$lng <- suppressWarnings(as.numeric(data$lng))
  data$lat <- suppressWarnings(as.numeric(data$lat))
  data$has_geo <- !(is.na(data$lng) | is.na(data$lat))
  bad_geo <- !data$has_geo
  if (any(bad_geo)) {
    warning(sum(bad_geo), " 行の緯度・経度が数値化できませんでした（一覧には表示し、地図のみ除外します）")
  }
  if (!any(data$has_geo)) {
    stop("有効な緯度・経度がある行がありません")
  }

  row.names(data) <- NULL

  data$popup_body <- vapply(seq_len(nrow(data)), function(i) {
    nm <- esc_field(data$name[i])
    if (!nzchar(nm)) {
      nm <- htmlEscape("（遺跡名なし）")
    }
    tp <- esc_field(data$type[i])
    pr <- esc_field(data$period[i])
    dc <- desc_as_html(data$desc[i])
    paste0(
      "<div style=\"line-height:1.5;max-width:320px;white-space:normal;overflow-wrap:break-word;word-break:break-word;\">",
      "<div style=\"font-weight:700;font-size:1.05em;margin:0 0 8px;padding-bottom:6px;border-bottom:1px solid #ccc;\">",
      nm,
      "</div>",
      "<div style=\"margin:6px 0;\"><span style=\"color:#555;\">種類</span><br/>",
      tp,
      "</div>",
      "<div style=\"margin:6px 0;\"><span style=\"color:#555;\">時代</span><br/>",
      pr,
      "</div>",
      "<div style=\"margin:10px 0 0;padding-top:8px;border-top:1px solid #ddd;\">",
      dc,
      "</div>",
      "</div>"
    )
  }, character(1))

  data$marker_row_id <- seq_len(nrow(data))

  data$type_plot <- as.character(data$type)
  data$type_plot[is.na(data$type_plot) | data$type_plot == ""] <- "(未分類)"
  data$type_plot <- as.factor(data$type_plot)

  n_types <- length(levels(data$type_plot))
  pal <- colorFactor(
    palette = colorRampPalette(
      brewer.pal(min(9, max(3, n_types)), "Set1")
    )(n_types),
    domain = data$type_plot
  )

  pick_choices <- stats::setNames(
    as.character(data$marker_row_id),
    paste0(seq_len(nrow(data)), ". ", as.character(data$name))
  )

  list(data = data, pal = pal, pick_choices = pick_choices)
}

resolve_marker_row <- function(clk, df) {
  if (is.null(clk)) {
    return(NA_integer_)
  }
  rid <- clk$id
  if (length(rid) && !is.na(rid) && nzchar(as.character(rid[1]))) {
    i <- suppressWarnings(as.integer(rid[1]))
    if (!is.na(i) && i >= 1L && i <= nrow(df)) {
      return(i)
    }
  }
  if (!is.null(clk$lat) && !is.null(clk$lng)) {
    dist <- sqrt((df$lat - clk$lat)^2 + (df$lng - clk$lng)^2)
    j <- which.min(dist)
    if (length(j) && is.finite(dist[j]) && dist[j] < 0.002) {
      return(as.integer(j))
    }
  }
  NA_integer_
}

safe_txt <- function(x, empty_label = "（なし）") {
  x <- as.character(x)[1]
  if (length(x) != 1L || is.na(x)) {
    return(empty_label)
  }
  x <- trimws(x)
  if (!nzchar(x)) {
    return(empty_label)
  }
  htmlEscape(x)
}

format_coord <- function(x) {
  x <- suppressWarnings(as.numeric(x)[1])
  if (length(x) != 1L || is.na(x) || !is.finite(x)) {
    return("（不明）")
  }
  formatC(x, format = "f", digits = 6)
}

ui <- fluidPage(
  titlePanel("伊都国遺跡マップ"),
  wellPanel(
    style = "margin-bottom:12px;",
    fileInput(
      "user_csv",
      "CSV ファイルを選んで読み込む",
      accept = c("text/csv", "text/comma-separated-values", ".csv"),
      buttonLabel = "参照...",
      placeholder = "未選択のときは ito_sites_clean.csv を自動検索"
    ),
    tags$p(
      style = "color:#666;font-size:0.9em;margin:0 0 8px 0;",
      "アプリと同じフォルダに ", tags$code("ito_sites_clean.csv"), " があると自動で読みます。別ファイルのときは上で指定してください。"
    ),
    selectInput(
      "pick_site",
      "一覧から遺跡を選ぶ",
      choices = c("（データ読み込み待ち）" = ""),
      selectize = TRUE
    ),
    uiOutput("tap_detail")
  ),
  leafletOutput("map", height = "55vh")
)

server <- function(input, output, session) {
  last_tap <- reactiveVal(NULL)

  sites_bundle <- reactive({
    path_upload <- NULL
    up <- input$user_csv
    if (!is.null(up) && is.data.frame(up) && nrow(up) >= 1L) {
      p <- up$datapath[1]
      if (length(p) && nzchar(p) && file.exists(p)) {
        path_upload <- p
      }
    }

    path <- path_upload %||% find_default_csv()
    validate(
      need(
        !is.na(path) && nzchar(path) && file.exists(path),
        paste(
          "CSV が見つかりません。",
          "① このアプリと同じフォルダに ito_sites_clean.csv を置く、",
          "または ② 上の「CSV ファイルを選んで読み込む」でファイルを指定してください。"
        )
      )
    )

    tryCatch(
      prepare_sites(read_sites_csv(path)),
      error = function(e) {
        validate(need(FALSE, paste0("CSV の処理に失敗しました: ", conditionMessage(e))))
      }
    )
  })

  observe({
    b <- sites_bundle()
    ch <- c("（未選択）" = "", b$pick_choices)
    sel <- if (nrow(b$data) > 0L) "1" else ""
    updateSelectInput(session, "pick_site", choices = ch, selected = sel)
    last_tap(NULL)
  })

  focus_row <- function(i) {
    b <- isolate(sites_bundle())
    d <- b$data
    if (is.na(i) || i < 1L || i > nrow(d)) {
      return()
    }
    last_tap(d[i, , drop = FALSE])
    if (!isTRUE(d$has_geo[i])) {
      return()
    }
    zm <- 16L
    if (!is.null(input$map_zoom) && is.numeric(input$map_zoom)) {
      current_zoom <- suppressWarnings(as.integer(input$map_zoom))
      if (length(current_zoom) == 1L && !is.na(current_zoom) && is.finite(current_zoom)) {
        zm <- max(current_zoom, 16L)
      }
    }
    leafletProxy("map", session) %>%
      setView(lng = d$lng[i], lat = d$lat[i], zoom = zm)
    if (!identical(as.character(isolate(input$pick_site)), as.character(i))) {
      updateSelectInput(session, "pick_site", selected = as.character(i))
    }
  }

  output$map <- renderLeaflet({
    b <- sites_bundle()
    d <- b$data
    d_map <- d[d$has_geo, , drop = FALSE]
    pal <- b$pal
    leaflet(d_map, options = leafletOptions(preferCanvas = TRUE, tap = FALSE)) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~lng,
        lat = ~lat,
        layerId = ~as.character(marker_row_id),
        color = ~pal(type_plot),
        radius = 20,
        stroke = TRUE,
        weight = 3,
        opacity = 0.95,
        fillOpacity = 0.82,
        label = ~htmlEscape(ifelse(is.na(name) | trimws(as.character(name)) == "", "（無題）", as.character(name))),
        labelOptions = labelOptions(
          direction = "auto",
          textsize = "13px",
          opacity = 0.95,
          noHide = FALSE,
          sticky = FALSE
        )
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = d$type_plot,
        title = "種類",
        opacity = 0.95
      )
  })

  observeEvent(input$map_marker_click, {
    b <- sites_bundle()
    i <- resolve_marker_row(input$map_marker_click, b$data)
    if (is.na(i)) {
      return()
    }
    focus_row(i)
  })

  observeEvent(input$pick_site, {
    if (!nzchar(as.character(input$pick_site))) {
      last_tap(NULL)
      return()
    }
    i <- suppressWarnings(as.integer(input$pick_site))
    focus_row(i)
  })

  observeEvent(input$map_zoom, {
    if (is.null(last_tap()) || nrow(last_tap()) == 0L) {
      b <- tryCatch(sites_bundle(), error = function(e) NULL)
      if (!is.null(b) && nrow(b$data) > 0L) {
        focus_row(1L)
      }
    }
  }, ignoreNULL = TRUE, once = TRUE)

  output$tap_detail <- renderUI({
    sites_bundle()
    row <- last_tap()
    if (is.null(row) || nrow(row) == 0L) {
      return(
        tags$p(
          style = "color:#555;margin:0;",
          "一覧で遺跡を選ぶか、マーカーをタップすると、ここに遺跡名・種類・時代・説明が表示されます。"
        )
      )
    }
    r <- row[1, ]
    tags$div(
      tags$h4(style = "margin-top:0;", safe_txt(r$name, "（遺跡名なし）")),
      tags$p(tags$strong("種類: "), safe_txt(r$type)),
      tags$p(tags$strong("時代: "), safe_txt(r$period)),
      tags$p(tags$strong("緯度: "), format_coord(r$lat)),
      tags$p(tags$strong("経度: "), format_coord(r$lng)),
      tags$p(
        style = "white-space:pre-wrap;margin-bottom:0;",
        tags$strong("説明: "),
        safe_txt(r$desc, "（説明なし）")
      )
    )
  })
}

app <- shinyApp(ui, server)

# RStudio の Source/Run で app.R を直接実行した場合に起動する。
# runApp(appDir=...) 経由では自動起動しない（重複起動防止）。
is_runapp_loader <- function() {
  calls <- sys.calls()
  if (!length(calls)) {
    return(FALSE)
  }
  txt <- vapply(calls, function(x) paste(deparse(x), collapse = ""), character(1))
  any(grepl("shinyAppDir_appR|shinyAppDir\\(|runApp\\(", txt))
}

if (interactive() && !is_runapp_loader()) {
  shiny::runApp(app, launch.browser = TRUE)
}

app
