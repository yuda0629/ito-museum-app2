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
  bad_geo <- is.na(data$lng) | is.na(data$lat)
  if (any(bad_geo)) {
    warning(sum(bad_geo), " 行の緯度・経度が数値化できませんでした（地図から除外します）")
    data <- data[!bad_geo, , drop = FALSE]
  }
  if (!nrow(data)) {
    stop("有効な緯度・経度がある行がありません")
  }

  row.names(data) <- NULL

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

# 時代列を表示・フィルタ用に正規化（空は同一ラベルにまとめる）
normalize_period <- function(x) {
  x <- trimws(as.character(x))
  x[is.na(x) | x == ""] <- "(時代不明)"
  x
}

# フィルタ後のデータ用に種類色パレットを組み立てる
palette_for_types <- function(type_plot_factor) {
  tp <- droplevels(as.factor(type_plot_factor))
  n_types <- nlevels(tp)
  if (n_types < 1L) {
    return(NULL)
  }
  colorFactor(
    palette = colorRampPalette(
      brewer.pal(min(9, max(3, n_types)), "Set1")
    )(n_types),
    domain = tp
  )
}

ui <- fluidPage(
  tags$head(
    tags$style(
      HTML(
        ".leaflet-popup-content-wrapper { max-width: min(360px, 92vw) !important; }
        .leaflet-popup-content { margin: 10px 12px !important; max-height: 50vh; overflow: auto; }
        .leaflet-container { background: #e8e8e8; }"
      )
    )
  ),
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
      "period_filter",
      "表示する時代（複数選択可）",
      choices = NULL,
      multiple = TRUE,
      selectize = FALSE,
      size = 10,
      width = "100%"
    ),
    tags$p(
      style = "color:#666;font-size:0.85em;margin:-4px 0 4px 0;",
      "CSV の ", tags$code("時代"), " / ", tags$code("年代"), " / ", tags$code("period"),
      " 列の値ごとに分類します。空欄は「(時代不明)」にまとめます。"
    ),
    tags$p(
      style = "color:#666;font-size:0.85em;margin:0 0 10px 0;",
      "複数の時代を選ぶ: ",
      tags$strong("Ctrl"),
      "（Mac は ",
      tags$strong("Command"),
      "）を押しながら項目をクリック。",
      tags$strong("全解除"),
      "すると地図上のマーカーは 0 件になります。"
    ),
    verbatimTextOutput("period_summary", placeholder = TRUE),
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
  # 前回「時代」選択肢を更新したときの署名（CSV や内容が変わったときだけ UI を更新し、ユーザーの絞り込みをリセットしない）
  period_choices_sig <- reactiveVal("")

  active_csv_path <- reactive({
    path_upload <- NULL
    up <- input$user_csv
    if (!is.null(up) && is.data.frame(up) && nrow(up) >= 1L) {
      p <- up$datapath[1]
      if (length(p) && nzchar(p) && file.exists(p)) {
        path_upload <- p
      }
    }
    path_upload %||% find_default_csv()
  })

  sites_bundle <- reactive({
    path <- active_csv_path()
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

  observe(
    {
      path <- active_csv_path()
      if (is.na(path) || !nzchar(path) || !file.exists(path)) {
        return()
      }
      b <- sites_bundle()
      pnorm <- normalize_period(b$data$period)
      u <- sort(unique(pnorm))
      mt <- suppressWarnings(as.numeric(file.info(path)$mtime))
      if (!is.finite(mt)) {
        mt <- 0
      }
      sig <- paste(
        normalizePath(path, winslash = "/", mustWork = FALSE),
        mt,
        nrow(b$data),
        paste(u, collapse = "\001"),
        sep = "\t"
      )
      if (identical(sig, isolate(period_choices_sig()))) {
        return()
      }
      period_choices_sig(sig)
      updateSelectInput(
        session,
        "period_filter",
        choices = stats::setNames(u, u),
        selected = u
      )
    },
    priority = 1
  )

  filtered_row_indices <- reactive({
    b <- sites_bundle()
    d <- b$data
    pnorm <- normalize_period(d$period)
    sel <- input$period_filter
    # 初期は NULL → 全件（選択肢更新前の一瞬）
    if (is.null(sel)) {
      return(seq_len(nrow(d)))
    }
    # 複数選択で 0 件なら「何も表示しない」（以前は全件になってしまい不具合に見えていた）
    if (length(sel) == 0L) {
      return(integer(0))
    }
    sel <- trimws(as.character(sel))
    which(pnorm %in% sel)
  })

  display_bundle <- reactive({
    b <- sites_bundle()
    idx <- filtered_row_indices()
    d <- b$data[idx, , drop = FALSE]
    if (!nrow(d)) {
      ctr_lat <- mean(b$data$lat, na.rm = TRUE)
      ctr_lng <- mean(b$data$lng, na.rm = TRUE)
      return(list(
        data = d,
        pal = NULL,
        pick_choices = character(0),
        center_lat = ctr_lat,
        center_lng = ctr_lng,
        zoom = 11L
      ))
    }
    d$type_plot <- droplevels(d$type_plot)
    pal <- palette_for_types(d$type_plot)
    pick_choices <- stats::setNames(
      as.character(d$marker_row_id),
      paste0(seq_len(nrow(d)), ". ", as.character(d$name))
    )
    list(
      data = d,
      pal = pal,
      pick_choices = pick_choices,
      center_lat = mean(d$lat, na.rm = TRUE),
      center_lng = mean(d$lng, na.rm = TRUE),
      zoom = 11L
    )
  })

  output$period_summary <- renderText({
    b <- sites_bundle()
    pnorm <- normalize_period(b$data$period)
    tb <- sort(table(pnorm), decreasing = TRUE)
    paste0(
      "時代別件数（全 ", nrow(b$data), " 件）: ",
      paste0(names(tb), " ", as.integer(tb), "件", collapse = " / ")
    )
  })

  observe({
    db <- display_bundle()
    ch <- c("（未選択）" = "", db$pick_choices)
    sel <- if (length(db$pick_choices)) as.character(db$pick_choices[[1]]) else ""
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
    zm <- 16L
    if (!is.null(input$map_zoom) && is.numeric(input$map_zoom)) {
      zm <- max(as.integer(input$map_zoom), 16L)
    }
    leafletProxy("map", session) %>%
      setView(lng = d$lng[i], lat = d$lat[i], zoom = zm)
    updateSelectInput(session, "pick_site", selected = as.character(i))
  }

  output$map <- renderLeaflet({
    db <- display_bundle()
    d <- db$data
    pal <- db$pal
    # preferCanvas=FALSE で一部環境でのマーカー操作時の黒画面化を避ける
    # maxZoom 26 までズーム可能（OSM 等は高倍率でタイルを拡大表示。実解像度は maxNativeZoom 付近まで）
    m <- leaflet(
      options = leafletOptions(
        preferCanvas = FALSE,
        tap = FALSE,
        minZoom = 3L,
        maxZoom = 26L
      )
    ) %>%
      addTiles(options = tileOptions(maxZoom = 26L, maxNativeZoom = 19L)) %>%
      setView(lng = db$center_lng, lat = db$center_lat, zoom = db$zoom)
    if (!nrow(d) || is.null(pal)) {
      return(m)
    }
    tp_lvls <- levels(droplevels(d$type_plot))
    m %>%
      addCircleMarkers(
        data = d,
        lng = ~lng,
        lat = ~lat,
        layerId = ~as.character(marker_row_id),
        color = ~pal(type_plot),
        radius = 20,
        stroke = TRUE,
        weight = 3,
        opacity = 0.95,
        fillOpacity = 0.82,
        # リッチ HTML ポップアップは描画負荷が高くクラッシュしやすいため要約テキストに
        popup = ~paste(
          ifelse(is.na(name) | trimws(as.character(name)) == "", "（無題）", as.character(name)),
          paste0("種類: ", ifelse(is.na(type) | trimws(as.character(type)) == "", "—", as.character(type))),
          paste0("時代: ", ifelse(is.na(period) | trimws(as.character(period)) == "", "—", as.character(period))),
          sprintf("緯度: %.6f", lat),
          sprintf("経度: %.6f", lng),
          "（詳細は左のパネル）",
          sep = "\n"
        ),
        popupOptions = popupOptions(
          maxWidth = 300,
          minWidth = 180,
          autoPan = TRUE,
          keepInView = TRUE,
          autoPanPadding = c(40, 40),
          closeButton = TRUE
        ),
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
        values = tp_lvls,
        title = "種類（色）",
        opacity = 1
      )
  })

  # 一覧またはマーカーで選んだ地点を、地図上で目立つハイライト（最前面に重ねる）
  observe({
    db <- tryCatch(display_bundle(), error = function(e) NULL)
    row <- last_tap()
    proxy <- leafletProxy("map", session)
    proxy %>% clearGroup("picked_highlight")
    if (is.null(db) || !nrow(db$data)) {
      return()
    }
    if (is.null(row) || nrow(row) == 0L) {
      return()
    }
    rid <- suppressWarnings(as.integer(row$marker_row_id[1]))
    if (is.na(rid) || !rid %in% db$data$marker_row_id) {
      return()
    }
    r <- row[1, ]
    lng <- as.numeric(r$lng)
    lat <- as.numeric(r$lat)
    if (!is.finite(lng) || !is.finite(lat)) {
      return()
    }
    proxy %>%
      addCircleMarkers(
        lng = lng,
        lat = lat,
        radius = 36,
        stroke = TRUE,
        color = "#e65100",
        weight = 5,
        fillColor = "#000000",
        fillOpacity = 0,
        group = "picked_highlight",
        layerId = "picked_ring_outer"
      ) %>%
      addCircleMarkers(
        lng = lng,
        lat = lat,
        radius = 26,
        stroke = TRUE,
        color = "#ffeb3b",
        weight = 4,
        fillColor = "#fff59d",
        fillOpacity = 0.45,
        group = "picked_highlight",
        layerId = "picked_ring_inner"
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
      db <- tryCatch(display_bundle(), error = function(e) NULL)
      if (!is.null(db) && nrow(db$data) > 0L) {
        i <- suppressWarnings(as.integer(db$data$marker_row_id[1]))
        if (!is.na(i)) {
          focus_row(i)
        }
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
          "一覧で遺跡を選ぶか、マーカーをクリックすると、ここに遺跡名・種類・時代・説明・緯度経度が表示されます（ポップアップは要約のみ）。"
        )
      )
    }
    r <- row[1, ]
    tags$div(
      tags$h4(style = "margin-top:0;", safe_txt(r$name, "（遺跡名なし）")),
      tags$p(tags$strong("種類: "), safe_txt(r$type)),
      tags$p(tags$strong("時代: "), safe_txt(r$period)),
      tags$p(
        style = "white-space:pre-wrap;margin-bottom:0;",
        tags$strong("説明: "),
        safe_txt(r$desc, "（説明なし）")
      ),
      tags$p(
        style = "color:#666;font-size:0.95em;margin-bottom:0;",
        tags$strong("緯度: "),
        sprintf("%.6f", as.numeric(r$lat)),
        "　",
        tags$strong("経度: "),
        sprintf("%.6f", as.numeric(r$lng))
      )
    )
  })
}

shinyApp(ui, server)