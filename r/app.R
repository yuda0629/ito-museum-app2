library(shiny)
library(leaflet)
library(dplyr)
library(readr)

# データ読み込み
csv_path <- file.path(dirname(getwd()), "data", "ito_sites_master.csv")
if (!file.exists(csv_path)) csv_path <- "ito_sites_master.csv"
sites_df <- tryCatch(
  read_csv(csv_path, show_col_types = FALSE),
  error = function(e) stop("CSV読み込み失敗: ", e)
)
colnames(sites_df) <- tolower(colnames(sites_df))

# 欠損値処理
sites_df <- sites_df %>%
  filter(!is.na(lat), !is.na(lng)) %>%
  mutate(marker_id = paste0("m", row_number()))

ui <- fluidPage(
  titlePanel("伊都国遺跡デジタルアーカイブ"),
  sidebarLayout(
    sidebarPanel(
      helpText("IT技術を活用した文化財の可視化サンプル"),
      checkboxGroupInput("periods", "時代選択:", 
                         choices = sort(unique(sites_df$period_clean)),
                         selected = unique(sites_df$period_clean))
    ),
    mainPanel(
      leafletOutput("map", height = "600px"),
      hr(),
      uiOutput("detail")
    )
  )
)

server <- function(input, output, session) {
  filtered_data <- reactive({
    req(input$periods)
    sites_df %>% filter(period_clean %in% input$periods)
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 130.225, lat = 33.570, zoom = 13)
  })
  
  observe({
    data <- filtered_data()
    leafletProxy("map", data = data) %>%
      clearMarkers() %>%
      addMarkers(lng = ~lng, lat = ~lat, layerId = ~marker_id, popup = ~name)
  })
  
  observeEvent(input$map_marker_click, {
    id <- input$map_marker_click$id
    site <- sites_df %>% filter(marker_id == id) %>% slice(1)
    output$detail <- renderUI({
      wellPanel(
        h4(site$name),
        p(paste("時代:", site$period)),
        p(paste("種別:", site$type)),
        p(site$desc)
      )
    })
  })
}

shinyApp(ui = ui, server = server)