library(dplyr)
library(shiny)
library(shinyWidgets)
library(shinyBS)
library(shinyjs)
# library(shinyTree)
library(sf)
library(leaflet)
library(leaflet.minicharts)
library(readxl)
library(openxlsx)

# preparing dataset
# RSlayanan <- rio::import("data/RSlayanan.xlsx",setclass = "tbl")
# RSlayanan <- RSlayanan %>%
#   tidyr::pivot_longer("Pelayanan medik dasar / umum":"Paliatif", names_to="Pelayanan", values_to="Jumlah")
# RSlayanan$Jumlah[is.na(RSlayanan$Jumlah)] <- 0
# rio::export(RSlayanan, "data/rslayanandata.rds")
# rslayanandata <- readRDS("data/rslayanandata.rds")

geocodes <- readRDS("data/geocodes1.rds")
bounds <- readRDS("data/bounds.rds")
bounds <- rbind(
  c("Nasional", -2.3071632, 117.2684654, -10.3599874813, 5.47982086834, 95.2930261576, 141.03385176),
  bounds
)

pelayananRefs <- readRDS("data/pelayananRefs.rds")
provinsiRefs <- readRDS("data/provinsiRefs.rds")
gruprs <- list("Grup Rumah Sakit"=c("Semua","RS Umum","RS Khusus"))
statusrs <- list("Status Rumah Sakit"=c("Semua","RS Publik","RS Privat"))
kelasrs <- list("Kelas Rumah Sakit"=c("Semua","A","B","C","D","D PRATAMA","Belum Ditetapkan"))
mypal <- c("#009392", "#39b185", "#9ccb86", "#e9e29c", "#eeb479", "#e88471", "#cf597e")

ui <- bootstrapPage(
  includeCSS("styles.css"),
  includeCSS("dask-style.css"),
  useShinyjs(),
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  dataTableOutput("table"),
  leafletOutput("map", width = "100%", height = "100%"),
  
  absolutePanel(
    id = "controls1",
    class = "panel panel-default",
    fixed = TRUE,
    top = 10,
    left = 10,
    width = 310,
    height = 400,
    draggable = TRUE,
    #selectInput("layanan", NULL, c("Pilih jenis pelayanan" = "", pelayananRefs), width = '400px'),
    
    selectInput("layanan", "Jenis Pelayanan", c("Pilih jenis pelayanan" = "", pelayananRefs)),#selected=pelayananRefs[[1]][1]),
    selectInput("prov", "Provinsi", c("Pilih provinsi" = "", provinsiRefs)),#,selected="Semua"),
    selectInput("gruprs", "Jenis Rumah Sakit", c("Pilih jenis rumah sakit" = "", gruprs)),#,selected="Semua"),
    selectInput("statusrs", "Status Rumah Sakit", c("Pilih status rumah sakit" = "", statusrs)),#,selected="Semua"),
    selectInput("kelasrs", "Kelas Rumah Sakit", c("Pilih kelas rumah sakit" = "", kelasrs)),#,selected="Semua"),
    
    # tags$head(
    #   tags$style(
    #     HTML( "#layanan ~ .selectize-control.single .selectize-input {border: 1px solid #fff;}")
    #   )
    # )
  ),
  absolutePanel(
    top = 20,
    right = 20,
    tags$a(img(src='dask-nov.png', align = "center"), href="http://kebijakankesehatanindonesia.net/datakesehatan/")
  )
)

server <- function(input, output, session) {
  # ouput map
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      htmlwidgets::onRender(
        "function(el, x) {
                    L.control.zoom({ position: 'bottomright' }).addTo(this)
                }"
      ) %>%
      addProviderTiles("CartoDB.Positron") %>%
      fitBounds(95.2930261576, -10.3599874813, 141.03385176, 5.47982086834)
  })
  
  observe({
    # di sini mau filter berkali-kali, tapi hasilnya error
    dataa <- reactive({
      RSlayanan %>%
        filter(Pelayanan %in% input$layanan)
    })
      # (Provinsi %in% input$prov) &
      #   (GroupRS %in% input$grouprs) &
      #   (StatusRS %in% input$statusrs) &
      #   (KelasRS %in% input$kelasrs))
    
    # untuk tes apakah tablenya sudah jadi/belum
    output$table <- renderDataTable(dataa())
    
    data.spatial <- geocodes %>%
      right_join(dataa(), by = c("Provinsi" = "Provinsi")) %>%
      select(Provinsi, Jumlah, lon, lat, geometry)
    
    proxy <- leafletProxy("map", session, data = data.spatial) %>% clearControls() %>% clearShapes()
    proxy
    pal <- colorNumeric(
      palette = mypal,
      domain = data.spatial$Jumlah, reverse = FALSE)
    
    # proxy %>% # Error in as.character: cannot coerce type 'closure' to vector of type 'character'
    #   addPolygons(
    #     fillColor = ~pal(Jumlah),
    #     weight = 1,
    #     opacity = 1,
    #     color = "white",
    #     dashArray = "3",
    #     fillOpacity = 0.7,
    #     highlight = highlightOptions(
    #       weight = 2,
    #       color = "#666",
    #       dashArray = "",
    #       fillOpacity = 0.7,
    #       bringToFront = FALSE),
    #     label = labels,
    #     labelOptions = labelOptions(
    #       style = list("font-weight" = "normal", padding = "3px 8px"),
    #       textsize = "15px",
    #       direction = "auto")
    #   ) %>%
    #   addLegend(position = "bottomright",
    #             pal = pal, values = ~Jumlah)
  })
}

shinyApp(ui, server)
