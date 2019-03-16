#!/usr/bin/env Rscript
library(DT)
library(leaflet)

server <- function(input, output, session) {
    source("functional.R", local = TRUE)
    source("reactive.R", local=TRUE)

    output$addressAddControls <- renderUI(
                                          {
                                              sidebarSearchForm(textId = "address",
                                                                buttonId = "addAddress",
                                                                label = "Add Address...",
                                                                icon = icon("plus"))
                                          }
    )

    output$addressSelectControls <- renderUI(
                                             {
                                                 addresses <- get.addresses.names()
                                                 selectizeInput("address.selected",
                                                                "Select Address",
                                                                selected = NULL,
                                                                multiple = FALSE,
                                                                addresses,
                                                                options = list(
                                                                               placeholder = 'Select Address',
                                                                               onInitialize = I('function() { this.setValue(""); }')
                                                                               )
                                                                )
                                             }
    )

    output$distanceZoneControls <- renderUI({
        tagList(
                numericInput("dist.A", "A", value = 25, min = 0),
                numericInput("dist.B", "B", value = 50, min = 0),
                numericInput("dist.C", "C", value = 75, min = 0),
                numericInput("dist.D", "D", value = 100, min = 0)
                )
    })

    output$timeZoneControls <- renderUI({
        tagList(
                numericInput("time.A", "A", value = 20, min = 0),
                numericInput("time.B", "B", value = 40, min = 0),
                numericInput("time.C", "C", value = 60, min = 0),
                numericInput("time.D", "D", value = 120, min = 0)
                )
    })

    output$controls <- renderMenu({
        sidebarMenu(
                    uiOutput("addressAddControls"),
                    uiOutput("addressSelectControls"),
                    tags$hr(),
                    tags$h4("Zone Ends (minutes)", align = "center"),
                    uiOutput("timeZoneControls"),
                    tags$hr(),
                    tags$h4("Zone Ends (miles)", align = "center"),
                    uiOutput("distanceZoneControls")
                    )
    })

    output$zonesTable <- renderDataTable({ zones.table() })

    output$zoneBox <- renderUI({
                         box(
                             title = "Zones (miles)",
                             dataTableOutput("zonesTable"))
    })

    output$map <- renderLeaflet({ map.zips() })
    output$downloadMap <- downloadHandler(
                                          filename = "map.pdf",
                                          content = function(file) {
                                              require(mapview)
                                              require(webshot)
                                              owd <- setwd(tempdir())
                                              on.exit(setwd(owd))
                                              saveWidget(map.zips(), "temp.html", selfcontained = FALSE)
                                              webshot("temp.html", file = file, cliprect = "viewport")
                                          })
    output$download <- renderUI({
        map <- map.zips()
        if (!is.null(map)) {
            downloadButton('downloadMap', 'Download Map')
        } else {
            tags$br()
        }
    })

    output$timeBox <- renderUI({
        box(
            title = "Drive Times (minutes)",
            leafletOutput("map"),
            uiOutput("download")
            )
    })

    output$main <- renderUI({
                         fluidRow(
                             column(12,
                                    align = "center",
                                    uiOutput("timeBox")),
                             column(12,
                                    align = "center",
                                    uiOutput("zoneBox"))
                             )
    })
}
