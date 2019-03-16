#!/usr/bin/env Rscript

source("ui.R", local = TRUE)
source("server.R", local = TRUE)

shinyApp(
    ui = ui,
    server = server,
    options = options(
        shiny.host = "172.20.24.71",
        shiny.port = 9002,
        shiny.trace = FALSE,
        shiny.reactlog = FALSE,
        shiny.sanitize.errors = TRUE,
        shiny.deprecation.messages = FALSE
    )
)
