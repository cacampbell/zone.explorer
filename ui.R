#!/usr/bin/env Rscript

library(shiny)
library(shinydashboard)

ui <- dashboardPage(
                    dashboardHeader(title = "Zone Explorer"),
                    dashboardSidebar(
                                     sidebarMenuOutput("controls")
                                     ),
                    dashboardBody(
                                  includeCSS("www/custom.css"),
                                  conditionalPanel(
                                          condition = "$('html').hasClass('shiny-busy')",
                                          tags$div("Loading...", id = "loadmessage"),
                                          tags$br()
                                  ),
                                  uiOutput("main")
                                  ),
                    skin = "blue"
                    )
