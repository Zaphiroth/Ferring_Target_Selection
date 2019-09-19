# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Ferring BI Tool
# Purpose:      Shiny ui
# programmer:   Zhe Liu
# Date:         15-07-2019
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

##---- load packages ----
# require(DT)
# require(reshape2)
# require(plyr)
# require(data.table)
# library(shiny)
# library(stringi)
# library(stringr)
# library(dplyr)
# library(plotly)
# library(tidyr)
# library(lubridate)
# library(purrr)
# library(readxl)
# library(RcppRoll)
# library(openxlsx)
# library(shinydashboard)
# library(rlang)
# library(shinyjs)
# library(webshot)
# library(leaflet)
# library(leafletCN)
# library(shinyWidgets)

##---- ui ----
ui <- dashboardPage(
  dashboardHeader(title = "Target Selection Tool",
                  titleWidth = "250px"),
  
  dashboardSidebar(
    tags$head(includeCSS('./www/fix_siderbar.css')),
    collapsed = FALSE,
    fluidRow(
      tags$div(
        column(
          12,
          # tags$head(tags$style(".progress-bar{background-color:#00a65a;}")),
          fileInput("raw", label = "Upload Raw Data"),
          fileInput("dtbt", label = "Upload Distribution Data"),
          numericInput("kPotnCtrb", label = "Potential Cumulated Con. (%)", value = 0, min = 0, max = 100),
          selectInput("growth_share", label = "Growth Rate or Market Share", choices = c("Growth Rate", "Market Share"), multiple = FALSE),
          conditionalPanel(condition = "input.growth_share == 'Growth Rate'",
                           numericInput("kGrowth", label = "Grwoth Rate (%)", value = 0)),
          conditionalPanel(condition = "input.growth_share == 'Market Share'",
                           numericInput("kShare", label = "Market Share (%)", value = 0, min = 0, max = 100)),
          selectInput("sku", label = "Selection SKU", choices = "", multiple = TRUE),
          selectInput("aban", label = "Abandoned Provinces", choices = "", multiple = TRUE),
          br(),
          fluidRow(
            tags$div(
              column(6, tags$div(actionButton(inputId = "go", label = "Go", width = "80px", style = "color:#000;"),
                                 style = "display:inline-block; width:100%;")),
              column(6, tags$div(actionButton(inputId = "record", label = "Record", width = "80px", style = "color:#000;"),
                                 style = "display:inline-block; width:100%;"),
                     style = "padding: 0px;")
            )
          ),
          br(),
            tags$div(downloadButton(outputId = "DownloadSel", label = "Download", style = "width:150px; color:#000;"),
                     style = "display:inline-block; width:100%; text-align:center;")
        )
      )
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    
    tabsetPanel(
      tabPanel(
        strong("Selection"),
        value = "1",
        
        br(),
        fluidRow(
          box(
            title = "Concentration Curve",
            status = "primary",
            solidHeader = TRUE,
            collapsible = FALSE,
            width = 5,
            tags$div(plotlyOutput("Conc", height = "604px"))
          ),
          
          box(
            title = "Hospital Segmentation",
            status = "primary",
            solidHeader = TRUE,
            collapsible = FALSE,
            width = 7,
            column(
              1,
              tags$div(strong(textOutput("seg.h1")),
                       style = "text-align:center; margin-top:165px;",
                       class = "text-vertical"),
              tags$div(strong(textOutput("seg.h2")),
                       style = "text-align:center; margin-top:265px;",
                       class = "text-vertical")
            ),
            column(
              11,
              fluidRow(
                conditionalPanel("output.TableA == null",
                                 br()),
                tags$div(strong(textOutput("seg.v")),
                         style = "text-align:center;")
              ),
              br(),
              box(
                title = "C",
                status = "primary",
                solidHeader = TRUE,
                collapsible = FALSE,
                width = 6,
                style = options()$seg.style,
                tags$div(DT::dataTableOutput("TableC", height = "200px"),
                         style = "font-size:90%;")
              ),
              box(
                title = "A",
                status = "primary",
                solidHeader = TRUE,
                collapsible = FALSE,
                width = 6,
                style = options()$seg.style,
                tags$div(DT::dataTableOutput("TableA", height = "200px"),
                         style = "font-size:90%;")
              ),
              box(
                title = "D",
                status = "primary",
                solidHeader = TRUE,
                collapsible = FALSE,
                width = 6,
                style = options()$seg.style,
                tags$div(DT::dataTableOutput("TableD", height = "200px"),
                         style = "font-size:90%;")
              ),
              box(
                title = "B",
                status = "primary",
                solidHeader = TRUE,
                collapsible = FALSE,
                width = 6,
                style = options()$seg.style,
                tags$div(DT::dataTableOutput("TableB", height = "200px"),
                         style = "font-size:90%;")
              )
            )
          )
        ),
        
        fluidRow(
          box(
            title = "Actual Situation",
            status = "primary",
            solidHeader = TRUE,
            collapsible = FALSE,
            width = 12,
            fluidRow(
              column(6, tags$div(plotlyOutput("ActualPlot1"))),
              column(6,
                     tags$div(selectInput("actual", label = "Selection SKU", choices = "", multiple = TRUE)),
                     tags$div(plotlyOutput("ActualPlot2")))
            )
          )
        ),
        
        # br(),
        fluidRow(
          box(
            title = "Analysis of the results of hospital selection",
            status = "primary",
            solidHeader = TRUE,
            collapsible = FALSE,
            width = 12,
            fluidRow(
              box(
                solidHeader = TRUE,
                collapsible = FALSE,
                width = 12,
                tags$div(
                  column(3, numericInput("productivity", label = "Productivity lowest limit by year", value = 0, min = 0)),
                  # column(3, numericInput("roi", label = "ROI lowest limit by year (%)", value = 0, min = 0)),
                  column(3, numericInput("growth", label = "Growth rate lowest limit by year (%)", value = 0, min = 0)),
                  # column(6, selectInput("region", label = "Region", choices = c("All", "北区", "东区", "南区", "中区"), 
                  #                       selected = "All", multiple = TRUE)),
                  column(3, selectInput("kpi1", label = "KPI", 
                                        choices = c("Hospital#" = "hospital_num", "City#" = "city_num", "FTE#" = "fte"), 
                                        selected = "Hospital#", multiple = FALSE))
                ),
                style = "background:#C8E6FF;"
              )
            ),
            br(),
            fluidRow(
              box(
                solidHeader = TRUE,
                collapsible = FALSE,
                width = 12,
                tags$div(plotlyOutput("HospitalPlot")),
                tags$div(DT::dataTableOutput("HospitalTable"),
                         style = "font-size:90%; overflow-x:scroll;",
                         class = "nowrap")
              )
            ),
            br(),
            fluidRow(
              box(
                solidHeader = TRUE,
                collapsible = FALSE,
                width = 12,
                tags$div(plotlyOutput("SharePlot"))
              )
            )
            # br(),
            # fluidRow(
            #   box(
            #     solidHeader = TRUE,
            #     collapsible = FALSE,
            #     width = 12,
            #     tags$div(
            #       # column(3, numericInput("productivity", label = "Productivity lowest limit by year", value = 0, min = 0)),
            #       # column(3, numericInput("roi", label = "ROI lowest limit by year", value = 0, min = 0)),
            #       # column(3, numericInput("growth", label = "Product sales growth rate lowest limit by year", value = 0, min = 0)),
            #       # column(6, selectInput("region", label = "Region", choices = c("All", "北区", "东区", "南区", "中区"), 
            #       #                       selected = "All", multiple = TRUE)),
            #       column(9),
            #       column(3, selectInput("kpi2", label = "KPI", 
            #                             choices = c("Avg. Productivity" = "productivity", "ROI" = "roi"), 
            #                             selected = "Avg. Productivity", multiple = FALSE))
            #     ),
            #     style = "background:#C8E6FF;"
            #   )
            # ),
            # br(),
            # fluidRow(
            #   box(
            #     solidHeader = TRUE,
            #     collapsible = FALSE,
            #     width = 12,
            #     tags$div(plotlyOutput("IndexPlot", height = "250px")),
            #     tags$div(DT::dataTableOutput("IndexTable"),
            #              style = "font-size:90%; overflow-x:scroll;",
            #              class = "nowrap")
            #   )
            # )
          )
        )
      ),
      
      tabPanel(
        strong("Recommendation"),
        value = "2",
        
        br(),
        fluidRow(
          box(
            solidHeader = TRUE,
            collapsible = FALSE,
            width = 12,
            tags$div(
              column(3, selectInput("scenario", label = "Scenario", choices = c("Max ROI", "All Standing"), 
                                    selected = "Max Return", multiple = FALSE)),
              column(7),
              column(2,
                     br(),
                     tags$div(downloadButton(outputId = "DownloadRcmd", label = "Download", style = "width:100px; color:#000;"),
                              style = "display:inline-block; width:100%; text-align:center;"))
            ),
            style = "background:#C8E6FF;"
          )
        ),
        
        # br(),
        fluidRow(
          box(
            title = "Concentration Curve",
            status = "primary",
            solidHeader = TRUE,
            collapsible = FALSE,
            width = 5,
            tags$div(plotlyOutput("ConcRcmd", height = "604px"))
          ),
          
          box(
            title = "Hospital Segmentation",
            status = "primary",
            solidHeader = TRUE,
            collapsible = FALSE,
            width = 7,
            column(
              1,
              tags$div(strong(textOutput("segRcmd.h")),
                       style = "text-align:center; margin-top:300px;",
                       class = "text-vertical")
            ),
            column(
              11,
              fluidRow(
                tags$div(strong(textOutput("segRcmd.v")),
                         style = "text-align:center;")
              ),
              br(),
              box(
                title = "C",
                status = "primary",
                solidHeader = TRUE,
                collapsible = FALSE,
                width = 6,
                style = options()$seg.style,
                tags$div(DT::dataTableOutput("TableCRcmd", height = "200px"),
                         style = "font-size:90%;")
              ),
              box(
                title = "A",
                status = "primary",
                solidHeader = TRUE,
                collapsible = FALSE,
                width = 6,
                style = options()$seg.style,
                tags$div(DT::dataTableOutput("TableARcmd", height = "200px"),
                         style = "font-size:90%;")
              ),
              box(
                title = "D",
                status = "primary",
                solidHeader = TRUE,
                collapsible = FALSE,
                width = 6,
                style = options()$seg.style,
                tags$div(DT::dataTableOutput("TableDRcmd", height = "200px"),
                         style = "font-size:90%;")
              ),
              box(
                title = "B",
                status = "primary",
                solidHeader = TRUE,
                collapsible = FALSE,
                width = 6,
                style = options()$seg.style,
                tags$div(DT::dataTableOutput("TableBRcmd", height = "200px"),
                         style = "font-size:90%;")
              )
            )
          )
        ),
        
        # br(),
        fluidRow(
          box(
            title = "Analysis of the results of hospital selection",
            status = "primary",
            solidHeader = TRUE,
            collapsible = FALSE,
            width = 12,
            fluidRow(
              box(
                solidHeader = TRUE,
                collapsible = FALSE,
                width = 12,
                tags$div(
                  # column(3, numericInput("productivity", label = "Productivity lowest limit by year", value = 0, min = 0)),
                  # column(3, numericInput("roi", label = "ROI lowest limit by year", value = 0, min = 0)),
                  # column(3, numericInput("growth", label = "Product sales growth rate lowest limit by year", value = 0, min = 0)),
                  # column(6, selectInput("region", label = "Region", choices = c("All", "北区", "东区", "南区", "中区"), 
                  #                       selected = "All", multiple = TRUE)),
                  column(9),
                  column(3, selectInput("kpi1.rcmd", label = "KPI", 
                                        choices = c("Hospital#" = "hospital_num", "City#" = "city_num", "FTE#" = "fte"), 
                                        selected = "Hospital#", multiple = FALSE))
                ),
                style = "background:#C8E6FF;"
              )
            ),
            br(),
            fluidRow(
              box(
                solidHeader = TRUE,
                collapsible = FALSE,
                width = 12,
                tags$div(plotlyOutput("HospitalPlotRcmd", height = "250px")),
                tags$div(DT::dataTableOutput("HospitalTableRcmd"),
                         style = "font-size:90%; overflow-x:scroll;",
                         class = "nowrap")
              )
            ),
            # br(),
            fluidRow(
              box(
                solidHeader = TRUE,
                collapsible = FALSE,
                width = 12,
                tags$div(
                  # column(3, numericInput("productivity", label = "Productivity lowest limit by year", value = 0, min = 0)),
                  # column(3, numericInput("roi", label = "ROI lowest limit by year", value = 0, min = 0)),
                  # column(3, numericInput("growth", label = "Product sales growth rate lowest limit by year", value = 0, min = 0)),
                  # column(6, selectInput("region", label = "Region", choices = c("All", "北区", "东区", "南区", "中区"), 
                  #                       selected = "All", multiple = TRUE)),
                  column(9),
                  column(3, selectInput("kpi2.rcmd", label = "KPI", 
                                        choices = c("Avg. Productivity" = "productivity", "ROI" = "roi"), 
                                        selected = "Avg. Productivity", multiple = FALSE))
                ),
                style = "background:#C8E6FF;"
              )
            ),
            br(),
            fluidRow(
              box(
                solidHeader = TRUE,
                collapsible = FALSE,
                width = 12,
                tags$div(plotlyOutput("IndexPlotRcmd", height = "250px")),
                tags$div(DT::dataTableOutput("IndexTableRcmd"),
                         style = "font-size:90%; overflow-x:scroll;",
                         class = "nowrap")
              )
            )
          )
        )
        
      )
    )
  )
)

















