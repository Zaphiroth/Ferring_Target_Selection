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
          numericInput("kPotnCtrb", label = "Potential Cumulated Con. (%)", value = NULL, min = 0, max = 100),
          selectInput("growth_share", label = "Growth Rate or Market Share", choices = c("Growth Rate", "Market Share"), multiple = FALSE),
          conditionalPanel(condition = "input.growth_share == 'Growth Rate'",
                           numericInput("kGrowth", label = "Grwoth Rate (%)", value = NULL)),
          conditionalPanel(condition = "input.growth_share == 'Market Share'",
                           numericInput("kShare", label = "Market Share (%)", value = NULL, min = 0, max = 100)),
          selectInput("sku", label = "Selection SKU", choices = "", multiple = TRUE),
          selectInput("aban", label = "Abandoned Provinces", choices = "", multiple = TRUE),
          br(),
          fluidRow(
            tags$div(column(4, actionButton("go", "Go", width = "60px", style = "color:#000;")),
                     style = "display:inline-block;margin-down: 1px;vertical-align:middle"),
            tags$div(column(8, downloadButton(outputId = "DownloadSel", label = "Download", style = "color:#000;")),
                     style = "display:inline-block;margin-down: 1px;vertical-align:middle")
          ),
          br(),
          actionButton("record1", "Record Scenario 1", width = "200px"),
          actionButton("record2", "Record Scenario 2", width = "200px")
        )
      )
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    
    br(),
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
            tags$div(plotlyOutput("Conc", height = "638px"))
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
                       style = "text-align:center; margin-top:200px;",
                       class = "text-vertical"),
              tags$div(strong(textOutput("seg.h2")),
                       style = "text-align:center; margin-top:260px;",
                       class = "text-vertical")
            ),
            column(
              11,
              fluidRow(
                tags$div(downloadButton("DownloadSeg", label = "Download", style = "width:100px; color:#000;"),
                         style = "text-align:right;"),
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
              column(6,
                     br(),
                     br(),
                     tags$div(plotlyOutput("ActualPlot1", height = "auto"))),
              column(6,
                     tags$div(selectInput("actual.sku", label = "Selection SKU", choices = "", multiple = TRUE)),
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
                  column(3, numericInput("productivity", label = "Productivity lowest limit by year", value = NULL)),
                  # column(3, numericInput("roi", label = "ROI lowest limit by year (%)", value = 0, min = 0)),
                  column(3, numericInput("growth", label = "Growth rate lowest limit by year (%)", value = NULL)),
                  # column(6, selectInput("region", label = "Region", choices = c("All", "北区", "东区", "南区", "中区"), 
                  #                       selected = "All", multiple = TRUE)),
                  column(3, selectInput("kpi1", label = "KPI", 
                                        choices = c("Hospital#" = "hospital_num", "City#" = "city_num", "FTE#" = "fte"), 
                                        selected = "Hospital#", multiple = FALSE)),
                  column(1),
                  column(2,
                         br(),
                         tags$div(downloadButton(outputId = "DownloadProv", label = "Download", style = "width:100px; color:#000;"),
                                  style = "display:inline-block; width:100%; text-align:center;"))
                ),
                style = "background:#C8E6FF;"
              )
            ),
            # br(),
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
                tags$div(
                  column(3, selectInput("covered.sku", label = "Selection SKU", choices = "", multiple = TRUE)),
                  column(7),
                  column(2,
                         br(),
                         tags$div(downloadButton(outputId = "DownloadHospital", label = "Download", style = "width:100px; color:#000;"),
                                  style = "display:inline-block; width:100%; text-align:center;"))
                ),
                style = "background:#C8E6FF;"
              )
            ),
            # br(),
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
              tags$div(strong(textOutput("segRcmd.h1")),
                       style = "text-align:center; margin-top:200px;",
                       class = "text-vertical"),
              tags$div(strong(textOutput("segRcmd.h2")),
                       style = "text-align:center; margin-top:260px;",
                       class = "text-vertical")
              # tags$div(strong(textOutput("segRcmd.h")),
              #          style = "text-align:center; margin-top:300px;",
              #          class = "text-vertical")
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
        
      ),
      
      tabPanel(
        strong("Dimension"),
        value = "3",
        
        br(),
        fluidRow(
          box(
            solidHeader = TRUE,
            collapsible = FALSE,
            width = 12,
            tags$div(
              column(6, selectInput("dimension", label = "Dimension", 
                                    choices = c("SKU" = "sku", "Province" = "province", "City" = "city", "City tier" = "tier", "Hospital level" = "hosp_level"),
                                    selected = c("sku", "province", "city", "tier", "hosp_level"), multiple = TRUE)),
              column(4),
              column(2, br(), 
                     tags$div(downloadButton("DownloadDimension", label = "Download", style = "width:100px; color:#000;"),
                              style = "display:inline-block; width:100%; text-align:center;"))
            ),
            style = "background:#C8E6FF;"
          ),
          
          br(),
          box(
            solidHeader = TRUE,
            collapsible = FALSE,
            width = 12,
            tags$div(DT::dataTableOutput("DimensionTable"),
                     style = "font-size:90%; overflow-x:scroll;",
                     class = "nowrap")
          )
        )
      ),
      
      tabPanel(
        strong("Evaluation"),
        value = "4",
        
        br(),
        fluidRow(
          column(10),
          column(2,
                 tags$div(downloadButton("DownloadEvaluation", label = "Download", style = "width:100px; color:#000;"),
                          style = "display:inline-block; width:100%; text-align:center;"))
        ),
        br(),
        fluidRow(
          box(
            solidHeader = TRUE,
            collapsible = FALSE,
            width = 12,
            tags$div(DT::dataTableOutput("EvaluationTable"),
                     style = "font-size:90%; overflow-x:scroll;",
                     class = "nowrap")
          )
        )
      )
    )
  )
)

















