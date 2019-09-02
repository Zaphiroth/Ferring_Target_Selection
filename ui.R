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
          numericInput("kPotnCtrb", label = "Input Potential Cumulated con (%)", value = 90, min = 0, max = 100),
          selectInput("sku", label = "Selection SKU", choices = c("Gly", "Pentasa TAB", "Pentase SUP"), 
                      multiple = TRUE, selected = c("Gly", "Pentasa TAB", "Pentase SUP")),
          selectInput("aban", label = "Abandoned Provinces", choices = "", selected = "", multiple = TRUE),
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
            width = 6,
            tags$div(plotlyOutput("Conc", height = "565px"))
          ),
          
          box(
            title = "Hospital Segmentation",
            status = "primary",
            solidHeader = TRUE,
            collapsible = FALSE,
            width = 6,
            column(
              6, 
              fluidRow(
                box(
                  title = "C",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = FALSE,
                  width = 12,
                  style = options()$seg.style,
                  tags$div(DT::dataTableOutput("TableC"),
                           style = "font-size:90%;")
                ),
                box(
                  title = "D",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = FALSE,
                  width = 12,
                  style = options()$seg.style,
                  tags$div(DT::dataTableOutput("TableD"),
                           style = "font-size:90%;")
                )
              )
            ),
            column(
              6, 
              fluidRow(
                box(
                  title = "A",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = FALSE,
                  width = 12,
                  style = options()$seg.style,
                  tags$div(DT::dataTableOutput("TableA"),
                           style = "font-size:90%;")
                ),
                box(
                  title = "B",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = FALSE,
                  width = 12,
                  style = options()$seg.style,
                  tags$div(DT::dataTableOutput("TableB"),
                           style = "font-size:90%;")
                )
              )
            )
          ),
          
          br(),
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
                  column(3, numericInput("roi", label = "ROI lowest limit by year (%)", value = 0, min = 0)),
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
            fluidRow(
              box(
                solidHeader = TRUE,
                collapsible = FALSE,
                width = 12, 
                tags$div(plotlyOutput("HospitalPlot", height = "300px")),
                tags$div(DT::dataTableOutput("HospitalTable"),
                         style = "font-size:90%; overflow-x:scroll;")
              )
            ),
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
                  column(3, selectInput("kpi2", label = "KPI", 
                                        choices = c("Avg. Productivity" = "productivity", "ROI" = "roi"), 
                                        selected = "Avg. Productivity", multiple = FALSE))
                ),
                style = "background:#C8E6FF;"
              )
            ),
            fluidRow(
              box(
                solidHeader = TRUE,
                collapsible = FALSE,
                width = 12, 
                column(
                  12,
                  tags$div(plotlyOutput("IndexPlot", height = "250px"))
                ),
                column(
                  12,
                  tags$div(DT::dataTableOutput("IndexTable"),
                           style = "font-size:90%; overflow-x:scroll;")
                )
              )
            )
          )
        )
      ),
      
      tabPanel(
        strong("Recommendation"),
        value = "2",
        
        br(),
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
        ),
        
        br(),
        fluidRow(
          box(
            title = "Concentration Curve",
            status = "primary",
            solidHeader = TRUE,
            collapsible = FALSE,
            width = 6,
            tags$div(plotlyOutput("ConcRcmd", height = "565px"))
          ),
          
          box(
            title = "Hospital Segmentation",
            status = "primary",
            solidHeader = TRUE,
            collapsible = FALSE,
            width = 6,
            column(
              6, 
              fluidRow(
                box(
                  title = "C",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = FALSE,
                  width = 12,
                  style = options()$seg.style,
                  tags$div(DT::dataTableOutput("TableCRcmd"),
                           style = "font-size:90%;")
                ),
                box(
                  title = "D",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = FALSE,
                  width = 12,
                  style = options()$seg.style,
                  tags$div(DT::dataTableOutput("TableDRcmd"),
                           style = "font-size:90%;")
                )
              )
            ),
            column(
              6, 
              fluidRow(
                box(
                  title = "A",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = FALSE,
                  width = 12,
                  style = options()$seg.style,
                  tags$div(DT::dataTableOutput("TableARcmd"),
                           style = "font-size:90%;")
                ),
                box(
                  title = "B",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = FALSE,
                  width = 12,
                  style = options()$seg.style,
                  tags$div(DT::dataTableOutput("TableBRcmd"),
                           style = "font-size:90%;")
                )
              )
            )
          ),
          
          br(),
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
            fluidRow(
              box(
                solidHeader = TRUE,
                collapsible = FALSE,
                width = 12, 
                column(
                  12,
                  tags$div(plotlyOutput("HospitalPlotRcmd", height = "250px"))
                ),
                column(
                  12,
                  tags$div(DT::dataTableOutput("HospitalTableRcmd"),
                           style = "font-size:90%; overflow-x:scroll;")
                )
              )
            ),
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
            fluidRow(
              box(
                solidHeader = TRUE,
                collapsible = FALSE,
                width = 12, 
                column(
                  12,
                  tags$div(plotlyOutput("IndexPlotRcmd", height = "250px"))
                ),
                column(
                  12,
                  tags$div(DT::dataTableOutput("IndexTableRcmd"),
                           style = "font-size:90%; overflow-x:scroll;")
                )
              )
            )
          )
        )
      )
    )
  )
)

















