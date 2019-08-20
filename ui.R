# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Ferring BI Tool
# Purpose:      Shiny ui
# programmer:   Zhe Liu
# Date:         15-07-2019
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

##---- load packages ----
require(DT)
require(reshape2)
require(plyr)
require(data.table)
library(shiny)
library(stringi)
library(stringr)
library(dplyr)
library(plotly)
library(tidyr)
library(lubridate)
library(purrr)
library(readxl)
library(RcppRoll)
library(openxlsx)
library(shinydashboard)
library(rlang)
library(shinyjs)
library(webshot)
library(leaflet)
library(leafletCN)
library(shinyWidgets)

##---- ui ----
ui <- dashboardPage(
  dashboardHeader(title = "Target Selection Tool",
                  titleWidth = "250px"),
  
  dashboardSidebar(
    width = "250px",
    collapsed = FALSE,
    fluidRow(
      tags$div(
        column(12, 
               # tags$head(tags$style(".progress-bar{background-color:#00a65a;}")),
               fileInput("raw", label = "Upload Raw Data")),
        column(12, numericInput("potn.ctrb", label = "Input Potential Cumulated con (%)", value = 90, min = 0, max = 100)),
        column(12, selectInput("sku", label = "Selection SKU", choices = c("Gly", "Pentasa TAB", "Pentase SUP"), 
                               multiple = TRUE, selected = c("Gly", "Pentasa TAB", "Pentase SUP"))),
        column(12, selectInput("aban", label = "Abandoned Provinces", choices = "", selected = "", multiple = TRUE))
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
            tags$div(plotlyOutput("conc", height = "500px"))
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
                tags$div(dataTableOutput("C", height = "250px")),
                tags$div(dataTableOutput("A", height = "250px"))
              )
            ),
            column(
              6, 
              fluidRow(
                tags$div(dataTableOutput("D", height = "250px")),
                tags$div(dataTableOutput("B", height = "250px"))
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
                  column(3, numericInput("roi", label = "ROI lowest limit by year", value = 0, min = 0)),
                  column(3, numericInput("growth", label = "Product sales growth rate lowest limit by year", value = 0, min = 0)),
                  # column(6, selectInput("region", label = "Region", choices = c("All", "北区", "东区", "南区", "中区"), 
                  #                       selected = "All", multiple = TRUE)),
                  column(3, selectInput("kpi1", label = "KPI", choices = c("Hospital#", "City#", "FTE#"), 
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
                tags$div(
                  plotlyOutput("hospital_plot", height = "250px")
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
                  column(3, selectInput("kpi2", label = "KPI", choices = c("Avg. Productivity", "ROI"), 
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
                tags$div(
                  plotlyOutput("index_plot", height = "250px")
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
            column(3, selectInput("scenario", label = "Scenario", choices = c("Max Return", "Max Productivity", "All Standing"), 
                                  selected = "Max Return", multiple = FALSE)),
            column(9)
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
            tags$div(plotlyOutput("conc_rcmd", height = "500px"))
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
                tags$div(dataTableOutput("C_rcmd", height = "250px")),
                tags$div(dataTableOutput("A_rcmd", height = "250px"))
              )
            ),
            column(
              6, 
              fluidRow(
                tags$div(dataTableOutput("D_rcmd", height = "250px")),
                tags$div(dataTableOutput("B_rcmd", height = "250px"))
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
                  column(3, selectInput("kpi1_rcmd", label = "KPI", choices = c("Hospital#", "City#", "FTE#"), 
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
                tags$div(
                  plotlyOutput("hospital_plot_rcmd", height = "250px")
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
                  column(3, selectInput("kpi2_rcmd", label = "KPI", choices = c("Avg. Productivity", "ROI"), 
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
                tags$div(
                  plotlyOutput("index_plot_rcmd", height = "250px")
                )
              )
            )
          )
        )
      )
    )
  )
)

















