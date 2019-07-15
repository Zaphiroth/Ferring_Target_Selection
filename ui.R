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
  dashboardHeader(title = "Target Selection Tool"),
  
  dashboardSidebar(
    collapsed = FALSE,
    fluidRow(
      tags$div(
        column(12, fileInput("raw", label = "Upload Raw Data")),
        column(12, numericInput("potn.ctrb", label = "Input Potential Cumulated con%", value = 100, min = 0, max = 100)),
        column(12, selectInput("aban", label = "Abandoned Provinces", choices = "", selected = "", multiple = TRUE)),
        column(12, numericInput("productivity", label = "Productivity lowest limit by year", value = 0, min = 0)),
        column(12, numericInput("product", label = "Product sales lowest limit by year", value = 0, min = 0)),
        column(12, numericInput("roi", label = "ROI lowest limit by year", value = 0, min = 0))
      )
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    
    tabsetPanel(
      tabPanel(
        strong("Dashboard"),
        value = "1",
        
        br(),
        fluidRow(
          box(
            title = "Concentration Curve",
            status = "success",
            solidHeader = TRUE,
            collapsible = FALSE,
            width = 12,
            tags$div(
              plotlyOutput("conc", height = "auto")
            ),
            tags$div(
              dataTableOutput("table")
            )
          ),
          
          box(
            title = "By province",
            status = "success",
            solidHeader = TRUE,
            collapsible = FALSE,
            width = 12,
            fluidRow(
              box(
                solidHeader = TRUE,
                collapsible = FALSE,
                width = 12,
                tags$div(
                  column(3, selectInput("region", label = "Region", choices = "", selected = "", multiple = TRUE)),
                  column(3, selectInput("kpi", label = "KPI", choices = "", selected = "", multiple = FALSE))
                )
              ),
              tags$div(
                plotlyOutput("bar", height = "auto")
              )
            )
          )
        )
      )
    )
  ),
  
  skin = "green"
)

















