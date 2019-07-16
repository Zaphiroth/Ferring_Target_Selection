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
        column(12, numericInput("productivity", label = "Productivity lowest limit by year", value = 0, min = 0)),
        column(12, numericInput("product", label = "Product sales lowest limit by year", value = 0, min = 0)),
        column(12, numericInput("roi", label = "ROI lowest limit by year", value = 0, min = 0)),
        column(12, selectInput("aban", label = "Abandoned Provinces", choices = "", selected = "", multiple = TRUE))
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
              plotlyOutput("conc", height = "500px")
            )
          ),
          
          br(),
          box(
            title = "Summary Table",
            status = "success",
            solidHeader = TRUE,
            collapsible = FALSE,
            width = 12,
            align = "center",
            tags$div(
              dataTableOutput("table", width = "800px")
            )
          ),
          
          br(),
          box(
            title = "By Province",
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
                  column(6, selectInput("region", label = "Region", choices = c("All", "北区", "东区", "南区", "中区"), 
                                        selected = "All", multiple = TRUE)),
                  column(6, selectInput("kpi", label = "KPI", choices = c("Hospital#", "City#", "FTE#", "Avg. Productivity", "ROI"), 
                                        selected = "Hospital#", multiple = FALSE))
                ),
                style = "background:#F0FFF0;"
              )
            ),
            fluidRow(
              box(
                solidHeader = TRUE,
                collapsible = FALSE,
                width = 12, 
                tags$div(
                  plotlyOutput("bar", height = "250px")
                )
              )
            )
          )
        )
      )
    )
  ),
  
  skin = "green"
)

















