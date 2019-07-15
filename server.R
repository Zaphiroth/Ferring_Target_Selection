# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Ferring BI Tool
# Purpose:      Shiny server
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

##---- load functions ----
source("./functions/concentration.R", encoding = "UTF-8")

##---- server ----
server <- function(input, output, session) {
  raw <- reactive({
    if (is.null(input$raw))
      return(NULL)
    
    inFile.raw <- input$raw
    raw <- read_xlsx(
      inFile.raw$datapath,
      na = "NA"
    ) %>% 
      setDF() %>% 
      select(`PHA code`, TERMINALCODE, Province, `Potential-2019(MAT201904)`, 
             大区, `MAT201904销售指标（RMB）`, FTE)
    colnames(raw) <- c("code1", "code2", "province", "potential", "region", "mat_target", "fte")
    
    raw
  })
  
  ## concentration data
  conc_data <- reactive({
    if (is.null(raw()))
      return(NULL)
    
    concentration(raw = raw())
  })
  
  ## concentration curve
  conc_plot <- reactive({
    if (is.null(conc_data()))
      return(NULL)
    
    if (is.null(input$potn.ctrb)) {
      prop <- 0
    } else {
      prop <- input$potn.ctrb/100
    }
    
    plot1 <- plot_ly(hoverinfo = "x+y")
    
    plot1 <- plot1 %>% 
      add_trace(x = as.numeric(rownames(conc_data())),
                y = conc_data()$potential_cumctrb,
                type = "scatter",
                mode = "lines") %>% 
      add_markers(x = nrow(conc_data()[which(conc_data()$potential_cumctrb <= prop), ]),
                  y = prop,
                  color = "red") %>% 
      add_segments(x = nrow(conc_data()[which(conc_data()$potential_cumctrb <= prop), ]), 
                   xend = nrow(conc_data()[which(conc_data()$potential_cumctrb <= prop), ]),
                   y = 0,
                   yend = prop,
                   color = "red") %>% 
      add_segments(x = 0,
                   xend = nrow(conc_data()[which(conc_data()$potential_cumctrb <= prop), ]),
                   y = prop,
                   yend = prop,
                   color = "red")
    
    plot1 <- plot1 %>% 
      layout(
        showlegend = FALSE
      )
    
  })
  
  output$conc <- renderPlotly({
    conc_plot()
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
}












