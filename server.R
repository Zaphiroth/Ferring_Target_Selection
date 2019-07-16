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
      select(`PHA code`, TERMINALCODE, Province, City, `Potential-2019(MAT201904)`, 
             大区, `MAT201904销售指标（RMB）`, FTE)
    colnames(raw) <- c("code1", "code2", "province", "city", "potential", "region", "mat_target", "fte")
    
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
    
    if (is.na(input$potn.ctrb)) {
      prop <- 0
    } else {
      prop <- input$potn.ctrb
    }
    
    plot1 <- plot_ly(hoverinfo = "x+y")
    
    plot1 <- plot1 %>% 
      add_trace(x = as.numeric(rownames(conc_data())),
                y = conc_data()$potential_cumctrb,
                type = "scatter",
                mode = "lines") %>% 
      add_markers(x = nrow(conc_data()[which(conc_data()$potential_cumctrb <= prop), ]),
                  y = prop,
                  color = "red")
    
    if (prop != 0) {
      plot1 <- plot1 %>% 
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
    }
    
    plot1 <- plot1 %>% 
      layout(
        showlegend = FALSE,
        xaxis = list(
          showticklabels = TRUE,
          zeroline = TRUE,
          title = "",
          showline = FALSE,
          mirror = "ticks"
        ),
        yaxis = list(
          showticklabels = TRUE,
          zeroline = TRUE,
          title = "",
          ticksuffix = "%",
          showline = FALSE,
          mirror = "ticks"
        )
      )
    
    if (prop != 0) {
      plot1 <- plot1 %>% 
        layout(
          shapes = list(
            list(
              type = "rect",
              fillcolor = "grey",
              opacity = 0.3,
              x0 = 0,
              x1 = nrow(conc_data()[which(conc_data()$potential_cumctrb <= prop), ]),
              xref = "x",
              y0 = 0,
              y1 = prop,
              yref = "y"
            )
          )
        )
    }
    
    return(plot1)
  })
  
  output$conc <- renderPlotly({
    conc_plot()
  })
  
  ## abandoned province
  observeEvent(conc_data(), {
    updateSelectInput(session,
                      inputId = "aban",
                      label = "Abandoned Provinces",
                      choices = sort(unique(conc_data()$province)),
                      selected = NULL)
  })
  
  
  ## select data
  sel_data <- reactive({
    if (is.null(conc_data()))
      return(NULL)
    
    plot_data <- conc_data()
    
    if (is.null(input$aban)) {
      plot_data <- plot_data
    } else {
      plot_data <- plot_data[which(!(plot_data$province %in% input$aban)), ]
    }
    
    if (is.na(input$productivity)) {
      plot_data <- plot_data
    } else {
      plot_data <- plot_data[which(plot_data$productivity >= input$productivity), ]
    }
    
    if (is.na(input$product)) {
      plot_data <- plot_data
    } else {
      plot_data <- plot_data[which(plot_data$mat_target >= input$product), ]
    }
    
    if (is.na(input$roi)) {
      plot_data <- plot_data
    } else {
      plot_data <- plot_data[which(plot_data$roi >= input$roi), ]
    }
    
    prov_city <- plot_data %>% 
      select(province, city) %>% 
      distinct() %>% 
      filter(!is.na(city)) %>% 
      group_by(province) %>% 
      summarise(`City#` = n()) %>% 
      ungroup()
    
    plot_data1 <- plot_data %>% 
      group_by(province) %>% 
      summarise(`Hospital#` = n(),
                `FTE#` = sum(fte, na.rm = TRUE),
                `Avg. Productivity` = mean(productivity),
                `ROI` = mean(roi)) %>% 
      ungroup() %>% 
      left_join(prov_city, by = c("province"))
    
    return(plot_data1)
  })
  
  ## summary table
  conc_table <- reactive({
    if (is.null(sel_data()))
      return(NULL)
    
    table_data1 <- sel_data()
    
    table_data2 <- tibble(
      "FTEs" = format(round(sum(table_data1$`FTE#`, na.rm = TRUE)), big.mark = ","),
      "Hospital No." = format(sum(table_data1$`Hospital#`, na.rm = TRUE), big.mark = ","),
      "City No." = format(sum(table_data1$`City#`, na.rm = TRUE), big.mark = ","),
      "Province No." = format(nrow(table_data1), big.mark = ",")
    )
    
    table1 <- datatable(
      table_data2,
      rownames = FALSE,
      options = list(
        autoWidth = TRUE,
        paging = FALSE,
        scrollY = FALSE,
        ordering = FALSE,
        bLengthChange = FALSE,
        searching = FALSE,
        bInfo = FALSE,
        columnDefs = list(
          list(
            className = "dt-center",
            targets = "_all"
          )
        ),
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#fff', 'color': '#1F497D'});",
          "}"
        )
      )
    )
    
    return(table1)
  })
  
  output$table <- renderDataTable({
    conc_table()
  })
  
  ## by province
  prov_data <- reactive({
    if (is.null(conc_data()))
      return(NULL)
    
    plot_data <- conc_data()
    
    if (is.null(input$aban)) {
      plot_data <- plot_data
    } else {
      plot_data <- plot_data[which(!(plot_data$province %in% input$aban)), ]
    }
    
    if (is.na(input$productivity)) {
      plot_data <- plot_data
    } else {
      plot_data <- plot_data[which(plot_data$productivity >= input$productivity), ]
    }
    
    if (is.na(input$product)) {
      plot_data <- plot_data
    } else {
      plot_data <- plot_data[which(plot_data$mat_target >= input$product), ]
    }
    
    if (is.na(input$roi)) {
      plot_data <- plot_data
    } else {
      plot_data <- plot_data[which(plot_data$roi >= input$roi), ]
    }
    
    if ("All" %in% input$region) {
      plot_data <- plot_data
    } else {
      plot_data <- plot_data[which(plot_data$region %in% input$region), ]
    }
    
    prov_city <- plot_data %>% 
      select(province, city) %>% 
      distinct() %>% 
      filter(!is.na(city)) %>% 
      group_by(province) %>% 
      summarise(`City#` = n()) %>% 
      ungroup()
    
    plot_data1 <- plot_data %>% 
      group_by(province) %>% 
      summarise(`Hospital#` = n(),
                `FTE#` = sum(fte, na.rm = TRUE),
                `Avg. Productivity` = mean(productivity),
                `ROI` = mean(roi)) %>% 
      ungroup() %>% 
      left_join(prov_city, by = c("province"))
    
    return(plot_data1)
  })
  
  
  prov_plot <- reactive({
    if (is.null(prov_data()) | is.null(input$region))
      return(NULL)
    
    plot_data2 <- prov_data()
    
    plot_data2 <- plot_data2[c("province", input$kpi)]
    colnames(plot_data2) <- c("x", "y")
    plot_data2 <- arrange(plot_data2, -y)
    
    plot2 <- plot_ly(hoverinfo = "x+y")
    
    plot2 <- plot2 %>% 
      add_bars(x = plot_data2$x,
               y = plot_data2$y,
               text = format(round(plot_data2$y, 2), big.mark = ","),
               textposition = "outside",
               type = "bar",
               color = I("#00a65a"))
    
    plot2 <- plot2 %>% 
      layout(
        showlegend = FALSE,
        xaxis = list(
          categoryorder = "array",
          categoryarray = ~plot_data2$x
        ),
        yaxis = list(
          range = c(0, max(plot_data2$y) * 1.2)
        )
      )
    
    return(plot2)
  })
  
  output$bar <- renderPlotly({
    prov_plot()
  })
  
  
  
  
  
  
  
  
}












