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
library(plotly)
library(dplyr)
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

options(shiny.maxRequestSize = 1000 * 1024 ^ 2)

##---- load functions ----
# source("./functions/concentration.R", encoding = "UTF-8")

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
      select(SKU, `Hospital`, Province, City, `Doctor#-A`, `Doctor#-B`, `Doctor#-C`, `Doctor#-D`, 
             `Potential（EUR）Y0`, `Potential（EUR）Y1`, `Target（EUR）`, flag)
    colnames(raw) <- c("sku", "hospital", "province", "city", "doctor_a", "doctor_b", "doctor_c", 
                       "doctor_d", "potential0", "potential1", "target", "flag")
    
    raw
  })
  
  ## calculation data ----
  calc_data <- reactive({
    if (is.null(raw()))
      return(NULL)
    raw <- raw()
    
    cum <- raw %>% 
      distinct() %>% 
      group_by(sku, hospital, province, city, flag) %>% 
      summarise(doctor_a = sum(doctor_a, na.rm = TRUE),
                doctor_b = sum(doctor_b, na.rm = TRUE),
                doctor_c = sum(doctor_c, na.rm = TRUE),
                doctor_d = sum(doctor_d, na.rm = TRUE),
                potential0 = sum(potential0, na.rm = TRUE),
                potential1 = sum(potential1, na.rm = TRUE),
                target = sum(target, na.rm = TRUE)) %>% 
      ungroup() %>% 
      arrange(-potential0) %>% 
      mutate(potential0_cumsum = cumsum(potential0),
             potential0_cumctrb = potential0_cumsum / sum(potential0, na.rm = TRUE) * 100,
             fte = (doctor_a * 4 + doctor_b * 2 + doctor_c * 1 + doctor_d * 1) / 8 * 12 / 185,
             cost = fte * 70000,
             productivity = target / fte,
             productivity = ifelse(is.na(productivity) | is.nan(productivity) | is.infinite(productivity),
                                   0,
                                   productivity),
             roi = (target - cost) / cost,
             growth = potential1 / potential0 - 1)
    
    cum1 <- cum %>% 
      filter(flag == 1)
    
    cum2 <- cum %>% 
      filter(flag == 0)
    
    out <- list("data1" = cum1,
                "data2" = cum2)
  })
  
  ## concentration curve ----
  conc_plot <- reactive({
    if (is.null(raw()))
      return(NULL)
    
    if (is.na(input$potn.ctrb)) {
      prop <- 0
    } else {
      prop <- input$potn.ctrb
    }
    
    plot_data <- bind_rows(calc_data()$data1, calc_data()$data2) %>% 
      arrange(potential0_cumctrb)
    
    plot1 <- plot_ly(hoverinfo = "x+y")
    
    plot1 <- plot1 %>% 
      add_trace(x = as.numeric(rownames(plot_data)),
                y = plot_data$potential0_cumctrb,
                type = "scatter",
                mode = "lines") %>% 
      add_markers(x = nrow(plot_data[which(plot_data$potential0_cumctrb <= prop), ]),
                  y = prop,
                  color = "red")
    
    if (prop != 0) {
      plot1 <- plot1 %>% 
        add_segments(x = nrow(plot_data[which(plot_data$potential0_cumctrb <= prop), ]),
                     xend = nrow(plot_data[which(plot_data$potential0_cumctrb <= prop), ]),
                     y = 0,
                     yend = prop,
                     color = "red") %>%
        add_segments(x = 0,
                     xend = nrow(plot_data[which(plot_data$potential0_cumctrb <= prop), ]),
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
              x1 = nrow(plot_data[which(plot_data$potential0_cumctrb <= prop), ]),
              xref = "x",
              y0 = 0,
              y1 = prop,
              yref = "y"
            )
          )
        )
    }
    
    plot1
  })
  
  output$conc <- renderPlotly({
    conc_plot()
  })
  
  ## segment ----
  
  
  ## abandoned province ----
  observeEvent(calc_data(), {
    aban_data <- bind_rows(calc_data()$data1, calc_data()$data2)
    
    updateSelectInput(session,
                      inputId = "aban",
                      label = "Abandoned Provinces",
                      choices = sort(unique(aban_data$province)),
                      selected = NULL)
  })
  
  ## province data ----
  prov_data <- reactive({
    if (is.null(calc_data()))
      return(NULL)
    
    if (is.na(input$potn.ctrb)) {
      prop <- 0
    } else {
      prop <- input$potn.ctrb
    }
    
    total_data <- calc_data()$data2 %>% 
      filter(potential0_cumctrb <= prop,
             !(province %in% input$aban))
    
    # if (is.null(input$aban)) {
    #   covered_data <- total_data
    # } else {
    #   covered_data <- total_data[which(!(total_data$province %in% input$aban)), ]
    # }
    
    if (is.na(input$productivity)) {
      covered_data <- total_data
    } else {
      covered_data <- total_data[which(total_data$productivity >= input$productivity), ]
    }
    
    if (is.na(input$roi)) {
      covered_data <- covered_data
    } else {
      covered_data <- covered_data[which(covered_data$roi >= input$roi), ]
    }
    
    if (is.na(input$growth)) {
      covered_data <- covered_data
    } else {
      covered_data <- covered_data[which(covered_data$growth >= input$growth), ]
    }
    
    covered_city_num <- covered_data %>% 
      bind_rows(calc_data()$data1) %>% 
      select(province, city) %>% 
      distinct() %>% 
      filter(!is.na(city)) %>% 
      group_by(province) %>% 
      summarise(covered_city_num = n()) %>% 
      ungroup()
    
    covered_prov_data <- covered_data %>% 
      bind_rows(calc_data()$data1) %>% 
      group_by(province) %>% 
      summarise(covered_hospital_num = n(),
                covered_doctor_a = sum(doctor_a, na.rm = TRUE),
                covered_doctor_b = sum(doctor_b, na.rm = TRUE),
                covered_doctor_c = sum(doctor_c, na.rm = TRUE),
                covered_doctor_d = sum(doctor_d, na.rm = TRUE),
                covered_potential0 = sum(potential0, na.rm = TRUE),
                covered_potential1 = sum(potential1, na.rm = TRUE),
                covered_target = sum(target, na.rm = TRUE)) %>% 
      ungroup() %>% 
      mutate(covered_fte = (covered_doctor_a * 4 + covered_doctor_b * 2 + covered_doctor_c * 1 + covered_doctor_d * 1) / 8 * 12 / 185,
             covered_cost = covered_fte * 70000,
             covered_productivity = covered_target / covered_fte,
             covered_productivity = ifelse(is.na(covered_productivity) | is.nan(covered_productivity) | is.infinite(covered_productivity),
                                           0,
                                           covered_productivity),
             covered_roi = (covered_target - covered_cost) / covered_cost) %>% 
      left_join(covered_city_num, by = c("province"))
    
    total_city_num <- total_data %>% 
      bind_rows(calc_data()$data1) %>% 
      select(province, city) %>% 
      distinct() %>% 
      filter(!is.na(city)) %>% 
      group_by(province) %>% 
      summarise(total_city_num = n()) %>% 
      ungroup()
    
    total_prov_data <- total_data %>% 
      bind_rows(calc_data()$data1) %>% 
      group_by(province) %>% 
      summarise(total_hospital_num = n(),
                total_doctor_a = sum(doctor_a, na.rm = TRUE),
                total_doctor_b = sum(doctor_b, na.rm = TRUE),
                total_doctor_c = sum(doctor_c, na.rm = TRUE),
                total_doctor_d = sum(doctor_d, na.rm = TRUE),
                total_potential0 = sum(potential0, na.rm = TRUE),
                total_potential1 = sum(potential1, na.rm = TRUE),
                total_target = sum(target, na.rm = TRUE)) %>% 
      ungroup() %>% 
      mutate(total_fte = (total_doctor_a * 4 + total_doctor_b * 2 + total_doctor_c * 1 + total_doctor_d * 1) / 8 * 12 / 185,
             total_cost = total_fte * 70000,
             total_productivity = total_target / total_fte,
             total_productivity = ifelse(is.na(total_productivity) | is.nan(total_productivity) | is.infinite(total_productivity),
                                         0,
                                         total_productivity),
             total_roi = (total_target - total_cost) / total_cost) %>% 
      left_join(total_city_num, by = c("province"))
    
    prov_data <- total_prov_data %>% 
      left_join(covered_prov_data, by = "province") %>% 
      mutate_all(function(x) {ifelse(is.na(x), 0, x)}) %>% 
      mutate(uncovered_hospital_num = total_hospital_num - covered_hospital_num,
             uncovered_city_num = total_city_num - covered_city_num,
             uncovered_doctor_a = total_doctor_a - covered_doctor_a,
             uncovered_doctor_b = total_doctor_b - covered_doctor_b,
             uncovered_doctor_c = total_doctor_c - covered_doctor_c,
             uncovered_doctor_d = total_doctor_d - covered_doctor_d,
             uncovered_potential0 = total_potential0 - covered_potential0,
             uncovered_potential1 = total_potential1 - covered_potential1,
             uncovered_target = total_target - covered_target,
             uncovered_fte = (uncovered_doctor_a * 4 + uncovered_doctor_b * 2 + uncovered_doctor_c * 1 + uncovered_doctor_d * 1) / 8 * 12 / 185,
             uncovered_cost = uncovered_fte * 70000,
             uncovered_productivity = uncovered_target / uncovered_fte,
             uncovered_productivity = ifelse(is.na(uncovered_productivity) | is.nan(uncovered_productivity) | is.infinite(uncovered_productivity),
                                             0,
                                             uncovered_productivity),
             uncovered_roi = (uncovered_target - uncovered_cost) / uncovered_cost)
    
    prov_data
  })
  
  ## plot1 ----
  prov_plot1 <- reactive({
    if (is.null(prov_data()) | is.null(input$kpi1))
      return(NULL)
    
    plot_data <- prov_data()
    plot_data <- plot_data[c("province", paste0("covered_", input$kpi1), paste0("uncovered_", input$kpi1), paste0("total_", input$kpi1))]
    colnames(plot_data) <- c("x", "y1", "y2", "y")
    plot_data <- arrange(plot_data, -y)
    
    plot <- plot_ly(hoverinfo = "name+x+y")
    
    plot <- plot %>% 
      add_bars(x = plot_data$x,
               y = plot_data$y1,
               type = "bar",
               name = "Covered",
               color = I("#4682B4")) %>% 
      add_bars(x = plot_data$x,
               y = plot_data$y2,
               type = "bar",
               name = "Uncovered",
               color = I("#FF8C00")) %>% 
      layout(
        barmode = "stack",
        showlegend = TRUE,
        xaxis = list(
          type = "category",
          categoryorder = "array",
          categoryarray = ~plot_data$x,
          title = "",
          mirror = "ticks"
        ),
        yaxis = list(
          title = "",
          mirror = "ticks"
        )
      )
    
    plot
  })
  
  output$hospital_plot <- renderPlotly({
    prov_plot1()
  })
  
  ## plot2 ----
  prov_plot2 <- reactive({
    if (is.null(prov_data()) | is.null(input$kpi2))
      return(NULL)
    
    plot_data <- prov_data()
    plot_data <- plot_data[c("province", paste0("covered_", input$kpi2), paste0("uncovered_", input$kpi2), paste0("total_", input$kpi2))]
    colnames(plot_data) <- c("x", "y1", "y2", "y")
    plot_data <- arrange(plot_data, -y)
    
    plot <- plot_ly(hoverinfo = "name+x+y")
    
    plot <- plot %>% 
      add_trace(x = plot_data$x,
                y = plot_data$y,
                type = "scatter",
                mode = "lines",
                name = "Total hospital",
                color = I("#00BFFF")) %>% 
      add_trace(x = plot_data$x,
                y = plot_data$y1,
                type = "scatter",
                mode = "lines",
                name = "Covered",
                color = I("#4682B4")) %>% 
      layout(
        showlegend = TRUE,
        xaxis = list(
          type = "category",
          categoryorder = "array",
          categoryarray = ~plot_data$x,
          showgrid = FALSE,
          title = "",
          mirror = "ticks"
        ),
        yaxis = list(
          title = "",
          mirror = "ticks"
        )
      )
    
    plot
  })
  
  output$index_plot <- renderPlotly({
    prov_plot2()
  })
  
  ## recommendation ----
  output$conc_rcmd <- renderPlotly({
    conc_plot()
  })
  
  prov_data_rcmd <- reactive({
    if (is.null(calc_data()))
      return(NULL)
    
    if (is.na(input$potn.ctrb)) {
      prop <- 0
    } else {
      prop <- input$potn.ctrb
    }
    
    total_data <- calc_data()$data2 %>% 
      filter(potential0_cumctrb <= prop,
             !(province %in% input$aban))
    
    if (input$scenario == "Max ROI") {
      covered_data <- total_data %>% 
        filter(roi >= 0) %>% 
        bind_rows(calc_data()$data1)
      
    } else if (input$scenario == "Max Productivity") {
      covered_data <- total_data %>% 
        filter(productivity >= 1) %>% 
        bind_rows(calc_data()$data1)
      
    } else {
      covered_data <- total_data %>% 
        bind_rows(calc_data()$data1)
    }
    
    covered_data <- total_data %>% 
      filter(!(province %in% input$aban),
             productivity >= input$productivity,
             roi >= input$roi,
             growth >= input$growth) %>% 
      bind_rows(calc_data()$data1)
    
    covered_city_num <- covered_data %>% 
      select(province, city) %>% 
      distinct() %>% 
      filter(!is.na(city)) %>% 
      group_by(province) %>% 
      summarise(covered_city_num = n()) %>% 
      ungroup()
    
    covered_prov_data <- covered_data %>% 
      group_by(province) %>% 
      summarise(covered_hospital_num = n(),
                covered_doctor_a = sum(doctor_a, na.rm = TRUE),
                covered_doctor_b = sum(doctor_b, na.rm = TRUE),
                covered_doctor_c = sum(doctor_c, na.rm = TRUE),
                covered_doctor_d = sum(doctor_d, na.rm = TRUE),
                covered_potential0 = sum(potential0, na.rm = TRUE),
                covered_potential1 = sum(potential1, na.rm = TRUE),
                covered_target = sum(target, na.rm = TRUE)) %>% 
      ungroup() %>% 
      mutate(covered_fte = (covered_doctor_a * 4 + covered_doctor_b * 2 + covered_doctor_c * 1 + covered_doctor_d * 1) / 8 * 12 / 185,
             covered_cost = covered_fte * 70000,
             covered_productivity = covered_target / covered_fte,
             covered_productivity = ifelse(is.na(covered_productivity) | is.nan(covered_productivity) | is.infinite(covered_productivity),
                                           0,
                                           covered_productivity),
             covered_roi = (covered_target - covered_cost) / covered_cost) %>% 
      left_join(covered_city_num, by = c("province"))
    
    covered_prov_data
  })
  
  prov_plot1_rcmd <- reactive({
    if (is.null(prov_data_rcmd()) | is.null(input$kpi1_rcmd))
      return(NULL)
    
    plot_data <- prov_data_rcmd()
    plot_data <- plot_data[c("province", paste0("covered_", input$kpi1_rcmd))]
    colnames(plot_data) <- c("x", "y")
    plot_data <- arrange(plot_data, -y)
    
    plot <- plot_ly(hoverinfo = "name+x+y")
    
    plot <- plot %>% 
      add_bars(x = plot_data$x,
               y = plot_data$y,
               type = "bar",
               name = "Covered",
               color = I("#4682B4")) %>% 
      layout(
        showlegend = TRUE,
        xaxis = list(
          type = "category",
          categoryorder = "array",
          categoryarray = ~plot_data$x,
          title = "",
          mirror = "ticks"
        ),
        yaxis = list(
          title = "",
          mirror = "ticks"
        )
      )
    
    plot
  })
  
  output$hospital_plot_rcmd <- renderPlotly({
    prov_plot1_rcmd()
  })
  
  prov_plot2_rcmd <- reactive({
    if (is.null(prov_data_rcmd()) | is.null(input$kpi2_rcmd))
      return(NULL)
    
    plot_data <- prov_data_rcmd()
    plot_data <- plot_data[c("province", paste0("covered_", input$kpi2_rcmd))]
    colnames(plot_data) <- c("x", "y")
    plot_data <- arrange(plot_data, -y)
    
    plot <- plot_ly(hoverinfo = "name+x+y")
    
    plot <- plot %>% 
      add_trace(x = plot_data$x,
                y = plot_data$y,
                type = "scatter",
                mode = "lines",
                name = "Covered",
                color = I("#4682B4")) %>% 
      layout(
        showlegend = TRUE,
        xaxis = list(
          type = "category",
          categoryorder = "array",
          categoryarray = ~plot_data$x,
          showgrid = FALSE,
          title = "",
          mirror = "ticks"
        ),
        yaxis = list(
          title = "",
          mirror = "ticks"
        )
      )
    
    plot
  })
  
  output$index_plot_rcmd <- renderPlotly({
    prov_plot2_rcmd()
  })
}












