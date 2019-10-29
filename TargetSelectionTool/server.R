# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Ferring BI Tool
# Purpose:      Shiny server
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
# library(plotly)
# library(dplyr)
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

##---- load functions ----
# source("./functions/concentration.R", encoding = "UTF-8")

## save scenario ----
scenario1 <- list(kPotnCtrb = 0,
                  aban = NULL,
                  productivity = 0,
                  growth = -Inf)

scenario2 <- list(kPotnCtrb = 0,
                  aban = NULL,
                  productivity = 0,
                  growth = -Inf)

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
      select(SKU, `Hospital`, `Hosp_level`, `Decile by market`, `Is there quota`, Province, City, `City Tier`, 
             `Doctor#-A`, `Doctor#-B`, `Doctor#-C`, `Doctor#-D`, `Potential（EUR）Y0`, `Potential（EUR）Y1`, 
             `Target（EUR）`, flag)
    colnames(raw) <- c("sku", "hospital", "hosp_level", "decile", "is", "province", "city", "tier", "doctor_a", 
                       "doctor_b", "doctor_c", "doctor_d", "potential0", "potential1", "target", "flag")
    
    raw
  })
  
  dtbt <- reactive({
    if (is.null(input$dtbt))
      return(NULL)
    
    inFile.dtbt <- input$dtbt
    dtbt <- read_xlsx(
      inFile.dtbt$datapath,
      na = "NA"
    ) %>% 
      setDF() %>% 
      select(`大区`, `大区经理`, `地区1`, `地区2`, `地区经理`, `代表区域`, `代表`, `产品组`, `医院编码`, 
             `医院名称`, `产品编码`, `产品名称`, `MAT 1905（EUR）`)
    colnames(dtbt) <- c("大区", "大区经理", "地区1", "地区2", "地区经理", "代表区域", "代表", "产品组", 
                        "医院编码", "医院名称", "产品编码", "产品名称", "mat")
    
    dtbt
  })
  
  toggleState("go", !is.null(input$raw))
  toggleState("record1", !is.null(input$raw))
  toggleState("record2", !is.null(input$raw))
  
  ## sku ----
  observeEvent(raw(), {
    updateSelectInput(session,
                      inputId = "sku",
                      label = "Selection SKU",
                      choices = sort(unique(raw()$sku)),
                      selected = NULL)
  })
  
  ## abandoned province ----
  observeEvent(raw(), {
    updateSelectInput(session,
                      inputId = "aban",
                      label = "Abandoned Provinces",
                      choices = sort(unique(raw()$province)),
                      selected = NULL)
  })
  
  ## calculation data ----
  CalcData <- reactive({
    input$go
    isolate({
      if (is.null(raw()) | is.null(input$sku))
        return(NULL)
      
      cum <- raw() %>% 
        distinct() %>% 
        filter(sku %in% input$sku) %>% 
        mutate(freq = ifelse(sku == "Gly",
                             doctor_a * 6 + doctor_b * 4 + doctor_c * 2 + doctor_d * 1,
                             ifelse(sku == "Pentasa SUP",
                                    doctor_a * 4 + doctor_b * 3 + doctor_c * 1 + doctor_d * 1,
                                    ifelse(sku == "Pentasa TAB",
                                           doctor_a * 4 + doctor_b * 3 + doctor_c * 1 + doctor_d * 1,
                                           0)))) %>% 
        group_by(hospital, hosp_level, decile, province, city, tier, is, flag) %>% 
        summarise(freq = sum(freq, na.rm = TRUE),
                  potential0 = sum(potential0, na.rm = TRUE),
                  potential1 = sum(potential1, na.rm = TRUE),
                  target = sum(target, na.rm = TRUE)) %>% 
        ungroup() %>% 
        arrange(-potential0) %>% 
        mutate(potential0_cumsum = cumsum(potential0),
               potential0_cumctrb = potential0_cumsum / sum(potential0, na.rm = TRUE) * 100,
               fte = freq / 8 * 12 / 185,
               cost = fte * 70000,
               productivity = target / fte,
               productivity = ifelse(is.na(productivity) | is.nan(productivity) | is.infinite(productivity),
                                     0,
                                     productivity),
               roi = (target - cost) / cost,
               growth = potential1 / potential0 - 1,
               growth = ifelse(is.na(growth),
                               0,
                               growth))
      
      cum1 <- cum %>% 
        filter(flag == 1)
      
      cum2 <- cum %>% 
        filter(flag == 0)
      
      out <- list("data1" = cum1,
                  "data2" = cum2)
    })
  })
  
  ## concentration curve ----
  ConcPlot <- reactive({
    input$go
    isolate({
      if (is.null(CalcData()))
        return(NULL)
      
      if (is.na(input$kPotnCtrb)) {
        kProp <- 0
      } else {
        kProp <- input$kPotnCtrb
      }
      
      plot.data <- bind_rows(CalcData()$data1, CalcData()$data2) %>% 
        arrange(potential0_cumctrb)
      
      plot1 <- plot_ly(hoverinfo = "x+y")
      
      plot1 <- plot1 %>% 
        add_trace(x = as.numeric(rownames(plot.data)),
                  y = plot.data$potential0_cumctrb,
                  type = "scatter",
                  mode = "lines",
                  color = I(options()$total.color)) %>% 
        add_markers(x = nrow(plot.data[which(plot.data$potential0_cumctrb <= kProp), ]),
                    y = kProp,
                    color = I(options()$marker.color))
      
      # if (kProp != 0) {
      #   plot1 <- plot1 %>% 
      #     add_segments(x = nrow(plot.data[which(plot.data$potential0_cumctrb <= kProp), ]),
      #                  xend = nrow(plot.data[which(plot.data$potential0_cumctrb <= kProp), ]),
      #                  y = 0,
      #                  yend = kProp,
      #                  color = "red") %>%
      #     add_segments(x = 0,
      #                  xend = nrow(plot.data[which(plot.data$potential0_cumctrb <= kProp), ]),
      #                  y = kProp,
      #                  yend = kProp,
      #                  color = "red")
      # }
      
      plot1 <- plot1 %>% 
        layout(
          showlegend = FALSE,
          xaxis = list(
            showticklabels = TRUE,
            tickformat = ",",
            showline = FALSE,
            zeroline = TRUE,
            title = "",
            mirror = "ticks"
          ),
          yaxis = list(
            showticklabels = TRUE,
            ticksuffix = "%",
            showline = FALSE,
            zeroline = TRUE,
            title = "",
            hoverformat = ".2f",
            mirror = "ticks"
          )
        )
      
      if (kProp != 0) {
        plot1 <- plot1 %>% 
          layout(
            shapes = list(
              list(
                type = "rect",
                fillcolor = options()$square.color,
                line = list(color = options()$square.color),
                opacity = 0.2,
                x0 = 0,
                x1 = nrow(plot.data[which(plot.data$potential0_cumctrb <= kProp), ]),
                xref = "x",
                y0 = 0,
                y1 = kProp,
                yref = "y"
              )
            )
          )
      }
      
      plot1
    })
  })
  
  output$Conc <- renderPlotly({
    ConcPlot()
  })
  
  ## segmentation ----
  SegData <- reactive({
    input$go
    isolate({
      if (is.null(CalcData()))
        return(NULL)
      
      if (is.na(input$kPotnCtrb)) {
        kProp <- 0
      } else {
        kProp <- input$kPotnCtrb
      }
      
      seg.data <- CalcData()$data2 %>% 
        filter(!(province %in% input$aban)) %>% 
        bind_rows(CalcData()$data1) %>% 
        mutate(market_share = target / potential0,
               market_share = ifelse(is.na(market_share),
                                     0,
                                     market_share))
      
      if (input$growth_share == "Growth Rate") {
        if (is.na(input$kGrowth)) {
          kIndex <- -Inf
        } else {
          kIndex <- input$kGrowth/100
        }
        
        seg.total <- seg.data %>% 
          mutate(segment = ifelse(potential0_cumctrb <= kProp & growth >= kIndex,
                                  "A",
                                  ifelse(potential0_cumctrb <= kProp & growth < kIndex,
                                         "B",
                                         ifelse(potential0_cumctrb > kProp & growth >= kIndex,
                                                "C",
                                                ifelse(potential0_cumctrb > kProp & growth < kIndex,
                                                       "D",
                                                       "0")))))
        
      } else if (input$growth_share == "Market Share") {
        if (is.na(input$kShare)) {
          kIndex <- 0
        } else {
          kIndex <- input$kShare/100
        }
        
        seg.total <- seg.data %>% 
          mutate(segment = ifelse(potential0_cumctrb <= kProp & market_share >= kIndex,
                                  "A",
                                  ifelse(potential0_cumctrb <= kProp & market_share < kIndex,
                                         "B",
                                         ifelse(potential0_cumctrb > kProp & market_share >= kIndex,
                                                "C",
                                                ifelse(potential0_cumctrb > kProp & market_share < kIndex,
                                                       "D",
                                                       "0")))))
        
      } else {
        return(NULL)
      }
      
      seg.a <- seg.total %>% 
        filter(segment == "A") %>% 
        mutate(hospital_num = n(),
               city_num = length(sort(unique(city)))) %>% 
        group_by(hospital_num, city_num) %>% 
        summarise(fte = sum(fte, na.rm = TRUE),
                  cost = sum(cost, na.rm = TRUE),
                  target = sum(target, na.rm = TRUE)) %>% 
        ungroup() %>% 
        mutate(roi = (target - cost) / cost * 100,
               productivity = target / fte) %>% 
        mutate(hospital_num = format(hospital_num, big.mark = ","),
               city_num = format(city_num, big.mark = ","),
               fte = format(round(fte,1), big.mark = ","),
               roi = paste0(round(roi, 1), "%"),
               productivity = format(round(productivity, 1), big.mark = ",")) %>% 
        select("Number of Hospital" = "hospital_num",
               "Number of City" = "city_num",
               "FTE" = "fte",
               "ROI" = "roi",
               "Productivity" = "productivity") %>% 
        melt(id.vars = NULL)
      
      seg.b <- seg.total %>% 
        filter(segment == "B") %>% 
        mutate(hospital_num = n(),
               city_num = length(sort(unique(city)))) %>% 
        group_by(hospital_num, city_num) %>% 
        summarise(fte = sum(fte, na.rm = TRUE),
                  cost = sum(cost, na.rm = TRUE),
                  target = sum(target, na.rm = TRUE)) %>% 
        ungroup() %>% 
        mutate(roi = (target - cost) / cost * 100,
               productivity = target / fte) %>% 
        mutate(hospital_num = format(hospital_num, big.mark = ","),
               city_num = format(city_num, big.mark = ","),
               fte = format(round(fte,1), big.mark = ","),
               roi = paste0(round(roi, 1), "%"),
               productivity = format(round(productivity, 1), big.mark = ",")) %>% 
        select("Number of Hospital" = "hospital_num",
               "Number of City" = "city_num",
               "FTE" = "fte",
               "ROI" = "roi",
               "Productivity" = "productivity") %>% 
        melt(id.vars = NULL)
      
      seg.c <- seg.total %>% 
        filter(segment == "C") %>% 
        mutate(hospital_num = n(),
               city_num = length(sort(unique(city)))) %>% 
        group_by(hospital_num, city_num) %>% 
        summarise(fte = sum(fte, na.rm = TRUE),
                  cost = sum(cost, na.rm = TRUE),
                  target = sum(target, na.rm = TRUE)) %>% 
        ungroup() %>% 
        mutate(roi = (target - cost) / cost * 100,
               productivity = target / fte) %>% 
        mutate(hospital_num = format(hospital_num, big.mark = ","),
               city_num = format(city_num, big.mark = ","),
               fte = format(round(fte,1), big.mark = ","),
               roi = paste0(round(roi, 1), "%"),
               productivity = format(round(productivity, 1), big.mark = ",")) %>% 
        select("Number of Hospital" = "hospital_num",
               "Number of City" = "city_num",
               "FTE" = "fte",
               "ROI" = "roi",
               "Productivity" = "productivity") %>% 
        melt(id.vars = NULL)
      
      seg.d <- seg.total %>% 
        filter(segment == "D") %>% 
        mutate(hospital_num = n(),
               city_num = length(sort(unique(city)))) %>% 
        group_by(hospital_num, city_num) %>% 
        summarise(fte = sum(fte, na.rm = TRUE),
                  cost = sum(cost, na.rm = TRUE),
                  target = sum(target, na.rm = TRUE)) %>% 
        ungroup() %>% 
        mutate(roi = (target - cost) / cost * 100,
               productivity = target / fte) %>% 
        mutate(hospital_num = format(hospital_num, big.mark = ","),
               city_num = format(city_num, big.mark = ","),
               fte = format(round(fte,1), big.mark = ","),
               roi = paste0(round(roi, 1), "%"),
               productivity = format(round(productivity, 1), big.mark = ",")) %>% 
        select("Number of Hospital" = "hospital_num",
               "Number of City" = "city_num",
               "FTE" = "fte",
               "ROI" = "roi",
               "Productivity" = "productivity") %>% 
        melt(id.vars = NULL)
      
      seg.na <- data.frame("variable" = c("Number of Hospital", "Number of City", 
                                          "FTE", "ROI", "Productivity"),
                           "value" = c(0, 0, 0, 0, 0))
      
      seg.list <- list("seg.a" = seg.a,
                       "seg.b" = seg.b,
                       "seg.c" = seg.c,
                       "seg.d" = seg.d)
      
      for (i in names(seg.list)) {
        if (nrow(seg.list[[i]]) == 0) {
          seg.list[[i]] <- seg.na
        }
      }
      
      seg.list[["total"]] <- seg.total
      seg.list[["kProp"]] <- kProp
      seg.list[["kIndex"]] <- kIndex
      
      seg.list
    })
  })
  
  output$seg.v <- renderText({
    paste0("Potential Cumulation Contribution = ", input$kPotnCtrb, "%")
  })
  
  output$seg.h1 <- renderText({
    if (input$growth_share == "Growth Rate") {
      paste0(input$growth_share, " >= ", input$kGrowth, "%")
    } else if (input$growth_share == "Market Share") {
      paste0(input$growth_share, " >= ", input$kShare, "%")
    }
  })
  
  output$seg.h2 <- renderText({
    if (input$growth_share == "Growth Rate") {
      paste0(input$growth_share, " < ", input$kGrowth, "%")
    } else if (input$growth_share == "Market Share") {
      paste0(input$growth_share, " < ", input$kShare, "%")
    }
  })
  
  output$TableA <- DT::renderDataTable({
    if (is.null(SegData()))
      return(NULL)
    
    DT::datatable(
      SegData()$seg.a,
      rownames = FALSE,
      colnames = NULL,
      options = list(
        columnDefs = list(
          list(className = "dt-left",
               targets = 0),
          list(className = "dt-right",
               targets = 1)
        ),
        paging = FALSE,
        scrollX = FALSE,
        searching = FALSE,
        ordering = FALSE,
        pageLength = 5,
        lengthChange = FALSE,
        bInfo = FALSE
      )
    ) %>% 
      formatStyle(
        c("variable"),
        fontWeight = "bold"
      ) %>% 
      formatStyle(
        c("variable", "value"),
        backgroundColor = options()$seg.color
      )
  })
  
  output$TableB <- DT::renderDataTable({
    if (is.null(SegData()))
      return(NULL)
    
    DT::datatable(
      SegData()$seg.b,
      rownames = FALSE,
      colnames = NULL,
      options = list(
        columnDefs = list(
          list(className = "dt-left",
               targets = 0),
          list(className = "dt-right",
               targets = 1)
        ),
        paging = FALSE,
        scrollX = FALSE,
        searching = FALSE,
        ordering = FALSE,
        pageLength = 5,
        lengthChange = FALSE,
        bInfo = FALSE
      )
    ) %>% 
      formatStyle(
        c("variable"),
        fontWeight = "bold"
      ) %>% 
      formatStyle(
        c("variable", "value"),
        backgroundColor = options()$seg.color
      )
  })
  
  output$TableC <- DT::renderDataTable({
    if (is.null(SegData()))
      return(NULL)
    
    DT::datatable(
      SegData()$seg.c,
      rownames = FALSE,
      colnames = NULL,
      options = list(
        columnDefs = list(
          list(className = "dt-left",
               targets = 0),
          list(className = "dt-right",
               targets = 1)
        ),
        paging = FALSE,
        scrollX = FALSE,
        searching = FALSE,
        ordering = FALSE,
        pageLength = 5,
        lengthChange = FALSE,
        bInfo = FALSE
      )
    ) %>% 
      formatStyle(
        c("variable"),
        fontWeight = "bold"
      ) %>% 
      formatStyle(
        c("variable", "value"),
        backgroundColor = options()$seg.color
      )
  })
  
  output$TableD <- DT::renderDataTable({
    if (is.null(SegData()))
      return(NULL)
    
    DT::datatable(
      SegData()$seg.d,
      rownames = FALSE,
      colnames = NULL,
      options = list(
        columnDefs = list(
          list(className = "dt-left",
               targets = 0),
          list(className = "dt-right",
               targets = 1)
        ),
        paging = FALSE,
        scrollX = FALSE,
        searching = FALSE,
        ordering = FALSE,
        pageLength = 5,
        lengthChange = FALSE,
        bInfo = FALSE
      )
    ) %>% 
      formatStyle(
        c("variable"),
        fontWeight = "bold"
      ) %>% 
      formatStyle(
        c("variable", "value"),
        backgroundColor = options()$seg.color
      )
  })
  
  ## actual plot1 ----
  ActualPlot1 <- reactive({
    input$go
    isolate({
      if (is.null(dtbt())) {
        return(NULL)
      }
      
      plot.data <- dtbt() %>% 
        group_by(`代表`) %>% 
        summarise(mat = sum(mat, na.rm = TRUE)) %>% 
        ungroup() %>% 
        mutate(section = cut(mat, breaks = seq(0, 600000, 60000), 
                             labels = c("0~60", "60~120", "120~180", "180~240", "240~300", 
                                        "300~360", "360~420", "420~480", "480~540", "540~600"))) %>% 
        arrange(section) %>% 
        group_by(section) %>% 
        summarise(rep_num = n()) %>% 
        ungroup()
      
      plot1 <- plot_ly(hoverinfo = "name+x+y")
      
      plot1 <- plot1 %>% 
        add_trace(x = plot.data$section,
                  y = plot.data$rep_num,
                  name = "代表",
                  type = "bar",
                  text = plot.data$rep_num,
                  textfont = list(color = I("#000")),
                  textposition = "outside",
                  color = I(options()$covered.color)) %>% 
        layout(
          title = "2018 GU Territory Distribution by Value",
          showlegend = FALSE,
          annotations = list(
            x = 0.9,
            y = 1,
            text = "RMB: Thousand",
            showarrow = FALSE,
            xref = "paper",
            yref = "paper"
          ),
          xaxis = list(
            title = "",
            type = "category",
            categoryorder = "array",
            categoryarray = ~plot.data$section,
            mirror = "ticks"
          ),
          yaxis = list(
            title = "",
            showticklabels = FALSE,
            mirror = "ticks"
          )
        )
      
      plot1
    })
  })
  
  output$ActualPlot1 <- renderPlotly({
    ActualPlot1()
  })
  
  ## actual plot2 ----
  observeEvent(input$go, {
    updateSelectInput(session,
                      inputId = "actual.sku",
                      label = "Selection SKU",
                      choices = input$sku,
                      selected = input$sku)
  })
  
  ActualPlot2 <- reactive({
    c(input$go, input$actual.sku)
    isolate({
      if (is.null(raw()) | is.null(input$actual.sku)) {
        return(NULL)
      }
      
      plot.data <- raw() %>% 
        filter(is == "Y",
               sku %in% input$actual.sku) %>% 
        group_by(hospital, decile) %>% 
        summarise(potential0 = sum(potential0, na.rm = TRUE),
                  target = sum(target, na.rm = TRUE)) %>% 
        ungroup() %>% 
        group_by(decile) %>% 
        summarise(potential0 = sum(potential0, na.rm = TRUE),
                  target = sum(target, na.rm = TRUE),
                  hosp_num = n()) %>% 
        ungroup() %>% 
        mutate(market_share = target / potential0,
               market_share = ifelse(is.na(market_share),
                                     0,
                                     market_share),
               decile = factor(decile, levels = c("D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "D9", "D10"))) %>% 
        arrange(decile)
      
      plot1 <- plot_ly(hoverinfo = "name+x+y")
      
      plot1 <- plot1 %>% 
        add_trace(x = plot.data$decile,
                  y = plot.data$hosp_num,
                  name = "Hospital",
                  type = "bar",
                  # text = plot.data$hosp_num,
                  # textposition = "outside",
                  color = I(options()$covered.color)) %>% 
        add_trace(x = plot.data$decile,
                  y = plot.data$market_share,
                  name = "Market share",
                  yaxis = "y2",
                  type = "scatter",
                  mode = "lines",
                  color = I(options()$share.color)) %>% 
        layout(
          title = "2018 GU Market Share by Decile",
          showlegend = TRUE,
          legend = list(
            x = 0.215,
            y = 1.05,
            orientation = "h"
          ),
          margin = list(
            l = 50,
            r = 50,
            b = 50,
            t = 50,
            pad = 4
          ),
          xaxis = list(
            title = "",
            type = "category",
            # categoryorder = "array",
            # categoryarray = ~c("D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "D9", "D10"),
            showticklabels = TRUE,
            mirror = "ticks"
          ),
          yaxis = list(
            side = "left",
            title = "",
            range = c(0, max(plot.data$hosp_num)*1.2),
            zeroline = TRUE,
            showgrid = TRUE,
            showticklabels = TRUE,
            tickformat = ",",
            hoverformat = ",",
            mirror = "ticks"
          ),
          yaxis2 = list(
            overlaying = "y",
            side = "right",
            title = "",
            range = c(0, max(plot.data$market_share)*1.2),
            zeroline = TRUE,
            showgrid = FALSE,
            showticklabels = TRUE,
            tickformat = "%",
            hoverformat = "%",
            mirror = "ticks"
          )
        )
      
      plot1
    })
  })
  
  output$ActualPlot2 <- renderPlotly({
    ActualPlot2()
  })
  
  ## province data ----
  ProvData <- reactive({
    if (is.null(CalcData()))
      return(NULL)
    
    if (is.na(input$kPotnCtrb)) {
      kProp <- 0
    } else {
      kProp <- input$kPotnCtrb
    }
    
    total.data <- CalcData()$data2 %>% 
      filter(!(province %in% input$aban)) %>% 
      bind_rows(CalcData()$data1)
    
    covered.data <- CalcData()$data2 %>% 
      filter(potential0_cumctrb <= kProp,
             !(province %in% input$aban))
    
    # if (is.null(input$aban)) {
    #   covered.data <- total.data
    # } else {
    #   covered.data <- total.data[which(!(total.data$province %in% input$aban)), ]
    # }
    
    if (is.na(input$productivity)) {
      covered.data <- covered.data
    } else {
      covered.data <- covered.data[which(covered.data$productivity >= input$productivity), ]
    }
    
    # if (is.na(input$roi)) {
    #   covered.data <- covered.data
    # } else {
    #   covered.data <- covered.data[which(covered.data$roi >= input$roi), ]
    # }
    
    if (is.na(input$growth)) {
      covered.data <- covered.data
    } else {
      covered.data <- covered.data[which(covered.data$growth >= input$growth/100), ]
    }
    
    covered.data <- bind_rows(covered.data, CalcData()$data1)
    
    actual.prov.data <- covered.data %>% 
      filter(is == "Y") %>% 
      group_by(province) %>% 
      summarise(actual_hospital_num = n(),
                actual_city_num = length(sort(unique(city))),
                actual_freq = sum(freq, na.rm = TRUE),
                actual_potential0 = sum(potential0, na.rm = TRUE),
                actual_potential1 = sum(potential1, na.rm = TRUE),
                actual_target = sum(target, na.rm = TRUE)) %>% 
      ungroup() %>% 
      mutate(actual_fte = actual_freq / 8 * 12 / 185,
             actual_cost = actual_fte * 70000,
             actual_productivity = actual_target / actual_fte,
             actual_productivity = ifelse(is.na(actual_productivity) | is.nan(actual_productivity) | is.infinite(actual_productivity),
                                          0,
                                          actual_productivity),
             actual_roi = (actual_target - actual_cost) / actual_cost)
    
    covered.prov.data <- covered.data %>% 
      group_by(province) %>% 
      summarise(covered_hospital_num = n(),
                covered_city_num = length(sort(unique(city))),
                covered_freq = sum(freq, na.rm = TRUE),
                covered_potential0 = sum(potential0, na.rm = TRUE),
                covered_potential1 = sum(potential1, na.rm = TRUE),
                covered_target = sum(target, na.rm = TRUE)) %>% 
      ungroup() %>% 
      mutate(covered_fte = covered_freq / 8 * 12 / 185,
             covered_cost = covered_fte * 70000,
             covered_productivity = covered_target / covered_fte,
             covered_productivity = ifelse(is.na(covered_productivity) | is.nan(covered_productivity) | is.infinite(covered_productivity),
                                           0,
                                           covered_productivity),
             covered_roi = (covered_target - covered_cost) / covered_cost)
    
    total.prov.data <- total.data %>% 
      group_by(province) %>% 
      summarise(total_hospital_num = n(),
                total_city_num = length(sort(unique(city))),
                total_freq = sum(freq, na.rm = TRUE),
                total_potential0 = sum(potential0, na.rm = TRUE),
                total_potential1 = sum(potential1, na.rm = TRUE),
                total_target = sum(target, na.rm = TRUE)) %>% 
      ungroup() %>% 
      mutate(total_fte = total_freq / 8 * 12 / 185,
             total_cost = total_fte * 70000,
             total_productivity = total_target / total_fte,
             total_productivity = ifelse(is.na(total_productivity) | is.nan(total_productivity) | is.infinite(total_productivity),
                                         0,
                                         total_productivity),
             total_roi = (total_target - total_cost) / total_cost)
    
    prov.data <- total.prov.data %>% 
      left_join(actual.prov.data, by  = "province") %>% 
      left_join(covered.prov.data, by = "province") %>% 
      mutate_all(function(x) {ifelse(is.na(x), 0, x)}) %>% 
      mutate(uncovered_hospital_num = total_hospital_num - covered_hospital_num,
             uncovered_city_num = total_city_num - covered_city_num,
             uncovered_freq = total_freq - covered_freq,
             uncovered_potential0 = total_potential0 - covered_potential0,
             uncovered_potential1 = total_potential1 - covered_potential1,
             uncovered_target = total_target - covered_target,
             uncovered_fte = uncovered_freq / 8 * 12 / 185,
             uncovered_cost = uncovered_fte * 70000,
             uncovered_productivity = uncovered_target / uncovered_fte,
             uncovered_productivity = ifelse(is.na(uncovered_productivity) | is.nan(uncovered_productivity) | is.infinite(uncovered_productivity),
                                             0,
                                             uncovered_productivity),
             uncovered_roi = (uncovered_target - uncovered_cost) / uncovered_cost)
    
    prov.data
  })
  
  ## plot1 ----
  ProvPlot1 <- reactive({
    c(input$go, input$productivity, input$growth, input$kpi1)
    isolate({
      if (is.null(ProvData()) | is.null(input$kpi1))
        return(NULL)
      if (nrow(ProvData()) == 0)
        return(NULL)
      
      plot.data <- ProvData()[c("province",
                                paste0("actual_", input$kpi1),
                                paste0("covered_", input$kpi1),
                                paste0("uncovered_", input$kpi1),
                                paste0("total_", input$kpi1),
                                "actual_productivity",
                                "covered_productivity",
                                "uncovered_productivity",
                                "total_productivity")]
      colnames(plot.data) <- c("x", "y1", "y2", "y3", "y", "z1", "z2", "z3", "z")
      plot.data <- plot.data %>% 
        # mutate(z1 = format(round(z1, 2), big.mark = ","),
        #        z2 = format(round(z2, 2), big.mark = ","),
        #        z3 = format(round(z3, 2), big.mark = ","),
        #        z = format(round(z, 2), big.mark = ",")) %>% 
        arrange(-y)
      
      plot1 <- plot_ly(hoverinfo = "name+x+y")
      
      plot1 <- plot1 %>% 
        add_bars(x = plot.data$x,
                 y = plot.data$y2,
                 type = "bar",
                 name = "Covered",
                 color = I(options()$covered.color)) %>% 
        add_bars(x = plot.data$x,
                 y = plot.data$y3,
                 type = "bar",
                 name = "Uncovered",
                 color = I(options()$uncovered.color)) %>% 
        add_trace(x = plot.data$x,
                  y = plot.data$z,
                  text = format(round(plot.data$z, 2), big.mark = ","),
                  textfont = list(color = options()$total.line.color),
                  textposition = "middle top",
                  yaxis = "y2",
                  type = "scatter",
                  mode = "lines",
                  name = "Total hospital productivity",
                  color = I(options()$total.line.color)) %>% 
        add_trace(x = plot.data$x,
                  y = plot.data$z2,
                  text = format(round(plot.data$z2, 2), big.mark = ","),
                  textfont = list(color = options()$covered.line.color),
                  textposition = "middle top",
                  yaxis = "y2",
                  type = "scatter",
                  mode = "lines",
                  name = "Covered productivity",
                  color = I(options()$covered.line.color)) %>% 
        layout(
          barmode = "stack",
          showlegend = TRUE,
          legend = list(
            x = 0,
            y = 1.2,
            orientation = "h"
          ),
          margin = list(
            l = 50,
            r = 50,
            b = 50,
            t = 50,
            pad = 4
          ),
          xaxis = list(
            type = "category",
            categoryorder = "array",
            categoryarray = ~plot.data$x,
            showline = FALSE,
            title = "",
            showticklabels = TRUE,
            mirror = "ticks"
          ),
          yaxis = list(
            side = "left",
            showticklabels = TRUE,
            tickformat = ",",
            showline = FALSE,
            showgrid = TRUE,
            zeroline = TRUE,
            title = "",
            range = c(0, max(plot.data$y)*1.2),
            mirror = "ticks"
          ),
          yaxis2 = list(
            overlaying = "y",
            side = "right",
            showticklabels = TRUE,
            # tickformat = ",",
            hoverformat = ",.2f",
            showline = FALSE,
            showgrid = FALSE,
            zeroline = TRUE,
            title = "",
            range = c(0, max(plot.data$z, plot.data$z1)*1.2),
            mirror = "ticks"
          )
        )
      
      if (input$kpi1 == "fte") {
        plot1 <- plot1 %>% 
          layout(
            yaxis = list(
              hoverformat = ",.2f"
            )
          )
      }
      
      plot1
    })
  })
  
  output$HospitalPlot <- renderPlotly({
    ProvPlot1()
  })
  
  ## table1 ----
  ProvTable1 <- reactive({
    c(input$go, input$productivity, input$growth, input$kpi1)
    isolate({
      if (is.null(ProvData()) | is.null(input$kpi1))
        return(NULL)
      if (nrow(ProvData()) == 0)
        return(NULL)
      
      table.data <- ProvData()
      table.data <- table.data[c("province",
                                 paste0("actual_", input$kpi1),
                                 paste0("covered_", input$kpi1),
                                 paste0("uncovered_", input$kpi1),
                                 paste0("total_", input$kpi1))]
      colnames(table.data) <- c("index", "Actual", "Covered", "Uncovered", "Total")
      
      ordering <- arrange(table.data, -`Total`)$index
      table.data <- table.data %>% 
        select(-Total) %>% 
        melt(id.vars = "index", variable.name = "省份") %>% 
        dcast(`省份`~index, value.var = "value") %>% 
        select("省份", ordering)
      
      if (input$kpi1 == "fte") {
        dgt = 2
      } else {
        dgt = 0
      }
      
      DT::datatable(
        table.data,
        rownames = FALSE,
        # extensions = c('FixedColumns', 'Buttons'),
        #filter = 'bottom',
        ##### this sentence need to be changed when new variables added
        options = list(
          # dom = '<"bottom">Bfrtpl',
          # buttons = I('colvis'),
          columnDefs = list(
            list(
              className = 'dt-center',
              targets = '_all'
            )
          ),
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#3C8DBC', 'color': '#fff'});",
            "}"
          ),
          paging = FALSE,
          scrollX = FALSE,
          searching = FALSE,
          ordering = FALSE,
          pageLength = 5,
          lengthChange = FALSE,
          bInfo = FALSE
        )
      ) %>% 
        # formatStyle(
        #   "省份",
        #   target = "row",
        #   # color = styleEqual("Total", options()$table.color),
        #   fontWeight = styleEqual("Total", "bold")
        # ) %>% 
        formatStyle(
          "省份",
          color = options()$table.color,
          fontWeight = "bold"
        ) %>% 
        formatRound(
          columns = TRUE,
          digits = dgt
          # interval = 3,
          # mark = ","
        )
    })
  })
  
  output$HospitalTable <- DT::renderDataTable({
    ProvTable1()
  })
  
  ## share plot ----
  observeEvent(input$go, {
    updateSelectInput(session,
                      inputId = "covered.sku",
                      label = "Selection SKU",
                      choices = input$sku,
                      selected = input$sku)
  })
  
  SharePlot <- reactive({
    c(input$go, input$covered.sku)
    isolate({
      if (is.null(raw()) | is.null(input$covered.sku)) {
        return(NULL)
      }
      
      cum <- raw() %>% 
        distinct() %>% 
        filter(sku %in% input$covered.sku) %>% 
        mutate(freq = ifelse(sku == "Gly",
                             doctor_a * 6 + doctor_b * 4 + doctor_c * 2 + doctor_d * 1,
                             ifelse(sku == "Pentasa SUP",
                                    doctor_a * 4 + doctor_b * 3 + doctor_c * 1 + doctor_d * 1,
                                    ifelse(sku == "Pentasa TAB",
                                           doctor_a * 4 + doctor_b * 3 + doctor_c * 1 + doctor_d * 1,
                                           0)))) %>%
        group_by(hospital, hosp_level, decile, province, city, tier, is, flag) %>% 
        summarise(freq = sum(freq, na.rm = TRUE),
                  potential0 = sum(potential0, na.rm = TRUE),
                  potential1 = sum(potential1, na.rm = TRUE),
                  target = sum(target, na.rm = TRUE)) %>% 
        ungroup() %>% 
        arrange(-potential0) %>%
        mutate(potential0_cumsum = cumsum(potential0),
               potential0_cumctrb = potential0_cumsum / sum(potential0, na.rm = TRUE) * 100,
               fte = freq / 8 * 12 / 185,
               cost = fte * 70000,
               productivity = target / fte,
               productivity = ifelse(is.na(productivity) | is.nan(productivity) | is.infinite(productivity),
                                     0,
                                     productivity),
               # roi = (target - cost) / cost,
               growth = potential1 / potential0 - 1,
               growth = ifelse(is.na(growth),
                               0,
                               growth))
      
      cum1 <- cum %>% 
        filter(flag == 1)
      
      cum2 <- cum %>% 
        filter(flag == 0)
      
      if (is.na(input$kPotnCtrb)) {
        kProp <- 0
      } else {
        kProp <- input$kPotnCtrb
      }
      
      total.data <- cum2 %>% 
        filter(potential0_cumctrb <= kProp,
               !(province %in% input$aban))
      
      if (is.na(input$productivity)) {
        covered.data <- total.data
      } else {
        covered.data <- total.data[which(total.data$productivity >= input$productivity), ]
      }
      
      if (is.na(input$growth)) {
        covered.data <- covered.data
      } else {
        covered.data <- covered.data[which(covered.data$growth >= input$growth/100), ]
      }
      
      plot.data <- covered.data %>% 
        bind_rows(cum1) %>% 
        group_by(hospital, decile) %>% 
        summarise(potential0 = sum(potential0, na.rm = TRUE),
                  target = sum(target, na.rm = TRUE)) %>% 
        ungroup() %>% 
        group_by(decile) %>% 
        summarise(potential0 = sum(potential0, na.rm = TRUE),
                  target = sum(target, na.rm = TRUE),
                  hosp_num = n()) %>% 
        ungroup() %>% 
        mutate(market_share = target / potential0,
               market_share = ifelse(is.na(market_share),
                                     0,
                                     market_share),
               decile = factor(decile, levels = c("D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "D9", "D10"))) %>% 
        arrange(decile)
      
      plot1 <- plot_ly(hoverinfo = "name+x+y")
      
      plot1 <- plot1 %>% 
        add_trace(x = plot.data$decile,
                  y = plot.data$hosp_num,
                  name = "Hospital",
                  type = "bar",
                  # text = plot.data$hosp_num,
                  # textposition = "outside",
                  color = I(options()$covered.color)) %>% 
        add_trace(x = plot.data$decile,
                  y = plot.data$market_share,
                  name = "Market share",
                  yaxis = "y2",
                  type = "scatter",
                  mode = "lines",
                  color = I(options()$share.color)) %>% 
        layout(
          title = "Market Share in Covered Hospitals",
          showlegend = TRUE,
          legend = list(
            x = 0.38,
            y = 1.05,
            orientation = "h"
          ),
          margin = list(
            l = 50,
            r = 50,
            b = 50,
            t = 50,
            pad = 4
          ),
          xaxis = list(
            title = "",
            type = "category",
            # categoryorder = "array",
            # categoryarray = ~c("D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "D9", "D10"),
            showticklabels = TRUE,
            mirror = "ticks"
          ),
          yaxis = list(
            side = "left",
            title = "",
            range = c(0, max(plot.data$hosp_num)*1.2),
            zeroline = TRUE,
            showgrid = TRUE,
            showticklabels = TRUE,
            tickformat = ",",
            hoverformat = ",",
            mirror = "ticks"
          ),
          yaxis2 = list(
            overlaying = "y",
            side = "right",
            title = "",
            range = c(0, max(plot.data$market_share)*1.2),
            zeroline = TRUE,
            showgrid = FALSE,
            showticklabels = TRUE,
            tickformat = "%",
            hoverformat = "%",
            mirror = "ticks"
          )
        )
      
      list("plot" = plot1,
           "plot.data" = plot.data)
    })
  })
  
  output$SharePlot <- renderPlotly({
    SharePlot()$plot
  })
  
  
  ## plot2 ----
  # ProvPlot2 <- eventReactive(input$go, {
  #   if (is.null(ProvData()) | is.null(input$kpi2))
  #     return(NULL)
  #   if (nrow(ProvData()) == 0)
  #     return(NULL)
  #   
  #   plot.data <- ProvData()
  #   plot.data <- plot.data[c("province",
  #                            paste0("covered_", input$kpi2),
  #                            paste0("uncovered_", input$kpi2),
  #                            paste0("total_", input$kpi2))]
  #   colnames(plot.data) <- c("x", "y1", "y2", "y")
  #   plot.data <- arrange(plot.data, -y)
  #   
  #   plot1 <- plot_ly(hoverinfo = "name+x+y")
  #   
  #   plot1 <- plot1 %>% 
  #     add_trace(x = plot.data$x,
  #               y = plot.data$y,
  #               type = "scatter",
  #               mode = "lines",
  #               name = "Total hospital",
  #               color = I(options()$total.color)) %>% 
  #     add_trace(x = plot.data$x,
  #               y = plot.data$y1,
  #               type = "scatter",
  #               mode = "lines",
  #               name = "Covered",
  #               color = I(options()$covered.color)) %>% 
  #     layout(
  #       showlegend = TRUE,
  #       xaxis = list(
  #         type = "category",
  #         categoryorder = "array",
  #         categoryarray = ~plot.data$x,
  #         showgrid = FALSE,
  #         title = "",
  #         mirror = "ticks"
  #       ),
  #       yaxis = list(
  #         showticklabels = TRUE,
  #         tickformat = ",",
  #         hoverformat = ",.2f",
  #         title = "",
  #         mirror = "ticks"
  #       )
  #     )
  #   
  #   plot1
  # })
  # 
  # output$IndexPlot <- renderPlotly({
  #   ProvPlot2()
  # })
  
  ## table2 ----
  # ProvTable2 <- eventReactive(input$go, {
  #   if (is.null(ProvData()) | is.null(input$kpi2))
  #     return(NULL)
  #   if (nrow(ProvData()) == 0)
  #     return(NULL)
  #   
  #   plot.data <- ProvData()
  #   plot.data <- plot.data[c("province",
  #                            paste0("covered_", input$kpi2),
  #                            paste0("uncovered_", input$kpi2),
  #                            paste0("total_", input$kpi2))]
  #   colnames(plot.data) <- c("index", "Covered", "Uncovered", "Total")
  #   
  #   ordering <- arrange(plot.data, -`Total`)$index
  #   plot.data <- melt(plot.data, id.vars = "index", variable.name = "省份") %>% 
  #     dcast(`省份`~index, value.var = "value") %>% 
  #     select("省份", ordering)
  #   
  #   plot.data
  # })
  # 
  # output$IndexTable <- DT::renderDataTable({
  #   if (is.null(ProvTable2()))
  #     return(NULL)
  #   
  #   DT::datatable(
  #     ProvTable2(),
  #     rownames = FALSE,
  #     # extensions = c('FixedColumns', 'Buttons'),
  #     #filter = 'bottom',
  #     ##### this sentence need to be changed when new variables added
  #     options = list(
  #       # dom = '<"bottom">Bfrtpl',
  #       # buttons = I('colvis'),
  #       columnDefs = list(
  #         list(
  #           className = 'dt-center',
  #           targets = '_all'
  #         )
  #       ),
  #       initComplete = JS(
  #         "function(settings, json) {",
  #         "$(this.api().table().header()).css({'background-color': '#3C8DBC', 'color': '#fff'});",
  #         "}"
  #       ),
  #       paging = FALSE,
  #       scrollX = FALSE,
  #       searching = FALSE,
  #       ordering = FALSE,
  #       pageLength = 5,
  #       lengthChange = FALSE,
  #       bInfo = FALSE
  #     )
  #   ) %>% 
  #     formatStyle(
  #       "省份",
  #       target = "row",
  #       # color = styleEqual("Total", options()$table.color),
  #       fontWeight = styleEqual("Total", "bold")
  #     ) %>% 
  #     formatStyle(
  #       "省份",
  #       color = options()$table.color,
  #       fontWeight = "bold"
  #     ) %>% 
  #     formatRound(
  #       columns = TRUE,
  #       digits = 2
  #       # interval = 3,
  #       # mark = ","
  #     )
  # })
  
  ## recommendation calculation data ----
  CalcDataRcmd <- reactive({
    if (is.null(CalcData()) | is.null(input$sku))
      return(NULL)
    
    total.data <- bind_rows(CalcData()$data1, CalcData()$data2) %>% 
      arrange(potential0_cumctrb) %>% 
      mutate(target_cumsum = cumsum(target),
             cost_cumsum = cumsum(cost),
             fte_cumsum = cumsum(fte),
             roi_cumsum = (target_cumsum - cost_cumsum) / cost_cumsum)
    
    if (input$scenario == "Max ROI") {
      mark <- rownames(total.data)[which(abs(total.data$roi_cumsum) == min(abs(total.data$roi_cumsum)))]
      
    } else {
      mark <- rownames(total.data)[length(rownames(total.data))]
    }
    
    cum1 <- filter(total.data, flag == 1)
    cum2 <- filter(total.data, flag == 0)
    
    list("data1" = cum1,
         "data2" = cum2,
         "mark" = mark)
  })
  
  ## recommendation concentration curve ----
  ConcPlotRcmd <- reactive({
    c(input$go, input$scenario)
    isolate({
      if (is.null(CalcDataRcmd()))
        return(NULL)
      
      plot.data <- bind_rows(CalcDataRcmd()$data1, CalcDataRcmd()$data2)
      x.mark <- round(as.numeric(CalcDataRcmd()$mark))
      y.mark <- ifelse(x.mark == "0", 0, plot.data$potential0_cumctrb[which(rownames(plot.data) == as.character(x.mark))])
      
      plot1 <- plot_ly(hoverinfo = "x+y")
      plot1 <- plot1 %>% 
        add_trace(x = as.numeric(rownames(plot.data)),
                  y = plot.data$potential0_cumctrb,
                  type = "scatter",
                  mode = "lines",
                  color = I(options()$total.color)) %>% 
        add_markers(x = x.mark,
                    y = y.mark,
                    color = I(options()$marker.color)) %>% 
        # add_segments(x = x.mark,
        #              xend = x.mark,
        #              y = 0,
        #              yend = y.mark,
        #              color = "red") %>%
        # add_segments(x = 0,
        #              xend = x.mark,
        #              y = y.mark,
        #              yend = y.mark,
        #              color = "red") %>%
        layout(
          showlegend = FALSE,
          xaxis = list(
            showticklabels = TRUE,
            tickformat = ",",
            showline = FALSE,
            zeroline = TRUE,
            title = "",
            mirror = "ticks"
          ),
          yaxis = list(
            showticklabels = TRUE,
            ticksuffix = "%",
            showline = FALSE,
            zeroline = TRUE,
            title = "",
            hoverformat = ".2f",
            mirror = "ticks"
          ),
          shapes = list(
            list(
              type = "rect",
              fillcolor = options()$square.color,
              line = list(color = options()$square.color),
              opacity = 0.2,
              x0 = 0,
              x1 = x.mark,
              xref = "x",
              y0 = 0,
              y1 = y.mark,
              yref = "y"
            )
          )
        )
      
      plot1
    })
  })
  
  output$ConcRcmd <- renderPlotly({
    ConcPlotRcmd()
  })
  
  ## recommendation segmentation ----
  SegDataRcmd <- reactive({
    c(input$go, input$scenario)
    isolate({
      if (is.null(CalcDataRcmd()))
        return(NULL)
      
      seg.data <- CalcDataRcmd()$data2 %>% 
        filter(!(province %in% input$aban)) %>% 
        bind_rows(CalcDataRcmd()$data1)
      
      total.data <- bind_rows(CalcDataRcmd()$data1, CalcDataRcmd()$data2)
      
      kRow <- as.numeric(CalcDataRcmd()$mark)
      kProp <- round(total.data$potential0_cumctrb[which(rownames(total.data) == as.character(kRow))], 2)
      kGrMean <- sum(seg.data$potential1, na.rm = TRUE) / sum(seg.data$potential0, na.rm = TRUE) - 1
      
      seg.a <- seg.data %>% 
        filter(potential0_cumctrb <= kProp,
               growth >= kGrMean) %>% 
        mutate(hospital_num = n(),
               city_num = length(sort(unique(city)))) %>% 
        group_by(hospital_num, city_num) %>% 
        summarise(fte = sum(fte, na.rm = TRUE),
                  cost = sum(cost, na.rm = TRUE),
                  target = sum(target, na.rm = TRUE)) %>% 
        ungroup() %>% 
        mutate(roi = (target - cost) / cost * 100,
               productivity = target / fte) %>% 
        mutate(hospital_num = format(hospital_num, big.mark = ","),
               city_num = format(city_num, big.mark = ","),
               fte = format(round(fte,1), big.mark = ","),
               roi = paste0(round(roi, 1), "%"),
               productivity = format(round(productivity, 1), big.mark = ",")) %>% 
        select("Number of Hospital" = "hospital_num",
               "Number of City" = "city_num",
               "FTE" = "fte",
               "ROI" = "roi",
               "Productivity" = "productivity") %>% 
        melt(id.vars = NULL)
      
      seg.b <- seg.data %>% 
        filter(potential0_cumctrb <= kProp,
               growth < kGrMean) %>% 
        mutate(hospital_num = n(),
               city_num = length(sort(unique(city)))) %>% 
        group_by(hospital_num, city_num) %>% 
        summarise(fte = sum(fte, na.rm = TRUE),
                  cost = sum(cost, na.rm = TRUE),
                  target = sum(target, na.rm = TRUE)) %>% 
        ungroup() %>% 
        mutate(roi = (target - cost) / cost * 100,
               productivity = target / fte) %>% 
        mutate(hospital_num = format(hospital_num, big.mark = ","),
               city_num = format(city_num, big.mark = ","),
               fte = format(round(fte,1), big.mark = ","),
               roi = paste0(round(roi, 1), "%"),
               productivity = format(round(productivity, 1), big.mark = ",")) %>% 
        select("Number of Hospital" = "hospital_num",
               "Number of City" = "city_num",
               "FTE" = "fte",
               "ROI" = "roi",
               "Productivity" = "productivity") %>% 
        melt(id.vars = NULL)
      
      seg.c <- seg.data %>% 
        filter(potential0_cumctrb > kProp,
               growth >= kGrMean) %>% 
        mutate(hospital_num = n(),
               city_num = length(sort(unique(city)))) %>% 
        group_by(hospital_num, city_num) %>% 
        summarise(fte = sum(fte, na.rm = TRUE),
                  cost = sum(cost, na.rm = TRUE),
                  target = sum(target, na.rm = TRUE)) %>% 
        ungroup() %>% 
        mutate(roi = (target - cost) / cost * 100,
               productivity = target / fte) %>% 
        mutate(hospital_num = format(hospital_num, big.mark = ","),
               city_num = format(city_num, big.mark = ","),
               fte = format(round(fte,1), big.mark = ","),
               roi = paste0(round(roi, 1), "%"),
               productivity = format(round(productivity, 1), big.mark = ",")) %>% 
        select("Number of Hospital" = "hospital_num",
               "Number of City" = "city_num",
               "FTE" = "fte",
               "ROI" = "roi",
               "Productivity" = "productivity") %>% 
        melt(id.vars = NULL)
      
      seg.d <- seg.data %>% 
        filter(potential0_cumctrb > kProp,
               growth < kGrMean) %>% 
        mutate(hospital_num = n(),
               city_num = length(sort(unique(city)))) %>% 
        group_by(hospital_num, city_num) %>% 
        summarise(fte = sum(fte, na.rm = TRUE),
                  cost = sum(cost, na.rm = TRUE),
                  target = sum(target, na.rm = TRUE)) %>% 
        ungroup() %>% 
        mutate(roi = (target - cost) / cost * 100,
               productivity = target / fte) %>% 
        mutate(hospital_num = format(hospital_num, big.mark = ","),
               city_num = format(city_num, big.mark = ","),
               fte = format(round(fte,1), big.mark = ","),
               roi = paste0(round(roi, 1), "%"),
               productivity = format(round(productivity, 1), big.mark = ",")) %>% 
        select("Number of Hospital" = "hospital_num",
               "Number of City" = "city_num",
               "FTE" = "fte",
               "ROI" = "roi",
               "Productivity" = "productivity") %>% 
        melt(id.vars = NULL)
      
      seg.na <- data.frame("variable" = c("Number of Hospital", "Number of City", 
                                          "FTE", "ROI", "Productivity"),
                           "value" = c(0, 0, 0, 0, 0))
      
      seg.list <- list("seg.a" = seg.a,
                       "seg.b" = seg.b,
                       "seg.c" = seg.c,
                       "seg.d" = seg.d)
      
      for (i in names(seg.list)) {
        if (nrow(seg.list[[i]]) == 0) {
          seg.list[[i]] <- seg.na
        }
      }
      
      seg.list[["kProp"]] <- kProp
      seg.list[["kGrMean"]] <- kGrMean
      
      seg.list
    })
  })
  
  output$segRcmd.v <- renderText({
    if (is.null(SegDataRcmd())) {
      "Potential Cumulation Contribution = NA%"
    } else {
      paste0("Potential Cumulation Contribution = ", SegDataRcmd()$kProp, "%")
    }
  })
  
  output$segRcmd.h1 <- renderText({
    if (is.null(SegDataRcmd())) {
      "Growth Rate >= NA%"
    } else {
      paste0("Growth Rate >= ", format(SegDataRcmd()$kGrMean, digits = 2L))
    }
  })
  
  output$segRcmd.h2 <- renderText({
    if (is.null(SegDataRcmd())) {
      "Growth Rate < NA%"
    } else {
      paste0("Growth Rate < ", format(SegDataRcmd()$kGrMean, digits = 2L))
    }
  })
  
  output$TableARcmd <- DT::renderDataTable({
    if (is.null(SegDataRcmd()))
      return(NULL)
    
    DT::datatable(
      SegDataRcmd()$seg.a,
      rownames = FALSE,
      colnames = NULL,
      options = list(
        columnDefs = list(
          list(className = "dt-left",
               targets = 0),
          list(className = "dt-right",
               targets = 1)
        ),
        paging = FALSE,
        scrollX = FALSE,
        searching = FALSE,
        ordering = FALSE,
        pageLength = 5,
        lengthChange = FALSE,
        bInfo = FALSE
      )
    ) %>% 
      formatStyle(
        c("variable"),
        fontWeight = "bold"
      ) %>% 
      formatStyle(
        c("variable", "value"),
        backgroundColor = options()$seg.color
      )
  })
  
  output$TableBRcmd <- DT::renderDataTable({
    if (is.null(SegDataRcmd()))
      return(NULL)
    
    DT::datatable(
      SegDataRcmd()$seg.b,
      rownames = FALSE,
      colnames = NULL,
      options = list(
        columnDefs = list(
          list(className = "dt-left",
               targets = 0),
          list(className = "dt-right",
               targets = 1)
        ),
        paging = FALSE,
        scrollX = FALSE,
        searching = FALSE,
        ordering = FALSE,
        pageLength = 5,
        lengthChange = FALSE,
        bInfo = FALSE
      )
    ) %>% 
      formatStyle(
        c("variable"),
        fontWeight = "bold"
      ) %>% 
      formatStyle(
        c("variable", "value"),
        backgroundColor = options()$seg.color
      )
  })
  
  output$TableCRcmd <- DT::renderDataTable({
    if (is.null(SegDataRcmd()))
      return(NULL)
    
    DT::datatable(
      SegDataRcmd()$seg.c,
      rownames = FALSE,
      colnames = NULL,
      options = list(
        columnDefs = list(
          list(className = "dt-left",
               targets = 0),
          list(className = "dt-right",
               targets = 1)
        ),
        paging = FALSE,
        scrollX = FALSE,
        searching = FALSE,
        ordering = FALSE,
        pageLength = 5,
        lengthChange = FALSE,
        bInfo = FALSE
      )
    ) %>% 
      formatStyle(
        c("variable"),
        fontWeight = "bold"
      ) %>% 
      formatStyle(
        c("variable", "value"),
        backgroundColor = options()$seg.color
      )
  })
  
  output$TableDRcmd <- DT::renderDataTable({
    if (is.null(SegDataRcmd()))
      return(NULL)
    
    DT::datatable(
      SegDataRcmd()$seg.d,
      rownames = FALSE,
      colnames = NULL,
      options = list(
        columnDefs = list(
          list(className = "dt-left",
               targets = 0),
          list(className = "dt-right",
               targets = 1)
        ),
        paging = FALSE,
        scrollX = FALSE,
        searching = FALSE,
        ordering = FALSE,
        pageLength = 5,
        lengthChange = FALSE,
        bInfo = FALSE
      )
    ) %>% 
      formatStyle(
        c("variable"),
        fontWeight = "bold"
      ) %>% 
      formatStyle(
        c("variable", "value"),
        backgroundColor = options()$seg.color
      )
  })
  
  ## recommendation province data ----
  ProvDataRcmd <- reactive({
    if (is.null(CalcDataRcmd()))
      return(NULL)
    
    covered.data <- CalcDataRcmd()$data2 %>% 
      filter(row_number() <= as.numeric(CalcDataRcmd()$mark),
             !(province %in% input$aban)) %>% 
      bind_rows(CalcDataRcmd()$data1)
    
    covered.prov.data <- covered.data %>% 
      group_by(province) %>% 
      summarise(covered_hospital_num = n(),
                covered_city_num = length(sort(unique(city))),
                covered_freq = sum(freq, na.rm = TRUE),
                covered_potential0 = sum(potential0, na.rm = TRUE),
                covered_potential1 = sum(potential1, na.rm = TRUE),
                covered_target = sum(target, na.rm = TRUE)) %>% 
      ungroup() %>% 
      mutate(covered_fte = covered_freq / 8 * 12 / 185,
             covered_cost = covered_fte * 70000,
             covered_productivity = covered_target / covered_fte,
             covered_productivity = ifelse(is.na(covered_productivity) | is.nan(covered_productivity) | is.infinite(covered_productivity),
                                           0,
                                           covered_productivity),
             covered_roi = (covered_target - covered_cost) / covered_cost)
    
    covered.prov.data
  })
  
  ## recommendation plot1 ----
  ProvPlot1Rcmd <- reactive({
    c(input$go, input$scenario, input$kpi1.rcmd)
    isolate({
      if (is.null(ProvDataRcmd()) | is.null(input$kpi1.rcmd))
        return(NULL)
      if (nrow(ProvDataRcmd()) == 0)
        return(NULL)
      
      plot.data <- ProvDataRcmd()
      plot.data <- plot.data[c("province", paste0("covered_", input$kpi1.rcmd))]
      colnames(plot.data) <- c("x", "y")
      plot.data <- arrange(plot.data, -y)
      
      plot1 <- plot_ly(hoverinfo = "name+x+y")
      
      plot1 <- plot1 %>% 
        add_bars(x = plot.data$x,
                 y = plot.data$y,
                 type = "bar",
                 name = "Covered",
                 color = I(options()$covered.color)) %>% 
        layout(
          showlegend = TRUE,
          xaxis = list(
            type = "category",
            categoryorder = "array",
            categoryarray = ~plot.data$x,
            title = "",
            showticklabels = TRUE,
            mirror = "ticks"
          ),
          yaxis = list(
            showticklabels = TRUE,
            tickformat = ",",
            showline = FALSE,
            zeroline = TRUE,
            title = "",
            mirror = "ticks"
          )
        )
      
      if (input$kpi1.rcmd == "fte") {
        plot1 <- plot1 %>% 
          layout(
            yaxis = list(
              hoverformat = ",.2f"
            )
          )
      }
      
      plot1
    })
  })
  
  output$HospitalPlotRcmd <- renderPlotly({
    ProvPlot1Rcmd()
  })
  
  ## recommendation table1 ----
  ProvTable1Rcmd <- reactive({
    c(input$go, input$scenario, input$kpi1.rcmd)
    isolate({
      if (is.null(ProvDataRcmd()) | is.null(input$kpi1.rcmd))
        return(NULL)
      if (nrow(ProvDataRcmd()) == 0)
        return(NULL)
      
      table.data <- ProvDataRcmd()
      table.data <- table.data[c("province", paste0("covered_", input$kpi1.rcmd))]
      colnames(table.data) <- c("index", "Covered")
      
      ordering <- arrange(table.data, -`Covered`)$index
      table.data <- melt(table.data, id.vars = "index", variable.name = "省份") %>% 
        dcast(`省份`~index, value.var = "value") %>% 
        select("省份", ordering)
      
      if (input$kpi1 == "fte") {
        dgt = 2
      } else {
        dgt = 0
      }
      
      DT::datatable(
        table.data,
        rownames = FALSE,
        # extensions = c('FixedColumns', 'Buttons'),
        #filter = 'bottom',
        ##### this sentence need to be changed when new variables added
        options = list(
          # dom = '<"bottom">Bfrtpl',
          # buttons = I('colvis'),
          columnDefs = list(
            list(
              className = 'dt-center',
              targets = '_all'
            )
          ),
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#3C8DBC', 'color': '#fff'});",
            "}"
          ),
          paging = FALSE,
          scrollX = FALSE,
          searching = FALSE,
          ordering = FALSE,
          pageLength = 5,
          lengthChange = FALSE,
          bInfo = FALSE
        )
      ) %>% 
        formatStyle(
          "省份",
          color = options()$table.color,
          fontWeight = "bold"
        ) %>% 
        # formatStyle(
        #   "省份",
        #   target = "row",
        #   color = styleEqual("Total", options()$table.color),
        #   fontWeight = styleEqual("Total", "bold")
        # ) %>% 
        formatRound(
          columns = TRUE,
          digits = dgt
          # interval = 3,
          # mark = ","
        )
    })
  })
  
  output$HospitalTableRcmd <- DT::renderDataTable({
    ProvTable1Rcmd()
  })
  
  ## recommendation plot2 ----
  ProvPlot2Rcmd <- reactive({
    c(input$go, input$scenario, input$kpi2.rcmd)
    isolate({
      if (is.null(ProvDataRcmd()) | is.null(input$kpi2.rcmd))
        return(NULL)
      if (nrow(ProvDataRcmd()) == 0)
        return(NULL)
      
      plot.data <- ProvDataRcmd()
      plot.data <- plot.data[c("province", paste0("covered_", input$kpi2.rcmd))]
      colnames(plot.data) <- c("x", "y")
      plot.data <- arrange(plot.data, -y)
      
      plot1 <- plot_ly(hoverinfo = "name+x+y")
      
      plot1 <- plot1 %>% 
        add_trace(x = plot.data$x,
                  y = plot.data$y,
                  type = "scatter",
                  mode = "lines",
                  name = "Covered",
                  color = I(options()$covered.color)) %>% 
        layout(
          showlegend = TRUE,
          xaxis = list(
            type = "category",
            categoryorder = "array",
            categoryarray = ~plot.data$x,
            showgrid = FALSE,
            title = "",
            mirror = "ticks"
          ),
          yaxis = list(
            showticklabels = TRUE,
            # tickformat = ",",
            hoverformat = ",.2f",
            title = "",
            mirror = "ticks"
          )
        )
      
      plot1
    })
  })
  
  output$IndexPlotRcmd <- renderPlotly({
    ProvPlot2Rcmd()
  })
  
  ## recommendation table2 ----
  ProvTable2Rcmd <- reactive({
    c(input$go, input$scenario, input$kpi1.rcmd)
    isolate({
      if (is.null(ProvDataRcmd()) | is.null(input$kpi2.rcmd))
        return(NULL)
      if (nrow(ProvDataRcmd()) == 0)
        return(NULL)
      
      table.data <- ProvDataRcmd()
      table.data <- table.data[c("province", paste0("covered_", input$kpi2.rcmd))]
      colnames(table.data) <- c("index", "Covered")
      
      ordering <- arrange(table.data, -`Covered`)$index
      table.data <- melt(table.data, id.vars = "index", variable.name = "省份") %>% 
        dcast(`省份`~index, value.var = "value") %>% 
        select("省份", ordering)
      
      DT::datatable(
        table.data,
        rownames = FALSE,
        # extensions = c('FixedColumns', 'Buttons'),
        #filter = 'bottom',
        ##### this sentence need to be changed when new variables added
        options = list(
          # dom = '<"bottom">Bfrtpl',
          # buttons = I('colvis'),
          columnDefs = list(
            list(
              className = 'dt-center',
              targets = '_all'
            )
          ),
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#3C8DBC', 'color': '#fff'});",
            "}"
          ),
          paging = FALSE,
          scrollX = FALSE,
          searching = FALSE,
          ordering = FALSE,
          pageLength = 5,
          lengthChange = FALSE,
          bInfo = FALSE
        )
      ) %>% 
        formatStyle(
          "省份",
          color = options()$table.color,
          fontWeight = "bold"
        ) %>% 
        # formatStyle(
        #   "省份",
        #   target = "row",
        #   color = styleEqual("Total", options()$table.color),
        #   fontWeight = styleEqual("Total", "bold")
        # ) %>% 
        formatRound(
          columns = TRUE,
          digits = 2
          # interval = 3,
          # mark = ","
        )
    })
  })
  
  output$IndexTableRcmd <- DT::renderDataTable({
    ProvTable2Rcmd()
  })
  
  ## dimension ----
  DimensionData <- reactive({
    c(input$go, input$dimension)
    isolate({
      if (is.null(CalcData()) | is.null(input$dimension)) {
        return(NULL)
      }
      
      if (is.na(input$kPotnCtrb)) {
        kProp <- 0
      } else {
        kProp <- input$kPotnCtrb
      }
      
      total.data.m <- raw() %>% 
        distinct() %>% 
        filter(sku %in% input$sku) %>% 
        mutate(freq = ifelse(sku == "Gly",
                             doctor_a * 6 + doctor_b * 4 + doctor_c * 2 + doctor_d * 1,
                             ifelse(sku == "Pentasa SUP",
                                    doctor_a * 4 + doctor_b * 3 + doctor_c * 1 + doctor_d * 1,
                                    ifelse(sku == "Pentasa TAB",
                                           doctor_a * 4 + doctor_b * 3 + doctor_c * 1 + doctor_d * 1,
                                           0)))) %>% 
        group_by(sku, hospital, hosp_level, decile, province, city, tier, is, flag) %>% 
        summarise(freq = sum(freq, na.rm = TRUE),
                  potential0 = sum(potential0, na.rm = TRUE),
                  potential1 = sum(potential1, na.rm = TRUE),
                  target = sum(target, na.rm = TRUE)) %>% 
        ungroup() %>% 
        arrange(-potential0) %>% 
        mutate(potential0_cumsum = cumsum(potential0),
               potential0_cumctrb = potential0_cumsum / sum(potential0, na.rm = TRUE) * 100,
               fte = freq / 8 * 12 / 185,
               cost = fte * 70000,
               productivity = target / fte,
               productivity = ifelse(is.na(productivity) | is.nan(productivity) | is.infinite(productivity),
                                     0,
                                     productivity),
               roi = (target - cost) / cost,
               growth = potential1 / potential0 - 1,
               growth = ifelse(is.na(growth),
                               0,
                               growth))
      
      total.data <- filter(total.data.m, flag == 0) %>% 
        filter(potential0_cumctrb <= kProp,
               !(province %in% input$aban))
      
      if (is.na(input$productivity)) {
        covered.data <- total.data
      } else {
        covered.data <- total.data[which(total.data$productivity >= input$productivity), ]
      }
      
      if (is.na(input$growth)) {
        covered.data <- covered.data
      } else {
        covered.data <- covered.data[which(covered.data$growth >= input$growth/100), ]
      }
      
      table.data <- covered.data %>% 
        bind_rows(filter(total.data.m, flag == 1)) %>% 
        group_by_at(vars(one_of(c(input$dimension, "hospital")))) %>% 
        summarise(fte = sum(fte, na.rm = TRUE),
                  cost = sum(cost, na.rm = TRUE),
                  target = sum(target, na.rm = TRUE)) %>% 
        ungroup() %>% 
        group_by_at(vars(one_of(input$dimension))) %>% 
        summarise(fte = sum(fte, na.rm = TRUE),
                  cost = sum(cost, na.rm = TRUE),
                  target = sum(target, na.rm = TRUE),
                  hosp_num = n()) %>% 
        ungroup() %>% 
        mutate(roi = (target - cost) / cost * 100,
               roi = paste0(round(roi, 1), "%"),
               fte = round(fte, 1)) %>% 
        arrange(-fte) %>% 
        select(input$dimension, 
               "Hospital#" = "hosp_num", 
               "FTE" = "fte", 
               "ROI" = "roi")
      
      name.mapping <- data.frame(name0 = c("sku", "province", "city", "tier", "hosp_level"),
                                 name1 = c("SKU", "Province", "City", "City tier", "Hospital level"),
                                 stringsAsFactors = FALSE)
      
      for (i in input$dimension) {
        if (length(colnames(table.data)[which(colnames(table.data) == i)]) == 1) {
          colnames(table.data)[which(colnames(table.data) == i)] <- name.mapping$name1[which(name.mapping$name0 == i)]
        }
      }
      
      t <- DT::datatable(
        table.data,
        rownames = FALSE,
        # extensions = c('FixedColumns', 'Buttons'),
        #filter = 'bottom',
        ##### this sentence need to be changed when new variables added
        options = list(
          # dom = '<"bottom">Bfrtpl',
          # buttons = I('colvis'),
          columnDefs = list(
            list(
              className = 'dt-center',
              targets = '_all'
            )
          ),
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#3C8DBC', 'color': '#fff'});",
            "}"
          ),
          paging = TRUE,
          scrollX = FALSE,
          searching = FALSE,
          ordering = FALSE,
          pageLength = 30,
          lengthChange = FALSE,
          bInfo = FALSE
        )
      )
      
      list(table = t,
           data = table.data)
    })
  })
  
  output$DimensionTable <- DT::renderDataTable({
    DimensionData()$table
  })
  
  ## evaluation ----
  ActualScenario <- reactive({
    if (is.null(CalcData())) {
      return(NULL)
    }
    
    actual <- bind_rows(CalcData()$data1, CalcData()$data2) %>% 
      filter(is == "Y")
    
    actual_aggr <- actual %>% 
      group_by(city) %>% 
      summarise(hosp_num = n(),
                target = sum(target, na.rm = TRUE),
                fte = sum(fte, na.rm = TRUE),
                cost = sum(cost, na.rm = TRUE)) %>% 
      ungroup() %>% 
      summarise(city_num = n(),
                hosp_num = sum(hosp_num, na.rm = TRUE),
                target = sum(target, na.rm = TRUE),
                fte = sum(fte, na.rm = TRUE),
                cost = sum(cost, na.rm = TRUE)) %>% 
      mutate(productivity = target / fte,
             roi = (target - cost) / cost) %>% 
      select("Hospital#" = "hosp_num",
             "City#" = "city_num",
             "FTE" = "fte",
             "Avg. Productivity" = "productivity",
             "ROI" = "roi") %>% 
      melt(id.vars = NULL) %>% 
      select("index" = "variable",
             "actual" = "value")
    
    list("actual" = actual,
         "actual_aggr" = actual_aggr)
  })
  
  MaxScenario <- reactive({
    if (is.null(CalcData())) {
      return(NULL)
    }
    
    maxx <- bind_rows(CalcData()$data1, CalcData()$data2) %>% 
      arrange(potential0_cumctrb) %>% 
      mutate(target_cumsum = cumsum(target),
             cost_cumsum = cumsum(cost),
             # fte_cumsum = cumsum(fte),
             roi_cumsum = (target_cumsum - cost_cumsum) / cost_cumsum)
    
    mark <- rownames(maxx)[which(abs(maxx$roi_cumsum) == min(abs(maxx$roi_cumsum)))]
    
    maxx <- CalcData()$data2 %>% 
      filter(row_number() <= mark,
             !(province %in% input$aban)) %>% 
      bind_rows(CalcData()$data1)
    
    max_aggr <- maxx %>% 
      group_by(city) %>% 
      summarise(hosp_num = n(),
                target = sum(target, na.rm = TRUE),
                fte = sum(fte, na.rm = TRUE),
                cost = sum(cost, na.rm = TRUE)) %>% 
      ungroup() %>% 
      summarise(city_num = n(),
                hosp_num = sum(hosp_num, na.rm = TRUE),
                target = sum(target, na.rm = TRUE),
                fte = sum(fte, na.rm = TRUE),
                cost = sum(cost, na.rm = TRUE)) %>% 
      mutate(productivity = target / fte,
             roi = (target - cost) / cost) %>% 
      select("Hospital#" = "hosp_num",
             "City#" = "city_num",
             "FTE" = "fte",
             "Avg. Productivity" = "productivity",
             "ROI" = "roi") %>% 
      melt(id.vars = NULL) %>% 
      select("index" = "variable",
             "max" = "value")
    
    list("max" = maxx,
         "max_aggr" = max_aggr)
  })
  
  observeEvent(input$record1, {
    if (is.na(input$kPotnCtrb)) {
      kProp <- 0
    } else {
      kProp <- input$kPotnCtrb
    }
    
    if (is.na(input$productivity)) {
      kProductivity <- 0
    } else {
      kProductivity <- input$productivity
    }
    
    if (is.na(input$growth)) {
      kGrowth <- -Inf
    } else {
      kGrowth <- input$growth/100
    }
    
    scenario1 <<- list(kPotnCtrb = kProp,
                       aban = input$aban,
                       productivity = kProductivity,
                       growth = kGrowth)
  })
  
  observeEvent(input$record2, {
    if (is.na(input$kPotnCtrb)) {
      kProp <- 0
    } else {
      kProp <- input$kPotnCtrb
    }
    
    if (is.na(input$productivity)) {
      kProductivity <- 0
    } else {
      kProductivity <- input$productivity
    }
    
    if (is.na(input$growth)) {
      kGrowth <- -Inf
    } else {
      kGrowth <- input$growth/100
    }
    
    scenario2 <<- list(kPotnCtrb = kProp,
                       aban = input$aban,
                       productivity = kProductivity,
                       growth = kGrowth)
  })
  
  Scenario1 <- eventReactive(input$record1, {
    if (is.null(CalcData())) {
      return(NULL)
    }
    
    scenario <- CalcData()$data2 %>% 
      filter(productivity >= scenario1$productivity,
             growth >= scenario1$growth) %>% 
      filter(potential0_cumctrb <= scenario1$kPotnCtrb,
             !(province %in% scenario1$aban)) %>% 
      bind_rows(CalcData()$data1) %>% 
      mutate(market_share = target / potential0,
             market_share = ifelse(is.na(market_share),
                                   0,
                                   market_share))
    
    scenario_aggr <- scenario %>% 
      group_by(city) %>% 
      summarise(hosp_num = n(),
                target = sum(target, na.rm = TRUE),
                fte = sum(fte, na.rm = TRUE),
                cost = sum(cost, na.rm = TRUE)) %>% 
      ungroup() %>% 
      summarise(city_num = n(),
                hosp_num = sum(hosp_num, na.rm = TRUE),
                target = sum(target, na.rm = TRUE),
                fte = sum(fte, na.rm = TRUE),
                cost = sum(cost, na.rm = TRUE)) %>% 
      mutate(productivity = target / fte,
             productivity = ifelse(is.na(productivity),
                                   0,
                                   productivity),
             roi = (target - cost) / cost,
             roi = ifelse(is.na(roi),
                          0,
                          roi)) %>% 
      select("Hospital#" = "hosp_num",
             "City#" = "city_num",
             "FTE" = "fte",
             "Avg. Productivity" = "productivity",
             "ROI" = "roi") %>% 
      melt(id.vars = NULL) %>% 
      select("index" = "variable",
             "scenario1" = "value")
    
    list("scenario" = scenario,
         "scenario_aggr" = scenario_aggr)
  })
  
  Scenario2 <- eventReactive(input$record2, {
    if (is.null(CalcData())) {
      return(NULL)
    }
    
    scenario <- CalcData()$data2 %>% 
      filter(productivity >= scenario2$productivity,
             growth >= scenario2$growth) %>% 
      filter(potential0_cumctrb <= scenario2$kPotnCtrb,
             !(province %in% scenario2$aban)) %>% 
      bind_rows(CalcData()$data1) %>% 
      mutate(market_share = target / potential0,
             market_share = ifelse(is.na(market_share),
                                   0,
                                   market_share))
    
    scenario_aggr <- scenario %>% 
      group_by(city) %>% 
      summarise(hosp_num = n(),
                target = sum(target, na.rm = TRUE),
                fte = sum(fte, na.rm = TRUE),
                cost = sum(cost, na.rm = TRUE)) %>% 
      ungroup() %>% 
      summarise(city_num = n(),
                hosp_num = sum(hosp_num, na.rm = TRUE),
                target = sum(target, na.rm = TRUE),
                fte = sum(fte, na.rm = TRUE),
                cost = sum(cost, na.rm = TRUE)) %>% 
      mutate(productivity = target / fte,
             productivity = ifelse(is.na(productivity),
                                   0,
                                   productivity),
             roi = (target - cost) / cost,
             roi = ifelse(is.na(roi),
                          0,
                          roi)) %>% 
      select("Hospital#" = "hosp_num",
             "City#" = "city_num",
             "FTE" = "fte",
             "Avg. Productivity" = "productivity",
             "ROI" = "roi") %>% 
      melt(id.vars = NULL) %>% 
      select("index" = "variable",
             "scenario2" = "value")
    
    list("scenario" = scenario,
         "scenario_aggr" = scenario_aggr)
  })
  
  Evaluation <- reactive({
    if (is.null(ActualScenario()) | is.null(MaxScenario()) | is.null(Scenario1()) | is.null(Scenario2())) {
      return(NULL)
    }
    
    evaluation1 <- ActualScenario()$actual_aggr %>% 
      left_join(MaxScenario()$max_aggr, by = "index") %>% 
      left_join(Scenario1()$scenario_aggr, by = "index") %>% 
      left_join(Scenario2()$scenario_aggr, by = "index") %>% 
      mutate(max_actual = max / actual - 1,
             scenario1_actual = scenario1 / actual - 1,
             scenario1_max = scenario1 / max - 1,
             scenario2_actual = scenario2 / actual - 1,
             scenario2_max = scenario2 / max - 1) %>% 
      mutate(max_actual = paste0(round(max_actual*100, 1), "%"),
             scenario1_actual = paste0(round(scenario1_actual*100, 1), "%"),
             scenario1_max = paste0(round(scenario1_max*100, 1), "%"),
             scenario2_actual = paste0(round(scenario2_actual*100, 1), "%"),
             scenario2_max = paste0(round(scenario2_max*100, 1), "%")) %>% 
      select("Index" = "index",
             "Max vs. Actual" = "max_actual",
             "Scenario Ⅰ vs. Actual" = "scenario1_actual",
             "Scenario Ⅰ vs. Max" = "scenario1_max",
             "Scenario Ⅱ vs. Actual" = "scenario2_actual",
             "Scenario Ⅱ vs. Max" = "scenario2_max")
    
    evaluation2 <- ActualScenario()$actual_aggr %>% 
      left_join(MaxScenario()$max_aggr, by = "index") %>% 
      left_join(Scenario1()$scenario_aggr, by = "index") %>% 
      left_join(Scenario2()$scenario_aggr, by = "index") %>% 
      melt() %>% 
      dcast(variable~index) %>% 
      mutate(`Hospital#` = format(`Hospital#`, big.mark = ","),
             `City#` = format(`City#`, big.mark = ","),
             FTE = format(round(FTE, 2), big.mark = ","),
             `Avg. Productivity` = format(round(`Avg. Productivity`, 2), big.mark = ","),
             ROI = paste0(round(ROI*100, 1), "%")) %>% 
      melt(id.vars = "variable", variable.name = "index") %>% 
      dcast(index~variable) %>% 
      select("Index" = "index",
             "Actual" = "actual",
             "Max" = "max",
             "Scenario Ⅰ" = "scenario1",
             "Scenario Ⅱ" = "scenario2")
    
    evaluation <- left_join(evaluation2, evaluation1, by = "Index")
    
    DT::datatable(
      evaluation,
      rownames = FALSE,
      options = list(
        columnDefs = list(
          list(className = "dt-center",
               targets = "_all")
        ),
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#3C8DBC', 'color': '#fff'});",
          "}"
        ),
        paging = FALSE,
        scrollX = FALSE,
        searching = FALSE,
        ordering = FALSE,
        pageLength = 5,
        lengthChange = FALSE,
        bInfo = FALSE
      )
    ) %>% 
      formatStyle(
        "Index",
        fontWeight = "bold"
      )
  })
  
  output$EvaluationTable <- DT::renderDataTable({
    Evaluation()
  })
  
  ## download ----
  output$DownloadSeg <- downloadHandler(
    filename = function() {
      "Hospital_segmentation.csv"
    },
    
    content = function(file) {
      if (is.na(input$kPotnCtrb)) {
        kProp <- 0
      } else {
        kProp <- input$kPotnCtrb
      }
      
      seg.data <- raw() %>% 
        distinct() %>% 
        filter(sku %in% input$sku) %>% 
        mutate(freq = ifelse(sku == "Gly",
                             doctor_a * 6 + doctor_b * 4 + doctor_c * 2 + doctor_d * 1,
                             ifelse(sku == "Pentasa SUP",
                                    doctor_a * 4 + doctor_b * 3 + doctor_c * 1 + doctor_d * 1,
                                    ifelse(sku == "Pentasa TAB",
                                           doctor_a * 4 + doctor_b * 3 + doctor_c * 1 + doctor_d * 1,
                                           0)))) %>% 
        group_by(sku, hospital, hosp_level, decile, province, city, tier, is, flag) %>% 
        summarise(freq = sum(freq, na.rm = TRUE),
                  potential0 = sum(potential0, na.rm = TRUE),
                  potential1 = sum(potential1, na.rm = TRUE),
                  target = sum(target, na.rm = TRUE)) %>% 
        ungroup() %>% 
        arrange(-potential0) %>% 
        mutate(potential0_cumsum = cumsum(potential0),
               potential0_cumctrb = potential0_cumsum / sum(potential0, na.rm = TRUE) * 100,
               fte = freq / 8 * 12 / 185,
               cost = fte * 70000,
               productivity = target / fte,
               productivity = ifelse(is.na(productivity) | is.nan(productivity) | is.infinite(productivity),
                                     0,
                                     productivity),
               roi = (target - cost) / cost,
               growth = potential1 / potential0 - 1,
               growth = ifelse(is.na(growth),
                               0,
                               growth))
      
      seg.data.m <- seg.data %>% 
        filter(flag == 0) %>% 
        filter(!(province %in% input$aban)) %>% 
        bind_rows(filter(seg.data, flag == 1)) %>% 
        mutate(market_share = target / potential0,
               market_share = ifelse(is.na(market_share),
                                     0,
                                     market_share))
      
      if (input$growth_share == "Growth Rate") {
        if (is.na(input$kGrowth)) {
          kIndex <- -Inf
        } else {
          kIndex <- input$kGrowth/100
        }
        
        seg.total <- seg.data %>% 
          mutate(segment = ifelse(potential0_cumctrb <= kProp & growth >= kIndex,
                                  "A",
                                  ifelse(potential0_cumctrb <= kProp & growth < kIndex,
                                         "B",
                                         ifelse(potential0_cumctrb > kProp & growth >= kIndex,
                                                "C",
                                                ifelse(potential0_cumctrb > kProp & growth < kIndex,
                                                       "D",
                                                       "0")))))
        
      } else if (input$growth_share == "Market Share") {
        if (is.na(input$kShare)) {
          kIndex <- 0
        } else {
          kIndex <- input$kShare/100
        }
        
        seg.total <- seg.data %>% 
          mutate(segment = ifelse(potential0_cumctrb <= kProp & market_share >= kIndex,
                                  "A",
                                  ifelse(potential0_cumctrb <= kProp & market_share < kIndex,
                                         "B",
                                         ifelse(potential0_cumctrb > kProp & market_share >= kIndex,
                                                "C",
                                                ifelse(potential0_cumctrb > kProp & market_share < kIndex,
                                                       "D",
                                                       "0")))))
        
      }
      
      write.csv(seg.total, file, row.names = FALSE, fileEncoding = "GB2312")
    }
  )
  
  output$DownloadProv <- downloadHandler(
    filename = function() {
      paste0("Province_", input$kpi1, "_data.csv")
    },
    
    content = function(file) {
      if (is.na(input$kPotnCtrb)) {
        kProp <- 0
      } else {
        kProp <- input$kPotnCtrb
      }
      
      total.data <- raw() %>% 
        distinct() %>% 
        filter(sku %in% input$sku) %>% 
        mutate(freq = ifelse(sku == "Gly",
                             doctor_a * 6 + doctor_b * 4 + doctor_c * 2 + doctor_d * 1,
                             ifelse(sku == "Pentasa SUP",
                                    doctor_a * 4 + doctor_b * 3 + doctor_c * 1 + doctor_d * 1,
                                    ifelse(sku == "Pentasa TAB",
                                           doctor_a * 4 + doctor_b * 3 + doctor_c * 1 + doctor_d * 1,
                                           0)))) %>% 
        group_by(sku, hospital, hosp_level, decile, province, city, tier, is, flag) %>% 
        summarise(freq = sum(freq, na.rm = TRUE),
                  potential0 = sum(potential0, na.rm = TRUE),
                  potential1 = sum(potential1, na.rm = TRUE),
                  target = sum(target, na.rm = TRUE)) %>% 
        ungroup() %>% 
        arrange(-potential0) %>% 
        mutate(potential0_cumsum = cumsum(potential0),
               potential0_cumctrb = potential0_cumsum / sum(potential0, na.rm = TRUE) * 100,
               fte = freq / 8 * 12 / 185,
               cost = fte * 70000,
               productivity = target / fte,
               productivity = ifelse(is.na(productivity) | is.nan(productivity) | is.infinite(productivity),
                                     0,
                                     productivity),
               roi = (target - cost) / cost,
               growth = potential1 / potential0 - 1,
               growth = ifelse(is.na(growth),
                               0,
                               growth))
      
      total.data.m <- total.data %>% 
        filter(flag == 0) %>% 
        filter(!(province %in% input$aban),
               potential0_cumctrb <= kProp) %>% 
        bind_rows(filter(total.data, flag == 1))
      
      if (is.na(input$productivity)) {
        covered.data <- total.data.m
      } else {
        covered.data <- total.data.m[which(total.data.m$productivity >= input$productivity), ]
      }
      
      if (is.na(input$growth)) {
        covered.data <- covered.data
      } else {
        covered.data <- covered.data[which(covered.data$growth >= input$growth/100), ]
      }
      
      actual.prov.data <- covered.data %>% 
        filter(is == "Y") %>% 
        group_by(sku, province) %>% 
        summarise(actual_hospital_num = n(),
                  actual_city_num = length(sort(unique(city))),
                  actual_freq = sum(freq, na.rm = TRUE),
                  actual_potential0 = sum(potential0, na.rm = TRUE),
                  actual_potential1 = sum(potential1, na.rm = TRUE),
                  actual_target = sum(target, na.rm = TRUE)) %>% 
        ungroup() %>% 
        mutate(actual_fte = actual_freq / 8 * 12 / 185,
               actual_cost = actual_fte * 70000,
               actual_productivity = actual_target / actual_fte,
               actual_productivity = ifelse(is.na(actual_productivity) | is.nan(actual_productivity) | is.infinite(actual_productivity),
                                            0,
                                            actual_productivity),
               actual_roi = (actual_target - actual_cost) / actual_cost)
      
      covered.prov.data <- covered.data %>% 
        group_by(sku, province) %>% 
        summarise(covered_hospital_num = n(),
                  covered_city_num = length(sort(unique(city))),
                  covered_freq = sum(freq, na.rm = TRUE),
                  covered_potential0 = sum(potential0, na.rm = TRUE),
                  covered_potential1 = sum(potential1, na.rm = TRUE),
                  covered_target = sum(target, na.rm = TRUE)) %>% 
        ungroup() %>% 
        mutate(covered_fte = covered_freq / 8 * 12 / 185,
               covered_cost = covered_fte * 70000,
               covered_productivity = covered_target / covered_fte,
               covered_productivity = ifelse(is.na(covered_productivity) | is.nan(covered_productivity) | is.infinite(covered_productivity),
                                             0,
                                             covered_productivity),
               covered_roi = (covered_target - covered_cost) / covered_cost)
      
      total.prov.data <- total.data %>% 
        group_by(sku, province) %>% 
        summarise(total_hospital_num = n(),
                  total_city_num = length(sort(unique(city))),
                  total_freq = sum(freq, na.rm = TRUE),
                  total_potential0 = sum(potential0, na.rm = TRUE),
                  total_potential1 = sum(potential1, na.rm = TRUE),
                  total_target = sum(target, na.rm = TRUE)) %>% 
        ungroup() %>% 
        mutate(total_fte = total_freq / 8 * 12 / 185,
               total_cost = total_fte * 70000,
               total_productivity = total_target / total_fte,
               total_productivity = ifelse(is.na(total_productivity) | is.nan(total_productivity) | is.infinite(total_productivity),
                                           0,
                                           total_productivity),
               total_roi = (total_target - total_cost) / total_cost)
      
      prov.data <- total.prov.data %>% 
        left_join(actual.prov.data, by  = c("sku", "province")) %>% 
        left_join(covered.prov.data, by = c("sku", "province")) %>% 
        mutate_all(function(x) {ifelse(is.na(x), 0, x)}) %>% 
        mutate(uncovered_hospital_num = total_hospital_num - covered_hospital_num,
               uncovered_city_num = total_city_num - covered_city_num,
               uncovered_freq = total_freq - covered_freq,
               uncovered_potential0 = total_potential0 - covered_potential0,
               uncovered_potential1 = total_potential1 - covered_potential1,
               uncovered_target = total_target - covered_target,
               uncovered_fte = uncovered_freq / 8 * 12 / 185,
               uncovered_cost = uncovered_fte * 70000,
               uncovered_productivity = uncovered_target / uncovered_fte,
               uncovered_productivity = ifelse(is.na(uncovered_productivity) | is.nan(uncovered_productivity) | is.infinite(uncovered_productivity),
                                               0,
                                               uncovered_productivity),
               uncovered_roi = (uncovered_target - uncovered_cost) / uncovered_cost)
      
      write.csv(prov.data, file, row.names = FALSE, fileEncoding = "GB2312")
    }
  )
  
  output$DownloadHospital <- downloadHandler(
    filename = function() {
      "Covered_hospital_market_share.csv"
    },
    
    content = function(file) {
      if (is.na(input$kPotnCtrb)) {
        kProp <- 0
      } else {
        kProp <- input$kPotnCtrb
      }
      
      total.data <- raw() %>% 
        distinct() %>% 
        filter(sku %in% input$covered.sku) %>% 
        mutate(freq = ifelse(sku == "Gly",
                             doctor_a * 6 + doctor_b * 4 + doctor_c * 2 + doctor_d * 1,
                             ifelse(sku == "Pentasa SUP",
                                    doctor_a * 4 + doctor_b * 3 + doctor_c * 1 + doctor_d * 1,
                                    ifelse(sku == "Pentasa TAB",
                                           doctor_a * 4 + doctor_b * 3 + doctor_c * 1 + doctor_d * 1,
                                           0)))) %>%
        group_by(sku, hospital, hosp_level, decile, province, city, tier, is, flag) %>% 
        summarise(freq = sum(freq, na.rm = TRUE),
                  potential0 = sum(potential0, na.rm = TRUE),
                  potential1 = sum(potential1, na.rm = TRUE),
                  target = sum(target, na.rm = TRUE)) %>% 
        ungroup() %>% 
        arrange(-potential0) %>%
        mutate(potential0_cumsum = cumsum(potential0),
               potential0_cumctrb = potential0_cumsum / sum(potential0, na.rm = TRUE) * 100,
               fte = freq / 8 * 12 / 185,
               cost = fte * 70000,
               productivity = target / fte,
               productivity = ifelse(is.na(productivity) | is.nan(productivity) | is.infinite(productivity),
                                     0,
                                     productivity),
               # roi = (target - cost) / cost,
               growth = potential1 / potential0 - 1,
               growth = ifelse(is.na(growth),
                               0,
                               growth))
      
      total.data.m <- total.data %>% 
        filter(flag == 0) %>% 
        filter(!(province %in% input$aban),
               potential0_cumctrb <= kProp) %>% 
        bind_rows(filter(total.data, flag == 1))
      
      if (is.na(input$productivity)) {
        total.data.m <- total.data.m
      } else {
        total.data.m <- total.data.m[which(total.data.m$productivity >= input$productivity), ]
      }
      
      if (is.na(input$growth)) {
        total.data.m <- total.data.m
      } else {
        total.data.m <- total.data.m[which(total.data.m$growth >= input$growth/100), ]
      }
      
      total.data.m <- total.data.m %>% 
        group_by(sku, hospital, decile) %>% 
        summarise(potential0 = sum(potential0, na.rm = TRUE),
                  target = sum(target, na.rm = TRUE)) %>% 
        ungroup() %>% 
        group_by(sku, decile) %>% 
        summarise(potential0 = sum(potential0, na.rm = TRUE),
                  target = sum(target, na.rm = TRUE),
                  hosp_num = n()) %>% 
        ungroup() %>% 
        mutate(market_share = target / potential0,
               market_share = ifelse(is.na(market_share),
                                     0,
                                     market_share),
               decile = factor(decile, levels = c("D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "D9", "D10"))) %>% 
        arrange(decile)
      
      write.csv(total.data.m, file, row.names = FALSE, fileEncoding = "GB2312")
    }
  )
  
  output$DownloadSel <- downloadHandler(
    filename = function() {
      "Selection_data.csv"
    },
    
    content = function(file) {
      if (is.na(input$kPotnCtrb)) {
        kProp <- 0
      } else {
        kProp <- input$kPotnCtrb
      }
      
      total.data <- raw() %>% 
        distinct() %>% 
        filter(sku %in% input$covered.sku) %>% 
        mutate(freq = ifelse(sku == "Gly",
                             doctor_a * 6 + doctor_b * 4 + doctor_c * 2 + doctor_d * 1,
                             ifelse(sku == "Pentasa SUP",
                                    doctor_a * 4 + doctor_b * 3 + doctor_c * 1 + doctor_d * 1,
                                    ifelse(sku == "Pentasa TAB",
                                           doctor_a * 4 + doctor_b * 3 + doctor_c * 1 + doctor_d * 1,
                                           0)))) %>%
        group_by(sku, hospital, hosp_level, decile, province, city, tier, is, flag) %>% 
        summarise(freq = sum(freq, na.rm = TRUE),
                  potential0 = sum(potential0, na.rm = TRUE),
                  potential1 = sum(potential1, na.rm = TRUE),
                  target = sum(target, na.rm = TRUE)) %>% 
        ungroup() %>% 
        arrange(-potential0) %>%
        mutate(potential0_cumsum = cumsum(potential0),
               potential0_cumctrb = potential0_cumsum / sum(potential0, na.rm = TRUE) * 100,
               fte = freq / 8 * 12 / 185,
               cost = fte * 70000,
               productivity = target / fte,
               productivity = ifelse(is.na(productivity) | is.nan(productivity) | is.infinite(productivity),
                                     0,
                                     productivity),
               # roi = (target - cost) / cost,
               growth = potential1 / potential0 - 1,
               growth = ifelse(is.na(growth),
                               0,
                               growth))
      
      total.data.m <- total.data %>% 
        filter(flag == 0) %>% 
        filter(!(province %in% input$aban),
               potential0_cumctrb <= kProp) %>% 
        bind_rows(filter(total.data, flag == 1))
      
      write.csv(total.data.m, file, row.names = FALSE, fileEncoding = "GB2312")
    }
  )
  
  output$DownloadRcmd <- downloadHandler(
    filename = function() {
      "recommendation_selection_data.csv"
    },
    
    content = function(file) {
      total.data <- raw() %>% 
        distinct() %>% 
        filter(sku %in% input$covered.sku) %>% 
        mutate(freq = ifelse(sku == "Gly",
                             doctor_a * 6 + doctor_b * 4 + doctor_c * 2 + doctor_d * 1,
                             ifelse(sku == "Pentasa SUP",
                                    doctor_a * 4 + doctor_b * 3 + doctor_c * 1 + doctor_d * 1,
                                    ifelse(sku == "Pentasa TAB",
                                           doctor_a * 4 + doctor_b * 3 + doctor_c * 1 + doctor_d * 1,
                                           0)))) %>%
        group_by(sku, hospital, hosp_level, decile, province, city, tier, is, flag) %>% 
        summarise(freq = sum(freq, na.rm = TRUE),
                  potential0 = sum(potential0, na.rm = TRUE),
                  potential1 = sum(potential1, na.rm = TRUE),
                  target = sum(target, na.rm = TRUE)) %>% 
        ungroup() %>% 
        arrange(-potential0) %>%
        mutate(potential0_cumsum = cumsum(potential0),
               potential0_cumctrb = potential0_cumsum / sum(potential0, na.rm = TRUE) * 100,
               fte = freq / 8 * 12 / 185,
               cost = fte * 70000,
               productivity = target / fte,
               productivity = ifelse(is.na(productivity) | is.nan(productivity) | is.infinite(productivity),
                                     0,
                                     productivity),
               # roi = (target - cost) / cost,
               growth = potential1 / potential0 - 1,
               growth = ifelse(is.na(growth),
                               0,
                               growth)) %>% 
        arrange(potential0_cumctrb)
      
      total.data.m <- total.data %>% 
        filter(flag == 0) %>% 
        filter(row_number() > as.numeric(CalcDataRcmd()$mark)) %>% 
        filter(!(province %in% input$aban)) %>% 
        bind_rows(filter(total.data, flag == 1))
      
      write.csv(total.data, file, row.names = FALSE, fileEncoding = "GB2312")
    }
  )
  
  output$DownloadDimension <- downloadHandler(
    filename = function() {
      "Dimension_data.csv"
    },
    
    content = function(file) {
      total.data <- DimensionData()$data
      
      write.csv(total.data, file, row.names = FALSE, fileEncoding = "GB2312")
    }
  )
  
  output$DownloadEvaluation <- downloadHandler(
    filename = function() {
      "Evaluation_data.xlsx"
    },
    
    content = function(file) {
      total.data1 <- raw() %>% 
        distinct() %>% 
        filter(sku %in% input$covered.sku) %>% 
        mutate(freq = ifelse(sku == "Gly",
                             doctor_a * 6 + doctor_b * 4 + doctor_c * 2 + doctor_d * 1,
                             ifelse(sku == "Pentasa SUP",
                                    doctor_a * 4 + doctor_b * 3 + doctor_c * 1 + doctor_d * 1,
                                    ifelse(sku == "Pentasa TAB",
                                           doctor_a * 4 + doctor_b * 3 + doctor_c * 1 + doctor_d * 1,
                                           0)))) %>%
        group_by(sku, hospital, hosp_level, decile, province, city, tier, is, flag) %>% 
        summarise(freq = sum(freq, na.rm = TRUE),
                  potential0 = sum(potential0, na.rm = TRUE),
                  potential1 = sum(potential1, na.rm = TRUE),
                  target = sum(target, na.rm = TRUE)) %>% 
        ungroup() %>% 
        arrange(-potential0) %>%
        mutate(potential0_cumsum = cumsum(potential0),
               potential0_cumctrb = potential0_cumsum / sum(potential0, na.rm = TRUE) * 100,
               fte = freq / 8 * 12 / 185,
               cost = fte * 70000,
               productivity = target / fte,
               productivity = ifelse(is.na(productivity) | is.nan(productivity) | is.infinite(productivity),
                                     0,
                                     productivity),
               # roi = (target - cost) / cost,
               growth = potential1 / potential0 - 1,
               growth = ifelse(is.na(growth),
                               0,
                               growth))
      
      total.data1.m <- total.data1 %>% 
        filter(flag == 0) %>% 
        filter(!(province %in% input$aban)) %>% 
        filter(is == "Y") %>% 
        bind_rows(filter(total.data1, flag == 1))
      
      total.data2 <- raw() %>% 
        distinct() %>% 
        filter(sku %in% input$covered.sku) %>% 
        mutate(freq = ifelse(sku == "Gly",
                             doctor_a * 6 + doctor_b * 4 + doctor_c * 2 + doctor_d * 1,
                             ifelse(sku == "Pentasa SUP",
                                    doctor_a * 4 + doctor_b * 3 + doctor_c * 1 + doctor_d * 1,
                                    ifelse(sku == "Pentasa TAB",
                                           doctor_a * 4 + doctor_b * 3 + doctor_c * 1 + doctor_d * 1,
                                           0)))) %>%
        group_by(sku, hospital, hosp_level, decile, province, city, tier, is, flag) %>% 
        summarise(freq = sum(freq, na.rm = TRUE),
                  potential0 = sum(potential0, na.rm = TRUE),
                  potential1 = sum(potential1, na.rm = TRUE),
                  target = sum(target, na.rm = TRUE)) %>% 
        ungroup() %>% 
        arrange(-potential0) %>%
        mutate(potential0_cumsum = cumsum(potential0),
               potential0_cumctrb = potential0_cumsum / sum(potential0, na.rm = TRUE) * 100,
               fte = freq / 8 * 12 / 185,
               cost = fte * 70000,
               productivity = target / fte,
               productivity = ifelse(is.na(productivity) | is.nan(productivity) | is.infinite(productivity),
                                     0,
                                     productivity),
               # roi = (target - cost) / cost,
               growth = potential1 / potential0 - 1,
               growth = ifelse(is.na(growth),
                               0,
                               growth)) %>% 
        arrange(potential0_cumctrb)
      
      total.data2.m <- total.data2 %>% 
        filter(flag == 0) %>% 
        filter(row_number() > as.numeric(CalcDataRcmd()$mark)) %>% 
        filter(!(province %in% input$aban)) %>% 
        bind_rows(filter(total.data2, flag == 1))
      
      total.data.m <- raw() %>% 
        distinct() %>% 
        filter(sku %in% input$covered.sku) %>% 
        mutate(freq = ifelse(sku == "Gly",
                             doctor_a * 6 + doctor_b * 4 + doctor_c * 2 + doctor_d * 1,
                             ifelse(sku == "Pentasa SUP",
                                    doctor_a * 4 + doctor_b * 3 + doctor_c * 1 + doctor_d * 1,
                                    ifelse(sku == "Pentasa TAB",
                                           doctor_a * 4 + doctor_b * 3 + doctor_c * 1 + doctor_d * 1,
                                           0)))) %>%
        group_by(sku, hospital, hosp_level, decile, province, city, tier, is, flag) %>% 
        summarise(freq = sum(freq, na.rm = TRUE),
                  potential0 = sum(potential0, na.rm = TRUE),
                  potential1 = sum(potential1, na.rm = TRUE),
                  target = sum(target, na.rm = TRUE)) %>% 
        ungroup() %>% 
        arrange(-potential0) %>%
        mutate(potential0_cumsum = cumsum(potential0),
               potential0_cumctrb = potential0_cumsum / sum(potential0, na.rm = TRUE) * 100,
               fte = freq / 8 * 12 / 185,
               cost = fte * 70000,
               productivity = target / fte,
               productivity = ifelse(is.na(productivity) | is.nan(productivity) | is.infinite(productivity),
                                     0,
                                     productivity),
               # roi = (target - cost) / cost,
               growth = potential1 / potential0 - 1,
               growth = ifelse(is.na(growth),
                               0,
                               growth))
      
      total.data3 <- total.data.m %>% 
        filter(flag == 0) %>% 
        filter(!(province %in% input$aban)) %>% 
        filter(productivity >= scenario1$productivity,
               growth >= scenario1$growth) %>% 
        filter(potential0_cumctrb <= scenario1$kPotnCtrb,
               !(province %in% scenario1$aban)) %>% 
        bind_rows(filter(total.data.m, flag == 1)) %>% 
        mutate(market_share = target / potential0,
               market_share = ifelse(is.na(market_share),
                                     0,
                                     market_share))
      
      total.data4 <- total.data.m %>% 
        filter(flag == 0) %>% 
        filter(!(province %in% input$aban)) %>% 
        filter(productivity >= scenario2$productivity,
               growth >= scenario2$growth) %>% 
        filter(potential0_cumctrb <= scenario2$kPotnCtrb,
               !(province %in% scenario2$aban)) %>% 
        bind_rows(filter(total.data.m, flag == 1)) %>% 
        mutate(market_share = target / potential0,
               market_share = ifelse(is.na(market_share),
                                     0,
                                     market_share))
      wb <- createWorkbook()
      addWorksheet(wb, "Actual")
      addWorksheet(wb, "Max")
      addWorksheet(wb, "Scenario Ⅰ")
      addWorksheet(wb, "Scenario Ⅱ")
      
      writeDataTable(wb, "Actual", total.data1)
      writeDataTable(wb, "Max", total.data2)
      writeDataTable(wb, "Scenario Ⅰ", total.data3)
      writeDataTable(wb, "Scenario Ⅱ", total.data4)
      
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  
  
}












