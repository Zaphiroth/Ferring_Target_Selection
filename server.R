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
                  kIndex = -Inf,
                  aban = NULL,
                  productivity = 0,
                  growth = -Inf)

scenario2 <- list(kPotnCtrb = 0,
                  kIndex = -Inf,
                  aban = NULL,
                  productivity = 0,
                  growth = -Inf)

##---- server ----
server <- function(input, output, session) {
  raw <- reactive({
    if (is.null(input$raw)) {
      return(NULL)
    }
    
    inFile.raw <- input$raw
    raw <- read_xlsx(
      inFile.raw$datapath,
      na = "NA"
    ) %>% 
      setDF() %>% 
      select(SKU, `Hospital`, `Hosp_level`, Province, City, `City Tier`, 
             `Potential（EUR）Y0`, `Potential（EUR）Y1`, `Is there quota`, 
             `Target（EUR）`, flag)
    colnames(raw) <- c("sku", "hospital", "hosp_level", "province", "city", "tier", 
                       "potential0", "potential1", "is", "target", "flag")
    
    # decile_map <- data.frame(group = 1:10,
    #                          decile = c("D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "D9", "D10"))
    
    raw <- raw %>% 
      group_by(sku) %>% 
      arrange(-potential0) %>% 
      mutate(potential_prop = cumsum(potential0) / sum(potential0, na.rm = TRUE),
             decile = ifelse(potential_prop <= 0.1, "D1", 
                             ifelse(potential_prop > 0.1 & potential_prop <= 0.2, "D2", 
                                    ifelse(potential_prop > 0.2 & potential_prop <= 0.3, "D3", 
                                           ifelse(potential_prop > 0.3 & potential_prop <= 0.4, "D4", 
                                                  ifelse(potential_prop > 0.4 & potential_prop <= 0.5, "D5", 
                                                         ifelse(potential_prop > 0.5 & potential_prop <= 0.6, "D6", 
                                                                ifelse(potential_prop > 0.6 & potential_prop <= 0.7, "D7", 
                                                                       ifelse(potential_prop > 0.7 & potential_prop <= 0.8, "D8", 
                                                                              ifelse(potential_prop > 0.8 & potential_prop <= 0.9, "D9", 
                                                                                     "D10")))))))))) %>% 
      ungroup()
    
    raw
  })
  
  dtbt <- reactive({
    if (is.null(input$dtbt)) {
      return(NULL)
    }
    
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
  
  rules <- reactive({
    if (is.null(input$fte_rules)) {
      return(NULL)
    }
    
    inFile.rules <- input$fte_rules
    
    doctor_num <- read_xlsx(
      inFile.rules$datapath,
      sheet = 1,
      na = "NA"
    ) %>% 
      setDF()
    colnames(doctor_num) <- c("doc_seg", "num_a", "num_b", "num_c", "num_d")
    
    doctor_freq <- read_xlsx(
      inFile.rules$datapath,
      sheet = 2,
      na = "NA"
    ) %>% 
      setDF()
    colnames(doctor_freq) <- c("sku", "freq_a", "freq_b", "freq_c", "freq_d")
    
    work_time <- read_xlsx(
      inFile.rules$datapath,
      sheet = 3,
      na = "NA"
    ) %>% 
      setDF()
    colnames(work_time) <- c("day", "month", "call", "year_work")
    
    list("doctor_num" = doctor_num,
         "doctor_freq" = doctor_freq,
         "work_time" = work_time)
  })
  
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
      if (is.null(raw()) | is.null(rules()) | is.null(input$sku))
        return(NULL)
      
      cum <- raw()[raw()$flag == 0, ] %>% 
        filter(sku %in% input$sku) %>% 
        filter(!(province %in% input$aban)) %>% 
        bind_rows(raw()[raw()$flag == 1, ]) %>% 
        unite("doc_seg", sku, decile, remove = FALSE, sep = "") %>% 
        left_join(rules()$doctor_num, by = "doc_seg") %>% 
        left_join(rules()$doctor_freq, by = "sku") %>% 
        cbind(rules()$work_time) %>% 
        mutate(freq = num_a*freq_a + num_b*freq_b + num_c*freq_c + num_d*freq_d,
               fte = freq / call * month / day) %>% 
        group_by(sku, hospital, hosp_level, decile, province, city, tier, is, flag) %>% 
        summarise(fte = sum(fte, na.rm = TRUE),
                  potential0 = sum(potential0, na.rm = TRUE),
                  potential1 = sum(potential1, na.rm = TRUE),
                  target = sum(target, na.rm = TRUE)) %>% 
        ungroup()
      
      cum1 <- filter(cum, flag == 1)
      cum2 <- filter(cum, flag == 0)
      
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
      
      plot.data <- CalcData()$data2 %>% 
        bind_rows(CalcData()$data1) %>% 
        group_by(hospital) %>% 
        summarise(potential0 = sum(potential0, na.rm = TRUE)) %>% 
        ungroup() %>% 
        arrange(-potential0) %>% 
        mutate(potential0_cumsum = cumsum(potential0),
               potential0_cumctrb = potential0_cumsum / sum(potential0, na.rm = TRUE) * 100)
      
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
      
      plot1 <- plot1 %>% 
        layout(
          showlegend = FALSE,
          xaxis = list(
            showticklabels = TRUE,
            tickformat = ",",
            showline = FALSE,
            zeroline = TRUE,
            title = "",
            range = c(0, nrow(plot.data)*1.1),
            mirror = "ticks"
          ),
          yaxis = list(
            showticklabels = TRUE,
            ticksuffix = "%",
            showline = FALSE,
            zeroline = TRUE,
            title = "",
            hoverformat = ".0f",
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
      if (is.null(CalcData())) {
        return(NULL)
      }
      
      if (is.na(input$kPotnCtrb)) {
        kProp <- 0
      } else {
        kProp <- input$kPotnCtrb
      }
      
      seg.data <- CalcData()$data2 %>% 
        bind_rows(CalcData()$data1) %>% 
        group_by(hospital, province, city, flag) %>% 
        summarise(fte = sum(fte, na.rm = TRUE),
                  potential0 = sum(potential0, na.rm = TRUE),
                  potential1 = sum(potential1, na.rm = TRUE),
                  target = sum(target, na.rm = TRUE)) %>% 
        ungroup() %>% 
        mutate(potential0_cumsum = cumsum(potential0),
               potential0_cumctrb = potential0_cumsum / sum(potential0, na.rm = TRUE) * 100,
               productivity = target / fte,
               productivity = ifelse(is.na(productivity) | is.nan(productivity) | is.infinite(productivity),
                                     0,
                                     productivity),
               growth = potential1 / potential0 - 1,
               growth = ifelse(is.na(growth),
                               0,
                               growth),
               market_share = target / potential0,
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
                                                       "0")))),
                 segment = ifelse(flag == 1, "A", segment))
        
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
                                                       "0")))),
                 segment = ifelse(flag == 1, "A", segment))
        
      } else {
        return(NULL)
      }
      
      seg.a <- seg.total %>% 
        filter(segment == "A") %>% 
        mutate(hospital_num = n(),
               city_num = length(sort(unique(city)))) %>% 
        group_by(hospital_num, city_num) %>% 
        summarise(fte = sum(fte, na.rm = TRUE),
                  target = sum(target, na.rm = TRUE)) %>% 
        ungroup() %>% 
        mutate(productivity = target / fte,
               hospital_num = format(hospital_num, big.mark = ","),
               city_num = format(city_num, big.mark = ","),
               fte = format(round(fte, 1), big.mark = ","),
               productivity = format(round(productivity, 0), big.mark = ",")) %>% 
        select("Number of Hospital" = "hospital_num",
               "Number of City" = "city_num",
               "FTE" = "fte",
               "Productivity" = "productivity") %>% 
        melt(id.vars = NULL)
      
      seg.b <- seg.total %>% 
        filter(segment == "B") %>% 
        mutate(hospital_num = n(),
               city_num = length(sort(unique(city)))) %>% 
        group_by(hospital_num, city_num) %>% 
        summarise(fte = sum(fte, na.rm = TRUE),
                  target = sum(target, na.rm = TRUE)) %>% 
        ungroup() %>% 
        mutate(productivity = target / fte,
               hospital_num = format(hospital_num, big.mark = ","),
               city_num = format(city_num, big.mark = ","),
               fte = format(round(fte, 1), big.mark = ","),
               productivity = format(round(productivity, 0), big.mark = ",")) %>% 
        select("Number of Hospital" = "hospital_num",
               "Number of City" = "city_num",
               "FTE" = "fte",
               "Productivity" = "productivity") %>% 
        melt(id.vars = NULL)
      
      seg.c <- seg.total %>% 
        filter(segment == "C") %>% 
        mutate(hospital_num = n(),
               city_num = length(sort(unique(city)))) %>% 
        group_by(hospital_num, city_num) %>% 
        summarise(fte = sum(fte, na.rm = TRUE),
                  target = sum(target, na.rm = TRUE)) %>% 
        ungroup() %>% 
        mutate(productivity = target / fte,
               hospital_num = format(hospital_num, big.mark = ","),
               city_num = format(city_num, big.mark = ","),
               fte = format(round(fte, 1), big.mark = ","),
               productivity = format(round(productivity, 0), big.mark = ",")) %>% 
        select("Number of Hospital" = "hospital_num",
               "Number of City" = "city_num",
               "FTE" = "fte",
               "Productivity" = "productivity") %>% 
        melt(id.vars = NULL)
      
      seg.d <- seg.total %>% 
        filter(segment == "D") %>% 
        mutate(hospital_num = n(),
               city_num = length(sort(unique(city)))) %>% 
        group_by(hospital_num, city_num) %>% 
        summarise(fte = sum(fte, na.rm = TRUE),
                  target = sum(target, na.rm = TRUE)) %>% 
        ungroup() %>% 
        mutate(productivity = target / fte) %>% 
        mutate(hospital_num = format(hospital_num, big.mark = ","),
               city_num = format(city_num, big.mark = ","),
               fte = format(round(fte, 1), big.mark = ","),
               productivity = format(round(productivity, 0), big.mark = ",")) %>% 
        select("Number of Hospital" = "hospital_num",
               "Number of City" = "city_num",
               "FTE" = "fte",
               "Productivity" = "productivity") %>% 
        melt(id.vars = NULL)
      
      seg.na <- data.frame("variable" = c("Number of Hospital", "Number of City", 
                                          "FTE", "Productivity"),
                           "value" = c(0, 0, 0, 0))
      
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
  
  ## covered data ----
  CoveredData <- reactive({
    if (is.null(SegData())) {
      return(NULL)
    }
    
    if (input$cover == TRUE) {
      covered.data <- SegData()$total %>% 
        filter(segment == "A")
    } else {
      covered.data <- SegData()$total %>% 
        filter(segment == "A" | segment == "B")
    }
    
    covered.data
  })
  
  ## actual plot ----
  ActualPlot1 <- reactive({
    input$go
    isolate({
      if (is.null(dtbt())) {
        return(NULL)
      }
      
      plot.data <- dtbt() %>% 
        unite("rep", `代表区域`, `代表`, sep = "-") %>% 
        group_by(rep) %>% 
        summarise(mat = sum(mat, na.rm = TRUE)) %>% 
        ungroup()
      
      maxx <- max(plot.data$mat)
      digit <- 10^(nchar(round(maxx))-1)
      upper_bound <- ceiling(maxx/digit) * digit
      cut_break <- seq(0, upper_bound, upper_bound / 10)
      cut_break_format <- cut_break / 1000
      
      cut_labels <- c()
      for (i in 1:10) {
        cut_label <- paste0(cut_break_format[i], "~", cut_break_format[i+1])
        cut_labels <- c(cut_labels, cut_label)
      }
      
      plot.data.m <- plot.data %>% 
        mutate(section = cut(mat, breaks = cut_break, labels = cut_labels)) %>% 
        arrange(section) %>% 
        group_by(section) %>% 
        summarise(rep_num = n()) %>% 
        ungroup()
      
      plot1 <- plot_ly(hoverinfo = "name+x+y")
      
      plot1 <- plot1 %>% 
        add_trace(x = plot.data.m$section,
                  y = plot.data.m$rep_num,
                  name = "代表",
                  type = "bar",
                  text = plot.data.m$rep_num,
                  textfont = list(color = I("#000")),
                  textposition = "outside",
                  color = I(options()$covered.color)) %>% 
        layout(
          title = "GU Territory Distribution by Value",
          showlegend = FALSE,
          margin = list(
            l = 50,
            r = 50,
            b = 50,
            t = 50,
            pad = 4
          ),
          # annotations = list(
          #   x = 0.9,
          #   y = 1,
          #   text = "RMB: Thousand",
          #   showarrow = FALSE,
          #   xref = "paper",
          #   yref = "paper"
          # ),
          xaxis = list(
            title = "",
            type = "category",
            categoryorder = "array",
            categoryarray = ~plot.data.m$section,
            mirror = "ticks"
          ),
          yaxis = list(
            title = "Unit: Thousand",
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
  
  observeEvent(input$go, {
    updateSelectInput(session,
                      inputId = "actual.sku1",
                      label = "Selection SKU",
                      choices = input$sku,
                      selected = input$sku)
  })
  
  ActualPlot2 <- reactive({
    c(input$go, input$actual.sku1)
    isolate({
      if (is.null(CalcData()) | is.null(input$actual.sku1)) {
        return(NULL)
      }
      
      decile.data <- CalcData()$data2 %>% 
        bind_rows(CalcData()$data1) %>% 
        group_by(decile) %>% 
        summarise(decile_num = n()) %>% 
        ungroup()
      
      plot.data <- CalcData()$data2 %>% 
        filter(is == "Y") %>% 
        filter(sku %in% input$actual.sku1) %>% 
        bind_rows(CalcData()$data1) %>% 
        group_by(decile) %>% 
        summarise(potential0 = sum(potential0, na.rm = TRUE),
                  target = sum(target, na.rm = TRUE)) %>% 
        ungroup() %>% 
        right_join(decile.data, by = "decile") %>% 
        mutate(market_share = target / potential0,
               decile = factor(decile,
                               levels = c("D1", "D2", "D3", "D4", "D5", 
                                          "D6", "D7", "D8", "D9", "D10"))) %>% 
        arrange(decile)
      plot.data[is.na(plot.data)] <- 0
      
      plot1 <- plot_ly(hoverinfo = "name+x+y")
      
      plot1 <- plot1 %>% 
        add_trace(x = plot.data$decile,
                  y = plot.data$decile_num,
                  name = "Hospital",
                  type = "bar",
                  # text = plot.data$hospital_num,
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
          title = "GU Market Share by Decile",
          showlegend = TRUE,
          legend = list(
            # x = 0.215,
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
            range = c(0, max(plot.data$decile_num)*1.2),
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
  
  observeEvent(input$go, {
    updateSelectInput(session,
                      inputId = "actual.sku2",
                      label = "Selection SKU",
                      choices = input$sku,
                      selected = input$sku)
  })
  
  ActualPlot3 <- reactive({
    c(input$go, input$actual.sku2)
    isolate({
      if (is.null(CalcData()) | is.null(CoveredData()) | is.null(input$actual.sku2)) {
        return(NULL)
      }
      
      potential.data <- CalcData()$data2 %>% 
        bind_rows(CalcData()$data1) %>% 
        group_by(decile) %>% 
        summarise(potential0 = sum(potential0, na.rm = TRUE)) %>% 
        ungroup()
      
      plot.data <- CalcData()$data2 %>% 
        filter(is == "Y") %>% 
        filter(sku %in% input$actual.sku2) %>% 
        bind_rows(CalcData()$data1) %>% 
        group_by(decile) %>% 
        summarise(target = sum(target, na.rm = TRUE)) %>% 
        ungroup() %>% 
        right_join(potential.data, by = "decile") %>% 
        mutate(remain = potential0 - target,
               decile = factor(decile,
                               levels = c("D1", "D2", "D3", "D4", "D5", 
                                          "D6", "D7", "D8", "D9", "D10"))) %>% 
        arrange(decile)
      plot.data[is.na(plot.data)] <- 0
      
      plot1 <- plot_ly(hoverinfo = "name+x+y")
      
      plot1 <- plot1 %>% 
        add_trace(x = plot.data$decile,
                  y = plot.data$remain,
                  name = "Potential",
                  type = "bar",
                  color = I(options()$uncovered.color)) %>% 
        add_trace(x = plot.data$decile,
                  y = plot.data$target,
                  name = "Covered target",
                  type = "bar",
                  color = I(options()$covered.color)) %>% 
        layout(
          barmode = "stack",
          title = "Sales and Potential by Decile",
          showlegend = TRUE,
          legend = list(
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
            showticklabels = TRUE,
            mirror = "ticks"
          ),
          yaxis = list(
            title = "",
            range = c(0, max(plot.data$potential0)*1.2),
            zeroline = TRUE,
            showgrid = TRUE,
            showticklabels = TRUE,
            # tickformat = ",",
            hoverformat = ",.0f",
            mirror = "ticks"
          )
        )
      
      plot1
    })
  })
  
  output$ActualPlot3 <- renderPlotly({
    ActualPlot3()
  })
  
  ## province data ----
  ProvData <- reactive({
    if (is.null(SegData()) | is.null(CoveredData())) {
      return(NULL)
    }
    
    total.data <- SegData()$total
    covered.data <- CoveredData()
    
    if (is.na(input$productivity)) {
      covered.data <- covered.data
    } else {
      covered.data <- covered.data[which(covered.data$productivity >= input$productivity), ]
    }
    
    if (is.na(input$growth)) {
      covered.data <- covered.data
    } else {
      covered.data <- covered.data[which(covered.data$growth >= input$growth/100), ]
    }
    
    actual.prov.data <- CalcData()$data2 %>% 
      filter(is == "Y") %>% 
      bind_rows(CalcData()$data1) %>% 
      group_by(province, city, hospital) %>% 
      summarise(fte = sum(fte, na.rm = TRUE),
                potential0 = sum(potential0, na.rm = TRUE),
                potential1 = sum(potential1, na.rm = TRUE),
                target = sum(target, na.rm = TRUE)) %>% 
      ungroup() %>% 
      group_by(province, city) %>% 
      summarise(hospital_num = n(),
                fte = sum(fte, na.rm = TRUE),
                potential0 = sum(potential0, na.rm = TRUE),
                potential1 = sum(potential1, na.rm = TRUE),
                target = sum(target, na.rm = TRUE)) %>% 
      ungroup() %>% 
      group_by(province) %>% 
      summarise(actual_city_num = n(),
                actual_hospital_num = sum(hospital_num, na.rm = TRUE),
                actual_fte = sum(fte, na.rm = TRUE),
                actual_potential0 = sum(potential0, na.rm = TRUE),
                actual_potential1 = sum(potential1, na.rm = TRUE),
                actual_target = sum(target, na.rm = TRUE)) %>% 
      ungroup() %>% 
      mutate(actual_productivity = actual_target / actual_fte,
             actual_productivity = ifelse(is.na(actual_productivity) | is.nan(actual_productivity) | is.infinite(actual_productivity),
                                          0,
                                          actual_productivity))
    
    covered.prov.data <- covered.data %>% 
      group_by(province, city, hospital) %>% 
      summarise(fte = sum(fte, na.rm = TRUE),
                potential0 = sum(potential0, na.rm = TRUE),
                potential1 = sum(potential1, na.rm = TRUE),
                target = sum(target, na.rm = TRUE)) %>% 
      ungroup() %>% 
      group_by(province, city) %>% 
      summarise(hospital_num = n(),
                fte = sum(fte, na.rm = TRUE),
                potential0 = sum(potential0, na.rm = TRUE),
                potential1 = sum(potential1, na.rm = TRUE),
                target = sum(target, na.rm = TRUE)) %>% 
      ungroup() %>% 
      group_by(province) %>% 
      summarise(covered_city_num = n(),
                covered_hospital_num = sum(hospital_num, na.rm = TRUE),
                covered_fte = sum(fte, na.rm = TRUE),
                covered_potential0 = sum(potential0, na.rm = TRUE),
                covered_potential1 = sum(potential1, na.rm = TRUE),
                covered_target = sum(target, na.rm = TRUE)) %>% 
      ungroup() %>% 
      mutate(covered_productivity = covered_target / covered_fte,
             covered_productivity = ifelse(is.na(covered_productivity) | is.nan(covered_productivity) | is.infinite(covered_productivity),
                                           0,
                                           covered_productivity))
    
    total.prov.data <- total.data %>% 
      group_by(province, city, hospital) %>% 
      summarise(fte = sum(fte, na.rm = TRUE),
                potential0 = sum(potential0, na.rm = TRUE),
                potential1 = sum(potential1, na.rm = TRUE),
                target = sum(target, na.rm = TRUE)) %>% 
      ungroup() %>% 
      group_by(province, city) %>% 
      summarise(hospital_num = n(),
                fte = sum(fte, na.rm = TRUE),
                potential0 = sum(potential0, na.rm = TRUE),
                potential1 = sum(potential1, na.rm = TRUE),
                target = sum(target, na.rm = TRUE)) %>% 
      ungroup() %>% 
      group_by(province) %>% 
      summarise(total_city_num = n(),
                total_hospital_num = sum(hospital_num, na.rm = TRUE),
                total_fte = sum(fte, na.rm = TRUE),
                total_potential0 = sum(potential0, na.rm = TRUE),
                total_potential1 = sum(potential1, na.rm = TRUE),
                total_target = sum(target, na.rm = TRUE)) %>% 
      ungroup() %>% 
      mutate(total_productivity = total_target / total_fte,
             total_productivity = ifelse(is.na(total_productivity) | is.nan(total_productivity) | is.infinite(total_productivity),
                                         0,
                                         total_productivity))
    
    prov.data <- total.prov.data %>% 
      left_join(actual.prov.data, by  = "province") %>% 
      left_join(covered.prov.data, by = "province") %>% 
      mutate_all(function(x) {ifelse(is.na(x), 0, x)}) %>% 
      mutate(uncovered_hospital_num = total_hospital_num - covered_hospital_num,
             uncovered_city_num = total_city_num - covered_city_num,
             uncovered_potential0 = total_potential0 - covered_potential0,
             uncovered_potential1 = total_potential1 - covered_potential1,
             uncovered_target = total_target - covered_target,
             uncovered_fte = total_fte - covered_fte,
             uncovered_productivity = uncovered_target / uncovered_fte,
             uncovered_productivity = ifelse(is.na(uncovered_productivity) | is.nan(uncovered_productivity) | 
                                               is.infinite(uncovered_productivity),
                                             0,
                                             uncovered_productivity))
    
    prov.data
  })
  
  ## plot ----
  ProvPlot1 <- reactive({
    c(input$go, input$productivity, input$growth, input$kpi1)
    isolate({
      if (is.null(ProvData()) | is.null(input$kpi1)) {
        return(NULL)
      } else if (nrow(ProvData()) == 0) {
        return(NULL)
      }
      
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
                  # text = format(round(plot.data$z, 2), big.mark = ","),
                  # textfont = list(color = options()$total.line.color),
                  # textposition = "middle top",
                  yaxis = "y2",
                  type = "scatter",
                  mode = "lines",
                  name = "Total hospital productivity",
                  color = I(options()$total.line.color)) %>% 
        add_trace(x = plot.data$x,
                  y = plot.data$z2,
                  # text = format(round(plot.data$z2, 2), big.mark = ","),
                  # textfont = list(color = options()$covered.line.color),
                  # textposition = "middle top",
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
            hoverformat = ",.0f",
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
              hoverformat = ",.1f"
            )
          )
      }
      
      plot1
    })
  })
  
  output$HospitalPlot <- renderPlotly({
    ProvPlot1()
  })
  
  ## table ----
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
        dgt = 1
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
                      inputId = "covered.sku1",
                      label = "Selection SKU",
                      choices = input$sku,
                      selected = input$sku)
  })
  
  SharePlot <- reactive({
    c(input$go, input$covered.sku1)
    isolate({
      if (is.null(CoveredData()) | is.null(CalcData()) | is.null(input$covered.sku1)) {
        return(NULL)
      }
      
      decile.data <- CalcData()$data2 %>% 
        bind_rows(CalcData()$data1) %>% 
        group_by(decile) %>% 
        summarise(decile_num = n()) %>% 
        ungroup()
      
      plot.data <- CalcData()$data2 %>% 
        bind_rows(CalcData()$data1) %>% 
        filter(hospital %in% CoveredData()$hospital) %>% 
        filter(sku %in% input$covered.sku1) %>% 
        group_by(decile) %>% 
        summarise(potential0 = sum(potential0, na.rm = TRUE),
                  target = sum(target, na.rm = TRUE)) %>% 
        ungroup() %>% 
        right_join(decile.data, by = "decile") %>% 
        mutate(market_share = target / potential0,
               decile = factor(decile, 
                               levels = c("D1", "D2", "D3", "D4", "D5", 
                                          "D6", "D7", "D8", "D9", "D10"))) %>% 
        arrange(decile)
      plot.data[is.na(plot.data)] <- 0
      
      plot1 <- plot_ly(hoverinfo = "name+x+y")
      
      plot1 <- plot1 %>% 
        add_trace(x = plot.data$decile,
                  y = plot.data$decile_num,
                  name = "Hospital",
                  type = "bar",
                  # text = plot.data$hospital_num,
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
            # x = 0.38,
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
            range = c(0, max(plot.data$decile_num)*1.2),
            zeroline = TRUE,
            showgrid = TRUE,
            showticklabels = TRUE,
            # tickformat = ",",
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
  
  output$SharePlot <- renderPlotly({
    SharePlot()
  })
  
  observeEvent(input$go, {
    updateSelectInput(session,
                      inputId = "covered.sku2",
                      label = "Selection SKU",
                      choices = input$sku,
                      selected = input$sku)
  })
  
  DecilePlot <- reactive({
    c(input$go, input$covered.sku2)
    isolate({
      if (is.null(CalcData()) | is.null(CoveredData()) | is.null(input$covered.sku2)) {
        return(NULL)
      }
      
      potential.data <- CalcData()$data2 %>% 
        bind_rows(CalcData()$data1) %>% 
        group_by(decile) %>% 
        summarise(potential0 = sum(potential0, na.rm = TRUE)) %>% 
        ungroup()
      
      plot.data <- CalcData()$data2 %>% 
        bind_rows(CalcData()$data1) %>% 
        filter(hospital %in% CoveredData()$hospital) %>% 
        filter(sku %in% input$covered.sku2) %>% 
        group_by(decile) %>% 
        summarise(target = sum(target, na.rm = TRUE)) %>% 
        ungroup() %>% 
        right_join(potential.data, by = "decile") %>% 
        mutate(remain = potential0 - target,
               decile = factor(decile,
                               levels = c("D1", "D2", "D3", "D4", "D5", 
                                          "D6", "D7", "D8", "D9", "D10"))) %>% 
        arrange(decile)
      plot.data[is.na(plot.data)] <- 0
      
      plot1 <- plot_ly(hoverinfo = "name+x+y")
      
      plot1 <- plot1 %>% 
        add_trace(x = plot.data$decile,
                  y = plot.data$remain,
                  name = "Potential",
                  type = "bar",
                  color = I(options()$uncovered.color)) %>% 
        add_trace(x = plot.data$decile,
                  y = plot.data$target,
                  name = "Covered target",
                  type = "bar",
                  color = I(options()$covered.color)) %>% 
        layout(
          barmode = "stack",
          title = "Sales and Potential in Covered Hospitals",
          showlegend = TRUE,
          legend = list(
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
            showticklabels = TRUE,
            mirror = "ticks"
          ),
          yaxis = list(
            title = "",
            range = c(0, max(plot.data$potential0)*1.2),
            zeroline = TRUE,
            showgrid = TRUE,
            showticklabels = TRUE,
            # tickformat = ",",
            hoverformat = ",.0f",
            mirror = "ticks"
          )
        )
      
      plot1
    })
  })
  
  output$DecilePlot <- renderPlotly({
    DecilePlot()
  })
  
  ## dimension ----
  DimensionData <- reactive({
    c(input$go, input$dimension)
    isolate({
      if (is.null(CalcData()) | is.null(CoveredData()) | is.null(input$dimension)) {
        return(NULL)
      }
      
      table.data <- CalcData()$data2 %>% 
        bind_rows(CalcData()$data1) %>% 
        filter(hospital %in% CoveredData()$hospital) %>% 
        group_by_at(vars(one_of(c(input$dimension, "hospital")))) %>% 
        summarise(fte = sum(fte, na.rm = TRUE),
                  target = sum(target, na.rm = TRUE)) %>% 
        ungroup() %>% 
        group_by_at(vars(one_of(input$dimension))) %>% 
        summarise(fte = sum(fte, na.rm = TRUE),
                  fte = round(fte, 1),
                  target = sum(target, na.rm = TRUE),
                  productivity = target / fte,
                  productivity = ifelse(is.na(productivity) | is.nan(productivity) | is.infinite(productivity),
                                        0,
                                        productivity),
                  productivity = format(round(productivity, 2), big.mark = ","),
                  hospital_num = n()) %>% 
        ungroup() %>% 
        arrange(-fte) %>% 
        select(input$dimension, 
               "Hospital#" = "hospital_num",
               "FTE" = "fte",
               "Productivity" = "productivity")
      
      name.mapping <- data.frame(name0 = c("sku", "province", "city", "tier", "hosp_level"),
                                 name1 = c("Sku", "Province", "City", "City tier", "Hospital level"),
                                 stringsAsFactors = FALSE)
      
      for (i in input$dimension) {
        if (!is.na(colnames(table.data)[which(colnames(table.data) == i)])) {
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
      
      list("table" = t,
           "data" = table.data)
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
    
    actual <- CalcData()$data2 %>% 
      filter(is == "Y") %>% 
      bind_rows(CalcData()$data1)
    
    actual_aggr <- actual %>% 
      group_by(city) %>% 
      summarise(hospital_num = n(),
                target = sum(target, na.rm = TRUE),
                fte = sum(fte, na.rm = TRUE)) %>% 
      ungroup() %>% 
      summarise(city_num = n(),
                hospital_num = sum(hospital_num, na.rm = TRUE),
                target = sum(target, na.rm = TRUE),
                fte = sum(fte, na.rm = TRUE)) %>% 
      mutate(productivity = target / fte) %>% 
      select("Hospital#" = "hospital_num",
             "City#" = "city_num",
             "FTE" = "fte",
             "Avg. Productivity" = "productivity") %>% 
      melt(id.vars = NULL) %>% 
      select("index" = "variable",
             "actual" = "value")
    
    list("actual" = actual,
         "actual_aggr" = actual_aggr)
  })
  
  observeEvent(input$record1, {
    if (is.na(input$kPotnCtrb)) {
      kProp <- 0
    } else {
      kProp <- input$kPotnCtrb
    }
    
    if (input$growth_share == "Growth Rate") {
      if (is.na(input$kGrowth)) {
        kIndex <- -Inf
      } else {
        kIndex <- input$kGrowth/100
      }
      
    } else if (input$growth_share == "Market Share") {
      if (is.na(input$kShare)) {
        kIndex <- 0
      } else {
        kIndex <- input$kShare/100
      }
      
    } else {
      return(NULL)
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
                       kIndex = kIndex,
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
    
    if (input$growth_share == "Growth Rate") {
      if (is.na(input$kGrowth)) {
        kIndex <- -Inf
      } else {
        kIndex <- input$kGrowth/100
      }
      
    } else if (input$growth_share == "Market Share") {
      if (is.na(input$kShare)) {
        kIndex <- 0
      } else {
        kIndex <- input$kShare/100
      }
      
    } else {
      return(NULL)
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
                       kIndex = kIndex,
                       aban = input$aban,
                       productivity = kProductivity,
                       growth = kGrowth)
  })
  
  Scenario1 <- eventReactive(input$record1, {
    if (is.null(CalcData()) | is.null(SegData())) {
      return(NULL)
    }
    
    scenario <- SegData()$total %>% 
      filter(potential0_cumctrb <= scenario1$kPotnCtrb)
    
    if (input$growth_share == "Growth Rate") {
      if (input$cover) {
        scenario <- scenario %>% 
          filter(growth >= scenario1$kIndex)
      }
      
    } else if (input$growth_share == "Market Share") {
      if (input$cover) {
        scenario <- scenario %>% 
          filter(market_share >= scenario1$kIndex)
      }
      
    } else {
      return(NULL)
    }
    
    scenario_aggr <- scenario %>% 
      group_by(city) %>% 
      summarise(hospital_num = n(),
                target = sum(target, na.rm = TRUE),
                fte = sum(fte, na.rm = TRUE)) %>% 
      ungroup() %>% 
      summarise(city_num = n(),
                hospital_num = sum(hospital_num, na.rm = TRUE),
                target = sum(target, na.rm = TRUE),
                fte = sum(fte, na.rm = TRUE)) %>% 
      mutate(productivity = target / fte,
             productivity = ifelse(is.na(productivity),
                                   0,
                                   productivity)) %>% 
      select("Hospital#" = "hospital_num",
             "City#" = "city_num",
             "FTE" = "fte",
             "Avg. Productivity" = "productivity") %>% 
      melt(id.vars = NULL) %>% 
      select("index" = "variable",
             "scenario1" = "value")
    
    list("scenario" = scenario,
         "scenario_aggr" = scenario_aggr)
  })
  
  Scenario2 <- eventReactive(input$record2, {
    if (is.null(CalcData()) | is.null(SegData())) {
      return(NULL)
    }
    
    scenario <- SegData()$total %>% 
      filter(potential0_cumctrb <= scenario2$kPotnCtrb)
    
    if (input$growth_share == "Growth Rate") {
      if (input$cover) {
        scenario <- scenario %>% 
          filter(growth >= scenario2$kIndex)
      }
      
    } else if (input$growth_share == "Market Share") {
      if (input$cover) {
        scenario <- scenario %>% 
          filter(market_share >= scenario2$kIndex)
      }
      
    } else {
      return(NULL)
    }
    
    scenario_aggr <- scenario %>% 
      group_by(city) %>% 
      summarise(hospital_num = n(),
                target = sum(target, na.rm = TRUE),
                fte = sum(fte, na.rm = TRUE)) %>% 
      ungroup() %>% 
      summarise(city_num = n(),
                hospital_num = sum(hospital_num, na.rm = TRUE),
                target = sum(target, na.rm = TRUE),
                fte = sum(fte, na.rm = TRUE)) %>% 
      mutate(productivity = target / fte,
             productivity = ifelse(is.na(productivity),
                                   0,
                                   productivity)) %>% 
      select("Hospital#" = "hospital_num",
             "City#" = "city_num",
             "FTE" = "fte",
             "Avg. Productivity" = "productivity") %>% 
      melt(id.vars = NULL) %>% 
      select("index" = "variable",
             "scenario2" = "value")
    
    list("scenario" = scenario,
         "scenario_aggr" = scenario_aggr)
  })
  
  Evaluation <- reactive({
    if (is.null(ActualScenario()) | is.null(Scenario1()) | is.null(Scenario2())) {
      return(NULL)
    }
    
    evaluation1 <- ActualScenario()$actual_aggr %>% 
      left_join(Scenario1()$scenario_aggr, by = "index") %>% 
      left_join(Scenario2()$scenario_aggr, by = "index") %>% 
      mutate(scenario1_actual = scenario1 / actual - 1,
             scenario2_actual = scenario2 / actual - 1) %>% 
      mutate(scenario1_actual = paste0(round(scenario1_actual*100, 1), "%"),
             scenario2_actual = paste0(round(scenario2_actual*100, 1), "%")) %>% 
      select("Index" = "index",
             "Scenario Ⅰ vs. Actual" = "scenario1_actual",
             "Scenario Ⅱ vs. Actual" = "scenario2_actual")
    
    evaluation2 <- ActualScenario()$actual_aggr %>% 
      left_join(Scenario1()$scenario_aggr, by = "index") %>% 
      left_join(Scenario2()$scenario_aggr, by = "index") %>% 
      melt() %>% 
      dcast(variable~index) %>% 
      mutate(`Hospital#` = format(`Hospital#`, big.mark = ","),
             `City#` = format(`City#`, big.mark = ","),
             FTE = format(round(FTE, 2), big.mark = ","),
             `Avg. Productivity` = format(round(`Avg. Productivity`, 2), big.mark = ",")) %>% 
      melt(id.vars = "variable", variable.name = "index") %>% 
      dcast(index~variable) %>% 
      select("Index" = "index",
             "Actual" = "actual",
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
      seg.info <- SegData()$total %>% 
        select(hospital, segment)
      
      seg.data <- CalcData()$data2 %>% 
        bind_rows(CalcData()$data1) %>% 
        group_by(sku, hospital, hosp_level, decile, province, city, tier, is, flag) %>% 
        summarise(potential0 = sum(potential0, na.rm = TRUE),
                  target = sum(target, na.rm = TRUE),
                  fte = sum(fte, na.rm = TRUE)) %>% 
        left_join(seg.info, by = "hospital") %>% 
        select("Sku" = "sku", "Hospital" = "hospital", "Hospital level" = "hosp_level", 
               "Decile" = "decile", "Province" = "province", "City" = "city", "Tier" = "tier", 
               "Actiual target hospital" = "is", "Flag" = "flag", "Potential" = "potential0", 
               "Target" = "target", "FTE" = "fte", "Segment" = "segment")
      
      write.csv(seg.data, file, row.names = FALSE, fileEncoding = "GB2312")
    }
  )
  
  output$DownloadProv <- downloadHandler(
    filename = function() {
      paste0("Province_", input$kpi1, "_data.csv")
    },
    
    content = function(file) {
      total.data <- CalcData()$data2 %>% 
        bind_rows(CalcData()$data1)
      
      
      covered.data <- CalcData()$data2 %>% 
        bind_rows(CalcData()$data1) %>% 
        filter(hospital %in% CoveredData()$hospital)
      
      actual.prov.data <- CalcData()$data2 %>% 
        filter(is == "Y") %>% 
        bind_rows(CalcData()$data1) %>% 
        group_by(province, city, sku) %>% 
        summarise(hospital_num = n()) %>% 
        ungroup() %>% 
        group_by(province, sku) %>% 
        summarise(actual_city_num = n(),
                  actual_hospital_num = sum(hospital_num, na.rm = TRUE)) %>% 
        ungroup()
      
      covered.prov.data <- covered.data %>% 
        group_by(province, city, sku) %>% 
        summarise(hospital_num = n()) %>% 
        ungroup() %>% 
        group_by(province, sku) %>% 
        summarise(covered_city_num = n(),
                  covered_hospital_num = sum(hospital_num, na.rm = TRUE)) %>% 
        ungroup()
      
      total.prov.data <- total.data %>% 
        group_by(province, city, sku, hospital) %>% 
        summarise(fte = sum(fte, na.rm = TRUE),
                  potential0 = sum(potential0, na.rm = TRUE),
                  target = sum(target, na.rm = TRUE)) %>% 
        ungroup() %>% 
        group_by(province, city, sku) %>% 
        summarise(hospital_num = n(),
                  fte = sum(fte, na.rm = TRUE),
                  potential0 = sum(potential0, na.rm = TRUE),
                  target = sum(target, na.rm = TRUE)) %>% 
        ungroup() %>% 
        group_by(province, sku) %>% 
        summarise(total_city_num = n(),
                  total_hospital_num = sum(hospital_num, na.rm = TRUE),
                  total_fte = sum(fte, na.rm = TRUE),
                  total_potential0 = sum(potential0, na.rm = TRUE),
                  total_target = sum(target, na.rm = TRUE)) %>% 
        ungroup()
      
      prov.data <- total.prov.data %>% 
        left_join(actual.prov.data, by  = c("province", "sku")) %>% 
        left_join(covered.prov.data, by = c("province", "sku")) %>% 
        mutate_all(function(x) {ifelse(is.na(x), 0, x)}) %>% 
        mutate(uncovered_hospital_num = total_hospital_num - covered_hospital_num,
               uncovered_city_num = total_city_num - covered_city_num) %>% 
        select("Sku" = "sku", "Province" = "province", "Total hospital number" = "total_hospital_num", 
               "Total city number" = "total_city_num", "Potential" = "total_potential0", 
               "Target" = "total_target", "FTE" = "total_fte", "Actual hospital number" = "actual_hospital_num", 
               "Actual city number" = "actual_city_num", "Covered hospital number" = "covered_hospital_num", 
               "Covered city number" = "covered_city_num", "Uncovered hospital number" = "uncovered_hospital_num", 
               "Uncovered city number" = "uncovered_city_num")
      
      write.csv(prov.data, file, row.names = FALSE, fileEncoding = "GB2312")
    }
  )
  
  output$DownloadSel <- downloadHandler(
    filename = function() {
      "Total_data.csv"
    },
    
    content = function(file) {
      total.data <- CalcData()$data2 %>% 
        bind_rows(CalcData()$data1) %>% 
        select("Sku" = "sku", "Hospital" = "hospital", "Hospital level" = "hosp_level", 
               "Decile" = "decile", "Province" = "province", "City" = "city", "Tier" = "tier", 
               "Actiual target hospital" = "is", "Flag" = "flag", "Potential" = "potential0", 
               "Target" = "target", "FTE" = "fte")
      
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
      actual.data <- ActualScenario()$actual %>% 
        mutate(market_share = target / potential0) %>% 
        select("Sku" = "sku", "Hospital" = "hospital", "Hospital level" = "hosp_level", 
               "Decile" = "decile", "Province" = "province", "City" = "city", "Tier" = "tier", 
               "Actiual target hospital" = "is", "Flag" = "flag", "Potential" = "potential0", 
               "Target" = "target", "FTE" = "fte", "Market share" = "market_share")
      
      scenario1.data <- CalcData()$data2 %>% 
        bind_rows(CalcData()$data1) %>% 
        filter(hospital %in% Scenario1()$scenario$hospital) %>% 
        mutate(market_share = target / potential0) %>% 
        select("Sku" = "sku", "Hospital" = "hospital", "Hospital level" = "hosp_level", 
               "Decile" = "decile", "Province" = "province", "City" = "city", "Tier" = "tier", 
               "Actiual target hospital" = "is", "Flag" = "flag", "Potential" = "potential0", 
               "Target" = "target", "FTE" = "fte", "Market share" = "market_share")
      
      scenario2.data <- CalcData()$data2 %>% 
        bind_rows(CalcData()$data1) %>% 
        filter(hospital %in% Scenario2()$scenario$hospital) %>% 
        mutate(market_share = target / potential0) %>% 
        select("Sku" = "sku", "Hospital" = "hospital", "Hospital level" = "hosp_level", 
               "Decile" = "decile", "Province" = "province", "City" = "city", "Tier" = "tier", 
               "Actiual target hospital" = "is", "Flag" = "flag", "Potential" = "potential0", 
               "Target" = "target", "FTE" = "fte", "Market share" = "market_share")
      
      wb <- createWorkbook()
      addWorksheet(wb, "Actual")
      addWorksheet(wb, "Scenario Ⅰ")
      addWorksheet(wb, "Scenario Ⅱ")
      writeDataTable(wb, "Actual", actual.data)
      writeDataTable(wb, "Scenario Ⅰ", scenario1.data)
      writeDataTable(wb, "Scenario Ⅱ", scenario2.data)
      
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  
  
}












