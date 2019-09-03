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
  
  ## sku ----
  observeEvent(raw(), {
    updateSelectInput(session,
                      inputId = "sku",
                      label = "Selection SKU",
                      choices = sort(unique(raw()$sku)),
                      selected = sort(unique(raw()$sku)))
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
    if (is.null(raw()) | is.null(input$sku))
      return(NULL)
    raw <- raw()
    
    cum <- raw %>% 
      distinct() %>% 
      filter(sku %in% input$sku) %>% 
      group_by(hospital, province, city, flag) %>% 
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
  ConcPlot <- reactive({
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
  
  output$Conc <- renderPlotly({
    ConcPlot()
  })
  
  ## segmentation ----
  SegData <- reactive({
    if (is.null(CalcData()) | is.null(input$kPotnCtrb))
      return(NULL)
    
    if (is.na(input$kPotnCtrb)) {
      kProp <- 0
    } else {
      kProp <- input$kPotnCtrb
    }
    
    seg.data <- bind_rows(CalcData()$data1, CalcData()$data2) %>% 
      filter(!(province %in% input$aban))
    
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
  
  output$seg.h <- renderText({
    if (is.null(SegData()))
      return("Growth Rate = 0")
    
    paste0("Growth Rate = ", format(SegData()$kGrMean, digits = 2L))
  })
  
  output$seg.v <- renderText({
    if (is.null(SegData()))
      return("Potential Cumulation Contribution = 0%")
    
    paste0("Potential Cumulation Contribution = ", SegData()$kProp, "%")
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
  
  ## province data ----
  ProvData <- reactive({
    if (is.null(CalcData()) | is.null(input$kPotnCtrb))
      return(NULL)
    
    if (is.na(input$kPotnCtrb)) {
      kProp <- 0
    } else {
      kProp <- input$kPotnCtrb
    }
    
    total.data <- CalcData()$data2 %>% 
      filter(potential0_cumctrb <= kProp,
             !(province %in% input$aban))
    
    # if (is.null(input$aban)) {
    #   covered.data <- total.data
    # } else {
    #   covered.data <- total.data[which(!(total.data$province %in% input$aban)), ]
    # }
    
    if (is.na(input$productivity)) {
      covered.data <- total.data
    } else {
      covered.data <- total.data[which(total.data$productivity >= input$productivity), ]
    }
    
    if (is.na(input$roi)) {
      covered.data <- covered.data
    } else {
      covered.data <- covered.data[which(covered.data$roi >= input$roi), ]
    }
    
    if (is.na(input$growth)) {
      covered.data <- covered.data
    } else {
      covered.data <- covered.data[which(covered.data$growth >= input$growth), ]
    }
    
    covered.prov.data <- covered.data %>% 
      bind_rows(CalcData()$data1) %>% 
      group_by(province) %>% 
      summarise(covered_hospital_num = n(),
                covered_city_num = length(sort(unique(city))),
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
             covered_roi = (covered_target - covered_cost) / covered_cost)
    
    total.prov.data <- total.data %>% 
      bind_rows(CalcData()$data1) %>% 
      group_by(province) %>% 
      summarise(total_hospital_num = n(),
                total_city_num = length(sort(unique(city))),
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
             total_roi = (total_target - total_cost) / total_cost)
    
    prov.data <- total.prov.data %>% 
      left_join(covered.prov.data, by = "province") %>% 
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
    
    prov.data
  })
  
  ## plot1 ----
  ProvPlot1 <- reactive({
    if (is.null(ProvData()) | is.null(input$kpi1))
      return(NULL)
    if (nrow(ProvData()) == 0)
      return(NULL)
    
    plot.data <- ProvData()
    plot.data <- plot.data[c("province",
                             paste0("covered_", input$kpi1),
                             paste0("uncovered_", input$kpi1),
                             paste0("total_", input$kpi1))]
    colnames(plot.data) <- c("x", "y1", "y2", "y")
    plot.data <- arrange(plot.data, -y)
    
    plot1 <- plot_ly(hoverinfo = "name+x+y")
    
    plot1 <- plot1 %>% 
      add_bars(x = plot.data$x,
               y = plot.data$y1,
               type = "bar",
               name = "Covered",
               color = I(options()$covered.color)) %>% 
      add_bars(x = plot.data$x,
               y = plot.data$y2,
               type = "bar",
               name = "Uncovered",
               color = I(options()$uncovered.color)) %>% 
      layout(
        barmode = "stack",
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
    
    if (input$kpi1 == "fte") {
      plot1 <- plot1 %>% 
        layout(
          yaxis = list(
            hoverformat = ".2f"
          )
        )
    }
    
    plot1
  })
  
  output$HospitalPlot <- renderPlotly({
    ProvPlot1()
  })
  
  ## table1 ----
  ProvTable1 <- reactive({
    if (is.null(ProvData()) | is.null(input$kpi1))
      return(NULL)
    if (nrow(ProvData()) == 0)
      return(NULL)
    
    plot.data <- ProvData()
    plot.data <- plot.data[c("province",
                             paste0("covered_", input$kpi1),
                             paste0("uncovered_", input$kpi1),
                             paste0("total_", input$kpi1))]
    colnames(plot.data) <- c("index", "Covered", "Uncovered", "Total")
    
    ordering <- arrange(plot.data, -`Total`)$index
    plot.data <- melt(plot.data, id.vars = "index", variable.name = "省份") %>% 
      dcast(`省份`~index, value.var = "value") %>% 
      select("省份", ordering)
    
    plot.data
  })
  
  output$HospitalTable <- DT::renderDataTable({
    if (is.null(ProvTable1()))
      return(NULL)
    
    if (input$kpi1 == "fte") {
      dgt = 2
    } else {
      dgt = 0
    }
    
    DT::datatable(
      ProvTable1(),
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
        target = "row",
        # color = styleEqual("Total", options()$table.color),
        fontWeight = styleEqual("Total", "bold")
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
  
  
  ## plot2 ----
  ProvPlot2 <- reactive({
    if (is.null(ProvData()) | is.null(input$kpi2))
      return(NULL)
    if (nrow(ProvData()) == 0)
      return(NULL)
    
    plot.data <- ProvData()
    plot.data <- plot.data[c("province",
                             paste0("covered_", input$kpi2),
                             paste0("uncovered_", input$kpi2),
                             paste0("total_", input$kpi2))]
    colnames(plot.data) <- c("x", "y1", "y2", "y")
    plot.data <- arrange(plot.data, -y)
    
    plot1 <- plot_ly(hoverinfo = "name+x+y")
    
    plot1 <- plot1 %>% 
      add_trace(x = plot.data$x,
                y = plot.data$y,
                type = "scatter",
                mode = "lines",
                name = "Total hospital",
                color = I(options()$total.color)) %>% 
      add_trace(x = plot.data$x,
                y = plot.data$y1,
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
          tickformat = ",",
          hoverformat = ",.2f",
          title = "",
          mirror = "ticks"
        )
      )
    
    plot1
  })
  
  output$IndexPlot <- renderPlotly({
    ProvPlot2()
  })
  
  ## table2 ----
  ProvTable2 <- reactive({
    if (is.null(ProvData()) | is.null(input$kpi2))
      return(NULL)
    if (nrow(ProvData()) == 0)
      return(NULL)
    
    plot.data <- ProvData()
    plot.data <- plot.data[c("province",
                             paste0("covered_", input$kpi2),
                             paste0("uncovered_", input$kpi2),
                             paste0("total_", input$kpi2))]
    colnames(plot.data) <- c("index", "Covered", "Uncovered", "Total")
    
    ordering <- arrange(plot.data, -`Total`)$index
    plot.data <- melt(plot.data, id.vars = "index", variable.name = "省份") %>% 
      dcast(`省份`~index, value.var = "value") %>% 
      select("省份", ordering)
    
    plot.data
  })
  
  output$IndexTable <- DT::renderDataTable({
    if (is.null(ProvTable2()))
      return(NULL)
    
    DT::datatable(
      ProvTable2(),
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
        target = "row",
        # color = styleEqual("Total", options()$table.color),
        fontWeight = styleEqual("Total", "bold")
      ) %>% 
      formatStyle(
        "省份",
        color = options()$table.color,
        fontWeight = "bold"
      ) %>% 
      formatRound(
        columns = TRUE,
        digits = 2
        # interval = 3,
        # mark = ","
      )
  })
  
  ## recommendation calculation data ----
  CalcDataRcmd <- reactive({
    if (is.null(CalcData()) | is.null(input$sku))
      return(NULL)
    
    total.data <- bind_rows(CalcData()$data1, CalcData()$data2) %>% 
      arrange(potential0_cumctrb) %>% 
      mutate(target_cumsum = cumsum(target),
             cost_cumsum = cumsum(cost),
             fte_cumsum = cumsum(fte),
             roi_cumsum = (target_cumsum - cost_cumsum) / cost_cumsum,
             productivity_cumsum = target_cumsum / fte_cumsum)
    
    if (input$scenario == "Max ROI") {
      mark <- rownames(total.data)[which(abs(total.data$roi_cumsum) == min(abs(total.data$roi_cumsum)))]
      
    } else {
      mark <- rownames(total.data)[length(rownames(total.data))]
    }
    
    list("total.data" = total.data,
         "mark" = mark)
  })
  
  ## recommendation concentration curve ----
  ConcPlotRcmd <- reactive({
    if (is.null(CalcDataRcmd()))
      return(NULL)
    
    plot.data <- CalcDataRcmd()$total.data
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
  
  output$ConcRcmd <- renderPlotly({
    ConcPlotRcmd()
  })
  
  ## segmentation ----
  SegDataRcmd <- reactive({
    if (is.null(CalcDataRcmd()))
      return(NULL)
    
    seg.data <- CalcDataRcmd()$total.data %>% 
      filter(!(province %in% input$aban))
    
    kRow <- as.numeric(CalcDataRcmd()$mark)
    kProp <- round(CalcDataRcmd()$total.data$potential0_cumctrb[which(rownames(CalcDataRcmd()$total.data) == as.character(kRow))], 2)
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
  
  output$segRcmd.h <- renderText({
    if (is.null(SegDataRcmd()))
      return("Growth Rate = 0")
    
    paste0("Growth Rate = ", format(SegDataRcmd()$kGrMean, digits = 2L))
  })
  
  output$segRcmd.v <- renderText({
    if (is.null(SegDataRcmd()))
      return("Potential Cumulation Contribution = 0%")
    
    paste0("Potential Cumulation Contribution = ", SegDataRcmd()$kProp, "%")
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
    
    covered.data <- CalcDataRcmd()$total.data %>% 
      filter(row_number() <= as.numeric(CalcDataRcmd()$mark),
             !(province %in% input$aban))
    
    covered.prov.data <- covered.data %>% 
      group_by(province) %>% 
      summarise(covered_hospital_num = n(),
                covered_city_num = length(sort(unique(city))),
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
             covered_roi = (covered_target - covered_cost) / covered_cost)
    
    covered.prov.data
  })
  
  ## recommendation plot1 ----
  ProvPlot1Rcmd <- reactive({
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
    
    plot1
  })
  
  output$HospitalPlotRcmd <- renderPlotly({
    ProvPlot1Rcmd()
  })
  
  ## recommendation table1 ----
  ProvTable1Rcmd <- reactive({
    if (is.null(ProvDataRcmd()) | is.null(input$kpi1.rcmd))
      return(NULL)
    if (nrow(ProvDataRcmd()) == 0)
      return(NULL)
    
    plot.data <- ProvDataRcmd()
    plot.data <- plot.data[c("province", paste0("covered_", input$kpi1.rcmd))]
    colnames(plot.data) <- c("index", "Covered")
    
    ordering <- arrange(plot.data, -`Covered`)$index
    plot.data <- melt(plot.data, id.vars = "index", variable.name = "省份") %>% 
      dcast(`省份`~index, value.var = "value") %>% 
      select("省份", ordering)
    
    plot.data
  })
  
  output$HospitalTableRcmd <- DT::renderDataTable({
    if (is.null(ProvTable1Rcmd()))
      return(NULL)
    
    if (input$kpi1 == "fte") {
      dgt = 2
    } else {
      dgt = 0
    }
    
    DT::datatable(
      ProvTable1Rcmd(),
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
  
  ## recommendation plot2 ----
  ProvPlot2Rcmd <- reactive({
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
          tickformat = ",",
          hoverformat = ",.2f",
          title = "",
          mirror = "ticks"
        )
      )
    
    plot1
  })
  
  output$IndexPlotRcmd <- renderPlotly({
    ProvPlot2Rcmd()
  })
  
  ## recommendation table2 ----
  ProvTable2Rcmd <- reactive({
    if (is.null(ProvDataRcmd()) | is.null(input$kpi2.rcmd))
      return(NULL)
    if (nrow(ProvDataRcmd()) == 0)
      return(NULL)
    
    plot.data <- ProvDataRcmd()
    plot.data <- plot.data[c("province", paste0("covered_", input$kpi2.rcmd))]
    colnames(plot.data) <- c("index", "Covered")
    
    ordering <- arrange(plot.data, -`Covered`)$index
    plot.data <- melt(plot.data, id.vars = "index", variable.name = "省份") %>% 
      dcast(`省份`~index, value.var = "value") %>% 
      select("省份", ordering)
    
    plot.data
  })
  
  output$IndexTableRcmd <- DT::renderDataTable({
    if (is.null(ProvTable2Rcmd()))
      return(NULL)
    
    DT::datatable(
      ProvTable2Rcmd(),
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
  
  ## download ----
  output$DownloadSel <- downloadHandler(
    filename = function() {
      "selection_data.csv"
    },
    
    content = function(file) {
      if (is.na(input$kPotnCtrb)) {
        kProp <- 0
      } else {
        kProp <- input$kPotnCtrb
      }
      
      total.data <- CalcData()$data2 %>% 
        filter(potential0_cumctrb <= kProp,
               !(province %in% input$aban)) %>% 
        bind_rows(CalcData()$data1)
      
      write.csv(total.data, file, row.names = FALSE, fileEncoding = "GB2312")
    }
  )
  
  output$DownloadRcmd <- downloadHandler(
    filename = function() {
      "recommendation_selection_data.csv"
    },
    
    content = function(file) {
      total.data <- CalcDataRcmd()$total.data %>% 
        filter(row_number() < as.numeric(CalcDataRcmd()$mark)) %>% 
        select(-target_cumsum, -cost_cumsum, -fte_cumsum, -roi_cumsum, -productivity_cumsum)
      
      write.csv(total.data, file, row.names = FALSE, fileEncoding = "GB2312")
    }
  )
  
  
  
  
}












