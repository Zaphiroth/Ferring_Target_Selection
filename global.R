if(Sys.getenv('SHINY_PORT') == "") options(shiny.maxRequestSize=1000*1024^2)

library(DT)
library(plyr)
library(data.table)
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
# library(shinyWidgets)
library(readr)

options(shiny.maxRequestSize = 1000 * 1024 ^ 2,
        seg.color = "#FFFFF0",
        seg.style = "background:#FFFFF0; height:200px;",
        marker.color = "#FF4500",
        square.color = "#FF4500",
        table.color = "#3C8DBC",
        covered.color = "#17B0F7",
        uncovered.color = "#B0C4DE",
        covered.line.color = "#FFC0CB",
        total.line.color = "#DC143C",
        share.color = "#DC143C",
        evaluation.color = "#C8E6FF")
