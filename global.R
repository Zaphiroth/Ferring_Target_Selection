if(Sys.getenv('SHINY_PORT') == "") options(shiny.maxRequestSize=1000*1024^2)

library(DT)
library(reshape2)
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
library(shinyWidgets)
library(readr)

options(shiny.maxRequestSize = 1000 * 1024 ^ 2,
        seg.color = "#FFFFF0",
        seg.style = "background:#FFFFF0; height:220px;",
        square.fill = "rgb(255, 255, 200)",
        table.color = "#3C8DBC",
        covered.color = "#17B0F7",
        uncovered.color = "#64E6FF",
        total.color = "#428BCA")
