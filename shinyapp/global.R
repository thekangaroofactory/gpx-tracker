

# -- dependencies
library(shiny)
library(bslib)
library(dplyr)
library(sf)
library(leaflet)
library(ggplot2)

# -- code
ktools::source_code("./R")

# -- params
app_version <- "v1.2"
DEBUG <- TRUE

# -- settings
DISTANCE_ANOMALY <- 100
SPEED_ANOMALY <- 50

LEG_DISTANCE_MIN <- 60
LEG_DISTANCE_MAX <- 85
