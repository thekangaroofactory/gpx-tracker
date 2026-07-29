

# -- dependencies
library(shiny)
library(bslib)
library(dplyr)
library(sf)
library(leaflet)
library(ggplot2)
# readr

# -- code
ktools::source_code("./R")

# -- params
app_version <- "v1.3"
DEBUG <- TRUE

# -- settings
DISTANCE_ANOMALY <- 100
SPEED_ANOMALY <- 50

LEG_DISTANCE_MIN <- 60
LEG_DISTANCE_MAX <- 85
LEG_DISTANCE_STEP <- 5
