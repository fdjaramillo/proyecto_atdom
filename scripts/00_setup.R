# ============================================================
# 00_setup.R
# Project setup: packages, and paths

# ---- Packages ----
library(grid)
library(tidyverse)       # dplyr, tidyr, ggplot2, readr, stringr, purrr, tibble
library(here)            # project-relative paths
library(lubridate)       # date handling: dmy(), ymd()
library(glue)            # SQL strings with variables
library(janitor)         # clean_names(), tabyl()
library(readxl)          # read Excel files
library(writexl)         # export Excel files
library(FactoMineR)      # Cluster
library(factoextra)      # Cluster
library(pheatmap)        # Heatmap
library(compareGroups)   #Results
library(epiR) #Incidence
library(broom)
library(nnet)
library(patchwork)
library(purrr)
library(stringr)
library(ggplot2)
library(labelled)
library(WeightIt)
library(cobalt)
library(purrr)
library(sf)
library(openrouteservice)
library(scales)
library(sf)
library(stringr)
library(stringi)
library(data.table)
library(xlsx)


# ---- General options ----

options(
  scipen = 999,
  dplyr.summarise.inform = FALSE
)

# ---- Source functions ----

source(here("R", "utils_functions.R"))
