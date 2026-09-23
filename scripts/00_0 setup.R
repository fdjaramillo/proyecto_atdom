# ============================================================
# 00_0setup.R
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
library(compareGroups)   #Results
library(epiR) #Incidence
library(broom)
library(nnet)
library(patchwork)
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
library(openxlsx)
library(hms)
library(emmeans)
library(sandwich)
library(lmtest)
library(FactoMineR)
library(factoextra)
library(missMDA)
library(pheatmap)



# ---- General options ----

options(
  scipen = 999,
  dplyr.summarise.inform = FALSE
)

# ---- Source functions ----

source(here("R", "utils_functions.R"))
source(here("R", "utils_validation.R"))
source(here("R", "utils_transformations.R"))
source(here("R", "utils_modeling.R"))

