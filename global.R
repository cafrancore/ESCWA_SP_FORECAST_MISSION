# ==== global.R ====

# 1) Paquetes (sin instalar en runtime)
pkgs <- c(
  "dplyr","tidyr","data.table","tibble","stringr","forcats","readxl","openxlsx",
  "sf","sp","raster","terra","leaflet","classInt",
  "ggplot2","ggpubr","viridis","RColorBrewer","scales","gridExtra","treemapify",
  "shiny","shinyjs","shinydashboard","shinyWidgets","DT",
  "psych","cluster","glmnet","pROC","randomForest","ppcor","vars",
  "haven","foreign","labelled","igraph","rpart","rpart.plot",
  "forecast","tseries","urca","pdp","here","proxyC","extrafont","factoextra","wesanderson","purrr"
)
invisible(lapply(unique(pkgs), require, character.only = TRUE, quietly = TRUE))
options(scipen = 999, dplyr.summarise.inform = FALSE)

if (Sys.info()[["sysname"]] == "Windows") {
  try(extrafont::loadfonts(device = "win", quiet = TRUE), silent = TRUE)
}

# 2) Paths
root_dir        <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
userLocation    <- root_dir
inputLocation   <- file.path(root_dir, "Input")
dataLocation    <- file.path(root_dir, "Data")
#outputLocation  <- file.path(root_dir, "New_Output")
Gislocation     <- file.path(inputLocation, "GIS")
dictionaryLocation <- file.path(inputLocation, "Dictionary")

# 3) Utilidades básicas
safe_source <- function(path, local = TRUE) {
  if (file.exists(path)) {
    message("Sourcing: ", path)
    source(path, local = local)
  } else {
    warning("No existe: ", path)
  }
}

# 4) Diccionario (labels)
dict_path <- file.path(dictionaryLocation, "DictionaryR.xlsx")
stopifnot(file.exists(dict_path))
VarLabels <- as.matrix(readxl::read_excel(dict_path, sheet = "Dashboard", range = "A1:C180"))
VarLabels[is.na(VarLabels)] <- ""
VarLabels[,1] <- trimws(VarLabels[,1])      # <-- tu FIX de Code


# 9) Idioma por defecto (1=en, 2=ar)
language <- 1
