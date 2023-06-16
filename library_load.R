library_load <-
  function(){
    if(!require(pacman)){install.packages("pacman")}
    pacman::p_load(
      "shiny",
      "shinyjs",
      "shinyalert",
      "shinydashboard",
      "shinyWidgets", # for widgets like progress bar
      "DT", # for data table
      "glmmTMB", # for co-variate model
      "gtheory", # for gtheory
      "lme4", # for lme modeling
      "markdown", # for better markdown format
      "MASS",
      "sjmisc", # check nested/crossed structure
      "tidyverse", # for better markdown format
      "stringr",
      "parallel" # bootstrapping parallel computing
    )
  }

library_load()