library_load <-
  function(){
    if(!require(pacman)){install.packages("pacman")}
    pacman::p_load(
      "DT", # for data table
      "glmmTMB", # for co-variate model
      "lme4", # for lme modeling
      "markdown", # for better markdown format
      "MASS",
      "sjmisc", # check nested/crossed structure
      "shiny",
      "shinyjs",
      "shinyalert",
      "shinydashboard",
      "shinyWidgets", # for widgets like progress bar
      "tidyverse", # for better markdown format
      "parallel" # bootstrapping parallel computing
    )
  }

library_load()