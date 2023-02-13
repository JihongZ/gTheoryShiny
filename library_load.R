library_load <-
  function(){
    if(!require(pacman)){install.packages("pacman")}
    if(!require(glmmTMB)){install.packages("glmmTMB", type="source")}
    pacman::p_load(
      "shiny",
      "shinyjs",
      "shinyWidgets", # for widgets like progress bar
      "lme4", # for lme modeling
      "DT", # for data table
      "markdown", # for better markdown format
      "sjmisc", # check nested/crossed structure
      "tidyverse", # for better markdown format
      "lme4qtl" # for co-variate model
    )
  }

library_load()