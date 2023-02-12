library_load <-
  function(){
    
    if(!require(pacman)){install.packages("pacman")}
    pacman::p_load(
      "shiny",
      "shinyjs",
      "shinyWidgets", # for widgets like progress bar
      "lme4", # for lme modeling
      "DT", # for data table
      "markdown", # for better markdown format
      "sjmisc", # check nested/crossed structure
      "tidyverse", # for better markdown format
      "lme4qtl" # for covariate model
    )
    
  }

library_load()