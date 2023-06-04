# names(tags)
###### --- 
# Functions to style the html
###### ---
blueColored <- function(x) {
  span(x, style = "color:blue")
}

tutorial_page <- 
  fluidPage(
    # title
    titlePanel("GtheoryShiny Tutorial"),
    h3("Example data:"),
    p(glue::glue("The shiny app currently has two example data sets. Users can click the Input data button in the Data Input page. Specifically, {paste0(c('Rajaratnam.2', 'Brennan.3.2'), collapse = ' and ')} are two example data sets in Brennan (2001a). Brennan.3.2 includes two facets -- {'Test and Rater'}; ID variable as {'Person'}; Outcome variable as {('Score')}.")),
    
    h3("Step 1: Read in example data or transformation"),
    p( "Click ", blueColored("Data Input"), " tab on the top navigation panel will open the page for data input and transformation. Chick Upload button and select the data. Check the ", em('long-format'), " checkbox under the Data property section." ),
    
    h3("Step 2: Specify each column's type"),
    p( "Click ", em("Data Structure"), " tab. Select ", blueColored("Person"), " for ", em("which column represents ID"), " question." ), 
    p( "Then check ", blueColored("Subset, Item, Rater, Ocasion"), " for ", em("Which column(s) represent facets"), " question." ), 
    p( "Finally select ", blueColored("Score"), " for ", em("Which column represents outcome"), " question." ),
    
    
    h3("Step 3: Run data analysis"),
    p("After confirm the facets/ID/Outcomes of data, click ", blueColored("Data Analysis"), " tab for the gtheory estimation. Note that the recommended formula for gtheory has been given to you in previous tab page. Alternatively, you may want to specify your formula in ", em("User-specified formula"), " section. Besides, you can choose the link function for the model. Finally, boostrap iterations can been selected for the bootstrapping standard diviation estimation." ), 
    p("Chick ",blueColored("Run"), " button to estimate and output gstudy results in the right panel. Then, download buttons called ", blueColored("Download gstudy result"), " will pop up for users to download factor scores or variance-covariance components."),
    
    ## notes
    h3("Updates (2023-02-12):"),
    p("This app has the function to estimate the fixed effects of covariates. To use this function, check", em("covariates"), "in Data Structure tab page. Then, the fixed effects estimates will be printed after users click ", em("gtheory estimate"), "button in Data Analysis tag page."),
    h3("Updates (2023-06-03):"),
    p("Upgrade the UI framework of app to shinydashboard; Univariate Gtheory has been texted. Testing of multivariate gtheory is ongoing. Currently the estimation of mGtheory uses", em('glmmTMB'), "package")
)
