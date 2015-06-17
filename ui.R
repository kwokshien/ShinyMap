#ui.R
#runApp(launch.browser=TRUE)
#runApp(display.mode = 'showcase')
#"E:/Company/Iconix/Big Data/2014_11_03 R/3 Big data week dangue/Shiny Barchart"

library(shiny)
require(rCharts)
require(devtools)
shinyUI(fluidPage(
  
  titlePanel("Total Dengue cases in Malaysia"),
  sidebarLayout(
  sidebarPanel(  
  selectInput(inputId="negeri",
              label = "Choose State",
              choices = sort(c(as.character(new_df$NEGERI),"ALL")),
              selected = "ALL"),
  selectInput(inputId = "Year",
              label = "Select Year",
              choices = c("2010","2011","2012","2013","2014","2015"),
              selected = "2010"),
  submitButton(text = "Submit")
  ),
  mainPanel(showOutput("map2","leaflet"))
)))
