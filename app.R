rm(list = ls())

# Install the following packages-------------------------------------------------------------------------------------------------------------
# install.packages("shiny")
# install.packages("shinydashboard")
# install.packages("shinydashboardPlus")
# install.packages("htmltools")
# install.packages("bsplus")
# install.packages("plotly")
# install.packages("numbers")
# install.packages("shinyjs")
# install.packages("DT")
# install.packages("shinyFeedback")
# install.packages("knitr")
# tinytex::install_tinytex()
# install.packages ("webshot")
# webshot::install_phantomjs()
# -------------------------------------------------------------------------------------------------------------------------------------------

# Load libraries ----------------------------------------------------------------------------------------------------------------------------
library("shiny")
library("shinydashboard")
library("shinydashboardPlus")
library("htmltools")
library("bsplus")
library("plotly")
library("numbers")
library("shinyjs")
library("DT")
library("shinyFeedback")
library("knitr")
# -------------------------------------------------------------------------------------------------------------------------------------------

source("ui.R")

# SERVER -------------------------------------------------------------------------------------------------------------------------------------

source("server.R")

#Run the shiny app to display webpage
shinyApp(ui = ui, server = server)
