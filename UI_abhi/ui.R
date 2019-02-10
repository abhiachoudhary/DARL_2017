library(shiny)

#http://rstudio.github.io/shiny/tutorial/#welcome
#http://shiny.rstudio.com/
#https://shiny.rstudio.com/articles/dynamic-ui.html

list_counties <- NA;

# Define UI for miles per gallon application
shinyUI(pageWithSidebar(
  # Application title
  headerPanel("Miles Per Gallon"),
  
  # Sidebar with controls to select the variable to plot against mpg
  # and to specify whether outliers should be included
  sidebarPanel(
    #textInput("caption", "Caption:", "Data Summary"),
    #idea from https://gist.github.com/wch/4211337
    uiOutput("choose_state"),
    uiOutput("choose_county"),
    uiOutput("choose_city"),
    sliderInput("salary_range", "Salary Range:",
                min = 10000, max = 300000, value = c(50000,100000)),
    actionButton("add_roles", "Add Roles")
  ),
  
  mainPanel(
    h3(textOutput("caption")), 
    #tableOutput("view_jobs")
    dataTableOutput("view_jobs")
  )
))

