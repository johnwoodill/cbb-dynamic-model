
library(shiny)

# Define UI for slider demo application
shinyUI(pageWithSidebar(
  
  #  Application title
  headerPanel("CBB Optimal Spraying and Harvesting Strategy"),
  
  # Sidebar with sliders that demonstrate various available options
  sidebarPanel(
    
    tags$h3("Parameters"),
        
    textInput("acres", "Acres", "5"),
    
    textInput("cherry_per_acre", "Cherry per acre", "7500"),
    
    textInput("cost_s", "Cost to Spray", "180.00"),
    
    textInput("cost_h", "Cost to Harvest (per lbs)", "0.50"),
    
    checkboxGroupInput("harvestschedule", "Harvest Schedule", 
                       c("January" = "jan", "February" = "feb", "March" = "mar", "April" = "apr", "May" = "may", 
                         "June" = "jun", "July" = "jul", "August" = "aug", "September" = "sep", "October" = "oct", "November" = "nov", "December" = "dec"),
                       selected = c("sep", "oct", "nov", "dec")),
        
    tags$h3("Initial Infestation"),
    
    sliderInput("ni", "Not infested", 
                min=000, max=100, value=90, step=1),
    
    sliderInput("ab_live", "A/B Live", 
                min=000, max=100, value=8, step=1),
    
    sliderInput("ab_dead", "A/B Dead", 
                min=000, max=100, value=1, step=1),
    
    sliderInput("cd", "C/D", 
                min=000, max=100, value=1, step=1),
    
    br(),
    h3("Estimation")


    ),
  
  # Show a table summarizing the values entered
  mainPanel(
    tableOutput("datatable")
    #plotOutput("graph1"),
    #plotOutput("graph2")

  )
))