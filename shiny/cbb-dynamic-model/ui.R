
library(shiny)

# Define UI for slider demo application
shinyUI(pageWithSidebar(
  
  #  Application title
  headerPanel("CBB Dynamic Model-Optimal Spraying and Harvesting Strategy"),
  
  # Sidebar with sliders that demonstrate various available options
  sidebarPanel(
    
    tags$h3("Parameters"),
        
    textInput("acres", "Acres", "2"),
    
    textInput("cherry_per_acre", "Cherry per acre", "7500"),
    
    textInput("cost_s", "Cost to Spray", "180.00"),
    
    textInput("cost_h", "Cost to Harvest (per lbs)", "0.50"),
    
    radioButtons("radio_decision", "Type of decision",
                 c("Cost v. Damage" = 1,
                   "Infestation v. AB Live " = 2)),
    
    checkboxGroupInput("harvestschedule", "Harvest Schedule", 
                       c("January" = "jan", "February" = "feb", "March" = "mar", "April" = "apr", "May" = "may", 
                         "June" = "jun", "July" = "jul", "August" = "aug", "September" = "sep", "October" = "oct", "November" = "nov", "December" = "dec"),
                       selected = c("sep", "oct", "nov", "dec")),
        
    tags$h3("Initial Infestation"),
    tags$h5("(Total must be <= 100)"),
    
    sliderInput("ab_live", "A/B Live", 
                min=000, max=100, value=8, step=1),
    
    sliderInput("ab_dead", "A/B Dead", 
                min=000, max=100, value=1, step=1),
    
    sliderInput("cd", "C/D", 
                min=000, max=100, value=1, step=1),
    
    br()
    ),
  
  # Show a table summarizing the values entered
  mainPanel(
    tableOutput("datatable"),
    
    textOutput("totalnetben"),
    plotOutput("graph1")
    #plotOutput("graph2")

  )
))