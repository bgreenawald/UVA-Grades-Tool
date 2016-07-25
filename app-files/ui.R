#Megan's Child

library(shiny)

#Paragraph containing a description of the app
desc <- "Select a course in the sidebar to get information about
  the past grades in that course to give you a more educated decision
  about which classes to take"
# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = "bootstrap.css",
  
  # Application title
  titlePanel("UVA Grades App"),
  
  #Application sidebar
  sidebarLayout(position = "right",
    
    #Side panel            
    sidebarPanel(
      
      uiOutput("ui"),
      
      textInput("course_input", label = "Course Name",
                value = "i.e CS 2150"),

      selectInput("graph_type", "Graph type:",
                          c("GPA Over Time", "Grade Distribution Over Time"))
      
  ),
    
    #Main panel
    mainPanel(
      
        h3(desc, align = 'center'),
        textOutput("text1"),
        
        plotOutput("plot")
        )
      
  )
  )
)