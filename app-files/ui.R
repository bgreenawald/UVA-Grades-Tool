#Megan's Child

library(shiny)

#Paragraph containing a description of the app
desc <- "Select a course in the sidebar to get information about
  the past grades in that course to give you a more educated decision
  about which classes to take"

#Add the back end functions to the namespace
setwd("C:/Users/Student/Documents/UVA Grades App")
source("app-files/back_end_functions.R")
source("app-files/plot.R")


# Read in the data
data <- read_excel("Grades.xlsx")
use <- Preprocess(data)

getInstructors <- function(x){
  #Split the input string
  course <- unlist(str_split(str_trim(x), " "))
  dept_label <- course[1]
  course_number <- course[2]
  
  #Filter out the irrelevant rows
  relevant_rows <-
    filter(use,
           Course.Number == as.integer(course_number),
           Subject == dept_label)
  
  return(relevant_rows)
}

# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = "bootstrap.css",
  
  # Application title
  titlePanel("UVA Grades App"),
  
  #Application sidebar
  sidebarLayout(position = "right",
    
    #Side panel            
    sidebarPanel(
      
      
      textInput("course_input", label = "Course Name",
                value = "i.e CS 2150"),
      
      uiOutput("ui"),
      
      selectInput("graph_type", "Graph type:",
                          c("GPA Over Time", "Grade Distribution Over Time")),
      
      conditionalPanel(condition = "input.graph_type == \"Grade Distribution Over Time\"",
                       checkboxGroupInput("grades", "Filter by grades", 
                                          choices = c("All Grades", "A+/A", "A-", 
                                                      "B+", "B", "B-", "C+", "C", "C-", "Not Passing", "Dropped/Withdrew/Other"))),
      
      conditionalPanel(condition = "input.graph_type == \"Grade Distribution Over Time\"",
                       checkboxGroupInput("type_grades", "Choose representation Type", 
                                          choices = c("Total Grades", "Percentage"), selected = "Total Grades"))
    
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