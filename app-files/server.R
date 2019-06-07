library(shiny)
library(plyr)
library(dplyr)
library(ggplot2)
library(readxl)
library(readr)
library(stringr)
library(reshape2)
library(reshape)
library(foreach)

# Add the back end functions to the namespace
setwd("/home/benji/Documents/UVA-Grades-Tool")
source("app-files/back_end_functions.R")
source("app-files/plot.R")


# Read in the data
data <- read_csv("/home/benji/Documents/UVA-Grades-Tool/app-files/Grades.csv")
use <- Preprocess(data)

# Common names for the merge
common_names <-
  .(
    Instructor.Last.Name,
    Instructor.First.Name,
    Instructor.Middle.Name,
    Instructor.Email,
    Course.Number,
    Title,
    Period
  )


#Set up variables to convert from the grade selection (A+) to the grade format (A_plus)
grades <- c("A_tot", "A_minus", "B_plus", "B", "B_minus", "C_plus", "C", "C_minus", "Not_Passing", "Other")
names(grades) <- c("A+/A", "A-", "B+", "B", "B-", "C+", "C", "C-", "Not Passing", "Dropped/Withdrew/Other")

#Main server function
shinyServer(function(input, output) {
  #Adjust the UI based on the users first input
  output$ui <- renderUI({
    if (input$course_input != "i.e CS 2150") {
      #Preprocess the data
      relevant_rows <- get_Course_Data2(use, input$course_input)
      
      #Present the user with new choices
      selectInput(
        "instructor",
        "Instructor Last Name",
        choices = c(relevant_rows$Instructor.Last.Name)
      )
      
      
    }
    
    
  })
  #Only render our graph once the user has entered input
  
  output$plot <- renderPlot({
    if (!is.null(input$instructor) && input$instructor != "") {
      #Preprocess the data
      plot_rows <- as.data.frame(row_merge(get_Course_Data(use, input$course_input, input$instructor)))
      
      #Plot depending on the input
      if (input$graph_type == "GPA Over Time") {
        GPAOverTimePlot(plot_rows)
        
      } else if (input$graph_type == "Grade Distribution Over Time") {
        if (!is.null(input$grades)) {
          #Code for reformatting
          if(input$type_grades == "Total Grades"){
            if ("All Grades" %in% input$grades) {
              GradeDistributionOverTime(plot_rows)
              
            }
            else{
              #Process the data
  
              plot_rows <- mutate(plot_rows, A_tot = A_plus + A)
              plot_rows <- mutate(plot_rows, Not_Passing = D_plus + D + D_minus + fail)
              plot_rows <- mutate(plot_rows, Other = OT + DR + W)
              plot_rows <- plot_rows[, c(grades[input$grades], "Period")]
              
              ParticularGradesOverTime(plot_rows)
            }
          }else if(input$type_grades == "Percentage"){
            if ("All Grades" %in% input$grades) {
              GradePercentageOverTime(plot_rows)
              
            }
            else{
              #Process the data
              
              #Process the data
              plot_rows <- mutate(plot_rows, A_tot = A_plus + A)
              plot_rows <- mutate(plot_rows, Not_Passing = D_plus + D + D_minus + fail)
              plot_rows <- mutate(plot_rows, Other = DR + W + OT)
              plot_rows <- plot_rows[, c(grades[input$grades], "Period", "Tot")]
              plot_rows <- to_Percent(plot_rows)
              ParticularPercentageOverTime(plot_rows)
            }
          }
        } else{
          NullPlot()
        }
      }
    } else{
      NullPlot()
    }
    
  })
  
})

