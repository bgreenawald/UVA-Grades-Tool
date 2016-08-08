library(shiny)
library(plyr)
library(dplyr)
library(ggplot2)
library(readxl)
library(stringr)
library(reshape2)
library(reshape)
library(foreach)

#Add the back end functions to the namespace
setwd("C:/Users/Student/Documents/UVA Grades App")
source("app-files/back_end_functions.R")
source("app-files/plot.R")


# Read in the data
data <- read_excel("Grades.xlsx")
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
grades <- c("A_tot", "A_minus", "B_plus", "B", "B_minus", "C_plus", "C", "C_minus", "Not_Passing")
names(grades) <- c("A+/A", "A-", "B+", "B", "B-", "C+", "C", "C-", "Not Passing")

#Main server function
shinyServer(function(input, output) {
  #Adjust the UI based on the users first input
  output$ui <- renderUI({
    if (input$course_input != "i.e CS 2150") {
      #Split the input string
      course <- unlist(str_split(str_trim(input$course_input), " "))
      dept_label <- course[1]
      course_number <- course[2]
      
      #Filter out the irrelevant rows
      relevant_rows <-
        filter(use,
               Course.Number == as.integer(course_number),
               Subject == dept_label)
      
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
    if (!is.null(input$instructor)) {
      #Split the input string
      course <-
        unlist(str_split(str_trim(input$course_input), " "))
      dept_label <- course[1]
      course_number <- course[2]
      
      plot_rows <- filter(use,
                          Course.Number == course_number,
                          Instructor.Last.Name == input$instructor)
      
      plot_rows <- row_merge(plot_rows)
      #Plot depending on the input
      if (input$graph_type == "GPA Over Time") {
        GPAOverTimePlot(plot_rows)
        
      } else if (input$graph_type == "Grade Distribution Over Time") {
        if (!is.null(input$grades)) {
          #Code for reformatting
          if ("All Grades" %in% input$grades) {
            GradeDistributionOverTime(plot_rows)
            
          }
          else{
            #Process the data
            input$grades
            
            plot_rows <- mutate(plot_rows, A_tot = A_plus + A)
            plot_rows <-mutate(plot_rows, Not_Passing = D_plus + D + D_minus + fail)
            plot_rows <- plot_rows[, c(grades[c(input$grades)], "Period")]
            
            ParticularGradesOverTime(plot_rows)
          }
        } 
      }
    }
    
  })
  
})