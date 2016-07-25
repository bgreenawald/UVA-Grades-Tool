library(shiny)
library(plyr)
library(dplyr)
library(ggplot2)
library(readxl)
library(stringr)
library(reshape2)

#Add the back end functions to the namespace
source("back_end_functions.R")
setwd("C:/Users/Student/Documents/UVA Grades App")

# Read in the data
data = read_excel("Grades.xlsx")
use = tbl_df(data)

# Turn the period into a factor so it can later be sorted temporally
use$Period <- factor(use$Period, levels = c("2011.F", "2012.S", "2012.F",
                                            "2013.S", "2013.F", "2014.S",
                                            "2014.F", "2015.S", "2015.F"), labels = 
                       c("Fall 2011", "Spring 2012", "Fall 2012", "Spring 2013",
                         "Fall 2013", "Spring 2014", "Fall 2014", "Spring 2015",
                         "Fall 2015"),
                     ordered = T)

# Common names for the merge
common_names <- .(Instructor.Last.Name, Instructor.First.Name, Instructor.Middle.Name,
                  Instructor.Email, Course.Number, Title, Period)

# Merge common sections
row_merge <- function(in_data_frame){
  return(ddply(.data = in_data_frame, .variables = common_names, summarize, A_plus = sum(A_plus),
               A = sum(A), A_minus = sum(A_minus), B_plus = sum(B_plus), B = sum(B), B_minus = sum(B_minus),
               C_plus = sum(C_plus), C = sum(C), C_minus = sum(C_minus), D_plus = sum(D_plus), D = sum(D),
               D_minus = sum(D_minus), fail = sum(fail), Tot = sum(Tot), DR = sum(DR), W = sum(W)))
}

#Main server function
shinyServer(function(input, output) {
  
  #Adjust the UI based on the users first input
  output$ui <- renderUI({
    
    if(input$course_input != "i.e CS 2150"){
      #Split the input string
      course <- unlist(str_split(str_trim(input$course_input), " "))
      dept_label <- course[1]
      course_number <- course[2]
      
      #Filter out the irrelevant rows
      relevant_rows <- filter(use, Course.Number == as.integer(course_number), Subject == dept_label)
      
      #Present the user with new choices
      selectInput("instructor", "Instructor Last Name", 
                  choices = c(relevant_rows$Instructor.Last.Name))
      
    }
    
    
  })
  #Only render our graph once the user has entered input
  
  output$plot <- renderPlot({
      if(!is.null(input$instructor)){
        #Split the input string
        course <- unlist(str_split(str_trim(input$course_input), " "))
        dept_label <- course[1]
        course_number <- course[2]
        
        plot_rows <- filter(use, Course.Number == course_number,
                            Instructor.Last.Name == input$instructor)
        
        plot_rows <- row_merge(plot_rows)
        #Plot depending on the input
        if(input$graph_type == "GPA Over Time"){
         
          
          plot_rows <- mutate(plot_rows, Course.GPA = calc_GPA_2(A_plus, A, A_minus, B_plus, B, B_minus, C_plus
                                                             , C, C_minus, D_plus, D, D_minus, Tot, DR, W))
          
          ggplot(plot_rows, aes(x = Period, y = Course.GPA), group = variable) + geom_point(colour = "Blue") + geom_line(group = 1, colour = "Orange")
          
        }else if(input$graph_type == "Grade Distribution Over Time"){
          #Code for reformatting 
          plot_rows <- select(plot_rows, A_plus, A, A_minus, B_plus, B, B_minus, C_plus, C, C_minus, D_plus, D, D_minus, fail, Period)
          melt_plot <- melt(plot_rows, id.vars = "Period")
          ggplot(melt_plot, aes(x = Period, y = value, colour = variable, group = variable)) + geom_line()
        }
        
      }
  
  })
  
  
  
})