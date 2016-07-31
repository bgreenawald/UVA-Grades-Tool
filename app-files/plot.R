#Author: Ben Greenawald
#Contains the plots for the
#UVA Grades App

library(shiny)
library(plyr)
library(dplyr)
library(ggplot2)
library(readxl)
library(stringr)
library(reshape2)
library(foreach)

theme_custom <- function(base_size = 10, base_family = ""){
  theme_dark(base_size = base_size, base_family = base_family)  %+replace%
    theme(
      axis.text = element_text(colour = "white"),
      axis.title.x = element_text(colour = "white", face = "bold", size = 16),
      axis.title.y = element_text(colour = "white", angle = 90, face = "bold", size = 16),
      plot.title = element_text(colour = "White", size = 22, vjust = 0, face = "bold"),
      panel.grid.major = element_line(colour = "black"),
      panel.grid.minor = element_line(colour = "black"),
      plot.background = element_rect(fill="black"),
      legend.background = element_rect(fill = "grey38"),
      legend.text = element_text(colour = "white", size = 12)
  )   
      
}

theme_set(theme_custom())

#Plot for the GPA of a single class over time
GPAOverTimePlot <- function(plot_rows){
  plot_rows <- mutate(plot_rows, Course.GPA = calc_GPA_2(A_plus, A, A_minus, 
                                                         B_plus, B, B_minus, C_plus,
                                                         C, C_minus, D_plus, D, D_minus, Tot, DR, W))
  
  return(ggplot(plot_rows, aes(x = Period, y = Course.GPA), group = variable) + 
    geom_point(colour = "blue") + geom_line(group = 1, colour = "blue", size = 2) +
      ggtitle("Average Course GPA Over Available Semesters \n  ") + 
      labs(y ="Average Course GPA \n ", x = " \n Semester"))
}

GradeDistributionOverTime <- function(plot_rows){
  #Process the data
  plot_rows <- mutate(plot_rows, A_tot = A_plus + A)
  plot_rows <- mutate(plot_rows, Not_Passing = D_plus + D + D_minus + fail)
  plot_rows <- select(plot_rows, A_tot, A_minus, B_plus, B, B_minus,
                      C_plus, C, C_minus, Not_Passing, Period)
  
  #Melt the data
  melt_plot <- melt(plot_rows, id.vars = "Period")
   
  # #Plot the data
  return(ggplot(melt_plot, aes(x = Period, y = value, colour = variable, group = variable)) + 
    geom_line(aes(linetype = variable), size = 1) + ggtitle("Number of All Grades Over Available Semesters \n") +
      labs(x = "\n Semester", y = "Total Number of Each Grade Given \n"))
}

ParticularGradesOverTime <- function(plot_rows){
  #Melt the data
  melt_plot <- melt(plot_rows, id.vars = "Period")
  
  
  #Plot the data
  return(ggplot(melt_plot, aes(x = Period, y = value, colour = variable, group = variable)) +
    geom_line(aes(linetype = variable)) + ggtitle("Number of Specified Grades Over Available Semesters \n") +
      labs(x = "\n Semester", y = "Total Number of Each Grade Given \n"))
}