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
library(ggthemes)

#Set up the custom solarized theme for the graphs
theme_custom <- theme_solarized_2(base_size = 16, light = FALSE) + theme(axis.text.x=element_text(angle=45, hjust=1))

legend_labels <- scale_linetype_discrete(name="Grade Given", 
                    breaks=c("A_tot", "A_minus", "B_plus", "B", "B_minus", "C_plus", "C", "C_minus", "Not_Passing", "Other"), 
                    labels=c("A+/A", "A-", "B+", "B", "B-", "C+", "C", "C-", "Not Passing", "Dropped/Withdrew/Other"))
legend_labels_2 <- scale_colour_discrete(name="Grade Given",
                    breaks=c("A_tot", "A_minus", "B_plus", "B", "B_minus", "C_plus", "C", "C_minus", "Not_Passing", "Other"), 
                    labels=c("A+/A", "A-", "B+", "B", "B-", "C+", "C", "C-", "Not Passing", "Dropped/Withdrew/Other"))
legend_labels_percent <- scale_linetype_discrete(name="Grades Given", 
                    breaks=c("A_tot_percent", "A_minus_percent", "B_plus_percent", "B_percent", "B_minus_percent", 
                             "C_plus_percent", "C_percent", "C_minus_percent", "Not_Passing_percent", "Other_percent"),
                    labels=c("A+/A", "A-", "B+", "B", "B-", "C+", "C", "C-", "Not Passing", "Dropped/Withdrew/Other"))
legend_labels_percent2 <- scale_colour_discrete(name="Grades Given", 
                    breaks=c("A_tot_percent", "A_minus_percent", "B_plus_percent", "B_percent", "B_minus_percent", 
                             "C_plus_percent", "C_percent", "C_minus_percent", "Not_Passing_percent", "Other_percent"),
                    labels=c("A+/A", "A-", "B+", "B", "B-", "C+", "C", "C-", "Not Passing", "Dropped/Withdrew/Other"))
#Plot for the GPA of a single class over time
GPAOverTimePlot <- function(plot_rows){
  plot_rows <- mutate(plot_rows, Course.GPA = calc_GPA_2(A_plus, A, A_minus, 
                                                         B_plus, B, B_minus, C_plus,
                                                         C, C_minus, D_plus, D, D_minus, Tot, DR, W, OT))
  
  return(ggplot(plot_rows, aes(x = Period, y = Course.GPA), group = variable) + 
    geom_point(colour = "blue") + geom_line(group = 1, colour = "blue", size = 2) +
      ggtitle("Average Course GPA Over Available Semesters \n ") + 
      labs(y ="Average Course GPA \n ", x = " \n Semester") + theme_custom)
}

GradeDistributionOverTime <- function(plot_rows){
  #Process the data
  plot_rows <- mutate(plot_rows, A_tot = A_plus + A)
  plot_rows <- mutate(plot_rows, Not_Passing = D_plus + D + D_minus + fail)
  plot_rows <- mutate(plot_rows, Other = DR + W + OT)
  plot_rows <- select(plot_rows, A_tot, A_minus, B_plus, B, B_minus,
                      C_plus, C, C_minus, Not_Passing, Other, Period)
  
  #Melt the data
  melt_plot <- melt(plot_rows, id.vars = "Period")
   
  # #Plot the data
  return(ggplot(melt_plot, aes(x = Period, y = value, colour = variable, group = variable)) + 
    geom_line(aes(linetype = variable), size = 1) + ggtitle("Number of All Grades Over Available Semesters \n") +
      labs(x = "\n Semester", y = "Total Number of Each Grade Given \n") + legend_labels  + legend_labels_2 + theme_custom)
}

ParticularGradesOverTime <- function(plot_rows){
  #Melt the data
  melt_plot <- melt(plot_rows, id.vars = "Period")
  
  
  #Plot the data
  return(ggplot(melt_plot, aes(x = Period, y = value, colour = variable, group = variable)) +
    geom_line(aes(linetype = variable)) + ggtitle("Number of Specified Grades Over Available Semesters \n") +
      labs(x = "\n Semester", y = "Total Number of Each Grade Given \n") + legend_labels  + legend_labels_2 + theme_custom)
}

GradePercentageOverTime <- function(plot_rows){
  #Process the data
  plot_rows <- mutate(plot_rows, A_tot = A_plus + A)
  plot_rows <- mutate(plot_rows, Not_Passing = D_plus + D + D_minus + fail)
  plot_rows <- mutate(plot_rows, Other = DR + W + OT)
  plot_rows <- select(plot_rows, A_tot, A_minus, B_plus, B, B_minus,
                      C_plus, C, C_minus, Not_Passing, Other, Period, Tot)
  
  plot_rows <- to_Percent(plot_rows)
  
  #Melt the data
  melt_plot <- melt(plot_rows, id.vars = "Period")
  
  # #Plot the data
  return(ggplot(melt_plot, aes(x = Period, y = value, colour = variable, group = variable)) + 
           geom_line(aes(linetype = variable), size = 1) + ggtitle("Percentage of All Grades Over Available Semesters \n") +
           labs(x = "\n Semester", y = "Percentage of Each Grade Given \n") + legend_labels_percent  + legend_labels_percent2 + theme_custom)
}

ParticularPercentageOverTime <- function(plot_rows){
  #Melt the data
  melt_plot <- melt(plot_rows, id.vars = "Period")
  
  # #Plot the data
  return(ggplot(melt_plot, aes(x = Period, y = value, colour = variable, group = variable)) + 
           geom_line(aes(linetype = variable), size = 1) + ggtitle("Percentage of Particular Over Available Semesters \n") +
           labs(x = "\n Semester", y = "Percentage of Grade Given \n") + legend_labels_percent  + legend_labels_percent2 + theme_custom)
}

NullPlot <- function(){
  df <- data.frame()
  g <- ggplot(df) + geom_point() + xlim(0, 10) + ylim(0, 10) + theme_custom
  return(g)
}
