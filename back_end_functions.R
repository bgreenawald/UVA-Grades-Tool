# Script to play around with various ideas for the grades app

setwd("C://Users/Student/Documents/UVA Grades App")

library(plyr)
library(dplyr)
library(ggplot2)
library(readxl)

# Read in the data
data = read_excel("Grades.xlsx")
use = tbl_df(data)

# Turn the period into a factor so it can later be sorted temporally
use$Period <- factor(use$Period, levels = c("2011.F", "2012.S", "2012.F",
                                               "2013.S", "2013.F", "2014.S",
                                               "2014.F", "2015.S"), labels = 
                          c("Fall 2011", "Spring 2012", "Fall 2012", "Spring 2013",
                            "Fall 2013", "Spring 2014", "Fall 2014", "Spring 2015"),
                     ordered = T)

# Common names for the merge
common_names <- .(Instructor.Last.Name, Instructor.First.Name, Instructor.Middle.Name,
                    Instructor.Email, Course.Number, Title, Period)

# Given some data frame within our grades data set, 
# merge sections within a given period and return the result.
# NOTE: GPA will now need to be recalculated so you must call
# calc_GPA right afterwards

row_merge <- function(in_data_frame){
  return(ddply(.data = in_data_frame, .variables = common_names, summarize, A_plus = sum(A_plus),
            A = sum(A), A_minus = sum(A_minus), B_plus = sum(B_plus), B = sum(B), B_minus = sum(B_minus),
            C_plus = sum(C_plus), C = sum(C), C_minus = sum(C_minus), D_plus = sum(D_plus), D = sum(D),
            D_minus = sum(D_minus), fail = sum(fail)))
}

# Given a row within the grades data set, calculate the GPA

calc_GPA <- function(in_data_row){
  # Calucate the total grade points earned in this row
  earned <- 4*in_data_row$A_plus + 4*in_data_row$A + 3.7*in_data_row$A_minus + 
    3.3*in_data_row$B_plus + 3*in_data_row$B + 2.7*in_data_row$B_minus + 
    2.3*in_data_row$C_plus + 2*in_data_row$C + 1.7*in_data_row$C_minus + 
    1.3*in_data_row$D_plus + 1*in_data_row$D + .7*in_data_row$D_minus
  
  # Calculate the maximum possible points that could be earned
  total <- in_data_row$Tot - in_data_row$DR - in_data_row$W
  
  # Return GPA (the ratio of these two). Note that number of credits will cancel
  # out so it is not included in this computation
  return(round(earned/total,2))
}