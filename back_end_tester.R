# Script to play around with various ideas for the grades app

setwd("C://Users/Student/Documents/UVA Grades App")

library(plyr)
library(dplyr)
library(ggplot2)
library(readxl)

# Read in the data
data = read_excel("Grades.xlsx")
use = tbl_df(data)

# Common names for the merge
common_names <- .(Instructor.Last.Name, Instructor.First.Name, Instructor.Middle.Name,
                    Instructor.Email, Course.Number, Title, Period)

middleton = filter(use, Instructor.Last.Name == "Middleton")

# Merge common sections
row_merge <- function(in_data_frame){
  return(ddply(.data = in_data_frame, .variables = common_names, summarize, A_plus = sum(A_plus),
            A = sum(A), A_minus = sum(A_minus), B_plus = sum(B_plus), B = sum(B), B_minus = sum(B_minus),
            C_plus = sum(C_plus), C = sum(C), C_minus = sum(C_minus), D_plus = sum(D_plus), D = sum(D),
            D_minus = sum(D_minus), fail = sum(fail)))
}
