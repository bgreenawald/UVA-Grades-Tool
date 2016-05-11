#Script to play around with various ideas for the grades app

setwd("C://Users/Student/Documents/UVA Grades App")

library(plyr)
library(dplyr)
library(ggplot2)
library(readxl)

data = read_excel("Grades.xlsx")
use = tbl_df(data)

middleton = filter(use, Instructor.Last.Name == "Middleton")

ddply()