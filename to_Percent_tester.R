# Test script for to_percent
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
grades <- c("A_tot", "A_minus", "B_plus", "B", "B_minus", "C_plus", "C", "C_minus", "Not_Passing", "Other")
names(grades) <- c("A+/A", "A-", "B+", "B", "B-", "C+", "C", "C-", "Not Passing", "Dropped/Withdrew/Other")

plot_rows <- row_merge(get_Course_Data(use, "CS 2150", "Bloomfield"))

#Process the data
plot_rows <- mutate(plot_rows, A_tot = A_plus + A)
plot_rows <- mutate(plot_rows, Not_Passing = D_plus + D + D_minus + fail)
plot_rows <- mutate(plot_rows, Other = DR + W + OT)
plot_rows <- select(plot_rows, A_tot, A_minus, B_plus, B, B_minus,
                    C_plus, C, C_minus, Not_Passing, Other, Period, Tot)

use_names <- NULL
grades <- c("A_tot", "B", "B_minus", "C_plus", "C", "C_minus", "Not_Passing", "Other")
data <- plot_rows
for(grade in grades){
  if(grade %in% colnames(data)){
    name <- paste(grade, "_percent", sep="")
    mutate_call <- lazyeval::interp(~ a / b, a = data[, grade], b = data$Tot)
    data <- data %>% mutate_(.dots = setNames(list(mutate_call), name))
    use_names <- append(use_names, name)
    #use_names <- c(use_names, name)
  }    
}

temp <- data[, c(use_names, "Period")]