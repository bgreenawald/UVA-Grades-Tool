# Script to play around with various ideas for the grades app


library(plyr)
library(dplyr)
library(ggplot2)
library(readxl)



# Common names for the merge
common_names <- .(Instructor.Last.Name, Instructor.First.Name, Instructor.Middle.Name,
                    Instructor.Email, Course.Number, Title, Period)

Preprocess <- function(data){
  use <- tbl_df(data)
  
  # Turn the period into a factor so it can later be sorted temporally
  use$Period <- factor(use$Period, levels = c("2011.F", "2012.S", "2012.F",
                                              "2013.S", "2013.F", "2014.S",
                                              "2014.F", "2015.S", "2015.F"), labels = 
                         c("Fall 2011", "Spring 2012", "Fall 2012", "Spring 2013",
                           "Fall 2013", "Spring 2014", "Fall 2014", "Spring 2015",
                           "Fall 2015"),
                       ordered = T)
  
  return(use)
}
# Given some data frame within our grades data set, 
# merge sections within a given period and return the result.
# NOTE: GPA will now need to be recalculated so you must call
# calc_GPA right afterwards

# Merge common sections
row_merge <- function(in_data_frame){
  return(ddply(.data = in_data_frame, .variables = common_names, summarize, A_plus = sum(A_plus),
               A = sum(A), A_minus = sum(A_minus), B_plus = sum(B_plus), B = sum(B), B_minus = sum(B_minus),
               C_plus = sum(C_plus), C = sum(C), C_minus = sum(C_minus), D_plus = sum(D_plus), D = sum(D),
               D_minus = sum(D_minus), fail = sum(fail), Tot = sum(Tot), DR = sum(DR), W = sum(W), OT = sum(OT)))
}

# Given a row within the grades data set, calculate the GPA

calc_GPA <- function(in_data_row){
  # Calucate the total grade points earned in this row
  earned <- 4*in_data_row$A_plus + 4*in_data_row$A + 3.7*in_data_row$A_minus + 
    3.3*in_data_row$B_plus + 3*in_data_row$B + 2.7*in_data_row$B_minus + 
    2.3*in_data_row$C_plus + 2*in_data_row$C + 1.7*in_data_row$C_minus + 
    1.3*in_data_row$D_plus + 1*in_data_row$D + .7*in_data_row$D_minus
  
  # Calculate the maximum possible points that could be earned
  total <- in_data_row$Tot - in_data_row$DR - in_data_row$W - in_data_row$OT
  
  # Return GPA (the ratio of these two). Note that number of credits will cancel
  # out so it is not included in this computation
  return(round(earned/total,2))
}

#Modded calcGPA by passing in individual grades and not a row
calc_GPA_2 <- function(A_plus, A, A_minus, B_plus, B, B_minus, C_plus
                       , C, C_minus, D_plus, D, D_minus, Tot, DR, W, OT){
  # Calucate the total grade points earned in this row
  earned <- 4*A_plus + 4*A + 3.7*A_minus + 
    3.3*B_plus + 3*B + 2.7*B_minus + 
    2.3*C_plus + 2*C + 1.7*C_minus + 
    1.3*D_plus + 1*D + .7*D_minus
  
  # Calculate the maximum possible points that could be earned
  total <- Tot - DR - W - OT
  
  # Return GPA (the ratio of these two). Note that number of credits will cancel
  # out so it is not included in this computation
  return(round(earned/total,2))
}

get_Course_Data <- function(use, course_input, instructor){
  course <- unlist(str_split(str_trim(course_input), " "))
  dept_label <- course[1]
  course_number <- course[2]
  
  #Filter out the irrelevant rows
  relevant_rows <-
    filter(use,
           Course.Number == as.integer(course_number),
           Subject == dept_label,
           Instructor.Last.Name == instructor)
  
  return(relevant_rows)
}

get_Course_Data2 <- function(use, course_input){
  course <- unlist(str_split(str_trim(course_input), " "))
  dept_label <- course[1]
  course_number <- course[2]
  
  #Filter out the irrelevant rows
  relevant_rows <-
    filter(use,
           Course.Number == as.integer(course_number),
           Subject == dept_label)
  
  return(relevant_rows)
}

to_Percent <- function(data){
  use_names <- NULL
  grades <- c("A_tot", "A_minus", "B_plus", "B", "B_minus", "C_plus", "C", "C_minus", "Not_Passing", "Other")
  
  for(grade in grades){
    if(grade %in% colnames(data)){
      name <- paste(grade, "_percent", sep="")
      mutate_call <- lazyeval::interp(~ a / b, a = data[, grade], b = data$Tot)
      data <- data %>% mutate_(.dots = setNames(list(mutate_call), name))
      use_names <- append(use_names, name)
    }    
  }

  
  return(data[, c(use_names, "Period")])
}

