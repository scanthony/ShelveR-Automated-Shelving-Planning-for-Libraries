# Set your working directory here: setwd("") 

library(dplyr)
library(stringr)
library(readxl)
library(rebus)

xlsx_table <- read_excel("ordered_data_with_thickness.xlsx", sheet = "Sheet1")
xlsx_table$thick <- as.numeric(xlsx_table$thick)

# Total number of books
num_shelves <- 2 * (7 * 7 + 26 * 9) * 7 # Total number of shelves
len_shelf <- 800 # Length of each shelf in milimeter

fullness <- 0.85  # Each shelf will be about 85% full, when shelved with books
num_shelves_planned <- 0 # The number of shelves planned to be used (a counter variable)
correct_factor <- 0.85

# This function will do the shelving planning 
plan_shelves <- function(src, fullness, correct_factor) {
  num_books <- nrow(src)
  store <- list() # This variable will store the whole shelving planning
  shelf <- "" # This variable will store a single shelf's books
  in_use <- 0 # Percentage of a single shelf's space in use
  
  for (i in 1:num_books) {
    if (toupper(src[[i, "lent_out"]]) == "FALSE") {
        in_use <- in_use + src[[i, "thick"]] / len_shelf
          
        if (in_use * correct_factor > fullness) {
          in_use <- src[[i, "thick"]] / len_shelf
          num_shelves_planned <- num_shelves_planned + 1
          store <- c(store, list(list(num_shelves_planned, shelf)))
          shelf <- ""
        }
          
        if (typeof(shelf) == "character") {
          shelf <- src[i, ]
        } else {
          shelf <- union(shelf, src[i, ])
        }
        
        if (i == num_books) {
          num_shelves_planned <- num_shelves_planned + 1
          store <- c(store, list(list(num_shelves_planned, shelf)))
        }
      }
      if (i %% 10000 == 0) {
        print(paste("In progress: ", as.character(i), " books already processed."))
      }
  }
  return(store)
}

# A helper function for getting the basic classification numbers
get_classes <- function(src) {
  
  trim_tail <- function(stri) {
    if (str_detect(stri, pattern= UPPER %R% END)) {
      return (stri)
    } else {
      return (str_sub(stri, 1, 1))
    }
  }
  
  num_books <- nrow(src)
  result <- c()
  
  for (i in 1:num_books) {
    string_current <- str_sub(src[[i, "call_num_comp"]], 1, 2)
    if ((string_current %in% result) == FALSE) {
      result <- c(result, string_current)
    }
  }
  
  result <- sapply(result, trim_tail)
  
  return (result)
}

# Generate a summary of the autormated planning for human readability
summary_shelves <- function(planned, klasses) {
  library(stringr) 
  dict <- rep(0, length(klasses))
  names(dict) <- klasses
  
  for (i in 1:length(planned)) {
    shelf_now <- planned[[i]][[2]]
    
    books_num_now <- length(shelf_now)
    for (j in 1:books_num_now) {
      current_item <- shelf_now[j, "call_num_comp"][[1]]

      # The tryCatch function helps to avoid crashes when unclean data are used.
      tryCatch({
        if (str_sub(current_item, 1, 1) != "T") {
          current_item <- str_sub(current_item, 1, 1)
        } else if (str_sub(current_item, 1, 2) == "T!") {
          current_item <- "T"
        } else {
          current_item <- str_sub(current_item, 1, 2)
        }
        
        dict[current_item] <- dict[current_item] + 1/books_num_now
      }, error = function(e) {
        print("ERROR encountered!")
      }, finally={})
    }
  }
  
  return (dict)
}

# The code below execute fucntions above to plan the shelves
planned <- plan_shelves(xlsx_table, fullness, correct_factor)
print(paste(as.character(length(planned)), " of ", as.character(num_shelves), " shelves used!"))

klasses <- get_classes(xlsx_table)

summary_sh <- summary_shelves(planned, klasses)
summary_sh

save(summary_sh, file="Shelfing_Summary.RData")
