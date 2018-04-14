# Set your working directory here: setwd("") 

library(dplyr)
library(stringr)
library(rebus)
library(readxl)
library(readr)

comparify_cla_num <- function(cla_num) {
  ## Designed for Chinese Library Classification scheme
  ## Replace this function with your library's own classification
  ## schemes, e.g. Library of Congress Classification, etc.
  
  cla_num1 <- ''
  cla_num2 <- ''
  
  # Clearing all dots in CLC numbers
  cla_num <- str_replace_all(cla_num, '\\.', "")
  
  # Clearing all right parenthesis in CLC numbers
  cla_num <- str_replace_all(cla_num, '\\)', "")
  
  # Replace "-", "(", and "=" subdivision symbol for automated ordering
  # Refer to the ASCII table and your classification number's shelving specifications
  cla_num <- str_replace_all(cla_num, '\\-', '!')
  cla_num <- str_replace_all(cla_num, '\\(', '#')
  cla_num <- str_replace_all(cla_num, '\\=', '$')
  
  # Handle CLC numbers start with T
  pad_non_T <- function(x) {
    # Input is a CLC number without column
    len <- nchar(x)
    return_value <- ''
    if (str_sub(x, 1, 1) == "T") {
      return_value <- x
    } else {
      part1 <- str_sub(x, 1, 1)
      part2 <- ''
      if (len > 1) {
        part2 <- str_sub(x, 2, len)
      }
      return_value <- str_c(part1, '.', part2)
    }
    return (return_value)
  }
  
  # Handling CLC numbers with a column ":"
  if (str_detect(cla_num, pattern = ":")) {
    cla_num_sep <- str_split(cla_num, pattern = ":", simplify = T)
    cla_num1 <- cla_num_sep[1]
    cla_num2 <- cla_num_sep[2]
  } else {
    cla_num1 <- cla_num
    cla_num2 <- '.'
  }
  
  # Run the "T-issue" function
  cla_num1 <- pad_non_T(cla_num1)
  cla_num2 <- pad_non_T(cla_num2)
  
  # Pad the CLC number 1 & 2
  cla_num1 <- str_pad(cla_num1, width = 20, side="right", pad = " ")
  cla_num2 <- str_pad(cla_num2, width = 20, side="right", pad = " ")
  return (paste(cla_num1, cla_num2, sep=':'))
}

# Uncomment the following code to test the function for "comparified" Chinese Library Classification Number

#to_be_ordered <- tibble(clc_orig = c('K825.5=6','K825.5-49','F729','K825.5', 'F729(225)','F729-49','H319.4:I712','B94-49','I561.45','H319.4:D','TS976','TP319'))
#to_be_ordered <- mutate(to_be_ordered, clc_comparified = clc_orig)
#to_be_ordered$clc_comparified <- sapply(to_be_ordered$clc_comparified, comparify_cla_num)
#order_idx <- order(to_be_ordered$clc_comparified, method="radix")
#already_ordered <- to_be_ordered[order_idx,]

comparify_call_num <- function(call_num){
  
  ## This function is designed for ordering the call numbers of book items at the 
  ## Lending Section of Sichuan Provincial Library. You may need to replace this 
  ## function with your own version, to meet the need of your local call numbers.
  
  ## Detect if this is a previous call number (starts without a year), 
  ## or, a new call number (starts wiht a year)
  if (is.na(call_num)) {return ("")}
  if (call_num=="") {return ("")}
  if (!str_detect(call_num, pattern=START %R% DIGIT %R% DIGIT %R% DIGIT %R% DIGIT %R% "\\/")) {
    call_num <- str_c('9999/', call_num)
  } else {
    call_num <- str_replace_all(call_num, fixed(":v"), "/v")
  }
  
  ## Now separate the call number into 4 parts
  sep_call_num <- str_split(call_num, fixed("/"), n=4, simplify = TRUE)
  
  ## "Comparify" the classification numbers
  sep_call_num[,2] <- comparify_cla_num(sep_call_num[,2])
  
  ## Pad the cutters and sub-cutters
  sep_call_num[,3] <- str_pad(sep_call_num[,3], width = 5, side="left", pad = "0")
  sep_call_num[,4] <- str_pad(sep_call_num[,4], width = 15, side="right", pad = " ")
  
  comparified = str_c(sep_call_num[1,2],sep_call_num[1,1],sep_call_num[1,3],sep_call_num[1,4],sep="/")
  
  return(comparified)
}

## Test the call number ordering

xlsx_table <- read_excel("source_data.xlsx", sheet = "Sheet1")
xlsx_table <- mutate(xlsx_table, call_num_comp = call_number)
xlsx_table <- filter(xlsx_table, !is.na(call_number))
xlsx_table$call_num_comp <- sapply(xlsx_table$call_number, comparify_call_num)
order_idx <- order(xlsx_table$call_num_comp, method="radix")
already_ordered <- xlsx_table[order_idx,]
already_ordered$call_number[30000:30100]

already_ordered <- mutate(already_ordered, call_num_comp_trim = str_sub(call_num_comp, 1, 40))

write_csv(already_ordered, "ordered_data.csv")
