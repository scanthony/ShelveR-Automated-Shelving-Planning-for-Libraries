# Set your working directory here: setwd("") 

library(dplyr)
library(stringr)
library(readxl)

# Data used for automatic estimation of the thickness of books
train_page = c(242,181,324,232)
train_thick = c(14,12.5,26,18) # Thicknes of each book in milimeter

model <- lm(train_thick ~ train_page)

# Uncomment the following line to test the model
# print(predict(model, data.frame(train_page = c(324, 200))))

xlsx_table <- read_excel("ordered_data.xlsx", sheet = "Sheet1")
xlsx_table <- mutate(xlsx_table, thick = page)
xlsx_table$thick <- predict(model, data.frame(train_page = as.numeric(xlsx_table$thick)))

write_csv(xlsx_table, "ordered_data_with_thickness.csv")
