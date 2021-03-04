# Data wrangling

library(tidyverse)
library(readxl)
library(vcdExtra)

# read in Excel files
delude_data <- read_excel("./data/excel/COCA & COHA delude.xlsx")
GR_data <- read_excel("./data/excel/GR's data.xlsx")
GR_data <- GR_data[,colnames(GR_data)!="interpretator_type"]

delude_data$variety <- rep("AmE", nrow(delude_data))
delude_data$verb <- rep("delude", nrow(delude_data))
delude_data$negation[delude_data$negation == "y?"] <- "y"

str(GR_data)
str(delude_data)


# Expand GR data to case form
GR_data_case <- expand.dft(GR_data, freq = "count")

# Reduce COCA / COHA data to relevant columns
delude_data_selection <- delude_data[, colnames(delude_data) %in% colnames(GR_data_case)]

str(delude_data_selection)
str(GR_data_case)


data_merged <- bind_rows(delude_data_selection, GR_data_case)
str(data_merged)


write.csv(data_merged, "./data/data_merged.csv", row.names = F)
