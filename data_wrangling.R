# Data wrangling

library(tidyverse)
library(readxl)

# read in Excel files
#------------------------------------------------------------------------------

# AmE News

delude_AmE_news <- read_excel("./data/excel/AmNews_delude.xlsx")
delude_AmE_news <- delude_AmE_news[,1:10]
colnames(delude_AmE_news)[8] <- "interpretator"
colnames(delude_AmE_news)[10] <- "complementizer"
str(delude_AmE_news)

deceive_AmE_news <- read_excel("./data/excel/AmNews_deceive.xlsx")
deceive_AmE_news <- deceive_AmE_news[,1:10]
colnames(deceive_AmE_news)[8] <- "interpretator"
colnames(deceive_AmE_news)[10] <- "complementizer"
str(deceive_AmE_news)

fool_AmE_news <- read_excel("./data/excel/AmNews_fool_V.xlsx")
fool_AmE_news <- fool_AmE_news[,1:10]
colnames(fool_AmE_news)[8] <- "interpretator"
colnames(fool_AmE_news)[10] <- "complementizer"
str(fool_AmE_news)


# BrE News

delude_BrE_news <- read_excel("./data/excel/BrNews_delude.xlsx")
delude_BrE_news <- delude_BrE_news[,1:10]
colnames(delude_BrE_news)[8] <- "interpretator"
colnames(delude_BrE_news)[10] <- "complementizer"
str(delude_BrE_news)

deceive_BrE_news <- read_excel("./data/excel/BrNews_deceive.xlsx")
deceive_BrE_news <- deceive_BrE_news[,1:10]
colnames(deceive_BrE_news)[8] <- "interpretator"
colnames(deceive_BrE_news)[10] <- "complementizer"
str(deceive_BrE_news)

fool_BrE_news <- read_excel("./data/excel/BrNews_fool_V.xlsx")
fool_BrE_news <- fool_BrE_news[,1:10]
colnames(fool_BrE_news)[8] <- "interpretator"
colnames(fool_BrE_news)[10] <- "complementizer"
str(fool_BrE_news)


# Historical collections

delude_historical <- read_excel("./data/excel/Historical_collections_delude.xlsx")
delude_historical <- delude_historical[,1:10]
colnames(delude_historical)[8] <- "interpretator"
colnames(delude_historical)[10] <- "complementizer"
str(delude_historical)

deceive_historical <- read_excel("./data/excel/Historical_collections_deceive.xlsx")
deceive_historical <- deceive_historical[,1:10]
colnames(deceive_historical)[8] <- "interpretator"
colnames(deceive_historical)[10] <- "complementizer"
str(deceive_historical)

fool_historical <- read_excel("./data/excel/Historical_collections_fool.xlsx")
fool_historical <- fool_historical[,1:10]
colnames(fool_historical)[8] <- "interpretator"
colnames(fool_historical)[10] <- "complementizer"
str(fool_historical)


# combine data sets

all_data <- rbind(
  delude_AmE_news,
  deceive_AmE_news,
  fool_AmE_news,
  delude_BrE_news,
  deceive_BrE_news,
  fool_BrE_news,
  delude_historical,
  deceive_historical,
  fool_historical
)


# Create new variables
#------------------------------------------------------------------------------

# Year
all_data$year_bins_20 <- cut(
  all_data$year, c(1700, seq(1820, 2020, by=20)), 
  labels = c("<1820", "1820-1840", "1840-1860", "1860-1880", "1880-1900",
             "1900-1920",  "1920-1940", "1940-1960", "1960-1980", "1980-2000", 
             "2000-2020"))
all_data$year_bins_40 <- cut(
  all_data$year, c(1700, seq(1820, 2020, by=40)), 
  labels = c("<1820", "1820-1860", "1860-1900",
             "1900-1940", "1940-1980", "1980-2020"))

all_data$year_bins_100 <- cut(
  all_data$year, c(1700, 1850, 1950, 2020), 
  labels = c("<1850", "1850-1950", "1950-2020"))



# Genre
unique(all_data$genre)
all_data$genre[all_data$genre == "NEWS"] <- "News" 
all_data$genre[all_data$genre == "fiction"] <- "Fiction" 

all_data$macro_genre <- as.character(all_data$genre)
all_data$macro_genre[all_data$genre == "Magazine"] <- "non-fiction"
all_data$macro_genre[all_data$genre == "News"] <- "non-fiction"
all_data$macro_genre[all_data$genre == "Non-fiction"] <- "non-fiction"
all_data$macro_genre[all_data$genre == "Drama"] <- "fiction"
all_data$macro_genre[all_data$genre == "Fiction"] <- "fiction"
all_data$macro_genre[all_data$genre == "Spoken"] <- "spoken"

# Variety
all_data$AmE_c <- ifelse(all_data$variety=="AmE", 1, -1)

# Interpretator
all_data$interpretator_present <- ifelse(all_data$interpretator_type == "none", 0, 1)


all_data$year_c <- (all_data$year - 2000)/50


write.csv(all_data, "./data/all_data.csv", row.names=F)




