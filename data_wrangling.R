# Data wrangling

library(tidyverse)
library(readxl)

# read in Excel files
#------------------------------------------------------------------------------

# AmE News

delude_AmE_news <- read_excel("./data/excel/AmNews_delude.xlsx")
delude_AmE_news <- delude_AmE_news[,1:10]
colnames(delude_AmE_news)[8] <- "interpretator"
colnames(delude_AmE_news)[10] <- "complementation"
str(delude_AmE_news)

deceive_AmE_news <- read_excel("./data/excel/AmNews_deceive.xlsx")
deceive_AmE_news <- deceive_AmE_news[,1:10]
colnames(deceive_AmE_news)[8] <- "interpretator"
colnames(deceive_AmE_news)[10] <- "complementation"
str(deceive_AmE_news)

fool_AmE_news <- read_excel("./data/excel/AmNews_fool_V.xlsx")
fool_AmE_news <- fool_AmE_news[,1:10]
colnames(fool_AmE_news)[8] <- "interpretator"
colnames(fool_AmE_news)[10] <- "complementation"
str(fool_AmE_news)


# BrE News

delude_BrE_news <- read_excel("./data/excel/BrNews_delude.xlsx")
delude_BrE_news <- delude_BrE_news[,1:10]
colnames(delude_BrE_news)[8] <- "interpretator"
colnames(delude_BrE_news)[10] <- "complementation"
str(delude_BrE_news)

deceive_BrE_news <- read_excel("./data/excel/BrNews_deceive.xlsx")
deceive_BrE_news <- deceive_BrE_news[,1:10]
colnames(deceive_BrE_news)[8] <- "interpretator"
colnames(deceive_BrE_news)[10] <- "complementation"
str(deceive_BrE_news)

fool_BrE_news <- read_excel("./data/excel/BrNews_fool_V.xlsx")
fool_BrE_news <- fool_BrE_news[,1:10]
colnames(fool_BrE_news)[8] <- "interpretator"
colnames(fool_BrE_news)[10] <- "complementation"
str(fool_BrE_news)


# Historical collections

delude_historical <- read_excel("./data/excel/Historical_collections_delude.xlsx")
delude_historical <- delude_historical[,1:10]
colnames(delude_historical)[8] <- "interpretator"
colnames(delude_historical)[10] <- "complementation"
str(delude_historical)

deceive_historical <- read_excel("./data/excel/Historical_collections_deceive.xlsx")
deceive_historical <- deceive_historical[,1:10]
colnames(deceive_historical)[8] <- "interpretator"
colnames(deceive_historical)[10] <- "complementation"
str(deceive_historical)

fool_historical <- read_excel("./data/excel/Historical_collections_fool.xlsx")
fool_historical <- fool_historical[,1:10]
colnames(fool_historical)[8] <- "interpretator"
colnames(fool_historical)[10] <- "complementation"
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

# Transitivity
unique(all_data$transitivity)
all_data$transitivity_short <- factor(all_data$transitivity, 
                                      labels=c("intr", "pass", "refl", "tran"))

# Genre
unique(all_data$genre)
all_data$genre[all_data$genre == "NEWS"] <- "News" 
all_data$genre_short <- factor(all_data$genre, 
                               labels=c("dra", "fic", "mag", "new", "non", "spo"))

# Interpretator
all_data$interpretator_present <- ifelse(all_data$interpretator_type == "none", 0, 1)


# Exclude cases (code as NA)
all_data$transitivity[all_data$transitivity == "intransitive"] <- NA
all_data$transitivity_short[all_data$transitivity_short == "intr"] <- NA
all_data$transitivity <- droplevels(all_data$transitivity)
all_data$transitivity_short <- droplevels(all_data$transitivity_short)

all_data$negation[all_data$negation == "(question)"] <- NA
all_data$negation <- droplevels(all_data$negation)

all_data$year_c <- (all_data$year - 2000)/50


str(all_data)
range(all_data$year)
subset(all_data, year<1750)

write.csv(all_data, "./data/all_data.csv", row.names=F)




