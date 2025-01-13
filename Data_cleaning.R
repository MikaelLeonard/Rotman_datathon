#' Rotman Datathon
#' Authors: Zachery Chan, Mausam Vaddakyil, Mikael Gouwtama 
#' Date: 

#### Loading libraries ####
library(readxl)
library(lubridate)

#### Importing Data ####

# Reading data from excel file 
raw_data <- read_excel("Datathon_data-2025-Raw.xlsx")

colnames(raw_data)

dim(raw_data)

# NAs in data
anyNA(raw_data)
# All in last 5 rows

#### Data preprocessing and cleaning ####

# Removing last 5 rows as they contain no data/observations 
data <- raw_data[-(3256: 3260), ]

# Recoding ".." as NAs
for (col in colnames(data)) {
  if (any(data[[col]] == "..", na.rm = TRUE)) {
    data[[col]][data[[col]] == ".."] <- NA
  }
}

dim(data)

# Using for loop to converty character format into numeric for relevant indicators
for (col in colnames(data)) {
  if (!col %in% c("Country Name", "Country Code", "Time", "Time Code")) {
    data[[col]] <- as.numeric(data[[col]])
  }
}

# converting from list to dataframe 
data_mtrx <- as.data.frame(data)

# Storing time in DATE format (USING IF NEEDED)
data_mtrx$Time_DATE <- paste0(data_mtrx$Time, "-01-01")

data_mtrx$Time_DATE <- (as.Date(data_mtrx$Time_DATE))

str(data_mtrx$Time_DATE)

summary(data_mtrx)

# Storing cleaned data with all variables in seperate df
clean_data <- data

# addressing high missingness in variables 
nonmiss_data <- clean_data 

# removing any variables with >30% missingness 
for (col in colnames(nonmiss_data)){
  pct_missing <- sum(is.na(nonmiss_data[[col]]))/length(nonmiss_data[[col]])
  
  # dropping the column if >30% missingness 
  if (pct_missing >= 0.3){
    cat(col, ":", pct_missing,"\n")
    nonmiss_data[[col]] <- NULL
  }
}

# Originally 283 variables
dim(clean_data)

# 147 variables removed due to high degree of missingness, left with 136 variables
dim(nonmiss_data)
