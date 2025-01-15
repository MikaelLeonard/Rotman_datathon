#Rotman Datathon 2025
# Libraries ----------------------------------------------------------------------------------------------------------

library(tidyverse)
library(funModeling)
library(Hmisc)
library(dplyr)
library(ggpubr)
library(tidyr)
library(knitr)
library(kableExtra)
library(readxl)
library(lubridate)

# Importing and cleaning data ----------------------------------------------------------------------------------------

#read raw data and metdata files
raw_data <- read_excel("Datathon_data-2025-Raw.xlsx", sheet = 1)
series_meta <- read_excel("Datathon_data-2025-Raw.xlsx", sheet = 2)
country_meta <- read_excel("Datathon_data-2025-Raw Metadata.xlsx", sheet = 2)
country.series_meta <- read_excel("Datathon_data-2025-Raw Metadata.xlsx", sheet = 3)
series.time_meta <- read_excel("Datathon_data-2025-Raw Metadata.xlsx", sheet = 4)
footnote_meta <- read_excel("Datathon_data-2025-Raw Metadata.xlsx", sheet = 5)

# Renaming the relevant columns
data <- raw_data %>%
  rename(
    country_name = "Country Name",
    country_code = "Country Code",
    year = "Time",
    time_code = "Time Code",
    cpi_FP.CPI.TOTL = "Consumer price index (2010 = 100) [FP.CPI.TOTL]",
    net_nat_income_NY.ADJ.NNTY.CD = "Adjusted net national income (current US$) [NY.ADJ.NNTY.CD]",
    net_nat_income_percapita_NY.ADJ.NNTY.PC.CD = "Adjusted net national income per capita (current US$) [NY.ADJ.NNTY.PC.CD]",
    net_nat_income_percent_NY.ADJ.NNTY.KD.ZG = "Adjusted net national income (annual % growth) [NY.ADJ.NNTY.KD.ZG]",
    gdp_percapita_NY.GDP.PCAP.PP.CD = "GDP per capita, PPP (current international $) [NY.GDP.PCAP.PP.CD]",
    gdp_percapita_growth_NY.GDP.PCAP.KD.ZG = "GDP per capita growth (annual %) [NY.GDP.PCAP.KD.ZG]",
    gdp_deflator_NY.GDP.DEFL.ZS = "GDP deflator (base year varies by country) [NY.GDP.DEFL.ZS]",
    gdp_NY.GDP.MKTP.CD = "GDP (current US$) [NY.GDP.MKTP.CD]",
    gdp_constant_NY.GDP.MKTP.KD = "GDP (constant 2015 US$) [NY.GDP.MKTP.KD]",
    business_startup_cost_IC.REG.COST.PC.ZS = "Cost of business start-up procedures (% of GNI per capita) [IC.REG.COST.PC.ZS]",
    good_service_expense_GC.XPN.GSRV.ZS = "Goods and services expense (% of expense) [GC.XPN.GSRV.ZS]",
    household_npish_expenditure_NE.CON.PRVT.KD.ZG = "Households and NPISHs Final consumption expenditure (annual % growth) [NE.CON.PRVT.KD.ZG]",
    household_npish_expenditure_percapita_NE.CON.PRVT.PC.KD.ZG = "Households and NPISHs Final consumption expenditure per capita growth (annual %) [NE.CON.PRVT.PC.KD.ZG]",
    part_time_employment_SL.TLF.PART.ZS = "Part time employment, total (% of total employment) [SL.TLF.PART.ZS]",
    final_consumption_expenditure_NE.CON.TOTL.ZS = "Final consumption expenditure (% of GDP) [NE.CON.TOTL.ZS]",
    net_primary_income_NY.GSR.NFCY.CD = "Net primary income (Net income from abroad) (current US$) [NY.GSR.NFCY.CD]",
    human_capital_index_HD.HCI.OVRL = "Human capital index (HCI) (scale 0-1) [HD.HCI.OVRL]",
    education_expenditure_SE.XPD.CTOT.ZS = "Current education expenditure, total (% of total expenditure in public institutions) [SE.XPD.CTOT.ZS]",
    life_expectancy_SP.DYN.LE00.IN = "Life expectancy at birth, total (years) [SP.DYN.LE00.IN]",
    gini_SI.POV.GINI ="Gini index [SI.POV.GINI]",
    unemployment_SL.UEM.TOTL.NE.ZS = "Unemployment, total (% of total labor force) (national estimate) [SL.UEM.TOTL.NE.ZS]",
    unemployment_youth_SL.UEM.1524.NE.ZS = "Unemployment, youth total (% of total labor force ages 15-24) (national estimate) [SL.UEM.1524.NE.ZS]",
    export_goods_service_NE.EXP.GNFS.KD.ZG ="Exports of goods and services (annual % growth) [NE.EXP.GNFS.KD.ZG]",
    merch_export_TX.VAL.MRCH.CD.WT = "Merchandise exports (current US$) [TX.VAL.MRCH.CD.WT]",
    merch_import_TM.VAL.MRCH.CD.WT = "Merchandise imports (current US$) [TM.VAL.MRCH.CD.WT]",
    manufacturing_NV.IND.MANF.KD.ZG = "Manufacturing, value added (annual % growth) [NV.IND.MANF.KD.ZG]",
    lpi_LP.LPI.INFR.XQ = "Logistics performance index: Quality of trade and transport-related infrastructure (1=low to 5=high) [LP.LPI.INFR.XQ]",
    transport_comm_export_TX.VAL.TRAN.ZS.WT ="Transport services (% of commercial service exports) [TX.VAL.TRAN.ZS.WT]",
    transport_comm_import_TM.VAL.TRAN.ZS.WT = "Transport services (% of commercial service imports) [TM.VAL.TRAN.ZS.WT]",
    transport_export_bop_BX.GSR.TRAN.ZS = "Transport services (% of service exports, BoP) [BX.GSR.TRAN.ZS]",
    transport_import_bop_BM.GSR.TRAN.ZS = "Transport services (% of service imports, BoP) [BM.GSR.TRAN.ZS]",
    air_transport_IS.AIR.GOOD.MT.K1 = "Air transport, freight (million ton-km) [IS.AIR.GOOD.MT.K1]",
    trade_NE.TRD.GNFS.ZS = "Trade (% of GDP) [NE.TRD.GNFS.ZS]",
    trade_goods_service_BN.GSR.GNFS.CD = "Net trade in goods and services (BoP, current US$) [BN.GSR.GNFS.CD]",
    net_primary_income_BN.GSR.FCTY.CD = "Net primary income (BoP, current US$) [BN.GSR.FCTY.CD]",
    household_consumption_expenditure_NE.CON.PRVT.KD.ZG = "Households and NPISHs Final consumption expenditure (annual % growth) [NE.CON.PRVT.KD.ZG]"
  )

data <- data %>%
  select(country_name, country_code, year, time_code, cpi_FP.CPI.TOTL, net_nat_income_NY.ADJ.NNTY.CD, net_nat_income_percapita_NY.ADJ.NNTY.PC.CD, net_nat_income_percent_NY.ADJ.NNTY.KD.ZG,
         gdp_percapita_NY.GDP.PCAP.PP.CD, gdp_percapita_growth_NY.GDP.PCAP.KD.ZG, gdp_deflator_NY.GDP.DEFL.ZS, gdp_NY.GDP.MKTP.CD, gdp_constant_NY.GDP.MKTP.KD,
         business_startup_cost_IC.REG.COST.PC.ZS, good_service_expense_GC.XPN.GSRV.ZS, household_npish_expenditure_percapita_NE.CON.PRVT.PC.KD.ZG,
         part_time_employment_SL.TLF.PART.ZS, final_consumption_expenditure_NE.CON.TOTL.ZS, net_primary_income_NY.GSR.NFCY.CD, human_capital_index_HD.HCI.OVRL, education_expenditure_SE.XPD.CTOT.ZS,
         life_expectancy_SP.DYN.LE00.IN, gini_SI.POV.GINI, unemployment_SL.UEM.TOTL.NE.ZS, unemployment_youth_SL.UEM.1524.NE.ZS, export_goods_service_NE.EXP.GNFS.KD.ZG, 
         merch_export_TX.VAL.MRCH.CD.WT, merch_import_TM.VAL.MRCH.CD.WT, manufacturing_NV.IND.MANF.KD.ZG, lpi_LP.LPI.INFR.XQ, transport_comm_export_TX.VAL.TRAN.ZS.WT, transport_comm_import_TM.VAL.TRAN.ZS.WT,
         transport_export_bop_BX.GSR.TRAN.ZS, transport_import_bop_BM.GSR.TRAN.ZS, air_transport_IS.AIR.GOOD.MT.K1, trade_NE.TRD.GNFS.ZS, trade_goods_service_BN.GSR.GNFS.CD,
         net_primary_income_BN.GSR.FCTY.CD, household_consumption_expenditure_NE.CON.PRVT.KD.ZG)

colnames(data)

dim(data)

# NAs in data
anyNA(data)
# All in last 5 rows

#### Data preprocessing and cleaning ####

# Removing last 5 rows as they contain no data/observations 
data <- data[-(3256: 3260), ]

# Recoding ".." as NAs
for (col in colnames(data)) {
  if (any(data[[col]] == "..", na.rm = TRUE)) {
    data[[col]][data[[col]] == ".."] <- NA
  }
}

dim(data)

# Using for loop to converty character format into numeric for relevant indicators
for (col in colnames(data)) {
  if (!col %in% c("country_name", "country_code", "time", "time_code")) {
    data[[col]] <- as.numeric(data[[col]])
  }
}

################################################
###Subsetting for different Countries/Regions###

#G7
g7 <- data %>%
  filter(data$country_name == c("Canada","France", "Germany", "Italy", "Japan", "United Kingdom", "United States"))

#OECD
oecd <- data %>%
  filter(data$country_name == c("Australia", "Austria", "Belgium", "Canada", "Chile", "Colombia", "Costa Rica", "Czech Republic",
                                "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland",
                                "Israel", "Italy", "Japan", "Korea", "Latvia", "Lithuania", "Luxembourg", "Mexico", "Netherlands",
                                "New Zealand", "Norway", "Poland", "Portugal", "Slovak Republic", "Slovenia", "Spain", "Sweden", 
                                "Switzerland", "Turkey", "United Kingdom", "United States"))


#Top 10 highest gdp
high.gdp <- data %>%
  filter(data$country_name == c("United States", "China", "Germany", "Japan", "India", "United Kingdom", "France", "Italy", 
  "Brazil", "Canada"))

#Top 10 lowest gdp
low.gdp <- data %>%
  filter(data$country_name == c("American Samoa", "São Tomé and Principe", "Dominica", "St. Martin (French part)", "Tonga", "Micronesia, Fed. Sts.",
                                "Palau", "Kiribati", "Marshall Islands", "Nauru", "Tuvalu"))

# Global South Countries
global.south <- data %>%
  filter(data$country_name == c(
  "Afghanistan", "Albania", "Algeria", "Andorra", "Angola", "Anguilla", "Antigua and Barbuda",
  "Argentina", "Armenia", "Aruba", "Azerbaijan", "Bahamas", "Bahrain", "Bangladesh", "Barbados",
  "Belarus", "Belize", "Benin", "Bhutan", "Bolivia", "Bosnia and Herzegovina", "Botswana",
  "Brazil", "Brunei Darussalam", "Bulgaria", "Burkina Faso", "Burundi", "Cambodia", "Cameroon",
  "Cape Verde", "Central African Republic", "Chad", "Channel Islands", "Chile", "China",
  "Colombia", "Comoros", "Congo", "Costa Rica", "Côte d'Ivoire", "Croatia", "Cuba", "Cyprus",
  "Czech Republic", "Djibouti", "Dominica", "Dominican Republic", "Ecuador", "Egypt", "El Salvador",
  "Equatorial Guinea", "Eritrea", "Ethiopia", "Fiji", "French Guiana", "French Polynesia", "Gabon",
  "Gambia", "Georgia", "Ghana", "Grenada", "Guadeloupe", "Guam", "Guatemala", "Guinea", "Guinea-Bissau",
  "Guyana", "Haiti", "Honduras", "Hong Kong", "India", "Indonesia", "Iran", "Iraq", "Jamaica", "Jordan",
  "Kazakhstan", "Kenya", "Kiribati", "Korea, Democratic People's Republic of", "Kuwait", "Kyrgyzstan",
  "Lao People's Democratic Republic", "Lebanon", "Lesotho", "Liberia", "Libya", "Madagascar", "Malawi",
  "Malaysia", "Maldives", "Mali", "Malta", "Marshall Islands", "Martinique", "Mauritania", "Mauritius",
  "Mayotte", "Mexico", "Micronesia, Federated States of", "Mongolia", "Montenegro", "Morocco", "Mozambique",
  "Myanmar", "Namibia", "Nauru", "Nepal", "Netherlands Antilles", "New Caledonia", "Nicaragua", "Niger",
  "Nigeria", "Oman", "Pakistan", "Palau", "Palestinian Territory, Occupied", "Panama", "Papua New Guinea",
  "Paraguay", "Peru", "Philippines", "Puerto Rico", "Qatar", "Republic of Moldova", "Réunion", "Romania",
  "Russian Federation", "Rwanda", "Saint Kitts and Nevis", "Saint Lucia", "Saint Vincent and the Grenadines",
  "Samoa", "Sao Tome and Principe", "Saudi Arabia", "Senegal", "Serbia", "Seychelles", "Sierra Leone",
  "Singapore", "Slovakia", "Solomon Islands", "Somalia", "South Africa", "South Sudan", "Sri Lanka", "Sudan",
  "Suriname", "Swaziland", "Syrian Arab Republic", "Taiwan", "Tajikistan", "Tanzania", "Thailand",
  "The former Yugoslav Republic of Macedonia", "Timor-Leste", "Togo", "Tonga", "Trinidad and Tobago",
  "Tunisia", "Turkey", "Turkmenistan", "Tuvalu", "Uganda", "Ukraine", "United Arab Emirates", "Uruguay",
  "Uzbekistan", "Vanuatu", "Venezuela", "Viet Nam", "Virgin Islands", "Western Sahara", "Yemen", "Zambia",
  "Zimbabwe"
))

# Global North Countries
global.north <- data %>%
  filter(data$country_name == c(
  "Australia", "Austria", "Belgium", "Canada", "Denmark", "Estonia", "Finland", "France", "Germany",
  "Greece", "Hungary", "Iceland", "Ireland", "Israel", "Italy", "Japan", "Korea, Republic of", "Latvia",
  "Lithuania", "Liechtenstein", "Luxembourg", "Monaco", "Netherlands", "New Zealand", "Norway", "Poland",
  "Portugal", "San Marino", "Slovenia", "Spain", "Sweden", "Switzerland", "United Kingdom", "United States"
))


#################
## Correlation ##















#########################
###### I'm Confused #####
# converting from list to dataframe 
data_mtrx <- as.data.frame(data)

# Storing time in DATE format (USING IF NEEDED)
data_mtrx$Time_DATE <- paste0(data_mtrx$Time, "-01-01")

data_mtrx$Time_DATE <- (as.Date(data_mtrx$Time_DATE))

str(data_mtrx$Time_DATE)

summary(data_mtrx)

##########################

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

