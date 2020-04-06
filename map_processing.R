## Script for combining scraped data from python with shapefile data from CoCt

# Import the necessary libraries
library(tidyverse)
library(sf)
library(sp)
library("RColorBrewer")
library(tools)
#setwd("C:/Users/User/Documents/Python Scripts/Maps - Final")

# Read in the house price data as scraped from the internet
data1 <- read.csv("./data/property_prices_ct.csv")
data2 <- read.csv("./data/property_prices_wp.csv")
data3 <- read.csv("./data/property_prices_wwp.csv")

data <- full_join(data3, data2)
data <- full_join(data, data1)

# Remove the column for 2020 because there are many missing values here
data <- data[,-11]

data <- na.omit(data)

data <- data %>% rename(Suburb = X)
data$Suburb <- toupper(as.character(data$Suburb))

# Remove the "Oudekraal" suburb as the data scraped is for a different region
data <- data[-42,]

# Read in the suburbs data and convert to CRS 4326
ct_suburbs <- st_read("./data/Official_Suburbs.shp")
ct_suburbs <- ct_suburbs %>% rename(Suburb = OFC_SBRB_N)
ct_suburbs <- ct_suburbs %>% st_transform(crs = 4326)
ct_suburbs$Suburb <- as.character(ct_suburbs$Suburb)


# Cycle through the columns and create a new column to calculate the investment return for each of the
# time periods in the data (e.g. year 1 to 3; year 9 to 10; year 3 to 10...)
# Note: we start with col[2] and end with col[10] because there are 9 years from 2011 to 2019, but the first 
#column in the dataset is the suburbs column, so we start with column 2 which is X2011, the prices for 2011
data_temp <- data
for (i in 2:9){
  for (j in (i+1):10){
    # Calculate the annual mean compounding growth rate
    new_col <- exp(log(data[,j]/data[,i]))^(1/(j-i))-1
    data_temp <- cbind(data_temp, new_col)

    names(data_temp)[grep("new_col", names(data_temp))] <- paste0("CX20",9+i,".20",9+j)
    
    # Calculate an average price column for the average price between years i and j
    avg_price_col <- (data[,j]+data[,i])/2
    data_temp <- cbind(data_temp, avg_price_col)
    names(data_temp)[grep("avg_price_col", names(data_temp))] <- paste0("AvgX20",9+i,".20",9+j)
  }
}

# Small suburb fix-ups for a few non-matching suburbs

ct_suburbs[704, "Suburb"] = "CAMPS BAY"
ct_suburbs[423, "Suburb"] = "SIMONS TOWN"
ct_suburbs[37, "Suburb"] = "BO KAAP"
ct_suburbs[765, "Suburb"] = "ZONNEBLOEM"
#view(ct_suburbs$Suburb)

# Remove duplicate entries in the price data
data_temp <- unique(data_temp)

# Commented out code for looking for the suburbs missing in the map
#view(ct_suburbs$Suburb, title = "CTBURBS")
#view(data_temp$Suburb, "DATA")

# Join the CoCT suburbs data with the price data and remove rows where there isn't matching data
# between the two data sources
combined_data <- st_as_sf(na.omit(inner_join( ct_suburbs, data_temp, by="Suburb")))

# Processing of maps to create larger areas to allow for faster filtering on the map

subcouncils <- st_read("./data/Sub_Councils.shp") %>% st_transform(crs = 4326)
suburbs <- st_read("./data/Official_Suburbs.shp") %>% st_transform(crs = 4326)
subcouncil_names <- read.csv("./data/subcouncil_names.csv")
# Joins the two SF files, adding a column indicating which subcouncil each suburb falls into
joined_map <- st_join(suburbs, subcouncils, left = FALSE, largest = TRUE)

# Update some of the names in this joined data set so they match the other data sets
joined_map$OFC_SBRB_N <- as.character(joined_map$OFC_SBRB_N)

joined_map$OFC_SBRB_N[joined_map$OFC_SBRB_N == "SIMON'S TOWN"] <- "SIMONS TOWN"
joined_map$OFC_SBRB_N[joined_map$OFC_SBRB_N == "CAMPS BAY / BAKOVEN"] <- "CAMPS BAY"
joined_map$OFC_SBRB_N[joined_map$OFC_SBRB_N == "BO-KAAP"] <- "BO KAAP"
joined_map$OFC_SBRB_N[joined_map$OFC_SBRB_N == "DISTRICT SIX"] <- "ZONNEBLOEM"

# Exctract the data I need from the data.frame indicating which suburbs are in which councils
subcouncils <- joined_map %>% select(OFC_SBRB_N, SUB_CNCL_1, SUB_CNCL_N)
subcouncils <- data.frame(subcouncils)
# Convert to remove unneccessary geometry column
class(subcouncils) <- 'data.frame' 
subcouncils <- subcouncils %>% select(-geometry) %>% rename(Suburb = OFC_SBRB_N)
subcouncils$Suburb <- as.character(subcouncils$Suburb)

# Add the sub-council names defined by me into the subcouncils table
subcouncils <- left_join(subcouncils, subcouncil_names, by="SUB_CNCL_1")

# Join data frames and save to file
combined_data <- st_as_sf(na.omit(left_join(combined_data, subcouncils, by="Suburb")))
combined_data$Suburb <- tools::toTitleCase(tolower(combined_data$Suburb))

save(combined_data, file="./data/mapAppData.Rdata")
