## Script for combining scraped data from python with shapefile data from CoCt

# Import the necessary libraries
library(tidyverse)
library(sf)
library(sp)
library("RColorBrewer")


# Read in the house price data as scraped from the internet
data1 <- read.csv("property_prices_ct.csv")
data2 <- read.csv("property_prices_wp.csv")
data3 <- read.csv("property_prices_wwp.csv")

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
    # Create a column that stores the values as discrete categories as this makes the map
    # and legend look better; keep the continuous values for pop-up displays
    data_temp <- data_temp %>% mutate(new_cat = ifelse(new_col < -1, "NA", 
                                                       ifelse(new_col < (-0.25), "< -25%", ifelse(new_col < (-0.1), "-25% to -10%",
                                                                                                  ifelse(new_col < 0, "-10% to 0%", ifelse(new_col < 0.1, "0% to 10%",
                                                                                                                                           ifelse(new_col < 0.25, "10% to 25%", "> 25%")))))))
    # Replace the name of the temp columns with year-specific names; C -> continuous; D -> discrete
    names(data_temp)[grep("new_col", names(data_temp))] <- paste0("CX20",9+i,".20",9+j)
    names(data_temp)[grep("new_cat", names(data_temp))] <- paste0("DX20",9+i,".20",9+j)
    
    # Calculate an average price column for the average price between years i and j
    avg_price_col <- (data[,j]+data[,i])/2
    data_temp <- cbind(data_temp, avg_price_col)
    names(data_temp)[grep("avg_price_col", names(data_temp))] <- paste0("AvgX20",9+i,".20",9+j)
  }
}

# Small suburb fix-ups for a few non-matching suburbs
ct_suburbs[74, "Suburb"] = "CAMPS BAY"
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

save(combined_data, file="mapAppData.Rdata")
