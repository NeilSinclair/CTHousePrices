# Shiny App that displays average house prices the change in house prices on a map of Cape Town
# The data was scraped using Python, saved as a .csv and then imported into R

# Import the necessary libraries
library(shiny)
library(ggplot2)
library(Rcpp)
library(tidyverse)
library(sf)
library(leaflet)
library(sp)
library("RColorBrewer")
library(DT)
library(data.table)

# Read in the house price data as scraped from the internet
data1 <- read.csv("C:/Users/User/OneDrive/Documents/UCT/Courses/EDA/Maps/property_prices_ct.csv")
data2 <- read.csv("C:/Users/User/OneDrive/Documents/UCT/Courses/EDA/Maps/property_prices_wp.csv")
data3 <- read.csv("C:/Users/User/OneDrive/Documents/UCT/Courses/EDA/Maps/property_prices_wwp.csv")

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
ct_suburbs <- st_read("C:/Users/User/OneDrive/Documents/UCT/Courses/EDA/Maps/data/Official_Suburbs.shp")
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

# Create the Shiny App UI
ui <- fluidPage(

    # Application title
    titlePanel("Cape Town suburb price data"),

    sidebarLayout(
        sidebarPanel(
            # Explaination of what to do 
            helpText("To view data for specific suburbs, select the 'Selected suburbs'
            radio buttom and then type the suburb name in the Suburb box. Press 'backspace' to remove a 
            suburb. To view price changes over time, uses the slider below to select different time frames."),
            # Radio buttons for selection All suburbs or just specific suburbs to display on the map
            radioButtons("radio", label = h4("Suburb display options"),
                         choices = list("All suburbs" = 1, "Selected suburb(s)" = 2)),
            # Drop down suburb-selection menu
            selectInput("suburb", label = "Suburb", choices = combined_data$Suburb, selected = "",
                        multiple = TRUE),
            radioButtons("priceRadio", label = h4("Map data display options"),
                         choices = list("% price change" = 1, "Average price" = 2)),
            # Slider to choose which years to see price change info for
            sliderInput("range", label = h3("Time period"), min = 2011, 
                           max = 2019, value = c(2017,2019), sep = "")
            ),
      
        mainPanel(
          helpText("This map visualises the compounded annual changes in house prices over time in 
                   the City of Cape Town. Click on suburbs on the map to see the latest price info 
                   and the suburb name."), 
          leafletOutput("suburbsMap")
           
        )
    )
)

server <- function(input, output) {
    
    output$suburbsMap <- renderLeaflet({
        x1 <- input$range[1]
        x2 <- input$range[2]
        # Select the discrete (categorical) and continuous returns
        return_range_D <- paste0("DX",x1,".",x2)
        return_range_C <- paste0("CX",x1,".",x2)
        
        # Select the data just years that are being compared by choosing the pre-calculated
        # column that stores the % return values
        filtered_data <- combined_data %>% select(!!as.symbol(return_range_D), !!as.symbol(return_range_C),
                                                  Suburb, X2019, !!as.symbol(paste0("AvgX",x1,".",x2)))
        names(filtered_data)[grep("^DX20", names(filtered_data))] <- "DReturn"
        names(filtered_data)[grep("^CX20", names(filtered_data))] <- "CReturn"
        
        # Remove impossible values (< -1) and multiply by 100 so that it shows up correctly as a %
        filtered_data <- filtered_data %>% dplyr::filter(CReturn > -1)
        filtered_data$CReturn <- filtered_data$CReturn*100
        filtered_data$X2019 <- substr(filtered_data$X2019/10^6, start = 1, stop = 4)
        
        # If the "show all suburbs" button is not selected, then only show the selected suburbs
        if(input$radio == 2){
           filtered_data <- filtered_data %>% filter(Suburb %in% input$suburb) 
        }
        
        # If the user wants to see the change in price over time, show the following map
        if(input$priceRadio == 1){
          
          pal <- colorFactor("RdYlGn", 
                             levels = as.factor(c("< -25%", "-25% to -10%", "-10% to 0%",
                                                  "0% to 10%", "10% to 25%", "> 25%")),
                             ordered = TRUE)
          leaflet() %>%
              addProviderTiles("Esri.WorldStreetMap") %>%
              setView(lng = 18.5, lat = -34, zoom = 10) %>%
              addPolygons(data = filtered_data, color = "black", weight = 1, fillColor = ~pal(DReturn), fillOpacity = 1,
                          popup = paste0("<b>Suburb: </b>", filtered_data$Suburb, "<br>",
                          "<b>Annual price change ", x1, " to ", x2, ":</b> ", as.integer(filtered_data$CReturn), 
                          "%<br>", "<b>Average 2019 price: R</b>", filtered_data$X2019, "mn")) %>%
              leaflet::addLegend("topleft", colors = brewer.pal(n = 6, name = "RdYlGn"), 
                        values = filtered_data$DReturn,
                      title = "Annual % \nprice change", opacity = 0.7,
                        labFormat = labelFormat(suffix = ""),  
                      labels = c("< -25%", "-25% to -10%", "-10% to 0%",
                       "0% to 10%", "10% to 25%", "> 25%"))
        } else {
### --- if the user wants to see the average house price ver the period, show this map --- ###
          price.bins <- c(0,250*10^3, 500*10^3, 10^6, 2*10^6, 4*10^6, 8*10^6, 10^8)
          price.labels <- c("< R250k", "R250k - R500k", "R500k - R1m",
                           "R1m - R2m", "R2m - R4m", "R4m - R8m", "> R8m")
          
          #filtered_data$AvgPrice <- filtered_data %>% select(!!as.symbol(paste0("AvgX",x1,".",x2)))
          names(filtered_data)[grep("^AvgX", names(filtered_data))] <- "AvgPrice"
          
          pal <- colorBin("RdYlGn", domain = filtered_data$AvgPrice, bins = price.bins)
          leaflet() %>%
            addProviderTiles("Esri.WorldStreetMap") %>%
            setView(lng = 18.5, lat = -34, zoom = 10) %>%
            addPolygons(data = filtered_data, color = "black", weight = 1, fillColor = ~pal(AvgPrice), fillOpacity = 1,
                        popup = paste0("<b>Suburb: </b>", filtered_data$Suburb, "<br>",
                        "<b>Annual price change ", x1, " to ", x2, ":</b> ", as.integer(filtered_data$CReturn), 
                        "%<br>", "<b>Average price from ",x1," to ", x2, ": R</b>", 
                        as.integer(filtered_data$AvgPrice/10^4)/10^2, " mn")) %>%
            leaflet::addLegend("topleft", colors = brewer.pal(n = length(price.labels), name = "RdYlGn"), 
                      values = filtered_data$DReturn,
                      title = "Average price over period", opacity = 0.7,  
                      labels = price.labels)
        }
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
