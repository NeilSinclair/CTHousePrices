# Shiny App that displays average house prices the change in house prices on a map of Cape Town
# The data was scraped using Python, saved as a .csv and then imported into R

# Import the necessary libraries
library(shiny)
library(ggplot2)
library(tidyverse)
library(sf)
library(leaflet)
library(sp)
library("RColorBrewer")

?leafletProxy
# The data has been processed by another script; load it in here
load(file = "mapAppData.Rdata")
combinded_data <- st_as_sf(combined_data)


# Create the Shiny App UI
ui <- fluidPage(

    # Application title
    titlePanel("Cape Town suburb price data"),

    sidebarLayout(
        sidebarPanel(
            # Explaination of what to do 
            helpText("To view data only for specific suburbs, select the 'Selected suburbs'
            radio button and then type the suburb name(s) in the Suburb box. Press 'backspace' to remove a 
            suburb. To view price changes over time, use the slider below to select different time periods."),
            # Radio buttons for selection All suburbs or just specific suburbs to display on the map
            radioButtons("radio", label = h4("Suburb display options"),
                         choices = list("All suburbs" = 1, "Selected suburb(s)" = 2)),
            # Drop down suburb-selection menu
            selectInput("suburb", label = "Suburb", choices = combined_data$Suburb, selected = "",
                        multiple = TRUE),
            radioButtons("priceRadio", label = h4("Map data display options"),
                         choices = list("Annualised % price change" = 1, "Average price over time period" = 2)),
            # Slider to choose which years to see price change info for
            sliderInput("range", label = h3("Time period"), min = 2011, 
                           max = 2019, value = c(2017,2019), sep = "")
            ),
      
        mainPanel(
          helpText("This map visualises the compounded annual changes in average house prices over time or the
                    average house price in the City of Cape Town, depending on
                    which option is selected. Click on suburbs on the map to see the latest price info, the
                   change in price over the time period selected and the suburb name."), 
          leafletOutput("suburbsMap")
           
        )
    )
)

server <- function(input, output) {
    
    output$suburbsMap <- renderLeaflet({
        leaflet() %>% addProviderTiles("Esri.WorldStreetMap") %>%
        setView(lng = 18.5, lat = -34, zoom = 10)

    })
      

    filtered_data <- reactive({
      # Initially, do some preprocessing on the dataset to select the data for the years selected by the user  
      x1 <- input$range[1]
      x2 <- input$range[2]
      # Select the discrete (categorical) and continuous returns
      return_range_D <- paste0("DX",x1,".",x2)
      return_range_C <- paste0("CX",x1,".",x2)
      
      # Select the data just years that are being compared by choosing the pre-calculated
      # column that stores the % return values
      
      filtered_df <- combined_data %>% select(!!as.symbol(return_range_D), !!as.symbol(return_range_C),
                                                Suburb, X2019, !!as.symbol(paste0("AvgX",x1,".",x2)))
      names(filtered_df)[grep("^DX20", names(filtered_df))] <- "DReturn"
      names(filtered_df)[grep("^CX20", names(filtered_df))] <- "CReturn"
      
      # Remove impossible (i.e. incorrect) values (< -1) and multiply by 100 so that it shows up correctly as a %
      filtered_df<- filtered_df %>% dplyr::filter(CReturn > -1)
      filtered_df$CReturn <- filtered_df$CReturn*100
      filtered_df$X2019 <- substr(filtered_df$X2019/10^6, start = 1, stop = 4)
      
      # If the "show all suburbs" button is not selected, then only show the selected suburbs
      if(input$radio == 2){
        filtered_df <- filtered_df %>% filter(Suburb %in% input$suburb) 
      }
      
      #Change the name of the average price column to AvgPrice
      names(filtered_df)[grep("^AvgX", names(filtered_df))] <- "AvgPrice"
      filtered_df <- st_as_sf(filtered_df)
    })

      observe({ 
        x1 <- input$range[1]
        x2 <- input$range[2]
        # If the user wants to see the change in price over time, show the following map
        if(input$priceRadio == 1){

          pal <- colorFactor("RdYlGn", 
                             levels = as.factor(c("< -25%", "-25% to -10%", "-10% to 0%",
                                                  "0% to 10%", "10% to 25%", "> 25%")), ordered = TRUE)
          leafletProxy("suburbsMap", data = filtered_data()) %>% clearShapes() %>% removeControl("legend") %>%
              addPolygons(data = filtered_data(), color = "black", weight = 1, fillColor = ~pal(DReturn), fillOpacity = 1,
                          popup = paste0("<b>Suburb: </b>", filtered_data()$Suburb, "<br>",
                          "<b>Annual price change ", x1, " to ", x2, ":</b> ", as.integer(filtered_data()$CReturn), 
                          "%<br>", "<b>Average 2019 price: R</b>", filtered_data()$X2019, "mn")) %>%
              leaflet::addLegend("topleft", colors = brewer.pal(n = 6, name = "RdYlGn"), 
                        values = filtered_data()$DReturn,
                      title = "Annual % \nprice change", opacity = 0.7,
                        labFormat = labelFormat(suffix = ""),  
                      labels = c("< -25%", "-25% to -10%", "-10% to 0%",
                       "0% to 10%", "10% to 25%", "> 25%"), layerId = "legend")
        } else {
### --- if the user wants to see the average house price ver the period, show this map --- ###
          price.bins <- c(0,250*10^3, 500*10^3, 10^6, 2*10^6, 4*10^6, 8*10^6, 10^8)
          price.labels <- c("< R250k", "R250k - R500k", "R500k - R1m",
                           "R1m - R2m", "R2m - R4m", "R4m - R8m", "> R8m")

          pal <- colorBin("RdYlGn", domain = filtered_data()$AvgPrice, bins = price.bins)
          leafletProxy("suburbsMap") %>% clearShapes() %>% removeControl("legend") %>%
            addPolygons(data = filtered_data(), color = "black", weight = 1, fillColor = ~pal(AvgPrice), fillOpacity = 1,
                        popup = paste0("<b>Suburb: </b>", filtered_data()$Suburb, "<br>",
                        "<b>Annual price change ", x1, " to ", x2, ":</b> ", as.integer(filtered_data()$CReturn),
                        "%<br>", "<b>Average price from ",x1," to ", x2, ": R</b>",
                        as.integer(filtered_data()$AvgPrice/10^4)/10^2, " mn")) %>%
            leaflet::addLegend("topleft", colors = brewer.pal(n = length(price.labels), name = "RdYlGn"),
                      values = filtered_data()$DReturn,
                      title = "Average price over period", opacity = 0.7,
                      labels = price.labels, layerId = "legend")
        }
        
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
