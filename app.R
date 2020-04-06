# Shiny App that displays average house prices the change in house prices on a map of Cape Town
# The data was scraped using Python, saved as a .csv and then imported into R

# Import the necessary libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(tidyverse)
library(sf)
library(leaflet)
library(sp)
library("RColorBrewer")


# The data has been processed by another script; load it in here
#setwd("C:/Users/User/Documents/Python Scripts/Maps - Final")

load(file = "./data/mapAppData.Rdata")
combinded_data <- st_as_sf(combined_data)
#view(combined_data[1:10,])

# Create the Shiny App UI
ui <- dashboardPage(

    # Application title
    dashboardHeader(title="Cape Town Suburb Price Data", titleWidth = 350),
    
    dashboardSidebar(disable = TRUE),
    dashboardBody(
      fluidRow(
        column(5,
            # Explaination of what to do 
            helpText(HTML("<h4>Area selection overview</h4>
            <ul><li>View average prices/price changes for all suburbs, suburbs by area or a selection 
            of suburbs by ticking the appropriate radio button (Area display options) and searching in 
            the appropriate search box (Area or Suburb)</li>
            <li>To remove an item in the search bar, click on it and press backspace or delete </li>
            <li>Use the slider below to select different time periods for the data in the map</li></ul>")),
            # Radio buttons for selection All suburbs or just specific suburbs to display on the map
            radioButtons("radio", label = h4("Area display options"),
                         choices = list("All suburbs" = 1, "Selected area(s)" = 2, 
                                        "Selected suburb(s)" = 3), selected = 2, inline = TRUE),
            # Drop down area-selection menu
            selectInput("area", label = "Area", choices = combined_data$SUB_CNCL_NAME, 
                        selected = c("Southern", "Atlantic, CBD and Surrounds"),
                        multiple = TRUE, width = 600),
            # Drop down suburb-selection menu
            selectInput("suburb", label = "Suburb", choices = combined_data$Suburb, 
                        selected = c("Pinelands","Woodstock","Mowbray","Salt River", "Vredehoek",
                                     "Oranjezicht", "Thornton", "Athlone", "Observatory", "Gardens"),
                        multiple = TRUE, width = 600),
            radioButtons("priceRadio", label = h4("Map data display options"),
                         choices = list("Annualised % price change" = 1, 
                                        "Average price over time period" = 2), inline = TRUE),
            # Slider to choose which years to see price change info for
            sliderInput("range", label = h4("Time period"), min = 2011, 
                           max = 2019, value = c(2017,2019), sep = "", width = 600)
            ),
      
        column(7,
          helpText("This map visualises the % change in house prices or the average prices over time in the 
          suburbs in and around Cape Town, depending on which option is selected. 
                    Click inside the suburbs on the map to see the suburb name along with the
                    average price and the change in price over the time period selected."), 
          leafletOutput("suburbsMap")
           
        )
    )
    )
)

server <- function(input, output) {
    # Render the map and then render new info on top of this (later in server) to reduce loading times
    output$suburbsMap <- renderLeaflet({
        leaflet(options = leafletOptions(attributionControl=FALSE)) %>% 
        addProviderTiles("Esri.WorldStreetMap") %>%
        setView(lng = 18.5, lat = -33.95, zoom = 11)

    })
      

    filtered_data <- reactive({
      # Initially, do some preprocessing on the dataset to select the data for the years selected by the user  
      x1 <- input$range[1]
      x2 <- input$range[2]
      # Select the discrete (categorical) and continuous returns

      return_range_C <- paste0("CX",x1,".",x2)
      
      # Select the data just years that are being compared by choosing the pre-calculated
      # column that stores the % return values
      
      filtered_df <- combined_data %>% select(!!as.symbol(return_range_C),
                                                Suburb, !!as.symbol(paste0("AvgX",x1,".",x2)),
                                              SUB_CNCL_NAME)

      names(filtered_df)[grep("^CX20", names(filtered_df))] <- "CReturn"
      
      # Remove impossible (i.e. incorrect) values (< -1) and multiply by 100 so that it shows up correctly as a %
      filtered_df<- filtered_df %>% dplyr::filter(CReturn > -1)
      filtered_df$CReturn <- filtered_df$CReturn*100
   
      
      # If the "show all suburbs" button is not selected, then only show the selected suburbs
      if(input$radio == 2){
        filtered_df <- filtered_df %>% filter(SUB_CNCL_NAME %in% input$area) 
      } else if(input$radio == 3){
        filtered_df <- filtered_df %>% filter(Suburb %in% input$suburb) 
      }
      
      #Change the name of the average price column to AvgPrice
      names(filtered_df)[grep("^AvgX", names(filtered_df))] <- "AvgPrice"
      filtered_df <- st_as_sf(filtered_df)
    })

      observe({ 
        x1 <- input$range[1]
        x2 <- input$range[2]
        legend_title <- paste0("Annualised % price <br> change: ",x1, " to ", x2)
        # If the user wants to see the change in price over time, show the following map
        if(input$priceRadio == 1){
          percent.bins <- c(-100,-25,-10,0,10,25,300)
          percent.labels <- c("< -25%", "-25% to -10%", "-10% to 0%",
                               "0% to 10%", "10% to 25%", "> 25%")
          
          pal <- colorBin("RdYlGn", domain = filtered_data()$CReturn, bins = percent.bins)
          leafletProxy("suburbsMap", data = filtered_data()) %>% clearShapes() %>% removeControl("legend") %>%
              addPolygons(data = filtered_data(), color = "black", weight = 1, fillColor = ~pal(CReturn), fillOpacity = 1,
                          popup = paste0("<b>Suburb: </b>", filtered_data()$Suburb, "<br>",
                          "<b>Annual price change ", x1, " to ", x2, ":</b> ", as.integer(filtered_data()$CReturn), 
                          "%<br>", "<b>Average price from ",x1," to ", x2, ": R</b>",
                          as.integer(filtered_data()$AvgPrice/10^4)/10^2, " mn")) %>%
              leaflet::addLegend("bottomleft", colors = brewer.pal(n = length(percent.labels), name = "RdYlGn"), 
                        values = filtered_data()$CReturn,
                        title = HTML(legend_title), opacity = 0.7,
                        labels = percent.labels, layerId = "legend")
        } else {
### --- if the user wants to see the average house price ver the period, show this map --- ###
          price.bins <- c(0,250*10^3, 500*10^3, 10^6, 2*10^6, 4*10^6, 8*10^6, 10^8)
          price.labels <- c("< R250k", "R250k - R500k", "R500k - R1m",
                           "R1m - R2m", "R2m - R4m", "R4m - R8m", "> R8m")
          # Create a dynamic legend title
          legend_title <- paste0("Average price between: <br> ",x1, " and ", x2)
          
          # Plot the map
          pal <- colorBin("RdYlGn", domain = filtered_data()$AvgPrice, bins = price.bins)
          leafletProxy("suburbsMap") %>% clearShapes() %>% removeControl("legend") %>%
            addPolygons(data = filtered_data(), color = "black", weight = 1, fillColor = ~pal(AvgPrice), fillOpacity = 1,
                        popup = paste0("<b>Suburb: </b>", filtered_data()$Suburb, "<br>",
                        "<b>Annual price change ", x1, " to ", x2, ":</b> ", as.integer(filtered_data()$CReturn),
                        "%<br>", "<b>Average price from ",x1," to ", x2, ": R</b>",
                        as.integer(filtered_data()$AvgPrice/10^4)/10^2, " mn")) %>%
            leaflet::addLegend("bottomleft", colors = brewer.pal(n = length(price.labels), name = "RdYlGn"),
                      values = filtered_data()$AvgPrice,
                      title = HTML(legend_title), opacity = 0.7,
                      labels = price.labels, layerId = "legend")
        }
        
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
