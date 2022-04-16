library(tidyverse)
library(tidyr)
#devtools::install_github("ropensci/plotly")
library(plotly)




#reading in excel files
hs_comp <- read.csv("table_603.10.byAges.csv", header = TRUE)
college_comp <- read.csv("table_603.20.byAges.csv", header = TRUE)


#trimming whitespace after country name
hs_comp$Country <- trimws(hs_comp$Country, which = c("right"))
college_comp$Country <- trimws(hs_comp$Country, which = c("right"))



#transforming data to tibble
hs_comp <- as_tibble(hs_comp)
college_comp <- as_tibble(college_comp)  



#Pivoting Data for better readability


age_range <- c("25_64", "25_34")

hs_comp_pop <- hs_comp[,1:8] %>% filter(Ages %in% age_range) %>% pivot_longer(P.2000:P.2019, 
                    names_to = "year" , names_prefix = "P."  , values_to = "pop_percent_hs") 

hs_comp_se <- hs_comp[, c(1:2, 9:12)] %>% filter(Ages %in% age_range) %>% pivot_longer(SE.2010:SE.2019, 
                            names_to = "year" , names_prefix = "SE.",values_to = "standard_error_hs") 

hs_data <- left_join(hs_comp_pop, hs_comp_se, by = c("Country", "Ages", "year"))



college_comp_pop <- college_comp[, 1:8] %>% filter(Ages %in% age_range) %>% pivot_longer(P.2000:P.2019, 
                         names_to = "year" , names_prefix = "P."  , values_to = "pop_percent_college") 

college_comp_se <-college_comp[, c(1:2, 9:12)] %>% filter(Ages %in% age_range) %>% pivot_longer(SE.2010:SE.2019, 
                    names_to = "year" , names_prefix = "SE.",values_to = "standard_error_college")


college_data <- left_join(college_comp_pop, college_comp_se, by = c("Country", "Ages", "year"))




#combining high school and college data into single data frame
new_data <- full_join(hs_data, college_data)
new_data%>%View()




#replaced NA values for hs population percentage and college to 0 so that
#postsecondary data appears better in plotly visual


new_data$pop_percent_hs[is.na(new_data$pop_percent_hs)] <- 0
new_data$pop_percent_college[is.na(new_data$pop_percent_college)] <- 0


new_data%>% View()


#**********************************************************
#**********************************************************

library(tidyverse)
library(geojsonio)
library(leaflet)
library(maps)
library(sp)
library(rgeos)
library(htmlwidgets)
library(lubridate)
library(rgdal)

#download the world spatial df 
#download.file("http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip"
  #            , destfile="DATA/world_shape_file.zip")

# Unzip this file. You can do it with R (as below), or clicking on the object you downloaded.
#unzip("DATA/world_shape_file.zip")


# Read this shape file with the rgdal library. 
world_spdf <- readOGR( 
  dsn= "DATA/world_shape_file", 
  layer="TM_WORLD_BORDERS_SIMPL-0.3",
  verbose=FALSE
)

# Clean the data object
world_spdf@data$POP2005[ which(world_spdf@data$POP2005 == 0)] = NA
world_spdf@data$POP2005 <- as.numeric(as.character(world_spdf@data$POP2005)) / 1000000 %>% round(2)

#show the world spatial data
#world_spdf@data

#joining the world spatial df with the new_data and select only relevant columns
data.spatial <- sp::merge(world_spdf, new_data, by.x='NAME',by.y='Country',  duplicateGeoms = TRUE)

#setdiff(world_spdf@data$NAME, new_data$Country)

#create a function for drawing the map
map.draw <- function(dat, fill.var, labels, pal, ID){
  dmap <- leaflet(dat) %>%
    setView(lat=25, lng=2 , zoom=2.5) %>%
    addTiles() %>%
    addPolygons(
      fillColor = ~pal(dat@data %>% pull(fill.var)),
      weight = 1, opacity = 1, color = "white",
      dashArray = "3", fillOpacity = 0.9,
      layerId = ~dat@data %>% pull(ID),
      highlight = highlightOptions(
        weight = 5, color = "#666", dashArray = "",
        fillOpacity = 0.9, bringToFront = TRUE),
      label = labels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px", direction = "auto")) %>%
    addLegend(pal = pal, values = ~dat@data %>% 
                pull(fill.var), opacity = 0.7, title = NULL,
              position = "bottomright")
  
}

#create the label
labels_country <- sprintf(
  "<strong>%s</strong><br/>Percentage of highschool completion: %g<br>
        Percentage of postsecondary diploma completion: %g",
  data.spatial$NAME,data.spatial$pop_percent_hs, 
  data.spatial$pop_percent_college) %>% 
  lapply(htmltools::HTML)

#drawing the map
pal.country.quantile <- colorQuantile(palette = "YlOrRd", domain = data.spatial$pop_percent_hs, n=8)
fig<-map.draw(dat = data.spatial, fill.var = 'pop_percent_hs', labels = labels_country, 
                pal = pal.country.quantile, ID = 'NAME')

fig

#save the figure
saveWidget(fig, "fig.html", selfcontained = FALSE)
