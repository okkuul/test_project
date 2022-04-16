library(tidyverse)
library(tidyr)
#devtools::install_github("ropensci/plotly")
library(plotly)
library(geojsonio)
library(leaflet)
library(maps)
library(sp)
library(rgeos)
library(htmlwidgets)
library(lubridate)
library(rgdal)
library(shinydashboard)
library(shiny)

# Prepare data
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


###################################
#### Added and changed parts ######
###################################


#combining high school and college data into single data frame
new_data_all <- full_join(hs_data, college_data)



bar_data <- new_data_all


bar_data$pop_percent_hs[is.na(bar_data$pop_percent_hs)] <- 0
bar_data$pop_percent_college[is.na(bar_data$pop_percent_college)] <- 0


View(bar_data)
str(bar_data)
bar_data$year <- as.factor(bar_data$year)



vis_data <- bar_data[, c(1:4,6)] %>%
  pivot_longer(pop_percent_hs:pop_percent_college, #improved dataset for visuals
               names_to = "percentage_name"  , values_to = "percentages") %>% 
  unite(age_percent, c(Ages, percentage_name))



#attach(vis_data)


country_names <- unique(vis_data$Country)
visuals = list()

for(i in country_names){
  #creating base for visualization
  f <- vis_data %>% filter(Country == i) %>% plot_ly() %>%
    add_bars(x = ~unique(year), y = ~percentages[age_percent == "25_64_pop_percent_hs"],
             name = ~"High School Completion (Ages 25-64)") %>%
    
    add_bars(x = ~unique(year), y = ~percentages[age_percent == "25_64_pop_percent_college"],
             name = ~"Post Secondary Degree (Ages 25-64)") %>%
    
    add_bars(x = ~unique(year), y = ~percentages[age_percent == "25_34_pop_percent_hs"],
             name = ~"High School Completion (Ages 25-34)", color = I("seagreen3")) %>%
    
    add_bars(x = ~unique(year), y = ~percentages[age_percent == "25_34_pop_percent_college"],
             name = ~"Post Secondary Degree (Ages 25-34)", color = I("orchid")) %>% 
    
    add_trace(data = vis_data[1:12,], x = ~unique(year), 
              y = ~percentages[age_percent == "25_64_pop_percent_hs"],
              name = ~"OECD Average Percent for High School Completion",
              type = "scatter", mode = "lines+markers",
              line = list(width = 3, color = "black"), visible = F) %>%
    
    add_trace(data = vis_data[1:12,], x = ~unique(year),
              y = ~percentages[age_percent == "25_64_pop_percent_college"],
              name = ~"OECD Average Percent for Post Secondary Degree",
              type = "scatter", mode = "lines+markers", 
              line = list(width = 3, color = "black", dash = "dash"), visible = F) %>%
    
    add_trace(data = vis_data[481:492,], x = ~unique(year), 
              y = ~percentages[age_percent == "25_34_pop_percent_hs"],
              name = ~"OECD Average Percent for High School Completion",
              type = "scatter", mode = "lines+markers",
              line = list(width = 3, color = "red"), visible = F) %>%
    
    add_trace(data = vis_data[481:492,], x = ~unique(year),
              y = ~percentages[age_percent == "25_34_pop_percent_college"],
              name = ~"OECD Average Percent for Post Secondary Degree",
              type = "scatter", mode = "lines+markers", 
              line = list(width = 3, color = "red", dash = "dash"), visible = F) 
  
  f <- f%>% 
    layout(xaxis = list(title = "year"), 
           yaxis = list(title = "population percentage"),
           title = "% of population that completed High School<br> vs Postsecondary Education by Age Group",
           updatemenus = list(
             list(
               type = "dropdown",
               active = 0,
               x = 1.2,
               xanchor = "right",
               y = 0.60,
               yanchor = "middle",
               buttons = list(
                 list(method =  "restyle", 
                      args = list("visible", list(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE,FALSE, FALSE)),
                      label = "All Age Ranges"),
                 
                 list(method =  "restyle",
                      args = list("visible", list(TRUE, TRUE, FALSE, FALSE, TRUE, TRUE,FALSE, FALSE)),
                      label = "Ages 25 to 64"),
                 
                 list(method = "restyle",
                      args = list("visible", list(FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE)),
                      label = "Ages 25 to 34")
               )
             )
           )
    )
  
  visuals[[i]] = f
}

country_bar <- function(select_country) {
  visuals[[select_country]]
}



###########################################################################
##### Warning: Can't display both discrete & non-discrete data on same axis              
###########################################################################              

###################################
#### Added and changed parts ######
###################################


#only select the latest year for the view
new_data <- new_data_all %>%
  filter(year == 2019)



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




#getting data for 25_34 age group
data.spatial34 <- new_data %>%
  filter(Ages == '25_34')
data.spatial34 <- sp::merge(world_spdf, data.spatial34, 
                              by.x='NAME',by.y='Country',  duplicateGeoms = TRUE)

#getting data for 25_64 age group
data.spatial64 <- new_data %>%
  filter(Ages == '25_64')
data.spatial64 <- sp::merge(world_spdf, data.spatial64, 
                            by.x='NAME',by.y='Country',  duplicateGeoms = TRUE)








#*******************************************************************
#create a function for drawing the map
map.draw <- function(dat, fill.var, labels, pal, ID){
  dmap <- leaflet(dat, width =1500, height = 850 ) %>%
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
  dmap
}

#create the labels
labels_34 <- sprintf(
  "<strong>%s</strong><br/>Percentage of highschool completion: %g<br>
        Percentage of postsecondary diploma completion: %g",
  data.spatial34$NAME,data.spatial34$pop_percent_hs, 
  data.spatial34$pop_percent_college) %>% 
  lapply(htmltools::HTML)

labels_64 <- sprintf(
  "<strong>%s</strong><br/>Percentage of highschool completion: %g<br>
        Percentage of postsecondary diploma completion: %g",
  data.spatial64$NAME,data.spatial64$pop_percent_hs, 
  data.spatial64$pop_percent_college) %>% 
  lapply(htmltools::HTML)


#*******************************************************************






#**************************************************************************
#**************************************************************************

sidebar <- dashboardSidebar(width = '330',
  sidebarMenu(
    menuItem("Highschool Completion Percentage", tabName = "hs",
             startExpanded = TRUE,
             menuSubItem("Age from 25 - 34",
                         tabName = "hs34"),
             menuSubItem("Age from 25 - 64",
                         tabName = "hs64")),
    
    menuItem("Postsecondary degree Completion Percentage",  tabName = "col",
             startExpanded = TRUE,
             menuSubItem("Age from 25 - 34",
                         tabName = "col34"),
             menuSubItem("Age from 25 - 64",
                         tabName = "col64")),
    
    menuItem("Select Country", icon = icon("bar-chart-o", verify_fa = FALSE), tabName = "tabOne",
             selectInput("country", "Country",
                         choices = c("Australia", "Austria", "Belgium", "Canada", "Chile", "Colombia", 
                                     "Czech Republic", "Denmark", "Estonia", "Finland", "France", "Germany", 
                                     "Greece", "Hungary", "Iceland", "Ireland", "Israel", "Italy", "Japan", 
                                     "Korea, Republic of", "Latvia", "Lithuania", "Luxembourg", "Mexico", 
                                     "Netherlands", "New Zealand", "Norway", "Poland", "Portugal", "Slovakia", 
                                     "Slovenia", "Spain", "Sweden", "Switzerland", "Turkey", "United Kingdom", 
                                     "United States", "China", "Russia", "OECD Average"), selectize=TRUE, width = '98%')
    )
  )
)

body <- dashboardBody(
  fluidRow(column(width= 12, box(title = "Bar chart", plotlyOutput("plot", height = 300), collapsible = TRUE))
  ),
  tabItems(
    tabItem(tabName = "hs34",
            h2("Highschool Completion Percentage Age from 25 - 34"),
            map.draw(dat = data.spatial34, fill.var = 'pop_percent_hs', labels = labels_34,
                     pal = colorQuantile(palette = "YlOrRd", domain = data.spatial34$pop_percent_hs, n=8)
                     , ID = 'NAME')
    ),
    tabItem(tabName = "hs64",
            h2("Highschool Completion Percentage Age from 25 - 64"),
            map.draw(dat = data.spatial64, fill.var = 'pop_percent_hs', labels = labels_64,
                     pal = colorQuantile(palette = "Purples", domain = data.spatial64$pop_percent_hs, n=8)
                     , ID = 'NAME')
    ),
    tabItem(tabName = "col34",
            h2("Postsecondary degree Completion Percentage Age from 25 - 34"),
            map.draw(dat = data.spatial34, fill.var = 'pop_percent_college', labels = labels_34,
                     pal = colorQuantile(palette = "Blues", domain = data.spatial34$pop_percent_college, n=8)
                     , ID = 'NAME')
    ),
    tabItem(tabName = "col64",
            h2("Postsecondary degree Completion Percentage Age from 25 - 64"),
            map.draw(dat = data.spatial64, fill.var = 'pop_percent_college', labels = labels_64,
                     pal = colorQuantile(palette = "Greens", domain = data.spatial64$pop_percent_college, n=8)
                     , ID = 'NAME')
    )
  )
            
)

# Put them together into a dashboardPage
ui<-dashboardPage(
  dashboardHeader(title = "NISS competition dashboard", titleWidth = '330'),
  sidebar,
  body
)

server <- function(input, output) {
  output$menuitem <- renderMenu({
    
  })

  output$plot <- renderPlotly({
    country_bar(input$country)
  })
  
}

shinyApp(ui, server)  
  
  
  
#**************************************************************************
#**************************************************************************