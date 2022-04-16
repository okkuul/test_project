library(tidyverse)
library(tidyr)
#install.packages("plotly")
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
#postsecondary data appears better in the visual


new_data$pop_percent_hs[is.na(new_data$pop_percent_hs)] <- 0
new_data$pop_percent_college[is.na(new_data$pop_percent_college)] <- 0




###############################CREATING VISUALS#################################

vis_data <- new_data[, c(1:4,6)] %>%pivot_longer(pop_percent_hs:pop_percent_college, #improved dataset for visuals
names_to = "percentage_name"  , values_to = "percentages") %>% unite(age_percent, c(Ages, percentage_name))

attach(vis_data)


country_names <- unique(new_data$Country)
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
             name = ~"Post Secondary Degree (Ages 25-34)", color = I("orchid"))%>%
    
    add_trace(data = vis_data[1:12,], x = ~unique(year), 
              y = ~percentages[age_percent == "25_64_pop_percent_hs"],
              name = ~"OECD Average Percent for High School Completion",
              type = "scatter", mode = "lines+markers",
              line = list(width = 3, color = "black"), visible = F)%>%
    
    add_trace(data = vis_data[1:12,], x = ~unique(year),
              y = ~percentages[age_percent == "25_64_pop_percent_college"],
              name = ~"OECD Average Percent for Post Secondary Degree",
              type = "scatter", mode = "lines+markers", 
              line = list(width = 3, color = "black", dash = "dash"), visible = F)%>%
    
    add_trace(data = vis_data[481:492,], x = ~unique(year), 
              y = ~percentages[age_percent == "25_34_pop_percent_hs"],
              name = ~"OECD Average Percent for High School Completion",
              type = "scatter", mode = "lines+markers",
              line = list(width = 3, color = "red"), visible = F)%>%
    
    add_trace(data = vis_data[481:492,], x = ~unique(year),
              y = ~percentages[age_percent == "25_34_pop_percent_college"],
              name = ~"OECD Average Percent for Post Secondary Degree",
              type = "scatter", mode = "lines+markers", 
              line = list(width = 3, color = "red", dash = "dash"), visible = F)
  
  
  
  
  #adjusting layouts based on dropdown selection
  
  f <- f %>% layout(xaxis = list(title = "year"),
                    yaxis = list(title = "population percentage"),
                    title = "% of population that completed High School vs Postsecondary Education by Age Group",
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

visuals$Chile #visual output


#Fin :)







