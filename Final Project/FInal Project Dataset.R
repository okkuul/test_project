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