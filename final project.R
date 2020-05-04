library(tidyverse)
library(rvest)
library(choroplethr)
library(choroplethrMaps)
library(gridExtra)

corona <- "https://www.worldometers.info/coronavirus/country/us/" %>%read_html()%>%html_nodes("table") %>% html_table(fill = TRUE) %>% .[1] %>% tibble() %>% .[1]

corona <- unnest(corona)
corona$TotalCases <- str_replace_all(corona$TotalCases, ",", "")
corona$TotalDeaths <- str_replace_all(corona$TotalDeaths, ",", "")
corona$`Deaths/1M pop` <- str_replace_all(corona$`Deaths/1M pop`, ",", "")

corona$TotalCases <- as.integer(corona$TotalCases)
corona$TotalDeaths <- as.integer(corona$TotalDeaths)
corona$`Deaths/1M pop` <- as.integer(corona$`Deaths/1M pop`)


corona <- corona %>% .[-1,] %>% .[-(52:63),]
#### create table to keep only state names in a tabele ####


corona$USAState <- tolower(corona$USAState)
state_table <- corona$USAState %>% tibble()
#### Create death rate variable
corona <- corona %>% mutate(death_rate = (TotalDeaths/TotalCases))

####   #####  ####


state_choropleth <- function (df, title = "", legend = "", num_colors = 7, 
                              zoom = NULL, reference_map = FALSE) 
{
  c = StateChoropleth$new(df)
  c$title = title
  c$legend = legend
  c$set_num_colors(num_colors)
  c$set_zoom(zoom)
  c$show_labels = F
  if (reference_map) {
    if (is.null(zoom)) {
      stop("Reference maps do not currently work with maps that have insets, such as maps of the 50 US States.")
    }
    c$render_with_reference_map()
  }
  else {
    c$render()
  }
}




#### Death Rate Map
death_rate <- corona %>% select(USAState, death_rate)
death_rate <- death_rate %>% rename(region = USAState, value = death_rate)

death_map <- state_choropleth(death_rate,title  = "Coronavirus Death Rate by US State",legend = "Death Rate", num_colors = 9)


#### Total Cases Map
total_cases <- corona[,1:2]

total_cases <- total_cases %>% rename(region = USAState, value = TotalCases)

total_cases$region <- tolower(total_cases$region)

cases_map <- state_choropleth(total_cases,title  = "Total Coronavirus Cases by US State",legend = "Cases", num_colors = 9)








#### Obesity rate and map

obesity <- "https://en.wikipedia.org/wiki/Obesity_in_the_United_States" %>%read_html()%>%html_nodes("table") %>% html_table(fill = TRUE) 

obesity <- obesity[1] %>% tibble()
obesity <- unnest(obesity)
obesity <- obesity %>% select(`States, district, & territories`, `Obese adults (2020)[75][70][76]`)
obesity <- obesity %>% rename(State = `States, district, & territories`, Obesity_rate = `Obese adults (2020)[75][70][76]`) 

### still inlcudes the notes for territories but doesnt matter because i am removing them anyway
obesity$Obesity_rate <- str_replace_all(obesity$Obesity_rate, "[^0-9|.]", "")
obesity$Obesity_rate <- as.double(obesity$Obesity_rate)


obesity$State <- tolower(obesity$State)
obesity <- obesity %>% inner_join(state_table, by= c("State" = "."))



obesity_map <- obesity %>% rename(region = State, value = Obesity_rate)

obesity_map <- state_choropleth(obesity_map,title  = "Adult Obesity Rate by US State",legend = "Obesity Rate", num_colors = 9)




#### Smoking rate and map

smoking <- "https://worldpopulationreview.com/states/smoking-rates-by-state/" %>%read_html()%>%html_nodes("table") %>% html_table(fill = TRUE) %>% tibble() %>% unnest()

smoking <- smoking %>% rename(smoking_rate = `Number of Smoking Adults`) %>% select(State,smoking_rate)
smoking$smoking_rate <- str_replace_all(smoking$smoking_rate, "[^0-9|.]", "")
smoking$smoking_rate <- as.double(smoking$smoking_rate)
smoking$State <- tolower(smoking$State)
smoking$smoking_rate <- as.double(smoking$smoking_rate)

smoking_map <- smoking %>% rename(region = State, value = smoking_rate)

smoking_map <- state_choropleth(smoking_map,title  = "Adult Smoking Rate by US State",legend = "Smoking Rate", num_colors = 9)






#### Healthcare ranking





healthcare <- "https://wallethub.com/edu/states-with-best-health-care/23457/" %>%read_html()%>%html_nodes("table") %>% html_table(fill = TRUE) %>% tibble() %>% unnest()

healthcare <- healthcare %>% select(State, `Total Score`) %>% rename(health_score = `Total Score`)
healthcare$State <- tolower(healthcare$State)



health_map <- healthcare %>% rename(region = State, value = health_score)

health_map <- state_choropleth(health_map,title  = "Healthcare Score by US State",legend = "Score", num_colors = 9)






#### Public health

public_health <- "https://www.usnews.com/news/best-states/rankings/health-care/public-health" %>%read_html()%>%html_nodes("table") %>% html_table(fill = TRUE)

public_health <- public_health[53] %>% tibble() %>% unnest()

public_health<- public_health %>% select(State,`Mental Health`,`Low Mortality Rate`,`Low Smoking Rate`,`Low Infant Mortality Rate`,`Low Obesity Rate`,`Low Suicide Rate`) %>% .[-1,]


public_health <- arrange(public_health, State) %>% .[-1,]
state_table1 <- arrange(state_table, .) %>% .[-9,]
public_health$State = state_table1$.


public_health <- public_health %>% mutate(average = ((`Mental Health`+`Low Mortality Rate`+`Low Smoking Rate`+`Low Infant Mortality Rate`+`Low Obesity Rate`+`Low Suicide Rate`)/6))

pubhealth_map <- public_health %>% select(State, average) %>% rename(region = State, value = average)
pubhealth_map$region <- tolower(pubhealth_map$region)

pubhealth_map <- state_choropleth(pubhealth_map,title  = "Public Health Score by US State",legend = "Score", num_colors = 9)



### Variables not significant for death rate. Likely because covid is effecting areas with dense population

final_table <- corona %>% select(USAState, death_rate) %>% inner_join(obesity, by= c("USAState" = "State")) %>% inner_join(smoking,  by= c("USAState" = "State")) %>% inner_join(healthcare, by = c("USAState" = "State")) %>% inner_join(public_health, by = c("USAState" = "State"))







grid.arrange(death_map + theme(legend.position = "bottom"), cases_map  + theme(legend.position = "bottom")+scale_fill_brewer(type='seq', palette=2), obesity_map + theme(legend.position = "bottom")+scale_fill_brewer(type='seq', palette=3), smoking_map + theme(legend.position = "bottom")+scale_fill_brewer(type='seq', palette=4), health_map + theme(legend.position = "bottom")+scale_fill_brewer(type='seq', palette=7), pubhealth_map + theme(legend.position = "bottom")+scale_fill_brewer(type='seq', palette=6), ncol=2)









ny <- "https://en.wikipedia.org/wiki/2020_coronavirus_pandemic_in_New_York_(state)" %>% read_html()%>%html_nodes("table") %>% html_table(fill = TRUE)

ny_county <- ny[6] %>% tibble() 

ny_county <- ny_county$.[[1]] %>% as.tibble(.name_repair = "unique") 
ny_county <- ny_county[,1:3]
ny_county <- ny_county %>% rename(County = `County [a]`, Cases = `Cases [b][c]`, Deaths = `Deaths [c]`)
ny_county <- ny_county[-1,]


ny_county$County <- str_replace_all(ny_county$County, "[^A-z]|[[:punct:]]", "") 


ny_county$Cases <- str_replace_all(ny_county$Cases, ",", "")
ny_county$Deaths <- str_replace_all(ny_county$Deaths, ",", "")


ny_county$Cases <- as.integer(ny_county$Cases)
ny_county$Deaths <- as.integer(ny_county$Deaths)




ny_county <- ny_county %>% mutate(death_rate = (Deaths/Cases))

ny_county[28,1] = "New York"



dense <- "https://en.wikipedia.org/wiki/List_of_counties_in_New_York" %>% read_html()%>%html_nodes("table") %>% html_table(fill = TRUE)
dense <- dense[3] %>% tibble() %>% unnest %>% select(County, Density)

dense$County <- str_replace_all(dense$County, " County", "")

ny_table <- ny_county %>% inner_join(dense, by = "County")
ny_table$Density <- str_replace_all(ny_table$Density, ",", "")
ny_table$Density <- as.double(ny_table$Density)
ny_table$County <- tolower(ny_table$County)







data(county.regions)
ny_map <- county.regions %>% filter(state.name == "new york") %>% select(region, county.name) %>% inner_join(ny_table, by = c("county.name"="County"))







#### Adding NJ

nj <- "https://en.wikipedia.org/wiki/2020_coronavirus_pandemic_in_New_Jersey" %>% read_html()%>%html_nodes("table") %>% html_table(fill = TRUE)

nj_county <- nj[3]%>% as.data.frame() %>% as.tibble() %>% .[,1:3] %>% rename(County = `County..a.`, Cases = `Cases..b..c.`, Deaths = `Deaths..c.`) %>% .[-1,]



nj_county$Cases <- str_replace_all(nj_county$Cases, ",", "")
nj_county$Deaths <- str_replace_all(nj_county$Deaths, ",", "")

nj_county$Cases <- as.integer(nj_county$Cases)
nj_county$Deaths <- as.integer(nj_county$Deaths)


nj_county <- nj_county %>% mutate(death_rate = (Deaths/Cases))



nj_dense <- "https://en.wikipedia.org/wiki/List_of_counties_in_New_Jersey" %>%  read_html()%>%html_nodes("table") %>% html_table(fill = TRUE)
nj_dense <- nj_dense[2] %>% tibble() %>% unnest() %>% select(County, `Density (per mi²)`) %>% rename(Density = `Density (per mi²)`)

nj_dense$Density <- str_replace_all(nj_dense$Density, ",", "")
nj_dense$Density <- as.double(nj_dense$Density)

nj_dense$County <- str_replace_all(nj_dense$County, " County", "")


nj_table <- nj_county %>% inner_join(nj_dense, by = "County")
nj_table$County <- tolower(nj_table$County)


nj_map <- county.regions %>% filter(state.name == "new jersey") %>% select(region, county.name) %>% inner_join(nj_table, by = c("county.name"="County"))


####Joining both states

ny_nj <- rbind(ny_map, nj_map)





#Density

map_density <- ny_nj %>% select(region, Density) %>% rename(value = Density)


county_density <- county_choropleth(map_density,title  = "Population Density by New York and New Jersey County",legend = "Density", state_zoom = c("new york", "new jersey"), num_colors = 9)

#Death rate

map_rate <- ny_nj %>% select(region, death_rate) %>% rename(value = death_rate)


county_death <- county_choropleth(map_rate,title  = "Death Rate by New York and New Jersey County",legend = "Death Rate", state_zoom = c("new york", "new jersey"), num_colors = 9)

#Cases

map_cases <- ny_nj %>% select(region, Cases) %>% rename(value = Cases)


county_cases <- county_choropleth(map_cases,title  = "Total Cases by New York and New Jersey County",legend = "Cases", state_zoom = c("new york", "new jersey"), num_colors = 9)



grid.arrange(county_death, county_cases, county_density)




