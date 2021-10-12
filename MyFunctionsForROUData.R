# used libraries

# general work with tidy libraries 
library(tidyverse)

# for the mapvalues function
library(plyr)

# work with data tables seems to be faster for large datasets
library(data.table)

# to read the json files
library(rjson)

#to read the shape files
library(rgdal)
# to simplify the shape files
library(rmapshaper)

# working with dates
library(lubridate)

# color palettes
library(broom)
library(viridis)
library(ggsci)

# plot
library(plotly)



# defined some colors for two color palettes
cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

cbp2 <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


readRouData <- function(fileName = "date_07_octombrie_la_13_00.json"){
  # read the data from a Json file containing the COVID data from Romania 
  # downloadable from https://datelazi.ro/
  # the data are transformed into a filtered tibble
  raw_json <- rjson::fromJSON(file = fileName)
  x <- raw_json$historicalData %>% Vectorize()
  df <- map_dfr(x, ~as_tibble(t(.)))
  filtered_df <- df %>% 
    mutate(parsed_date = ymd(parsedOnString)) %>% 
    mutate(averageAge = as.numeric(averageAge)) %>% 
    mutate(numberInfected = as.numeric(numberInfected)) %>% 
    mutate(numberCured = as.numeric(numberCured)) %>% 
    mutate(numberDeceased = as.numeric(numberDeceased)) %>% 
    mutate(percentageOfWomen = as.numeric(percentageOfWomen)) %>% 
    mutate(percentageOfMen = as.numeric(percentageOfMen)) %>% 
    mutate(percentageOfChildren = as.numeric(percentageOfChildren)) %>% 
    mutate(numberTotalDosesAdministered = as.numeric(numberTotalDosesAdministered)) %>% 
    mutate(activeCases = numberInfected - numberCured - numberDeceased) %>% 
    select(parsed_date, numberInfected, numberCured, numberDeceased, activeCases, averageAge, percentageOfWomen, percentageOfMen, percentageOfChildren, numberTotalDosesAdministered, incidence, countyInfectionsNumbers, large_cities_incidence, small_cities_incidence) %>%
    #arrange(parsed_date) %>% 
    mutate(infected_per_day = lag(numberInfected)-numberInfected) %>%
    mutate(cured_per_day =lag(numberCured)- numberCured) %>% 
    mutate(deceased_per_day = lag(numberDeceased)-numberDeceased)
  filtered_df<-filtered_df[!duplicated(filtered_df$parsed_date),]
  filtered_df
}

plotTimeData <-function(filtered_df){
  # filter and transform, transform and plot the data from the COVID-19data data.frame
  # the plot contains the active cases divided by 10 displayed as a line
  # infected per day, cured per day and deceased per day as bar/col plot
  coeff <- 10
  p1 <- filtered_df %>% 
    mutate(cured_per_day = -cured_per_day) %>% 
    mutate(deceased_per_day = -deceased_per_day) %>%
    mutate(activeCases_div_10 = activeCases/coeff) %>% 
    select(parsed_date, activeCases_div_10, infected_per_day, cured_per_day, deceased_per_day) %>%
    pivot_longer(cols = - c("parsed_date", "activeCases_div_10"), names_to = "value_name", values_to = "count") %>%
    mutate(value_name = 
             factor(value_name, levels = c("infected_per_day", "cured_per_day", "deceased_per_day", "activeCases"))) %>% 
    ggplot(aes(x = parsed_date))+
    geom_col(aes(y = count, fill = value_name), position = position_stack())+
    geom_line(aes(y = activeCases_div_10), color = cbp2[2], size = 1.5)+
    scale_fill_jco(name = "Legend") +
    scale_y_continuous(
      name = "", # "Infection + Cured + Decesased",
      sec.axis = sec_axis(~.*coeff, name="Active Cases"),
      limits = c(-10000,25000))+
    labs(title = "COVID-19 Romania", 
         x = "Date",
         y="",
         legend.name = "")+
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "none",
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank())
  ggplotly(p1)
}

readRomShape <- function(fileName = "ro_judete_poligon"){
  # read the Romanian county shape file from the folder fileName
  # reduce the resolution of the shape file
  # add the id of the counties
  Ro_spdf <- readOGR( 
      dsn= fileName, 
      layer= fileName,
      verbose=FALSE
    )
  Ro_spdf <- ms_simplify(Ro_spdf,keep = 0.001, keep_shapes = TRUE)
  Ro_spdf <- tidy(Ro_spdf, region = "name")
  conversion <- data.frame(
    id = Ro_spdf %>% distinct(id),
    new_id = c("AB", "AR", "AG", "BC", "BH", "BN", "BT", "BV", "BR", "B", "BZ", "CL", "CS", "CJ", "CT", "CV", "DB", "DJ", "GL", "GR", "GJ", "HR", "HD", "IS", "IL", "IF", "MM", "MH", "MS", "NT", "OT", "PH", "SJ", "SM", "SB", "SV", "TR", "TM", "TL", "VL", "VS", "VN")
  )
  Ro_spdf$id <- mapvalues(Ro_spdf$id, from=conversion$id, to = conversion$new_id)
  Ro_spdf
}

combineRomDataDate <- function(TimeDate = "2021-10-02", Shape, Covid){
  # old version of the function for combine the Romanian Shape with the Romanian Covid data for a specific date
  county_incidence <- Covid %>% 
    filter(parsed_date == TimeDate) %>% 
    select(incidence) %>% 
    unlist() %>% unname()
  
  county_incidence_df <-
    data.frame(
      county = county_short_names,
      infection = county_incidence
    ) %>% 
    mutate(county = factor(county, levels = County_order)) %>% 
    arrange(county)
  
  id_county_incidence <-
    data.frame(
      id = Shape %>% distinct(id),
      county_incidence_df)
  
  map_data <- full_join(Shape,county_incidence_df, by = c("id" = "county"))
}

plotCovidShape <- function(ShapeData){
  # old function for plotting the Covid data on the Romania map for a specific date
  p2 <- ggplot() +
    geom_polygon(data = ShapeData, aes( x = long, y = lat, group = group, fill = infection), color="white") +
    scale_fill_fermenter(n.breaks = 9, palette = "RdYlGn", limits = c(0,10))+
    theme_minimal()+
    theme(plot.title = element_text(hjust = 0.5),
          #legend.position = "none",
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.y=element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank())
  ggplotly(p2)
}

new_group_Map_Covid <- function(covid_df, map_shape){
  # new function that combines all the data for Covid in Romania with the Romanian shape file
  covidData <- as.data.table(covid_df %>% 
                      select(parsed_date, incidence) %>% 
                      unnest_wider(incidence) %>% 
                      pivot_longer(cols = - c("parsed_date"), names_to = "id", values_to = "incidence") %>% 
                      pivot_wider(names_from = 'parsed_date', values_from = 'incidence'))
  covidData <- melt(covidData,
              id.vars = 'id',
              variable.name = 'Date',
              value.name = "incidence")
  
  mapData <- as.data.table(map_shape)
  mapCovidData <- left_join(mapData, covidData, by = c('id'='id')) %>% 
    mutate(Date = ymd(Date)) %>% 
    arrange(Date)
  mapCovidData <- drop_na(mapCovidData, 'incidence')
}

plot_new_Covid_Shape <- function(ShapeData, plot_date){
  # new function that plot the Covid data from a specific date on the Romania map
  p <- ShapeData %>% filter(Date == ymd(plot_date)) %>% 
    ggplot() +
    geom_polygon(aes(x = long, y = lat, group = group, fill = incidence), color="white") +
    scale_fill_fermenter(n.breaks = 9, palette = "RdYlGn", limits = c(0,10))+
    ggtitle('COVID19 - cases in Romania',
            subtitle = 'Date: {plot_date}')+
    theme_minimal()+
    theme(plot.title = element_text(hjust = 0.5),
          #legend.position = "none",
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.y=element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank())
  ggplotly(p)
}
  
#CovidData <- readRouData()
#plotTimeData(CovidData)
#RomShape <- readRomShape()
#CovidShape <- combineRomDataDate("2021-10-02", RomShape, CovidData)
#plotCovidShape(CovidShape)
#plotCovidShape(combineRomDataDate("2021-10-02", readRomShape(), readRouData()))
#dateT <- new_group_Map_Covid(CovidData,RomShape)

#covid_df <- CovidData

#map_shape<-RomShape
#covidData <- as.data.table(CovidData %>% 
#                             select(parsed_date, incidence) %>% 
#                             unnest_wider(incidence) %>% 
#                             pivot_longer(cols = - c("parsed_date"), names_to = "id", values_to = "incidence") %>% 
#                             pivot_wider(names_from = 'parsed_date', values_from = 'incidence'))
#mapData <- as.data.table(RomShape)

#covid_df<-CovidData
#map_shape<-RomShape

#covidData <- as.data.table(covid_df %>% 
#                             select(parsed_date, incidence) %>% 
#                             unnest_wider(incidence) %>% 
#                             pivot_longer(cols = - c("parsed_date"), names_to = "id", values_to = "incidence") %>% 
#                             pivot_wider(names_from = 'parsed_date', values_from = 'incidence'))
#covidData <- melt(covidData,
#                  id.vars = 'id',
#                  variable.name = 'Date',
#                  value.name = "incidence")
#mapData <- as.data.table(map_shape)
#mapCovidData <- left_join(mapData, covidData, by = c('id'='id')) %>% mutate(Date = ymd(Date)) %>% arrange(Date) %>% drop_na()

