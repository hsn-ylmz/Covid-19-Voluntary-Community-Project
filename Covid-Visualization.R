
#Initializing necessary libraries
library(dplyr)
library(ggplot2)

setwd('D:/Covid-19 Visualization/') ## Setting Working Directory##

## Open covid-19 data from csv file##
## It contains both uncornfirmed and confirmed cases ## 
covid.raw <- read.csv(file = 'coronavirus_dataset.csv',
                        header = TRUE, sep = ',')

## Filter for getting for only confirmed cases ##
confirmed_case <<- covid.raw %>% filter(type == 'confirmed')




cumulative_plot <- function(){
  ## Cumulative cases plot to data includes cases from all over the world ##
  
  confirmed_case %>%
    
    ## Group by date to get date-cumulative cases plot##
    group_by(date) %>%
    ## Counts cases for every date input##
    count(cases,date) %>%
    ## Summarise the cases in every date counts##
    summarise(sums = sum(cases)) %>%
    ## Calculate cumulative counts of cases in 
    ## for every day and relocate it new column named as cumulative ##
    mutate(cumulative = cumsum(sums)) %>% 
    ##Select just data and cumulative colums of data ##
    select(date, cumulative) %>%
    #Create ggplot object, x-axis as date, y-axis as cumulative cases##
    ggplot(aes(date, cumulative))+
    geom_line(group = 1, size = 1.2, color= 'Red')+
    theme(axis.text.x = element_text(angle = 70, hjust = 1))+
    ggtitle('Epidemia to Pandemia (Covid-19)')+
    xlab('Date (year-month-day)') +
    ylab('Cumulative Confirmed Cases') +
    ggsave("cumulative_cases.png", width = 15, height = 5)
}



china_vs_world_plot <- function(){
  ## Plot cumulative cases in china vs other countries in all over the world ##
  
  confirmed_case %>%
    
    ## Creates two groups as China and Others and store them in country row ##
    mutate(country = ifelse(Country.Region == 'China', 'China', 'Others')) %>%
    ## Take this two distinctive groups and 
    ## group by country and date in between eachothers ##
    group_by(country, date) %>%
    ##Count cases and calculate cumulative cases..
    count(cases, date) %>%
    summarise(sums = sum(cases)) %>%
    mutate(cumulative = cumsum(sums)) %>% ##..##
    #Create ggplot object, x-axis as date, y-axis as cumulative cases##
    ggplot() +
    geom_line(aes(date, cumulative, color = country, group = country), size = 1.5) +
    theme(axis.text.x = element_text(angle = 70, hjust = 1))+
    ggtitle('China vs World')+
    xlab('Date (year-month-day)') +
    ylab('Cumulative Confirmed Cases')+
    ggsave("china_vs_world.png", width = 15, height = 5)
}



china_vs_world_events <- function(){
  ## Cumulative cases plot China vs World with WHO events in table ##
  
  ##Get China vs World plot##
  plot_chi_vs_world <- china_vs_world_plot()
  ##Create evets table##
  who_events <- tribble(
    ~ ddate, ~ event,
    "2020-01-30", "Global health\nemergency declared",
    "2020-03-11", "Pandemic\ndeclared",
    "2020-02-13", "China reporting\nchange"
  )
  ##Put that events at exact dates in graph ##
  plot_chi_vs_world +
    geom_vline(aes(xintercept = ddate), data = who_events, linetype = 'dashed') +
    scale_y_log10()+
    ylab('Cumulative Confirmed Cases in Log10 Scale')+
    ## Modify text location on x and y axis'##
    geom_text(aes(x = ddate, label = event),
              data = who_events, y = log10(1e06))+
    ggsave("WHO_events.png", width = 15, height = 5)
}



after_feb15_china <- function(){
  ## Case plot in china after February 15th ##
  
  ## Creates two groups as China and Others and store them in country row ##
  china_after_feb15 <- confirmed_case %>%
    mutate(country = ifelse(Country.Region == 'China', 'China', 'Others')) %>%
    group_by(country, date) %>%
    count(cases, date) %>%
    summarise(sums = sum(cases)) %>%
    mutate(cumulative = cumsum(sums)) %>%
    ## Filter it import just country == China data ##
    filter(as.Date(date) > '2020-02-15' & country == 'China')
  china_after_feb15$date <- as.Date(china_after_feb15$date)
  
  china_after_feb15 %>% ggplot(aes(date, cumulative)) +
    geom_line(group = 1, size = 1.2, color= 'Red')+
    theme(axis.text.x = element_text(angle = 70, hjust = 1))+
    ## Add a line wrt Linear Model ##
    geom_smooth(method = "lm", se = FALSE)+
    ggtitle('China after February 15th')+
    xlab('Date') +
    ylab("Cumulative Confirmed Cases")+
    ggsave("china_feb15.png", width = 10, height = 5)
  
}



wo_china <- function(){
  ## Get Data from China vs World section and Filter it for Others ##
  plot_chi_vs_world <- china_vs_world_plot()
  not_china <- plot_chi_vs_world$data %>% filter(country == 'Others')
  not_china$date <- as.Date(not_china$date)
  
  
  plt_not_china <- not_china %>% ggplot(aes(date, cumulative)) +
    geom_line(group = 1, size = 1.2, color= 'Red')+
    theme(axis.text.x = element_text(angle = 70, hjust = 1))+
    geom_smooth(method = "lm", se = FALSE)+
    ggtitle('World Cases without China')+
    xlab('Date')+
    ylab("Cumulative Confirmed Cases")
  plt_not_china
  ggsave("wo_china.png", width = 15, height = 5)
}



wo_china_log10 <- function(){
  plot_chi_vs_world <- china_vs_world_plot()
  not_china <- plot_chi_vs_world$data %>% filter(country == 'Others')
  not_china$date <- as.Date(not_china$date)
  
  
  plt_not_china <- not_china %>% ggplot(aes(date, cumulative)) +
    geom_line(group = 1, size = 1.2, color= 'Red')+
    theme(axis.text.x = element_text(angle = 70, hjust = 1))+
    geom_smooth(method = "lm", se = FALSE)+
    ggtitle('World Cases without China')+
    xlab('Date')+
    ylab("Cumulative Confirmed Cases in log10 Scale")+
    scale_y_log10()
  plt_not_china
  ggsave("wo_china_log10.png", width = 15, height = 5)
}




top_7 <- function(){
  top_countries_by_total_cases <- confirmed_case %>%
    rename(country = Country.Region) %>%
    group_by(country, date) %>%
    count(cases, date) %>%
    summarise(sums = sum(cases)) %>%
    mutate(cumulative = cumsum(sums))
  top_countries_by_total_cases <- top_countries_by_total_cases %>%
    group_by(country) %>%
    summarise(total_cases = max(cumulative)) %>%
    top_n(7, total_cases) %>%
    arrange(desc(total_cases))
  
  top_countries_by_total_cases
  write.table(top_countries_by_total_cases, 'Top 7 Countries.csv', 
              quote= F, sep = ',')
}





top_7_wo_china <- function(){
  top_countries_outside_china <- confirmed_case %>%
    rename(country = Country.Region) %>%
    filter(country != 'China') %>%
    group_by(country, date) %>%
    count(cases, date) %>%
    summarise(sums = sum(cases)) %>%
    mutate(cumulative = cumsum(sums))
  summary_outside <- top_countries_outside_china %>%
    group_by(country) %>%
    summarise(total = max(cumulative)) %>%
    top_n(7, total)
  top_countries_outside_china %>%
    filter(country == summary_outside$country) %>%
    ggplot(aes(date, cumulative, group = country, color = country))+
    geom_line(size = 1.2, check_overlap = T) +
    theme(axis.text.x = element_text(angle = 70, hjust = 1))+
    ggtitle('Top 7 Countries without China')+
    xlab('Date')+
    ylab('Cumulative Confirmed Cases')
  ggsave("top_7_wo_china.png", width = 15, height = 5)
}

