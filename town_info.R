### Generate features based on town information from wikipedia
###

rm(list = ls())

library(rvest)
library(dplyr)

# load the dataset
dataset <- get(load("/Users/victorjs/desktop/insight/Boston.RData"))
temp <- revalue(dataset$Town, c('Foxboro'      = 'Foxborough',
                               'South-Easton'  = 'Easton',
                               'North-Andover' = 'North Andover',
                               'Chestnut-Hill' = 'Chestnut Hill',
                               'North-Reading' = 'North Reading',
                               'West-Roxbury'  = 'West Roxbury',
                               'Tyngsboro'     = 'Tyngsborough', 
                               'Hyde-Park'     = 'Hyde Park'))
dataset$Town <- temp

# scrape the town information from wikipedia
middlesex <- 'https://en.wikipedia.org/wiki/Middlesex_County,_Massachusetts' %>% 
    read_html() %>%
    html_node(xpath = '//*[@id="mw-content-text"]/div/table[4]') %>%
    html_table()


norfolk <- 'https://en.wikipedia.org/wiki/Norfolk_County,_Massachusetts' %>% 
    read_html() %>%
    html_node(xpath = '//*[@id="mw-content-text"]/div/table[4]') %>%
    html_table()

suffolk <- 'https://en.wikipedia.org/wiki/Suffolk_County,_Massachusetts' %>% 
    read_html() %>%
    html_node(xpath = '//*[@id="mw-content-text"]/div/table[5]') %>%
    html_table()
suffolk = suffolk[, -4]

essex <- 'https://en.wikipedia.org/wiki/Essex_County,_Massachusetts' %>% 
    read_html() %>%
    html_node(xpath = '//*[@id="mw-content-text"]/div/table[4]') %>%
    html_table()

bristol <- 'https://en.wikipedia.org/wiki/Bristol_County,_Massachusetts' %>% 
    read_html() %>%
    html_node(xpath = '//*[@id="mw-content-text"]/div/table[3]') %>%
    html_table()

plymouth <- 'https://en.wikipedia.org/wiki/Plymouth_County,_Massachusetts' %>% 
    read_html() %>%
    html_node(xpath = '//*[@id="mw-content-text"]/div/table[3]') %>%
    html_table()

worcester <- 'https://en.wikipedia.org/wiki/Worcester_County,_Massachusetts' %>% 
    read_html() %>%
    html_node(xpath = '//*[@id="mw-content-text"]/div/table[3]') %>%
    html_table()

hampshire <- 'https://en.wikipedia.org/wiki/Hampshire_County,_Massachusetts' %>% 
    read_html() %>%
    html_node(xpath = '//*[@id="mw-content-text"]/div/table[4]') %>%
    html_table()

# some housekeeping
county <- rbind(middlesex, norfolk, suffolk, essex, bristol, plymouth, worcester, hampshire)

roxbury <- suffolk[4, ]
roxbury$Town <- 'Roxbury'
county <- rbind(county, roxbury)

e_boston <- suffolk[4, ]
e_boston$Town <- 'East-Boston'
county <- rbind(county, e_boston)


towns <- unique(dataset$Town)
sapply(towns, FUN = function(x) x %in% county$Town)

town_info <- data.frame(capita_income = character(), 
                        median_house  = character(), 
                        median_family = character(), 
                        population    = character(),
                        num_houses    = character())

for (i in 1:dim(dataset)[1]) {
    town <- dataset$Town[i]
    if (town %in% county$Town) {
        tmp <- county[grep(town, county$Town)[1], 4:8]
        town_info <- rbind(town_info, tmp)
    } else {
        tmp <- county[grep('Suffolk', county$Town)[1], 4:8]
        town_info <- rbind(town_info, tmp)
    }
}

for (j in 1:5) {
    tmp1 <- town_info[, j]
    tmp1 <- gsub('\\$', '', tmp1)
    tmp1 <- gsub(',', '', tmp1)
    tmp1 <- as.numeric(tmp1)
    town_info <- cbind(town_info, tmp1)
}
colnames(town_info) <- c(colnames(town_info)[1:5], 
                         'per_capita_in', 
                         'median_house_in',
                         'median_family_in', 
                         'pop', 
                         'num_house')
dataset <- cbind(dataset, town_info)
dataset <- dataset[, -c(13, 14, 15,16,17)]
#data <- get(load("/Users/victorjs/desktop/insight/Boston.RData"))
write.csv(dataset, file = '/Users/victorjs/desktop/insight/boston_new.csv', row.names = FALSE)