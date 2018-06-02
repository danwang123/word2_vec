### Scrape property information from Rentals.com
###
### Functions: 
###   BuildDataset:    The main function used to build dataset
###   GetPropertyHref: Obtain reference numbers for all the properties.
###   GetAptInfo:      Get the basic information for an apartment.
###   GetHouseInfo:    Get the basic information for a house.
###   GetInfo:         Combine GetAptInfo and GetHouseInfo.
###

rm(list = ls())

library(rvest)
library(dplyr)
library(reshape)
library(ggmap)


GetPropertyHref <- function(url) {
    # Obtain reference numbers for all the properties.
    #
    # Args:
    #   url: The websit address.
    #
    # Returns:
    #   A vector of reference numbers for all the properties.
    href <- read_html(url) %>%
        html_nodes('.result') %>%
        html_attr('data-tag_listing_id')
    return(href)
}


GetInfo <- function(item, state, town) {
    # Get the basic information (rent, number of bedrooms and so on) for a property.
    #
    # Args:
    #   item:  The reference number for the property.
    #   state: The state of the property.
    #   town:  The town of the property.
    #
    # Returns:
    #   A data frame contains the basic information of the property.
    page <- paste('http://www.rentals.com', state, town, item, sep = '/') %>% 
        read_html
    junk <- unlist(strsplit(item, split = '/'))
    
    town <- junk[3]
    utilities <- grep("[Uu]tilities", 
                      readLines(paste('http://www.rentals.com', 
                                      state, town, item, sep = '/'))) %>% 
        length %>% # check is there a match
        as.logical
    
    # some info is written in different format for house and apt 
    # call the corresponding function to get correct results
    if (is.na(as.numeric(junk[4]))) {
        info <- GetHouseInfo(page) 
    } else info <- GetAptInfo(page)
    
    return(info %>% mutate('town' = town, 'utilities' = utilities))
}


GetHouseInfo <- function(page) {
    # Get the basic information for a house.
    #
    # Args:
    #   page: The website address for the house.
    #
    # Returns:
    #   A data frame contains the basic information for the house.
    floorplan <- unlist(strsplit(page %>% html_node('#summary_floorplan') %>% 
                                     html_text, '[|]'))
    
    full_loc <- page %>% 
        html_node('#summary_address') %>% 
        html_text
    
    coordinates <- geocode(full_loc)
    location <- full_loc %>% strsplit(split = ',')
    state_zip <- location[[1]][length(location[[1]])] %>% 
        strsplit(split = ' ') %>% 
        unlist
    
    price <- page %>% 
        html_node('#summary_price strong') %>% 
        html_text
    
    data.frame('address'      = location[[1]][1], 
               'state'        = state_zip[2], 
               'zip'          = state_zip[3], 
               'rent'         = price, 
               'Bd.'          = floorplan[1], 
               'SqFt.'        = floorplan[3], 
               'Apt or House' = "House", 
               'longitude'    = coordinates$lon, 
               'latitude'     = coordinates$lat)
}


GetAptInfo <- function(page) {
    # Get the basic information for an apartment.
    #
    # Args:
    #   page: The website address for the apartment.
    #
    # Returns:
    #   A data frame contains the basic information for the apartment.
    tab <- html_table(html_nodes(page, 'table'))[[2]]
    full_loc <- page %>% 
        html_node('#summary_address') %>% 
        html_text
    
    coordinates <- geocode(full_loc)
    location <- full_loc %>% strsplit(split = ',')
    state_zip <- location[[1]][length(location[[1]])] %>% 
        strsplit(split = ' ') %>% 
        unlist
    
    data.frame('address'      = location[[1]][1], 
               'state'        = state_zip[2], 
               'zip'          = state_zip[3], 
               'rent'         = tab$Price, 
               'Bd.'          = tab$Bd., 
               'SqFt.'        = tab$'Sq. Ft.', 
               'Apt or House' = "Apartment",
               'longitude'    = coordinates$lon, 
               'latitude'     = coordinates$lat)
}

### define a function to build datasets ###
### it takes two arguments: town and state###
### it returns all rental info for the town and state specified in a data frame ###
BuildDataset <- function(town, state) {
    # Build dataset for the selected town and state
    #
    # Args:
    #   town:  The town you are interested in.
    #   state: The state you are interested in.
    #
    # Returns:
    #   A data frame contains the basic information for all the available properties.
    url <- paste("http://www.rentals.com", state, town, "?per_page=10000", sep = "/")
    
    dataset <- url %>% 
        GetPropertyHref %>% 
        as.list %>% 
        lapply(function(x) GetInfo(x, state, town)) %>% 
        merge_recurse
    
    # housekeeping
    dataset$town <- sapply(dataset$town, function(x) gsub('-', ' ', x))
    
    rents <- gsub('-', ' ', rents <- gsub('\\$|,', '', dataset$rent))
    rents <- sapply(rents, 
                    function(x) x %>% 
                        strsplit(" ") %>% 
                        unlist %>% 
                        as.numeric %>% 
                        mean(na.rm = TRUE))
    rents[is.nan(rents)] <- NA
    dataset$rent <- rents
    
    sqft <- gsub('-', ' ', gsub(',', '', dataset$SqFt.))
    sqft <- sapply(sqft, 
                   function(x) x %>% 
                       strsplit(" ") %>% 
                       unlist %>% 
                       as.numeric %>% 
                       mean(na.rm = TRUE))
    sqft[is.nan(sqft)] <- NA
    dataset$SqFt. <- sqft
    
    num.bedrooms <- as.numeric(gsub('[^0-9]','',dataset$Bd.))
    num.bedrooms[is.na(num.bedrooms)] <- 1
    dataset$Bd. <- num.bedrooms
    
    dataset$utilities <- sapply(dataset$utilities, 
                                function(x) if(x) 'Yes' else 'No')
    
    address_temp <- character(0)
    for (i in 1:length(dataset$address)) 
        address_temp[i] <- gsub(dataset$town[i], '', dataset$address[i]) 
    dataset$address <- address_temp
    
    dataset[c('address', 'town', 'state', 'zip', 'longitude', 'latitude', 
              'rent', 'Bd.', 'SqFt.', 'utilities', 'Apt.or.House')] 
}

### test all the functions ###
dataset_amherst_ma <- BuildDataset('Amherst', 'Massachusetts')




