### Generate features based on spatial information
###
### Functions: 
###   CalDistance:       Calculate the distance between each address and famous attractions.
###   CalDistanceHelper: A helper function for CalDistance.
###   CalDisToSub:       Calculate the distance between each address and its nearest subway stations.
###   CalDisToSubHelper: A helper function for CalDisToSub.
###

rm(list = ls())

library(caret)

CalDistanceHelper <- function(x, coordinates) {
    # Calculate the distance between each address and famous attractions.
    #
    # Args:
    #   x:           The coordinates of an attraction.
    #   coordinates: The coordinates of a property.
    #
    # Returns:
    #   The euclidean distance, manhattan distance and maximum distance.
    temp <- matrix(c(-x[1], x[2], coordinates[1], coordinates[2]), 
                   nrow = 2, 
                   byrow = TRUE)
    return(c(dist(temp, method = 'euclidean'), 
             dist(temp, method = 'manhattan'), 
             dist(temp, method = 'maximum')))
}


CalDistance <- function(ref, coordinates) {
    # Calculate the distance between each address and famous attractions.
    #
    # Args:
    #   ref:         A list of coordinates of all attractions.
    #   coordinates: The coordinates of a property.
    #
    # Returns:
    #   The euclidean distance, manhattan distance and maximum distance.
    results <- apply(ref, MARGIN = 1, FUN = function(x) CalDistanceHelper(x, coordinates))
    return(data.frame(euclidean = results[1, ], 
                      manhattan = results[2, ],
                      maximum   = results[3, ]))
}


CalDisToSubHelper = function(x, subs) {
    # Calculate the distance between each address and its nearest subway station.
    #
    # Args:
    #   x:    The coordinates of a property.
    #   sbus: The coordinates of a subway station.
    #
    # Returns:
    #   The euclidean distance.
    temp = apply(subs, MARGIN = 1, FUN = function(y) (y[1] + x[1])^2 + (y[2] - x[2])^2)
    return(max(temp))
}


CalDisToSub = function(ref, subs) {
    # Calculate the distance between each address and its nearest subway stations.
    #
    # Args:
    #   ref:  A list of coordinates of all properties.
    #   subs: The coordinates of all subway stations.
    #
    # Returns:
    #   The euclidean distance, manhattan distance and maximum distance.
    results = apply(ref, MARGIN = 1, FUN = function(x) CalDisToSubHelper(x, subs))
    return(results)
}

# load data
root <- '~/Desktop/Projects/Rental price prediction/'
dataset <- read.csv(paste(root, 'data/Boston_dataset.csv', sep = ''))

# some housekeeping
# get rid of those rows which have missing values in Rent, Longitude and Latitude
dataset <- dataset[!is.na(dataset$Rent), ]
dataset <- dataset[!is.na(dataset$Longitude), ]
dataset <- dataset[!is.na(dataset$Latitude), ]

# add one column to indicate the missing pattern of SqFt and set the missing value to 0
dataset$SqFt[is.na(dataset$SqFt)] <- 0
dataset$missing_sqft <- ifelse(dataset$SqFt == 0, 1, 0)

# remove feature 'state' and 'Bd.'
dataset <- subset(dataset, select = -c(Bd., state))

# reorder all the features
dataset <- data.frame(Rent = dataset$Rent, 
                      subset(dataset, select = -Rent))

# convert num.bedrooms to numeric
dataset$num.bedrooms <- as.numeric(dataset$num.bedrooms)
dataset$num.bedrooms[dataset$num.bedrooms == 'NA'] <- 0.5


### list of attractions
mit        <- c(71.0942, 42.3601)
harvard    <- c(71.1167, 42.3770)
bu         <- c(71.1054, 42.3505)
bc         <- c(71.1685, 42.3355)
northeast  <- c(71.0892, 42.3398)
common     <- c(71.0655, 42.3550)
faneuil    <- c(71.0548, 42.3602)
copley     <- c(71.0778, 42.3472)
s_station  <- c(71.0554, 42.3514)
airport    <- c(71.0096, 42.3656)
middlesex  <- c(71.1014, 42.4524)
outlet     <- c(71.3524, 42.0382)
framingham <- c(71.4162, 42.2793)
lowell     <- c(71.3162, 42.6334)
quincy     <- c(71.0023, 42.2529)
lynn       <- c(70.9495, 42.4668)

locations <- c(mit, harvard, bu, bc, northeast, common, faneuil, copley, s_station,
              airport, middlesex, outlet, framingham, lowell, quincy, lynn)

# calculate the distance between each address and famous attractions
distances <- subset(dataset, select = c(Longitude, Latitude))
for (i in seq(1, 31, 2)) {
    temp <- CalDistance(subset(dataset, select = c(Longitude, Latitude)), 
                        c(locations[i], locations[i+1]))
    distances <- cbind(distances, temp)
}
distances <- distances[, 3:dim(distances)[2]]
colnames(distances) <- c('mit_euc', 'mit_man', 'mit_max',
                         'harvard_euc', 'harvard_man', 'harvard_max',
                         'bu_euc', 'bu_man', 'bu_max',
                         'bc_euc', 'bc_man', 'bc_max',
                         'northeast_euc', 'northeast_man', 'northeast_max',
                         'common_euc', 'common_man', 'common_max',
                         'faneuil_euc', 'faneuil_man', 'faneuil_max',
                         'copley_euc', 'copley_man', 'copley_max',
                         's_station_euc', 's_station_man', 's_station_max',
                         'airport_euc', 'airport_man', 'airport_max',
                         'middlesex_euc', 'middlesex_man', 'middlesex_max',
                         'outlet_euc', 'outlet_man', 'outlet_max',
                         'framingham_euc', 'framingham_man', 'framingham_max',
                         'lowell_euc', 'lowell_man', 'lowell_max',
                         'quincy_euc', 'quincy_man', 'quincy_max',
                         'lynn_euc', 'lynn_man', 'lynn_max')
dataset = cbind(dataset, distances)


# calculate the shortest distance to the subway station
boston_sub <- read.csv(paste(root, 'data/boston_sub_coords.csv', sep = ''))
closest_sub <- CalDisToSub(subset(dataset, select = c(Longitude, Latitude)), 
                         boston_sub)
dataset$closest_sub <- closest_sub

### create dummy variables
dummy <- dummyVars(~Apt.or.House + Utilities.included, data = dataset, levelsOnly = TRUE)
dummy_vars <- predict(dummy, dataset)
colnames(dummy_vars) <- c('apt', 'house', 'utility_no', 'utility_yes')
dataset <- cbind(dataset, dummy_vars)


# remove highly correlated features
# dataset <- subset(dataset, select = -c(mit_euc, mit_man, mit_max, bu_euc, bu_man, bu_max,
#                                        northeast_euc, northeast_man, northeast_max,
#                                        common_euc, common_man, common_max, copley_euc,
#                                        copley_man, copley_max, harvard_man, harvard_max,
#                                        bc_max, bc_man, faneuil_man, faneuil_max,
#                                        s_station_man, s_station_max, airport_man, airport_max,
#                                        middlesex_man, middlesex_max))