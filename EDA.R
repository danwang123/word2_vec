### Perform some exploratory data analysis
###


rm(list = ls())

library(ggplot2)
library(dplyr)
library(corrplot)

### data are stored in '~/Desktop/predict_boston_rental_price/data/'
root <- '~/Desktop/Projects/Rental price prediction/'
dataset <- read.csv(paste(root, 'data/old_oct_16/Boston_dataset.csv', sep = ''))
y <- dataset$Rent
X <- dataset[, -3]
dataset <- data.frame(Rent = y, X)

########################## check missing pattern ##########################
features <- colnames(dataset)
missing_data <- colSums(is.na(dataset))
barplot(height    = missing_data, 
        names.arg = features,
        ylab      = 'number of missing values',
        main      = 'Number of missing values in different columns')

## missing percent in Rent by Town
m_perc_town <- dataset %>% 
    group_by(Town) %>% 
    summarise(percent = sum(is.na(Rent)) / length(Rent))

## missing percent in Rent by number of bedrooms
m_perc_bd <- dataset %>% 
    group_by(num.bedrooms) %>% 
    summarise(percent = sum(is.na(Rent)) / length(Rent))

## missing percent in SqFt by Town
m_perc_town_sqft <- dataset %>% 
    group_by(Town) %>% 
    summarise(percent = sum(is.na(SqFt)) / length(SqFt))

## missing percent in SqFt by number of bedrooms
m_perc_bd_sqft <- dataset %>% 
    group_by(num.bedrooms) %>% 
    summarise(percent = sum(is.na(SqFt)) / length(SqFt))


## check missing pattern in Rent for different towns
missing_Rent <- dataset[is.na(dataset$Rent), ] %>%
    group_by(Town) %>%
    summarise(total = n())
missing_Rent <- merge(missing_Rent, m_perc_town, key = Town)
missing_Rent <- missing_Rent[order(missing_Rent$total, decreasing = TRUE), ]
plot <- barplot(height    = missing_Rent$total[1:10], 
                names.arg = missing_Rent$Town[1:10],
                ylim      = c(0, 430),
                ylab      = 'number of missing values',
                main      = 'Number of missing values in Rent for different towns')
text(plot,
     missing_Rent$total[1:10] + 10, 
     labels = round(missing_Rent$percent[1:10], 2), 
     cex = 0.8)

## check missing pattern in Rent for different number of bedrooms
missing_Rent_bd <- dataset[is.na(dataset$Rent), ] %>%
    group_by(num.bedrooms) %>%
    summarise(total = n())
missing_Rent_bd <- merge(missing_Rent_bd, m_perc_bd, key = num.bedrooms)
missing_Rent_bd <- missing_Rent_bd[order(missing_Rent_bd$total, decreasing = TRUE), ]
plot <- barplot(height    = missing_Rent_bd$total, 
                names.arg = missing_Rent_bd$num.bedrooms,
                ylim      = c(0, 1000),
                ylab      = 'number of missing values',
                main      = 'Missing values VS Bd.')
text(plot,
     missing_Rent_bd$total + 30, 
     labels = round(missing_Rent_bd$percent, 2), 
     cex = 0.8)

## check missing pattern in SqFt for different towns
missing_SqFt <- dataset[is.na(dataset$SqFt), ] %>%
    group_by(Town) %>%
    summarise(total = n())
missing_SqFt <- missing_SqFt[complete.cases(missing_SqFt), ]
missing_SqFt <- merge(missing_SqFt, m_perc_town_sqft, key = Town)
missing_SqFt <- missing_SqFt[order(missing_SqFt$total, decreasing = TRUE), ]
plot <- barplot(height    = missing_SqFt$total[1:10], 
                names.arg = missing_SqFt$Town[1:10],
                ylim      = c(0, 1000),
                ylab      = 'number of missing values',
                main      = 'Number of missing values in SqFt for different towns')
text(plot,
     missing_SqFt$total[1:10] + 30, 
     labels = round(missing_SqFt$percent[1:10], 2), 
     cex = 0.8)

## check missing pattern in SqFt for different number of bedrooms
missing_SqFt_bd <- dataset[is.na(dataset$SqFt), ] %>%
    group_by(num.bedrooms) %>%
    summarise(total = n())
missing_SqFt_bd <- merge(missing_SqFt_bd, m_perc_bd_sqft, key = num.bedrooms)
missing_SqFt_bd <- missing_SqFt_bd[order(missing_SqFt_bd$total, decreasing = TRUE), ]
barplot(height    = missing_SqFt_bd$total, 
        names.arg = missing_SqFt_bd$num.bedrooms,
        ylim      = c(0, 1250),
        ylab      = 'number of missing values',
        main      = 'Missing values VS Bd.')
text(plot,
     missing_SqFt_bd$total + 30, 
     labels = round(missing_SqFt_bd$percent, 2), 
     cex = 0.8)
########################## check missing pattern ##########################


########################## Rent VS different features ##########################
rm(list = ls())
root <- '~/Desktop/Projects/Rental price prediction/'
dataset <- read.csv(paste(root, 'data/boston_data_complete.csv', sep = ''))

# number of apartments in each town
by_town <- dataset %>% 
    group_by(Town) %>% 
    summarise(count = n())
by_town <- by_town[order(by_town$count, decreasing = TRUE), ]
with(by_town, barplot(names.arg = Town[1:5], 
                      height    = count[1:5],
                      ylab      = 'Count',
                      xlab      = 'Town',
                      main      = 'Number of apartment in each town'))

# number of apartments for different number of bedrooms
by_bd <- dataset %>% 
    group_by(num.bedrooms) %>% 
    summarise(count = n())
by_bd <- by_bd[order(by_bd$count, decreasing = TRUE), ]
with(by_bd, barplot(names.arg = num.bedrooms, 
                    height    = count,
                    ylab      = 'Count',
                    xlab      = 'Number of Bedrooms',
                    main      = 'Number of apts VS number of bds'))

# boxplot of Rent for different number of bedrooms
ggplot(data = dataset, aes(x = as.factor(num.bedrooms), y = Rent)) +
    geom_boxplot() +
    xlab('Number of bedrooms') +
    ggtitle('Rent VS Number of bedrooms') +
    geom_hline(yintercept = mean(dataset$Rent), col = 'red') +
    geom_hline(yintercept = median(dataset$Rent), col = 'red', linetype = 2) +
    theme(plot.title = element_text(size = 12, colour = 'black', hjust = 0.5),
          axis.text = element_text(size = 10), axis.title = element_text(size = 12))

# boxplot of Rent for different towns (only show 7 towns with most number of apartments)
ggplot(data = dataset %>% filter(Town %in% by_town$Town[1:7]),
       aes(x = Town, y = Rent)) +
    geom_boxplot() +
    ggtitle('Rent VS Town') +
    geom_hline(yintercept = mean(dataset$Rent), col = 'red') +
    geom_hline(yintercept = median(dataset$Rent), col = 'red', linetype = 2) +
    theme(plot.title = element_text(size = 12, colour = 'black', hjust = 0.5),
          axis.text = element_text(size = 10), axis.title = element_text(size = 12))

# boxplot of Rent VS utilities
ggplot(data = dataset, aes(x = as.factor(Utilities.included), y = Rent)) +
    geom_boxplot() +
    xlab('Utilities') +
    ggtitle('Rent VS Utilities') +
    geom_hline(yintercept = mean(dataset$Rent), col = 'red') +
    geom_hline(yintercept = median(dataset$Rent), col = 'red', linetype = 2) +
    theme(plot.title = element_text(size = 12, colour = 'black', hjust = 0.5),
          axis.text = element_text(size = 10), axis.title = element_text(size = 12))

# density plot for SqFt
ggplot(dataset %>% 
           filter(Town %in% by_town$Town[1:3]) %>%
           filter(SqFt > 0),
       aes(x = SqFt)) + 
    geom_density(aes(group = Town, colour = Town, fill = Town), alpha = 0.3) +
    ylab('Density') +
    theme(plot.title = element_text(size = 12, colour = 'black', hjust = 0.5),
          axis.text = element_text(size = 10), axis.title = element_text(size = 12))

# per capita income for different towns 
by_town_income <- dataset %>% 
    group_by(Town) %>%
    summarise(income = sum(per_capita_in) / length(per_capita_in))
by_town_income <- merge(by_town_income, by_town, key = Town)
by_town_income <- by_town_income[order(by_town_income$count, decreasing = TRUE), ]
ggplot(by_town_income[1:7, ], aes(x = Town, y = income, fill = Town)) + 
    geom_bar(stat="identity", alpha = 0.5) +
    ylab('Income') +
    theme(plot.title = element_text(size = 12, colour = 'black', hjust = 0.5),
          axis.text = element_text(size = 10), axis.title = element_text(size = 12))
########################## Rent VS different features ##########################

########################## Correlation among features ##########################
feature_class <- sapply(dataset, class)
num_features <- colnames(dataset)[feature_class == 'numeric' | feature_class == 'integer']
dataset_num <- dataset[num_features]
dataset_num <- subset(dataset_num, select = -c(mit_euc, mit_man, mit_max, bu_euc, bu_man, bu_max,
                                               northeast_euc, northeast_man, northeast_max,
                                               common_euc, common_man, common_max, copley_euc,
                                               copley_man, copley_max, harvard_man, harvard_max,
                                               bc_max, bc_man, faneuil_man, faneuil_max,
                                               s_station_man, s_station_max, airport_man, airport_max,
                                               middlesex_man, middlesex_max, zip,
                                               lynn_max, lynn_man, quincy_man, quincy_max,
                                               lowell_man, lowell_max, outlet_man, outlet_max,
                                               framingham_man, framingham_max, faneuil_euc, airport_euc))
dataset_cor <- cor(dataset_num[, 1:(dim(dataset_num)[2] - 4)])
corrplot(dataset_cor, method="square")
########################## Correlation among features ##########################

