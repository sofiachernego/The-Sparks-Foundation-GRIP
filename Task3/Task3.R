# Exploratory data analysis - retail ====
# try to find weak areas where you can make more profit 
# what business problems can you derive by exploring the data?
# create a dashboard/storyboard 

# Download packages/libraries
library(readr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(magrittr)

# Download data ====
retail <- read.csv("/Users/macbookair/Desktop/GRIP/Task3/SampleSuperstore.csv")
summary(retail)

# Data cleaning and preparation ====
## Check missing values ====
retail.na <- is.na(retail)
print(retail.na)
summary(retail.na)

## Check duplicated values ====
duplicated.retail <- duplicated(retail)
summary(duplicated.retail)

# There are 17 cases of duplicated values
## Dropping duplicated values ====
retail.unique <- retail[!duplicated(retail), ]

# Remove the country and the postal code
retail.unique <- retail.unique %>% select(-c(Country, Postal.Code))


# Exploratory analysis ====
## Shipment mode analysis ====
ggplot(retail.unique, aes(Ship.Mode, fill = Ship.Mode)) +
  geom_bar()+
  ggtitle("Frequency of purchases by Shipment Mode")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# standard class in the most frequent shipping method

ggplot(retail.unique, aes(Ship.Mode, Sales, fill = Ship.Mode)) +
  geom_col() +
  ggtitle("Sales by the Shipment Mode")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
#st class generates most sales, followed by second and first classes

ggplot(retail.unique, aes(Ship.Mode, Profit, fill = Ship.Mode)) +
  geom_col() +
  ggtitle("Profit by the Shipment Mode")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


## Segment analysis ====
ggplot(retail.unique, aes(Segment, fill = Segment)) +
  geom_bar()+
  ggtitle("Frequency of purchases by Segment")
# consumer is the most frequent segment, followed by corporate and home office
# home office segment is the weak area, let's look if sales-wise it is the same

ggplot(retail.unique,aes(Segment,Sales, fill = Segment)) +
  geom_col()+
  ggtitle("Segmentwise Sales")


## City analysis ====
# Identify cities with most frequent sales
city_freq <- table(retail.unique$City)
sorted_freq <- sort(city_freq, decreasing = TRUE)
top_cities <- names(head(sorted_freq, 10))
print(top_cities)
# New York City, LA, Philadelphia, San Francisco, Seattle, Houston, Chicago, 
# Columbus, San Diego and Springfield are the top cities 

## Category analysis ====
ggplot(retail.unique, aes(Category, fill = Category)) +
  geom_bar()
# office supplies is the most frequent category, let's look at it in more detail 

ggplot(retail.unique,aes(Category,Sales, fill = Category))+
  geom_col()+
  ggtitle("categorywise Sales")
# office supplies generate least sales


## Sub category analysis ====
ggplot(retail.unique, aes(Sub.Category, fill = Sub.Category)) +
  geom_bar()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# Binders, paper, furnishings, phones and storage are the top 5 purchased items
# let's look at sales within sub categories 

ggplot(retail.unique,aes(Sub.Category,Sales, fill = Sub.Category))+
  geom_col()+
  ggtitle("Sub categorywise Sales")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) # rotates the names for easier read
# chairs and phones generate most sales
# fasteners, labels, art generate least sales

ggplot(retail.unique,aes(Sub.Category,Profit, fill = Sub.Category))+
  geom_col()+
  ggtitle("Sub categorywise Profit")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


## Sales analysis ====
ggplot(retail.unique, aes(Sales)) +
  geom_boxplot()
# sales has some abnormally high value

# replace that value with an average sales value
maxSales <- max(retail.unique$Sales)
retail.unique$Sales <- replace(retail.unique$Sales, retail.unique$Sales==maxSales, mean(retail.unique$Sales))
summary(retail.unique$Sales)


## Quantity analysis ====
ggplot(retail.unique, aes(Quantity, fill = "pink"))+
  geom_bar()+
  scale_x_continuous(breaks = seq(0, max(retail.unique$Quantity), by = 1))+
  theme_bw()+
  ggtitle("Frequency of purchases by Quantity of items")+
  theme(legend.position = "none")


## Profit analysis ====


## Discount analysis ====
ggplot(retail.unique, aes(Discount))+
  geom_histogram()

ggplot(retail.unique, aes(Discount, Sales, fill = Discount))+
  geom_col()+
  ggtitle("Sales by discount")
# most sales were made when no discount was applied or when 20% was applied

ggplot(retail.unique, aes(Discount, Profit, fill = Discount))+
  geom_col()+
  ggtitle("Profit by discount")
# losing profit on heavily discounted items 


## Region analysis ====
ggplot(retail.unique, aes(Region, fill = Region))+
  geom_bar()+
  ggtitle("Frequency of purchases by the Region")
# most frequent purchases in West, followed by East and Central
# least purchases in the South

ggplot(retail.unique, aes(Region, Sales, fill = Region))+
  geom_col()+
  ggtitle("Sales by the Region")
# same pattern as above

ggplot(retail.unique, aes(Region, Profit, fill = Region))+
  geom_violin()+
  ggtitle("Profit bt the Region")
# East shows to be losing the most
# Central region seems to be generating most profit? 

# Sales in each region by segment
ggplot(retail.unique, aes(Region, Sales, fill = Segment)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Region", y = "Sales", fill = "Segment") +
  ggtitle("Segment-wise Sales in Each Region")


## State analysis ====
# Calculate the frequency of each state
state_frequency <- count(retail.unique, State)

# Reorder the levels of the State factor based on the frequency of Sales
retail.unique$State <- factor(retail.unique$State, levels = state_frequency$State[order(state_frequency$n)])

# Create the bar plot with the reordered levels
ggplot(retail.unique, aes(State, fill = State)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, margin = margin(15, 0, 15, 0)), 
        legend.position = "none")+
  xlab("State") +
  ylab("Sales") +
  ggtitle("Sales by State")


