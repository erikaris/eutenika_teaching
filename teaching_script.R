# Materials for teaching at Eutenika

#===========================================================================
# 1. required library

library(tidyverse)   # the core of other packages
library(readr)       # for reading various types of data
library(dplyr)       # for playing with the data later. 
library(ggplot2)     # for creating graph and plot
library(countrycode) # for changing the country name to its english name
# library(ISOcodes)    # for changing the country name to its english name
library(gtools)      # for replacing missing values with 0
library(plotrix)     # for computing std.error
library(corrplot)    # for plotting the correlation matrix
library(GGally) # for creating scatter plot

#===========================================================================

# 2. set working directory
setwd("/run/media/erikaris/DATA/opportunities/malang_teaching")

#===========================================================================

# 3. import the dataset
hb <- read_csv("./data/hotel-booking-demand/hotel_bookings.csv", na = c("", "NA", "NULL", "NaN", "null", "Null", "<NA>"))

#===========================================================================

# 4. Explore the imported data 
# 4a. look at the whole data
  view(hb)   # option 2: hb
# 4b. look at the structure
str(hb)
# 4c. get a glimpse of the data
glimpse (hb)
# 4d. the first 5 rows
head(hb)
# 4e. the last 5 rows
tail(hb)
# 4f. see the dimension of the data (row and column)
dim(hb)
# 4g. see the statistical summaries of each column. 
summary(hb)
# 4h. get the list of column names. 
names(hb)

# =========== 5. cleaning/tidying ====================

# 5a. is there any NA values?
hb_na <- apply(is.na(hb), 2, FUN=which)
glimpse(hb_na)  

# 5b. how many NA values we have
na_count <-sapply(hb, function(y) sum(is.na(y)))
na_count

# null values exist in the column 'children', 'country', 'agent', and 'company'
# what to do with the null value? 
# fill the NA with 0 using dplyr's coalesce(). 
# we'll learn it by doing in the next sessions. 
# from now, we'll only be working on the 'hb_clean' dataset. 


hb_clean <- hb %>% 
              # handle the missing values by replacing any missing characters (e.g: NA, N/A, NaN, Null, NULL, null) to 0.
              mutate(children = coalesce(children, 0), # replace NA with 0 using coalesce()
                     agent = coalesce(agent, 0), 
                     company = coalesce(company, 0), 
                     country = coalesce(country, 'unknown')
              ) %>% 
              # replace country 'CN' with 'CHN', and 'TMP' with 'TLS'
              # > unique(hb$meal)
              # [1] "BB"        "FB"        "HB"        "SC"        "Undefined"
              # replace meal 'Undefined' with 'SC'
              mutate(country = replace(country, country == "CN", "CHN"), 
                     country = replace(country, country == "TMP", "TLS"), 
                     meal = replace(meal, meal == "Undefined", "SC"), 
                     guests = adults + children + babies) %>% 
              mutate(country_name = countrycode(country, origin = 'iso3c', destination = 'country.name')) %>%  # change country name to its real name
              filter(is_canceled == 0, guests > 0)  # we'll only care about the booking that are not cancelled.

# =============Starting Answering Questions ===========================

# here's wehere we start using dplyr. 
# the raw data is misleading as this includes cancelled booking
# thus, we need to filter before group_by
# we also want to show the percentage
# thus, need to define variable 'total_guests' first. 
 
# ======== 6. Where do the guests come from? ========================

total_guests <- hb_clean %>% 
                  summarize(sum(guests)) %>% 
                  as.numeric()

guest_origin <- hb_clean %>% 
                  group_by(country_name) %>% 
                  summarize(guests_country = sum(guests)) %>% 
                  arrange(desc(guests_country)) %>% 
                  # use sprintf to round the number
                  mutate(guest_pct = sprintf("%0.2f",guests_country*100/total_guests)) 

guest_origin5 <- guest_origin %>%  top_n(5)


# =====================================================================

# 6a. plot the data
hb_guestplot <- ggplot(guest_origin5, aes(x="", y=guest_pct, fill=country_name)) +
                  geom_bar(width = 1, stat = "identity") +
                  coord_polar("y", start=0) 


# ============ 7. what is the average room rate per night ================

# hb_ar = harga booking average rate. 
# Counting adults and children as paying guests only, not babies.

hb_ar <- hb_clean %>% 
          group_by(hotel) %>% 
          summarize(rate_avg = mean(adr))


# but the number of guests varies accross the days. 
# so, it's fair to also consider the number of guests. 


# ============ 8. what is average room rate per night per person  =========================


# hb_arp = harga booking average rate person. 
# Counting adults and children as paying guests only, not babies.

hb_arp <- hb_clean %>% 
            mutate(rate_person = adr/(adults + children)) %>%  # replace country 'CN' with 'CHN'
            group_by(hotel) %>% 
            select(hotel, adr, adults, children, rate_person) %>% 
            summarize(rate_person_avg = sprintf("%0.2f", mean(rate_person)))


#============= 9. Price of room types per night and person  ==============================

# hb_arpt = harga booking average rate people type

hb_arpt <- hb_clean %>% 
            mutate(rate_person = adr/(adults + children)) %>% 
            group_by(hotel, reserved_room_type) %>% 
            summarize(mean_rp = mean(rate_person), sd_rp = sd(rate_person)) %>% 
            ggplot(aes(x = reserved_room_type, y = mean_rp, fill = hotel)) +
              geom_bar(position = position_dodge(), stat = "identity") +
              geom_errorbar(aes(ymin = mean_rp - sd_rp, ymax = mean_rp + sd_rp), position = position_dodge(), stat = 'identity', width = 0.9, colour = 'black') +
              scale_x_discrete(name = "reserved room type") +
              scale_y_continuous(name = "average rate per night per person", limits = c(0, 120), breaks = seq(0, 120, 10))
  
  
# ========== 10. How does the price per night vary between 2015 - 2017 ================

# For line graphs, the data points must be grouped so that it knows which points to connect. In this case, it is simple -- all points should be connected, so group=1. When more variables are used and multiple lines are drawn, the grouping for lines is usually done by variable.

# > unique(hb_clean$arrival_date_year)
# [1] 2015 2016 2017

# we need to convert the arrival_date_month to factor so that we can sort it later. 

hb_clean %>% 
  mutate(arrival_date_month = factor(arrival_date_month, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))) %>% 
  mutate(rate_person = adr/(adults + children)) %>% 
  group_by(hotel, arrival_date_month) %>% 
  summarize(rate_month_avg = mean(rate_person), rate_month_sd = sd(rate_person)) %>% 
  ggplot(aes(x = arrival_date_month, y = rate_month_avg, col = hotel, group = hotel)) + 
    geom_line() +
    geom_ribbon(aes(ymin = rate_month_avg - rate_month_sd, ymax = rate_month_avg + rate_month_sd, alpha = 0.6, fill = hotel)) +
  labs(title = "Average hotel rate per night between 2015 - 2017", x = 'month', y = 'rate (Euro)')

# This clearly shows that the prices in the Resort hotel are much higher during the summer (no surprise here).
# The price of the city hotel varies less and is most expensive during spring and autumn.


# ========== 11. Which are the most busy month? ================

# we want to find the average number of hotel guests per month
# month vs number of guests. 

hb_busy <- hb_clean %>% 
            mutate(arrival_date_month = factor(arrival_date_month, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))) %>% 
            group_by(hotel, arrival_date_month) %>% 
            summarize(guests_month = sum(guests)) %>% 
            # normalize the data because the dataset contains July and August date from 3 years, the other month from 2 years
            mutate(guests_month_norm = ifelse((arrival_date_month == 'July' | arrival_date_month == 'August'), guests_month/3, guests_month/2)) %>% 
            ggplot(aes(x = arrival_date_month, y = guests_month_norm, group = hotel, col = hotel)) +
              geom_line() +
              labs(title = "Number of cumulative hotel guests per month", x = "month", y = "number of guests") +
              theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
              scale_y_continuous(name = "average rate per night per person", limits = c(1500, 4500), breaks = seq(1500, 4500, 500)) 

# The City hotel has more guests during spring and autumn, when the prices are also highest. In July and August there are less visitors, although prices are lower.

# Guest numbers for the Resort hotel go down slighty from June to September, which is also when the prices are highest. Both hotels have the fewest guests during the winter.


# ============ 12. How long do people stay at the hotels? =============

hb_nights <- hb_clean %>% 
              mutate(total_nights = stays_in_week_nights + stays_in_weekend_nights) %>% 
              filter(total_nights > 0) %>% 
              group_by(hotel, total_nights) %>% 
              summarize(num_trx = n()) %>% 
              mutate(num_trx_pct = round(num_trx*100/sum(num_trx), digits = 2))

# plot it
ggplot(hb_nights, aes(x = total_nights, y = num_trx_pct, fill = hotel)) +
  geom_bar(position = position_dodge(), stat = 'identity')

# top 5 bookings for both hotels

hb_nights5 <- hb_clean %>% 
                mutate(total_nights = stays_in_week_nights + stays_in_weekend_nights) %>% 
                filter(total_nights > 0) %>% 
                group_by(hotel, total_nights) %>% 
                summarize(num_trx = n()) %>% 
                mutate(num_trx_pct = round(num_trx*100/sum(num_trx), digits = 2)) %>% 
                top_n(5)


# plot it
ggplot(hb_nights5, aes(x = total_nights, y = num_trx_pct, fill = hotel)) +
  geom_bar(position = position_dodge(), stat = 'identity') + 
  scale_x_continuous(name = "length of stay (nights)", breaks = seq(0, 8, 1)) +
  labs(title = "Top 5 Length of Stay", y = "number of bookings")


# For the city hotel there is a clear preference for 1-4 nights.
# For the resort hotel, 1-4 nights are also often booked, but 7 nights also stand out as being very popular.


#=============== 13. Which are the most busy month?  ==============================================

hb_guests_all <- hb %>% 
  mutate(arrival_date_month = factor(arrival_date_month, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))) %>% 
  filter(is_canceled == 0, (adults + children) > 0) %>% 
  mutate(children = coalesce(children, 0), # replace NA with 0
         country = replace(country, country == "CN", "CHN"), 
         rate_person = adr/(adults + children), 
         hotel = replace(hotel, hotel == "City Hotel", "city_hotel"), 
         hotel = replace(hotel, hotel == "Resort Hotel", "resort_hotel"), 
         guests = adults + children + babies) %>% 
  group_by(hotel, arrival_date_year, arrival_date_month) %>% 
  summarize(guest_month_total = sum(guests))


summarize(guests = mean(adults + children + babies)) %>%
  ggplot() +
  geom_line(aes(x = arrival_date_month, y = month_avg, col = hotel, group = hotel)) + 
  labs(title = "Average hotel rate per night over the years in Euro", x = "month", y = "average night rate (Euro)")

# =================== 14. check correlation between variables ==========================

# 1. create the correlation matrix of the hb_clean
# notes that correlation can only be calculated on columns that are numeric. 
# therefore, we have to select numeric columns. 

# need to modify the hb_clean a bit. 
# we want to take the whole rows for both `canceled` and `not canceled` data

hb_clean_cancel <- hb %>% 
                    # handle the missing values by replacing any missing characters (e.g: NA, N/A, NaN, Null, NULL, null) to 0.
                    mutate(children = coalesce(children, 0), # replace NA with 0 using coalesce()
                           agent = coalesce(agent, 0), 
                           company = coalesce(company, 0), 
                           country = coalesce(country, 'unknown')
                    ) %>% 
                    # replace country 'CN' with 'CHN', and 'TMP' with 'TLS'
                    # > unique(hb$meal)
                    # [1] "BB"        "FB"        "HB"        "SC"        "Undefined"
                    # replace meal 'Undefined' with 'SC'
                    mutate(country = replace(country, country == "CN", "CHN"), 
                           country = replace(country, country == "TMP", "TLS"), 
                           meal = replace(meal, meal == "Undefined", "SC"), 
                           guests = adults + children + babies) %>% 
                    mutate(country_name = countrycode(country, origin = 'iso3c', destination = 'country.name'))

# select numeric columns only
hb_num <- hb_clean_cancel %>% 
            select_if(is.numeric) %>% 
            select(-c(is_canceled, is_repeated_guest, arrival_date_year))

# check the remaining columns, is there any column that is not suitable for computing correlation. 
str(hb_num)

# create the correlation matrix
hb_num_cor <- cor(hb_num)

# create corrplot
corrplot(hb_num_cor, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)


# ==================================================





# create another correlation plot using package GGally
# takes more time than corrplot
ggpairs(hb_num)


# chart of correlation matrix
# chart.Correlation(hb_num, histogram=TRUE, pch=19)


# my_data <- mtcars[, c(1,3,4,5,6,7)]
# chart.Correlation(my_data, histogram=TRUE, pch=19)


hb_num <- hb %>% select_if(hb, is.numeric) 

hb_num2 <- hb_num %>% filter(is.na(colSums))

hb_num[,colSums(hb_num) > 0]

hb_num <- dplyr::select_if(hb, is.numeric)

hb_num[, colSums(hb_num != 0) > 0]



cor_hb_num <- cor(hb_num)

cor_hb <- cor(hb)
cor_hbclean <- cor(hb_clean)

# 2. visualize in heatmap

# 3. visualize in correlogram: http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software

# 4. visualize in ggally and ggpairs. 
# https://www.datacamp.com/community/blog/r-correlation-tutorial


# ========== 13. How does the price per night varies over the months in 2017 ================

# For line graphs, the data points must be grouped so that it knows which points to connect. In this case, it is simple -- all points should be connected, so group=1. When more variables are used and multiple lines are drawn, the grouping for lines is usually done by variable.

hb_year_2017 <- hb %>% 
                  mutate(arrival_date_month = factor(arrival_date_month, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))) %>% 
                  filter(is_canceled == 0, (adults + children) > 0, arrival_date_year == 2017) %>% 
                  mutate(children = coalesce(children, 0), # replace NA with 0
                         country = replace(country, country == "CN", "CHN"), 
                         rate_person = adr/(adults + children), 
                         hotel = replace(hotel, hotel == "City Hotel", "city_hotel"), 
                         hotel = replace(hotel, hotel == "Resort Hotel", "resort_hotel")) %>% 
                  group_by(hotel, arrival_date_year, arrival_date_month) %>% 
                  summarize(month_avg = mean(rate_person)) %>% 
                  ggplot() +
                  geom_line(aes(x = arrival_date_month, y = month_avg, col = hotel, group = hotel)) 


#======================

asdf <- c(1, 5, 0, 6, 8)
ghjk <- c(7, 1, 5, 1, 2)

df <- tibble(asdf, ghjk)

ggplot(df, aes(x=asdf, y = ghjk))+
  geom_line()


# hb_arpt2 <- hb %>% 
#               filter(is_canceled == 0, (adults + children) > 0) %>% 
#               mutate(children = coalesce(children, 0), # replace NA with 0
#                      country = replace(country, country == "CN", "CHN"), 
#                      rate_person = adr/(adults + children)) %>%  # replace country 'CN' with 'CHN'
#               group_by(hotel, reserved_room_type) %>% 
#               select(hotel, reserved_room_type, adr, adults, children, rate_person)
# 
# hb_arpt2_plot <- ggplot(hb_arpt2, aes(reserved_room_type, y = rate_person, fill = hotel)) +
#                   scale_x_discrete(name = "reserved room type") +
#                   scale_y_continuous(name = "average rate per night per person", limits = c(0, 300), breaks = seq(0, 300, 25)) + 
#                   geom_bar(position=position_dodge(), stat="identity")+
#                   geom_errorbar(aes(ymin = mean(rate_person) - std.error(rate_person), ymax = mean(rate_person) + std.error(rate_person)), position=position_dodge(width=0.9), colour="black", stat = 'identity')
                

# asdf <- hb %>% 
#           filter(is_canceled == 0, (adults + children + babies) > 0, country == 'PRT') %>% 
#           nrow()
#   

