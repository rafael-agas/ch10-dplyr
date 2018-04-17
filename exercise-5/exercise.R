# Exercise 5: dplyr grouped operations

# Install the `nycflights13` package. Load (`library()`) the package.
# You'll also need to load `dplyr`
#install.packages("nycflights13")  # should be done already
library(nycflights13)
library(dplyr)
View(flights)
# What was the average departure delay in each month?
# Save this as a data frame `dep_delay_by_month`
# Hint: you'll have to perform a grouping operation then summarizing your data
dep_delay_by_month <- flights %>%  group_by(month) 
  %>% summarise(mean = mean(dep_delay, na.rm = TRUE))

# Which month had the greatest average departure delay?
month <- dep_delay_by_month %>% filter(mean == max(dep_delay_by_month))
  %>% select(month)

# If your above data frame contains just two columns (e.g., "month", and "delay" in that order), you can create
# a scatterplot by passing that data frame to the 'plot()' function
plot(dep_delay_by_month)

# To which destinations were the average arrival delays the highest?
# Hint: you'll have to perform a grouping operation then summarize your data
# You can use the `head()` function to view just the first few rows
arrival_delay <- flights %>% group_by(dest) %>% 
  summarize(mean = mean(arr_delay, na.rm = TRUE)) %>% 
  arrange(-mean)

# You can look up these airports in the `airports` data frame!

# Which city was flown to with the highest average speed?
highest_speed <- flights %>% 
  mutate(speed = (distance / air_time) * 60) %>%
  group_by(dest) %>%
  summarize(mean = mean(speed, na.rm= TRUE)) %>%
  filter(mean == max(mean, na.rm = TRUE))