Introduction:
  1.1: The problem statement is to address which state and state county in the US had the most deaths by COVID-19 from 1/12/2020 – 1/13/2021. It is important to look at which state had the most deaths by COVID-19 because from there we can see which state had didn’t enough resources to help reduce deaths as well as population size, economic status and health status of the state’s population. It is also important to look at the difference between deaths per county in the top states. The distribution may be different based on which county is more populated. A county that is more populated may have more deaths than a rural county. As well, a county in one state that doesn’t have the largest number of deaths may have more deaths than a county with more deaths by state. This would explain the importance of resources.

1.2: I plan to address this problem by combining the data sets us, us-counties, and us-states. From here I can find which states had the most deaths by COVID-19 and which counties had the most deaths by COVID-19. The data will be from 1/12/20 – 1/13/21 which is the peak of the COVID-19 pandemic. 

1.3: Merging the data sets and using the correct methods such has merge, mutate and summarize will help find which counties and states have the highest deaths by COVID-19. I will find the top 10 number of deaths by state and top 10 number of deaths by county. Using graphs of the progression of COVID-19 in each county and each state will help demonstrate how fast or slow the transmission of COVID-19 is based on if the county is rural or urban as well if the state is mostly rural or urban.

1.4: The analysis will give the reader insight on how the number of deaths by county and state can differ. This means the state with the largest number of deaths may not include the county that has the greatest number of deaths. 

2.1:Packages loaded
2.2: Messages and warning surpressed
```{r, message=FALSE,warning=FALSE}
library(dplyr)
library(tidyverse)
```
2.3:Tidyverse: Tidyverse includes ggplot2 which used for creating graphs such as scatter plots, histograms and bar graphs, dplyr which is used for data manipulation such as merging the data, filtering the data or summarizing the data, string which manipulates strings and readr which allows us to read csv files such as us, us-states and us-counties.

3.1: [Links for csv files](https://github.com/nytimes/covid-19-data)
3.2:The source data comes from 3 csv files. All files contain COVID-19 data from 1/12/20-1/13/21. One file us.csv contains the variables cases, day and deaths which represent the cases and deaths of COVID-19 each day from 1/12/20-1/13/21 in the United States. The file us-states.csv contains the variables cases, deaths, day, and state  which represent the cases and deaths by COVID in each state for every day in 1/12/20-1/13/21. The file us-counties cotains the variables cases, deaths, day, state and county which represent the cases and deaths by COVID-19 in each county of each state on each day from the period 1/12/2020-1/12/2021.Missing values are coded as NA.
3.3 In text
3.4 In text as variable (final dataset). Variable shows first 10 values.
```{r, message=FALSE}
us <- read_csv("us.csv")
counties <- read_csv("us-counties.csv")
states <- read_csv("us-states.csv")
```

```{r}
#grouping the counties together so that each county isn't recognized as different and combining counties and summing each county's respective cases and deaths so that the data is set up as cases per county and deaths per county.

grouped_counties <- counties %>%
  group_by(county,state) %>%
  summarize(total_cases = sum(cases), total_deaths = sum(deaths))

grouped_counties

#organizing data so that data is organized from highest to lowest deaths 
ordered_data <- arrange(grouped_counties, desc(total_deaths))

ordered_data %>% head(10)

#top 10 counties by death
topdeaths <- ordered_data %>%
  arrange(desc(total_deaths)) %>%
  head(10)


#summary of new data set, and each variable within the dataset
summary(ordered_data)
summary(ordered_data$total_cases)
summary(ordered_data$total_deaths)

#organizing data so that data is organized from highest to lowest cases
ordered_data_cases <- arrange(grouped_counties, desc(total_cases))
#top 10 counties by cases
topcases <- ordered_data %>%
  arrange(desc(total_cases))%>%
  head(10)
```
3.5: 
  Of the new dataset, there are two variables, total cases of COVID-19 per county in the time period 1/12/2020-1/13/2021 and total deaths by COVID-19 per county in the time period 1/12/2020-1/13/2021.Of the total cases variable, there was a max 72142831 cases in one of the US on one day during the time period and a minimum of 35 in one county.The average number of cases in the US per county was 1041022. The minimum number of deaths was 0 and the maximum was 6304432. The average number of deaths of all the counties was 27594. There were 1930 counties.


4.1: Finding the amount of cases and deaths most populated counties (based on literature) in the US have and seeing if they match the top 10 cases and deaths by counties found in the previous code. #Most populated counties: Los Angeles, Cook, Harris, Maricopa, San Diego, Orange County, Miami-Dade, New York City, Riverside, Clark.
```{r}
#Los Angeles
LA <- ordered_data %>%
  filter(county == "Los Angeles" & state == "California")
print(LA[, c("county", "state", "total_cases", "total_deaths")])

Cook <- ordered_data %>%
  filter(county == "Cook" & state == "Illinois")
print(Cook[, c("county", "state", "total_cases", "total_deaths")])

Harris <- ordered_data %>%
  filter(county == "Harris"& state == "Texas")
print(Harris[, c("county", "state", "total_cases", "total_deaths")])

Maricopa <- ordered_data %>%
  filter(county == "Maricopa")
print(Maricopa[, c("county", "state", "total_cases", "total_deaths")])

SanDiego <- ordered_data %>%
  filter(county == "San Diego")
print(SanDiego[, c("county", "state", "total_cases", "total_deaths")])

OrangeCounty <- ordered_data %>%
  filter(county == "Orange"& state=="California" )
print(OrangeCounty[, c("county", "state", "total_cases", "total_deaths")])

MiamiDade <- ordered_data %>%
  filter(county == "Miami-Dade" & state == "Florida")
print(MiamiDade[, c("county", "state", "total_cases", "total_deaths")])

NYC <- ordered_data %>%
  filter(county == "New York City" & state == "New York")
print(NYC[, c("county", "state", "total_cases", "total_deaths")])

Riverside <- ordered_data %>%
  filter(county == "Riverside")
print(Riverside[, c("county", "state", "total_cases", "total_deaths")])

Clark <- ordered_data %>%
  filter(county == "Clark" & state == "Nevada")
print(Clark[, c("county", "state", "total_cases", "total_deaths")])

most_pop <- bind_rows(LA,Cook,Harris, Maricopa, SanDiego, OrangeCounty,MiamiDade,NYC,Riverside,Clark)
most_pop
ordered_most_pop_deaths<- arrange(most_pop, desc(total_deaths))
ordered_most_pop_cases <- arrange(most_pop, desc(total_cases))
ordered_most_pop_deaths
ordered_most_pop_deaths
```
4.2, 4.3: Plots
```{r}
#Bar Plot from most populated counties from literature vs tot_deaths
plot1 <- ggplot(most_pop, aes(x = county, y = total_deaths, fill = county)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Deaths by County",
       x = "County",
       y = "Total Deaths") +
  theme_minimal() +
  scale_fill_discrete(name = "County")
print(plot1)

#Scatter plot from most populated counties from literature vs tot_cases
ggplot(most_pop, aes(x = county, y = total_cases, color = county)) +
  geom_point(size = 3) +
  labs(title = "Scatterplot of Total Cases by County",
       x = "County",
       y = "Total Cases") +
  theme_minimal()

#scatter from top 10 deaths of county vs tot_deaths
ggplot(topdeaths, aes(x = county, y = total_deaths, color = county)) +
  geom_point(size = 1) +
  labs(title = "Total Cases Over Time by County",
       x = "county",
       y = "Total Cases",
       color = "County") +
  theme_minimal()

#bar from top 10 cases of county vs totcases
plot2 <- ggplot(topcases, aes(x = county, y = total_cases, fill = county)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Cases by County",
       x = "County",
       y = "Total Cases") +
  theme_minimal() +
  scale_fill_discrete(name = "County")
print(plot2)

#scatter total deaths vs state
ggplot(topdeaths, aes(x = state, y = total_deaths, color = state)) +
  geom_point(size = 1) +
  labs(title = "Total deaths Over Time by state",
       x = "State",
       y = "Total Deaths",
       color = "State") +
  theme_minimal()

#bar plot of total cases vs state
plot3 <- ggplot(topcases, aes(x = state, y = total_cases, fill = state)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Cases by state",
       x = "state",
       y = "Total Cases") +
  theme_minimal() +
  scale_fill_discrete(name = "State")
print(plot3)
```
4.4: Tables
```{r}
#Top ten deaths calculated from dataset
top_ten_deaths <- ordered_data %>%
  arrange(desc(total_deaths)) %>%
  head(10) %>%
  select(county,state, total_deaths)
print(top_ten_deaths)

#top ten cases found from dataset
top_ten_cases <- ordered_data %>%
  arrange(desc(total_cases))%>%
  head(10)%>%
  select(county,state, total_cases)
print(top_ten_cases)

#top ten deaths found from most populated counties from literature
top_ten_deaths_pop <- most_pop %>%
  arrange(desc(total_deaths))%>%
  select(county,state, total_deaths)
print(top_ten_deaths_pop)

#top ten cases found from most populated counties from literature
top_ten_cases_pop <- most_pop %>%
  arrange(desc(total_cases))%>%
  select(county,state, total_cases)
print(top_ten_cases_pop)
```
6.1: The total_deaths and total_cases do not match up by county and state. For example, the top ten counties with the most deaths are not the same counties that have the most cases. 
6.2:I addressed this problem by creating manipulating the original dataset to be ordered by total deaths and total cases by COVID-19 per county. 
6.3: From the analysis, I found that the top ten counties with the most deaths did not match up perfectly to the top ten counties with the most cases. For example the top 3 counties for total deaths and cases were the same however, after that the next 7 counties were different. I also looked into literature to see the top ten most populated counties in the US and see if those top ten counties are the same for total deaths and cases. For the most populated counties, the trend remained the same, the counties with the top deaths did not match up to the counties with the top cases. For example, Maricopa Arizona was 5th for top cases and 4th for top deaths.

6.4: Some implications of the analysis are that overall, populated counties where urban cities are will have a higher amount of cases and deaths by COVID-19. Some may have a higher case rate than death rate relative to others because of amount of resources in that county. A county with alot of people but not alot of resources, hospitals or money may have a higher amount of deaths than a county with a similar amount of cases and people with alot of resources, hospitals and money. 

6.5: Some limitations of my analysis are that population size of these counties were not included in the dataset. I used literature to see which counties in the United States were most populated. If population size per county was listed, then I could've introduced a new variable "population size" to make my analysis a bit more accurate.















