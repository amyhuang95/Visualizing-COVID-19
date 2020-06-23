# Indivudual project
# Topic: COVID-19 time series
# Date: March 25, 2020
# Data source: https://github.com/CSSEGISandData/COVID-19

# -------------------------------------------------Preparations-------------------------------------------------------- #

#install.packages(c("tidyverse","kableExtra"))
library(tidyverse)
library(knitr)
library(kableExtra)

# -------------------------------------------------Load Data----------------------------------------------------------- #
# retrieve each dataset from JHU
url_confirmed="https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/
csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
url_deaths="https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/
csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
url_recovered="https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/
csse_covid_19_time_series/time_series_covid19_recovered_global.csv"

# read in the csv files
raw.confirmed <- read_csv(url(url_confirmed))
raw.deaths <- read_csv(url(url_deaths))
raw.recovered <- read_csv(url(url_recovered))

# save in local computer
write.csv(raw.confirmed, file = ".\\data\\covid-19-confirmed-global.csv", row.names = FALSE)
write.csv(raw.deaths, file = ".\\data\\covid-19-deaths-global.csv", row.names = FALSE)
write.csv(raw.recovered, file = ".\\data\\covid-19-recovered-global.csv", row.names = FALSE)

# combine three dataset into a list since lapply passes through a list
raw <- list(raw.confirmed,raw.deaths,raw.recovered)
reshaped <- lapply(raw, function(x){
  cleaned <- x %>% 
    pivot_longer(cols = -c('Province/State','Country/Region','Lat','Long'), 
                 names_to = "date", 
                 values_to = "cases", 
                 values_drop_na = TRUE) %>% 
    mutate(date = as.Date(date,"%m/%d/%y")) %>%  
    filter(date <= "2020-05-06") %>% 
    group_by(`Country/Region`, date) %>% 
    summarize(cum_cases = sum(cases)) 
  cleaned
}
)

# unlist the results of lapply back to three data frames
global_confirmed <- as.data.frame(unlist(reshaped[1],recursive = FALSE))
global_deaths <- as.data.frame(unlist(reshaped[2], recursive = FALSE))
global_recovered <- as.data.frame(unlist(reshaped[3], recursive = FALSE))

# Overview of data
country <- data.frame(unique(global_confirmed$Country.Region))
summarize(country,n())
min(global_confirmed$date)
max(global_confirmed$date)

# --------------------------------------------------Analysis--------------------------------------------------------- #
### Plot 1: Cumulative confirmed cases over time (Worldwide)
# annotation
who_events <- tribble(
  ~ date, ~ event,
  "2020-01-30", "Global health emergency\ndeclared (Jan. 30)",
  "2020-03-11", "Pandemic declared \n(Mar. 11)") %>%
  mutate(date = as.Date(date))

# plotting
plt_global_confirmed <- global_confirmed %>% 
  group_by(date) %>% 
  summarise(cum_cases=sum(cum_cases)) %>% 
  ggplot(aes(x = date, y = cum_cases))+
  geom_line()+
  scale_y_continuous(labels = scales::unit_format(unit = "m", scale = 1e-6))+
  labs(
    title = "Cumulative confirmed cases of COVID-19 over time (worldwide)",
    x = "", 
    y = "Cumulative Cases (Million)",
    caption = "Data source: JHU CSSE.")+
  geom_vline(aes(xintercept=date), who_events,linetype = "dashed")+
  geom_text(aes(x = date+1,label = event), who_events, y = 3000000, hjust = 0)+
  theme_bw()
plt_global_confirmed

### Plot 2: Cumulative cases for each type of case over time (Worldwide)
# calculate confirmed, death, recovered cases, respectively
all_cases_overtime <- lapply(list(global_confirmed, global_recovered, global_deaths), function(x){
  cumulated <- x %>% 
    group_by(date) %>% 
    summarize(cum_cases = sum(cum_cases))
  cumulated
}
)

# make a line plot
plt_all_cases <- ggplot(
  bind_rows(all_cases_overtime, .id="df"), 
  aes(x = date, y = cum_cases, color = df)) + 
  geom_line() + 
  scale_y_continuous(labels = scales::unit_format(unit = "m", scale = 1e-6))+
  scale_color_discrete(
    name = "Type of case", 
    breaks = c("1", "2", "3"), 
    labels = c("Confirmed", "Recovered", "Death"))+
  labs(
    title = "Different types of cases of COVID-19 over time (worldwide)",
    x = "", 
    y = "Cumulative Cases (Million)",
    caption = "Data source: JHU CSSE.")+
  theme_bw()
plt_all_cases

### Plot 3: Confirmed cases - China vs the world
# subsetting & filtering data
china_vs_world <- global_confirmed %>%
  filter(date <= "2020-03-15") %>% 
  mutate(is_china = if_else(Country.Region == "China","China","Not China")) %>%
  group_by(is_china, date) %>%
  summarize(cum_cases = sum(cum_cases))

# annotation
who_events <- data.frame(
  "date" = c("2020-01-30","2020-03-11","2020-02-13"), 
  "event" = c("Global health\nemergency declared","Pandemic\ndeclared","China reporting change"), 
  "height" = c(75000,44000,60000)) %>% 
  mutate(date = as.Date(date))

# plot
plt_china_vs_world <- china_vs_world %>%
  ggplot()+
  geom_line(aes(x=date, y=cum_cases, group=is_china, color=is_china))+
  scale_y_continuous(
    labels = scales::unit_format(unit = "k", scale = 1e-3))+
  labs(
    title = "Confirmed COVID-19 cases in the early phase",
    subtitle = "China vs the rest of the world",
    x = "",
    y = "Cumulative Confirmed Cases",
    caption = "Data source: JHU CSSE.")+
  geom_vline(
    aes(xintercept=date), 
    who_events,
    linetype = "dashed")+
  geom_text(
    aes(x = date + 1, y = height, label = event), 
    who_events, 
    hjust = 0, 
    size = 3)+
  theme_bw()+
  theme(legend.title = element_blank())
plt_china_vs_world


### Plot 4: Confirmed cases - US vs the world
us_vs_world <- global_confirmed %>%
  filter(date >= "2020-03-03") %>% 
  mutate(is_us = if_else(Country.Region == "US","US","The rest of the world")) %>%
  group_by(is_us,date) %>%
  summarize(cum_cases = sum(cum_cases))

# annotation
us_events <- data.frame(
  "date" = c("2020-03-13","2020-03-26","2020-04-20"), 
  "event" = c("National\nemergency\ndeclared","Reached most cases\nin the world","States proposed\n plans to reopen"), 
  "height" = c(2000000,2000000,650000)
) %>% 
  mutate(date = as.Date(date))

# plotting
plt_us_vs_world <- us_vs_world %>%
  ggplot()+
  geom_line(aes(x = date, y = cum_cases, group = is_us, color = is_us))+
  labs(
    title = "Comfirmed COVID-19 cases since breakout in the US",
    subtitle = "US vs the rest of the world",
    x = "",
    y = "Cumulative Confirmed Cases",
    caption = "Data source: JHU CSSE.")+
  scale_y_continuous(labels = scales::unit_format(unit = "k", scale = 1e-3))+
  geom_vline(
    aes(xintercept=date), 
    us_events,
    linetype = "dashed")+
  geom_text(
    aes(x = date + 1, y = height, label = event), 
    us_events, 
    hjust = 0, 
    size = 3)+
  theme_bw()+
  theme(legend.title = element_blank())
plt_us_vs_world

### Plot 5: Linear prediction: US vs the world
trend_us_v_world <- us_vs_world %>%
  ggplot(aes(x = date, y = cum_cases))+
  geom_line()+
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed")+
  facet_wrap(~is_us, scales = "free")+
  scale_y_log10()+
  scale_y_continuous(labels = scales::unit_format(unit = "k", scale = 1e-3))+
  labs(
    title = "Linear prediction of confirmed COVID-19 cases",
    subtitle = "US vs the rest of the world",
    x = "",
    y = "(Log) Cumulative Cases",
    caption = "Method: Linear prediction\nData source: JHU CSSE.")+
  theme_bw()
trend_us_v_world

### Table 1: Top infected countries
global_confirmed %>% 
  group_by(Country.Region) %>%
  summarise(total_cases = max(cum_cases)) %>%
  arrange(desc(total_cases)) %>%
  head(n=10) %>% 
  knitr::kable(
    col.names = c("Region","Total confirmed cases"), 
    align = "rr", 
    caption = "Top infected regions") %>% 
  kable_styling(
    bootstrap_options = "striped", 
    full_width = F, 
    position = "center"
  )


### Table 2: Top impacted countries
global_deaths %>%
  group_by(Country.Region) %>%
  summarize(death_toll = max(cum_cases))%>%
  arrange(desc(death_toll)) %>%
  head(n=10) %>% 
  knitr::kable(col.names = c("Region","Death Toll"), 
               align = "rr",
               caption = "Top fatality countries") %>% 
  kable_styling(
    bootstrap_options = "striped", 
    full_width = F, 
    position = "center"
  )

confirmed_death <- merge(global_confirmed, global_deaths, 
                         by = c("Country.Region", "date"), 
                         suffixes = c(".confirmed",".deaths"))

death <- confirmed_death %>%
  group_by(Country.Region) %>%
  summarize(
    total_confirmed = max(cum_cases.confirmed),
    total_death = max(cum_cases.deaths),
    rate = total_death / total_confirmed)

summary(death$rate)

death %>% 
  arrange(desc(rate)) %>%
  head(n=10) %>% 
  knitr::kable(
    col.names = c("Region", "Total confirmed cases","Death toll", "Infection fatality rate"), 
    align = "rrrr", 
    caption = "Top infection fatality rate") %>% 
  kable_styling(
    bootstrap_options = "striped", 
    full_width = F, 
    position = "center"
  )

### Table 3: Top recovered countries
confirmed_recovered <- merge(global_confirmed, global_recovered, 
                             by = c("Country.Region", "date"), 
                             suffixes = c(".confirmed",".recovered"))

recovered <- confirmed_recovered %>%
  group_by(Country.Region) %>%
  summarize(
    total_confirmed = max(cum_cases.confirmed),
    total_recovered = max(cum_cases.recovered),
    rate = total_recovered / total_confirmed)

summary(recovered$rate)

recovered %>%
  group_by(Country.Region) %>%
  arrange(desc(rate)) %>%
  head(n=10) %>% 
  knitr::kable(
    col.names = c("Region","Total confirmed cases", "Total recovered cases", "Infection recovery rate"), 
    align = "rrrr", 
    caption = "Top recovered countries") %>% 
  kable_styling(
    bootstrap_options = "striped", 
    full_width = F, 
    position = "center"
  )








