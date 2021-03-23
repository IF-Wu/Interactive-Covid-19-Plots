# Packages used in this assignment
library(httr)
library(RCurl)
library(XML)
library(RJSONIO)
library(plotly)
library(ggplot2)
library(htmlwidgets)
library(maps)
library(ggmap)
library(tidyverse)
library(gganimate)
library(transformr)
library(gifski)
library(png)
library(animation)

# Get the Covid-19 data from New York Times
# The URL for the JSON changes each day. So today's URL, we want to find it in the current day's page.
# We get the top-level page
ny <- htmlParse(GET("https://www.nytimes.com/interactive/2020/us/coronavirus-us-cases.html?action=click&module=Top%20Stories&pgtype=Homepage"))
# Then we find the HTML elements that have an immediate child text node that contains the string USA.json.
js <- xpathSApply(ny, "//*[contains(./text(), 'USA.json')]", xmlValue)
#These are <script> elements containing JavaScript, not JSON.
# But we can find the URL with a regular expression in these
u <- gsub('.*"(https://[^"]+USA.json)".*', "\\1", js)
u <- unique(u)
# There is only 1 URL repeated multiple times.
# So now we have these
tt <- GET(u, verbose = TRUE, followlocation = TRUE)
tt <- rawToChar(tt$content)
us <- fromJSON(tt)
length(us$data)

# Get California counties
CA_index <- grep("California", us$data)
CA_geoid <- us$data[[CA_index]]$geoid

CA_counties <- list()
for(i in 1:length(us$data)){
  if(length(grep(CA_geoid, us$data[[i]]$hierarchy)) > 0){
    CA_counties[[length(CA_counties) + 1]] = us$data[[i]]
  }
}

# Remove the county that name is Unknown
CA_counties[[grep("Unknown", CA_counties)]] <- NULL

length(CA_counties)

# Functions to get the daily cases and pro-rated value for daily cases
Daily_cases <- function(county){
  daily = c()
  case = county$cases
  daily = append(daily, case[1])
  for(i in 2:length(case)){
    dif = case[i] - case[i - 1]
    daily = append(daily, dif)
  }
  return(daily)
}

Daily_deaths <- function(county){
  daily = c()
  death = county$deaths
  daily = append(daily, death[1])
  for(i in 2:length(death)){
    dif = death[i] - death[i - 1]
    daily = append(daily, dif)
  }
  return(daily)
}

PR_cases <- function(county){
  case = Daily_cases(county)
  pop = county$population
  pr = case / pop
  return(pr)
}

PR_deaths <- function(county){
  death = Daily_deaths(county)
  pop = county$population
  pr = death / pop
  return(pr)
}

# Create a data frame for all covid data in CA
start_date <- unlist(unique(lapply(CA_counties, "[[", "range")))[1]
end_date <- unlist(unique(lapply(CA_counties, "[[", "range")))[2]
all_date <- seq(as.Date(start_date), as.Date(end_date), by = "day")
all_counties <- unlist((lapply(CA_counties, "[[", "display_name")))
daily_cases <- unlist(lapply(CA_counties, Daily_cases))
daily_deaths <- unlist(lapply(CA_counties, Daily_deaths))
daily_cases_PR <- unlist(lapply(CA_counties, PR_cases))
daily_deaths_PR <- unlist(lapply(CA_counties, PR_deaths))
all_cases <- unlist(lapply(CA_counties, "[[", "cases"))
all_deaths <- unlist(lapply(CA_counties, "[[", "deaths"))
all_cases_PR <- unlist(lapply(CA_counties, function(x) x$cases/x$population))
all_deaths_PR <- unlist(lapply(CA_counties, function(x) x$deaths/x$population))
CA_covid_data <- data.frame(Date = rep(all_date, length(CA_counties)), 
                            County = rep(all_counties, each = length(all_date)),
                            Daily_Cases = daily_cases,
                            Daily_Cases_PR = daily_cases_PR * 100,
                            Daily_Deaths = daily_deaths,
                            Daily_Deaths_PR = daily_deaths_PR * 100,
                            Cumulative_Cases = all_cases,
                            Cumulative_Cases_PR = all_cases_PR * 100,
                            Cumulative_Deaths = all_deaths,
                            Cumulative_Deaths_PR = all_deaths_PR * 100)

# Time series plot for Covid cases for each county in CA
text <- paste("Cumulative_Cases_Pro_rate:", CA_covid_data$Cumulative_Cases_PR, "%", "<br>",
              "Cumulative_Deaths:", CA_covid_data$Cumulative_Deaths, "<br>",
              "Cumulative_Deaths_Pro_rate:", CA_covid_data$Cumulative_Deaths_PR, "%", "<br>",
              "Daily_Cases:", CA_covid_data$Daily_Cases, "<br>",
              "Daily_Cases_Pro_rate:", CA_covid_data$Daily_Cases_PR, "%", "<br>",
              "Daily_Deaths:", CA_covid_data$Daily_Deaths, "<br>",
              "Daily_Deaths_Pro_rate:", CA_covid_data$Daily_Deaths_PR, "%")

covid <- highlight_key(CA_covid_data, ~County, "Highlight one or more county")
time_series_cases <- ggplot(covid, aes(x = Date, y = Cumulative_Cases, text = text)) + 
                       geom_point(aes(color = County))

plty <- ggplotly(time_series_cases) %>% highlight(selectize = TRUE, persistent = TRUE, off = 'plotly_doubleclick')
saveWidget(plty, "HW5.html", selfcontained = FALSE)

# Map of CA counties
# First do some basic work -- get the geo data and merge it with our covid data
states <- map_data("state")
ca_df <- subset(states, region == "california")
counties <- map_data("county")
ca_county <- subset(counties, region == "california")
CA_covid_data <- CA_covid_data %>% mutate(subregion = tolower(County))
covid_map <- inner_join(ca_county, CA_covid_data, by = "subregion")
covid_map <- covid_map %>% mutate(xloc = -122.5, yloc = 33)

# Use gganimate to draw a animation map
ca_covid_map <- ggplot() + 
  theme_void() + 
  geom_polygon(data = covid_map, aes(x = long, y = lat, fill = Cumulative_Cases_PR, group = group), color = "white") + 
  geom_polygon(data = ca_df, aes(x = long, y = lat, group = group), color = "black", fill = NA) +
  scale_fill_gradient(low = "lightyellow", high = "red4", na.value = "grey80") +
  ggtitle("Map for Covid Cases in California") +
  labs(fill = "Pro-rated value for cumulative cases", caption = "Introduction: The map shows the time-varying pro-rated values
for cumulative cases relative to the county's population in California.") +
  theme(plot.caption = element_text(hjust = 0, face = "italic")) +
  geom_text(data = covid_map, aes(x = xloc, y = yloc, label = Date), check_overlap = TRUE, size = 5, fontface = "bold") +
  transition_states(Date)

animated_map <- animate(ca_covid_map, nframes = 2*length(unique(covid_map$Date)))

# Save the map as a gif file.
anim_save("covid-map.gif", animation = animated_map)