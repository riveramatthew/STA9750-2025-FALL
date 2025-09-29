# Load libraries
library(ggplot2)
library(dplyr)
library(lubridate)

### 1. Education & Equity - Scatter Plot
# Placeholder data
set.seed(123)
education <- data.frame(
  income = runif(100, 20000, 100000),      # household income
  achievement = rnorm(100, 75, 10)         # test scores
)

ggplot(education, aes(x = income, y = achievement)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Income vs. Student Achievement",
       x = "Median Household Income",
       y = "Average Test Score")

### 2. Health & Society - Choropleth Map (mock with counties)
# NOTE: Replace with real county health data
library(maps)
county_map <- map_data("county")

# Fake health metric
set.seed(321)
county_health <- county_map %>%
  group_by(region, subregion) %>%
  summarise(health_score = runif(1, 60, 90), .groups = "drop")

ggplot(county_map, aes(long, lat, group = group)) +
  geom_polygon(fill = "grey90", color = "white") +
  geom_polygon(data = left_join(county_map, county_health,
                                by = c("region", "subregion")),
               aes(fill = health_score),
               color = "white") +
  scale_fill_viridis_c() +
  labs(title = "County Health Score (Placeholder)",
       fill = "Score")

### 3. Transportation & Urban Life - Time Series
# Placeholder ride data
rides <- data.frame(
  date = seq.Date(from = as.Date("2023-01-01"),
                  to = as.Date("2023-03-31"),
                  by = "day"),
  rides = round(runif(90, 50000, 120000))
)

ggplot(rides, aes(x = date, y = rides)) +
  geom_line(color = "darkorange") +
  labs(title = "Daily Rideshare Demand (Placeholder)",
       x = "Date",
       y = "Number of Rides")
