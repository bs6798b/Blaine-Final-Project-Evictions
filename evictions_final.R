# Blaine Smith
# Documentation for Evictions Research and Data Analysis

library(tidyverse)
library(dplyr)
library(tidyr)
library(rgdal)
library(geojsonio)
library(sp)
library(leaflet)

# I am using the Princeton Eviction Lab which can be found at
# https://evictionlab.org/get-the-data/. There is state- and national-level
# data broken up by state, county, city, tract, and block-group. The census
# tract data is used for the purpose of this analysis.
tracts <- read_csv("~/Desktop/final_proj/tracts.csv")

# To  make the tracts data more syntactically compatible with R, I rename columns 
# to replace "-" with "_".
names(tracts) <- c("GEOID", "year", "name", "parent_location", "population", "poverty_rate", "renter_occupied_households", "pct_renter_occupied", "median_gross_rent", "median_household_income", "median_property_value", "rent_burden", "pct_white", "pct_af_am", "pct_hispanic", "pct_am_ind", "pct_asian", "pct_nh_pi", "pct_multiple", "pct_other", "eviction_filings", "evictions", "eviction_rate", "eviction_filing_rate", "low_flag", "imputed", "subbed")

# Because most spatially-driven policymaking in DC is done by ward, I tranformed
# the data by adding ward to each geocoded observation to be able to make more 
# meaningful inferences. I did this using https://censusreporter.org data to
# place census tracts spatially within wards. For the less than ten census 
# tracts that had area in more two wards, I assigned them the ward containging 
# the most surface area. This information is in wards.csv which contains only 
# two variables: name (census tracts) and ward. I left join the two dfs by 
# "name", functionally adding ward to each observation in tracts.
wards <- read_csv("~/Desktop/final_proj/wards.csv")
tracts2 <- left_join(wards, tracts, by = "name")

# Using geojsonio:geojson_read, I am able to read the .geojson file of my
# census data into R. Now it is compatible for use with other packages that
# allow the configuration of spatial data.
tracts_json <- geojson_read("~/Desktop/final_proj/tracts.geojson", what = "sp")

# I found that leaflet allowed me the easiest access to making choropleth maps
# so I use leaflet(tracts_json) as my basemap.
m <- leaflet(tracts_json)

# The following subsets are useful for over-time comparison of shifting
# demographics and more useful data visualization than by the full tracts
# df which includes each geolocation for each year.
Y2000 <- subset(tracts2, year == '2000')
Y2001 <- subset(tracts2, year == '2001')
Y2002 <- subset(tracts2, year == '2002')
Y2003 <- subset(tracts2, year == '2003')
Y2004 <- subset(tracts2, year == '2004')
Y2005 <- subset(tracts2, year == '2001')
Y2006 <- subset(tracts2, year == '2006')
Y2007 <- subset(tracts2, year == '2007')
Y2008 <- subset(tracts2, year == '2008')
Y2009 <- subset(tracts2, year == '2009')
Y2010 <- subset(tracts2, year == '2010')
Y2011 <- subset(tracts2, year == '2011')
Y2012 <- subset(tracts2, year == '2012')
Y2013 <- subset(tracts2, year == '2013')
Y2014 <- subset(tracts2, year == '2014')
Y2015 <- subset(tracts2, year == '2015')
Y2016 <- subset(tracts2, year == '2016')

# Like the subsets by year, the following subsets by ward aid the analysis
# of spatial disparities in evictions.
ward1 <- subset(tracts2, ward == 'One')
ward2 <- subset(tracts2, ward == 'Two')
ward3 <- subset(tracts2, ward == 'Three')
ward4 <- subset(tracts2, ward == 'Four')
ward5 <- subset(tracts2, ward == 'Five')
ward6 <- subset(tracts2, ward == 'Six')
ward7 <- subset(tracts2, ward == 'Seven')
ward8 <- subset(tracts2, ward == 'Eight')

## Data Analysis

# % Black by Census Tract in 2000
m %>% addPolygons()
bins_aa <- c(0, 15, 30, 45, 60, 70, 80, 90, 100)
pal1 <- colorBin("YlOrRd", domain = tracts_json$paa.00, bins = bins_aa)
m %>% addPolygons(
  fillColor = ~pal1(paa.00),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.7
) %>% 
  addLegend(pal = pal1, values = bins_aa, group = "addPolygons", position = "bottomright", title = "% Black") 

# % Black by Census Tract in 2016
pal2 <- colorBin("YlOrRd", domain = tracts_json$paa.16, bins = bins_aa)
m %>% addPolygons(
  fillColor = ~pal2(paa.16),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.7
) %>% 
  addLegend(pal = pal2, values = bins_aa, group = "addPolygons", position = "bottomright", title = "% Black")

# % Eviction Rate by Census Tract in 2006
bins_er <- c(0, 1, 2, 4, 6, 8, 10, Inf)
pal3 <- colorBin("YlOrRd", domain = tracts_json$er.06, bins = bins_er)
m %>% addPolygons(
  fillColor = ~pal3(er.06),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.7
) %>% 
  addLegend(pal = pal3, values = bins_er, group = "addPolygons", position = "bottomright", title = "Eviction Rate")

# % Eviction Rate by Census Tract in 2012
pal4 <- colorBin("YlOrRd", domain = tracts_json$er.16, bins = bins_er)
m %>% addPolygons(
  fillColor = ~pal4(er.10),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.7
) %>% 
  addLegend(pal = pal4, values = bins_er, group = "addPolygons", position = "bottomright", title = "Eviction Rate")


# % Eviction Rate by Census Tract in 2016
pal5 <- colorBin("YlOrRd", domain = tracts_json$er.16, bins = bins_er)
m %>% addPolygons(
  fillColor = ~pal5(er.16),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.7
) %>% 
  addLegend(pal = pal5, values = bins_er, group = "addPolygons", position = "bottomright", title = "Eviction Rate")

# Poverty Rate by Census Tract in 2000
pal6 <- colorBin("YlOrRd", domain = tracts_json$pr.00, bins = bins_pr)
bins_pr <- c(0, 5, 10, 20, 25, 30, 35, 40, Inf)
m %>% addPolygons(
  fillColor = ~pal6(pr.00),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.7
) %>% 
  addLegend(pal = pal6, values = bins_pr, group = "addPolygons", position = "bottomright", title = "Poverty Rate")

# Poverty Rate by Census Tract in 2016
pal7 <- colorBin("YlOrRd", domain = tracts_json$pr.16, bins = bins_pr)
m %>% addPolygons(
  fillColor = ~pal7(pr.16),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.7
) %>% 
  addLegend(pal = pal7, values = bins_pr, group = "addPolygons", position = "bottomright", title = "Poverty Rate")

black_by_ward_2000 <- Y2000 %>% 
  group_by(ward) %>% 
  summarise(mean_pct_black = mean(pct_af_am, na.rm = TRUE)) %>% 
  arrange(desc(mean_pct_black))
as.tibble(black_by_ward_2000)

black_by_ward_2016 <- Y2016 %>% 
  group_by(ward) %>% 
  summarise(mean_pct_black = mean(pct_af_am, na.rm = TRUE)) %>% 
  arrange(desc(mean_pct_black))
as.tibble(black_by_ward_2016)

poverty_by_ward_2000 <- Y2000 %>% 
  group_by(ward) %>% 
  summarise(mean_poverty_rate = mean(poverty_rate, na.rm = TRUE)) %>% 
  arrange(desc(mean_poverty_rate))
as.tibble(poverty_by_ward_2000)

poverty_by_ward_2016 <- Y2016 %>% 
  group_by(ward) %>% 
  summarise(mean_poverty_rate = mean(poverty_rate, na.rm = TRUE)) %>% 
  arrange(desc(mean_poverty_rate))
as.tibble(poverty_by_ward_2016)

pop_by_ward_2000 <- Y2000 %>% 
  group_by(ward) %>% 
  summarise(population = mean(population, na.rm = TRUE)) %>% 
  arrange(desc(population))
as.tibble(pop_by_ward_2000)

pop_by_ward_2016 <- Y2016 %>% 
  group_by(ward) %>% 
  summarise(population = mean(population, na.rm = TRUE)) %>% 
  arrange(desc(population))
as.tibble(pop_by_ward_2016)

eviction_rate_comp <- tracts2 %>% 
  group_by(ward) %>% 
  summarise(mean_ev_rate = mean(eviction_rate, na.rm = TRUE)) %>% 
  arrange(desc(mean_ev_rate))
as.tibble(eviction_rate_comp)

ev_rate_diff <- mean(Y2016$'eviction_rate', na.rm = TRUE) - mean(Y2006$'eviction_rate', na.rm = TRUE)

# Eviction Rate by Poverty Rate and % Black
ggplot(data = tracts2) + 
  geom_point(aes(x = poverty_rate, y = eviction_rate, color = pct_af_am), size = .5, na.rm = TRUE) +
  labs(title = "Eviction Rate by Poverty Rate and % Black", x = "Poverty Rate", y = "Eviction Rate", color = "% Black")

# Eviction Rate by % Black and Ward
ggplot(data = tracts2) + 
  geom_point(aes(x = pct_af_am, y = eviction_rate, color = ward), size = .75, na.rm = TRUE) +
  labs(title = "Eviction Rate by % Black and Ward", x = "% Black", y = "Eviction Rate", color = "Ward")

burden_by_race <- tracts2 %>% 
  group_by(pct_af_am) %>% 
  summarise(mean_burden = mean(rent_burden, na.rm = TRUE)) %>% 
  arrange(desc(mean_burden))
plot(burden_by_race)

fit <- lm(eviction_rate ~ pct_af_am + poverty_rate + ward, data = tracts2)
summary(fit)

fit_black <- lm(eviction_rate ~ pct_af_am, data = tracts2)
summary(fit_black)

fit_poverty <- lm(eviction_rate ~ poverty_rate, data = tracts2)
summary(fit_poverty)

fit_ward <- lm(eviction_rate ~ ward, data = tracts2)
summary(fit_ward)