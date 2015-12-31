#
# Name: mapping_acs_data_tracts.R
# Author: pattyf@berkeley.edu
# Last updated: 12/30/2015
#
# This script follows the blog post at:
# Help page for acs14lite
# http://rpubs.com/walkerke/acs14lite
# Help page for tigris package
# https://rpubs.com/walkerke/tigris 
# help page for acs plus
# http://zevross.com/blog/2015/10/14/manipulating-and-mapping-us-census-data-in-r-using-the-acs-tigris-and-leaflet-packages-3/

# clean start - remove any objects in the enviroment
rm(list = ls())

# Install & Load the R libraries that we will use
# if they are not already on your system
# install.packages(c('dplyr', 'ggplot2', 'ggmap', 'devtools'))
# devtools::install_github('walkerke/acs14lite')
# devtools::install_github('walkerke/tigris')
# devtools::install_github("becarioprecario/cartodb-r/CartoDB", dep = TRUE)

#load the libraries
library(acs14lite) # used to fetch ACS 2010-2014 data from the Census API (requires API key)
library(tigris) # used to fetch TIGER data (shapefiles)
library(dplyr) # used to process ACS data
library(ggplot2) # to make maps with ggplot
library(ggmap) # for mapping using remote basemaps like Google Maps data

# Set the current working directory
setwd("~/Documents/Dlab/dlab_workshops/rcensus")

# Read in my census API key
# The Census API (application programming interface) lets programmers directly access
# census data.  To use the API you must sign up for a key at:
# http://api.census.gov/data/key_signup.html
# You can directly assign your Census API key to a variable
# my_census_api_key <- "f2d6666666666666666666666666" # not a real key
# But it is safer to store that line in a file and then
# read in, or source, the file when you need to use the key
source("keys/census_api_key.R")

# Set the API key for the acs14lite library
set_api_key(my_census_api_key) 
#api.key.install(my_census_api_key) # this is how you would do it for tigris package

# ##############################################
# Indentify the ACS variable of interest
# ##############################################
#
# The ACS variable must be available for your geography of interest. 
# If it is not then null values will be returned for the variable.
# The following variable exists for county, tract and block group aggregations in 2010-2014
## B17021: Poverty Status of Individuals in the Past 12 months by Living Arrangement
## B17021_001: count of people for whom poverty status has been determined (the sample)
## B17021_002: count of those people whose income in the past 12 months is below poverty
## E suffix = variable estimate
## M suffix = margin of error

# You can use the following URL to explore this variable on the Census American Factfinder Website
# You will need select Add Geographies to view data for your geography of interest (e.g., tract, blockgroup)
# http://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?pid=ACS_14_5YR_B17021&prodType=table

# You can view these data in your browser by putting the following URLs in your web browser address bar
# Note you will need to add your census API key
# http://api.census.gov/data/2014/acs5?get=NAME,B17021_001E&for=tract:*&in=state:06+county:075&key=your_census_apikey

# Use the acs14lite R library to fetch ACS 2010-2014 poverty data for San Francisco census tracts
# Available geographies for exploring an ACS variable with the acs14lite package
# include: 'us', 'region', 'division', 'state', 'county', 'tract', 'block group'
# For package details see:
# ??acs14lite

sf_poverty <- acs14(geography = 'tract', state = 'CA', county = 'San Francisco', 
                    variable = c('B17021_001E', 'B17021_001M', 'B17021_002E', 'B17021_002M'))

# Look at the data
head(sf_poverty)

# Use the dplyr mutate function to convert raw counts of people below poverty to percent of
# total people who where surveyed. Ditto for the margin of error
# Note the cool R pipe syntax (%>%) for feeding the ouput of one command to the next.
# You many need to upgrade your version of R to enable this syntax or install the magrittr package.

sf_poverty14 <- sf_poverty %>%
  mutate(geoid = paste0(state, county, tract),
         pctpov = round(100 * (B17021_002E / B17021_001E), 1),
         moepov = round(100 * (moe_prop(B17021_002E, B17021_001E, B17021_002M, B17021_001M)), 1)) %>%
  select(geoid, pctpov, moepov)

# You can also rewrite this code without pipes.
## Less fashionable, a bit more repetitive BUT easier to read and debug
#sf_poverty14_2 <- mutate(sf_poverty,
#       geoid = paste0(state, county, tract),
#       pctpov = round(100 * (B17021_002E / B17021_001E), 1),
#       moepov = round(100 * (moe_prop(B17021_002E, B17021_001E, B17021_002M, B17021_001M)), 1))
# sf_poverty14_2 <-  select(sf_poverty14_2,geoid, pctpov, moepov)
# identical(sf_poverty14, sf_poverty14_2)

# Look at the downloaded ACS data:
head(sf_poverty14)

# Create a map of the data
# Use the TIGRIS package to download TIGER geographic data in the form of ESRI shapefiles.
# Tigris defaults to 2014 shapefiles
# The cb=TRUE will retrieve the more generalized tiger data
# The tigris functions for retrieving geographic data are named after the type of data
# that they retrieve, eg tracts, block_groups, counties, states
# Use ??tigris for details and package index

sf_tracts <- tracts('CA', 'San Francisco', cb=TRUE)
# sf_tracts is a spatial data object of class SpatialPolygonsDataFrame
# For details see ?"SpatialPolygonsDataFrame-class"

# Use the acs14lite function geo_join
# to join the ACS data (sf_poverty14) to the tracts spatial data (sf_tracts)
# Both data objects have a geoid/GEOID value (only the column names vary by capitalization)

sf_tracts2 <- geo_join(sf_tracts, sf_poverty14, "GEOID", "geoid")

# Look at the data
class(sf_tracts2)
str(sf_tracts2)
str(sf_tracts2@data)

# Remove any tracts with no data for the ACS variable
# We do this for SF only because the Farralon Islands are so far off the coast
# of SF that they mess up the plot when we create a map of the data
sf_tracts2 <- sf_tracts2[!is.na(sf_tracts2$pctpov),]

# Use the base plotting system to plot the sf_tracts2 data as a map
# This requires the sp library to manage and plot spatial data
library(sp)
plot(sf_tracts2)

# Alternatively, create a map with a legend using ggplot2
library(ggplot2)

## First, use fortify() to make the census data an object that ggplot can map.
ggplotData <- fortify(sf_tracts2, data=sf_tracts2@data, region="geoid")
head(ggplotData) # take a look at the result of the fortify command

## Join the ACS data back to the fortified data frame
ggplotData <- merge(ggplotData,sf_tracts2@data, by.x="id", by.y="geoid")
head(ggplotData)

## Map it - color regions by census variable
p1 <-ggplot() +
  geom_polygon(data = ggplotData, aes(x = long, y = lat, group = group, 
                                      fill = pctpov), color = 'NA') +
  coord_map()
p1

# or emphasize areas of highest poverty

p2 <- ggplot() +
  geom_polygon(data = ggplotData, aes(x = long, y = lat, group = group, fill = pctpov), alpha=0.75) +
  scale_fill_distiller(palette = "Reds", name="Percent") +
  guides(fill = guide_legend(reverse = TRUE)) +
  ggtitle("Percent of Individuals below Poverty Level\n ACS 2010-2014 Data") +
  theme_classic() +
  coord_map()
p2

# Add a basemap using ggmap
sf_basemap <-get_map('San Francisco', zoom=12)

p3 <- ggmap(sf_basemap) +
  geom_polygon(data = ggplotData, aes(x = long, y = lat, group = group, fill = pctpov), alpha=0.75) +
  scale_fill_distiller(palette = "Reds", name="Percent") +
  guides(fill = guide_legend(reverse = TRUE)) +
  ggtitle("Percent of Individuals below Poverty Level\n ACS 2010-2014 Data") +
  theme_nothing(legend=TRUE) +
  coord_map()
p3

#save the map using ggplot's ggsave function
ggsave(plot=p3, filename="sf_poverty.png")

# COMPARE ACS 5 Year 2010-2014 data with 2006-2010 data
# Two issues:
# 1) 2005-2009 5yr ACS data are available but not via the Census API 
# 2) acs14lite is only for 2010-2014 5yr ACS data
# Work around
# 1) We will use 2006-2010 (one year overlap)
# 2) We will look at the acs14lite functions and make small changes to grab these data.

# Here is the acs14lite load_data function
# cut and paste into this script so we can call it directly
# note: comments added
load_data <- function(api_call) {
  df <- data.frame(jsonlite::fromJSON(api_call), stringsAsFactors = FALSE)
  colnames(df) <- df[1, ] # the first row has the column names - so extract and rename the columns
  df <- df[-1, ] # now remove the first row
  rownames(df) <- NULL # remove rownames
  return(df)
}

# Fetch the ACS data
call <- paste0("http://api.census.gov/data/2010/acs5?get=NAME,B17021_001E,B17021_001M,B17021_002E,B17021_002M&for=tract:*&in=state:06+county:075&key=", my_census_api_key)
sf_poverty10 <- load_data(call)

# The API call returns our ACS data as characters/strings not numbers 
# so convert using as.numeric function
myvars <- c('B17021_001E', 'B17021_001M', 'B17021_002E', 'B17021_002M')
sf_poverty10[,myvars] <- apply(sf_poverty10[,myvars], 2, function(x) as.numeric(x))

# Reformat our variables of interest as percents
sf_poverty10b <- sf_poverty10 %>%
  mutate(geoid = paste0(state, county, tract),
         pctpov10 = round(100 * (B17021_002E / B17021_001E), 1),
         moepov10 = round(100 * (moe_prop(B17021_002E, B17021_001E, B17021_002M, B17021_001M)), 1)) %>%
  select(geoid, pctpov10, moepov10)

# Remove rows with null values
sf_poverty10b <- sf_poverty10b[!is.na(sf_poverty10b$pctpov10),] 

# Join to the ggplotData dataframe with the 2010-2014 ACS data
ggplotData <- merge(ggplotData, sf_poverty10b, by.x="id", by.y="geoid") # add to our ggplotData data frame

# ggplot it
p4 <- ggplot() +
  geom_polygon(data = ggplotData , aes(x=long, y=lat, group = group, fill=pctpov10), color="NA") +
  scale_fill_gradientn(colours = c("lightgrey","grey","red"), values = c(0,0.15,.25,.5,.75,1)) + coord_map()
p4

## MAP PERCENT CHANGE
ggplotData$pct_change <- ggplotData$pctpov - ggplotData$pctpov10
p5 <- ggplot() +
  geom_polygon(data = ggplotData , aes(x=long, y=lat, group = group, fill=pct_change), color="NA") +
  scale_fill_gradientn(colours = c("blue","grey","red"),  values = c(0,0.15,.25,.5,.75,1)) + coord_map()
p5

# Compute margin of error for percent change
#http://www2.census.gov/programs-surveys/acs/tech_docs/statistical_testing/2013StatisticalTesting3and5.pdf
moe_pctchange <- function(moe_period1, moe_period2,period_len=5, overlap_len=1) {
  #Standard Error = Margin of Error / Z
  # where Z = 1.645 for 2006
  Z <- 1.645
  SE_period1 <- moe_period1 / Z
  SE_period2 <- moe_period2 / Z
  C <- overlap_len/period_len # number of years of overlap divided by number of years in period
   SE_change = sqrt(1 - C) * sqrt(SE_period1 + SE_period2)
   MOE_change = SE_change * Z
   return(MOE_change)
}

ggplotData$moe_pctchange <- mapply(moe_pctchange,ggplotData$pctpov,ggplotData$pctpov10)

# CartoDB.com provides an easy platform for creating interactive online maps 
# of our ACS data
# 
# devtools::install_github("becarioprecario/cartodb-r/CartoDB", dep = TRUE)
# 

library(CartoDB) 
# Click on the heart icon in your cartodb home page to get your API Key
cdb_username <- 'my_cartodb_username'
cdb_apikey <- 'my_cartodb_apikey'
cartodb(cdb_username, cdb_apikey)
#r2cartodb(sf_tracts2, 'sf_poverty_tracts2')

#Because we added the following vars to ggplot data frame we need to also add to the
# SpatialPolygonsDataFrame in order to view in cartodb
sf_tracts_poverty <- geo_join(sf_tracts2, sf_poverty10b, "GEOID", "geoid")
sf_tracts_poverty$pct_change <- sf_tracts_poverty$pctpov - sf_tracts_poverty$pctpov10
#r2cartodb(sf_tracts_poverty, 'sf_poverty_tracts')

# Now, head to your CartoDB account to style your map!

# #######################################################################
# Re-run all that with block group data instead of tract level data!
# #######################################################################
# Some of the lines that need to change:
## 1) sf_poverty_bg <- acs14(geography = 'block group', state = 'CA', county = 'San Francisco', ....
## 2) mutate(geoid = paste0(state, county, tract, `block group`),
## 3) sf_blockgroups <- block_groups('CA', 'San Francisco', cb=TRUE)
# #######################################################################

