
path <- "C:/Users/eshee/Box Sync/Documents/Virginia Tech DSPG SDAL/R/ari_base_char/"
setwd(path)


library(ggplot2)
#install.packages("tigris")
library(tigris) # get county and state shapefiles
library(dplyr) # join data frames
#install.packages("acs")
library(acs) # get a sample of ACS income data to plot
#install.packages("viridis")
library(viridis) # viridis color palette
#install.packages("rgdal")
#install.packages("rgeos")
library(rgdal)
library(rgeos)
#install.packages("mapproj")
library(mapproj)
#install.packages("tidycensus")
library(tidycensus)
library(readxl)

county_shape <- counties()
state_shape <- states()

# For plotting purposes, exclude all states not in continental US (look in state_shape@data$NAME)
state_exclude <- c("Puerto Rico","American Samoa","United States Virgin Islands","Guam",
                   "Commonwealth of the Northern Mariana Islands","Hawaii","Alaska")
state_shape <- state_shape[ !(state_shape@data$NAME %in% state_exclude), ]
state_include <- state_shape@data$STATEFP
county_shape <- county_shape[ county_shape@data$STATEFP %in% state_include, ]

#Must convert from "SpatialPolygonsDataFrame" to "sp"


# 'fortify' shapefiles to create data frames we can use in ggplot
#  Install these packages first: rgdal, rgeos and ggplot2

county_df <- ggplot2::fortify(county_shape,region="GEOID") # could use other identifiers instead, e.g. region="COUNTYFP" or region="NAMELSAD"
state_df <- ggplot2::fortify(state_shape,region="NAME")



# plot county and state boundaries using geom_polygon in ggplot
ggplot() +
  geom_polygon(data=county_df, aes(x=long, y=lat, group=group), color="black", fill=NA, size=0.3) +
  geom_polygon(data=state_df, aes(x=long, y=lat, group=group), color="grey30", fill=NA) +
  theme_void() +
  coord_map()


# Install sheen@vt.edu Census API key
# census_api_key('5fd2ac6c093dfcb983b9399244d00ebdd818c35b', install = TRUE)


# attach some county-level data to county_df
# as an example, take an ACS table; median income by county
acs_income_by_county <- get_acs(geography="county",state=state_include,year=2016,span=5,
        table="B19013",cache_table=TRUE, output="wide",summary_var=NULL)

acs_income_by_county$GEOID
# join median income to county_df; joins the "id" column in county_df to the "GEOID" column in acs_income_by_county
county_df2 <- county_df %>% left_join(acs_income_by_county, by=c("id"="GEOID"))

# color counties by median income
income_plot <- ggplot() +
  geom_polygon(data=county_df2, aes(x=long, y=lat, group=group, fill=B19013_001E), color="grey70", size=0.3) +
  geom_polygon(data=state_df, aes(x=long, y=lat, group=group), color="grey30", fill=NA) +
  theme_void() +
  coord_map()

# change the color scheme and adjust the legend as needed (including direction, text size, etc; see guide_legend)
income_plot +
  scale_fill_viridis(guide = guide_legend(direction="vertical",
                                          title="Median Income"))


##########################################################
###    Emily: attach County Health records data        ###
##########################################################
library(readxl)
countyHealth <- read_excel("2018CountyHealthRankingsData_Cleaned.xlsx",
                           sheet = "Ranked Measure Data",
                           range = "A2:FH3144")
colnames(countyHealth)
#Must merge countyHealth$excess_drink column to county_df based on FIPS = GEOID
countyHealth$FIPS
acs_income_by_county$GEOID

# join median income to county_df; joins the "id" column in county_df to the "GEOID" column in acs_income_by_county
county_df3 <- county_df2 %>% left_join(countyHealth, by=c("id"="FIPS"))

# color counties by % excessive drinkers
drink_plot <- ggplot() +
  geom_polygon(data=county_df3, aes(x=long, y=lat, group=group, fill=excess_drink), color="grey70", size=0.3) +
  geom_polygon(data=state_df, aes(x=long, y=lat, group=group), color="grey30", fill=NA) +
  coord_map() +
  scale_fill_viridis(guide = guide_legend(direction="vertical",
                                          title="% Excess Drinkers")) +
  ggtitle("Adults Reporting Excessive Drinking by county") +
  theme_void()

drink_plot

