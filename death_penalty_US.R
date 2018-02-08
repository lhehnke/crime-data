##---------------------------------------------------------------------------------------##
##                    EXECUTIONS IN THE UNITED STATES: 1801-1900                         ##
##---------------------------------------------------------------------------------------##


## R version 3.4.3 (2017-11-30)

## Author: Lisa Hehnke || lhehnke.github.io || @DataPlanes

## Source: http://deathpenaltyusa.org/


#-------#
# Setup #
#-------#

# Install package geogrid from GitHub
## Source: https://github.com/jbaileyh/geogrid
install.packages("devtools")
library(devtools)
devtools::install_github("jbaileyh/geogrid")

# Install and load packages using pacman
if (!require("pacman")) install.packages("pacman")
library(pacman)

p_load(geogrid, magrittr, maptools, raster, rvest, tidyverse, tmap)


#----------------------#
# Scrape data from web #
#----------------------#

# Create URL for each year
years <- seq(from = 1801, to = 1900, 1) # data available from 1751 onwards
urls <- paste0("http://deathpenaltyusa.org/usa1/date/", years, ".htm") 

# Scrape tables
get_table <- function(url) {
  url %>%
    read_html() %>%
    html_nodes(xpath = "/html/body/div[8]/table") %>% 
    html_table(header = TRUE, fill = TRUE) 
} 

death_penalty <- lapply(urls, get_table)

# Create URLs for missing years and scrape respective tables
years_missing <- c("1864", "1877", "1878", "1879", "1880", "1881", "1882", "1883",
                   "1892", "1893", "1894", "1895", "1896", "1897", "1898")
years_missing2 <- c("1848", "1876", "1891")

urls_missing <- paste0("http://deathpenaltyusa.org/usa1/date/", years_missing, ".htm") 
urls_missing2 <- paste0("http://deathpenaltyusa.org/usa1/date/", years_missing2, ".htm") 

get_table_missing <- function(url) {
  url %>%
    read_html() %>%
    html_nodes(xpath = "/html/body/div[2]/table") %>% 
    html_table(header = TRUE, fill = TRUE) 
}  

get_table_missing2 <- function(url) {
  url %>%
    read_html() %>%
    html_nodes(xpath = "/html/body/div[1]/table[2]") %>% 
    html_table(header = TRUE, fill = TRUE) 
}  

death_penalty_missing <- lapply(urls_missing, get_table_missing)
death_penalty_missing2 <- lapply(urls_missing2, get_table_missing2)


#----------------#
# Data wrangling #
#----------------#

# Convert lists to data.frame
death_penalty_df <- do.call(rbind, lapply(c(death_penalty, death_penalty_missing, death_penalty_missing2), data.frame))

# Convert text to lowercase
death_penalty_df %<>% 
  rename_all(tolower) %>%
  mutate_all(tolower)

# Rename columns
colnames(death_penalty_df) <- c("id", "name", "age", "race", "sex", "occupation", "crime", "method", "month", "day", "year", "state", "st")      

# Remove blank lines
death_penalty_df <- subset(death_penalty_df, !is.na(id)) 

# Change class to numeric
death_penalty_df[, c("day", "year")] <- lapply(death_penalty_df[, c("day", "year")], as.numeric)

# Replace ? with unknown
death_penalty_df %<>%
  mutate_all(funs(gsub("\\?", "unknown", .))) %>%
  mutate_all(funs(gsub("^$", "unknown", .))) 

# Sort data
death_penalty_df %<>% arrange(year, id)

# Clean up type of crime
death_penalty_df$crime %<>%
  gsub("aid runaway \r\n    slve|accessory to \r\n    mur|consp to \r\n    murder", "accessory to crime", .) %>%
  gsub("rape-theft-robbery|rape-robbery|attempted \r\nrape", "rape", .) %>% 
  gsub("murder-burglary|robbery-murder|theft-murder|kidnap-murder|rape-murder|murder-rape-rob|arson-murder|attempted \r\n    murder", "murder", .) %>%
  gsub("robbery|horse \r\nstealing|theft-stealing", "theft/robbery", .) %>%
  gsub("housebrkng-burgl|burg-att rape", "burglary", .) %>%
  gsub("guerilla \r\n    activit", "guerilla activity", .) %>%  
  gsub("sodmy-buggry-bst", "buggery/bestiality", .) %>% 
  gsub("unspec felony", "other", .) %>% 
  gsub("spying-espionage", "spying/espionage", .) %>% 
  gsub("murder", "(attempted) murder", .) %>%
  gsub("rape", "(attempted) rape", .) 

# Clean up race
death_penalty_df$race %<>% gsub("nat amer", "native american", .)

#saveRDS(death_penalty_df, "death_penalties_US_1801-1900.rds")
#death_penalty_df <- readRDS("death_penalties_US_1801-1900.rds")


#---------------------#
# Download shapefiles #
#---------------------#

# Download, unzip and import US shapefiles from USCB webpage
temp <- tempfile()
download.file("http://www2.census.gov/geo/tiger/GENZ2014/shp/cb_2014_us_state_5m.zip", temp, mode = "w")
unzip(temp)
US_shp <- readShapeSpatial("cb_2014_us_state_5m.shp", proj4string = CRS("+proj=longlat +ellps=WGS84"))
unlink(temp)

# Crop country border to Continental US extent
US_shp_cropped <- crop(US_shp, extent(-124.848974, -66.885444, 24.396308, 49.384358)) 


#--------------------------#
# Map number of executions #
#--------------------------#

# Count deaths by state
deaths_sum <- death_penalty_df %>% count(state)

# First letter to uppercase for joining
deaths_sum$state <- gsub("^(\\w)(\\w+)", "\\U\\1\\L\\2", deaths_sum$state, perl = TRUE)

# Left_join data by state
US_shp_cropped@data <- left_join(US_shp_cropped@data, deaths_sum, by = c("NAME" = "state"))

# Set color palette
## Source: http://www.colourlovers.com/palette/170249/Vampyrism
pal <- c("#D3AF8E", "#7A0E0E", "#5A0B0B", "#380606", "#201B1B")

# Map number of executions in each state
map <- tm_shape(US_shp_cropped) +
  tm_fill(col = "n", palette = pal, auto.palette.mapping = FALSE, breaks = c(1, 100, 200, 300, 400, 500),
          textNA = "No data", title = "No. of executions", text.size = "AREA", style = "fixed") +
  tm_text("STUSPS", size = "AREA", root = 4) +
  tm_borders(col = "white") +
  tm_credits("1801-1900", size = 0.8, position = c("left", "bottom")) +
  tm_legend(legend.outside = TRUE, position = c("right", "center")) #+
  #tm_layout(frame = FALSE)
map

save_tmap(map, "Executions_1801-1900.png", width = 1460, height = 615) 


#------------------------------------#
# Hexagon plot: Number of executions #
#------------------------------------#

# Calculate and view hexagonal grid options 
par(mfrow = c(2, 3), mar = c(0, 0, 2, 0))
for (i in 1:6) {
  new_cells <- calculate_grid(shape = US_shp_cropped, grid_type = "hexagonal", seed = i)
  plot(new_cells, main = paste("Seed", i, sep = " "))
}

# Calculate chosen hexagonal grid
new_cells_hex <- calculate_grid(shape = US_shp_cropped, grid_type = "hexagonal", seed = 1)
result_hex <- assign_polygons(US_shp_cropped, new_cells_hex)

# Function for tidying SpatialPolygonsDataFrame
## Source: https://github.com/jbaileyh/geogrid#example
clean <- function(shape) {
  shape@data$id = rownames(shape@data)
  shape.points = fortify(shape, region="id")
  shape.df = merge(shape.points, shape@data, by="id")
}

result_hex_df <- clean(result_hex)
US_shp_cropped_df <- clean(US_shp_cropped)

# Hexagon plot
ggplot(result_hex_df) +
  geom_polygon(aes(x = long, y = lat, fill = n, group = group), color = "white") +
  geom_text(aes(V1, V2, label = STUSPS), size = 5, color = "white") +
  scale_fill_gradientn(colours = pal, name = "No. of executions,\n1801-1900") + 
  theme_void() + theme(legend.position = "right") +
  coord_equal() 

ggsave("Executions_1801-1900_hex.png", width = 15, height = 6, units = "in", dpi = 100)

# Extract centroids and merge with SpatialPolygonsDataFrame
centroids_df <- as.data.frame(coordinates(US_shp_cropped))
names_df <- as.data.frame(US_shp_cropped$NAME)
centroids_df <- cbind(centroids_df, names_df)
names(centroids_df) <- c("Longitude", "Latitude", "NAME") 
US_shp_cropped@data <- left_join(US_shp_cropped@data, centroids_df, by = c("NAME"))

# Regular plot (ggplot2 version)
ggplot(US_shp_cropped_df) +
  geom_polygon(aes(x = long, y = lat, fill = n, group = group)) +
  geom_text(aes(Longitude, Latitude, label = STUSPS), size = 5, color = "white") +
  coord_equal() +
  scale_fill_gradientn(colours = pal, name = "No. of executions,\n1801-1900") + 
  theme_void() + theme(legend.position = "right")

ggsave("Executions_1801-1900_ggplot.png", width = 15, height = 6, units = "in", dpi = 100)


#----------------------#
# Executions over time #
#----------------------#

# Set theme for visualizations
viz_theme <- theme(
  strip.background = element_rect(colour = "grey20", fill = "#92a1a9"),
  axis.line = element_line(colour = "grey20"),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.border = element_blank(),
  panel.background = element_blank(),
  strip.text = element_text(size = rel(1), face = "bold"),
  plot.caption = element_text(colour = "#4e5975"),
  text = element_text(family = "Avenir"))

# Count executions by year
death_penalty_ts <- death_penalty_df %>% count(year) 

# Add 01-01 to year due to missing values in month/day
death_penalty_ts$year <- as.Date(paste0(death_penalty_ts$year, '-01-01'))

# Set 10 year breaks and manually add 1900
breaks <- death_penalty_ts$year[seq(1, length(death_penalty_ts$year), 10)]
breaks <- append(breaks, as.Date("1900-01-01"))

# Plot timeline
ggplot(death_penalty_ts, aes(year, n)) +
  geom_line(col = "#5A0B0B", size = 1) + 
  scale_x_date(breaks = breaks, date_labels = "%Y") +
  labs(x = "Year", y = "Count", title = "Number of executions in the US over time, 1801-1900", subtitle = " ") +
  theme(text = element_text(size = 20)) + 
  viz_theme + ylim(0, 150) 

ggsave("plot1.png", width = 12, height = 8, units = "in", dpi = 100)


#-----------------------------#
# Executions by type of crime #
#-----------------------------#

# Plot executions by crime
death_penalty_df %>%  
  count(crime, sort = TRUE) %>% 
  mutate(crime = reorder(crime, n)) %>%
  ggplot(aes(crime, n, label = n)) +
  geom_bar(stat = "identity", fill = "#380606", col = "#7A0E0E", width = 0.5, alpha = 0.9) +
  geom_text(color = "#380606", hjust = -0.5, size = 4) +
  labs(x = "Crime", y = "Count", title = "Number of executions in the US by crime, 1801-1900", subtitle = " ") +
  theme(text = element_text(size = 20)) + 
  viz_theme + ylim(0, 6000) + coord_flip()

ggsave("plot2.png", width = 12, height = 8, units = "in", dpi = 100)


#-------------------------------------#
# Executions by sex, race, and method #
#-------------------------------------#

# Plot executions by sex
death_penalty_df %>%  
  count(sex, sort = TRUE) %>% 
  mutate(sex = reorder(sex, n)) %>%
  ggplot(aes(sex, n, label = n)) +
  geom_bar(stat = "identity", fill = "#380606", col = "#7A0E0E", width = 0.5, alpha = 0.9) +
  geom_text(color = "#380606", vjust = -0.5, size = 4) +
  labs(x = "Sex", y = "Count", title = "Number of executions in the US by sex, 1801-1900", subtitle = " ") +
  theme(text = element_text(size = 20)) + 
  viz_theme + ylim(0, 6000)

ggsave("plot3.png", width = 12, height = 8, units = "in", dpi = 100)

# Plot executions by race
death_penalty_df %>%  
  count(race, sort = TRUE) %>% 
  mutate(race = reorder(race, n)) %>%
  ggplot(aes(race, n, label = n)) +
  geom_bar(stat = "identity", fill = "#380606", col = "#7A0E0E", width = 0.5, alpha = 0.9) +
  geom_text(color = "#380606", vjust = -0.5, size = 4) +
  labs(x = "Race", y = "Count", title = "Number of executions in the US by race, 1801-1900", subtitle = " ") +
  theme(text = element_text(size = 20)) + 
  viz_theme + ylim(0, 6000) + theme(axis.text.x = element_text(angle = 65, vjust = 0.5))

ggsave("plot4.png", width = 12, height = 8, units = "in", dpi = 100)

# Plot executions by method
death_penalty_df %>%  
  count(method, sort = TRUE) %>% 
  mutate(method = reorder(method, n)) %>%
  ggplot(aes(method, n, label = n)) +
  geom_bar(stat = "identity", fill = "#380606", col = "#7A0E0E", width = 0.5, alpha = 0.9) +
  geom_text(color = "#380606", vjust = -0.5, size = 4) +
  labs(x = "Method", y = "Count", title = "Number of executions in the US by method, 1801-1900", subtitle = " ") +
  theme(text = element_text(size = 20)) + 
  viz_theme + ylim(0, 6000) + theme(axis.text.x = element_text(angle = 65, vjust = 0.5))

ggsave("plot5.png", width = 12, height = 8, units = "in", dpi = 100)
