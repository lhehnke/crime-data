##---------------------------------------------------------------------------------------##
##                       DEATH PENALTIES IN THE US: 1800-1900                            ##
##---------------------------------------------------------------------------------------##


## R version 3.4.3 (2017-11-30)

## Source: http://deathpenaltyusa.org/


#-------#
# Setup #
#-------#

# Install and load packages using pacman
if (!require("pacman")) install.packages("pacman")
library(pacman)

p_load(magrittr, maptools, raster, rvest, tidyverse, tmap)


#----------------------#
# Scrape data from web #
#----------------------#

# Create URL for each year
years <- seq(from = 1800, to = 1900, 1) # data available from 1751 onwards
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

# Convert list to data.frame
death_penalty_df <- do.call(rbind, lapply(death_penalty, data.frame))
death_penalty_missing_df <- do.call(rbind, lapply(death_penalty_missing, data.frame))
death_penalty_missing2_df <- do.call(rbind, lapply(death_penalty_missing2, data.frame))

# Merge data
death_penalty_df <- rbind(death_penalty_df, death_penalty_missing_df)
death_penalty_df <- rbind(death_penalty_df, death_penalty_missing2_df)

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

# Replace ? with "Unknown
death_penalty_df %<>%
  mutate_all(funs(gsub("\\?", "unknown", .))) 

# Sort data
death_penalty_df %<>% arrange(year, id)


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
  tm_credits("1800-1900", size = 0.8, position = c("left", "bottom")) +
  tm_legend(legend.outside = TRUE, position = c("right", "center")) #+
  #tm_layout(frame = FALSE)
map

save_tmap(map, "Executions_1800-1900.png", width = 1460, height = 615) 


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
death_penalty_ts <- death_penalty_df %>%  
  count(year) 

# Add 01-01 to year due to missing values in month/day
death_penalty_ts$year <- as.Date(paste0(death_penalty_ts$year, '-01-01'))

# Set 10 year breaks
breaks <- death_penalty_ts$year[seq(1, length(death_penalty_ts$year), 10)]

# Plot timeline
ggplot(death_penalty_ts, aes(year, n)) +
  geom_line(col = "#380606") + 
  scale_x_date(breaks = breaks, date_labels = "%Y") +
  labs(x = "", y = "Count", title = "Number of executions over time, 1800-1900", subtitle = " ") +
  viz_theme + ylim(0, 150) #+ theme(axis.text.x = element_text(angle = 65, vjust = 0.5))


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
  labs(x = "Sex", y = "Count", title = "Number of executions by sex, 1800-1900", subtitle = " ") +
  viz_theme + ylim(0, 6000)

# Plot executions by race
death_penalty_df %>%  
  count(race, sort = TRUE) %>% 
  mutate(race = reorder(race, n)) %>%
  ggplot(aes(race, n, label = n)) +
  geom_bar(stat = "identity", fill = "#380606", col = "#7A0E0E", width = 0.5, alpha = 0.9) +
  geom_text(color = "#380606", vjust = -0.5, size = 4) +
  labs(x = "Race", y = "Count", title = "Number of executions by race, 1800-1900", subtitle = " ") +
  viz_theme + ylim(0, 6000) + theme(axis.text.x = element_text(angle = 65, vjust = 0.5))

# Plot executions by method
death_penalty_df %>%  
  count(method, sort = TRUE) %>% 
  mutate(method = reorder(method, n)) %>%
  ggplot(aes(method, n, label = n)) +
  geom_bar(stat = "identity", fill = "#380606", col = "#7A0E0E", width = 0.5, alpha = 0.9) +
  geom_text(color = "#380606", vjust = -0.5, size = 4) +
  labs(x = "Method", y = "Count", title = "Number of executions by method, 1800-1900", subtitle = " ") +
  viz_theme + ylim(0, 6000) + theme(axis.text.x = element_text(angle = 65, vjust = 0.5))
