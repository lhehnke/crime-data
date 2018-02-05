# crime-data
Script for scraping, wrangling, mapping, and visualizing historical data on US executions

## Description: death_penalty_US

The script covers

* scraping tables containing information on executions in the United States with *rvest*
* wrangling scraped data
* mapping executions by state with *tmap*
* visualizing the number of executions by
    * sex
    * race
    * method of execution
  
using data on N = 5480 executions in the US from 1800 to 1900.  

Data source: deathpenaltyusa.org 

Example plot:

<p align="center"><img src="https://raw.githubusercontent.com/lhehnke/crime-data/master/Executions_1800-1900.png " width="973px" height="410x" vspace="40px"/></p>
