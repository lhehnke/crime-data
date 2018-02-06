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
  
using data on N = 5455 executions in the US from 1801 to 1900.  

Data source: www.deathpenaltyusa.org 

Example map:

<p align="center"><img src="https://raw.githubusercontent.com/lhehnke/lhehnke.github.io/master/img/death-penalty/Executions_1801-1900.png" width="973px" height="410x" vspace="40px"/></p>

Example plot:

<p align="center"><img src="https://raw.githubusercontent.com/lhehnke/lhehnke.github.io/master/img/death-penalty/plot1.png" width="600px" height="400x" vspace="40px"/></p>
