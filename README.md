# crime-data
Historical data on executions in the US (1801-1900) and script for scraping, wrangling, mapping, and visualizing data on US executions

## Description: death_penalties_US_1801-1900

Contains the following scraped data on executed criminals in the United States (N = 5455):

* name
* age
* race
* sex
* occupation
* crime
* method
* month/day/year of execution
* state

Data source: www.deathpenaltyusa.org 

## Description: death_penalty_US

The script covers

* scraping tables containing information on executions in the United States with *rvest*
* wrangling scraped data
* mapping executions by state with *tmap*
* visualizing the number of executions by
    * sex
    * race
    * method of execution
  
using the above data on executions in the US from 1801 to 1900.  

Example map:

<p align="center"><img src="https://raw.githubusercontent.com/lhehnke/lhehnke.github.io/master/img/death-penalty/Executions_1801-1900_hex.png" width="973px" height="410x" vspace="40px"/></p>

Example plot:

<p align="center"><img src="https://raw.githubusercontent.com/lhehnke/lhehnke.github.io/master/img/death-penalty/plot1.png" width="600px" height="400x" vspace="40px"/></p>
