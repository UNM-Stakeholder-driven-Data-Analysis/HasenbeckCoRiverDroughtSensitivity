#### Trying to read in from API ####

# My api token for DWR is: SBsie9CimoXPeuGBzrzzkj4nJOmM4aYVrKIbw2xkPr0=
# CBT IDs: 5104634, 0404634
#CBT Monthly Diversion data url: https://dwr.state.co.us/Rest/GET/api/v2/structures/divrec/divrecmonth/?format=csv&dateFormat=dateOnly&min-dataMeasDate=01%2F1980&max-dataMeasDate=now&wdid=5104634&apiKey=SBsie9CimoXPeuGBzrzzkj4nJOmM4aYVrKIbw2xkPr0%3D


#Fresh attempt using suggestions from Alex
library(httr)
CBTdata <- GET("https://dwr.state.co.us/Rest/GET/api/v2/structures/divrec/divrecmonth/?format=csv&dateFormat=dateOnly&wdid=0404634")


(CBTdata)






?res
?httr

> res
Response [https://dwr.state.co.us/Rest/GET/api/v2/telemetrystations/telemetrytimeseriesraw/?format=csv&abbrev=PLAKERCO&parameter=DISCHRG]
Date: 2023-02-16 17:26
Status: 200
Content-Type: text/csv; charset=utf-8
Size: 248 kB





#This seemes to get the closest, was generated using R studio import button using this url but it gives a 400 error. 
  # https://dwr.state.co.us/Rest/GET/api/v2/structures/divrec/divrecmonth/?format=csv&dateFormat=dateOnly&min-dataMeasDate=01%2F1980&max-dataMeasDate=now&wdid=5104634&apiKey=SBsie9CimoXPeuGBzrzzkj4nJOmM4aYVrKIbw2xkPr0%3D
library(readr)
CBTdata <- read_csv("https://dwr.state.co.us/Rest/GET/api/v2/structures/divrec/divrecmonth/?format=csv&dateFormat=dateOnly&min-dataMeasDate=01%2F1980&max-dataMeasDate=now&wdid=5104634&apiKey=SBsie9CimoXPeuGBzrzzkj4nJOmM4aYVrKIbw2xkPr0%3D")









#I do not know what this or what it does. It's the remnants of a few stack exchange searches 
library(readr)
X_format_csv_dateFormat_dateOnly_min_dataMeasDate_01_2F1980_max_dataMeasDate_now_wdid_5104634_apiKey_SBsie9CimoXPeuGBzrzzkj4nJOmM4aYVrKIbw2xkPr0_3D <- 
  read_csv(
View(X_format_csv_dateFormat_dateOnly_min_dataMeasDate_01_2F1980_max_dataMeasDate_now_wdid_5104634_apiKey_SBsie9CimoXPeuGBzrzzkj4nJOmM4aYVrKIbw2xkPr0_3D)


curl::curl("https://dwr.state.co.us/Rest/GET/api/v2/structures/divrec/divrecmonth/?format=csv&dateFormat=dateOnly&min-dataMeasDate=01%2F1980&max-dataMeasDate=now&wdid=5104634&apiKey=SBsie9CimoXPeuGBzrzzkj4nJOmM4aYVrKIbw2xkPr0%3D") %>% read_html() 


"https://dwr.state.co.us/Rest/GET/api/v2/structures/divrec/divrecmonth/?format=csv&dateFormat=dateOnly&min-dataMeasDate=01%2F1980&max-dataMeasDate=now&wdid=5104634&apiKey=SBsie9CimoXPeuGBzrzzkj4nJOmM4aYVrKIbw2xkPr0%3D"

library(textreadr)
if (!require("pacman")) install.packages("pacman")
pacman::p_load_gh("trinker/textreadr")

#### Trying to install DWR R tools package ####
library(cdssr)
# install.packages("devtools")
install.packages("anguswg-ucsb/cdssr")
# Load package
library(cdssr)

install.packages("remotes")

library(remotes) 

install.version("anguswg-ucsb/cdssr")

#random commands that didn't work 
utils::globalVariables(c("."))

install_github("cdssr",username="anguswg-ucsb")

install.packages(git2r) 

install.packages("devtools")
get_structure_divrecmonth<- function(
    wdid            = "7600572",
    wc_identifier   = NULL,
    start_date      = "1900-01-01",
    end_date        = Sys.Date(),
    api_key         = "SBsie9CimoXPeuGBzrzzkj4nJOmM4aYVrKIbw2xkPr0=")
  
  
  
  

library("remotes")
install_github("/anguswg-ucsb/cdssr")
library(remotes)
install_github("cran/anguswg-ucsb/cdssr")
