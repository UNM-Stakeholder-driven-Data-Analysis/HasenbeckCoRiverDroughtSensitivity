#### Trying to read in from API ####

# My api token for DWR is: SBsie9CimoXPeuGBzrzzkj4nJOmM4aYVrKIbw2xkPr0=
# CBT IDs: 5104634, 0404634
#CBT Monthly Diversion data url: https://dwr.state.co.us/Rest/GET/api/v2/structures/divrec/divrecmonth/?format=csv&dateFormat=dateOnly&min-dataMeasDate=01%2F1980&max-dataMeasDate=now&wdid=5104634&apiKey=SBsie9CimoXPeuGBzrzzkj4nJOmM4aYVrKIbw2xkPr0%3D


#All WDIDs, includes transbasin diversions within the Colorado: DWR_Structure_IDs: 
#0304604,7600600,7600572,4800573,304607,4800576,0304600,4800577,0304602,4704602,4704603,0304603,5104601,0304601,5101213,5101212,5104634,5104655,5101269,5101309,5101310,5104625,3600829,0704682,3604658,0704658,3604626,3604684,0800653,0804611,3604685,3604683,3604699,3604684,3604683,3604688,3704641,3704624,3704648,3704614,3704643,3804625,1104612,3804613,1104617,1104618,7900851,7900968,2600702,2000920,3104637,3104638,7804672,7804670,7804671,2904669,2904667,7704636,7704635,3004662,3004660,3004661,7204715,4004657,4504657,5804684,5804686,5804630,4200541

#URL for all WDIDs, monthly: 
#https://dwr.state.co.us/Rest/GET/api/v2/structures/divrec/divrecmonth/?format=csv&dateFormat=dateOnly&wdid=0304604%2C7600600%2C7600572%2C4800573%2C304607%2C4800576%2C0304600%2C4800577%2C0304602%2C4704602%2C4704603%2C0304603%2C5104601%2C0304601%2C5101213%2C5101212%2C5104634%2C5104655%2C5101269%2C5101309%2C5101310%2C5104625%2C3600829%2C0704682%2C3604658%2C0704658%2C3604626%2C3604684%2C0800653%2C0804611%2C3604685%2C3604683%2C3604699%2C3604684%2C3604683%2C3604688%2C3704641%2C3704624%2C3704648%2C3704614%2C3704643%2C3804625%2C1104612%2C3804613%2C1104617%2C1104618%2C7900851%2C7900968%2C2600702%2C2000920%2C3104637%2C3104638%2C7804672%2C7804670%2C7804671%2C2904669%2C2904667%2C7704636%2C7704635%2C3004662%2C3004660%2C3004661%2C7204715%2C4004657%2C4504657%2C5804684%2C5804686%2C5804630%2C4200541&apiKey=SBsie9CimoXPeuGBzrzzkj4nJOmM4aYVrKIbw2xkPr0%3D

#URL for all WDIDs, monthly, force parsable response: 
#https://dwr.state.co.us/Rest/GET/api/v2/structures/divrec/divrecmonth/?format=csvforced&dateFormat=dateOnly&wdid=0304604%2C7600600%2C7600572%2C4800573%2C304607%2C4800576%2C0304600%2C4800577%2C0304602%2C4704602%2C4704603%2C0304603%2C5104601%2C0304601%2C5101213%2C5101212%2C5104634%2C5104655%2C5101269%2C5101309%2C5101310%2C5104625%2C3600829%2C0704682%2C3604658%2C0704658%2C3604626%2C3604684%2C0800653%2C0804611%2C3604685%2C3604683%2C3604699%2C3604684%2C3604683%2C3604688%2C3704641%2C3704624%2C3704648%2C3704614%2C3704643%2C3804625%2C1104612%2C3804613%2C1104617%2C1104618%2C7900851%2C7900968%2C2600702%2C2000920%2C3104637%2C3104638%2C7804672%2C7804670%2C7804671%2C2904669%2C2904667%2C7704636%2C7704635%2C3004662%2C3004660%2C3004661%2C7204715%2C4004657%2C4504657%2C5804684%2C5804686%2C5804630%2C4200541&apiKey=SBsie9CimoXPeuGBzrzzkj4nJOmM4aYVrKIbw2xkPr0%3D


#Fresh attempt using suggestions from Alex
library(httr)
CBTdata <- GET("https://dwr.state.co.us/Rest/GET/api/v2/structures/divrec/divrecmonth/?format=csv&dateFormat=dateOnly&wdid=0404634")


(CBTdata)


#Attempting to pull all WDIDs:
library(httr)
AllWDIDdata <- GET("https://dwr.state.co.us/Rest/GET/api/v2/structures/divrec/divrecmonth/?format=csv&dateFormat=dateOnly&wdid=0304604%2C7600600%2C7600572%2C4800573%2C304607%2C4800576%2C0304600%2C4800577%2C0304602%2C4704602%2C4704603%2C0304603%2C5104601%2C0304601%2C5101213%2C5101212%2C5104634%2C5104655%2C5101269%2C5101309%2C5101310%2C5104625%2C3600829%2C0704682%2C3604658%2C0704658%2C3604626%2C3604684%2C0800653%2C0804611%2C3604685%2C3604683%2C3604699%2C3604684%2C3604683%2C3604688%2C3704641%2C3704624%2C3704648%2C3704614%2C3704643%2C3804625%2C1104612%2C3804613%2C1104617%2C1104618%2C7900851%2C7900968%2C2600702%2C2000920%2C3104637%2C3104638%2C7804672%2C7804670%2C7804671%2C2904669%2C2904667%2C7704636%2C7704635%2C3004662%2C3004660%2C3004661%2C7204715%2C4004657%2C4504657%2C5804684%2C5804686%2C5804630%2C4200541&apiKey=SBsie9CimoXPeuGBzrzzkj4nJOmM4aYVrKIbw2xkPr0%3D")


#Attempting to pull all WDIDs, force parsable response:
library(httr)
AllWDIDdataparsable <- GET("https://dwr.state.co.us/Rest/GET/api/v2/structures/divrec/divrecmonth/?format=csvforced&dateFormat=dateOnly&wdid=0304604%2C7600600%2C7600572%2C4800573%2C304607%2C4800576%2C0304600%2C4800577%2C0304602%2C4704602%2C4704603%2C0304603%2C5104601%2C0304601%2C5101213%2C5101212%2C5104634%2C5104655%2C5101269%2C5101309%2C5101310%2C5104625%2C3600829%2C0704682%2C3604658%2C0704658%2C3604626%2C3604684%2C0800653%2C0804611%2C3604685%2C3604683%2C3604699%2C3604684%2C3604683%2C3604688%2C3704641%2C3704624%2C3704648%2C3704614%2C3704643%2C3804625%2C1104612%2C3804613%2C1104617%2C1104618%2C7900851%2C7900968%2C2600702%2C2000920%2C3104637%2C3104638%2C7804672%2C7804670%2C7804671%2C2904669%2C2904667%2C7704636%2C7704635%2C3004662%2C3004660%2C3004661%2C7204715%2C4004657%2C4504657%2C5804684%2C5804686%2C5804630%2C4200541&apiKey=SBsie9CimoXPeuGBzrzzkj4nJOmM4aYVrKIbw2xkPr0%3D")
AllWDIDdataParsable = GET("https://dwr.state.co.us/Rest/GET/api/v2/structures/divrec/divrecmonth/?format=csv&dateFormat=dateOnly&wdid=0404634") %>%
  httr::content("parsed")

View(AllWDIDdataParsable)

(AllWDIDdata)

View(AllWDIDdata)


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
