#### Option 1: Reading in from API - Takes a long time to run. ####

# My api token for DWR is: SBsie9CimoXPeuGBzrzzkj4nJOmM4aYVrKIbw2xkPr0=

#WDID is what DWR calls the structure ids. Here are all WDIDs to input into URL generator
  #This includes transbasin diversions within the Colorado basin, too.
  #0304604,7600600,7600572,4800573,304607,4800576,0304600,4800577,0304602,4704602,4704603,0304603,5104601,0304601,5101213,5101212,5104634,5104655,5101269,5101309,5101310,5104625,3600829,0704682,3604658,0704658,3604626,3604684,0800653,0804611,3604685,3604683,3604699,3604684,3604683,3604688,3704641,3704624,3704648,3704614,3704643,3804625,1104612,3804613,1104617,1104618,7900851,7900968,2600702,2000920,3104637,3104638,7804672,7804670,7804671,2904669,2904667,7704636,7704635,3004662,3004660,3004661,7204715,4004657,4504657,5804684,5804686,5804630,4200541

#This is the url to generate the monthly diversion structure data:https://dwr.state.co.us/Rest/GET/Help/DivRecMonthGenerator

#URL for all WDIDs, monthly: 
#https://dwr.state.co.us/Rest/GET/api/v2/structures/divrec/divrecmonth/?format=csv&dateFormat=dateOnly&wdid=0304604%2C7600600%2C7600572%2C4800573%2C304607%2C4800576%2C0304600%2C4800577%2C0304602%2C4704602%2C4704603%2C0304603%2C5104601%2C0304601%2C5101213%2C5101212%2C5104634%2C5104655%2C5101269%2C5101309%2C5101310%2C5104625%2C3600829%2C0704682%2C3604658%2C0704658%2C3604626%2C3604684%2C0800653%2C0804611%2C3604685%2C3604683%2C3604699%2C3604684%2C3604683%2C3604688%2C3704641%2C3704624%2C3704648%2C3704614%2C3704643%2C3804625%2C1104612%2C3804613%2C1104617%2C1104618%2C7900851%2C7900968%2C2600702%2C2000920%2C3104637%2C3104638%2C7804672%2C7804670%2C7804671%2C2904669%2C2904667%2C7704636%2C7704635%2C3004662%2C3004660%2C3004661%2C7204715%2C4004657%2C4504657%2C5804684%2C5804686%2C5804630%2C4200541&apiKey=SBsie9CimoXPeuGBzrzzkj4nJOmM4aYVrKIbw2xkPr0%3D

#URL for all WDIDs, monthly, forcing a parsable response: 
#https://dwr.state.co.us/Rest/GET/api/v2/structures/divrec/divrecmonth/?format=csvforced&dateFormat=dateOnly&wdid=0304604%2C7600600%2C7600572%2C4800573%2C304607%2C4800576%2C0304600%2C4800577%2C0304602%2C4704602%2C4704603%2C0304603%2C5104601%2C0304601%2C5101213%2C5101212%2C5104634%2C5104655%2C5101269%2C5101309%2C5101310%2C5104625%2C3600829%2C0704682%2C3604658%2C0704658%2C3604626%2C3604684%2C0800653%2C0804611%2C3604685%2C3604683%2C3604699%2C3604684%2C3604683%2C3604688%2C3704641%2C3704624%2C3704648%2C3704614%2C3704643%2C3804625%2C1104612%2C3804613%2C1104617%2C1104618%2C7900851%2C7900968%2C2600702%2C2000920%2C3104637%2C3104638%2C7804672%2C7804670%2C7804671%2C2904669%2C2904667%2C7704636%2C7704635%2C3004662%2C3004660%2C3004661%2C7204715%2C4004657%2C4504657%2C5804684%2C5804686%2C5804630%2C4200541&apiKey=SBsie9CimoXPeuGBzrzzkj4nJOmM4aYVrKIbw2xkPr0%3D

#Metadata for WDID data: https://dnrweblink.state.co.us/dwr/ElectronicFile.aspx?docid=3600965&dbid=0 

#Attempting to pull all WDIDs, force parsable response:
library(dplyr)
library(httr)
AllWDIDdataparsable <- GET("https://dwr.state.co.us/Rest/GET/api/v2/structures/divrec/divrecmonth/?format=csvforced&dateFormat=dateOnly&wdid=0304604%2C7600600%2C7600572%2C4800573%2C304607%2C4800576%2C0304600%2C4800577%2C0304602%2C4704602%2C4704603%2C0304603%2C5104601%2C0304601%2C5101213%2C5101212%2C5104634%2C5104655%2C5101269%2C5101309%2C5101310%2C5104625%2C3600829%2C0704682%2C3604658%2C0704658%2C3604626%2C3604684%2C0800653%2C0804611%2C3604685%2C3604683%2C3604699%2C3604684%2C3604683%2C3604688%2C3704641%2C3704624%2C3704648%2C3704614%2C3704643%2C3804625%2C1104612%2C3804613%2C1104617%2C1104618%2C7900851%2C7900968%2C2600702%2C2000920%2C3104637%2C3104638%2C7804672%2C7804670%2C7804671%2C2904669%2C2904667%2C7704636%2C7704635%2C3004662%2C3004660%2C3004661%2C7204715%2C4004657%2C4504657%2C5804684%2C5804686%2C5804630%2C4200541&apiKey=SBsie9CimoXPeuGBzrzzkj4nJOmM4aYVrKIbw2xkPr0%3D")
AllWDIDdataParsable = GET("https://dwr.state.co.us/Rest/GET/api/v2/structures/divrec/divrecmonth/?format=csvforced&dateFormat=dateOnly&wdid=0304604%2C7600600%2C7600572%2C4800573%2C304607%2C4800576%2C0304600%2C4800577%2C0304602%2C4704602%2C4704603%2C0304603%2C5104601%2C0304601%2C5101213%2C5101212%2C5104634%2C5104655%2C5101269%2C5101309%2C5101310%2C5104625%2C3600829%2C0704682%2C3604658%2C0704658%2C3604626%2C3604684%2C0800653%2C0804611%2C3604685%2C3604683%2C3604699%2C3604684%2C3604683%2C3604688%2C3704641%2C3704624%2C3704648%2C3704614%2C3704643%2C3804625%2C1104612%2C3804613%2C1104617%2C1104618%2C7900851%2C7900968%2C2600702%2C2000920%2C3104637%2C3104638%2C7804672%2C7804670%2C7804671%2C2904669%2C2904667%2C7704636%2C7704635%2C3004662%2C3004660%2C3004661%2C7204715%2C4004657%2C4504657%2C5804684%2C5804686%2C5804630%2C4200541&apiKey=SBsie9CimoXPeuGBzrzzkj4nJOmM4aYVrKIbw2xkPr0%3D") %>%
  httr::content("parsed")

write.csv(AllWDIDdataParsable,"data/raw/AllWDIDdataParsable.csv")

#### Option 2: Run from CSV. Start here if not pulling from API. This runs from local csv ####
AllWDIDdataParsableDF <- read.csv("data/processed/AllWDIDdataParsable.csv")

#### Tidying up data #### 
#Cleaning up comma delineated 
library(dplyr)
library(tidyr)
library(tidyverse)

#Using tutorial from tidyverse 
#https://tidyr.tidyverse.org/reference/separate.html
#REMEMBER ELEANOR TO CHANGE TO ALLWDIDdataPARSABLE 

AllWDIDdataClean <- 
  #Seperating ResultDateTime column into appropriate columns
  separate(
    AllWDIDdataParsableDF,
    c(ResultDateTime),
    c("measInterval","measCount","dataMeasDate","dataValue","measUnits","obsCode","approvalStatus","modified"),
    sep = ",",
    remove = FALSE,
    convert = FALSE,
    extra = "warn",
    fill = "warn",
    )

#Seperating resultcount into appropriate columna
AllWDIDdataClean2 <-  separate(
    AllWDIDdataClean,
    c(ResultCount),
    c("wcIdentifier","S:4","F:","U:Q","T:7","G:","To:"),
    sep = " ",
    remove = FALSE,
    convert = FALSE,
    extra = "warn",
    fill = "warn",
  )




AllWDIDdataClean3 <- AllWDIDdataClean2[-c(1,2), ]

#Select only the relevant columns

AllWDIDsimplified <- AllWDIDdataClean3  %>%
  #remove un-needed columns 
  select("wcIdentifier","measInterval", "dataMeasDate", "dataValue", "obsCode", "approvalStatus", "modified")
  #rename columns 
  rename(AllWDIDsimplified, 
         "WDID" = "wcIdentifier", 
         "MeasurementInterval" ="measInterval", 
         "DateMeasured" = "dataMeasDate", 
         "AFdiverted" = "dataValue",
         "ObservationCode" = "obsCode",
         "ApprovalStatus" = "approvalStatus", 
         "DateModified" = "modified")

  #Renaming categories based on metadata
AllWDIDsimplified$obsCode[AllWDIDsimplified$obsCode == "*"] <- "obs"   #amt observed in person
AllWDIDsimplified$obsCode[AllWDIDsimplified$obsCode == "U"] <- "unk-user" # user supplied unk reliability 
AllWDIDsimplified$obsCode[AllWDIDsimplified$obsCode == "K"] <- "k-user" #user supplied known reliability
AllWDIDsimplified$obsCode[AllWDIDsimplified$obsCode == "k"] <- "k-user" #user supplied known reliability 
AllWDIDsimplified$obsCode[AllWDIDsimplified$obsCode == "R"] <- "obs"   #amount recorded by device
AllWDIDsimplified$obsCode[AllWDIDsimplified$obsCode == "S"] <- "estimate"   #amount estimated in person 
AllWDIDsimplified$obsCode[AllWDIDsimplified$obsCode == "E"] <- "estimate"   #date and amount estimated 
AllWDIDsimplified$obsCode[AllWDIDsimplified$obsCode == "C"] <- "calculated" #formula 
AllWDIDsimplified$obsCode[AllWDIDsimplified$obsCode == "M"] <- "modeled"   # model


  View(AllWDIDsimplified)

#Scaling to arrange 
#Function to normalize 
  function(x){(x-min(x))/(max(x)-min(x))}
  
  6
  # normalize (scale to 1)
  range01 <- function(x){(x-min(x))/(max(x)-min(x))}
  dat$var = range01(dat$var)
  
#that function scales between 0 and 1, so use the forum to see how to adjust between -4 and 4

write_csv(AllWDIDsimplified,"data/processed/AllWDIDtidied.csv")



