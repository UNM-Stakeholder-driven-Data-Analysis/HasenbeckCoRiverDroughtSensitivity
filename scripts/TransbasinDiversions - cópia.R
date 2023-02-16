####Read me#### 
##This script cleans up DWR transbasin diversion data from Open Water Foundation:
  #https://github.com/OpenWaterFoundation/owf-data-co-transbasin-diversions

## Content of csv :
  # TransbasinDiversionName - name of the transbasin diversion; adapted from Water Education Colorado's Citizen's Guide to Colorado's Transbasin Diversions, see Attribution section below
  # Source_WaterDistrict_ID - Division of Water Resources' Water District ID of the water source
  # Source_WaterDistrict_ID_Flag - data status of Source_WaterDistrict values; see more detail below
  # Source_WaterDistrict_Name - Division of Water Resources' Water District name of the water source
  # Source_WaterDivision - Division of Water Resources' Water Division of the water source
  # Source_WaterDivision_Flag - data status of Source_WaterDivision values; see more detail below
  # Source_GNIS_Name_CSV - Geographic Names Information System name of the water source(s), in comma-separated format, to link federal datasets
  # Source_GNIS_Name_CSV_Flag - data status of Source_GNIS_Name_CSV values; see more detail below
  # Source_GNIS_ID_CSV - Geographic Names Information System identifier of the water source(s), in comma-separated format, to link federal datasets
  # Source_GNIS_ID_CSV_Flag - data status of Source_GNIS_ID_CSV values; see more detail below
  # Source_Structure_WDID_CSV - WDIDs (state identifiers for structures such as ditches and reservoirs) of structures associated with the source of the transbasin diversion, in comma-separated format, to link state datasets. In a typical transbasin diversion, there is a "source" structure(s) which is the point of diversion from a stream; this structure(s) may or may not have diversion records.
  # Source_Structure_WDID_CSV_Flag - data status of Source_Structure_WDID_CSV values; see more detail below
  # Source_IBCCBasin - Interbasin Compact Committee basin of the water source
  # Source_IBCCBasin_Flag - data status of Source_IBCCBasin values; see more detail below
  # Destination_WaterDistrict_ID - Division of Water Resources' Water District ID of the water destination
  # Destination_WaterDistrict_ID_Flag - data status of Destination_WaterDistrict values; see more detail below
  # Destination_WaterDistrict_Name - Division of Water Resources' Water District name of the water destination
  # Destination_WaterDivision - Division of Water Resources' Water Division of the water destination
  # Destination_WaterDivision_Flag - data status of Destination_WaterDivision values; see more detail below
  # Destination_GNIS_Name_CSV - Geographic Names Information System name of the water destination, in comma-separated format, to link federal datasets
  # Destination_GNIS_Name_CSV_Flag - data status of Destination_GNIS_Name_CSV values; see more detail below
  # Destination_GNIS_ID_CSV - Geographic Names Information System identifier of the water destination, in comma-separated format, to link federal datasets
  # Destination_GNIS_ID_CSV_Flag - data status of Destination_GNIS_ID_CSV values; see more detail below
  # Destination_Structure_WDID_CSV - WDIDs (state identifiers for structures such as ditches and reservoirs) of structures associated with the destination of the transbasin diversion, in comma-separated format, to link state datasets. In a typical transbasin diversion, there is a "destination" structure, sometimes called a release structure, with a WDID similar to the source structure (the district number differs, but the structure ID is the same); this may or may not have diversion records.
  # Destination_Structure_WDID_CSV_Flag - data status of Destination_Structure_WDID_CSV values; see more detail below
  # Destination_IBCCBasin - Interbasin Compact Committee basin of the water destination
  # Destination_IBCCBasin_Flag - data status of Destination_IBCCBasin values; see more detail below
  # Description - multi-sentence description of the transbasin diversion project, for educational purposes
  # Comment - any other information about the transbasin diversion
  # Data Flags

##For many data columns, a second column of the same name with the word "_Flag" added to the column name is present. 
  #These columns are an indication of data status as it relates to missing data. 
  #The following conventions are used:
    # G = Value is a known/good value.
    # g = Value is an estimated (but good) value. The associated cell is also highlighted in yellow.
    # N = Value is not applicable for the municipality and a blank cell is expected.
    # I = Incomplete values; cell has been populated but may not yet contain all values or may need to be further verified
    # M = Value is known to be missing in original source and therefore a blank cell indicates that a value cannot be provided.
    # m = Value is estimated to be missing. The associated cell is also highlighted in gray.
    # z = Value is unable to be confirmed. A value is possible but cannot be confirmed one way or the other. The associated cell is also highlighted in orange.
    # x = OWF has not made an attempt to populate the cell at this time. The value is missing because OWF has not attempted to find the value. The associated cell is also highlighted in black.

#### Read in data ####
Transbasin_Diversions = read.csv("data/raw/Colorado-Transbasin-Diversions.csv", header = TRUE)

####Load libraries####
library(tidyverse)


#### Cleaning dataframe to relevant columns and rows####

Transbasin_Diversions_Clean = Transbasin_Diversions %>%
  #Filter colums to only name, stream, source basin, destination basin, DWR structure id
  select(TransbasinDiversionName,Source_GNIS_Name_CSV, Source_IBCCBasin, Destination_IBCCBasin, Structure_WDID_CSV) %>% 
  #rename columns
  rename(Transbasin_Diversion_Name = TransbasinDiversionName,
         Source_Streams = Source_GNIS_Name_CSV,
         Source_basin = Source_IBCCBasin,
         Destination_basin = Destination_IBCCBasin,
         DWR_Structure_IDs = Structure_WDID_CSV)


  #remove transbasin diversions from one Colorado sub basin to another (transfers where destination basin is Gunnison or Colorado)
  filter(Destination_basin!="Colorado",
       Destination_basin!="Gunnison") %>%
  #remove transbasin diversions originating from Rio Grande 
  filter(Source_basin!="Rio Grande")

####exporting df as csv ####
write.csv(Transbasin_Diversions_Clean, "data/processed/Transbasin-Diversions-clean.csv")

