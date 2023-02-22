####Read me #### 

#This script first cleans and then combines two SWSI datasets.

#libraries 
library(tidyverse)
library(dplyr)

SWSI1981to2011 <- read.csv("data/raw/SWSI1981to2011.csv")
SWSI2010tonow <- read.csv("data/raw/SWSI2010tonow.csv")


####Cleaning SWSI 2010 to now data####

SWSI2010tonowCLEAN <- SWSI2010tonow %>%
  select("basin","report_year","report_month","swsi") %>%
  pivot_wider(names_from = "basin", values_from = "swsi")
  
SWSI2010tonowCLEAN <- SWSI2010tonow %>%
  select("basin","report_year","report_month","swsi") %>%
  pivot_wider(names_from = "basin", values_from = "swsi")

  
  pivot_wider(data = SWSI2010tonow, 
              id_cols = "basin", 
              names_from = student, 
              values_from = c("A", "B"))
  ?spread
  
?spread


View(SWSI2010tonowCLEAN)
?select
  