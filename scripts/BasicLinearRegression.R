#### this script is an intial linear model of TBDs and SWSI. 

#### libraries 
library(tidyverse) 
library(lme4) # for creating mixed models
library(car) # for Anova(), vif()
library(MuMIn) # for AICc
library(emmeans) # for emmeans, emtrends, all the post hoc tests and plotting


## here is a cheatsheet for emmeans. Super helpful!!
# https://timmastny.rbind.io/blog/tests-pairwise-categorical-mean-emmeans-contrast/

####data prep (this is saved to csv so can skip to read data for linear regression) ####
Diversions <- read_csv("data/processed/TBdiversionsofinterest")
SWSI <- read_csv("data/processed/SWSI1981to2023.csv")


SWSI <- SWSI %>%
  pivot_longer(cols = "Gunnison":"San_Juan", 
               names_to = "basin",
               values_to = "SWSI")


#Joining data 
Diversions = 
  Diversions %>%
  mutate(yr = lubridate::year(Date)) %>%
  mutate(mo = lubridate::month(Date)) %>%
  dplyr::select(Structure, yr, mo, Amount) %>%
  group_by(Structure, yr, mo) %>%
  mutate(date = paste(yr, mo, "1", sep="-")) %>%
  rename(Date = date) 

Diversions$Date = as.Date(Diversions$Date)

#panicked writing to csv bc i didn't know how i did it but now i do so i might delete when i have time 

write_csv(Diversions,file = "data/processed/DiversionsDateMatchSWSI") 

CombinedData <- full_join(Diversions,SWSI, 
                          by = "Date") 
#panicked writing to csv bc i didn't know how i did it but now i do so i might delete when i have time 
write_csv(CombinedData,file = "data/processed/DiversionsANDSWSI")

#### Read data for linear regression ####

CombinedData <- read_csv("data/processed/DiversionsANDSWSI", )
  
View(CombinedData)
#### linear model (no random effects) ####

## plot the data ####
Diversions %>%
  ggplot(aes(x = Structure, y = Amount)) + 
  geom_boxplot()

SWSI %>% ggplot(aes(x = basin, y = SWSI)) + 
  geom_boxplot()

CombinedData %>%
  ggplot(aes(x = Structure, y = Amount, color = basin)) + 
  geom_boxplot()


# create the linear model
#response variable 
linearmodel <- lm(Amount ~ SWSI * basin, data = CombinedData, na.action=na.omit)
plot(linearmodel) # check assumptions



# run type 3 ANOVA
Anova(linearmodel, type = 3)

m1 <- lm(Amount ~ SWSI * basin, data = CombinedData, na.action=na.omit) #does not fit
m2 <- lm(Amount ~ SWSI + basin, data = CombinedData, na.action=na.omit) #does not fit
m3 <- lm(Amount ~ basin, data = CombinedData, na.action=na.omit) #does not fit
m4 <- lm(Amount ~ SWSI, data = CombinedData, na.action=na.omit) #does not fit 
null <- lm(Amount ~  1, data = CombinedData, na.action=na.omit) #does not fit 

plot(m1)
plot(m2)
plot(m3)
plot(m4)
plot(null)

AICc(m1, m2, m3, m4, null)
#
#df     AICc
#m1   15 285500.2
#m2    9 285499.5
#m3    8 285612.2
#m4    3 285488.9 **
#null  2 291178.0

anova(m1, m4)
#I'm not sure what this means? 

#post-hoc test
CombinedData %>%
  select(basin, Amount, SWSI) %>%
  drop_na() %>%
  ggplot(aes(x = SWSI, y = Amount)) + 
  geom_boxplot() + facet_grid(~basin)


# tukey test comparing species for females and for males
emmeans(m1, pairwise ~ SWSI | basin)
#Doesn't calculate :( 


post_hoc_penguins <- emmeans(mass_species_m1, pairwise ~ species | sex)

post_hoc_penguins$emmeans %>%
  as.data.frame() %>%
  ggplot(aes(x = species, y = emmean, color = sex)) + 
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), size = 2) + 
  ylab("Body Mass") + theme_bw()


#### Adams Tunnel Only#### 
# create the linear model
#response variable 
AdamsOnly_Colorado <-
  CombinedData %>%
  group_by(basin,Structure) %>%
  filter(basin == c("Colorado","South_Platte"))

?filter
View(AdamsOnly_Colorado)

linearmodel <- lm(Amount ~ SWSI * basin, data = AdamsOnly_Colorado, na.action=na.omit)



plot(linearmodel) # check assumptions


# run type 3 ANOVA
Anova(linearmodel, type = 3)

m1 <- lm(Amount ~ SWSI * basin, data = CombinedData, na.action=na.omit) #does not fit
m2 <- lm(Amount ~ SWSI + basin, data = CombinedData, na.action=na.omit) #does not fit
m3 <- lm(Amount ~ basin, data = CombinedData, na.action=na.omit) #does not fit
m4 <- lm(Amount ~ SWSI, data = CombinedData, na.action=na.omit) #does not fit 
null <- lm(Amount ~  1, data = CombinedData, na.action=na.omit) #does not fit 

plot(m1)
plot(m2)
plot(m3)
plot(m4)
plot(null)

AICc(m1, m2, m3, m4, null)
#
#df     AICc
#m1   15 285500.2
#m2    9 285499.5
#m3    8 285612.2
#m4    3 285488.9 **
#null  2 291178.0

anova(m1, m4)
#I'm not sure what this means? 

#post-hoc test
CombinedData %>%
  select(basin, Amount, SWSI) %>%
  drop_na() %>%
  ggplot(aes(x = SWSI, y = Amount)) + 
  geom_boxplot() + facet_grid(~basin)


# tukey test comparing species for females and for males
emmeans(m1, pairwise ~ SWSI | basin)
#Doesn't calculate :( 


post_hoc_penguins <- emmeans(mass_species_m1, pairwise ~ species | sex)

post_hoc_penguins$emmeans %>%
  as.data.frame() %>%
  ggplot(aes(x = species, y = emmean, color = sex)) + 
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), size = 2) + 
  ylab("Body Mass") + theme_bw()


