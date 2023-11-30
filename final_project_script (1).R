## Project:  STA 215, Fall 2023, Final Project
# Located:   Kline TCNJ Google Drive
# File Name: template
# Date:      2023_11_16
# Who:       Zachary D. Kline



## Load packages
# NOTE: Run base.R if these commands return an error!
library(readr)
library(dplyr)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(haven)
library(forcats)
library(psych)

# Load data 
data <- read_delim("raw_data.csv")



##################################################################################
############### STEP 1: Table 1    ####################   
##################################################################################

# EXAMINE Changes_Tempo
table(data$Changes_Tempo)
mean(data$Changes_Tempo)
sd(data$Changes_Tempo)
describe(data$Changes_Tempo)
summary(data$Changes_Tempo)

# EXAMINE Song_Reaction
table(data$Song_Reaction)
mean(data$Song_Reaction)
sd(data$Song_Reaction)
describe(data$Song_Reaction)
summary(data$Song_Reaction)

# EXAMINE Chorus_Repeated
table(data$Chorus_Repeated)
mean(data$Chorus_Repeated)
sd(data$Chorus_Repeated)
describe(data$Chorus_Repeated)
summary(data$Chorus_Repeated)

# EXAMINE Title_Repeated
table(data$Title_Repeated)
mean(data$Title_Repeated)
sd(data$Title_Repeated)
describe(data$Title_Repeated)
summary(data$Title_Repeated)

# EXAMINE High_Notes
table(data$High_Notes)
mean(data$High_Notes)
sd(data$High_Notes)
describe(data$High_Notes)
summary(data$High_Notes)

# EXAMINE Beat_Drops
table(data$Beat_Drops)
mean(data$Beat_Drops)
sd(data$Beat_Drops)
describe(data$Beat_Drops)
summary(data$Beat_Drops)

# EXAMINE Bridge_Exists
table(data$Bridge_Exists)
mean(data$Bridge_Exists)
sd(data$Bridge_Exists)
describe(data$Bridge_Exists)
summary(data$Bridge_Exists)

# EXAMINE Happiness_Level
table(data$Happiness_Level)
mean(data$Happiness_Level)
sd(data$Happiness_Level)
describe(data$Happiness_Level)
summary(data$Happiness_Level)

##################################################################################
####################  STEP 2: Table  2             ####################   
##################################################################################

table(data$Bridge_Exists, data$Song_Reaction)

##################################################################################
####################   STEP 3: Chi squared test             ####################   
##################################################################################

chisq.test(table(data$Bridge_Exists, data$Song_Reaction))

##################################################################################
####################  STEP 4: ANOVA                ####################   
##################################################################################
anova_adapted <- aov(Song_Reaction ~ Changes_Tempo, data= data)
summary(anova_adapted)
##################################################################################
####################  STEP 5: Correlation                ####################   
##################################################################################
plot(data$Beat_Drops, data$Happiness_Level)
cor(data$Beat_Drops, data$Happiness_Level)

##################################################################################
####################  STEP 6: Linear Regression                ####################   
##################################################################################
linear_relationship <- lm(Beat_Drops ~ Happiness_Level, data=data)
summary(linear_relationship)

##################################################################################
####################  STEP 7: Figure 1                                ####################   
##################################################################################
plot(data$Beat_Drops, data$Happiness_Level)
summary(linear_relationship)
abline(linear_relationship, col = "red")

##################################################################################
####################  STEP 8: Examine residuals                     ####################   
##################################################################################
plot(data$Beat_Drops, residuals (linear_relationship))
abline (h= 0, col = "red")