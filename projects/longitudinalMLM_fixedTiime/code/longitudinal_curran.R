#************************************************************************
# Title: longitudinal_curran.R
# Author: William Murrah
# Description: Varying time example of longitudinal mixed-effects model
#              from chapter 5 of Hox, Moerbeek, & van de Schoot.
# Created: Monday, 06 July 2020
# R version: R version 4.0.2 (2020-06-22)
# Project(working) directory: /home/wmmurrah/Projects/QMER/MultilevelModeleR/projects/longitudinalMLM_fixedTiime
#************************************************************************

library(haven) 
library(tidyverse)
library(psych)
library(lme4)
library(lmerTest)
library(ggplot2)
library(texreg)
library(performance)
library(interactions)

# import and clean data ---------------------------------------------------
curran <- read_sav("data/CurranData.sav")

curran <- within(curran, {
  id <- factor(id)
  kidgen <- factor(kidgen, labels = c("girl", "boy"))
})

# Create long version of data
curranlong <- curran %>% 
  gather( key = key, value = val, anti1:read4) %>% 
  separate(col = key, into = c("variable", "occasion"), sep = 4) %>% 
  spread(key = variable, value = val) %>% 
  arrange(id, occasion) %>% 
  mutate(time = as.numeric(occasion)-1,
         kidagetv = kidage + 2*time,
         kidage6 = kidagetv - 6,
         kidagesqr = kidage6^2,
         c_momage = momage - mean(momage),
         c_homecog = homecog - mean(homecog),
         c_homeemo = homeemo - mean(homeemo)) %>% 
  filter(!is.na(read) | !is.na(anti))


# Missing data analysis ---------------------------------------------------

aggr(curranlong, numbers = TRUE)

# Explore Data ------------------------------------------------------------

# Compare the linear (green) and quadratic (red) functions to the 
# loess (blue) smoothed line.
ggplot(curranlong, aes(x = kidagetv, y = read)) + geom_point() +  
  geom_smooth(method = "lm", color = "green", se = FALSE) +
  geom_smooth(se = FALSE) +
  geom_smooth(method = "lm", formula = y  ~ poly(x, 2), color = "red", 
              se = FALSE)

# Compare gender
ggplot(curranlong, aes(x = kidagetv, y = read, color = kidgen)) + 
  geom_point() + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 2))

# Run additional graphs explore the relations between variables.

# Models ------------------------------------------------------------------

mod0 <- lmer(read ~ 1 + (1 | id), curranlong, REML = FALSE)
modage <- lmer(read ~ kidage6 + (1 | id), curranlong, REML = FALSE)
modage2 <- update(modage, . ~ . + kidagesqr)
modage2_2 <- update(modage2, . ~ . + c_momage + c_homecog + c_homeemo)
modage2_2rs <- update(modage2_2, . ~ . - (1 | id) + (kidage6 | id))
modage2_2rsx <- update(modage2_2rs, . ~ . + kidage6:c_momage + 
                      kidage6:c_homeemo + kidage6:c_homecog)
modage2_2rsx2 <- update(modage2_2rsx, . ~ . - kidage6:c_homecog)


anova(modage2_2rsx, modage2_2rsx2)

summary(modage)
summary(modage2)
summary(modage2_2)
summary(modage2_2rs)
summary(modage2_2rsx)
summary(modage2_2rsx2)


# Does the slope for age vary across individuals?
anova(modage2_2, modage2_2rs)

# Evidence of cross-level interactions?
anova(modage2_2rs, modage2_2rsx)

screenreg(list(modage, modage2, modage2_2, modage2_2rs, modage2_2rsx2), digits = 3,
          custom.model.names = c("Model 0", "Model 1", "Model 2", "Model 3",
                                 "Model 4"))

# Interactions for final model
# Create model with uncentered variables for plots
modfull <- lmer(read ~ kidagetv + I(kidagetv^2) + momage +  
    homecog + homeemo + kidagetv:momage + kidagetv:homeemo + (kidagetv | id), 
    curranlong, REML = FALSE)

interact_plot(modfull, kidagetv, momage, modx.values = c(21, 25.5,  29))
interact_plot(modfull, kidagetv, homeemo, modx.values = c(0, 9.2, 13))

sim_slopes(modfull, kidagetv, momage, jnplot = TRUE, johnson_neyman = TRUE)
sim_slopes(modfull, kidagetv, homeemo, jnplot = TRUE, johnson_neyman = TRUE)
