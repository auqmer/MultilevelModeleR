#**************************************************************************
# Title: moodExample_HoffmanCh4.R
# Author: William Murrah
# Description:
# Created: Friday, 09 November 2018
# R version: R version 3.5.1 (2018-07-02)
# Directory: /home/wmmurrah/Projects/GLMsimulation
#**************************************************************************
# packages used -----------------------------------------------------------
library(tidyverse)  
library(car)
library(psych)
library(nlme)
library(lamisc)

mood <- haven::read_dta("data/STATA_Chapter4.dta")

mood <- transform(mood,
                  studyday = factor(studyday),
                  personid = factor(personid)
)

# mood <- mood %>% 
#   mutate(studyday = factor(studyday),
#          personid = factor(personid)) 

# Reshape data
 a.wide2 <- reshape(a.long2, direction="wide", v.names="c",
 idvar="id", timevar="time")

moodwide <- reshape(mood, direction = "wide", v.names = "posmood",
                     idvar = "personid", timevar = "studyday")

moodwide <- mood %>% 
  spread(studyday, posmood)

headTail(mood)


mood %>% 
  group_by(studyday) %>% 
  summarise(mean = mean(posmood),
            SD = sd(posmood),
            Freq. = n())

aggregate(posmood ~ studyday, mood, 
          FUN = function(x)  c(M = round(mean(x),2),
                                   SD = round(sd(x), 2),
                                   Freq. = (length(x))))



ggplot(mood, aes(studyday, posmood, group = personid, color = personid)) + 
  geom_line() +  # geom_point() +
  stat_summary(fun = mean, geom = "line", lwd = 2, color = "red",
               aes(group = 1)) + 
  theme(legend.position = "none")

# Models ------------------------------------------------------------------
mod0 <- lme(fixed = posmood ~ 1, mood, random = ~1 | personid)
nmod1 <- lme(fixed = posmood ~ studyday, random = ~ 1 | personid, data = mood,
             weights = varIdent(form=~1 | studyday))

unstruct <- gls(posmood ~ studyday, mood, 
                correlation=corSymm(form = ~ 1 |personid),  
                weights=varIdent(form = ~ 1|studyday),method = "REML")
summary(unstruct)
getVarCov(unstruct)
unstruct$modelStruct$corStruct

# Compound Symmetry -------------------------------------------------------
comsym <- gls(posmood ~ studyday, mood, 
              correlation=corCompSymm(form = ~ 1 |personid),
              method="REML")
summary(comsym)
getVarCov(comsym)
comsym$modelStruct$corStruct

# Visualize the 
cc <- corMatrix(comsym$modelStruct$corStruct)[[5]]
cc
# CS Heterogeneous --------------------------------------------------------

hetercom <- gls(posmood ~ studyday, mood, 
                correlation=corCompSymm(form = ~ 1 |personid),
                weights=varIdent(form = ~1|studyday), 
                method="REML")
getVarCov(hetercom)

# Autoregressive ----------------------------------------------------------

auto1 <- gls(posmood ~ studyday, mood, 
             correlation=corAR1(form = ~ 1 |personid),
             method="REML")
cc <- corMatrix(auto1$modelStruct$corStruct)[[5]]
print(cc)

cc * auto1$sigma^2

# Autoregressive Heterogeneous --------------------------------------------

hauto1 <- gls(posmood ~ studyday, mood, 
              correlation=corAR1(form = ~ 1 |personid), 
              weights=varIdent(form = ~1|studyday), method="REML")
corandcov(hauto1)



# Toeplitz ----------------------------------------------------------------

toep <- gls(posmood ~ studyday, mood, 
            correlation=corARMA(form = ~ 1 |personid, p=3, q=0), 
            method="REML")
cc <- corMatrix(toep$modelStruct$corStruct)[[5]]
print(cc)
cc * toep$sigma^2


anova(unstruct,comsym,hetercom,auto1,hauto1,toep)
