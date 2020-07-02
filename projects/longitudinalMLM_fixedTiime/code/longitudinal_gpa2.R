#************************************************************************
# Title: longitudinalGPA2.R
# Author: William Murrah
# Description: Import and convert gpa data to csv from Chapter 5 of Hox
#              and conduct analysis
# Created: Wednesday, 01 July 2020
# R version: R version 4.0.2 (2020-06-22)
# Project(working) directory: /home/wmmurrah/Courses/ERMA_Multilevel_Modeling
#************************************************************************

# install the haven package which allows importing SPSS data, among other 
# formats (Stata, SAS)
library(haven) 
library(lme4)
library(lmerTest)
library(ggplot2)
library(texreg)
library(performance)
library(interactions)
# Import Data -------------------------------------------------------------


gpa2long <- read_sav("data/gpa2long.sav")


# Clean Data --------------------------------------------------------------

gpa2long <- within(gpa2long, {
  student <- factor(student)
  gender <- factor(sex, labels = c("male", "female"))
  admitted <- factor(admitted, labels = c("no", "yes"))
})

# Explore Data ------------------------------------------------------------

ggplot(gpa2long, aes(y = gpa, x = occas, group = student)) + 
  geom_line()

ggplot(gpa2long, aes(y = gpa, x = occas, group = student)) + 
  geom_smooth(method = "lm", se = FALSE)

# Create some more plots exploring the covariates

# Models ------------------------------------------------------------------

mod0 <- lmer(gpa ~ 1 + (1 | student), gpa2long, REML = FALSE)
modtime <- lmer(gpa ~ occas + (1 | student), gpa2long, REML = FALSE)
modtjob <- lmer(gpa ~ occas + job + (1 | student), gpa2long, REML = FALSE)
modfull <- lmer(gpa ~ occas + job + highgpa + gender + (1 | student), gpa2long,
                REML = FALSE)
summary(modfull)

screenreg(list(mod0, modtime, modtjob, modfull), digits = 3)
 

# Testing Nonlinear trends in gpa growth
modtimepoly <- lmer(gpa ~ poly(occas, 2) + (1 | student), gpa2long, REML = FALSE)
summary(modtimepoly)

# Adding Random Slopes ----------------------------------------------------

modfullrs <- lmer(gpa ~ occas + job + highgpa + gender + (occas |student), 
                  gpa2long, REML = FALSE)
summary(modfullrs)

screenreg(list(modtime, modfullrs))

# Test the addition of the random slope for occasions.
ranova(modfullrs)
anova(modfull, modfullrs) # equivalent


# Cross-level Interaction occassion*gender --------------------------------

modfullrsx <- lmer(gpa ~ occas + job + highgpa + gender + gender:occas + 
                     (occas |student), 
                  gpa2long, REML = FALSE)
screenreg(list(modfull, modfullrs, modfullrsx), digits = 3)

# Get correlation between variance components 
VarCorr(modfullrsx)

# Figure 5.4, p. 82.
interact_plot(modfullrsx, occas, gender)


# What about smaller sample size (at level 2)? ----------------------------

# create a smaller data set by sampling individuals from the full data:
n <- 12 # level 2 sample size. You can change this to play around with n.
# Index of sampled students
set.seed(20200702) # set the random seed for reproducibility
sampidx <- as.numeric(sample(unique(gpa2long$student), replace = FALSE, size = n)) 

# Create smaller sample data
gpa2long12 <- gpa2long[gpa2long$student %in% sampidx, ]

