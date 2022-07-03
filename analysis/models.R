#==============================================================================#
# ::::: script header ::::: ----
#==============================================================================#

# title: script for running models on AWS
# author: fred zenker
# created: 2020-05-11
# updated: 2022-06-10

#==============================================================================#
# ::::: packages ::::: ----
#==============================================================================#

# load packages ...

library(tidyverse) # for data processing
library(lmerTest) # for mixed-effects modeling
library(beepr) # for notifications
library(tictoc) # for timing operations
library(emmeans) #  for post-hoc tests
library(performance) # for checking model performance
library(ordinal) # for clmm models

#==============================================================================#
# ::::: self-paced reading task (sprt) :::::  ----
#==============================================================================#

#------------------------------------------------------------------------------#
# modeling: proficiency ~ accuracy ----
#------------------------------------------------------------------------------#

# read in data ...

ct <- read_csv('data/ctest_scored.csv', col_types = cols(.default = 'f', accuracy = 'l'))

proficiency <- ct %>%
  group_by(study, group, participant) %>%
  summarise(proficiency = sum(accuracy, na.rm=T)) %>%
  ungroup()

spr_crit_clean <- read_csv('data/spr_crit_clean.csv')

# set class of columns ...

md <- spr_crit_clean %>%
  mutate(dependency = as.factor(dependency),
         environment = as.factor(environment),
         group = as.factor(group),
         accuracy = as.logical(accuracy)) %>%
  mutate(environment = fct_relevel(environment, 'short', 'long', 'island'))

# check minimum accuracy rate

min(md$acc_rate)

# add mean-centered proficiency scores ...

md <- md %>%
  left_join(proficiency, by = c('study', 'group', 'participant')) %>%
  mutate(proficiency = as.numeric(proficiency)) %>%
  group_by(study) %>%
  mutate(proficiency = scale(proficiency, scale = FALSE)) %>%
  ungroup()

# filter for analysis ...

md <- md %>%
  filter(group %in% c('korean', 'mandarin')) %>%
  mutate(group = fct_drop(group))

# check participants ...

check <- md %>%
  group_by(study, group, participant) %>%
  summarise() %>%
  ungroup() %>%
  group_by(study, group) %>%
  summarise(n = n()) %>%
  ungroup()

# remove participants with no proficiency scores ...

md <- md %>%
  filter(is.na(proficiency) == FALSE)

# check participants ...

check <- md %>%
  group_by(study, group, participant) %>%
  summarise() %>%
  ungroup() %>%
  group_by(study, group) %>%
  summarise(n = n()) %>%
  ungroup()

summary(md)

# apply deviation coding ...

contrasts(md$dependency) <- contr.treatment(2) - matrix(rep(1/2, 2), ncol=1)
contrasts(md$environment) <- contr.treatment(3) - matrix(rep(1/3, 6), ncol=2)
contrasts(md$group) <- contr.treatment(2) - matrix(rep(1/2, 2), ncol=1)

# view contrasts ...

contrasts(md$dependency)
contrasts(md$environment)
contrasts(md$group)

#------------------------------------------------------------------------------#
# + orc ----
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# + + model 1 
#------------------------------------------------------------------------------#

tic()
model1 <- glmer(accuracy ~ proficiency * dependency * environment * group + 
                  (1 | participant) + 
                  (1 | item), 
                data = filter(md, study == '210510_do'), family = binomial, 
                control = glmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_orc_proficiency_acc_md1.rds')
summary(model1)
toc()
beep()