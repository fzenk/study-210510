#------------------------------------------------------------------------------#
#### script info ####
#------------------------------------------------------------------------------#

# title: analysis for study 210510
# contributors: fred zenker
# created: 2020-05-11
# updated: 2022-04-11
# see key at end of script for list of abbreviations

#------------------------------------------------------------------------------#
#### packages ####
#------------------------------------------------------------------------------#

# load packages
library(tidyverse) # for data processing
library(lmerTest) # for mixed-effects modeling
library(readxl) # for reading .xlsx files
library(base64enc) # for converting recordings from base64 to audio files
library(stringdist) # for calculating edit distance on c-test responses
library(hunspell) # for spell checking of c-test responses
library(patchwork) # for plotting
library(jsonlite) # for unpacking json
library(beepr) # for notifications
library(tictoc) # for timing operations
library(ggh4x) # for plotting
library(emmeans) #  for post-hoc tests; see https://marissabarlaz.github.io/portfolio/contrastcoding/

citation('performance')

#------------------------------------------------------------------------------#
#### read in data ####
#------------------------------------------------------------------------------#

df <- read_csv('data/data.csv', col_types = cols(.default = 'c')) %>%
  select(-audio_data)

ct <- read_csv('data/ctest_scored.csv', col_types = cols(.default = 'f', accuracy = 'l'))

library(report)

mod1 <- readRDS('analysis/models/orc_spr_doen_region1_mod.rds')

report(mod1)

#------------------------------------------------------------------------------#
#### plot number of participants per task ####
#------------------------------------------------------------------------------#

# summarise

plot <- df %>%
  group_by(study, group, task, participant) %>%
  summarise() %>%
  ungroup() %>%
  group_by(study, group, task) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(task = as.factor(task), group = as.factor(group),
         study = case_when(study == '210510_do' ~ 'ORC',
                           study == '210510_su' ~ 'SRC'))

# plot

ggplot(data = plot, aes(x = task, y = n, group = group, col = group, shape = group, label = n)) +
  geom_line(lwd = 1) +
  geom_point(size = 2) +
  geom_text(size = 3, col = 'black', nudge_y = 1.5) +
  theme_classic() +
  scale_x_discrete(name='task', limits = c('language_survey', 'ept', 'sprt', 'english_ajt', 'korean_ajt', 'mandarin_ajt', 'ctest', 'exit_survey'), 
                   labels = c('lbq', 'ept', 'spr', 'eajt', 'kajt', 'majt', 'ctest', 'exit')) +
  scale_y_continuous(name='participants', limits=c(50, 100)) +
  theme(text = element_text(size = 12),
        legend.position = 'bottom') +
  facet_wrap(~study)

# save

ggsave("plots/participant_counts.png", width=6.5, height=4.5, dpi=600)

#------------------------------------------------------------------------------#
#### exit survey ####
#------------------------------------------------------------------------------#

exit <- df %>%
  filter(task == 'exit_survey') %>%
  arrange(study, group, participant) %>%
  select_if(function(x){!all(is.na(x))})

#------------------------------------------------------------------------------#
#### language survey ####
#------------------------------------------------------------------------------#

survey <- df %>%
  filter(task == 'language_survey') %>%
  arrange(study, group, participant) %>%
  select_if(function(x){!all(is.na(x))})

age <- survey %>%
  filter(item == 'age') %>%
  mutate(response = as.numeric(response)) %>%
  group_by(study, group) %>%
  summarise(mean = mean(response, na.rm = TRUE),
            sd = sd(response, na.rm = TRUE),
            min = min(response, na.rm = TRUE),
            max = max(response, na.rm = TRUE)) %>%
  ungroup()
rm(age)

aoa <- survey %>%
  filter(item == 'english_aoa') %>%
  mutate(response = as.numeric(response)) %>%
  group_by(study, group) %>%
  summarise(mean = mean(response, na.rm = TRUE),
            sd = sd(response, na.rm = TRUE),
            min = min(response, na.rm = TRUE),
            max = max(response, na.rm = TRUE)) %>%
  ungroup()
rm(aoa)

lor <- read_csv('data/length_of_residence.csv', col_types = cols(.default = 'f', accuracy = 'l'))

lor <- lor %>%
  mutate(study = case_when(str_detect(participant, 'do') == TRUE ~ '210510_do',
                           str_detect(participant, 'su') == TRUE ~ '210510_su'),
         group = case_when(str_detect(participant, 'en') == TRUE ~ 'english',
                           str_detect(participant, 'ko') == TRUE ~ 'korean',
                           str_detect(participant, 'zh') == TRUE ~ 'mandarin')) %>%
  select(study, group, participant, lor) %>%
  mutate(lor = as.numeric(as.character(lor)))

lor <- lor %>%
  group_by(study, group) %>%
  summarise(mean = mean(lor, na.rm = TRUE),
            sd = sd(lor, na.rm = TRUE),
            min = min(lor, na.rm = TRUE),
            max = max(lor, na.rm = TRUE)) %>%
  ungroup()

#------------------------------------------------------------------------------#
#### ctest: prep dataframe ####
#------------------------------------------------------------------------------#

ct <- df %>% filter(task == 'ctest') %>%
  arrange(study, group, participant) %>%
  select_if(function(x){!all(is.na(x))})

ct <- ct %>%
  mutate(response = str_remove_all(response, '[^[:alnum:]]'))

#------------------------------------------------------------------------------#
#### ctest: score responses ####
#------------------------------------------------------------------------------#

# check number of participants per group

check <- ct %>%
  group_by(study, group, participant) %>%
  summarise() %>%
  ungroup() %>%
  group_by(study, group) %>%
  summarise(n = n()) %>%
  ungroup()

# drop unneeded columns

ct <- ct %>% select(study, group, participant, item, response)

# read in correct answers

answers <- read_csv('data/answers.csv', col_types = cols(.default = 'c')) %>%
  select(item, word, onset, exact, acceptable)

# add correct answers to dataframe

ct <- ct %>% left_join(answers, by = 'item')

# correct spelling and determine accuracy

ct <- ct %>%
  mutate(response = str_trim(tolower(response)),
         onset = str_trim(tolower(onset)),
         acceptable = str_trim(tolower(acceptable)),
         word = str_trim(tolower(word))) %>%
  mutate(response = case_when(str_detect(item, 'brown') ~ response,
                              TRUE ~ str_c(onset, response))) %>%
  mutate(response_regex = str_c('\\b', response, '\\b')) %>%
  mutate(match1 = str_detect(acceptable, response_regex)) %>%
  mutate(spelling = case_when(match1 == FALSE ~ as.character(hunspell_check(response)))) %>%
  mutate(suggestion = case_when(spelling == FALSE ~ as.character(hunspell_suggest(response)))) %>%
  mutate(suggestion_regex = str_replace_all(suggestion, c('c\\("' = '\\\\b', 
                                                          '", "' = '\\\\b|\\\\b',
                                                          '"\\)' = '\\\\b'))) %>%
  mutate(match2 = str_detect(acceptable, suggestion_regex)) %>%
  mutate(accuracy = case_when(match1 == TRUE ~ TRUE,
                              match2 == TRUE ~ TRUE,
                              TRUE ~ FALSE))

beep(1)

# make list of proficiency scores

proficiency <- ct %>%
  group_by(study, group, participant) %>%
  summarise(proficiency = mean(accuracy, na.rm=T)) %>%
  ungroup()

check <- proficiency %>%
  mutate(proficiency = proficiency * 100) %>%
  group_by(study, group) %>%
  summarise(mean = mean(proficiency, na.rm = TRUE),
            sd = sd(proficiency, na.rm = TRUE),
            min = min(proficiency, na.rm = TRUE),
            max = max(proficiency, na.rm = TRUE)) %>%
  ungroup()

check <- proficiency %>%
  group_by(study, group) %>%
  summarise(n = n()) %>%
  ungroup()

# check incorrect responses

check <- ct %>%
  filter(accuracy == FALSE) %>%
  select(study, group, participant, item, word, response, accuracy)

# write to csv

write_csv(ct, 'data/ctest_scored.csv')

#------------------------------------------------------------------------------#
#### ctest: plot ####
#------------------------------------------------------------------------------#

check <- ct %>%
  group_by(study, group, participant) %>%
  summarise() %>%
  ungroup() %>%
  group_by(study, group) %>%
  summarise(n = n()) %>%
  ungroup()

# summarise for plotting 

plot <- ct %>%
  group_by(study, group, participant) %>%
  summarise(accuracy = mean(accuracy)) %>%
  ungroup()

check <- plot %>%
  group_by(study, group) %>%
  summarise(mean = mean(accuracy),
            min = min(accuracy),
            max = max(accuracy)) %>%
  ungroup()

check <- ct %>%
  group_by(study, group) %>%
  summarise(mean = mean(accuracy)) %>%
  ungroup()

# define plot data

p1 <- ggplot(data=filter(plot, study == '210510_do'), aes(x=group, y=accuracy*100, fill=group, label=participant))
p2 <- ggplot(data=filter(plot, study == '210510_su'), aes(x=group, y=accuracy*100, fill=group, label=participant))

# plot styling

s <- list(
  geom_hline(yintercept=50),
  geom_violin(fill = 'lightblue'),
  geom_boxplot(width = .1, fill='white'),
  #geom_jitter(size=1, shape=1, alpha=.25, position = position_jitter(seed=2, width=.15)),
  #geom_text(size = 2.5, col = "black", position = position_jitter(seed=2)),
  theme_classic(),
  scale_x_discrete(name="group",
                   limits = c('english', 'korean', 'mandarin'), labels = c('ENS', 'KLE', 'MLE')),
  scale_y_continuous(name="% accuracy",
                     limits=c(0, 100)),
  theme(text = element_text(size = 12),
        plot.title = element_text(size = 12, hjust = .5),
        legend.position = "hide")
)

# print plots

p1 + s
ggsave("plots/orc/ctest.png", width=6, height=2, dpi=600)

p2 + s
ggsave("plots/src/ctest.png", width=6, height=2, dpi=600)

#------------------------------------------------------------------------------#
#### ctest: modeling ####
#------------------------------------------------------------------------------#

md <- ct %>%
  filter(study == '210510_do')

contrasts(md$group)

md <- md %>%
  mutate(group = fct_relevel(group, 'korean', 'mandarin', 'english'))

model1 <- glmer(accuracy ~ group + (1 | participant) + (1 + group | item), 
                data = md, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(model1)
beepr::beep(1)

# reference level = english
# Fixed effects:
#               Estimate Std. Error z value Pr(>|z|)    
# (Intercept)     3.0152     0.3248   9.282   <2e-16 ***
# groupkorean    -2.4521     0.2881  -8.511   <2e-16 ***
# groupmandarin  -2.5050     0.2942  -8.514   <2e-16 ***

# reference level = korean
# Fixed effects:
#               Estimate Std. Error z value Pr(>|z|)    
# (Intercept)    0.56316    0.27606   2.040   0.0413 *  
# groupmandarin -0.05295    0.18682  -0.283   0.7768    
# groupenglish   2.45207    0.28810   8.511   <2e-16 ***

#------------------------------------------------------------------------------#
#### ept: prep dataframe ####
#------------------------------------------------------------------------------#

ep <- df %>%
  filter(task == 'ept') %>%
  arrange(study, group, participant) %>%
  select_if(function(x){!all(is.na(x))})

#------------------------------------------------------------------------------#
#### ept: inter-rater reliability ####
#------------------------------------------------------------------------------#

temp <- ep %>%
  mutate(agree = case_when(type_rater1 == type_rater2 ~ TRUE,
                           TRUE ~ FALSE))

check <- temp %>%
  group_by(study, group) %>%
  summarise(irr = mean(agree, na.rm = TRUE)) %>%
  ungroup()

# study     group      irr
# <chr>     <chr>    <dbl>
# 1 210510_do english  0.966
# 2 210510_do korean   0.949
# 3 210510_do mandarin 0.934
# 4 210510_su english  0.980
# 5 210510_su korean   0.953
# 6 210510_su mandarin 0.899

#------------------------------------------------------------------------------#
#### ept: bar plot critical ####
#------------------------------------------------------------------------------#

# remove nontarget responses

target <- ep %>%
  filter(type != 'nontarget')

# summarise for plotting

plot <- target %>%
  filter(is.na(condition) == FALSE) %>%
  mutate(type = as.character(type)) %>%
  group_by(study, group, condition) %>%
  count(type) %>%
  ungroup() %>%
  complete(type, nesting(condition), fill = list(n = 0)) %>%
  group_by(study, group, condition) %>%
  mutate(sum = sum(n)) %>%
  ungroup() %>%
  mutate(prop = (n/sum)*100) %>%
  mutate(prc = as.character(round(prop))) %>%
  mutate(prc = as.numeric(case_when(prc == '0' ~ NA_character_, TRUE ~ prc))) %>%
  mutate(type = factor(type, levels = c('gap', 'other', 'resumption')))

# facet labels

groups <- c(`english` = 'ENS', `korean` = 'KLE', `mandarin` = 'MLE')

# define data for plots

p1 <- ggplot(data=filter(plot, study == '210510_do'), aes(x=condition, y=prop, group=type, fill=type, label=prc))
p2 <- ggplot(data=filter(plot, study == '210510_su'), aes(x=condition, y=prop, group=type, fill=type, label=prc))

# add styling

s <- list(
  geom_bar(stat = "identity", col = "black", width = .5, alpha=.8),
  geom_text(size = 3, col = "black", position = position_stack(vjust = 0.5)),
  theme_classic(),
  scale_x_discrete(name="environment", limits = c('cond1', 'cond2', 'cond3'), labels = c('short', 'long', 'island')),
  scale_y_continuous(name="% responses", limits=c(0, 101), breaks=c(0, 20, 40, 60, 80, 100)),
  scale_fill_manual(name="dependency", values=c("#648fff", "gray", "#ffb000")),
  theme(text = element_text(size = 12), plot.title = element_text(size = 12, hjust = .5), legend.position = "right", legend.margin=margin(1, 1, 1, 1)),
  facet_wrap(~group, labeller = as_labeller(groups))
)

# print plots

p1 + s
ggsave("plots/orc/ept_barplot.png", width=6.5, height=2.5, dpi=600)

p2 + s
ggsave("plots/src/ept_barplot.png", width=6.5, height=2.5, dpi=600)

#------------------------------------------------------------------------------#
#### ept: bar plot filler ####
#------------------------------------------------------------------------------#

# remove nontarget responses

target <- ep %>%
  filter(type != 'nontarget')

# summarise for plotting

plot <- target %>%
  filter(is.na(condition) == TRUE) %>%
  mutate(type = as.character(type)) %>%
  group_by(study, group) %>%
  count(type) %>%
  ungroup() %>%
  group_by(study, group) %>%
  mutate(sum = sum(n)) %>%
  ungroup() %>%
  mutate(prop = (n/sum)*100) %>%
  mutate(prc = as.character(round(prop))) %>%
  mutate(prc = as.numeric(case_when(prc == '0' ~ NA_character_, TRUE ~ prc))) %>%
  mutate(type = factor(type, levels = c('gap', 'other', 'resumption')))

# facet labels

groups <- c(`english` = 'ENS', `korean` = 'KLE', `mandarin` = 'MLE')

# define data for plots

p1 <- ggplot(data=filter(plot, study == '210510_do'), aes(x=group, y=prop, group=type, fill=type, label=prc))
p2 <- ggplot(data=filter(plot, study == '210510_su'), aes(x=group, y=prop, group=type, fill=type, label=prc))

# add styling

s <- list(
  geom_bar(stat = "identity", col = "black", width = .5, alpha=.8),
  geom_text(size = 3, col = "black", position = position_stack(vjust = 0.5)),
  theme_classic(),
  scale_x_discrete(name="group", limits = c('english', 'korean', 'mandarin'), labels = c('ENS', 'KLE', 'MLE')),
  scale_y_continuous(name="% responses", limits=c(0, 101), breaks=c(0, 20, 40, 60, 80, 100)),
  scale_fill_manual(name="dependency", values=c("#648fff", "gray", "#ffb000")),
  theme(text = element_text(size = 12), plot.title = element_text(size = 12, hjust = .5), legend.position = "right", legend.margin=margin(1, 1, 1, 1))
)

# print plots

p1 + s
ggsave("plots/orc/ept_barplot_filler.png", width=6.5, height=2.5, dpi=600)

p2 + s
ggsave("plots/src/ept_barplot_filler.png", width=6.5, height=2.5, dpi=600)

#------------------------------------------------------------------------------#
#### ept: bar plot critical nontarget ####
#------------------------------------------------------------------------------#

# make list of participants that excludes those who only gave nontarget responses

temp <- ep %>%
  filter(type != 'nontarget') %>%
  group_by(study, group, participant) %>%
  summarise() %>%
  ungroup()

# summarise for plotting

plot <- ep %>%
  filter(participant %in% temp$participant) %>%
  filter(is.na(condition) == FALSE) %>%
  mutate(type = as.character(type)) %>%
  group_by(study, group, condition) %>%
  count(type) %>%
  ungroup() %>%
  complete(type, nesting(condition), fill = list(n = 0)) %>%
  group_by(study, group, condition) %>%
  mutate(sum = sum(n)) %>%
  ungroup() %>%
  mutate(prop = (n/sum)*100) %>%
  mutate(prc = as.character(round(prop))) %>%
  mutate(prc = as.numeric(case_when(prc == '0' ~ NA_character_, TRUE ~ prc))) %>%
  mutate(type = factor(type, levels = c('gap', 'nontarget', 'other', 'resumption')))

# facet labels

groups <- c(`english` = 'ENS', `korean` = 'KLE', `mandarin` = 'MLE')

# define data for plots

p1 <- ggplot(data=filter(plot, study == '210510_do'), aes(x=condition, y=prop, group=type, fill=type, label=prc))
p2 <- ggplot(data=filter(plot, study == '210510_su'), aes(x=condition, y=prop, group=type, fill=type, label=prc))

# add styling

s <- list(
  geom_bar(stat = "identity", col = "black", width = .5, alpha=.8),
  geom_text(size = 3, col = "black", position = position_stack(vjust = 0.5)),
  theme_classic(),
  scale_x_discrete(name="environment", limits = c('cond1', 'cond2', 'cond3'), labels = c('short', 'long', 'island')),
  scale_y_continuous(name="% responses", limits=c(0, 101), breaks=c(0, 20, 40, 60, 80, 100)),
  scale_fill_manual(name="dependency", values=c('#648fff', 'gray60', 'gray85', '#ffb000'), labels = c('gap', 'nontarget', 'restructure', 'resumption')),
  theme(text = element_text(size = 12), plot.title = element_text(size = 12, hjust = .5), legend.position = "right", legend.margin=margin(1, 1, 1, 1)),
  facet_wrap(~group, labeller = as_labeller(groups))
)

# print plots

p1 + s
ggsave("plots/orc/ept_barplot_with_nontarget.png", width=6.5, height=2.75, dpi=600)

p2 + s
ggsave("plots/src/ept_barplot_with_nontarget.png", width=6.5, height=2.75, dpi=600)

#------------------------------------------------------------------------------#
#### ept: scatter plot of proficiency effects ####
#------------------------------------------------------------------------------#

# summarise data for plotting

plot <- ep %>%
  filter(condition %in% c('cond1', 'cond2', 'cond3')) %>%
  mutate(type = case_when(type == 'gap' ~ 'gap',
                              type == 'resumption' ~ 'resumption',
                              TRUE ~ 'other')) %>%
  mutate(type = as.character(type)) %>%
  group_by(study, group, participant) %>%
  count(type) %>%
  ungroup() %>%
  group_by(study, group, participant) %>%
  mutate(sum = sum(n)) %>%
  ungroup() %>%
  mutate(prop = (n/sum)*100) %>%
  mutate(prc = as.character(round(prop))) %>%
  mutate(prc = as.numeric(case_when(prc == '0' ~ NA_character_, TRUE ~ prc))) %>%
  mutate(type = factor(type, levels = c('gap', 'other', 'resumption')))

plot <- plot %>%
  complete(type, nesting(study, group, participant), fill = list(prop = 0, n = 0)) %>%
  select(study, group, participant, type, n, prop) %>%
  arrange(study, group, participant, type, prop)

plot <- plot %>%
  left_join(proficiency, by = c('study', 'group', 'participant')) %>%
  filter(proficiency != 'NA') %>%
  filter(type == 'resumption')

# facet labels

groups <- c(`english` = 'ENS', `korean` = 'KLE', `mandarin` = 'MLE')

# define data for plots

p1 <- ggplot(data=filter(plot, study == '210510_do'), aes(x=proficiency*100, y=prop))
p2 <- ggplot(data=filter(plot, study == '210510_su'), aes(x=proficiency*100, y=prop))

# generate plot

s <- list(
  geom_smooth(method=lm, col="#785ef0"), 
  geom_point(shape = 1),
  theme_classic(),
  scale_x_continuous(name='proficiency'),
  scale_y_continuous(name="% resumption", limits = c(-5, 100)),
  scale_fill_manual(name='group', values=c("#9b82f3", "#00a78f")),
  theme(text = element_text(size = 12),
        plot.title = element_text(size = 12, hjust = .5), 
        legend.position = "right"),
  facet_wrap(~group, labeller = as_labeller(groups))
)

# write plots

p1 + s
ggsave("plots/orc/ept_proficiency.png", width=6.5, height=3, dpi=600)

p2 + s
ggsave("plots/src/ept_proficiency.png", width=6.5, height=3, dpi=600)

#------------------------------------------------------------------------------#
#### ept: simple linear regression analysis ####
#------------------------------------------------------------------------------#

# simple regression analysis

md <- plot %>% filter(group == 'korean' & study == '210510_do')

cor(md$proficiency, md$prop) 

model <- lm(prop ~ proficiency, data = md)

summary(model)

# doen: ' ' (r-squared = 0.000434, F = 0.03778, p = 0.8463)
# doko: '.' (r-squared = 0.05509, F = 3.848, p = 0.05402)
# dozh: ' ' (r-squared = 0.006488, F = 0.4832, p = 0.4891)
# suen: ' ' (r-squared = 0.005779, F = 0.343, p = 0.5604)
# suko: ' ' (r-squared = 0.03603, F = 2.392, p = 0.1269)
# suzh: ' ' (r-squared = 0.00, F = 0.00, p = 0.997)

#------------------------------------------------------------------------------#
#### ept: bar plot critical by-item ####
#------------------------------------------------------------------------------#

# summarise for plotting

plot <- ep %>%
  filter(condition %in% c('cond1', 'cond2', 'cond3')) %>%
  mutate(category = case_when(type == 'gap' ~ 'gap',
                              type == 'resumption' ~ 'resumption',
                              TRUE ~ 'other')) %>%
  mutate(category = as.character(category)) %>%
  group_by(study, item, condition) %>%
  count(category) %>%
  ungroup() %>%
  complete(category, nesting(condition), fill = list(n = 0)) %>%
  group_by(study, item, condition) %>%
  mutate(sum = sum(n)) %>%
  ungroup() %>%
  mutate(prop = (n/sum)*100) %>%
  mutate(prc = as.character(round(prop))) %>%
  mutate(prc = as.numeric(case_when(prc == '0' ~ NA_character_, TRUE ~ prc))) %>%
  mutate(category = factor(category, levels = c('gap', 'other', 'resumption'))) %>%
  mutate(study = case_when(study == '210510_do' ~ 'ORC',
                           study == '210510_su' ~ 'SRC'))

# generate plot

ggplot(data=plot, aes(x=condition, y=prop, group=category, fill=category, label=prc)) +
  geom_bar(stat = "identity", col = "black", width = .5, alpha = .8) +
  geom_text(size = 3, col = "black", position = position_stack(vjust = 0.5)) +
  theme_classic() +
  scale_x_discrete(name="environment", limits = c('cond1', 'cond2', 'cond3'), labels = c('short', 'long', 'island')) +
  scale_y_continuous(name="% responses", limits=c(0, 101), breaks=c(0, 20, 40, 60, 80, 100)) +
  scale_fill_manual(name="dependency", values=c('#648fff', 'gray', '#ffb000')) +
  theme(text = element_text(size = 12), plot.title = element_text(size = 12, hjust = .5), legend.position = "right") +
  facet_grid(study~item)

# save plot

ggsave("plots/ept_plot_item.png", width=10, height=5, dpi=600)

#------------------------------------------------------------------------------#
#### ept: modeling ####
#------------------------------------------------------------------------------#

# remove participants who only gave nontarget responses

temp <- ep %>%
  filter(type != 'nontarget') %>%
  group_by(study, group, participant) %>%
  summarise() %>%
  ungroup()

md <- ep %>%
  filter(participant %in% temp$participant)

# code responses as ±resumption

md <- md %>%
  mutate(resumption = case_when(type == 'resumption' ~ TRUE,
                                TRUE ~ FALSE)) %>%
  filter(group %in% c('english', 'korean', 'mandarin')) %>%
  mutate(group = fct_drop(group)) %>%
  mutate(environment = fct_relevel(environment, 'long', 'short', 'island'))

md2 <- md %>%
  filter(study == '210510_do' & group == 'mandarin' & is.na(condition) == FALSE) %>%
  mutate(condition = fct_drop(condition))

md2 <- md2 %>%
  filter(type %in% c('gap', 'resumption'))

# view contrasts
contrasts(md2$environment)

model1 <- glmer(resumption ~ environment + (environment|participant) + (environment|item), 
                data = md2, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(model1)
beepr::beep(1)

# doen (reference level = cond2) singular fit
# Fixed effects:
#                     Estimate Std. Error z value Pr(>|z|)    
# (Intercept)       -10.424930   0.001255   -8309   <2e-16 ***
# environmentshort  -31.971440   0.001296  -24673   <2e-16 ***
# environmentisland   9.651172   0.001255    7692   <2e-16 ***

# doko (reference level = cond2) failed to converge

# dozh (reference level = cond2) failed to converge


# models
model1 <- glmer(resumption ~ environment + (environment|participant) + (environment|item), 
                data = md2, family = binomial, glmerControl(optimizer = "bobyqa"))
summary(model1)

model2 <- glmer(type ~ environment + (1|participant) + (1|item), 
                data = md2, family = binomial, glmerControl(optimizer = "bobyqa"))
summary(model2)

model3 <- glmer(type ~ environment + (environment|item), 
                data = md2, family = binomial, glmerControl(optimizer = "bobyqa"))
summary(model3)

# *** filter to participants who used resumption at least once and try again ***

# make list of participants who used resumption at least once
check <- md %>%
  filter(type == 'resumption') %>%
  group_by(participant) %>%
  summarise() %>%
  ungroup() %>%
  mutate(resumer = TRUE)
# english resumers: 13/33 = 0.3939394 = 39%
# korean resumers: 13/35 = 0.3428571 = 37%

# add information about who used resumption to dataframe
md <- md %>%
  left_join(check, by = 'participant')

# filter to participants who used resumption at least once
md2 <- md %>%
  filter(resumer == TRUE)

# summarise data for plotting
plot <- md2 %>%
  filter(cond %in% c('cond1', 'cond2', 'cond3')) %>%
  mutate(category = case_when(resumption == TRUE ~ 'resumption',
                              resumption == FALSE ~ 'other')) %>%
  mutate(category = as.character(category)) %>%
  group_by(group, cond) %>%
  count(category) %>%
  ungroup() %>%
  complete(category, nesting(cond), fill = list(n = 0)) %>%
  group_by(group, cond) %>%
  mutate(sum = sum(n)) %>%
  ungroup() %>%
  mutate(prop = (n/sum)*100) %>%
  filter(group %in% c('english', 'korean')) %>%
  mutate(prc = as.character(round(prop))) %>%
  mutate(category = factor(category, levels = c('other', 'resumption')))

# bar plot
ggplot(data=plot, aes(x=cond, y=prop, group=category, fill=category, label=prc)) +
  geom_bar(stat = "identity", col = "black", width = .5) +
  geom_text(size = 3.5, col = "black", position = position_stack(vjust = 0.5)) +
  theme_classic() +
  scale_x_discrete(name="environment", limits = c('cond1', 'cond2', 'cond3'), labels = c('short', 'long', 'island')) +
  scale_y_continuous(name="% responses", limits=c(0, 101), breaks=c(0, 20, 40, 60, 80, 100)) +
  scale_fill_manual(name="dependency", values=c("#9b82f3", "#00a78f")) +
  theme(text = element_text(size = 12), plot.title = element_text(size = 12, hjust = .5), legend.position = "right", legend.margin=margin(1, 1, 1, 1)) +
  facet_wrap(~group)

# save plot
ggsave("data/plots/ept_binary_resumers.png", width=5.5, height=2.5, dpi=600)

md3 <- md2 %>%
  filter(group == 'korean') %>%
  #filter(environment != 'NA') %>%
  filter(cond %in% c('cond1', 'cond2', 'cond3')) %>%
  select(group, participant, item, type, resumption, cond, resumer) %>%
  mutate(participant = as.factor(participant),
         item = as.factor(item))

str(md3)

# view contrasts
contrasts(md3$environment)

# full model
model1 <- glmer(resumption ~ cond + (cond|participant) + (cond|item), 
                data = md3, family = binomial, glmerControl(optimizer = "bobyqa"))
summary(model1)

model2 <- glmer(resumption ~ environment + (1|participant) + (1|item), 
                data = md3, family = binomial, glmerControl(optimizer = "bobyqa"))
summary(model2)

model2 <- glm(resumption ~ environment, data = md3, family = binomial)
summary(model2)

# *** attempt 3 ***

md <- target %>%
  filter(cond %in% c('cond1', 'cond2', 'cond3')) %>%
  mutate(cond = fct_drop(cond)) %>%
  mutate(category = case_when(type == 'gap' ~ 'gap',
                              type == 'resumption' ~ 'resumption',
                              TRUE ~ 'other')) %>%
  mutate(category = as.character(category)) %>%
  group_by(group, cond, participant) %>%
  count(category) %>%
  ungroup() %>%
  complete(category, nesting(group, cond, participant), fill = list(n = 0)) %>%
  filter(category == 'resumption')

md <- md %>%
  mutate(cond = fct_relevel(cond, 'cond1', 'cond2', 'cond3'))

# view contrasts
contrasts(md$cond)

model1 <- lmer(n ~ cond + (1|participant), data = md)
summary(model1)
# Fixed effects:
#   Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept) 7.944e-17  1.167e-01 1.777e+02   0.000    1.000    
# condcond2   1.587e-01  1.533e-01 1.238e+02   1.036    0.302    
# condcond3   9.543e-01  1.547e-01 1.249e+02   6.170 8.76e-09 ***

#------------------------------------------------------------------------------#
#### spr: prep dataframe ####
#------------------------------------------------------------------------------#

# create dataframe for reading time data

spr <- df %>%
  filter(task == 'sprt') %>%
  arrange(study, group, participant) %>%
  select_if(function(x){!all(is.na(x))})

#------------------------------------------------------------------------------#
#### spr: plots for rt data ####
#------------------------------------------------------------------------------#

# filter to critical trials
ds <- spr %>%
  filter(!condition %in% c('grammatical', 'ungrammatical')) %>%
  mutate(rt = as.numeric(as.character(rt)),
         participant = as.factor(participant))

# run fct_drop on 'participant'
ds <- ds %>%
  mutate(participant = fct_drop(participant))

# check participants
check <- ds %>%
  group_by(study, group, participant) %>%
  summarise() %>%
  summarise(n = n()) %>%
  ungroup()

# trim based on rt
trim <- ds %>%
  filter(rt <= 3000) %>%
  filter(rt >= 200) %>%
  group_by(study, group, condition, region) %>%
  mutate(sd2 = mean(rt, na.rm=T) + (2 * (sd(rt, na.rm=T)))) %>%
  ungroup() %>%
  mutate(rt = case_when(rt > sd2 ~ sd2, TRUE ~ as.numeric(rt))) %>%
  select(-sd2) %>%
  group_by(study, group, participant) %>%
  mutate(acc_rate = mean(as.logical(accuracy))) %>%
  ungroup()

# check participants
check <- trim %>%
  group_by(study, group, participant) %>%
  summarise() %>%
  summarise(n = n()) %>%
  ungroup()

plot <- trim %>%
  group_by(study, group, participant, acc_rate) %>%
  summarise() %>%
  ungroup()

# inspect accuracy rates
ggplot(plot, aes(x=group, y=acc_rate, fill=group, label=participant)) + 
  geom_hline(yintercept=.5) +
  geom_violin() +
  geom_boxplot(width = .1, fill='white') +
  theme_classic() +
  scale_x_discrete(name="group", 
                   limits = c('english', 'korean', 'mandarin'),
                   labels = c('ENS', 'KLE', 'MLE')) +
  scale_y_continuous(name="accuracy rate", 
                     limits=c(0, 1)) +
  theme(text = element_text(size = 12), 
        plot.title = element_text(size = 12, hjust = .5), 
        legend.position = "hide") +
  facet_wrap(~study)

# trim based on accuracy
trim <- trim %>%
  filter(acc_rate > .5)

# check participants
check <- trim %>%
  group_by(study, group, participant) %>%
  summarise() %>%
  summarise(n = n()) %>%
  ungroup()

# summarise data for plotting by group
plot <- trim %>%
  mutate(environment = as.factor(environment)) %>%
  mutate(environment = fct_relevel(environment, 'short', 'long', 'island')) %>%
  group_by(study, group, region, dependency, environment, condition) %>%
  summarise(mean_rt = mean(rt, na.rm=T),
            sd = sd(rt, na.rm=T),
            n = n()) %>%
  mutate(se = sd / sqrt(n),
         ci = qt(1 - (0.05 / 2), n - 1) * se) %>%
  ungroup() %>%
  filter(region %in% c(7, 8, 9, 10, 11, 12, 13, 14, 15)) %>%
  filter(group %in% c('english', 'korean', 'mandarin')) %>%
  mutate(panel = case_when(group == 'english' ~ 'ENS',
                           group == 'korean' ~ 'KLE',
                           group == 'mandarin' ~ 'MLE')) %>%
  mutate(environment = fct_relevel(environment, 'short', 'long', 'island'))

# generate plot

p1a <- ggplot(data=filter(plot, study == '210510_do', environment == 'short'), aes(x=region, y=mean_rt, group=dependency, col=dependency, shape=dependency))
p1b <- ggplot(data=filter(plot, study == '210510_do', environment == 'long'), aes(x=region, y=mean_rt, group=dependency, col=dependency, shape=dependency))
p1c <- ggplot(data=filter(plot, study == '210510_do', environment == 'island'), aes(x=region, y=mean_rt, group=dependency, col=dependency, shape=dependency))

p2a <- ggplot(data=filter(plot, study == '210510_su', environment == 'short'), aes(x=region, y=mean_rt, group=dependency, col=dependency, shape=dependency))
p2b <- ggplot(data=filter(plot, study == '210510_su', environment == 'long'), aes(x=region, y=mean_rt, group=dependency, col=dependency, shape=dependency))
p2c <- ggplot(data=filter(plot, study == '210510_su', environment == 'island'), aes(x=region, y=mean_rt, group=dependency, col=dependency, shape=dependency))

s <- list(
  geom_line(lwd=1),
  geom_point(size=2),
  geom_errorbar(aes(ymin=mean_rt-ci, ymax=mean_rt+ci), width=.5, lwd=1, linetype=1),
  theme_classic(),
  scale_y_continuous(name="mean reading time (ms)", limits=c(300, 850)),
  scale_colour_manual(name="dependency", values=c('#648fff', '#ffb000'), labels=c('gap', 'resumption')),
  scale_shape_manual(name="dependency", values=c(16, 15), labels=c('gap', 'resumption')),
  theme(text = element_text(size = 12),
        plot.title = element_text(size = 12, hjust = .5)),
  facet_grid2(vars(panel), vars(environment), axes = 'all', remove_labels = 'y')#,
  #geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), data = f_labels, alpha = .15)
)

p1a + s + 
  theme(legend.position = 'none', 
        strip.background.y = element_blank(), 
        strip.text.y = element_blank(),
        axis.title.x = element_blank()) +
  annotate('rect', xmin = 4.5, xmax = 7.5, ymin = 300, ymax = 850, alpha = .15) +
  scale_x_discrete(name="region", limits=c('8', '9', '10', '11', '12', '13', '14', '15')) +
p1b + s +
  theme(legend.position = 'none',
        axis.title.y = element_blank(),
        strip.background.y = element_blank(),
        strip.text.y = element_blank()) +
  annotate('rect', xmin = 4.5, xmax = 7.5, ymin = 300, ymax = 850, alpha = .15) +
  scale_x_discrete(name="region", limits=c('8', '9', '10', '11', '12', '13', '14', '15')) +
p1c + s +
  theme(legend.position = c(-.8, -.15),
        legend.direction = 'horizontal',
        legend.margin = margin(t = 0, unit='cm'),
        plot.margin = margin(b = 12),
        axis.title.y = element_blank(),
        axis.title.x = element_blank()) +
  annotate('rect', xmin = 4.5, xmax = 7.5, ymin = 300, ymax = 850, alpha = .15) +
  scale_x_discrete(name="region", limits=c('8', '9', '10', '11', '12', '13', '14', '15'))

ggsave('plots/orc/spr_rt.png', width=6.5, height=4.5, dpi=600)

p2a + s + 
  theme(legend.position = 'none', 
        strip.background.y = element_blank(), 
        strip.text.y = element_blank(),
        axis.title.x = element_blank()) +
  annotate('rect', xmin = 2.5, xmax = 5.5, ymin = 300, ymax = 850, alpha = .15) +
  scale_x_discrete(name="region", limits=c('7', '8', '9', '10', '11', '12', '13', '14')) +
p2b + s +
  theme(legend.position = 'none',
        axis.title.y = element_blank(),
        strip.background.y = element_blank(),
        strip.text.y = element_blank()) +
  annotate('rect', xmin = 2.5, xmax = 5.5, ymin = 300, ymax = 850, alpha = .15) +
  scale_x_discrete(name="region", limits=c('7', '8', '9', '10', '11', '12', '13', '14')) +
p2c + s +
  theme(legend.position = c(-.8, -.15),
        legend.direction = 'horizontal',
        legend.margin = margin(t = 0, unit='cm'),
        plot.margin = margin(b = 12),
        axis.title.y = element_blank(),
        axis.title.x = element_blank()) +
  annotate('rect', xmin = 4.5, xmax = 7.5, ymin = 300, ymax = 850, alpha = .15) +
  scale_x_discrete(name="region", limits=c('7', '8', '9', '10', '11', '12', '13', '14'))

ggsave('plots/src/spr_rt.png', width=6.5, height=4.5, dpi=600)

#------------------------------------------------------------------------------#
#### spr: plots for accuracy data ####
#------------------------------------------------------------------------------#

# trim based on accuracy
trim <- ds %>%
  group_by(study, group, participant) %>%
  mutate(acc_rate = mean(as.logical(accuracy))) %>%
  ungroup() %>%
  filter(acc_rate >.5)
  
#summarize data for plotting by group
plot <- trim %>%
  mutate(accuracy = as.logical(accuracy)) %>%
  group_by(study, group, participant, item, dependency, environment, accuracy) %>%
  summarise() %>%
  ungroup() %>%
  group_by(study, group, dependency, environment) %>%
  summarise(mean = mean(accuracy, na.rm=T) * 100,
            sd = sd(accuracy, na.rm=T) * 100,
            n = n()) %>%
  mutate(se = sd / sqrt(n),
         ci = qt(1 - (0.05 / 2), n - 1) * se) %>%
  ungroup() %>%
  mutate(panel = case_when(group == 'english' ~ 'ENS',
                           group == 'korean' ~ 'KLE',
                           group == 'mandarin' ~ 'MLE'))

p1 <- ggplot(data=filter(plot, study == '210510_do'), aes(x=environment, y=mean, group=dependency, col=dependency, shape=dependency))
p2 <- ggplot(data=filter(plot, study == '210510_su'), aes(x=environment, y=mean, group=dependency, col=dependency, shape=dependency))

# create plot
s <- list(
  geom_line(lwd = 1),
  geom_point(size = 2),
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.2, lwd=1, linetype=1),
  theme_classic(),
  scale_x_discrete(name="environment", limits = c("short", "long", "island"), labels = c("short", "long", "island")),
  scale_y_continuous(name="% accuracy", limits=c(68, 100)),
  scale_colour_manual(name="dependency", values=c('#648fff', '#ffb000'), labels=c("gap", "resumption")),
  scale_shape_manual(name="dependency", values=c(16, 15), labels=c("gap", "resumption")),
  theme(text = element_text(size = 12), 
        plot.title = element_text(size = 12, hjust = .5), 
        legend.position = "none", 
        axis.title.y = element_text(margin=margin(r=-3)),
        legend.margin=margin(0, 0, 0, 0),
        legend.box.margin = margin(0, 0, 0, 0)),
  facet_wrap(~panel)
)

p1 + s
bucld2 <- p1 + s + 
  labs(caption = 'glmer: accuracy ~ dependency * environment +\n(1 + dependency * environment | person) +\n(1 + dependency * environment | item)', hjust = .5) +
  theme(plot.caption = element_text(hjust = .5))
bucld2
ggsave("plots/orc/spr_accuracy.png", width=6.5, height=2.5, dpi=600)

p2 + s
ggsave("plots/src/spr_accuracy.png", width=6.5, height=2.5, dpi=600)

#------------------------------------------------------------------------------#
#### spr: trim data and calculate RRTs ####
#------------------------------------------------------------------------------#

# filter to critical trials

spr_crit <- spr %>%
  filter(!condition %in% c('grammatical', 'ungrammatical')) %>%
  mutate(rt = as.numeric(as.character(rt)),
         participant = as.factor(participant)) %>%
  mutate(participant = fct_drop(participant))

# trim based on rt

spr_trim <- spr_crit %>%
  filter(rt <= 3000) %>%
  filter(rt >= 200)

# adjust region numbers

spr_trim <- spr_trim %>%
  mutate(region = as.numeric(region)) %>%
  mutate(region2 = case_when(study == '210510_do' ~ region - 11,
                             study == '210510_su' & environment %in% c('short', 'long') ~ region - 8,
                             study == '210510_su' & environment == 'island' ~ region - 10))

# make word length column

spr_trim <- spr_trim %>%
  mutate(stimulus = str_trim(tolower(stimulus))) %>%
  mutate(length = nchar(stimulus))
  
# calculate RRTs by participant for each study and add to dataframe

class(spr_trim$length)
class(spr_trim$region)

spr_trim <- spr_trim %>%
  mutate(region = as.factor(region))

spr_trim_orc <- spr_trim %>%
  filter(study == '210510_do')

mod_spr_rrt_orc <- lmer(rt ~ length + region + (1 | participant), data = spr_trim_orc)

spr_trim_orc <- spr_trim_orc %>%
  mutate(rrt = residuals(mod_spr_rrt_orc))

spr_trim_src <- spr_trim %>%
  filter(study == '210510_su')

mod_spr_rrt_src <- lmer(rt ~ length + region + (1 | participant), data = spr_trim_src)

spr_trim_src <- spr_trim_src %>%
  mutate(rrt = residuals(mod_spr_rrt_src))

spr_trim <- bind_rows(spr_trim_orc, spr_trim_src)

# replace extreme RRTs

check <- spr_trim %>%
  group_by(study, group, condition, region2) %>%
  mutate(sd2 = mean(rrt, na.rm = T) + (2 * (sd(rrt, na.rm = T)))) %>%
  ungroup() %>%
  mutate(extreme = case_when(rrt > sd2 ~ TRUE, TRUE ~ FALSE)) %>%
  select(-sd2) %>%
  group_by(study) %>%
  summarise(mean = mean(extreme, na.rm = TRUE) * 100) %>%
  ungroup()

spr_trim <- spr_trim %>%
  group_by(study, group, condition, region2) %>%
  mutate(sd2 = mean(rrt, na.rm = T) + (2 * (sd(rrt, na.rm = T)))) %>%
  ungroup() %>%
  mutate(rrt = case_when(rrt > sd2 ~ sd2, TRUE ~ as.numeric(rrt))) %>%
  select(-sd2)

# calculate accuracy rates on comprehension question by participant

spr_trim <- spr_trim %>%
  group_by(study, group, participant) %>%
  mutate(acc_rate = mean(as.logical(accuracy))) %>%
  ungroup()

# check distribution of RRTs

hist(spr_trim$rrt)
qqnorm(spr_trim$rrt)

# https://stat.ethz.ch/pipermail/r-help/2008-July/168808.html

#------------------------------------------------------------------------------#
#### spr: plots of raw RTs ####
#------------------------------------------------------------------------------#

# check participants

check <- spr_trim %>%
  group_by(study, group, participant) %>%
  summarise() %>%
  summarise(n = n()) %>%
  ungroup()

# inspect accuracy rates

plot <- spr_trim %>%
  group_by(study, group, participant, acc_rate) %>%
  summarise() %>%
  ungroup()

ggplot(plot, aes(x=group, y=acc_rate, fill=group, label=participant)) + 
  geom_hline(yintercept=.5) +
  geom_violin() +
  geom_boxplot(width = .1, fill='white') +
  theme_classic() +
  scale_x_discrete(name="group", 
                   limits = c('english', 'korean', 'mandarin'),
                   labels = c('ENS', 'KLE', 'MLE')) +
  scale_y_continuous(name="accuracy rate", 
                     limits=c(0, 1)) +
  theme(text = element_text(size = 12), 
        plot.title = element_text(size = 12, hjust = .5), 
        legend.position = "hide") +
  facet_wrap(~study)

# trim based on accuracy

spr_trim <- spr_trim %>%
  filter(acc_rate > .5)

# check participants

check <- spr_trim %>%
  group_by(study, group, participant) %>%
  summarise() %>%
  summarise(n = n()) %>%
  ungroup()

# summarise data for plotting by group

plot <- spr_trim %>%
  mutate(environment = as.factor(environment)) %>%
  mutate(environment = fct_relevel(environment, 'short', 'long', 'island')) %>%
  group_by(study, group, region2, dependency, environment, condition) %>%
  summarise(mean_rt = mean(rt, na.rm=T),
            sd = sd(rt, na.rm=T),
            n = n()) %>%
  mutate(se = sd / sqrt(n),
         ci = qt(1 - (0.05 / 2), n - 1) * se) %>%
  ungroup() %>%
  filter(region2 %in% c(-3, -2, -1, 0, 1, 2, 3, 4)) %>%
  filter(group %in% c('english', 'korean', 'mandarin')) %>%
  mutate(panel = case_when(group == 'english' ~ 'ENS',
                           group == 'korean' ~ 'KLE',
                           group == 'mandarin' ~ 'MLE')) %>%
  mutate(environment = fct_relevel(environment, 'short', 'long', 'island'))

# generate plot

p1 <- ggplot(data=filter(plot, study == '210510_do'), aes(x=region2, y=mean_rt, group=dependency, col=dependency, shape=dependency))
p2 <- ggplot(data=filter(plot, study == '210510_su'), aes(x=region2, y=mean_rt, group=dependency, col=dependency, shape=dependency))

s <- list(
  annotate('rect', xmin = 0.6, xmax = 1.4, ymin = 300, ymax = 900, alpha = .15),
  annotate('rect', xmin = 1.6, xmax = 2.4, ymin = 300, ymax = 900, alpha = .15),
  annotate('rect', xmin = 2.6, xmax = 3.4, ymin = 300, ymax = 900, alpha = .15),
  geom_hline(yintercept = 0),
  geom_vline(xintercept = 0),
  geom_line(lwd=1),
  geom_point(size=2),
  geom_errorbar(aes(ymin=mean_rt-ci, ymax=mean_rt+ci), width=.5, lwd=1, linetype=1),
  theme_classic(),
  scale_y_continuous(name="raw reading time (ms)", limits=c(270, 930)),
  scale_x_continuous(name="region", limits=c(-3.25, 4.25), breaks = c(-3, -2, -1, 0, 1, 2, 3, 4)),
  scale_colour_manual(name="dependency", values=c('#648fff', '#ffb000'), labels=c('gap', 'resumption')),
  scale_shape_manual(name="dependency", values=c(16, 15), labels=c('gap', 'resumption')),
  theme(text = element_text(size = 12),
        legend.position = 'bottom',
        legend.margin = margin(t = -.4, unit = 'cm'),
        plot.title = element_text(size = 12, hjust = .5)),
  facet_grid2(vars(panel), vars(environment), axes = 'all', remove_labels = 'y')
)

p1 + s + 
  geom_text(data = filter(plot, environment == 'island', panel == 'ENS'), mapping = aes(x = 1, y = 910, label = '*'), col = 'black') +
  geom_text(data = filter(plot, environment == 'long', panel == 'KLE'), mapping = aes(x = 2, y = 910, label = '*'), col = 'black') +
  geom_text(data = filter(plot, environment == 'short', panel == 'MLE'), mapping = aes(x = 2, y = 910, label = '*'), col = 'black') +
  geom_text(data = filter(plot, environment == 'long', panel == 'MLE'), mapping = aes(x = 2, y = 910, label = '*'), col = 'black') +
  geom_text(data = filter(plot, environment == 'island', panel == 'MLE'), mapping = aes(x = 2, y = 910, label = '*'), col = 'black')

ggsave('plots/orc/spr_rawrt.png', width=6.5, height=4.5, dpi=600)

p2 + s +
  geom_text(data = filter(plot, environment == 'short', panel == 'ENS'), mapping = aes(x = 2, y = 920, label = '·'), col = 'black') +
  geom_text(data = filter(plot, environment == 'island', panel == 'ENS'), mapping = aes(x = 2, y = 910, label = '*'), col = 'black') +
  geom_text(data = filter(plot, environment == 'island', panel == 'ENS'), mapping = aes(x = 3, y = 910, label = '*'), col = 'black') +
  geom_text(data = filter(plot, environment == 'long', panel == 'KLE'), mapping = aes(x = 2, y = 910, label = '*'), col = 'black') +
  geom_text(data = filter(plot, environment == 'short', panel == 'KLE'), mapping = aes(x = 3, y = 910, label = '*'), col = 'black') +
  geom_text(data = filter(plot, environment == 'island', panel == 'KLE'), mapping = aes(x = 3, y = 910, label = '*'), col = 'black') +
  geom_text(data = filter(plot, environment == 'long', panel == 'MLE'), mapping = aes(x = 1, y = 910, label = '*'), col = 'black') +
  geom_text(data = filter(plot, environment == 'long', panel == 'MLE'), mapping = aes(x = 2, y = 910, label = '*'), col = 'black') +
  geom_text(data = filter(plot, environment == 'island', panel == 'MLE'), mapping = aes(x = 1, y = 910, label = '*'), col = 'black') +
  geom_text(data = filter(plot, environment == 'island', panel == 'MLE'), mapping = aes(x = 2, y = 920, label = '·'), col = 'black')

ggsave('plots/src/spr_rawrt.png', width=6.5, height=4.5, dpi=600)

#------------------------------------------------------------------------------#
#### spr: modeling for raw RTs ####
#------------------------------------------------------------------------------#

# filter data for analysis

md <- spr_trim %>%
  filter(region2 == 2,
         study == '210510_do',
         group == 'mandarin')

# check distribution

hist(md$rt)
qqnorm(md$rt)

md <- md %>%
  mutate(dependency = as.factor(dependency),
         environment = as.factor(environment)) %>%
  mutate(environment = fct_relevel(environment, 'short', 'long', 'island'))

# view contrasts

contrasts(md$dependency)
contrasts(md$environment)

# full model

mod_spr_1 <- lmer(rt ~ environment*dependency + (environment*dependency|participant) + (environment*dependency|item), data = md)
summary(mod_spr_1)
beep(1)

# doen region 1
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error      df t value Pr(>|t|)   
# (Intercept)                           -6.094      8.216  37.583  -0.742  0.46288   
# environmentlong                        6.518     12.629  40.439   0.516  0.60859   
# environmentisland                     28.809     11.464  65.225   2.513  0.01446 * 
# dependencypronoun                      2.508     11.716  93.254   0.214  0.83097   
# environmentlong:dependencypronoun    -15.898     17.741  44.225  -0.896  0.37507   
# environmentisland:dependencypronoun  -52.826     16.303 113.487  -3.240  0.00157 **
saveRDS(mod_spr_1, file='models/orc_spr_rawrt_doen_region1_mod.rds') 
cat(capture.output(summary(mod_spr_1)), file = paste('models/orc_spr_rawrt_doen_region1_mod.txt', sep = ''), sep='\n')
# mod_spr_1 <- readRDS('models/orc_spr_doen_region1_mod.rds')
# https://www.r-bloggers.com/2012/04/a-better-way-of-saving-and-loading-objects-in-r/

# doen region 2
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error      df t value Pr(>|t|)    
# (Intercept)                          354.140     12.954  85.520  27.338   <2e-16 ***
# environmentlong                       -2.863     12.105 128.014  -0.237   0.8134    
# environmentisland                     29.335     15.576  37.188   1.883   0.0675 .  
# dependencypronoun                     25.527     16.561  55.505   1.541   0.1289    
# environmentlong:dependencypronoun    -19.319     21.131  45.512  -0.914   0.3654    
# environmentisland:dependencypronoun  -54.305     24.577  50.547  -2.210   0.0317 *  
saveRDS(mod_spr_1, file='models/orc_spr_rawrt_doen_region2_mod.rds') 
cat(capture.output(summary(mod_spr_1)), file = paste('models/orc_spr_rawrt_doen_region1_mod.txt', sep = ''), sep='\n')

# doen region 3
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error      df t value Pr(>|t|)    
# (Intercept)                          353.289     11.332  94.850  31.177   <2e-16 ***
# environmentlong                       -4.343      9.479 152.562  -0.458    0.648    
# environmentisland                     15.606      9.585 113.045   1.628    0.106    
# dependencypronoun                     13.880      9.159 198.322   1.516    0.131    
# environmentlong:dependencypronoun      2.136     15.023  41.767   0.142    0.888    
# environmentisland:dependencypronoun  -18.094     15.151  46.068  -1.194    0.238 
saveRDS(mod_spr_1, file='models/orc_spr_rawrt_doen_region3_mod.rds') 
cat(capture.output(summary(mod_spr_1)), file = paste('models/orc_spr_rawrt_doen_region3_mod.txt', sep = ''), sep='\n')

# doko region 1
# (skip for now)
saveRDS(mod_spr_1, file='models/orc_spr_rawrt_doko_region1_mod.rds') 
cat(capture.output(summary(mod_spr_1)), file = paste('models/orc_spr_rawrt_doko_region1_mod.txt', sep = ''), sep='\n')

# doko region 2
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error     df t value Pr(>|t|)    
# (Intercept)                           394.87      20.38  51.47  19.372   <2e-16 ***
#   environmentlong                        38.87      24.93  34.48   1.559    0.128    
# environmentisland                      38.41      33.69  48.11   1.140    0.260    
# dependencypronoun                     -27.10      23.70 104.21  -1.143    0.255    
# environmentlong:dependencypronoun     -55.22      35.00  46.51  -1.578    0.121    
# environmentisland:dependencypronoun   -22.64      38.64  55.24  -0.586    0.560 
saveRDS(mod_spr_1, file='models/orc_spr_rawrt_doko_region2_mod.rds') 
cat(capture.output(summary(mod_spr_1)), file = paste('models/orc_spr_rawrt_doko_region2_mod.txt', sep = ''), sep='\n')

# doko region 3
# (skip for now)
saveRDS(mod_spr_1, file='models/orc_spr_rawrt_doko_region3_mod.rds') 
cat(capture.output(summary(mod_spr_1)), file = paste('models/orc_spr_rawrt_doko_region3_mod.txt', sep = ''), sep='\n')

# dozh region 1
# (skip for now)
saveRDS(mod_spr_1, file='models/orc_spr_rawrt_dozh_region1_mod.rds') 
cat(capture.output(summary(mod_spr_1)), file = paste('models/orc_spr_rawrt_dozh_region1_mod.txt', sep = ''), sep='\n')

# dozh region 2
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error     df t value Pr(>|t|)    
# (Intercept)                           439.15      19.76  56.21  22.221   <2e-16 ***
# environmentlong                        37.46      29.64  36.78   1.264   0.2143    
# environmentisland                      39.86      28.87  41.52   1.381   0.1747    
# dependencypronoun                     -36.68      21.49  92.77  -1.707   0.0912 .  
# environmentlong:dependencypronoun     -37.57      37.22  41.11  -1.009   0.3187    
# environmentisland:dependencypronoun   -34.59      35.20  52.20  -0.983   0.3303 
saveRDS(mod_spr_1, file='models/orc_spr_rawrt_dozh_region2_mod.rds') 
cat(capture.output(summary(mod_spr_1)), file = paste('models/orc_spr_rawrt_dozh_region2_mod.txt', sep = ''), sep='\n')

# dozh region 3
# (skip for now)
saveRDS(mod_spr_1, file='models/orc_spr_rawrt_dozh_region3_mod.rds') 
cat(capture.output(summary(mod_spr_1)), file = paste('models/orc_spr_rawrt_dozh_region3_mod.txt', sep = ''), sep='\n')

# suen region 1
# boundary (singular) fit: see help('isSingular')

saveRDS(mod_spr_1, file='models/src_spr_rawrt_suen_region1_mod.rds') 
cat(capture.output(summary(mod_spr_1)), file = paste('models/src_spr_rawrt_suen_region1_mod.txt', sep = ''), sep='\n')

# suen region 2
# boundary (singular) fit: see help('isSingular')

saveRDS(mod_spr_1, file='models/src_spr_rawrt_suen_region2_mod.rds') 
cat(capture.output(summary(mod_spr_1)), file = paste('models/src_spr_rawrt_suen_region2_mod.txt', sep = ''), sep='\n')

# suen region 3
# boundary (singular) fit: see help('isSingular')

saveRDS(mod_spr_1, file='models/src_spr_rawrt_suen_region3_mod.rds') 
cat(capture.output(summary(mod_spr_1)), file = paste('models/src_spr_rawrt_suen_region3_mod.txt', sep = ''), sep='\n')

# suko region 1
# boundary (singular) fit: see help('isSingular')

saveRDS(mod_spr_1, file='models/src_spr_rawrt_suko_region1_mod.rds') 
cat(capture.output(summary(mod_spr_1)), file = paste('models/src_spr_rawrt_suko_region1_mod.txt', sep = ''), sep='\n')

# suko region 2
# boundary (singular) fit: see help('isSingular')

saveRDS(mod_spr_1, file='models/src_spr_rawrt_suko_region2_mod.rds') 
cat(capture.output(summary(mod_spr_1)), file = paste('models/src_spr_rawrt_suko_region2_mod.txt', sep = ''), sep='\n')

# suko region 3
# boundary (singular) fit: see help('isSingular')

saveRDS(mod_spr_1, file='models/src_spr_rawrt_suko_region3_mod.rds') 
cat(capture.output(summary(mod_spr_1)), file = paste('models/src_spr_rawrt_suko_region3_mod.txt', sep = ''), sep='\n')

# suzh region 1
# boundary (singular) fit: see help('isSingular')

saveRDS(mod_spr_1, file='models/src_spr_rawrt_suzh_region1_mod.rds') 
cat(capture.output(summary(mod_spr_1)), file = paste('models/src_spr_rawrt_suzh_region1_mod.txt', sep = ''), sep='\n')

# suzh region 2
# boundary (singular) fit: see help('isSingular')
 
saveRDS(mod_spr_1, file='models/src_spr_rawrt_suzh_region2_mod.rds') 
cat(capture.output(summary(mod_spr_1)), file = paste('models/src_spr_rawrt_suzh_region2_mod.txt', sep = ''), sep='\n')

# suzh region 3
# boundary (singular) fit: see help('isSingular')

saveRDS(mod_spr_1, file='models/src_spr_rawrt_suzh_region3_mod.rds') 
cat(capture.output(summary(mod_spr_1)), file = paste('models/src_spr_rawrt_suzh_region3_mod.txt', sep = ''), sep='\n')

# https://marissabarlaz.github.io/portfolio/contrastcoding/

# check assumptions: linearity, normality of the residuals, homogeneity of residual variance (homoscedasticity), no autoccorelation and no multicollinearity (only for models without interaction terms)

performance::check_model(mod_spr_1) # perform checks
ggsave('plots/check_model.png', width=10, height=10, dpi=600) # save output
performance::model_performance(mod_spr_1) # check model performance
lattice::qqmath(mod_spr_1, id = 0.05) # check for normality of residuals (identified outliers)
car::leveneTest(residuals(mod_spr_1) ~ md$environment * md$dependency) # check homogeneity of residual variance (should not be significant)

# https://easystats.github.io/performance/index.html
# https://ademos.people.uic.edu/Chapter18.html
# https://bookdown.org/animestina/phd_july_19/testing-the-assumptions.html

# post-hoc tests: pairwise comparisons of estimated marginal means (see https://stats.stackexchange.com/questions/424304/when-to-correct-for-multiple-comparisons-with-specific-reference-to-emmeans-in)

# pairs(emmeans(mod_spr_1, 'dependency', by = 'environment'))

mod_spr_1 %>%
  emmeans(~ dependency * environment) %>%
  contrast('pairwise', by = 'environment') %>%
  summary(by = NULL, adjust = 'holm')
beep(1)

# doen region 1
# contrast      environment estimate   SE   df t.ratio p.value
# gap - pronoun short          -2.51 11.8 31.1  -0.213  0.8328
# gap - pronoun long           13.39 11.9 26.9   1.122  0.5437
# gap - pronoun island         50.32 11.8 29.2   4.247  0.0006 ***
# Degrees-of-freedom method: kenward-roger 
# P value adjustment: holm method for 3 tests 

# doen region 2
# contrast      environment estimate   SE   df t.ratio p.value
# gap - pronoun short         -25.53 16.6 46.0  -1.535  0.2630 (rrt: '*')
# gap - pronoun long           -6.21 14.6 35.9  -0.425  0.6734
# gap - pronoun island         28.78 16.3 36.4   1.769  0.2558 (rrt: '*')
# Degrees-of-freedom method: kenward-roger 
# P value adjustment: holm method for 3 tests 

# doko region 2
# contrast      environment estimate   SE   df t.ratio p.value
# gap - pronoun short           27.1 24.0 26.7   1.131  0.2680
# gap - pronoun long            82.3 27.8 28.5   2.963  0.0183
# gap - pronoun island          49.7 31.4 32.8   1.585  0.2452
# Degrees-of-freedom method: kenward-roger 
# P value adjustment: holm method for 3 tests 

# dozh region 1


# dozh region 2


# suen region 1


# suen region 2


# suen region 3


# suko region 2


# suko region 3


# suzh region 1


# suzh region 2


#------------------------------------------------------------------------------#
#### spr: plots of residual RTs ####
#------------------------------------------------------------------------------#

# check participants

check <- spr_trim %>%
  group_by(study, group, participant) %>%
  summarise() %>%
  summarise(n = n()) %>%
  ungroup()

# inspect accuracy rates

plot <- spr_trim %>%
  group_by(study, group, participant, acc_rate) %>%
  summarise() %>%
  ungroup()

ggplot(plot, aes(x=group, y=acc_rate, fill=group, label=participant)) + 
  geom_hline(yintercept=.5) +
  geom_violin() +
  geom_boxplot(width = .1, fill='white') +
  theme_classic() +
  scale_x_discrete(name="group", 
                   limits = c('english', 'korean', 'mandarin'),
                   labels = c('ENS', 'KLE', 'MLE')) +
  scale_y_continuous(name="accuracy rate", 
                     limits=c(0, 1)) +
  theme(text = element_text(size = 12), 
        plot.title = element_text(size = 12, hjust = .5), 
        legend.position = "hide") +
  facet_wrap(~study)

# trim based on accuracy

spr_trim <- spr_trim %>%
  filter(acc_rate > .5)

# check participants

check <- spr_trim %>%
  group_by(study, group, participant) %>%
  summarise() %>%
  summarise(n = n()) %>%
  ungroup()

# summarise data for plotting by group

plot <- spr_trim %>%
  mutate(environment = as.factor(environment)) %>%
  mutate(environment = fct_relevel(environment, 'short', 'long', 'island')) %>%
  group_by(study, group, region2, dependency, environment, condition) %>%
  summarise(mean_rrt = mean(rrt, na.rm=T),
            sd = sd(rrt, na.rm=T),
            n = n()) %>%
  mutate(se = sd / sqrt(n),
         ci = qt(1 - (0.05 / 2), n - 1) * se) %>%
  ungroup() %>%
  filter(region2 %in% c(-3, -2, -1, 0, 1, 2, 3, 4)) %>%
  filter(group %in% c('english', 'korean', 'mandarin')) %>%
  mutate(panel = case_when(group == 'english' ~ 'ENS',
                           group == 'korean' ~ 'KLE',
                           group == 'mandarin' ~ 'MLE')) %>%
  mutate(environment = fct_relevel(environment, 'short', 'long', 'island'))

# generate plot

p1 <- ggplot(data=filter(plot, study == '210510_do'), aes(x=region2, y=mean_rrt, group=dependency, col=dependency, shape=dependency))
p2 <- ggplot(data=filter(plot, study == '210510_su'), aes(x=region2, y=mean_rrt, group=dependency, col=dependency, shape=dependency))

s <- list(
  annotate('rect', xmin = 0.6, xmax = 1.4, ymin = -180, ymax = 180, alpha = .15),
  annotate('rect', xmin = 1.6, xmax = 2.4, ymin = -180, ymax = 180, alpha = .15),
  annotate('rect', xmin = 2.6, xmax = 3.4, ymin = -180, ymax = 180, alpha = .15),
  geom_hline(yintercept = 0),
  geom_vline(xintercept = 0),
  geom_line(lwd=1),
  geom_point(size=2),
  geom_errorbar(aes(ymin=mean_rrt-ci, ymax=mean_rrt+ci), width=.5, lwd=1, linetype=1),
  theme_classic(),
  scale_y_continuous(name="residual reading time (ms)", limits=c(-200, 200)),
  scale_x_continuous(name="region", limits=c(-3.25, 4.25), breaks = c(-3, -2, -1, 0, 1, 2, 3, 4)),
  scale_colour_manual(name="dependency", values=c('#648fff', '#ffb000'), labels=c('gap', 'resumption')),
  scale_shape_manual(name="dependency", values=c(16, 15), labels=c('gap', 'resumption')),
  theme(text = element_text(size = 12),
        legend.position = 'bottom',
        legend.margin = margin(t = -.4, unit = 'cm'),
        plot.title = element_text(size = 12, hjust = .5)),
  facet_grid2(vars(panel), vars(environment), axes = 'all', remove_labels = 'y')
)

p1 + s + 
  geom_text(data = filter(plot, environment == 'island', panel == 'ENS'), mapping = aes(x = 1, y = 185, label = '*'), col = 'black') +
  geom_text(data = filter(plot, environment == 'short', panel == 'ENS'), mapping = aes(x = 2, y = 185, label = '*'), col = 'black') +
  geom_text(data = filter(plot, environment == 'island', panel == 'ENS'), mapping = aes(x = 2, y = 185, label = '*'), col = 'black') +
  geom_text(data = filter(plot, environment == 'long', panel == 'KLE'), mapping = aes(x = 2, y = 185, label = '*'), col = 'black') +
  geom_text(data = filter(plot, environment == 'island', panel == 'KLE'), mapping = aes(x = 2, y = 195, label = '·'), col = 'black') +
  geom_text(data = filter(plot, environment == 'short', panel == 'MLE'), mapping = aes(x = 2, y = 185, label = '*'), col = 'black') +
  geom_text(data = filter(plot, environment == 'long', panel == 'MLE'), mapping = aes(x = 2, y = 185, label = '*'), col = 'black') +
  geom_text(data = filter(plot, environment == 'island', panel == 'MLE'), mapping = aes(x = 2, y = 185, label = '*'), col = 'black')

ggsave('plots/orc/spr_rrt.png', width=6.5, height=4.5, dpi=600)

p2 + s +
  geom_text(data = filter(plot, environment == 'short', panel == 'ENS'), mapping = aes(x = 2, y = 195, label = '·'), col = 'black') +
  geom_text(data = filter(plot, environment == 'island', panel == 'ENS'), mapping = aes(x = 2, y = 185, label = '*'), col = 'black') +
  geom_text(data = filter(plot, environment == 'island', panel == 'ENS'), mapping = aes(x = 3, y = 185, label = '*'), col = 'black') +
  geom_text(data = filter(plot, environment == 'long', panel == 'KLE'), mapping = aes(x = 2, y = 185, label = '*'), col = 'black') +
  geom_text(data = filter(plot, environment == 'short', panel == 'KLE'), mapping = aes(x = 3, y = 185, label = '*'), col = 'black') +
  geom_text(data = filter(plot, environment == 'island', panel == 'KLE'), mapping = aes(x = 3, y = 185, label = '*'), col = 'black') +
  geom_text(data = filter(plot, environment == 'long', panel == 'MLE'), mapping = aes(x = 1, y = 185, label = '*'), col = 'black') +
  geom_text(data = filter(plot, environment == 'long', panel == 'MLE'), mapping = aes(x = 2, y = 185, label = '*'), col = 'black') +
  geom_text(data = filter(plot, environment == 'island', panel == 'MLE'), mapping = aes(x = 1, y = 185, label = '*'), col = 'black') +
  geom_text(data = filter(plot, environment == 'island', panel == 'MLE'), mapping = aes(x = 2, y = 195, label = '·'), col = 'black')

ggsave('plots/src/spr_rrt.png', width=6.5, height=4.5, dpi=600)

#------------------------------------------------------------------------------#
#### spr: modeling for residual RTs ####
#------------------------------------------------------------------------------#

# filter data for analysis

md <- spr_trim %>%
  filter(region2 == 3,
         study == '210510_su',
         group == 'mandarin')

# check distribution

hist(md$rrt)
qqnorm(md$rrt)

md <- md %>%
  mutate(dependency = as.factor(dependency),
         environment = as.factor(environment)) %>%
  mutate(environment = fct_relevel(environment, 'short', 'long', 'island'))

# view contrasts

contrasts(md$dependency)
contrasts(md$environment)

# full model

mod_spr_1 <- lmer(rrt ~ environment*dependency + (environment*dependency|participant) + (environment*dependency|item), data = md)
summary(mod_spr_1)
beep(1)

# doen region 1
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#                                     Estimate Std. Error      df t value Pr(>|t|)   
# (Intercept)                           -6.094      8.216  37.583  -0.742  0.46288   
# environmentlong                        6.518     12.629  40.439   0.516  0.60859   
# environmentisland                     28.809     11.464  65.225   2.513  0.01446 * 
# dependencypronoun                      2.508     11.716  93.254   0.214  0.83097   
# environmentlong:dependencypronoun    -15.898     17.741  44.225  -0.896  0.37507   
# environmentisland:dependencypronoun  -52.826     16.303 113.487  -3.240  0.00157 **
saveRDS(mod_spr_1, file='models/orc_spr_doen_region1_mod.rds') 
cat(capture.output(summary(mod_spr_1)), file = paste('models/orc_spr_doen_region1_mod.txt', sep = ''), sep='\n')
# mod_spr_1 <- readRDS('models/orc_spr_doen_region1_mod.rds')
# https://www.r-bloggers.com/2012/04/a-better-way-of-saving-and-loading-objects-in-r/

# doen region 2
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error      df t value Pr(>|t|)    
# (Intercept)                           18.909      7.447  82.527   2.539 0.012985 *  
#   environmentlong                       -1.983      7.723 814.992  -0.257 0.797429    
# environmentisland                     26.983      9.589  47.803   2.814 0.007086 ** 
#   dependencypronoun                     24.560      9.479  68.194   2.591 0.011697 *  
#   environmentlong:dependencypronoun    -16.569     13.173  36.622  -1.258 0.216433    
# environmentisland:dependencypronoun  -50.294     14.149  73.771  -3.555 0.000665 ***
saveRDS(mod_spr_1, file='models/orc_spr_doen_region2_mod.rds') 
cat(capture.output(summary(mod_spr_1)), file = paste('models/orc_spr_doen_region1_mod.txt', sep = ''), sep='\n')
# mod_spr_1 <- readRDS('models/orc_spr_doen_region2_mod.rds')

# doen region 3
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error       df t value Pr(>|t|)  
# (Intercept)                          16.2765     8.8078  68.0970   1.848   0.0690 .
# environmentlong                      -4.5110     7.6742  67.6572  -0.588   0.5586  
# environmentisland                    12.8944     7.7875  63.8697   1.656   0.1027  
# dependencypronoun                    13.0496     7.5429  86.1457   1.730   0.0872 .
# environmentlong:dependencypronoun     0.6997    11.9051  38.2415   0.059   0.9534  
# environmentisland:dependencypronoun -16.4919    12.7929  34.1701  -1.289   0.2060 
saveRDS(mod_spr_1, file='models/orc_spr_doen_region3_mod.rds') 
cat(capture.output(summary(mod_spr_1)), file = paste('models/orc_spr_doen_region3_mod.txt', sep = ''), sep='\n')

# doko region 1
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error      df t value Pr(>|t|)
# (Intercept)                          -22.700     17.960  46.022  -1.264    0.213
# environmentlong                       -2.302     20.502  42.972  -0.112    0.911
# environmentisland                     -6.051     22.236  31.049  -0.272    0.787
# dependencypronoun                    -33.742     22.493  36.054  -1.500    0.142
# environmentlong:dependencypronoun     12.442     25.884  90.654   0.481    0.632
# environmentisland:dependencypronoun   31.638     31.099  31.953   1.017    0.317
saveRDS(mod_spr_1, file='models/orc_spr_doko_region1_mod.rds') 
cat(capture.output(summary(mod_spr_1)), file = paste('models/orc_spr_doko_region1_mod.txt', sep = ''), sep='\n')

# doko region 2
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error     df t value Pr(>|t|)  
# (Intercept)                           -40.71      17.18  68.23  -2.370   0.0206 *
# environmentlong                        35.00      17.41  50.39   2.010   0.0498 *
# environmentisland                      28.40      20.58  61.53   1.380   0.1726  
# dependencypronoun                     -19.45      14.52  95.38  -1.340   0.1834  
# environmentlong:dependencypronoun     -46.21      23.52  47.88  -1.965   0.0553 .
# environmentisland:dependencypronoun   -20.75      25.09  55.81  -0.827   0.4118  
saveRDS(mod_spr_1, file='models/orc_spr_doko_region2_mod.rds') 
cat(capture.output(summary(mod_spr_1)), file = paste('models/orc_spr_doko_region2_mod.txt', sep = ''), sep='\n')
# mod_spr_1 <- readRDS('models/orc_spr_doko_region2_mod.rds')

# doko region 3
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error      df t value Pr(>|t|)
# (Intercept)                          -25.335     18.481  57.836  -1.371    0.176
# environmentlong                      -20.596     16.682  50.175  -1.235    0.223
# environmentisland                      3.596     18.830  29.555   0.191    0.850
# dependencypronoun                    -30.901     18.613  43.521  -1.660    0.104
# environmentlong:dependencypronoun      5.751     23.048  96.870   0.250    0.803
# environmentisland:dependencypronoun    8.417     25.537  30.334   0.330    0.744
saveRDS(mod_spr_1, file='models/orc_spr_doko_region3_mod.rds') 
cat(capture.output(summary(mod_spr_1)), file = paste('models/orc_spr_doko_region3_mod.txt', sep = ''), sep='\n')

# dozh region 1
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error      df t value Pr(>|t|)  
# (Intercept)                          -12.773     21.019  50.146  -0.608   0.5461  
# environmentlong                        4.094     24.508  36.950   0.167   0.8683  
# environmentisland                    -15.724     20.665  89.120  -0.761   0.4487  
# dependencypronoun                    -39.771     23.848  45.319  -1.668   0.1023  
# environmentlong:dependencypronoun      4.434     29.625  80.336   0.150   0.8814  
# environmentisland:dependencypronoun   54.108     30.646  39.095   1.766   0.0853 .
saveRDS(mod_spr_1, file='models/orc_spr_dozh_region1_mod.rds') 
cat(capture.output(summary(mod_spr_1)), file = paste('models/orc_spr_dozh_region1_mod.txt', sep = ''), sep='\n')
# mod_spr_1 <- readRDS('models/orc_spr_dozh_region1_mod.rds')

# dozh region 2
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error     df t value Pr(>|t|)  
# (Intercept)                           -40.50      17.77  58.03  -2.279   0.0264 *
# environmentlong                        25.54      20.19  36.88   1.265   0.2138  
# environmentisland                      33.54      21.17  42.22   1.585   0.1205  
# dependencypronoun                     -35.86      14.94  65.28  -2.400   0.0193 *
# environmentlong:dependencypronoun     -26.83      24.06  48.97  -1.115   0.2702  
# environmentisland:dependencypronoun   -22.34      25.85  47.17  -0.864   0.3919
saveRDS(mod_spr_1, file='models/orc_spr_dozh_region2_mod.rds') 
cat(capture.output(summary(mod_spr_1)), file = paste('models/orc_spr_dozh_region2_mod.txt', sep = ''), sep='\n')
# mod_spr_1 <- readRDS('models/orc_spr_dozh_region2_mod.rds')

# dozh region 3
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error      df t value Pr(>|t|)
# (Intercept)                          -16.755     19.898  53.210  -0.842    0.404
# environmentlong                      -16.891     21.333  32.707  -0.792    0.434
# environmentisland                     -5.552     24.676  35.726  -0.225    0.823
# dependencypronoun                    -23.355     20.605  45.019  -1.133    0.263
# environmentlong:dependencypronoun     10.054     25.456  57.158   0.395    0.694
# environmentisland:dependencypronoun    8.032     29.335  39.327   0.274    0.786
saveRDS(mod_spr_1, file='models/orc_spr_dozh_region3_mod.rds') 
cat(capture.output(summary(mod_spr_1)), file = paste('models/orc_spr_dozh_region3_mod.txt', sep = ''), sep='\n')

# suen region 1
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error       df t value Pr(>|t|)    
# (Intercept)                         -94.4957    11.5507  52.3598  -8.181 6.28e-11 ***
# environmentlong                       0.4633    15.2077  68.2516   0.030   0.9758    
# environmentisland                    35.4499    17.8622  44.7862   1.985   0.0533 .  
# dependencypronoun                    35.7479    15.8883  35.0287   2.250   0.0308 *  
# environmentlong:dependencypronoun    -5.9114    26.0650  33.3015  -0.227   0.8220    
# environmentisland:dependencypronoun -54.1687    27.3279  28.7903  -1.982   0.0571 . 
saveRDS(mod_spr_1, file='models/src_spr_suen_region1_mod.rds') 
cat(capture.output(summary(mod_spr_1)), file = paste('models/src_spr_suen_region1_mod.txt', sep = ''), sep='\n')
# mod_spr_1 <- readRDS('models/src_spr_suen_region1_mod.rds')

# suen region 2
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error      df t value Pr(>|t|)    
# (Intercept)                           -55.49      12.68   45.73  -4.377 6.93e-05 ***
# environmentlong                        13.23      18.61   45.12   0.711   0.4807    
# environmentisland                      94.65      12.18  271.83   7.769 1.63e-13 ***
# dependencypronoun                      29.40      13.26   57.68   2.216   0.0306 *  
# environmentlong:dependencypronoun     -33.09      19.92   55.13  -1.661   0.1024    
# environmentisland:dependencypronoun  -109.14      19.11   55.37  -5.711 4.60e-07 ***
saveRDS(mod_spr_1, file='models/src_spr_suen_region2_mod.rds') 
cat(capture.output(summary(mod_spr_1)), file = paste('models/src_spr_suen_region2_mod.txt', sep = ''), sep='\n')
# mod_spr_1 <- readRDS('models/src_spr_suen_region2_mod.rds')

# suen region 3
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error      df t value Pr(>|t|)    
# (Intercept)                          -103.74      16.55   28.21  -6.267 8.66e-07 ***
# environmentlong                        18.05      15.42   32.72   1.170  0.25034    
# environmentisland                     187.31      23.15   45.43   8.090 2.38e-10 ***
# dependencypronoun                      17.83      15.81   34.41   1.128  0.26721    
# environmentlong:dependencypronoun     -11.90      22.59   32.70  -0.527  0.60200    
# environmentisland:dependencypronoun   -74.48      22.52   42.96  -3.308  0.00191 ** 
saveRDS(mod_spr_1, file='models/src_spr_suen_region3_mod.rds') 
cat(capture.output(summary(mod_spr_1)), file = paste('models/src_spr_suen_region3_mod.txt', sep = ''), sep='\n')
# mod_spr_1 <- readRDS('models/src_spr_suen_region3_mod.rds')

# suko region 1
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error       df t value Pr(>|t|)
# (Intercept)                          11.8752    26.7665  32.1748   0.444    0.660
# environmentlong                     -14.9229    39.0395  35.4573  -0.382    0.705
# environmentisland                     0.8691    39.7102  29.9687   0.022    0.983
# dependencypronoun                   -15.8171    32.4809  63.1945  -0.487    0.628
# environmentlong:dependencypronoun   -14.2447    54.4191  54.4822  -0.262    0.794
# environmentisland:dependencypronoun -13.4231    56.9894  38.8554  -0.236    0.815
saveRDS(mod_spr_1, file='models/src_spr_suko_region1_mod.rds') 
cat(capture.output(summary(mod_spr_1)), file = paste('models/src_spr_suko_region1_mod.txt', sep = ''), sep='\n')

# suko region 2
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error       df t value Pr(>|t|)   
# (Intercept)                           -9.833     19.532   39.301  -0.503  0.61747   
# environmentlong                       73.007     29.217   34.140   2.499  0.01744 * 
# environmentisland                     -7.263     26.873   36.802  -0.270  0.78847   
# dependencypronoun                    -12.304     27.666   40.168  -0.445  0.65888   
# environmentlong:dependencypronoun   -103.034     35.722   47.964  -2.884  0.00586 **
# environmentisland:dependencypronoun    4.768     41.479   32.796   0.115  0.90918  
saveRDS(mod_spr_1, file='models/src_spr_suko_region2_mod.rds') 
cat(capture.output(summary(mod_spr_1)), file = paste('models/src_spr_suko_region2_mod.txt', sep = ''), sep='\n')

# suko region 3
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error      df t value Pr(>|t|)    
# (Intercept)                           -76.23      18.03   70.73  -4.228 6.94e-05 ***
# environmentlong                        60.95      29.51   45.10   2.065 0.044701 *  
# environmentisland                     125.22      27.11   62.84   4.619 1.96e-05 ***
# dependencypronoun                      67.74      26.50   51.88   2.556 0.013558 *  
# environmentlong:dependencypronoun    -101.79      39.39   47.10  -2.584 0.012927 *  
# environmentisland:dependencypronoun  -140.36      34.38   81.45  -4.083 0.000103 ***
saveRDS(mod_spr_1, file='models/src_spr_suko_region3_mod.rds') 
cat(capture.output(summary(mod_spr_1)), file = paste('models/src_spr_suko_region3_mod.txt', sep = ''), sep='\n')
# mod_spr_1 <- readRDS('models/src_spr_suko_region3_mod.rds')

# suzh region 1
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error      df t value Pr(>|t|)    
# (Intercept)                            12.81      23.81   45.28   0.538 0.593192    
# environmentlong                        54.39      35.07   46.38   1.551 0.127767    
# environmentisland                     108.05      31.21   74.53   3.462 0.000892 ***
# dependencypronoun                     -10.62      29.14   63.91  -0.364 0.716848    
# environmentlong:dependencypronoun    -112.29      47.51   39.20  -2.364 0.023153 *  
# environmentisland:dependencypronoun  -116.55      43.54   63.17  -2.677 0.009457 ** 
saveRDS(mod_spr_1, file='models/src_spr_suzh_region1_mod.rds') 
cat(capture.output(summary(mod_spr_1)), file = paste('models/src_spr_suzh_region1_mod.txt', sep = ''), sep='\n')

# suzh region 2
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)                          -63.1519    19.2731   79.8946  -3.277  0.00156 ** 
# environmentlong                      171.1295    31.4737   59.9389   5.437 1.05e-06 ***
# environmentisland                     53.4415    32.3608   58.6065   1.651  0.10400    
# dependencypronoun                     -0.9302    24.0921  367.5248  -0.039  0.96922    
# environmentlong:dependencypronoun   -176.4626    40.7114   69.6624  -4.334 4.82e-05 ***
# environmentisland:dependencypronoun  -50.2838    34.2306  265.3421  -1.469  0.14303  
saveRDS(mod_spr_1, file='models/src_spr_suzh_region2_mod.rds') 
cat(capture.output(summary(mod_spr_1)), file = paste('models/src_spr_suzh_region2_mod.txt', sep = ''), sep='\n')

# suzh region 3
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error      df t value Pr(>|t|)
# (Intercept)                           13.256     33.098  47.181   0.401    0.691
# environmentlong                       31.380     37.616  36.211   0.834    0.410
# environmentisland                    -23.822     39.817  55.114  -0.598    0.552
# dependencypronoun                    -20.730     32.301  38.533  -0.642    0.525
# environmentlong:dependencypronoun     -3.404     52.579  40.549  -0.065    0.949
# environmentisland:dependencypronoun  -13.256     40.077  54.968  -0.331    0.742
saveRDS(mod_spr_1, file='models/src_spr_suzh_region3_mod.rds') 
cat(capture.output(summary(mod_spr_1)), file = paste('models/src_spr_suzh_region3_mod.txt', sep = ''), sep='\n')

# https://marissabarlaz.github.io/portfolio/contrastcoding/

# check assumptions

performance::check_model(mod_spr_1) # perform checks
ggsave('plots/check_model.png', width=10, height=10, dpi=600) # save output for viewing
performance::model_performance(mod_spr_1) # check model performance

# post-hoc tests: pairwise comparisons of estimated marginal means (see https://stats.stackexchange.com/questions/424304/when-to-correct-for-multiple-comparisons-with-specific-reference-to-emmeans-in)

# pairs(emmeans(mod_spr_1, 'dependency', by = 'environment'))

mod_spr_1 %>%
  emmeans(~ dependency * environment) %>%
  contrast('pairwise', by = 'environment') %>%
  summary(by = NULL, adjust = 'holm')
beep(1)

# doen region 1
# contrast      environment estimate   SE   df t.ratio p.value
# gap - pronoun short          -2.51 11.8 31.1  -0.213  0.8328
# gap - pronoun long           13.39 11.9 26.9   1.122  0.5437
# gap - pronoun island         50.32 11.8 29.2   4.247  0.0006 **
# Degrees-of-freedom method: kenward-roger 
# P value adjustment: holm method for 3 tests 

# doen region 2
# contrast      environment estimate   SE   df t.ratio p.value
# gap - pronoun short         -24.56 9.53 33.8  -2.577  0.0357 *
# gap - pronoun long           -7.99 8.68 23.4  -0.920  0.3668
# gap - pronoun island         25.73 9.68 33.7   2.659  0.0357 *
# Degrees-of-freedom method: kenward-roger 
# P value adjustment: holm method for 3 tests 

# doko region 2
# contrast      environment estimate   SE   df t.ratio p.value
# gap - pronoun short           19.5 14.7 23.9   1.325  0.1976
# gap - pronoun long            65.7 17.3 28.2   3.790  0.0022 **
# gap - pronoun island          40.2 18.8 32.5   2.143  0.0794 .
# Degrees-of-freedom method: kenward-roger 
# P value adjustment: holm method for 3 tests 

# dozh region 1
# contrast      environment estimate   SE   df t.ratio p.value
# gap - pronoun short           39.8 24.1 27.4   1.650  0.3310
# gap - pronoun long            35.3 23.6 28.4   1.497  0.3310
# gap - pronoun island         -14.3 23.9 26.4  -0.599  0.5541
# Degrees-of-freedom method: kenward-roger 
# P value adjustment: holm method for 3 tests 

# dozh region 2
# contrast      environment estimate   SE   df t.ratio p.value
# gap - pronoun short           35.9 15.2 22.2   2.364  0.0386 *
# gap - pronoun long            62.7 19.1 33.5   3.284  0.0072 **
# gap - pronoun island          58.2 24.0 46.2   2.425  0.0386 *
# Degrees-of-freedom method: kenward-roger 
# P value adjustment: holm method for 3 tests 

# suen region 1
# contrast      environment estimate   SE   df t.ratio p.value
# gap - pronoun short          -35.7 15.9 23.0  -2.242  0.1048
# gap - pronoun long           -29.8 20.1 39.0  -1.488  0.2896
# gap - pronoun island          18.4 22.2 46.3   0.831  0.4101
# Degrees-of-freedom method: kenward-roger 
# P value adjustment: holm method for 3 tests 

# suen region 2
# contrast      environment estimate   SE   df t.ratio p.value
# gap - pronoun short         -29.40 13.3 23.9  -2.207  0.0743 .
# gap - pronoun long            3.69 15.0 28.2   0.246  0.8072
# gap - pronoun island         79.75 13.0 21.7   6.129  <.0001 ***
# Degrees-of-freedom method: kenward-roger 
# P value adjustment: holm method for 3 tests 

# suen region 3
# contrast      environment estimate   SE   df t.ratio p.value
# gap - pronoun short         -17.83 15.9 31.8  -1.124  0.5389
# gap - pronoun long           -5.93 14.6 23.4  -0.406  0.6886
# gap - pronoun island         56.65 17.5 35.7   3.242  0.0077 **
# Degrees-of-freedom method: kenward-roger 
# P value adjustment: holm method for 3 tests

# suko region 2
# contrast      environment estimate   SE   df t.ratio p.value
# gap - pronoun short          12.30 28.0 30.8   0.440  0.9618
# gap - pronoun long          115.34 31.1 37.2   3.708  0.0020 **
# gap - pronoun island          7.54 25.3 25.0   0.298  0.9876
# Degrees-of-freedom method: kenward-roger 
# P value adjustment: sidak method for 3 tests 

# suko region 3
# contrast      environment estimate   SE   df t.ratio p.value
# gap - pronoun short          -67.7 26.8 25.3  -2.527  0.0363 *
# gap - pronoun long            34.1 33.8 34.3   1.007  0.3208
# gap - pronoun island          72.6 24.4 22.8   2.971  0.0206 *
# Degrees-of-freedom method: kenward-roger 
# P value adjustment: holm method for 3 tests 

# suzh region 1
# contrast      environment estimate   SE   df t.ratio p.value
# gap - pronoun short           10.6 29.3 23.6   0.363  0.7200
# gap - pronoun long           122.9 36.7 35.4   3.350  0.0039 **
# gap - pronoun island         127.2 30.4 25.5   4.185  0.0009 ***
# Degrees-of-freedom method: kenward-roger 
# P value adjustment: holm method for 3 tests 

# suzh region 2
# contrast      environment estimate   SE   df t.ratio p.value
# gap - pronoun short           0.93 24.2 21.0   0.038  0.9697
# gap - pronoun long          177.39 31.7 35.7   5.600  <.0001 ***
# gap - pronoun island         51.21 24.5 20.8   2.090  0.0982 .
# Degrees-of-freedom method: kenward-roger 
# P value adjustment: holm method for 3 tests 

#------------------------------------------------------------------------------#
#### spr: modeling for reading times at critical region ####
#------------------------------------------------------------------------------#

trim <- trim %>%
  mutate(region = as.numeric(region)) %>%
  mutate(region2 = case_when(study == '210510_do' ~ region - 11,
                             study == '210510_su' & environment %in% c('short', 'long') ~ region - 8,
                             study == '210510_su' & environment == 'island' ~ region - 10))

# md <- trim %>%
#   filter(region2 %in% c('1', '2', '3')) %>%
#   mutate(logrt = log(rt))

md <- trim %>%
  filter(region2 %in% c('1', '2', '3')) %>%
  mutate(logrt = log(rt)) %>%
  group_by(study, group, participant, item, environment, dependency) %>%
  summarise(logrt = mean(logrt)) %>%
  ungroup()

md <- md %>%
  filter(study == '210510_do')

# summarise for plotting with logrts
plot <- md %>%
  group_by(group, environment, dependency) %>%
  summarise(mean = mean(logrt, na.rm=T),
            sd = sd(logrt, na.rm=T),
            n = n()) %>%
  mutate(se = sd / sqrt(n),
         ci = qt(1 - (0.05 / 2), n - 1) * se) %>%
  ungroup()

# facet labels
groups <- c(`english` = 'ENS', `korean` = 'KLE', `mandarin` = 'MLE')

# generate plot
p <- ggplot(data=plot, aes(x=environment, y=mean, group=dependency, col=dependency, shape=dependency)) +
  geom_line(lwd=1) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.5, lwd=1, linetype=1) +
  theme_classic() +
  scale_y_continuous(name="logRT", limits=c(5.75, 6.15)) +
  scale_x_discrete(name="environment", limits=c("short", "long", "island")) +
  scale_colour_manual(name="dependency", values=c('#648fff', '#ffb000'), limits=c('gap', 'pronoun'), labels=c('gap', 'resumption')) +
  scale_shape_manual(name="dependency", values=c(16, 15), limits=c('gap', 'pronoun'), labels=c('gap', 'resumption')) +
  theme(text = element_text(size = 12), 
        plot.title = element_text(size = 12, hjust = .5), 
        legend.position = 'right',
        legend.margin = margin(0, .1, 0, -.1, 'cm'),
        legend.box.margin = margin(0, .1, 0, -.1, 'cm'),
        plot.margin = margin(0, 0, 0, 0, 'cm')) +
  facet_wrap(~group, labeller = as_labeller(groups))

p

bucld1 <- p + 
  labs(caption = 'lmer: logRT ~ dependency * environment +\n(1 + dependency * environment | person) +\n(1 + dependency * environment | item)', hjust = .5) +
  theme(plot.caption = element_text(hjust = .5))

bucld1

# save plot
ggsave("plots/src/spr_logrt.png", width=6.5, height=2.5, dpi=600)
ggsave("plots/src/spr_bucld.png", width=6.5, height=2.5, dpi=600)

md2 <- md %>%
  filter(group == 'mandarin')

check <- md2 %>%
  group_by(participant) %>%
  summarise(rt = mean(rt, na.rm = T)) %>%
  ungroup()

# check distribution
hist(md2$logrt)
qqnorm(md2$logrt)

md2 <- md2 %>%
  mutate(dependency = as.factor(dependency),
         environment = as.factor(environment))

# relevel 'environment'
md2 <- md2 %>%
  mutate(environment = fct_relevel(environment, 'short', 'long', 'island'))

# view contrasts
contrasts(md2$dependency)
contrasts(md2$environment)

# source on different coding schemes
# https://marissabarlaz.github.io/portfolio/contrastcoding/

# full model
model1 <- lmer(logrt ~ environment*dependency + (environment*dependency|participant) + (environment*dependency|item), data = md2)
summary(model1)
beepr::beep(1)

# doen
#
# without averaging...
# boundary (singular) fit: see ?isSingular
# Model failed to converge with 2 negative eigenvalues: -1.8e-01 -2.1e+02 
# Fixed effects:
#                                       Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)                          5.851203   0.022422 93.520637 260.962  < 2e-16 ***
# environmentlong                      0.001173   0.017325 25.809871   0.068  0.94656    
# environmentisland                    0.051808   0.015862 55.688740   3.266  0.00187 ** 
# dependencypronoun                    0.031849   0.016839 50.373166   1.891  0.06433 .  
# environmentlong:dependencypronoun   -0.021834   0.026851 32.208411  -0.813  0.42210    
# environmentisland:dependencypronoun -0.096617   0.026958 50.634057  -3.584  0.00076 ***
#
# with averaging...
# boundary (singular) fit: see ?isSingular (no nonconvergence issue!)
# Fixed effects:
#   Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)                          5.850046   0.022933 92.341638 255.096  < 2e-16 ***
#   environmentlong                      0.002268   0.018068 35.901916   0.126  0.90082    
# environmentisland                    0.046766   0.016368 63.415208   2.857  0.00578 ** 
#   dependencypronoun                    0.029873   0.016747 46.307951   1.784  0.08102 .  
# environmentlong:dependencypronoun   -0.022133   0.026378 31.151029  -0.839  0.40784    
# environmentisland:dependencypronoun -0.094453   0.026906 39.106631  -3.510  0.00114 ** 

# doko
#
# without averaging...
# boundary (singular) fit: see ?isSingular
# Fixed effects:
#                                      Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)                          5.934595   0.026826 62.710435 221.229   <2e-16 ***
# environmentlong                      0.019087   0.021199 44.580646   0.900   0.3728    
# environmentisland                    0.015728   0.021860 36.997765   0.719   0.4764    
# dependencypronoun                   -0.052107   0.025114 38.996818  -2.075   0.0446 *  
# environmentlong:dependencypronoun   -0.034343   0.033133 35.650116  -1.037   0.3069    
# environmentisland:dependencypronoun  0.007855   0.032453 36.360479   0.242   0.8101  
#
# with averaging...
# boundary (singular) fit: see ?isSingular
# Fixed effects:
#   Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)                          5.931231   0.026867 61.290832 220.766   <2e-16 ***
#   environmentlong                      0.017635   0.021429 38.444778   0.823   0.4156    
# environmentisland                    0.014878   0.023069 36.228101   0.645   0.5230    
# dependencypronoun                   -0.052715   0.025207 53.744282  -2.091   0.0412 *  
#   environmentlong:dependencypronoun   -0.037004   0.033533 35.008117  -1.104   0.2773    
# environmentisland:dependencypronoun  0.008101   0.032819 36.788360   0.247   0.8064  

# dozh
# 
# without averaging...
# boundary (singular) fit: see ?isSingular
# Fixed effects:
#                                     Estimate Std. Error       df t value Pr(>|t|)    
# (Intercept)                          6.04310    0.02902 71.85333 208.216   <2e-16 ***
# environmentlong                      0.01385    0.02538 29.86785   0.545    0.589    
# environmentisland                    0.01920    0.02505 30.28848   0.767    0.449    
# dependencypronoun                   -0.05321    0.02263 31.95354  -2.352    0.025 *  
# environmentlong:dependencypronoun   -0.01920    0.02852 46.29351  -0.673    0.504    
# environmentisland:dependencypronoun  0.01515    0.03179 34.61964   0.476    0.637  
# 
# with averaging...
# boundary (singular) fit: see ?isSingular
# Fixed effects:
#   Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)                          6.042285   0.029077 69.899753 207.803   <2e-16 ***
#   environmentlong                      0.009459   0.025225 29.886558   0.375    0.710    
# environmentisland                    0.016977   0.024402 26.450115   0.696    0.493    
# dependencypronoun                   -0.057216   0.022710 31.827845  -2.519    0.017 *  
#   environmentlong:dependencypronoun   -0.016692   0.030010 71.320673  -0.556    0.580    
# environmentisland:dependencypronoun  0.020514   0.032382 34.959917   0.633    0.531   

# suen
# 
# without averaging...
# boundary (singular) fit: see ?isSingular
# Model failed to converge with max|grad| = 0.00434682 (tol = 0.002, component 1)
# Fixed effects:
#                                       Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                          5.977e+00  4.155e-02  6.131e+01 143.830  < 2e-16 ***
# environmentlong                     -5.446e-06  2.342e-02  4.028e+01   0.000  0.99982    
# environmentisland                    6.638e-02  2.287e-02  4.498e+01   2.903  0.00571 ** 
# dependencypronoun                    4.723e-02  2.029e-02  4.097e+01   2.328  0.02495 *  
# environmentlong:dependencypronoun   -1.384e-02  3.634e-02  4.333e+01  -0.381  0.70517    
# environmentisland:dependencypronoun -1.787e-01  2.902e-02  4.038e+01  -6.157 2.74e-07 ***
#
# with averaging...
# boundary (singular) fit: see ?isSingular
# Fixed effects:
#   Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)                          5.977221   0.042378 60.989130 141.045  < 2e-16 ***
#   environmentlong                      0.001879   0.022681 34.112852   0.083  0.93446    
# environmentisland                    0.059808   0.020932 55.346670   2.857  0.00601 ** 
#   dependencypronoun                    0.044526   0.019783 44.847104   2.251  0.02935 *  
#   environmentlong:dependencypronoun   -0.007345   0.034787 38.583846  -0.211  0.83389    
# environmentisland:dependencypronoun -0.171831   0.028749 63.337184  -5.977 1.14e-07 ***

# suko
#
# without averaging...
# boundary (singular) fit: see ?isSingular
# Model failed to converge with 2 negative eigenvalues: -1.5e-02 -5.7e+01 
# Fixed effects:
#                                     Estimate Std. Error       df t value Pr(>|t|)    
# (Intercept)                          6.16793    0.03943 60.84806 156.444  < 2e-16 ***
# environmentlong                      0.05860    0.03062 41.39200   1.914  0.06257 .  
# environmentisland                   -0.06033    0.03099 37.26530  -1.947  0.05913 .  
# dependencypronoun                    0.02906    0.03045 55.30056   0.954  0.34403    
# environmentlong:dependencypronoun   -0.12063    0.04500 53.47754  -2.681  0.00974 ** 
# environmentisland:dependencypronoun -0.08803    0.05120 46.32925  -1.719  0.09224 .  
#
# with averaging...
# boundary (singular) fit: see ?isSingular
# Fixed effects:
#   Estimate Std. Error       df t value Pr(>|t|)    
# (Intercept)                          6.16656    0.04064 63.37313 151.723   <2e-16 ***
#   environmentlong                      0.06103    0.03484 37.89302   1.752   0.0879 .  
# environmentisland                   -0.06185    0.03279 29.00591  -1.886   0.0693 .  
# dependencypronoun                    0.03172    0.03120 39.61232   1.017   0.3155    
# environmentlong:dependencypronoun   -0.12690    0.04788 48.77736  -2.651   0.0108 *  
#   environmentisland:dependencypronoun -0.09374    0.05205 33.56103  -1.801   0.0807 .  


# suzh
#
# without averaging...
# boundary (singular) fit: see ?isSingular
# Model failed to converge with 2 negative eigenvalues: -3.3e-02 -1.4e+02
# Fixed effects:
#                                     Estimate Std. Error       df t value Pr(>|t|)    
# (Intercept)                          6.33656    0.03737 70.78606 169.553  < 2e-16 ***
# environmentlong                      0.09084    0.02311 34.22378   3.931 0.000392 ***
# environmentisland                   -0.05647    0.02819 50.45756  -2.003 0.050535 .  
# dependencypronoun                   -0.01636    0.02554 33.48949  -0.641 0.526159    
# environmentlong:dependencypronoun   -0.12255    0.03729 32.82149  -3.286 0.002421 ** 
# environmentisland:dependencypronoun -0.08562    0.03437 29.11434  -2.491 0.018694 *  
# 
# with averaging...
# boundary (singular) fit: see ?isSingular
# Fixed effects:
#   Estimate Std. Error       df t value Pr(>|t|)    
# (Intercept)                          6.33265    0.03677 68.54185 172.231  < 2e-16 ***
#   environmentlong                      0.09841    0.02852 45.77619   3.451  0.00121 ** 
#   environmentisland                   -0.05380    0.02776 46.83593  -1.938  0.05861 .  
# dependencypronoun                   -0.01445    0.02532 32.47188  -0.571  0.57214    
# environmentlong:dependencypronoun   -0.13491    0.04346 40.84855  -3.104  0.00346 ** 
#   environmentisland:dependencypronoun -0.09084    0.03548 36.40588  -2.560  0.01475 *  

# post-hoc tests: pairwise comparisons of estimated marginal means

library(emmeans) # see https://marissabarlaz.github.io/portfolio/contrastcoding/
pairs(emmeans(model1, "dependency", by = "environment"))
beep(1)

# doen
#
# without averaging...
# environment = short:
#   contrast      estimate     SE  df z.ratio p.value
# gap - pronoun  -0.0318 0.0168 Inf  -1.891  0.0586   # gap marginally faster
#
# environment = long:
#   contrast      estimate     SE  df z.ratio p.value
# gap - pronoun  -0.0100 0.0193 Inf  -0.518  0.6043   # n.s.
# 
# environment = island:
#   contrast      estimate     SE  df z.ratio p.value
# gap - pronoun   0.0648 0.0200 Inf   3.238  0.0012   # gap significantly slower
#
# with averaging...
# environment = short:
#   contrast      estimate     SE   df t.ratio p.value
# gap - pronoun -0.02987 0.0168 25.1  -1.775  0.0880
# 
# environment = long:
#   contrast      estimate     SE   df t.ratio p.value
# gap - pronoun -0.00774 0.0189 26.5  -0.409  0.6858
# 
# environment = island:
#   contrast      estimate     SE   df t.ratio p.value
# gap - pronoun  0.06458 0.0196 31.6   3.291  0.0025

# doko
# 
# without averaging...
# environment = short:
#   contrast      estimate     SE  df z.ratio p.value
# gap - pronoun   0.0521 0.0251 Inf   2.075  0.0380
# 
# environment = long:
#   contrast      estimate     SE  df z.ratio p.value
# gap - pronoun   0.0865 0.0245 Inf   3.531  0.0004
# 
# environment = island:
#   contrast      estimate     SE  df z.ratio p.value
# gap - pronoun   0.0443 0.0231 Inf   1.913  0.0557
#
# with averaging...
# environment = short:
#   contrast      estimate     SE   df t.ratio p.value
# gap - pronoun   0.0527 0.0254 32.2   2.074  0.0461
# 
# environment = long:
#   contrast      estimate     SE   df t.ratio p.value
# gap - pronoun   0.0897 0.0241 27.1   3.720  0.0009
# 
# environment = island:
#   contrast      estimate     SE   df t.ratio p.value
# gap - pronoun   0.0446 0.0232 23.8   1.920  0.0669

# dozh
# 
# without averaging...
# environment = short:
#   contrast      estimate     SE  df z.ratio p.value
# gap - pronoun   0.0532 0.0226 Inf   2.352  0.0187
# 
# environment = long:
#   contrast      estimate     SE  df z.ratio p.value
# gap - pronoun   0.0724 0.0246 Inf   2.941  0.0033
# 
# environment = island:
#   contrast      estimate     SE  df z.ratio p.value
# gap - pronoun   0.0381 0.0248 Inf   1.537  0.1242
#
# with averaging...
# environment = short:
#   contrast      estimate     SE   df t.ratio p.value
# gap - pronoun   0.0572 0.0229 23.5   2.497  0.0199
# 
# environment = long:
#   contrast      estimate     SE   df t.ratio p.value
# gap - pronoun   0.0739 0.0250 33.1   2.951  0.0058
# 
# environment = island:
#   contrast      estimate     SE   df t.ratio p.value
# gap - pronoun   0.0367 0.0244 30.7   1.503  0.1429

# suen
# 
# without averaging...
# environment = short:
#   contrast      estimate     SE  df z.ratio p.value
# gap - pronoun  -0.0472 0.0203 Inf  -2.328  0.0199
# 
# environment = long:
#   contrast      estimate     SE  df z.ratio p.value
# gap - pronoun  -0.0334 0.0272 Inf  -1.226  0.2202
# 
# environment = island:
#   contrast      estimate     SE  df z.ratio p.value
# gap - pronoun   0.1315 0.0236 Inf   5.561  <.0001
#
# with averaging...
# environment = short:
#   contrast      estimate     SE   df t.ratio p.value
# gap - pronoun  -0.0445 0.0198 21.0  -2.244  0.0357
# 
# environment = long:
#   contrast      estimate     SE   df t.ratio p.value
# gap - pronoun  -0.0372 0.0267 38.5  -1.391  0.1722
# 
# environment = island:
#   contrast      estimate     SE   df t.ratio p.value
# gap - pronoun   0.1273 0.0239 29.4   5.322  <.0001

# suko
#
# without averaging...
# environment = short:
#   contrast      estimate     SE  df z.ratio p.value
# gap - pronoun  -0.0291 0.0304 Inf  -0.954  0.3399
# 
# environment = long:
#   contrast      estimate     SE  df z.ratio p.value
# gap - pronoun   0.0916 0.0323 Inf   2.832  0.0046
# 
# environment = island:
#   contrast      estimate     SE  df z.ratio p.value
# gap - pronoun   0.0590 0.0325 Inf   1.815  0.0695
# 
# with averaging...
# environment = short:
#   contrast      estimate     SE   df t.ratio p.value
# gap - pronoun  -0.0317 0.0315 28.9  -1.008  0.3220
# 
# environment = long:
#   contrast      estimate     SE   df t.ratio p.value
# gap - pronoun   0.0952 0.0331 35.4   2.879  0.0067
# 
# environment = island:
#   contrast      estimate     SE   df t.ratio p.value
# gap - pronoun   0.0620 0.0322 27.6   1.928  0.0643

# suzh
#
# without averaging...
# environment = short:
#   contrast      estimate     SE  df z.ratio p.value
# gap - pronoun   0.0164 0.0255 Inf   0.641  0.5218
# 
# environment = long:
#   contrast      estimate     SE  df z.ratio p.value
# gap - pronoun   0.1389 0.0268 Inf   5.188  <.0001
# 
# environment = island:
#   contrast      estimate     SE  df z.ratio p.value
# gap - pronoun   0.1020 0.0221 Inf   4.621  <.0001
# 
# with averaging...
# environment = short:
# contrast      estimate     SE   df t.ratio p.value
# gap - pronoun   0.0144 0.0254 23.8   0.570  0.5743
# 
# environment = long:
#   contrast      estimate     SE   df t.ratio p.value
# gap - pronoun   0.1494 0.0318 38.9   4.698  <.0001
# 
# environment = island:
#   contrast      estimate     SE   df t.ratio p.value
# gap - pronoun   0.1053 0.0235 20.9   4.477  0.0002

#------------------------------------------------------------------------------#
#### spr: modeling for reading times at RP region ####
#------------------------------------------------------------------------------#

temp <- trim %>%
  mutate(region = as.numeric(region)) %>%
  mutate(region2 = case_when(study == '210510_do' ~ region - 11,
                             study == '210510_su' & environment %in% c('short', 'long') ~ region - 8,
                             study == '210510_su' & environment == 'island' ~ region - 10))

md <- temp %>%
  filter(dependency == 'pronoun', region2 == '0') %>%
  mutate(logrt = log(rt)) %>%
  group_by(study, group, participant, item, environment, dependency) %>%
  summarise(logrt = mean(logrt)) %>%
  ungroup()

md <- md %>%
  filter(study == '210510_su')

# summarise for plotting with logrts
plot <- md %>%
  group_by(group, environment, dependency) %>%
  summarise(mean = mean(logrt, na.rm=T),
            sd = sd(logrt, na.rm=T),
            n = n()) %>%
  mutate(se = sd / sqrt(n),
         ci = qt(1 - (0.05 / 2), n - 1) * se) %>%
  ungroup()

# facet labels
groups <- c(`english` = 'ENS', `korean` = 'KLE', `mandarin` = 'MLE')

# generate plot
ggplot(data=plot, aes(x=environment, y=mean, group=dependency, col=dependency, shape=dependency)) +
  geom_line(lwd=1) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.5, lwd=1, linetype=1) +
  theme_classic() +
  scale_y_continuous(name="logRT", limits=c(5.8, 6.5)) +
  scale_x_discrete(name="environment", limits=c("short", "long", "island")) +
  scale_colour_manual(name="dependency", values=c('#648fff', '#ffb000'), limits=c('gap', 'pronoun'), labels=c('gap', 'resumption')) +
  scale_shape_manual(name="dependency", values=c(16, 15), limits=c('gap', 'pronoun'), labels=c('gap', 'resumption')) +
  theme(text = element_text(size = 12), plot.title = element_text(size = 12, hjust = .5), legend.position = "right", legend.margin = margin(1, 1, 1, -5)) +
  facet_wrap(~group, labeller = as_labeller(groups))

# save plot
ggsave("plots/orc/spr_logrt_rp_region.png", width=6.5, height=2.5, dpi=600)

md2 <- md %>%
  filter(group == 'korean')

check <- md2 %>%
  group_by(participant) %>%
  summarise(rt = mean(rt, na.rm = T)) %>%
  ungroup()

# check distribution
hist(md2$logrt)
qqnorm(md2$logrt)

md2 <- md2 %>%
  mutate(environment = as.factor(environment))

# relevel 'environment'
md2 <- md2 %>%
  mutate(environment = fct_relevel(environment, 'short', 'long', 'island'))

# view contrasts
contrasts(md2$environment)

# source on different coding schemes
# https://marissabarlaz.github.io/portfolio/contrastcoding/

# full model
model1 <- lmer(logrt ~ environment + (environment|participant) + (environment|item), data = md2)
summary(model1)
beepr::beep(1)

# doen
# boundary (singular) fit: see ?isSingular
# Fixed effects:
# Estimate Std. Error       df t value Pr(>|t|)    
# (Intercept)        6.10923    0.03572 83.42812 171.013  < 2e-16 ***
#   environmentlong   -0.07456    0.02650 76.48794  -2.813  0.00623 ** 
#   environmentisland -0.07803    0.02874 31.03139  -2.715  0.01074 *  

# doko
# boundary (singular) fit: see ?isSingular
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)         6.218143   0.040100  62.006752 155.068   <2e-16 ***
#   environmentlong     0.005061   0.039165  31.211881   0.129    0.898    
# environmentisland  -0.030397   0.037478 267.743902  -0.811    0.418  

# dozh
# boundary (singular) fit: see ?isSingular
# Fixed effects:
#   Estimate Std. Error       df t value Pr(>|t|)    
# (Intercept)        6.30768    0.04016 62.87836 157.064   <2e-16 ***
#   environmentlong    0.03785    0.03924 47.22513   0.965    0.340    
# environmentisland  0.05520    0.03884 40.57832   1.421    0.163 

# suen
# boundary (singular) fit: see ?isSingular
# Fixed effects:
#   Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)        5.975911   0.045757 57.689233 130.601   <2e-16 ***
#   environmentlong   -0.001635   0.026567 48.897678  -0.062    0.951    
# environmentisland  0.053619   0.027180 43.077641   1.973    0.055 .  

# suko
# boundary (singular) fit: see ?isSingular
# Fixed effects:
# Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)         6.07648    0.04653  56.31235 130.603  < 2e-16 ***
#   environmentlong    -0.01132    0.04698  33.88362  -0.241  0.81105    
# environmentisland   0.12001    0.03765 114.28651   3.188  0.00185 ** 

# suzh
# boundary (singular) fit: see ?isSingular
# Fixed effects:
#   Estimate Std. Error       df t value Pr(>|t|)    
# (Intercept)        6.17680    0.03322 67.84903  185.94   <2e-16 ***
#   environmentlong   -0.01445    0.03283 67.27076   -0.44   0.6614    
# environmentisland  0.05609    0.03032 68.08138    1.85   0.0687 . 

#------------------------------------------------------------------------------#
#### spr: modeling for accuracy data ####
#------------------------------------------------------------------------------#

temp <- ds %>%
  group_by(study, group, participant) %>%
  mutate(acc_rate = mean(as.logical(accuracy))) %>%
  ungroup() %>%
  filter(acc_rate > .5)

check <- temp %>%
  group_by(study, group, participant, acc_rate) %>%
  summarise() %>%
  ungroup() %>%
  group_by(study, group) %>%
  summarise(n = n()) %>%
  ungroup()

# temp <- temp %>%
#   filter(acc_rate < .95)

temp <- temp %>%
  group_by(study, group, participant, item, dependency, environment, acc_rate, accuracy) %>%
  summarise() %>%
  ungroup() %>%
  mutate(dependency = fct_drop(dependency),
         environment = fct_drop(environment),
         accuracy = as.logical(accuracy),
         participant = as.factor(participant),
         item = as.factor(item)) %>%
  mutate(environment = fct_relevel(environment, 'short', 'long', 'island'))

md <- temp %>% 
  filter(study == '210510_su', group == 'english')

plot <- md %>%
  mutate(accuracy = as.logical(accuracy)) %>%
  group_by(study, group, participant, item, dependency, environment, accuracy) %>%
  summarise() %>%
  ungroup() %>%
  group_by(study, group, dependency, environment) %>%
  summarise(mean = mean(accuracy, na.rm=T) * 100,
            sd = sd(accuracy, na.rm=T) * 100,
            n = n()) %>%
  mutate(se = sd / sqrt(n),
         ci = qt(1 - (0.05 / 2), n - 1) * se) %>%
  ungroup()

ggplot(data=plot, aes(x=environment, y=mean, group=dependency, col=dependency, shape=dependency)) +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.2, lwd=1, linetype=1) +
  geom_line(lwd = 1) +
  geom_point(size = 2) +
  theme_classic() +
  scale_x_discrete(name="environment", limits = c("short", "long", "island"), labels = c("short", "long", "island")) +
  scale_y_continuous(name="% accuracy", limits=c(50, 100)) +
  scale_colour_manual(name="dependency", values=c('#648fff', '#ffb000'), labels=c("gap", "resumption")) +
  scale_shape_manual(name="dependency", values=c(16, 15), labels=c("gap", "resumption")) +
  theme(text = element_text(size = 12)) +
  facet_wrap(~group)

# view contrasts
contrasts(md$dependency)
contrasts(md$environment)

# contrasts(md$dependency) <- contr.sdif(2)
# contrasts(md$environment) <- contr.treatment(3, base = 1)

model1 <- glmer(accuracy ~ environment*dependency + (1|participant) + (1|item), 
                data = md, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(model1)
beep(1)

# doen (everyone)
# no warnings
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                          3.46214    0.28318  12.226   <2e-16 ***
# environmentlong                     -0.20781    0.30020  -0.692   0.4888    
# environmentisland                   -0.14000    0.30434  -0.460   0.6455    
# dependencypronoun                    0.77305    0.37414   2.066   0.0388 *  
# environmentlong:dependencypronoun    0.19967    0.52196   0.383   0.7021    
# environmentisland:dependencypronoun  0.05358    0.51838   0.103   0.9177 
# doen (acc_rate < 1)
# no warnings
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                          2.51807    0.25247   9.974   <2e-16 ***
#   environmentlong                     -0.21181    0.30015  -0.706   0.4804    
# environmentisland                   -0.13535    0.30463  -0.444   0.6568    
# dependencypronoun                    0.77184    0.37329   2.068   0.0387 *  
#   environmentlong:dependencypronoun    0.20276    0.52067   0.389   0.6970    
# environmentisland:dependencypronoun  0.03881    0.51805   0.075   0.9403 

# doko (everyone)
# no warnings
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                          1.50940    0.19893   7.587 3.26e-14 ***
#   environmentlong                     -0.17875    0.19643  -0.910    0.363    
# environmentisland                   -0.01253    0.19907  -0.063    0.950    
# dependencypronoun                    1.44112    0.25887   5.567 2.59e-08 ***
#   environmentlong:dependencypronoun   -0.03271    0.35364  -0.092    0.926    
# environmentisland:dependencypronoun -0.06833    0.35920  -0.190    0.849 
# doko (acc_rate < 1)
# no warnings
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                          1.22304    0.18372   6.657 2.79e-11 ***
#   environmentlong                     -0.17436    0.19817  -0.880    0.379    
# environmentisland                   -0.01094    0.20046  -0.055    0.956    
# dependencypronoun                    1.43280    0.25967   5.518 3.43e-08 ***
#   environmentlong:dependencypronoun   -0.03210    0.35520  -0.090    0.928    
# environmentisland:dependencypronoun -0.06782    0.36026  -0.188    0.851 

# dozh (everyone)
# no warnings
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                          2.98831    0.30673   9.743   <2e-16 ***
#   environmentlong                     -0.21689    0.28529  -0.760   0.4471    
# environmentisland                   -0.42952    0.27638  -1.554   0.1202    
# dependencypronoun                    0.79967    0.33360   2.397   0.0165 *  
#   environmentlong:dependencypronoun   -0.03142    0.45960  -0.068   0.9455    
# environmentisland:dependencypronoun  0.57751    0.47465   1.217   0.2237 
# dozh (acc_rate < 1)
# no warnings
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                           2.2013     0.2622   8.396   <2e-16 ***
#   environmentlong                      -0.2060     0.2756  -0.748   0.4547    
# environmentisland                    -0.4135     0.2675  -1.546   0.1222    
# dependencypronoun                     0.7930     0.3219   2.464   0.0138 *  
#   environmentlong:dependencypronoun    -0.0326     0.4436  -0.073   0.9414    
# environmentisland:dependencypronoun   0.5557     0.4584   1.212   0.2254  

# suen
# no warnings
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                           2.8663     0.2852  10.050  < 2e-16 ***
#   environmentlong                      -0.3428     0.2942  -1.165  0.24397    
# environmentisland                    -1.0521     0.2735  -3.846  0.00012 ***
#   dependencypronoun                     0.5596     0.3500   1.599  0.10989    
# environmentlong:dependencypronoun     0.1055     0.4708   0.224  0.82267    
# environmentisland:dependencypronoun   0.9465     0.4658   2.032  0.04216 * 

# suko
# no warnings
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                           3.0116     0.2838  10.612  < 2e-16 ***
#   environmentlong                      -1.1114     0.2642  -4.207 2.59e-05 ***
#   environmentisland                    -1.3356     0.2628  -5.082 3.73e-07 ***
#   dependencypronoun                     0.7606     0.3624   2.099   0.0358 *  
#   environmentlong:dependencypronoun     0.5178     0.4566   1.134   0.2568    
# environmentisland:dependencypronoun   0.5981     0.4507   1.327   0.1844

# suzh
# no warnings
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                           2.9099     0.2600  11.192  < 2e-16 ***
#   environmentlong                      -1.1145     0.2500  -4.459 8.24e-06 ***
#   environmentisland                    -0.9474     0.2533  -3.741 0.000183 ***
#   dependencypronoun                     0.3864     0.3075   1.257 0.208876    
# environmentlong:dependencypronoun     1.2225     0.4172   2.930 0.003389 ** 
#   environmentisland:dependencypronoun   0.5130     0.3961   1.295 0.195309  

model2 <- glmer(accuracy ~ environment*dependency + (environment+dependency|participant) + (environment+dependency|item), 
                 data = md, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(model2)
beep(1)

# doen (everyone)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                          3.75637    0.42457   8.847   <2e-16 ***
#   environmentlong                     -0.15519    0.53299  -0.291    0.771    
# environmentisland                    0.20504    0.59951   0.342    0.732    
# dependencypronoun                    0.26287    0.50284   0.523    0.601    
# environmentlong:dependencypronoun    0.03442    0.61201   0.056    0.955    
# environmentisland:dependencypronoun -0.17084    0.70430  -0.243    0.808 
# doen (acc_rate < 1)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                          2.67421    0.35534   7.526 5.24e-14 ***
#   environmentlong                     -0.10618    0.47168  -0.225    0.822    
# environmentisland                    0.15677    0.52545   0.298    0.765    
# dependencypronoun                    0.67129    0.48530   1.383    0.167    
# environmentlong:dependencypronoun   -0.02378    0.63366  -0.038    0.970    
# environmentisland:dependencypronoun -0.33612    0.68063  -0.494    0.621 

# doko (everyone)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                          1.506560   0.209931   7.176 7.15e-13 ***
#   environmentlong                     -0.048274   0.254663  -0.190    0.850    
# environmentisland                   -0.009459   0.236098  -0.040    0.968    
# dependencypronoun                    1.605217   0.343882   4.668 3.04e-06 ***
#   environmentlong:dependencypronoun   -0.215912   0.402479  -0.536    0.592    
# environmentisland:dependencypronoun -0.088500   0.402628  -0.220    0.826
# doko (acc_rate < 1)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                          1.218329   0.187289   6.505 7.77e-11 ***
#   environmentlong                     -0.083037   0.241180  -0.344    0.731    
# environmentisland                   -0.007022   0.222817  -0.032    0.975    
# dependencypronoun                    1.630619   0.335038   4.867 1.13e-06 ***
#   environmentlong:dependencypronoun   -0.217186   0.401518  -0.541    0.589    
# environmentisland:dependencypronoun -0.088992   0.401828  -0.221    0.825    

# dozh (everyone)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                           3.7050     0.5492   6.746 1.52e-11 ***
#   environmentlong                      -0.4084     0.5945  -0.687   0.4922    
# environmentisland                    -1.2438     0.5980  -2.080   0.0375 *  
#   dependencypronoun                     0.5180     0.5123   1.011   0.3120    
# environmentlong:dependencypronoun     0.1261     0.5756   0.219   0.8265    
# environmentisland:dependencypronoun   0.7946     0.6427   1.236   0.2164 
# dozh (acc_rate < 1)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                           2.6971     0.4400   6.129 8.84e-10 ***
#   environmentlong                      -0.3820     0.4783  -0.799   0.4245    
# environmentisland                    -0.9541     0.5078  -1.879   0.0602 .  
# dependencypronoun                     0.7009     0.4602   1.523   0.1277    
# environmentlong:dependencypronoun     0.0685     0.5591   0.123   0.9025    
# environmentisland:dependencypronoun   0.7187     0.6277   1.145   0.2522
# dozh (acc_rate < .95)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                           1.6545     0.3897   4.246 2.18e-05 ***
#   environmentlong                      -0.1186     0.4757  -0.249    0.803    
# environmentisland                    -0.4621     0.5060  -0.913    0.361    
# dependencypronoun                     1.0653     0.4614   2.309    0.021 *  
#   environmentlong:dependencypronoun    -0.1642     0.5920  -0.277    0.782    
# environmentisland:dependencypronoun   0.6359     0.6754   0.942    0.346 

# suen
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                          2.73582    0.32084   8.527   <2e-16 ***
#   environmentlong                     -0.06544    0.42040  -0.156   0.8763    
# environmentisland                   -0.95245    0.37220  -2.559   0.0105 *  
#   dependencypronoun                    0.92021    0.50184   1.834   0.0667 .  
# environmentlong:dependencypronoun    0.07872    0.56548   0.139   0.8893    
# environmentisland:dependencypronoun  0.99479    0.56413   1.763   0.0778 .

# suko
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                           3.1201     0.3884   8.034 9.45e-16 ***
#   environmentlong                      -0.9997     0.4338  -2.304 0.021213 *  
#   environmentisland                    -1.4318     0.3997  -3.582 0.000341 ***
#   dependencypronoun                     1.0071     0.5273   1.910 0.056124 .  
# environmentlong:dependencypronoun     0.1397     0.5711   0.245 0.806775    
# environmentisland:dependencypronoun   0.3615     0.5373   0.673 0.501063 

# suzh
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                           3.3426     0.4341   7.700 1.36e-14 ***
#   environmentlong                      -1.4830     0.4627  -3.205  0.00135 ** 
#   environmentisland                    -1.2545     0.4447  -2.821  0.00479 ** 
#   dependencypronoun                     0.5528     0.4418   1.251  0.21082    
# environmentlong:dependencypronoun     0.8905     0.5113   1.742  0.08158 .  
# environmentisland:dependencypronoun   0.3846     0.4852   0.793  0.42797  

model3 <- glmer(accuracy ~ environment*dependency + (environment*dependency|participant) + (environment*dependency|item), 
                data = md, family = binomial, control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6)))
summary(model3)
beep(1)

# doen
# boundary (singular) fit: see help('isSingular')
# maxfun < 10 * length(par)^2 is not recommended.
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                          3.96845    0.58768   6.753 1.45e-11 ***
#   environmentlong                     -0.50071    0.70818  -0.707    0.480    
# environmentisland                    0.58987    1.13255   0.521    0.602    
# dependencypronoun                    0.05405    0.81267   0.067    0.947    
# environmentlong:dependencypronoun    1.63585    1.45330   1.126    0.260    
# environmentisland:dependencypronoun  0.16351    1.54086   0.106    0.915

# doko
# boundary (singular) fit: see help('isSingular')
# maxfun < 10 * length(par)^2 is not recommended.
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                           1.5996     0.2552   6.267 3.69e-10 ***
#   environmentlong                      -0.1140     0.3174  -0.359 0.719498    
# environmentisland                     0.0617     0.3634   0.170 0.865192    
# dependencypronoun                     1.8992     0.5766   3.294 0.000989 ***
#   environmentlong:dependencypronoun    -0.4151     0.7032  -0.590 0.554963    
# environmentisland:dependencypronoun  -0.7602     0.7267  -1.046 0.295531 

# dozh (everyone)
# boundary (singular) fit: see help('isSingular')
# maxfun < 10 * length(par)^2 is not recommended.
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                          4.002192   0.707125   5.660 1.52e-08 ***
#   environmentlong                     -0.836077   0.761871  -1.097   0.2725    
# environmentisland                   -1.474838   0.771649  -1.911   0.0560 .  
# dependencypronoun                   -0.003471   0.857323  -0.004   0.9968    
# environmentlong:dependencypronoun    1.453131   1.210031   1.201   0.2298    
# environmentisland:dependencypronoun  2.194013   1.311622   1.673   0.0944 .
# dozh (acc_rate < 1, maxfun = 1e6)
# boundary (singular) fit: see help('isSingular')
# Number of obs: 1410, groups:  participant, 47; item, 30
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                           2.8684     0.5287   5.425 5.79e-08 ***
#   environmentlong                      -0.6137     0.5661  -1.084   0.2783    
# environmentisland                    -1.1182     0.5889  -1.899   0.0576 .  
# dependencypronoun                     0.4032     0.6746   0.598   0.5501    
# environmentlong:dependencypronoun     0.9940     0.9821   1.012   0.3115    
# environmentisland:dependencypronoun   1.9849     1.1120   1.785   0.0743 . 
# dozh (acc_rate < .95, maxfun = 1e6)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                           1.7229     0.4281   4.025 5.71e-05 ***
#   environmentlong                      -0.1540     0.5106  -0.302    0.763    
# environmentisland                    -0.5575     0.5381  -1.036    0.300    
# dependencypronoun                     0.9105     0.5699   1.598    0.110    
# environmentlong:dependencypronoun     0.3500     0.9061   0.386    0.699    
# environmentisland:dependencypronoun   2.2973     1.6549   1.388    0.165 

# suen
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                           2.8235     0.3614   7.813 5.59e-15 ***
#   environmentlong                      -0.2804     0.4742  -0.591    0.554    
# environmentisland                    -0.9157     0.4414  -2.074    0.038 *  
#   dependencypronoun                     0.8108     0.6343   1.278    0.201    
# environmentlong:dependencypronoun     0.5541     0.9406   0.589    0.556    
# environmentisland:dependencypronoun   1.0232     0.9361   1.093    0.274  

# suko
# boundary (singular) fit: see help('isSingular')
# maxfun < 10 * length(par)^2 is not recommended.
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                           3.4143     0.5180   6.591 4.37e-11 ***
#   environmentlong                      -1.0371     0.6121  -1.694  0.09024 .  
# environmentisland                    -1.7849     0.5329  -3.349  0.00081 ***
#   dependencypronoun                     1.8994     1.4512   1.309  0.19060    
# environmentlong:dependencypronoun    -0.6714     1.6663  -0.403  0.68700    
# environmentisland:dependencypronoun  -0.0762     1.5447  -0.049  0.96065 

# suzh
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                           3.1621     0.4346   7.276 3.44e-13 ***
#   environmentlong                      -1.2895     0.4762  -2.708  0.00677 ** 
#   environmentisland                    -0.9121     0.5069  -1.799  0.07198 .  
# dependencypronoun                     1.1386     0.8190   1.390  0.16443    
# environmentlong:dependencypronoun     0.7630     1.0768   0.709  0.47856    
# environmentisland:dependencypronoun  -0.1817     1.0033  -0.181  0.85633 

anova(model1, model2)

# doen
#         npar    AIC     BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model1    8 897.79  945.08 -440.89   881.79                     
# model2   26 921.01 1074.72 -434.50   869.01 12.781 18     0.8044

# doko
#        npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model1    8 1537.2 1582.0 -760.63   1521.2                     
# model2   26 1564.9 1710.3 -756.47   1512.9 8.3159 18     0.9736

# dozh
#        npar    AIC    BIC  logLik deviance Chisq Df Pr(>Chisq)
# model1    8 1000.0 1044.9 -492.00   984.01                    
# model2   26 1014.5 1160.2 -481.23   962.46 21.55 18     0.2526

# suen
#        npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model1    8 1025.6 1069.7 -504.79   1009.6                     
# model2   26 1055.5 1198.8 -501.75   1003.5 6.0827 18     0.9959

# suko
#        npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model1    8 1153.7 1198.0 -568.83   1137.7                     
# model2   26 1181.9 1326.1 -564.97   1129.9 7.7169 18     0.9826

# suzh
#        npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model1    8 1260.2 1305.2 -622.08   1244.2                     
# model2   26 1280.3 1426.8 -614.15   1228.3 15.844 18     0.6034

anova(model2, model3)

# doen
#        npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model2   26 921.01 1074.7 -434.50   869.01                     
# model3   48 958.14 1241.9 -431.07   862.14 6.8703 22     0.9991

# doko
#        npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model2   26 1564.9 1710.3 -756.47   1512.9                     
# model3   48 1592.1 1860.4 -748.03   1496.1 16.879 22       0.77

# dozh
#        npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model2   26 1014.5 1160.2 -481.23   962.46                     
# model3   48 1047.8 1316.9 -475.92   951.84 10.616 22     0.9798

# suen
#        npar    AIC    BIC  logLik deviance Chisq Df Pr(>Chisq)
# model2   26 1055.5 1198.8 -501.75  1003.50                    
# model3   48 1093.6 1358.2 -498.79   997.57 5.925 22     0.9997

# suko
#        npar    AIC    BIC  logLik deviance Chisq Df Pr(>Chisq)
# model2   26 1181.9 1326.1 -564.97   1129.9                    
# model3   48 1205.2 1471.4 -554.62   1109.2  20.7 22     0.5394

# suzh
#        npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model2   26 1280.3 1426.8 -614.15   1228.3                     
# model3   48 1311.2 1581.7 -607.62   1215.2 13.062 22     0.9314

anova(model1, model3)

# doen
#        npar    AIC     BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model1    8 897.79  945.08 -440.89   881.79                     
# model3   48 958.14 1241.92 -431.07   862.14 19.651 40     0.9971

# doko
#        npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model1    8 1537.2 1582.0 -760.63   1521.2                     
# model3   48 1592.1 1860.4 -748.03   1496.1 25.195 40     0.9673

# dozh
#        npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model1    8 1000.0 1044.9 -492.00   984.01                     
# model3   48 1047.8 1316.9 -475.92   951.84 32.166 40     0.8064

# suen
#        npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model1    8 1025.6 1069.7 -504.79  1009.58                     
# model3   48 1093.6 1358.2 -498.79   997.57 12.008 40          1

# suko
#        npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model1    8 1153.7 1198.0 -568.83   1137.7                     
# model3   48 1205.2 1471.4 -554.62   1109.2 28.416 40     0.9147

# suzh
#        npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model1    8 1260.2 1305.2 -622.08   1244.2                     
# model3   48 1311.2 1581.7 -607.62   1215.2 28.906 40     0.9035

# https://bookdown.org/ndphillips/YaRrr/comparing-regression-models-with-anova.html

pairs(emmeans(model1, "dependency", by = "environment"))
beep(1)

# dozh
# environment = short:
#   contrast      estimate    SE  df z.ratio p.value
# gap - pronoun   -0.793 0.322 Inf  -2.464  0.0138
# environment = long:
#   contrast      estimate    SE  df z.ratio p.value
# gap - pronoun   -0.760 0.305 Inf  -2.492  0.0127
# environment = island:
#   contrast      estimate    SE  df z.ratio p.value
# gap - pronoun   -1.349 0.329 Inf  -4.100  <.0001

# suzh
# environment = short:
#   contrast      estimate    SE  df z.ratio p.value
# gap - pronoun   -0.386 0.307 Inf  -1.257  0.2089
# environment = long:
#   contrast      estimate    SE  df z.ratio p.value
# gap - pronoun   -1.609 0.285 Inf  -5.646  <.0001
# environment = island:
#   contrast      estimate    SE  df z.ratio p.value
# gap - pronoun   -0.899 0.250 Inf  -3.593  0.0003

pairs(emmeans(model2, "dependency", by = "environment"))
beep(1)

# dozh
# environment = short:
#   contrast      estimate    SE  df z.ratio p.value
# gap - pronoun   -0.701 0.460 Inf  -1.523  0.1277
# environment = long:
#   contrast      estimate    SE  df z.ratio p.value
# gap - pronoun   -0.769 0.415 Inf  -1.853  0.0638
# environment = island:
#   contrast      estimate    SE  df z.ratio p.value
# gap - pronoun   -1.420 0.405 Inf  -3.506  0.0005

pairs(emmeans(model3, "dependency", by = "environment"))
beep(1)

# dozh
# environment = short:
# contrast      estimate    SE  df z.ratio p.value
# gap - pronoun   -0.403 0.675 Inf  -0.598  0.5501
# environment = long:
#   contrast      estimate    SE  df z.ratio p.value
# gap - pronoun   -1.397 0.714 Inf  -1.956  0.0505
# environment = island:
#   contrast      estimate    SE  df z.ratio p.value
# gap - pronoun   -2.388 0.877 Inf  -2.722  0.0065

allFit(model3) # compare optimizers

#------------------------------------------------------------------------------#
#### spr: scatter plot of proficiency effects for accuracy data ####
#------------------------------------------------------------------------------#

# trim based on accuracy
trim <- ds %>%
  group_by(study, group, participant) %>%
  mutate(acc_rate = mean(as.logical(accuracy))) %>%
  ungroup() %>%
  filter(acc_rate >.5)

plot <- trim %>%
  group_by(study, group, participant, acc_rate) %>%
  summarise() %>%
  ungroup()

plot <- plot %>%
  left_join(proficiency, by = c('study', 'group', 'participant')) %>%
  filter(proficiency != 'NA')

# facet labels

groups <- c(`english` = 'ENS', `korean` = 'KLE', `mandarin` = 'MLE')

# define data for plots

p1 <- ggplot(data=filter(plot, study == '210510_do'), aes(x=proficiency*100, y=acc_rate*100))
p2 <- ggplot(data=filter(plot, study == '210510_su'), aes(x=proficiency*100, y=acc_rate*100))

# generate plot

s <- list(
  geom_smooth(method=lm, col="#785ef0"), 
  geom_point(shape = 1),
  theme_classic(),
  scale_x_continuous(name='proficiency'),
  scale_y_continuous(name="% accuracy", limits = c(-5, 100)),
  scale_fill_manual(name='group', values=c("#9b82f3", "#00a78f")),
  theme(text = element_text(size = 12),
        plot.title = element_text(size = 12, hjust = .5), 
        legend.position = "right"),
  facet_wrap(~group, labeller = as_labeller(groups))
)

# write plots

p1 + s
ggsave("plots/orc/spr_accuracy_proficiency_effect.png", width=6.5, height=3, dpi=600)

p2 + s
ggsave("plots/src/spr_accuracy_proficiency_effect.png", width=6.5, height=3, dpi=600)

md <- plot %>% 
  filter(study == '210510_do', group == 'mandarin')
cor(md$proficiency, md$acc_rate) 
model <- lm(proficiency ~ acc_rate, data = md)
summary(model)

# doen
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   0.2326     0.1280   1.817   0.0727 .  
# acc_rate      0.6504     0.1335   4.872 4.98e-06 ***

# doko
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   0.1328     0.1201   1.105 0.273121    
# acc_rate      0.5456     0.1405   3.884 0.000246 ***

# dozh
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)   
# (Intercept)  0.04817    0.16075   0.300  0.76538   
# acc_rate     0.59116    0.17370   3.403  0.00114 **

# ENS -> b = 0.65, t = 4.87, p < .001
# KLE -> b = 0.55, t = 3.88, p < .001
# MLE -> b = 0.59, t = 3.40, p = .001

#------------------------------------------------------------------------------#
#### ajt: preprocessing ####
#------------------------------------------------------------------------------#

# create dataframe for ajt data

ajt <- df %>%
  filter(task %in% c('english_ajt', 'korean_ajt', 'mandarin_ajt')) %>%
  arrange(study, group, participant) %>%
  select_if(function(x){!all(is.na(x))})

# check 'NA' responses from skipped trials

check <- ajt %>%
  filter(!response %in% c(1, 2, 3, 4, 5, 6))

# remove 'NA' responses

ajt <- ajt %>%
  filter(is.na(response) == FALSE)

# add binary accept/reject column

ajt <- ajt %>%
  mutate(acceptance = case_when(response > 3.5 ~ TRUE,
                                response < 3.5 ~ FALSE))

# calculate z-scores for each task by participant on ratings for all trials

ajt <- ajt %>%
  mutate(response = as.numeric(response)) %>%
  group_by(study, group, task, participant) %>%
  mutate(zscore = (response - mean(response, na.rm=T)) / sd(response, na.rm = T)) %>%
  ungroup()

check <- ajt %>%
  mutate(condition = as.factor(condition))

summary(check$condition)

# calculate filler accuracy by participant

temp <- ajt %>%
  filter(condition %in% c('grammatical', 'ungrammatical')) %>%
  mutate(accuracy = as.logical(accuracy)) %>%
  group_by(study, group, task, participant) %>%
  summarise(acc_rate = mean(accuracy, na.rm = T)) %>%
  ungroup()

# inspect accuracy rates

ggplot(temp, aes(x=group, y=acc_rate, fill=group, label=participant)) + 
  geom_hline(yintercept=.5) +
  geom_violin() +
  geom_boxplot(width = .1, fill='white') +
  theme_classic() +
  scale_x_discrete(name="group", 
                   limits = c('english', 'korean', 'mandarin'),
                   labels = c('ENS', 'KLE', 'MLE')) +
  scale_y_continuous(name="accuracy rate", 
                     limits=c(0, 1)) +
  theme(text = element_text(size = 12), 
        plot.title = element_text(size = 12, hjust = .5), 
        legend.position = "hide") +
  facet_wrap(~task)

# trim based on accuracy rate

ajt <- ajt %>%
  left_join(temp, by = c('study', 'group', 'task', 'participant')) %>%
  filter(acc_rate > .5)

# inspect accuracy rates by participant

plot <- ajt %>%
  group_by(group, task, participant, acc_rate) %>%
  summarise() %>%
  ungroup()

# inspect accuracy rates

ggplot(plot, aes(x=group, y=acc_rate, fill=group, label=participant)) + 
  geom_hline(yintercept=.5) +
  geom_violin() +
  geom_boxplot(width = .1, fill='white') +
  theme_classic() +
  scale_x_discrete(name="group", 
                   limits = c('english', 'korean', 'mandarin'),
                   labels = c('ENS', 'KLE', 'MLE')) +
  scale_y_continuous(name="accuracy rate", 
                     limits=c(0, 1)) +
  theme(text = element_text(size = 12), 
        plot.title = element_text(size = 12, hjust = .5), 
        legend.position = "hide") +
  facet_wrap(~task)

# check number of participants

check <- ajt %>%
  group_by(study, task, group, participant) %>%
  summarise() %>%
  ungroup() %>%
  group_by(study, task, group) %>%
  summarise(n = n()) %>%
  ungroup()

#------------------------------------------------------------------------------#
#### ajt: interaction plots - critical trials - z-scores ####
#------------------------------------------------------------------------------#

# filter to critical trials

crit <- ajt %>% 
  filter(!condition %in% c('grammatical', 'ungrammatical'))

# summarize for plotting

plot <- crit %>%
  group_by(study, group, task, dependency, environment) %>%
  summarise(mean = mean(zscore, na.rm = T),
            sd = sd(zscore, na.rm = T),
            n = n()) %>%
  mutate(se = sd / sqrt(n),
         ci = qt(1 - (0.05 / 2), n - 1) * se) %>%
  ungroup() %>%
  filter(dependency != 'NA') %>%
  mutate(panel = case_when(group == 'english' & task == 'english_ajt' ~ 'ENS on English AJT',
                           group == 'korean' & task == 'english_ajt' ~ 'KLE on English AJT',
                           group == 'korean' & task == 'korean_ajt' ~ 'KLE on Korean AJT',
                           group == 'mandarin' & task == 'english_ajt' ~ 'MLE on English AJT',
                           group == 'mandarin' & task == 'mandarin_ajt' ~ 'MLE on Mandarin AJT')) %>%
  mutate(panel = factor(panel, levels = c('ENS on English AJT', 
                                          'KLE on English AJT', 
                                          'MLE on English AJT', 
                                          'KLE on Korean AJT', 
                                          'MLE on Mandarin AJT')))

# define data

p1 <- ggplot(data=filter(plot, study == '210510_do', group == 'english' & task == 'english_ajt'), 
             aes(x=environment, y=mean, group=dependency, col=dependency, shape=dependency))
p2 <- ggplot(data=filter(plot, study == '210510_do', group == 'korean' & task == 'english_ajt'), 
             aes(x=environment, y=mean, group=dependency, col=dependency, shape=dependency))
p3 <- ggplot(data=filter(plot, study == '210510_do', group == 'mandarin' & task == 'english_ajt'), 
             aes(x=environment, y=mean, group=dependency, col=dependency, shape=dependency))
p4 <- ggplot(data=filter(plot, study == '210510_do', group == 'korean' & task == 'korean_ajt'), 
             aes(x=environment, y=mean, group=dependency, col=dependency, shape=dependency))
p5 <- ggplot(data=filter(plot, study == '210510_do', group == 'mandarin' & task == 'mandarin_ajt'), 
             aes(x=environment, y=mean, group=dependency, col=dependency, shape=dependency))

p6 <- ggplot(data=filter(plot, study == '210510_su', group == 'english' & task == 'english_ajt'), 
             aes(x=environment, y=mean, group=dependency, col=dependency, shape=dependency))
p7 <- ggplot(data=filter(plot, study == '210510_su', group == 'korean' & task == 'english_ajt'), 
             aes(x=environment, y=mean, group=dependency, col=dependency, shape=dependency))
p8 <- ggplot(data=filter(plot, study == '210510_su', group == 'mandarin' & task == 'english_ajt'), 
             aes(x=environment, y=mean, group=dependency, col=dependency, shape=dependency))
p9 <- ggplot(data=filter(plot, study == '210510_su', group == 'korean' & task == 'korean_ajt'), 
             aes(x=environment, y=mean, group=dependency, col=dependency, shape=dependency))
p10 <- ggplot(data=filter(plot, study == '210510_su', group == 'mandarin' & task == 'mandarin_ajt'), 
              aes(x=environment, y=mean, group=dependency, col=dependency, shape=dependency))

# add styling

s <- list(
  annotate("rect", 
           xmin = 0, xmax = 4, 
           ymin = -1.1+(2.2/3), 
           ymax = -1.1+(2.2/3)+(2.2/3), 
           alpha = .15),
  geom_hline(yintercept=0),
  geom_line(lwd = 1),
  geom_point(size = 2),
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), 
                width=.2, lwd=1, linetype=1),
  theme_classic(),
  scale_x_discrete(name='environment', 
                   limits = c('short', 'long', 'island'), 
                   labels = c('short', 'long', 'island')),
  scale_y_continuous(name='mean z-score', 
                     limits=c(-1.5, 1.5)),
  scale_colour_manual(name='dependency', 
                      values=c('#648fff', '#ffb000'), 
                      labels=c('gap', 'resumption')),
  scale_shape_manual(name='dependency', 
                     values=c(16, 15), 
                     labels=c('gap', 'resumption')),
  theme(text = element_text(size = 12),
        plot.title = element_text(size = 12, hjust = .5)),
  facet_wrap(~panel)
)

# print and save

p1 + s + theme(legend.position = "none",
               axis.title.x = element_blank(), 
               plot.margin = margin(0, 0, -2, 0, "cm")) + 
p2 + s + theme(legend.position = "none", 
               axis.title.x = element_blank(), 
               axis.title.y = element_blank(), 
               plot.margin = margin(0, 0, -2, 0, "cm")) + 
p3 + s + theme(legend.position = "none", 
               axis.title.x = element_blank(), 
               axis.title.y = element_blank(), 
               plot.margin = margin(0, 0, -2, 0, "cm")) + 
patchwork::plot_spacer() + 
p4 + s + theme(legend.position = c(-.85, .65), 
               axis.title.y = element_blank()) + 
p5 + s + theme(legend.position = "none", 
               axis.title.x = element_blank(), 
               axis.title.y = element_blank())

ggsave("plots/orc/ajt_crit_zscore.png", width=6.5, height=3.5, dpi=600)

p6 + s + theme(legend.position = "none", 
               axis.title.x = element_blank(), 
               plot.margin = margin(0, 0, -2, 0, "cm")) + 
p7 + s + theme(legend.position = "none", 
               axis.title.x = element_blank(), 
               axis.title.y = element_blank(), 
               plot.margin = margin(0, 0, -2, 0, "cm")) + 
p8 + s + theme(legend.position = "none", 
               axis.title.x = element_blank(), 
               axis.title.y = element_blank(), 
               plot.margin = margin(0, 0, -2, 0, "cm")) + 
patchwork::plot_spacer() + 
p9 + s + theme(legend.position = c(-.85, .65),
               axis.title.y = element_blank()) + 
p10 + s + theme(legend.position = "none", 
                axis.title.x = element_blank(), 
                axis.title.y = element_blank())

ggsave("plots/src/ajt_crit_zscore.png", width=6.5, height=3.5, dpi=600)

#------------------------------------------------------------------------------#
#### ajt: interaction plots - critical trials - raw ratings ####
#------------------------------------------------------------------------------#

# filter to critical trials

crit <- ajt %>% 
  filter(!condition %in% c('grammatical', 'ungrammatical'))

# summarize for plotting

plot <- crit %>%
  group_by(study, group, task, dependency, environment) %>%
  summarise(mean = mean(response, na.rm = T),
            sd = sd(response, na.rm = T),
            n = n()) %>%
  mutate(se = sd / sqrt(n),
         ci = qt(1 - (0.05 / 2), n - 1) * se) %>%
  ungroup() %>%
  filter(dependency != 'NA') %>%
  mutate(panel = case_when(group == 'english' & task == 'english_ajt' ~ 'ENS on English AJT',
                           group == 'korean' & task == 'english_ajt' ~ 'KLE on English AJT',
                           group == 'korean' & task == 'korean_ajt' ~ 'KLE on Korean AJT',
                           group == 'mandarin' & task == 'english_ajt' ~ 'MLE on English AJT',
                           group == 'mandarin' & task == 'mandarin_ajt' ~ 'MLE on Mandarin AJT')) %>%
  mutate(panel = factor(panel, levels = c('ENS on English AJT', 
                                          'KLE on English AJT', 
                                          'MLE on English AJT', 
                                          'KLE on Korean AJT', 
                                          'MLE on Mandarin AJT')))

# define data

p1 <- ggplot(data=filter(plot, study == '210510_do', group == 'english' & task == 'english_ajt'), 
             aes(x=environment, y=mean, group=dependency, col=dependency, shape=dependency))
p2 <- ggplot(data=filter(plot, study == '210510_do', group == 'korean' & task == 'english_ajt'), 
             aes(x=environment, y=mean, group=dependency, col=dependency, shape=dependency))
p3 <- ggplot(data=filter(plot, study == '210510_do', group == 'mandarin' & task == 'english_ajt'), 
             aes(x=environment, y=mean, group=dependency, col=dependency, shape=dependency))
p4 <- ggplot(data=filter(plot, study == '210510_do', group == 'korean' & task == 'korean_ajt'), 
             aes(x=environment, y=mean, group=dependency, col=dependency, shape=dependency))
p5 <- ggplot(data=filter(plot, study == '210510_do', group == 'mandarin' & task == 'mandarin_ajt'), 
             aes(x=environment, y=mean, group=dependency, col=dependency, shape=dependency))

p6 <- ggplot(data=filter(plot, study == '210510_su', group == 'english' & task == 'english_ajt'), 
             aes(x=environment, y=mean, group=dependency, col=dependency, shape=dependency))
p7 <- ggplot(data=filter(plot, study == '210510_su', group == 'korean' & task == 'english_ajt'), 
             aes(x=environment, y=mean, group=dependency, col=dependency, shape=dependency))
p8 <- ggplot(data=filter(plot, study == '210510_su', group == 'mandarin' & task == 'english_ajt'), 
             aes(x=environment, y=mean, group=dependency, col=dependency, shape=dependency))
p9 <- ggplot(data=filter(plot, study == '210510_su', group == 'korean' & task == 'korean_ajt'), 
             aes(x=environment, y=mean, group=dependency, col=dependency, shape=dependency))
p10 <- ggplot(data=filter(plot, study == '210510_su', group == 'mandarin' & task == 'mandarin_ajt'), 
              aes(x=environment, y=mean, group=dependency, col=dependency, shape=dependency))

# add styling

s <- list(
  annotate("rect", 
           xmin = 0, xmax = 4, 
           ymin = 1+(5/3), 
           ymax = 1+(5/3)+(5/3), 
           alpha = .15),
  geom_hline(yintercept = 3.5),
  geom_line(lwd = 1),
  geom_point(size = 2),
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), 
                width=.2, lwd=1, linetype=1),
  theme_classic(),
  scale_x_discrete(name='environment', 
                   limits = c('short', 'long', 'island'), 
                   labels = c('short', 'long', 'island')),
  scale_y_continuous(name='rating', 
                     limits=c(1, 6), breaks = c(1, 2, 3, 4, 5, 6)),
  scale_colour_manual(name='dependency', 
                      values=c('#648fff', '#ffb000'), 
                      labels=c('gap', 'resumption')),
  scale_shape_manual(name='dependency', 
                     values=c(16, 15), 
                     labels=c('gap', 'resumption')),
  theme(text = element_text(size = 12),
        plot.title = element_text(size = 12, hjust = .5)),
  facet_wrap(~panel)
)

# print and save

p1 + s + theme(legend.position = "none",
               axis.title.x = element_blank(), 
               plot.margin = margin(0, 0, -2, 0, "cm")) + 
  p2 + s + theme(legend.position="none", 
                 axis.title.x = element_blank(), 
                 axis.title.y = element_blank(), 
                 plot.margin = margin(0, 0, -2, 0, "cm")) + 
  p3 + s + theme(legend.position = "none", 
                 axis.title.x = element_blank(), 
                 axis.title.y = element_blank(), 
                 plot.margin = margin(0, 0, -2, 0, "cm")) + 
  patchwork::plot_spacer() + 
  p4 + s + theme(legend.position = c(-.7, .65), 
                 axis.title.y = element_blank()) + 
  p5 + s + theme(legend.position = "none", 
                 axis.title.x = element_blank(), 
                 axis.title.y = element_blank())

# bucld plot

p <- ggplot(data=filter(plot, study == '210510_do'), 
             aes(x=environment, y=mean, group=dependency, col=dependency, shape=dependency))

p + s + facet_wrap(~panel, ncol = 5) +
  labs(caption = 'glmer: acceptance ~ dependency * environment + (1 + dependency * environment | person) + (1 + dependency * environment | item)', hjust = .5) +
  theme(plot.caption = element_text(hjust = .5))

ggsave("plots/orc/ajt_crit_rating.png", width=6.5, height=3.5, dpi=600)

ggsave("plots/orc/bucld_ajt.png", width=9.3, height=2.25, dpi=600)

bucld1 + bucld2 +  plot_layout(widths = c(1, 1)) +
  theme(plot.margin = margin(0, 0, 0, 0, "cm"))

ggsave("plots/orc/bucld.png", width=9.3, height=2.5, dpi=600)

p6 + s + theme(legend.position = "none", 
               axis.title.x = element_blank(), 
               plot.margin = margin(0, 0, -2, 0, "cm")) + 
  p7 + s + theme(legend.position = "none", 
                 axis.title.x = element_blank(), 
                 axis.title.y = element_blank(), 
                 plot.margin = margin(0, 0, -2, 0, "cm")) + 
  p8 + s + theme(legend.position = "none", 
                 axis.title.x = element_blank(), 
                 axis.title.y = element_blank(), 
                 plot.margin = margin(0, 0, -2, 0, "cm")) + 
  patchwork::plot_spacer() + 
  p9 + s + theme(legend.position = c(-.7, .65), 
                 axis.title.y = element_blank()) + 
  p10 + s + theme(legend.position = "none", 
                  axis.title.x = element_blank(), 
                  axis.title.y = element_blank())

ggsave("plots/src/ajt_crit_rating.png", width=6.5, height=3.5, dpi=600)

#------------------------------------------------------------------------------#
#### ajt: interaction plots - critical trials - acceptance rates ####
#------------------------------------------------------------------------------#

# filter to critical trials

crit <- ajt %>% 
  filter(!condition %in% c('grammatical', 'ungrammatical'))

# summarize for plotting

plot <- crit %>%
  mutate(acceptance = case_when(response > 3.5 ~ TRUE,
                                response < 3.5 ~ FALSE)) %>%
  group_by(study, group, task, dependency, environment) %>%
  summarise(mean = mean(acceptance, na.rm = T) * 100,
            sd = sd(acceptance, na.rm = T) * 100,
            n = n()) %>%
  mutate(se = sd / sqrt(n),
         ci = qt(1 - (0.05 / 2), n - 1) * se) %>%
  ungroup() %>%
  filter(dependency != 'NA') %>%
  mutate(panel = case_when(group == 'english' & task == 'english_ajt' ~ 'ENS on English AJT',
                           group == 'korean' & task == 'english_ajt' ~ 'KLE on English AJT',
                           group == 'korean' & task == 'korean_ajt' ~ 'KLE on Korean AJT',
                           group == 'mandarin' & task == 'english_ajt' ~ 'MLE on English AJT',
                           group == 'mandarin' & task == 'mandarin_ajt' ~ 'MLE on Mandarin AJT')) %>%
  mutate(panel = factor(panel, levels = c('ENS on English AJT', 
                                          'KLE on English AJT', 
                                          'MLE on English AJT', 
                                          'KLE on Korean AJT', 
                                          'MLE on Mandarin AJT')))

# define data

p1 <- ggplot(data=filter(plot, study == '210510_do', group == 'english' & task == 'english_ajt'), 
             aes(x=environment, y=mean, group=dependency, col=dependency, shape=dependency))
p2 <- ggplot(data=filter(plot, study == '210510_do', group == 'korean' & task == 'english_ajt'), 
             aes(x=environment, y=mean, group=dependency, col=dependency, shape=dependency))
p3 <- ggplot(data=filter(plot, study == '210510_do', group == 'mandarin' & task == 'english_ajt'), 
             aes(x=environment, y=mean, group=dependency, col=dependency, shape=dependency))
p4 <- ggplot(data=filter(plot, study == '210510_do', group == 'korean' & task == 'korean_ajt'), 
             aes(x=environment, y=mean, group=dependency, col=dependency, shape=dependency))
p5 <- ggplot(data=filter(plot, study == '210510_do', group == 'mandarin' & task == 'mandarin_ajt'), 
             aes(x=environment, y=mean, group=dependency, col=dependency, shape=dependency))

p6 <- ggplot(data=filter(plot, study == '210510_su', group == 'english' & task == 'english_ajt'), 
             aes(x=environment, y=mean, group=dependency, col=dependency, shape=dependency))
p7 <- ggplot(data=filter(plot, study == '210510_su', group == 'korean' & task == 'english_ajt'), 
             aes(x=environment, y=mean, group=dependency, col=dependency, shape=dependency))
p8 <- ggplot(data=filter(plot, study == '210510_su', group == 'mandarin' & task == 'english_ajt'), 
             aes(x=environment, y=mean, group=dependency, col=dependency, shape=dependency))
p9 <- ggplot(data=filter(plot, study == '210510_su', group == 'korean' & task == 'korean_ajt'), 
             aes(x=environment, y=mean, group=dependency, col=dependency, shape=dependency))
p10 <- ggplot(data=filter(plot, study == '210510_su', group == 'mandarin' & task == 'mandarin_ajt'), 
              aes(x=environment, y=mean, group=dependency, col=dependency, shape=dependency))

# add styling

s <- list(
  annotate("rect", 
           xmin = 0, xmax = 4, 
           ymin = 0+(100/3), 
           ymax = 0+(100/3)+(100/3), 
           alpha = .15),
  geom_hline(yintercept = 50),
  geom_line(lwd = 1),
  geom_point(size = 2),
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), 
                width=.2, lwd=1, linetype=1),
  theme_classic(),
  scale_x_discrete(name='environment', 
                   limits = c('short', 'long', 'island'), 
                   labels = c('short', 'long', 'island')),
  scale_y_continuous(name='% acceptance', 
                     limits=c(0, 100)),
  scale_colour_manual(name='dependency', 
                      values=c('#648fff', '#ffb000'), 
                      labels=c('gap', 'resumption')),
  scale_shape_manual(name='dependency', 
                     values=c(16, 15), 
                     labels=c('gap', 'resumption')),
  theme(text = element_text(size = 12),
        plot.title = element_text(size = 12, hjust = .5)),
  facet_wrap(~panel)
)

# print and save

p1 + s + theme(legend.position = "none",
               axis.title.x = element_blank(), 
               plot.margin = margin(0, 0, -2, 0, "cm")) + 
  p2 + s + theme(legend.position="none", 
                 axis.title.x = element_blank(), 
                 axis.title.y = element_blank(), 
                 plot.margin = margin(0, 0, -2, 0, "cm")) + 
  p3 + s + theme(legend.position = "none", 
                 axis.title.x = element_blank(), 
                 axis.title.y = element_blank(), 
                 plot.margin = margin(0, 0, -2, 0, "cm")) + 
  patchwork::plot_spacer() + 
  p4 + s + theme(legend.position = c(-.8, .65), 
                 axis.title.y = element_blank()) + 
  p5 + s + theme(legend.position = "none", 
                 axis.title.x = element_blank(), 
                 axis.title.y = element_blank())

ggsave("plots/orc/ajt_crit_acceptance.png", width=6.5, height=3.5, dpi=600)

p6 + s + theme(legend.position = "none", 
               axis.title.x = element_blank(), 
               plot.margin = margin(0, 0, -2, 0, "cm")) + 
  p7 + s + theme(legend.position = "none", 
                 axis.title.x = element_blank(), 
                 axis.title.y = element_blank(), 
                 plot.margin = margin(0, 0, -2, 0, "cm")) + 
  p8 + s + theme(legend.position = "none", 
                 axis.title.x = element_blank(), 
                 axis.title.y = element_blank(), 
                 plot.margin = margin(0, 0, -2, 0, "cm")) + 
  patchwork::plot_spacer() + 
  p9 + s + theme(legend.position = c(-.8, .65), 
                 axis.title.y = element_blank()) + 
  p10 + s + theme(legend.position = "none", 
                  axis.title.x = element_blank(), 
                  axis.title.y = element_blank())

ggsave("plots/src/ajt_crit_acceptance.png", width=6.5, height=3.5, dpi=600)

#------------------------------------------------------------------------------#
#### ajt: scatter plots by item and by participant ####
#------------------------------------------------------------------------------#

# summarise for plotting by item

plot <- ajt %>%
  mutate(item = str_remove(item, 'item')) %>%
  mutate(dependency = as.character(dependency)) %>%
  mutate(dependency = fct_relevel(dependency, 'gap', 'pronoun', 'filler')) %>%
  mutate(condition = as.factor(condition),
         dependency = as.factor(dependency)) %>%
  group_by(study, item, environment, dependency, condition) %>%
  summarise(mean = mean(response, na.rm = T)) %>%
  ungroup()

# generate and save

ggplot(plot, aes(x=condition, y=mean, fill=dependency, label=item)) +
  geom_hline(yintercept=3.5) +
  geom_violin() +
  geom_boxplot(width = .1, fill='white') +
  #geom_text(size = 2.5, col = "black", position = position_jitter(seed=2)) +
  theme_classic() +
  scale_x_discrete(name="environment", labels=c('short', 'long', 'island', 'short', 'long', 'island', 'gram', 'ungram')) +
  scale_y_continuous(name="rating", limits=c(1, 6), breaks=c(1, 2, 3, 4, 5, 6)) +
  scale_fill_manual(name='dependency', values=c('#648fff', '#ffb000', 'coral2'), labels=c('gap', 'RP', 'filler')) +
  theme(text = element_text(size = 12), 
        plot.title = element_text(size = 12, hjust = .5), 
        legend.position = "right") +
  facet_wrap(~study)

ggsave("plots/ajt_scatter_rating_item.png", width=8, height=4, dpi=600)

# summarise data for plotting by participant (raw ratings)

plot <- ajt %>%
  mutate(dependency = as.character(dependency)) %>%
  mutate(dependency = fct_relevel(dependency, 'gap', 'pronoun', 'filler')) %>%
  mutate(condition = as.factor(condition),
         dependency = as.factor(dependency)) %>%
  group_by(study, group, task, participant, environment, dependency, condition) %>%
  summarise(mean = mean(response, na.rm = T)) %>%
  ungroup()

plot <- plot %>%
  filter(study == '210510_do', group == 'english', task == 'english_ajt')

# generate and save

ggplot(plot, aes(x=condition, y=mean, fill=dependency, label=participant)) +
  geom_hline(yintercept=3.5) +
  geom_violin() +
  geom_boxplot(width = .1, fill='white') +
  #geom_jitter(shape = 1, size = 1, position = position_jitter(seed = 2, width = .2)) +
  #geom_text(size = 1, col = "black", position = position_jitter(seed=2)) +
  theme_classic() +
  scale_x_discrete(name="environment", labels=c('short', 'long', 'island', 'short', 'long', 'island', 'gram', 'ungram')) +
  scale_y_continuous(name="mean rating", limits=c(1, 6), breaks=c(1, 2, 3, 4, 5, 6)) +
  scale_fill_manual(name='dependency', values=c("#9b82f3", "#00a78f", 'coral2'), labels=c('gap', 'RP', 'filler')) +
  theme(text = element_text(size = 12), 
        plot.title = element_text(size = 12, hjust = .5), 
        legend.position = "right") +
  facet_grid(group~study~task)

ggsave("plots/ajt_scatter_rating_ppt.png", width=8, height=8, dpi=600)

# summarise data for plotting by participant (percent acceptance)

plot <- ajt %>%
  mutate(dependency = as.character(dependency)) %>%
  mutate(dependency = fct_relevel(dependency, 'gap', 'pronoun', 'filler')) %>%
  mutate(condition = as.factor(condition),
         dependency = as.factor(dependency)) %>%
  group_by(study, group, task, participant, environment, dependency, condition) %>%
  summarise(mean = mean(acceptance, na.rm = T)) %>%
  ungroup()

plot <- plot %>%
  filter(study == '210510_do', group == 'mandarin', task == 'mandarin_ajt')

# generate and save

ggplot(plot, aes(x=condition, y=mean, fill=dependency, label=participant)) +
  geom_hline(yintercept=.5) +
  geom_violin() +
  #geom_boxplot(width = .1, fill='white') +
  geom_jitter(shape = 1, size = 1, position = position_jitter(seed = 2, width = .2)) +
  #geom_text(size = 1, col = "black", position = position_jitter(seed=2)) +
  theme_classic() +
  scale_x_discrete(name="environment", labels=c('short', 'long', 'island', 'short', 'long', 'island', 'gram', 'ungram')) +
  scale_y_continuous(name="mean rating", limits=c(0, 1)) +
  scale_fill_manual(name='dependency', values=c("#9b82f3", "#00a78f", 'coral2'), labels=c('gap', 'RP', 'filler')) +
  theme(text = element_text(size = 12), 
        plot.title = element_text(size = 12, hjust = .5), 
        legend.position = "right") +
  facet_grid(group~study~task)

ggsave("plots/ajt_scatter_acceptance_ppt.png", width=8, height=8, dpi=600)

#------------------------------------------------------------------------------#
#### ajt: relationship between spr and ajt data (all ORC environments) ####
#------------------------------------------------------------------------------#

rt_score <- trim %>%
  mutate(region = as.numeric(region)) %>%
  mutate(region2 = case_when(study == '210510_do' ~ region - 11,
                             study == '210510_su' & environment %in% c('short', 'long') ~ region - 8,
                             study == '210510_su' & environment == 'island' ~ region - 10))

rt_score <- rt_score %>%
  filter(region2 %in% c('1', '2', '3')) %>%
  mutate(logrt = log(rt)) %>%
  group_by(study, group, participant, item, dependency) %>%
  summarise(logrt = mean(logrt)) %>%
  ungroup()

rt_score <- rt_score %>%
  filter(study == '210510_do')

rt_score <- rt_score %>%
  group_by(group, participant) %>%
  summarise(mean_gap = mean(logrt[dependency == 'gap'], na.rm = TRUE),
         mean_rp = mean(logrt[dependency == 'pronoun'], na.rm = TRUE)) %>%
  ungroup()

rt_score <- rt_score %>%
  mutate(rt_dif = mean_gap - mean_rp)

rt_score <- rt_score %>%
  select(-mean_gap, -mean_rp)

ggplot(rt_score, aes(x=group, y=rt_dif, fill=group, label=participant)) +
  geom_hline(yintercept = 0) +
  geom_violin() +
  geom_boxplot(width = .1, fill='white') +
  theme_classic()

ajt_score <- crit %>%
  filter(study == '210510_do',
         task == 'english_ajt',
         dependency == 'pronoun') %>%
  group_by(group, participant) %>%
  summarise(ajt_score = mean(response, na.rm=T)) %>%
  ungroup()

ggplot(ajt_score, aes(x=group, y=ajt_score, fill=group, label=participant)) +
  geom_hline(yintercept = 3.5) +
  geom_violin() +
  geom_boxplot(width = .1, fill='white') +
  theme_classic()

plot <- rt_score %>%
  left_join(ajt_score, by = c('group', 'participant')) %>%
  filter(is.na(rt_dif) == FALSE, is.na(ajt_score) == FALSE) %>%
  mutate(panel = case_when(group == 'english' ~ 'ENS on English AJT',
                           group == 'korean' ~ 'KLE on English AJT',
                           group == 'mandarin' ~ 'MLE on English AJT'))

ggplot(plot, aes(x=ajt_score, y=rt_dif)) + 
  ggtitle('SPRT results vs. AJT results (all ORC environments)') +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 3.5) +
  geom_smooth(method=lm, col="#785ef0") +
  geom_point(shape = 1) +
  theme_classic() +
  scale_x_continuous(name='acceptability of RP trials (raw ratings)', breaks = c(1, 2, 3, 4, 5, 6)) +
  scale_y_continuous(name="RP advantage (logRTs)", limits = c(-1, 1), breaks = c(-1, -.5, 0, .5, 1)) +
  theme(text = element_text(size = 12),
        plot.title = element_text(hjust = .5),
        legend.position = "right") +
  facet_grid(~panel)

# save plot
ggsave("plots/orc/spr_v_ajt_all.png", width=6.5, height=2.5, dpi=600)

md <- plot %>% 
  filter(group == "mandarin")
cor(md$ajt_score, md$rt_dif) 
model <- lm(ajt_score ~ rt_dif, data = md)
summary(model)

# doen
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   2.1256     0.1103  19.270   <2e-16 ***
#   rt_dif        1.5534     1.1129   1.396    0.166   

# doko
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  2.88075    0.22077  13.048   <2e-16 ***
#   rt_dif       0.02937    1.22560   0.024    0.981  

# dozh
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   3.5133     0.2072  16.952   <2e-16 ***
#   rt_dif        1.0006     1.1542   0.867    0.389  

#------------------------------------------------------------------------------#
#### ajt: relationship between spr and ajt data (ORC island environment) ####
#------------------------------------------------------------------------------#

rt_score <- trim %>%
  mutate(region = as.numeric(region)) %>%
  mutate(region2 = case_when(study == '210510_do' ~ region - 11,
                             study == '210510_su' & environment %in% c('short', 'long') ~ region - 8,
                             study == '210510_su' & environment == 'island' ~ region - 10))

rt_score <- rt_score %>%
  filter(environment == 'island') %>%
  filter(region2 %in% c('1', '2', '3')) %>%
  mutate(logrt = log(rt)) %>%
  group_by(study, group, participant, item, dependency) %>%
  summarise(logrt = mean(logrt)) %>%
  ungroup()

rt_score <- rt_score %>%
  filter(study == '210510_do')

rt_score <- rt_score %>%
  group_by(group, participant) %>%
  summarise(mean_gap = mean(logrt[dependency == 'gap'], na.rm = TRUE),
            mean_rp = mean(logrt[dependency == 'pronoun'], na.rm = TRUE)) %>%
  ungroup()

rt_score <- rt_score %>%
  mutate(rt_dif = mean_gap - mean_rp)

rt_score <- rt_score %>%
  select(-mean_gap, -mean_rp)

ggplot(rt_score, aes(x=group, y=rt_dif, fill=group, label=participant)) +
  geom_hline(yintercept = 0) +
  geom_violin() +
  geom_boxplot(width = .1, fill='white') +
  theme_classic()

ajt_score <- crit %>%
  filter(study == '210510_do',
         environment == 'island',
         task == 'english_ajt',
         dependency == 'pronoun') %>%
  group_by(group, participant) %>%
  summarise(ajt_score = mean(response, na.rm=T)) %>%
  ungroup()

ggplot(ajt_score, aes(x=group, y=ajt_score, fill=group, label=participant)) +
  geom_hline(yintercept = 3.5) +
  geom_violin() +
  geom_boxplot(width = .1, fill='white') +
  theme_classic()

plot <- rt_score %>%
  left_join(ajt_score, by = c('group', 'participant')) %>%
  filter(is.na(rt_dif) == FALSE, is.na(ajt_score) == FALSE) %>%
  mutate(panel = case_when(group == 'english' ~ 'ENS on English AJT',
                           group == 'korean' ~ 'KLE on English AJT',
                           group == 'mandarin' ~ 'MLE on English AJT'))

ggplot(plot, aes(x=ajt_score, y=rt_dif)) + 
  ggtitle('SPRT results vs. AJT results (ORC island environment)') +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 3.5) +
  geom_smooth(method=lm, col="#785ef0") +
  geom_point(shape = 1) +
  theme_classic() +
  scale_x_continuous(name='acceptability of RP trials (raw ratings)', breaks = c(1, 2, 3, 4, 5, 6)) +
  scale_y_continuous(name="RP advantage (logRTs)", limits = c(-1, 1), breaks = c(-1, -.5, 0, .5, 1)) +
  theme(text = element_text(size = 12),
        plot.title = element_text(hjust = .5),
        legend.position = "right") +
  facet_grid(~panel)

# save plot
ggsave("plots/orc/spr_v_ajt_island.png", width=6.5, height=2.5, dpi=600)

md <- plot %>% 
  filter(group == "mandarin")
cor(md$ajt_score, md$rt_dif) 
model <- lm(ajt_score ~ rt_dif, data = md)
summary(model)

# doen
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  2.50494    0.17846   14.04   <2e-16 ***
#   rt_dif      -0.09108    0.91173   -0.10    0.921  

# doko
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   3.5260     0.2303  15.309   <2e-16 ***
#   rt_dif       -0.3132     0.9174  -0.341    0.734 

# dozh
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   3.6452     0.2155  16.914   <2e-16 ***
#   rt_dif        0.2330     1.0023   0.232    0.817  

#------------------------------------------------------------------------------#
#### ajt: relationship between spr and ajt data (ORC long environment) ####
#------------------------------------------------------------------------------#

rt_score <- trim %>%
  mutate(region = as.numeric(region)) %>%
  mutate(region2 = case_when(study == '210510_do' ~ region - 11,
                             study == '210510_su' & environment %in% c('short', 'long') ~ region - 8,
                             study == '210510_su' & environment == 'island' ~ region - 10))

rt_score <- rt_score %>%
  filter(environment == 'long') %>%
  filter(region2 %in% c('1', '2', '3')) %>%
  mutate(logrt = log(rt)) %>%
  group_by(study, group, participant, item, dependency) %>%
  summarise(logrt = mean(logrt)) %>%
  ungroup()

rt_score <- rt_score %>%
  filter(study == '210510_do')

rt_score <- rt_score %>%
  group_by(group, participant) %>%
  summarise(mean_gap = mean(logrt[dependency == 'gap'], na.rm = TRUE),
            mean_rp = mean(logrt[dependency == 'pronoun'], na.rm = TRUE)) %>%
  ungroup()

rt_score <- rt_score %>%
  mutate(rt_dif = mean_gap - mean_rp)

rt_score <- rt_score %>%
  select(-mean_gap, -mean_rp)

ggplot(rt_score, aes(x=group, y=rt_dif, fill=group, label=participant)) +
  geom_hline(yintercept = 0) +
  geom_violin() +
  geom_boxplot(width = .1, fill='white') +
  theme_classic()

ajt_score <- crit %>%
  filter(study == '210510_do',
         environment == 'long',
         task == 'english_ajt',
         dependency == 'pronoun') %>%
  group_by(group, participant) %>%
  summarise(ajt_score = mean(response, na.rm=T)) %>%
  ungroup()

ggplot(ajt_score, aes(x=group, y=ajt_score, fill=group, label=participant)) +
  geom_hline(yintercept = 3.5) +
  geom_violin() +
  geom_boxplot(width = .1, fill='white') +
  theme_classic()

plot <- rt_score %>%
  left_join(ajt_score, by = c('group', 'participant')) %>%
  filter(is.na(rt_dif) == FALSE, is.na(ajt_score) == FALSE) %>%
  mutate(panel = case_when(group == 'english' ~ 'ENS on English AJT',
                           group == 'korean' ~ 'KLE on English AJT',
                           group == 'mandarin' ~ 'MLE on English AJT'))

ggplot(plot, aes(x=ajt_score, y=rt_dif)) + 
  ggtitle('SPRT results vs. AJT results (ORC long environment)') +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 3.5) +
  geom_smooth(method=lm, col="#785ef0") +
  geom_point(shape = 1) +
  theme_classic() +
  scale_x_continuous(name='acceptability of RP trials (raw ratings)', breaks = c(1, 2, 3, 4, 5, 6)) +
  scale_y_continuous(name="RP advantage (logRTs)", limits = c(-1, 1), breaks = c(-1, -.5, 0, .5, 1)) +
  theme(text = element_text(size = 12),
        plot.title = element_text(hjust = .5),
        legend.position = "right") +
  facet_grid(~panel)

# save plot
ggsave("plots/orc/spr_v_ajt_long.png", width=6.5, height=2.5, dpi=600)

md <- plot %>% 
  filter(group == "mandarin")
cor(md$ajt_score, md$rt_dif) 
model <- lm(ajt_score ~ rt_dif, data = md)
summary(model)

# doen
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   1.9884     0.1073  18.524  < 2e-16 ***
#   rt_dif        1.8966     0.6638   2.857  0.00533 ** 

# doko
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   2.6379     0.2426  10.871 5.35e-16 ***
#   rt_dif        0.3197     0.9560   0.334    0.739 

# dozh
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   3.5702     0.2328  15.337   <2e-16 ***
#   rt_dif       -0.4305     0.9193  -0.468    0.641  

#------------------------------------------------------------------------------#
#### ajt: relationship between spr and ajt data (ORC short environment) ####
#------------------------------------------------------------------------------#

rt_score <- trim %>%
  mutate(region = as.numeric(region)) %>%
  mutate(region2 = case_when(study == '210510_do' ~ region - 11,
                             study == '210510_su' & environment %in% c('short', 'long') ~ region - 8,
                             study == '210510_su' & environment == 'island' ~ region - 10))

rt_score <- rt_score %>%
  filter(environment == 'short') %>%
  filter(region2 %in% c('1', '2', '3')) %>%
  mutate(logrt = log(rt)) %>%
  group_by(study, group, participant, item, dependency) %>%
  summarise(logrt = mean(logrt)) %>%
  ungroup()

rt_score <- rt_score %>%
  filter(study == '210510_do')

rt_score <- rt_score %>%
  group_by(group, participant) %>%
  summarise(mean_gap = mean(logrt[dependency == 'gap'], na.rm = TRUE),
            mean_rp = mean(logrt[dependency == 'pronoun'], na.rm = TRUE)) %>%
  ungroup()

rt_score <- rt_score %>%
  mutate(rt_dif = mean_gap - mean_rp)

rt_score <- rt_score %>%
  select(-mean_gap, -mean_rp)

ggplot(rt_score, aes(x=group, y=rt_dif, fill=group, label=participant)) +
  geom_hline(yintercept = 0) +
  geom_violin() +
  geom_boxplot(width = .1, fill='white') +
  theme_classic()

ajt_score <- crit %>%
  filter(study == '210510_do',
         environment == 'short',
         task == 'english_ajt',
         dependency == 'pronoun') %>%
  group_by(group, participant) %>%
  summarise(ajt_score = mean(response, na.rm=T)) %>%
  ungroup()

ggplot(ajt_score, aes(x=group, y=ajt_score, fill=group, label=participant)) +
  geom_hline(yintercept = 3.5) +
  geom_violin() +
  geom_boxplot(width = .1, fill='white') +
  theme_classic()

plot <- rt_score %>%
  left_join(ajt_score, by = c('group', 'participant')) %>%
  filter(is.na(rt_dif) == FALSE, is.na(ajt_score) == FALSE) %>%
  mutate(panel = case_when(group == 'english' ~ 'ENS on English AJT',
                           group == 'korean' ~ 'KLE on English AJT',
                           group == 'mandarin' ~ 'MLE on English AJT'))

ggplot(plot, aes(x=ajt_score, y=rt_dif)) + 
  ggtitle('SPRT results vs. AJT results (ORC short environment)') +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 3.5) +
  geom_smooth(method=lm, col="#785ef0") +
  geom_point(shape = 1) +
  theme_classic() +
  scale_x_continuous(name='acceptability of RP trials (raw ratings)', breaks = c(1, 2, 3, 4, 5, 6)) +
  scale_y_continuous(name="RP advantage (logRTs)", limits = c(-1, 1), breaks = c(-1, -.5, 0, .5, 1)) +
  theme(text = element_text(size = 12),
        plot.title = element_text(hjust = .5),
        legend.position = "right") +
  facet_grid(~panel)

# save plot
ggsave("plots/orc/spr_v_ajt_short.png", width=6.5, height=2.5, dpi=600)

md <- plot %>% 
  filter(group == "mandarin")
cor(md$ajt_score, md$rt_dif) 
model <- lm(ajt_score ~ rt_dif, data = md)
summary(model)

# doen
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   2.0182     0.1125  17.935   <2e-16 ***
#   rt_dif        1.2662     0.6810   1.859   0.0663 .  

# doko
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   2.4165     0.2150   11.24   <2e-16 ***
#   rt_dif        0.6053     0.9164    0.66    0.511  

# dozh
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   3.4795     0.2197   15.84   <2e-16 ***
#   rt_dif        1.6109     1.0132    1.59    0.117 

#------------------------------------------------------------------------------#
#### ajt: proficiency effects - all environments - raw ratings ####
#------------------------------------------------------------------------------#

# summarize data for plotting

plot <- crit %>%
  filter(dependency == 'pronoun') %>%
  group_by(study, group, participant, task, dependency) %>%
  summarise(mean = mean(response, na.rm=T),
            sd = sd(response, na.rm=T),
            n = n()) %>%
  mutate(se = sd / sqrt(n),
         ci = qt(1 - (0.05 / 2), n - 1) * se) %>%
  ungroup() %>%
  mutate(panel = case_when(group == 'english' & task == 'english_ajt' ~ 'ENS on English AJT',
                           group == 'korean' & task == 'english_ajt' ~ 'KLE on English AJT',
                           group == 'korean' & task == 'korean_ajt' ~ 'KLE on Korean AJT',
                           group == 'mandarin' & task == 'english_ajt' ~ 'MLE on English AJT',
                           group == 'mandarin' & task == 'mandarin_ajt' ~ 'MLE on Mandarin AJT')) %>%
  mutate(panel = factor(panel, levels = c('ENS on English AJT', 
                                          'KLE on English AJT', 
                                          'MLE on English AJT', 
                                          'KLE on Korean AJT', 
                                          'MLE on Mandarin AJT'))) %>%
  filter(panel %in% c('KLE on English AJT', 'MLE on English AJT'))

score <- ct %>%
  group_by(participant) %>%
  summarise(ctest = mean(accuracy, na.rm=T)) %>%
  ungroup()

plot <- plot %>%
  left_join(score, by = 'participant') %>%
  filter(ctest != 'NA')

# generate and save

ggplot(plot, aes(x=ctest, y=mean)) + 
  geom_smooth(method=lm, col="#785ef0") +
  geom_point(shape = 1) +
  theme_classic() +
  scale_x_continuous(name='proficiency') +
  scale_y_continuous(name="mean rating", breaks = c(1, 2, 3, 4, 5, 6)) +
  theme(text = element_text(size = 12), 
        plot.title = element_text(size = 12, hjust = .5), 
        legend.position = "right") +
  facet_grid(study~panel)

ggsave("plots/ajt_scatter_proficiency_rating.png", width=6.5, height=3, dpi=600)

# simple regression analysis

md <- plot %>% 
  filter(study == '210510_su', group == "mandarin", task == 'english_ajt')
cor(md$ctest, md$mean) 
model <- lm(mean ~ ctest, data = md)
summary(model)

# doko
# Multiple R-squared:  0.05276,	Adjusted R-squared:  0.03818 
# F-statistic:  3.62 on 1 and 65 DF,  p-value: 0.06151 . (marginal)

# dozh
# Multiple R-squared:  0.04471,	Adjusted R-squared:  0.03125 
# F-statistic: 3.323 on 1 and 71 DF,  p-value: 0.07254 . (marginal)

# suko
# Multiple R-squared:  0.02743,	Adjusted R-squared:  0.01223 
# F-statistic: 1.805 on 1 and 64 DF,  p-value: 0.1839 (n.s.)

# suzh
# Multiple R-squared:  0.0004505,	Adjusted R-squared:  -0.01447 
# F-statistic: 0.0302 on 1 and 67 DF,  p-value: 0.8626 (n.s.)

#------------------------------------------------------------------------------#
#### ajt: proficiency effects - island environment - raw ratings ####
#------------------------------------------------------------------------------#

# summarize data for plotting

plot <- crit %>%
  filter(dependency == 'pronoun' & environment == 'island') %>%
  group_by(study, group, participant, task, dependency) %>%
  summarise(mean = mean(response, na.rm=T),
            sd = sd(response, na.rm=T),
            n = n()) %>%
  mutate(se = sd / sqrt(n),
         ci = qt(1 - (0.05 / 2), n - 1) * se) %>%
  ungroup() %>%
  mutate(panel = case_when(group == 'english' & task == 'english_ajt' ~ 'ENS on English AJT',
                           group == 'korean' & task == 'english_ajt' ~ 'KLE on English AJT',
                           group == 'korean' & task == 'korean_ajt' ~ 'KLE on Korean AJT',
                           group == 'mandarin' & task == 'english_ajt' ~ 'MLE on English AJT',
                           group == 'mandarin' & task == 'mandarin_ajt' ~ 'MLE on Mandarin AJT')) %>%
  mutate(panel = factor(panel, levels = c('ENS on English AJT', 
                                          'KLE on English AJT', 
                                          'MLE on English AJT', 
                                          'KLE on Korean AJT', 
                                          'MLE on Mandarin AJT'))) %>%
  filter(panel %in% c('KLE on English AJT', 'MLE on English AJT'))

score <- ct %>%
  group_by(participant) %>%
  summarise(ctest = mean(accuracy, na.rm=T)) %>%
  ungroup()

plot <- plot %>%
  left_join(score, by = 'participant') %>%
  filter(ctest != 'NA')

# generate plot
ggplot(plot, aes(x=ctest, y=mean)) + 
  geom_smooth(method=lm, col="#785ef0") +
  geom_point(shape = 1) +
  theme_classic() +
  scale_x_continuous(name='proficiency') +
  scale_y_continuous(name="mean rating", breaks = c(1, 2, 3, 4, 5, 6)) +
  theme(text = element_text(size = 12), 
        plot.title = element_text(size = 12, hjust = .5), 
        legend.position = "right") +
  facet_grid(study~panel)

ggsave("plots/ajt_scatter_proficiency_rating_long.png", width=4, height=4, dpi=600)

# simple regression analysis

md <- plot %>% 
  filter(study == '210510_do', group == "korean", task == 'english_ajt')
cor(md$ctest, md$mean) 
model <- lm(mean ~ ctest, data = md)
summary(model)

# doko
# Multiple R-squared:  0.02766,	Adjusted R-squared:  0.0127 
# F-statistic: 1.849 on 1 and 65 DF,  p-value: 0.1786 (n.s.)

# dozh
# Multiple R-squared:  0.06621,	Adjusted R-squared:  0.05306 
# F-statistic: 5.034 on 1 and 71 DF,  p-value: 0.02797 (significant)

# suko
# Multiple R-squared:  0.01532,	Adjusted R-squared:  -6.278e-05 
# F-statistic: 0.9959 on 1 and 64 DF,  p-value: 0.3221 (n.s.)

# suzh
# Multiple R-squared:  0.01298,	Adjusted R-squared:  -0.001749 
# F-statistic: 0.8813 on 1 and 67 DF,  p-value: 0.3512 (n.s.)

#------------------------------------------------------------------------------#
#### ajt: proficiency effects - island environment - binary ratings ####
#------------------------------------------------------------------------------#

# summarize data for plotting

plot <- crit %>%
  filter(group %in% c('korean', 'mandarin') & dependency == 'pronoun' & environment %in% c('short', 'long', 'island')) %>%
  group_by(study, group, participant, task, dependency) %>%
  mutate(acceptance = case_when(response < 3.5 ~ FALSE,
                                response > 3.5 ~ TRUE)) %>%
  summarise(mean = mean(acceptance, na.rm=T),
            sd = sd(response, na.rm=T),
            n = n()) %>%
  mutate(se = sd / sqrt(n),
         ci = qt(1 - (0.05 / 2), n - 1) * se) %>%
  ungroup() %>%
  mutate(panel = case_when(group == 'english' & task == 'english_ajt' ~ 'ENS on English AJT',
                           group == 'korean' & task == 'english_ajt' ~ 'KLE on English AJT',
                           group == 'korean' & task == 'korean_ajt' ~ 'KLE on Korean AJT',
                           group == 'mandarin' & task == 'english_ajt' ~ 'MLE on English AJT',
                           group == 'mandarin' & task == 'mandarin_ajt' ~ 'MLE on Mandarin AJT')) %>%
  mutate(panel = factor(panel, levels = c('ENS on English AJT', 
                                          'KLE on English AJT', 
                                          'MLE on English AJT', 
                                          'KLE on Korean AJT', 
                                          'MLE on Mandarin AJT'))) %>%
  filter(panel %in% c('KLE on English AJT', 'MLE on English AJT'))

score <- ct %>%
  group_by(participant) %>%
  summarise(ctest = mean(accuracy, na.rm=T)) %>%
  ungroup()

plot <- plot %>%
  left_join(score, by = 'participant') %>%
  filter(ctest != 'NA')

# generate plot
ggplot(plot, aes(x=ctest, y=mean)) + 
  geom_smooth(method=lm, col="#785ef0") +
  geom_point(shape = 1) +
  theme_classic() +
  scale_x_continuous(name='proficiency') +
  scale_y_continuous(name="mean rating") +
  theme(text = element_text(size = 12), 
        plot.title = element_text(size = 12, hjust = .5), 
        legend.position = "right") +
  facet_grid(study~panel)

ggsave("plots/ajt_scatter_proficiency_acceptance_all.png", width=4, height=4, dpi=600)

# simple regression analysis

md <- plot %>% 
  filter(study == '210510_do', group == "korean", task == 'english_ajt')
cor(md$ctest, md$mean) 
model <- lm(mean ~ ctest, data = md)
summary(model)

#------------------------------------------------------------------------------#
#### ajt: modeling - glmer - critical ####
#------------------------------------------------------------------------------#

# prep data

md <- crit %>%
  mutate(environment = fct_relevel(environment, 'short', 'long', 'island'),
         dependency = as.factor(dependency)) %>%
  mutate(acceptance = case_when(response > 3.5 ~ TRUE,
                                response < 3.5 ~ FALSE)) %>%
  filter(study == '210510_do', 
         group == 'mandarin',
         task == 'mandarin_ajt')

# view contrasts

contrasts(md$dependency)
contrasts(md$environment)

# model 1 (maximal model)

tic()
model1 <- glmer(acceptance ~ environment * dependency + 
                (1 + environment * dependency | participant) + 
                (1 + environment * dependency | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl=list(maxfun = 1e6)))
summary(model1)
toc()
beep(1)

# doen-ejt
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                           4.4571     0.6719   6.634 3.27e-11 ***
# environmentlong                      -0.5082     0.9037  -0.562    0.574    
# environmentisland                    -4.8227     0.7766  -6.210 5.31e-10 ***
# dependencypronoun                    -7.6782     0.8221  -9.340  < 2e-16 ***
# environmentlong:dependencypronoun     0.3287     1.0535   0.312    0.755    
# environmentisland:dependencypronoun   5.2213     0.9500   5.496 3.88e-08 ***

# doko-ejt
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                           2.9764     0.6280   4.739 2.15e-06 ***
#   environmentlong                      -1.3425     0.5440  -2.468   0.0136 *  
#   environmentisland                    -1.6203     0.6397  -2.533   0.0113 *  
#   dependencypronoun                    -4.9261     0.9046  -5.446 5.16e-08 ***
#   environmentlong:dependencypronoun     1.6596     0.7399   2.243   0.0249 *  
#   environmentisland:dependencypronoun   3.7220     0.8014   4.645 3.41e-06 ***

# doko-kjt
# Model failed to converge: degenerate  Hessian with 11 negative eigenvalues
# (see below for results from simplified model)

# dozh-ejt
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                           3.1582     0.5517   5.725 1.04e-08 ***
#   environmentlong                      -1.1486     0.5834  -1.969   0.0490 *  
#   environmentisland                    -1.3680     0.6388  -2.141   0.0322 *  
#   dependencypronoun                    -3.0913     0.7052  -4.383 1.17e-05 ***
#   environmentlong:dependencypronoun     1.1183     0.6163   1.814   0.0696 .  
# environmentisland:dependencypronoun   1.5071     0.6879   2.191   0.0285 * 

# dozh-zjt
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                           0.9123     0.3970   2.298 0.021557 *  
#   environmentlong                      -0.3938     0.3776  -1.043 0.297049    
# environmentisland                    -2.1612     0.3792  -5.699 1.21e-08 ***
#   dependencypronoun                    -2.3961     0.4080  -5.873 4.28e-09 ***
#   environmentlong:dependencypronoun     1.5354     0.4196   3.660 0.000253 ***
#   environmentisland:dependencypronoun   2.6887     0.4684   5.740 9.47e-09 ***

# model 2 (simplified model for when the maximal model does not converge)

tic()
model2 <- glmer(acceptance ~ environment * dependency + 
                  (1 + environment + dependency | participant) + 
                  (1 + environment + dependency | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl=list(maxfun = 1e6)))
summary(model2)
toc()
beep(1)

# dozh-ejt
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                           5.0245     0.6145   8.177 2.92e-16 ***
# environmentlong                      -4.0218     0.6540  -6.150 7.76e-10 ***
# environmentisland                    -4.5681     0.6432  -7.102 1.23e-12 ***
# dependencypronoun                    -7.3361     0.7318 -10.025  < 2e-16 ***
# environmentlong:dependencypronoun     4.9090     0.7376   6.655 2.83e-11 ***
# environmentisland:dependencypronoun   6.9606     0.7171   9.706  < 2e-16 ***

# dozh-zjt
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                           0.8427     0.3499   2.408 0.016025 *  
#   environmentlong                      -0.3218     0.3216  -1.000 0.317101    
# environmentisland                    -2.0913     0.3280  -6.376 1.81e-10 ***
#   dependencypronoun                    -2.0930     0.2874  -7.283 3.26e-13 ***
#   environmentlong:dependencypronoun     1.2474     0.3246   3.842 0.000122 ***
#   environmentisland:dependencypronoun   2.3292     0.3367   6.917 4.60e-12 ***

# post-hoc tests: pairwise comparisons of estimated marginal means

pairs(emmeans(model1, "dependency", by = "environment"))

# doen-ejt
# environment = short:
#   contrast      estimate    SE  df z.ratio p.value
# gap - pronoun     7.68 0.822 Inf   9.340  <.0001
# environment = long:
#   contrast      estimate    SE  df z.ratio p.value
# gap - pronoun     7.35 0.804 Inf   9.136  <.0001
# environment = island:
#   contrast      estimate    SE  df z.ratio p.value
# gap - pronoun     2.46 0.609 Inf   4.034  0.0001
# Results are given on the log odds ratio (not the response) scale.

# doko-ejt
# environment = short:
#   contrast      estimate    SE  df z.ratio p.value
# gap - pronoun     4.93 0.905 Inf   5.446  <.0001
# environment = long:
#   contrast      estimate    SE  df z.ratio p.value
# gap - pronoun     3.27 0.817 Inf   3.999  0.0001
# environment = island:
#   contrast      estimate    SE  df z.ratio p.value
# gap - pronoun     1.20 0.704 Inf   1.711  0.0871
# Results are given on the log odds ratio (not the response) scale. 

# doko-kjt
# (see below)

# dozh-ejt
# environment = short:
#   contrast      estimate    SE  df z.ratio p.value
# gap - pronoun     3.09 0.705 Inf   4.383  <.0001
# environment = long:
#   contrast      estimate    SE  df z.ratio p.value
# gap - pronoun     1.97 0.473 Inf   4.170  <.0001
# environment = island:
#   contrast      estimate    SE  df z.ratio p.value
# gap - pronoun     1.58 0.505 Inf   3.139  0.0017
# Results are given on the log odds ratio (not the response) scale. 

# dozh-zjt
# environment = short:
#   contrast      estimate    SE  df z.ratio p.value
# gap - pronoun    2.396 0.408 Inf   5.873  <.0001
# environment = long:
#   contrast      estimate    SE  df z.ratio p.value
# gap - pronoun    0.861 0.289 Inf   2.975  0.0029
# environment = island:
#   contrast      estimate    SE  df z.ratio p.value
# gap - pronoun   -0.293 0.301 Inf  -0.971  0.3317
# Results are given on the log odds ratio (not the response) scale. 

pairs(emmeans(model2, "dependency", by = "environment"))

# doko-kjt
# environment = short:
#   contrast      estimate    SE  df z.ratio p.value
# gap - pronoun    7.336 0.732 Inf  10.025  <.0001
# environment = long:
#   contrast      estimate    SE  df z.ratio p.value
# gap - pronoun    2.427 0.415 Inf   5.851  <.0001
# environment = island:
#   contrast      estimate    SE  df z.ratio p.value
# gap - pronoun    0.375 0.355 Inf   1.056  0.2908
# Results are given on the log odds ratio (not the response) scale. 

anova(model1, model2)

# dozh-ejt
# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)  
# model2   26 1635.9 1781.7 -791.95   1583.9                       
# model1   48 1648.1 1917.2 -776.04   1552.1 31.828 22    0.08037 .

# dozh-zjt
# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model2   26 2212.5 2361.2 -1080.3   2160.5                     
# model1   48 2235.2 2509.6 -1069.6   2139.2 21.349 22     0.4993

#------------------------------------------------------------------------------#
#### ajt: modeling - old ####
#------------------------------------------------------------------------------#

md <- crit %>%
  mutate(environment = fct_relevel(environment, 'short', 'long', 'island')) %>%
  filter(group == 'english',
         task == 'English AJT')

# check distribution
hist(md$zscore)
qqnorm(md$zscore)

# view contrasts
contrasts(md$dependency)
contrasts(md$environment)

# full model
model1 <- lmer(zscore ~ environment*dependency + (environment*dependency|participant) + (environment*dependency|item), data = md)
summary(model1)
# singular fit
# Fixed effects:
# Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                            0.85068    0.02250 1011.82261  37.810  < 2e-16 ***
# environmentlong                       -0.22160    0.04713   66.60669  -4.701 1.34e-05 ***
# environmentisland                     -1.20600    0.08514   71.86721 -14.165  < 2e-16 ***
# dependencypronoun                     -1.85627    0.04332   81.98937 -42.847  < 2e-16 ***
# environmentlong:dependencypronoun      0.21812    0.05586   63.35406   3.904 0.000232 ***
# environmentisland:dependencypronoun    1.42418    0.08093   65.39559  17.598  < 2e-16 ***

# *** 210324 korean group on english ajt ***
# singular fit
# Fixed effects:
#   Estimate Std. Error       df t value Pr(>|t|)    
# (Intercept)                          0.40649    0.08384 33.57062   4.848 2.78e-05 ***
#   environmentlong                     -0.41398    0.11255 35.26985  -3.678 0.000777 ***
#   environmentisland                   -0.71794    0.14137 31.91241  -5.079 1.59e-05 ***
#   dependencypronoun                   -1.15057    0.15695 33.77773  -7.331 1.78e-08 ***
#   environmentlong:dependencypronoun    0.54750    0.14980 31.13529   3.655 0.000939 ***
#   environmentisland:dependencypronoun  1.20599    0.19221 33.18410   6.274 4.21e-07 ***

# *** 210324 korean group on korean ajt ***
# singular fit
# Fixed effects:
#   Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)                           0.78158    0.04757 683.62113  16.429  < 2e-16 ***
#   environmentlong                      -0.73922    0.10210  50.10363  -7.240 2.50e-09 ***
#   environmentisland                    -0.95340    0.11390  47.27339  -8.371 6.85e-11 ***
#   dependencypronoun                    -1.59611    0.10628  44.15165 -15.018  < 2e-16 ***
#   environmentlong:dependencypronoun     0.95378    0.13556  36.31382   7.036 2.81e-08 ***
#   environmentisland:dependencypronoun   1.41796    0.17055  33.73098   8.314 1.12e-09 ***

# *** 210324 english group on english ajt ***
# singular fit
# Fixed effects:
#   Estimate Std. Error       df t value Pr(>|t|)    
# (Intercept)                          0.81679    0.04537 76.36099  18.004  < 2e-16 ***
#   environmentlong                     -0.38897    0.09243 34.47047  -4.208 0.000174 ***
#   environmentisland                   -0.70165    0.10951 35.57941  -6.407  2.1e-07 ***
#   dependencypronoun                   -1.57087    0.10410 35.61274 -15.091  < 2e-16 ***
#   environmentlong:dependencypronoun    0.42729    0.12638 33.87896   3.381 0.001833 ** 
#   environmentisland:dependencypronoun  1.03418    0.12815 32.98285   8.070  2.6e-09 ***

pr <- md %>%
  filter(dependency == 'pronoun')

model2 <- lmer(zscore ~ environment + (environment|participant) + (environment|item), data = pr)
summary(model2)
# singular fit
# Fixed effects:
# Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)       -1.005764   0.034279 61.894844 -29.341  < 2e-16 ***
# environmentlong   -0.003251   0.039736 38.805748  -0.082  0.93522    
# environmentisland  0.218585   0.071319 62.040170   3.065  0.00322 ** 

# *** 210324 korean group on english ajt ***
# singular fit
# Fixed effects:
#   Estimate Std. Error       df t value Pr(>|t|)    
# (Intercept)       -0.75038    0.11977 33.15911  -6.265 4.34e-07 ***
#   environmentlong    0.13993    0.09295 66.03889   1.505  0.13699    
# environmentisland  0.49245    0.14510 32.53437   3.394  0.00183 ** 

# *** 210324 korean group on korean ajt ***
# singular fit
# Fixed effects:
#   Estimate Std. Error       df t value Pr(>|t|)    
# (Intercept)       -0.81317    0.09432 34.30109  -8.622 4.18e-10 ***
#   environmentlong    0.21466    0.11562 32.00828   1.857  0.07260 .  
# environmentisland  0.46118    0.12490 26.44362   3.692  0.00102 ** 

# *** 210324 english group on english ajt ***
# singular fit
# Fixed effects:
#   Estimate Std. Error       df t value Pr(>|t|)    
# (Intercept)       -0.75358    0.07887 31.02200  -9.555 9.33e-11 ***
#   environmentlong    0.03585    0.08212 32.05004   0.437 0.665390    
# environmentisland  0.33151    0.07879 29.12423   4.208 0.000225 ***

is <- md %>%
  filter(environment == 'island')

model3 <- lmer(zscore ~ dependency + (dependency|participant) + (dependency|item), data = is)
summary(model3)
# fit not singular
# Fixed effects:
# Estimate Std. Error       df t value Pr(>|t|)    
# (Intercept)       -0.35564    0.08157 62.00954  -4.360 4.99e-05 ***
# dependencypronoun -0.43230    0.08137 60.91007  -5.313 1.61e-06 ***

# *** 210324 korean group on english ajt ***
# singular fit
# Fixed effects:
#   Estimate Std. Error       df t value Pr(>|t|)   
# (Intercept)       -0.31180    0.11339 31.86501   -2.75  0.00974 **
#   dependencypronoun  0.05158    0.17168 32.08280    0.30  0.76579  

# *** 210324 korean group on korean ajt ***
# singular fit
# Fixed effects:
#   Estimate Std. Error      df t value Pr(>|t|)  
# (Intercept)        -0.1739     0.1008 32.4518  -1.725   0.0941 .
# dependencypronoun  -0.1876     0.1458 35.6735  -1.287   0.2065  

# *** 210324 english group on english ajt ***
# singular fit
# Fixed effects:
#   Estimate Std. Error       df t value Pr(>|t|)    
# (Intercept)        0.11310    0.09792 33.29438   1.155 0.256325    
# dependencypronoun -0.53471    0.12655 31.91697  -4.225 0.000186 ***

ga <- md %>%
  filter(dependency == 'gap')

model4 <- lmer(zscore ~ environment + (environment|participant) + (environment|item), data = ga)
summary(model4)
# singular fit
# Fixed effects:
# Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)         0.85077    0.02260 173.72905  37.638  < 2e-16 ***
# environmentlong    -0.22409    0.05165  55.85300  -4.339 6.07e-05 ***
# environmentisland  -1.20636    0.09229  63.43687 -13.072  < 2e-16 ***

# *** 210324 korean group on english ajt ***
# singular fit
# Fixed effects:
#   Estimate Std. Error       df t value Pr(>|t|)    
# (Intercept)        0.40726    0.08185 31.44499   4.976 2.22e-05 ***
#   environmentlong   -0.41249    0.11205 31.49271  -3.681 0.000864 ***
#   environmentisland -0.71803    0.14122 31.97708  -5.084 1.56e-05 ***

# *** 210324 korean group on korean ajt ***
# singular fit
# Fixed effects:
#   Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)         0.78125    0.04083 332.20184  19.132  < 2e-16 ***
#   environmentlong    -0.73820    0.10027  42.92572  -7.362 3.90e-09 ***
#   environmentisland  -0.95627    0.11292  44.22831  -8.468 8.31e-11 ***

# *** 210324 english group on english ajt ***
# singular fit
# Fixed effects:
#   Estimate Std. Error       df t value Pr(>|t|)    
# (Intercept)        0.81646    0.03511 80.68459  23.252  < 2e-16 ***
#   environmentlong   -0.39160    0.09151 33.97185  -4.279 0.000145 ***
#   environmentisland -0.70217    0.10705 34.85836  -6.559 1.45e-07 ***

# source on different coding schemes
# https://marissabarlaz.github.io/portfolio/contrastcoding/

#------------------------------------------------------------------------------#
#### ajt: interaction plots for fillers ####
#------------------------------------------------------------------------------#

# filter to filler trials
fill <- ajt %>% filter(cond %in% c('grammatical', 'ungrammatical', 'gram', 'ungr')) %>%
  mutate(cond = case_when(cond == 'gram' ~ 'grammatical',
                          cond == 'ungr' ~ 'ungrammatical',
                          TRUE ~ cond))

# create 'set' factor
fill <- fill %>%
  mutate(set = case_when(item %in% c('item31', 'item32', 'item33', 'item34', 'item35', 'item36', 'item37') ~ 'set1',
                         item %in% c('item38', 'item39', 'item40', 'item41', 'item42', 'item43', 'item44') ~ 'set2',
                         item %in% c('item45', 'item46', 'item47', 'item48', 'item49', 'item50', 'item51') ~ 'set3',
                         item %in% c('item52', 'item53', 'item54', 'item55', 'item56', 'item57', 'item58') ~ 'set4',
                         item %in% c('item59', 'item60', 'item61', 'item62', 'item63', 'item64', 'item65') ~ 'set5',
                         item %in% c('item66', 'item67', 'item68', 'item69', 'item70', 'item71', 'item72') ~ 'set6'))

# create 'superset' factor
fill <- fill %>%
  mutate(superset = case_when(set %in% c('set1', 'set2') ~ 'superset1',
                              set %in% c('set3', 'set4') ~ 'superset2',
                              set == 'set5' ~ 'superset3',
                              set == 'set6' ~ 'superset4'))

# summarize data for plotting by group
plot <- fill %>%
  mutate(cond = fct_drop(cond),
         set = fct_drop(superset)) %>%
  group_by(group, task, superset, cond) %>%
  summarise(mean = mean(zscore, na.rm=T),
            sd = sd(zscore, na.rm=T),
            n = n()) %>%
  mutate(se = sd / sqrt(n),
         ci = qt(1 - (0.05 / 2), n - 1) * se) %>%
  ungroup() %>%
  filter(cond != 'NA') %>%
  mutate(panel = case_when(group == 'english' & task == 'English AJT' ~ 'ENS on English AJT',
                           group == 'korean' & task == 'English AJT' ~ 'KLE on English AJT',
                           group == 'korean' & task == 'Korean AJT' ~ 'KLE on Korean AJT',
                           group == 'mandarin' & task == 'English AJT' ~ 'MLE on English AJT',
                           group == 'mandarin' & task == 'Korean AJT' ~ 'MLE on Mandarin AJT'))

# add styling
s <- list(
  annotate("rect", xmin = 0, xmax = 5, ymin = -1.3+(2.6/3), ymax = -1.3+(2.6/3)+(2.6/3), alpha = .15),
  geom_hline(yintercept=0),
  geom_line(lwd = 1),
  geom_point(size = 2),
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.2, lwd=1, linetype=1),
  theme_classic(),
  scale_x_discrete(name="type", limits = c("superset1", "superset2", "superset3", "superset4"), labels = c("F1", "F2", "F3", "F4")),
  scale_y_continuous(name="mean z-score", limits=c(-1.3, 1.3), breaks=c(-1.5, -1, -.5, 0, .5, 1, 1.5)),
  scale_colour_manual(name="condition", values=c('#648fff', '#ffb000'), labels=c("grammatical", "ungrammatical")),
  scale_shape_manual(name="condition", values=c(16, 15), labels=c("grammatical", "ungrammatical")),
  theme(text = element_text(size = 12),
        plot.title = element_text(size = 12, hjust = .5)),
  facet_wrap(~panel)
)

# define data for panels
p1 <- ggplot(data=filter(plot, group == 'english' & task == 'English AJT'), aes(x=superset, y=mean, group=cond, col=cond, shape=cond))
p2 <- ggplot(data=filter(plot, group == 'korean' & task == 'English AJT'), aes(x=superset, y=mean, group=cond, col=cond, shape=cond))
p3 <- ggplot(data=filter(plot, group == 'mandarin' & task == 'English AJT'), aes(x=superset, y=mean, group=cond, col=cond, shape=cond))
p4 <- ggplot(data=filter(plot, group == 'korean' & task == 'Korean AJT'), aes(x=superset, y=mean, group=cond, col=cond, shape=cond))
p5 <- ggplot(data=filter(plot, group == 'mandarin' & task == 'Korean AJT'), aes(x=superset, y=mean, group=cond, col=cond, shape=cond))

# arrange and print
p1 + s + theme(legend.position="none", axis.title.x = element_blank(), plot.margin = margin(0, 0, -2, 0, "cm")) + 
  p2 + s + theme(legend.position="none", axis.title.x = element_blank(), axis.title.y = element_blank(), plot.margin = margin(0, 0, -2, 0, "cm")) + 
  p3 + s + theme(legend.position="none", axis.title.x = element_blank(), axis.title.y = element_blank(), plot.margin = margin(0, 0, -2, 0, "cm")) + 
  patchwork::plot_spacer() + 
  p4 + s + theme(legend.position = c(-.8, .65), axis.title.y = element_blank()) + 
  p5 + s + theme(legend.position="none", axis.title.x = element_blank(), axis.title.y = element_blank())

# save plot
ggsave("data/objects/plots/ajt_fillers_zscore.png", width=6.5, height=3.5, dpi=600)

# summarize data for plotting by participant
plot <- fill %>%
  mutate(cond = fct_drop(cond),
         set = fct_drop(superset)) %>%
  group_by(task, run_id, participant, superset, cond) %>%
  summarise(mean = mean(zscore, na.rm=T),
            sd = sd(zscore, na.rm=T),
            n = n()) %>%
  mutate(se = sd / sqrt(n),
         ci = qt(1 - (0.05 / 2), n - 1) * se) %>%
  ungroup() %>%
  filter(cond != 'NA')

# create plot faceted by participant
ggplot(data=plot, aes(x=superset, y=mean, group=cond, col=cond, shape=cond)) +
  geom_hline(yintercept=0) +
  geom_line(lwd = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.2, lwd=1, linetype=1) +
  theme_classic() +
  scale_x_discrete(name="set", limits = c("superset1", "superset2", "superset3", "superset4"), labels = c("set1", "set2", "set3", "set4")) +
  scale_y_continuous(name="mean z-score", limits=c(-4, 4)) +
  scale_colour_manual(name="condition", values=c('#648fff', '#ffb000'), labels=c("grammatical", "ungrammatical")) +
  scale_shape_manual(name="condition", values=c(16, 15), labels=c("grammatical", "ungrammatical")) +
  theme(text = element_text(size = 12), plot.title = element_text(size = 12, hjust = .5), legend.position = "right", legend.margin=margin(1, 1, 1, -5)) +
  facet_grid(task~participant)

# save plot faceted by participant
ggsave("data/objects/snulife/plots/ajt_fillers_zscore_ppt.png", width=5.5, height=3.5, dpi=600)

# summarize data for plotting by raw ratings
plot <- fill %>%
  mutate(cond = fct_drop(cond),
         set = fct_drop(superset)) %>%
  group_by(group, task, superset, cond) %>%
  summarise(mean = mean(button_pressed, na.rm=T),
            sd = sd(button_pressed, na.rm=T),
            n = n()) %>%
  mutate(se = sd / sqrt(n),
         ci = qt(1 - (0.05 / 2), n - 1) * se) %>%
  ungroup() %>%
  filter(cond != 'NA') %>%
  mutate(panel = case_when(group == 'english' & task == 'English AJT' ~ 'ENS on English AJT',
                           group == 'korean' & task == 'English AJT' ~ 'KLE on English AJT',
                           group == 'korean' & task == 'Korean AJT' ~ 'KLE on Korean AJT',
                           group == 'mandarin' & task == 'English AJT' ~ 'MLE on English AJT',
                           group == 'mandarin' & task == 'Korean AJT' ~ 'MLE on Mandarin AJT'))

# add styling
s <- list(
  annotate("rect", xmin = 0, xmax = 5, ymin = 1+(5/3), ymax = 1+(5/3)+(5/3), alpha = .15),
  geom_hline(yintercept=3.5),
  geom_line(lwd = 1),
  geom_point(size = 2),
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.2, lwd=1, linetype=1),
  theme_classic(),
  scale_x_discrete(name="type", limits = c("superset1", "superset2", "superset3", "superset4"), labels = c("F1", "F2", "F3", "F4")),
  scale_y_continuous(name="mean rating", limits=c(1, 6), breaks=c(1, 2, 3, 4, 5, 6)),
  scale_colour_manual(name="condition", values=c('#648fff', '#ffb000'), labels=c("grammatical", "ungrammatical")),
  scale_shape_manual(name="condition", values=c(16, 15), labels=c("grammatical", "ungrammatical")),
  theme(text = element_text(size = 12),
        plot.title = element_text(size = 12, hjust = .5)),
  facet_wrap(~panel)
)

# define data for panels
p1 <- ggplot(data=filter(plot, group == 'english' & task == 'English AJT'), aes(x=superset, y=mean, group=cond, col=cond, shape=cond))
p2 <- ggplot(data=filter(plot, group == 'korean' & task == 'English AJT'), aes(x=superset, y=mean, group=cond, col=cond, shape=cond))
p3 <- ggplot(data=filter(plot, group == 'mandarin' & task == 'English AJT'), aes(x=superset, y=mean, group=cond, col=cond, shape=cond))
p4 <- ggplot(data=filter(plot, group == 'korean' & task == 'Korean AJT'), aes(x=superset, y=mean, group=cond, col=cond, shape=cond))
p5 <- ggplot(data=filter(plot, group == 'mandarin' & task == 'Korean AJT'), aes(x=superset, y=mean, group=cond, col=cond, shape=cond))

# arrange and print
p1 + s + theme(legend.position="none", axis.title.x = element_blank(), plot.margin = margin(0, 0, -2, 0, "cm")) + 
  p2 + s + theme(legend.position="none", axis.title.x = element_blank(), axis.title.y = element_blank(), plot.margin = margin(0, 0, -2, 0, "cm")) + 
  p3 + s + theme(legend.position="none", axis.title.x = element_blank(), axis.title.y = element_blank(), plot.margin = margin(0, 0, -2, 0, "cm")) + 
  patchwork::plot_spacer() + 
  p4 + s + theme(legend.position = c(-.8, .65), axis.title.y = element_blank()) + 
  p5 + s + theme(legend.position="none", axis.title.x = element_blank(), axis.title.y = element_blank())

# save plot
ggsave("data/objects/plots/ajt_fillers_raw.png", width=6.5, height=3.5, dpi=600)

# summarize data for plotting by acceptance
plot <- fill %>%
  mutate(cond = fct_drop(cond),
         set = fct_drop(set),
         acceptance = case_when(button_pressed > 3.5 ~ TRUE,
                                button_pressed < 3.5 ~ FALSE)) %>%
  group_by(task, group, superset, cond) %>%
  summarise(mean = mean(acceptance, na.rm=T) * 100,
            sd = sd(acceptance, na.rm=T) * 100,
            n = n()) %>%
  mutate(se = sd / sqrt(n),
         ci = qt(1 - (0.05 / 2), n - 1) * se) %>%
  ungroup() %>%
  filter(cond != 'NA') %>%
  mutate(panel = case_when(group == 'english' & task == 'English AJT' ~ 'ENS on English AJT',
                           group == 'korean' & task == 'English AJT' ~ 'KLE on English AJT',
                           group == 'korean' & task == 'Korean AJT' ~ 'KLE on Korean AJT',
                           group == 'mandarin' & task == 'English AJT' ~ 'MLE on English AJT',
                           group == 'mandarin' & task == 'Korean AJT' ~ 'MLE on Mandarin AJT'))

# define data for panels
p1 <- ggplot(data=filter(plot, group == 'english' & task == 'English AJT'), aes(x=superset, y=mean, group=cond, col=cond, shape=cond))
p2 <- ggplot(data=filter(plot, group == 'korean' & task == 'English AJT'), aes(x=superset, y=mean, group=cond, col=cond, shape=cond))
p3 <- ggplot(data=filter(plot, group == 'mandarin' & task == 'English AJT'), aes(x=superset, y=mean, group=cond, col=cond, shape=cond))
p4 <- ggplot(data=filter(plot, group == 'korean' & task == 'Korean AJT'), aes(x=superset, y=mean, group=cond, col=cond, shape=cond))
p5 <- ggplot(data=filter(plot, group == 'mandarin' & task == 'Korean AJT'), aes(x=superset, y=mean, group=cond, col=cond, shape=cond))

# add styling
s <- list(
  annotate("rect", xmin = 0, xmax = 5, ymin = (100/3), ymax = (100/3)+(100/3), alpha = .15),
  geom_hline(yintercept=50),
  geom_line(lwd = 1),
  geom_point(size = 2),
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.2, lwd=1, linetype=1),
  theme_classic(),
  scale_x_discrete(name="type", limits = c("superset1", "superset2", "superset3", "superset4"), labels = c("F1", "F2", "F3", "F4")),
  scale_y_continuous(name='% acceptance', limits=c(0, 100)),
  scale_colour_manual(name="condition", values=c('#648fff', '#ffb000'), labels=c("grammatical", "ungrammatical")),
  scale_shape_manual(name="condition", values=c(16, 15), labels=c("grammatical", "ungrammatical")),
    theme(text = element_text(size = 12),
        plot.title = element_text(size = 12, hjust = .5)),
  facet_wrap(~panel)
)

# arrange and print
p1 + s + theme(legend.position="none", axis.title.x = element_blank(), plot.margin = margin(0, 0, -2, 0, "cm")) + 
  p2 + s + theme(legend.position="none", axis.title.x = element_blank(), axis.title.y = element_blank(), plot.margin = margin(0, 0, -2, 0, "cm")) + 
  p3 + s + theme(legend.position="none", axis.title.x = element_blank(), axis.title.y = element_blank(), plot.margin = margin(0, 0, -2, 0, "cm")) + 
  patchwork::plot_spacer() + 
  p4 + s + theme(legend.position = c(-.8, .65), axis.title.y = element_blank()) + 
  p5 + s + theme(legend.position="none", axis.title.x = element_blank(), axis.title.y = element_blank())

# save plot
ggsave("data/objects/plots/ajt_fillers_acceptance.png", width=6.5, height=3.5, dpi=600)

# clear objects from workspace
rm(plot, check)

#------------------------------------------------------------------------------#
#### ajt: density plots for ajt critical trials ####
#------------------------------------------------------------------------------#

# density plot of z-score data
density <- crit %>%
  filter(!cond %in% c('gram', 'ungr')) %>%
  mutate(cond = fct_drop(cond),
         environment = fct_relevel(environment, 'short', 'long', 'island')) %>%
  mutate(panel = case_when(group == 'english' & task == 'English AJT' ~ 'ENS\nEnglish AJT',
                           group == 'korean' & task == 'English AJT' ~ 'KLE\nEnglish AJT',
                           group == 'korean' & task == 'Korean AJT' ~ 'KLE\nKorean AJT',
                           group == 'mandarin' & task == 'English AJT' ~ 'MLE\nEnglish AJT',
                           group == 'mandarin' & task == 'Korean AJT' ~ 'MLE\nMandarin AJT'))
ggplot(density, aes(x=zscore, fill=dependency)) + 
  geom_vline(xintercept=0) +
  geom_density(alpha=.7) +
  theme_classic() +
  scale_x_continuous(name="z-score", limits = c(-3, 3)) +
  scale_fill_manual(name="dependency", values=c('#648fff', '#ffb000'), labels=c("gap", "RP")) +
  theme(text = element_text(size = 12), 
        plot.title = element_text(size = 12, hjust = .5), 
        legend.position = "bottom", 
        legend.margin=margin(0, 0, 0, 0),
        legend.box.margin = margin(-10, -10, 0, -10)) +
  facet_grid(environment ~ panel)

#save plot
ggsave("data/objects/plots/ajt_density_crit_zscore.png", width=6.5, height=3.5, dpi=600)

# density plot of raw ratings
density <- crit %>%
  filter(!cond %in% c('gram', 'ungr')) %>%
  mutate(cond = fct_drop(cond),
         environment = fct_relevel(environment, 'short', 'long', 'island')) %>%
  mutate(panel = case_when(group == 'english' & task == 'English AJT' ~ 'ENS\nEnglish AJT',
                           group == 'korean' & task == 'English AJT' ~ 'KLE\nEnglish AJT',
                           group == 'korean' & task == 'Korean AJT' ~ 'KLE\nKorean AJT',
                           group == 'mandarin' & task == 'English AJT' ~ 'MLE\nEnglish AJT',
                           group == 'mandarin' & task == 'Korean AJT' ~ 'MLE\nMandarin AJT'))
ggplot(density, aes(x=button_pressed, fill=dependency)) + 
  geom_vline(xintercept=3.5) +
  #geom_density(alpha=.3, aes(y=..scaled..)) +
  geom_histogram(alpha = .3, col = 'black') +
  theme_classic() +
  scale_x_continuous(name="z-score") +
  scale_fill_manual(name="dependency", values=c('#648fff', '#ffb000'), labels=c("gap", "RP")) +
  theme(text = element_text(size = 12), 
        plot.title = element_text(size = 12, hjust = .5), 
        legend.position = "bottom", 
        legend.margin=margin(0, 0, 0, 0),
        legend.box.margin = margin(-10, -10, 0, -10)) +
  facet_grid(environment ~ panel)

# save plot
ggsave("data/objects/plots/ajt_density_crit_raw.png", width=5, height=2.75, dpi=600)

# density by acceptance rates
plot <- crit %>%
  mutate(dependency = fct_drop(dependency),
         environment = fct_drop(environment),
         acceptance = case_when(button_pressed > 3.5 ~ TRUE,
                                button_pressed < 3.5 ~ FALSE)) %>%
  group_by(group, task, participant, dependency, environment) %>%
  summarise(mean = mean(acceptance, na.rm=T) * 100) %>%
  ungroup() %>%
  filter(dependency != 'NA') %>%
  mutate(panel = case_when(group == 'english' & task == 'English AJT' ~ 'ENS on English AJT',
                           group == 'korean' & task == 'English AJT' ~ 'KLE on English AJT',
                           group == 'korean' & task == 'Korean AJT' ~ 'KLE on Korean AJT',
                           group == 'mandarin' & task == 'English AJT' ~ 'MLE on English AJT',
                           group == 'mandarin' & task == 'Korean AJT' ~ 'MLE on Mandarin AJT')) %>%
  mutate(panel = factor(panel, levels = c('ENS on English AJT', 'KLE on English AJT', 'MLE on English AJT', 'KLE on Korean AJT', 'MLE on Mandarin AJT')))

ggplot(plot, aes(x=mean, fill=dependency)) + 
  geom_vline(xintercept=50) +
  geom_density(alpha=.3) +
  theme_classic() +
  scale_x_continuous(name="% acceptance") +
  scale_fill_manual(name="dependency", values=c('#648fff', '#ffb000'), labels=c("gap", "RP")) +
  theme(text = element_text(size = 12), 
        plot.title = element_text(size = 12, hjust = .5), 
        legend.position = "bottom", 
        legend.margin=margin(0, 0, 0, 0),
        legend.box.margin = margin(-10, -10, 0, -10)) +
  facet_grid(panel ~ environment)

#------------------------------------------------------------------------------#
#### ajt: bar plot of rating distributions ####
#------------------------------------------------------------------------------#

# summarise for plotting by panel
plot <- ajt %>%
  mutate(panel = case_when(group == 'english' & task == 'English AJT' ~ 'ENS on English AJT',
                           group == 'korean' & task == 'English AJT' ~ 'KLE on English AJT',
                           group == 'korean' & task == 'Korean AJT' ~ 'KLE on Korean AJT',
                           group == 'mandarin' & task == 'English AJT' ~ 'MLE on English AJT',
                           group == 'mandarin' & task == 'Korean AJT' ~ 'MLE on Mandarin AJT'))

# check proportion of responses for each value on rating scale by panel
check <- plot %>%
  mutate(button_pressed = factor(button_pressed)) %>%
  group_by(panel) %>%
  mutate(tot = n()) %>%
  ungroup() %>%
  group_by(panel, button_pressed) %>%
  summarise(perc = n() / tot) %>%
  ungroup() %>%
  group_by(panel, button_pressed, perc) %>%
  summarise() %>%
  ungroup() %>%
  group_by(panel) %>%
  mutate(tot = sum(perc)) %>%
  ungroup()

# check proportion of responses for highest and lowest values on rating scale
check <- check %>%
  filter(button_pressed %in% c('1', '6')) %>%
  group_by(panel) %>%
  summarise(sum = sum(perc)) %>%
  ungroup() %>%
  mutate(mean = mean(sum))

# check proportion of responses for middle values on rating scale
check <- check %>%
  filter(button_pressed %in% c('3', '4')) %>%
  group_by(panel) %>%
  summarise(sum = sum(perc)) %>%
  ungroup() %>%
  mutate(mean = mean(sum))


# define data for panels
p1 <- ggplot(data=filter(plot, panel == 'ENS on English AJT'), aes(x=button_pressed))
p2 <- ggplot(data=filter(plot, panel == 'KLE on English AJT'), aes(x=button_pressed))
p3 <- ggplot(data=filter(plot, panel == 'MLE on English AJT'), aes(x=button_pressed))
p4 <- ggplot(data=filter(plot, panel == 'KLE on Korean AJT'), aes(x=button_pressed))
p5 <- ggplot(data=filter(plot, panel == 'MLE on Mandarin AJT'), aes(x=button_pressed))

# add styling
s <- list(
  # geom_vline(xintercept=3.5),
  geom_bar(aes(y = (..count..) / sum(..count..) * 100), col = 'black', fill = 'lightblue'),
    #geom_bar(stat = "identity", col = "black", fill = 'lightblue', width = .5, alpha=.8),
    theme_classic(),
    scale_x_continuous(name="rating", breaks = c(1, 2, 3, 4, 5, 6)),
    scale_y_continuous(name="% responses", limits = c(0, 60)),
    theme(text = element_text(size = 12), 
          plot.title = element_text(size = 12, hjust = .5), 
          legend.position = "bottom", 
          legend.margin=margin(0, 0, 0, 0),
          legend.box.margin = margin(-10, -10, 0, -10)),
    facet_wrap(~panel)
)

# arrange plots
p1 + s + theme(legend.position="none", axis.title.x = element_blank(), plot.margin = margin(0, 0, -2, 0, "cm")) + 
  p2 + s + theme(legend.position="none", axis.title.x = element_blank(), axis.title.y = element_blank(), plot.margin = margin(0, 0, -2, 0, "cm")) + 
  p3 + s + theme(legend.position="none", axis.title.x = element_blank(), axis.title.y = element_blank(), plot.margin = margin(0, 0, -2, 0, "cm")) + 
  patchwork::plot_spacer() + 
  p4 + s + theme(legend.position = c(-.85, .65), axis.title.y = element_blank()) + 
  p5 + s + theme(legend.position="none", axis.title.x = element_blank(), axis.title.y = element_blank())

#save plot
ggsave("data/objects/plots/ajt_rating_distribution.png", width=6.5, height=3.5, dpi=600)




#save plot
ggsave("data/objects/plots/ajt_rating_distribution_everyone.png", width=5, height=1.5, dpi=600)

#------------------------------------------------------------------------------#
#### corrections to files 
#------------------------------------------------------------------------------#

files <- fs::dir_ls('data/subjects/korean/raw', regexp = 'task5.*\\.csv$')

for (file in files) {
  x <- read_csv(file, col_types = cols(.default = 'c')) %>%
    rename(response = responses)
  write_csv(x, file)
}

#------------------------------------------------------------------------------#
#### check submissions 
#------------------------------------------------------------------------------#

# read in data from file
sub <- read_csv('data/objects/korean/payment-korea.csv', col_types = cols(.default = 'c')) %>%
  select(run_id, response) %>%
  filter(response != '"') %>%
  separate_rows(response, sep = '","') %>%
  separate(response, c("field", "value"), sep = ":") %>%
  mutate(field = str_remove_all(field, '\\"|\\{|\\}'),
         value = str_remove_all(value, '\\"|\\{|\\}')) %>%
  pivot_wider(names_from = field, values_from = value) %>%
  filter(str_detect(pid, "researcher", negate=T)) %>%
  filter(pid != 'a')

# tidy up
rm(sub)

#------------------------------------------------------------------------------#
#### key ####
#------------------------------------------------------------------------------#

# tasks...
# language_survey: language background survey
# ept: english oral elicited production task
# sprt: english self-paced reading task
# english_ajt: english acceptability judgment task
# korean_ajt: korean acceptability judgment task
# mandarin_ajt: mandarin acceptability judgment task
# ctest: english c-test
# exit_survey: exit survey

# groups...
# ens: english native speakers
# kle: l1-korean l2 learners of english
# mle: l1-mandarin l2 learners of english

#------------------------------------------------------------------------------#
#### chopping block ####
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
#### ept: complex bar plot
#------------------------------------------------------------------------------#

# exclude participants who produced relative clauses at least half the time

clean <- ep %>%
  mutate(rc_count = case_when(type %in% c('nontarget') ~ FALSE, TRUE ~ TRUE)) %>%
  group_by(participant) %>%
  mutate(rc_rate = mean(rc_count, na.rm = T)) %>%
  ungroup() %>%
  filter(rc_rate >= .5)

# recode response types

clean <- clean %>%
  mutate(type_old = type) %>%
  mutate(type = case_when(type == 'gap' ~ 'gap_t',
                          type == 'resumption' ~ 'resumption_t',
                          type == 'nontarget' ~ 'nontarget',
                          type == 'other' & str_detect(subtype, 'resumption') == FALSE ~ 'gap_o',
                          type == 'other' & str_detect(subtype, 'resumption') == TRUE ~ 'resumption_o',
                          TRUE ~ 'gap_o'))

check <- clean %>%
  filter(is.na(type) == TRUE)

# summarise for plotting

plot <- clean %>%
  filter(is.na(condition) == FALSE) %>%
  mutate(type = as.character(type)) %>%
  group_by(study, group, condition) %>%
  count(type) %>%
  ungroup() %>%
  complete(type, nesting(condition), fill = list(n = 0)) %>%
  group_by(study, group, condition) %>%
  mutate(sum = sum(n)) %>%
  ungroup() %>%
  mutate(prop = (n/sum)*100) %>%
  mutate(prc = as.character(round(prop))) %>%
  mutate(prc = as.numeric(case_when(prc == '0' ~ NA_character_, TRUE ~ prc))) %>%
  mutate(type = factor(type, levels = c('gap_t', 'gap_o', 'nontarget', 'resumption_o', 'resumption_t')))

check <- plot %>%
  filter(is.na(type) == TRUE)

# facet labels

groups <- c(`english` = 'ENS', `korean` = 'KLE', `mandarin` = 'MLE')

# define data for plots

p1 <- ggplot(data=filter(plot, study == '210510_do'), aes(x=condition, y=prop, group=type, fill=type, label=prc))
p2 <- ggplot(data=filter(plot, study == '210510_su'), aes(x=condition, y=prop, group=type, fill=type, label=prc))

# add styling

s <- list(
  geom_bar(stat = "identity", col = "black", width = .5, alpha=.8),
  geom_text(size = 3, col = "black", position = position_stack(vjust = 0.5)),
  theme_classic(),
  scale_x_discrete(name="environment", limits = c('cond1', 'cond2', 'cond3'), labels = c('short', 'long', 'island')),
  scale_y_continuous(name="% responses", limits=c(0, 101), breaks=c(0, 20, 40, 60, 80, 100)),
  scale_fill_manual(name="dependency", values=c('#648fff', 'lightblue', 'gray', 'yellow', '#ffb000')),
  theme(text = element_text(size = 12), plot.title = element_text(size = 12, hjust = .5), legend.position = "right", legend.margin=margin(1, 1, 1, 1)),
  facet_wrap(~group, labeller = as_labeller(groups))
)

# print plots

p1 + s
ggsave("plots/orc/ept_simple.png", width=6.5, height=2.5, dpi=600)

p2 + s
ggsave("plots/src/ept_simple.png", width=6.5, height=2.5, dpi=600)

#------------------------------------------------------------------------------#
#### ept: interaction plot
#------------------------------------------------------------------------------#

# read in combined dataframe from .csv
ep <- read_csv('data/ept/data.csv', col_types = cols(.default = 'f'))
str(ep)

# check participants
check <- ep %>%
  mutate(participant = as.factor(participant))
str(check$participant)
summary(check$participant)

# filter to gap and resumption responses
# ep <- ep %>%
#   filter(type %in% c('gap', 'resumption')) %>%
#   mutate(type = fct_drop(type))

# exclude participants who repeated the test sentence or produced other deficient responses more than half the time
ep <- ep %>%
  mutate(repeat_count = case_when(type %in% c('repeat', 'nontarget', 'incomplete', 'NA') ~ 1, TRUE ~ 0)) %>%
  group_by(participant, run_id) %>%
  mutate(repeat_rate = mean(repeat_count, na.rm = T)) %>%
  ungroup() %>%
  filter(repeat_rate < .5) %>%
  mutate(participant = fct_drop(participant))

# check participants
check <- ep %>%
  mutate(participant = as.factor(participant))
str(check$participant)
summary(check$participant)

# remove nontarget responses
target <- ep %>%
  filter(!type %in% c('repeat', 'nontarget', 'incomplete', 'NA'))

# check participants
check <- target %>%
  mutate(participant = as.factor(participant))
str(check$participant)
summary(check$participant)

# summarise data for plotting
plot <- target %>%
  filter(cond %in% c('cond1', 'cond2', 'cond3')) %>%
  mutate(category = case_when(type == 'gap' ~ 'gap',
                              type == 'resumption' ~ 'resumption',
                              TRUE ~ 'other')) %>%
  mutate(category = as.character(category)) %>%
  group_by(group, cond, participant) %>%
  count(category) %>%
  ungroup() %>%
  complete(category, nesting(group, cond, participant), fill = list(n = 0)) %>%
  group_by(group, cond, category) %>%
  summarise(mean = mean(n),
            sd = sd(n),
            num = n()) %>%
  ungroup() %>%
  mutate(se = sd / sqrt(num),
         ci = qt(1 - (0.05 / 2), num - 1) * se) %>%
  mutate(prop = (mean / 5) * 100,
         ci2 = (ci / 5) * 100) %>%
  mutate(panel = 'all participants')

# facet labels
groups <- c(`english` = 'L1-English Group\nEnglish OPT', `korean` = 'L1-Korean Group\nEnglish OPT')

# interaction plot
ggplot(data=plot, aes(x=cond, y=mean, group=category, col=category, shape=category)) +
  geom_hline(yintercept=2.5) +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.2, lwd=1, linetype=1) +
  geom_line(lwd = 1) +
  geom_point(size = 2) +
  theme_classic() +
  scale_x_discrete(name="environment", limits = c("cond1", "cond2", "cond3"), labels = c("short", "long", "island")) +
  scale_y_continuous(name="mean tokens", limits=c(-.1, 5), breaks = c(0, 1, 2, 3, 4, 5)) +
  scale_colour_manual(name="dependency", values=c('#648fff', '#ffb000', "grey"), limits=c("gap", "resumption", "other")) +
  scale_shape_manual(name="dependency", values=c(16, 15, 17), limits=c("gap", "resumption", "other")) +
  theme(text = element_text(size = 12), plot.title = element_text(size = 12, hjust = .5), legend.position = "right", legend.margin=margin(1, 1, 1, -5)) +
  facet_wrap(~group, labeller = as_labeller(groups))

# save plot
ggsave("data/plots/ept_interaction_plot_exclude.png", width=5.5, height=2.5, dpi=600)

# make list of participants who used resumption at least once
resumers <- target %>%
  filter(type == 'resumption') %>%
  group_by(participant) %>%
  summarise() %>%
  ungroup() %>%
  mutate(resumer = TRUE)

# add information about who used resumption to dataframe
target2 <- target %>%
  left_join(resumers, by = 'participant')

# filter to participants who used resumption at least once
target2 <- target2 %>%
  filter(resumer == TRUE)

# summarise 'target2' for plotting
plot2 <- target2 %>%
  filter(cond %in% c('cond1', 'cond2', 'cond3')) %>%
  mutate(category = case_when(type == 'gap' ~ 'gap',
                              type == 'resumption' ~ 'resumption',
                              TRUE ~ 'other')) %>%
  mutate(category = as.character(category)) %>%
  group_by(group, cond, participant) %>%
  count(category) %>%
  ungroup() %>%
  complete(category, nesting(group, cond, participant), fill = list(n = 0)) %>%
  group_by(group, cond, category) %>%
  summarise(mean = mean(n),
            sd = sd(n),
            num = n()) %>%
  ungroup() %>%
  mutate(se = sd / sqrt(num),
         ci = qt(1 - (0.05 / 2), num - 1) * se) %>%
  mutate(prop = (mean / 5) * 100,
         ci = (ci / 5) * 100) %>%
  mutate(panel = 'resumption users')

# combine plot dataframes
plot3 <- bind_rows(plot, plot2) %>%
  filter(category != 'other')

# facet labels
groups <- c(`english` = 'L1-English Group\nEnglish OPT', 
            `korean` = 'L1-Korean Group\nEnglish OPT',
            `all participants` = 'all participants (n = 68)',
            `resumption users` = 'resumption users (n = 26)')

# interaction plot
ggplot(data=plot3, aes(x=cond, y=prop, group=category, col=category, shape=category)) +
  geom_hline(yintercept=50) +
  geom_errorbar(aes(ymin=prop-ci, ymax=prop+ci), width=.2, lwd=1, linetype=1) +
  geom_line(lwd = 1) +
  geom_point(size = 2) +
  theme_classic() +
  scale_x_discrete(name="environment", limits = c("cond1", "cond2", "cond3"), labels = c("short", "long", "island")) +
  scale_y_continuous(name="% responses", limits=c(-5, 100), breaks = c(0, 20, 40, 60, 80, 100)) +
  scale_colour_manual(name="dependency", values=c('#648fff', '#ffb000'), limits=c("gap", "resumption")) +
  scale_shape_manual(name="dependency", values=c(16, 15), limits=c("gap", "resumption")) +
  theme(text = element_text(size = 12), plot.title = element_text(size = 12, hjust = .5), legend.position = "right", legend.margin=margin(1, 1, 1, -5)) +
  facet_grid(panel~group, labeller = as_labeller(groups))

# save plot
ggsave("data/plots/ept_interaction_plot_resumer_comparison.png", width=5.5, height=4.5, dpi=600)

#------------------------------------------------------------------------------#
#### ept: scatter plot of resumption rate against proficiency scores (by environment)
#------------------------------------------------------------------------------#

# summarise data for plotting

plot <- ep %>%
  filter(condition %in% c('cond1', 'cond2', 'cond3')) %>%
  mutate(type = case_when(type == 'gap' ~ 'gap',
                          type == 'resumption' ~ 'resumption',
                          TRUE ~ 'other')) %>%
  mutate(type = as.character(type)) %>%
  group_by(study, group, participant, environment) %>%
  count(type) %>%
  ungroup() %>%
  group_by(study, group, participant, environment) %>%
  mutate(sum = sum(n)) %>%
  ungroup() %>%
  mutate(prop = (n/sum)*100) %>%
  mutate(prc = as.character(round(prop))) %>%
  mutate(prc = as.numeric(case_when(prc == '0' ~ NA_character_, TRUE ~ prc))) %>%
  mutate(type = factor(type, levels = c('gap', 'other', 'resumption'))) %>%
  mutate(environment = factor(environment, levels = c('short', 'long', 'island')))

plot <- plot %>%
  complete(type, nesting(study, group, participant, environment), fill = list(prop = 0, n = 0)) %>%
  select(study, group, environment, participant, type, n, prop) %>%
  arrange(study, group, environment, participant, type, prop)

plot <- plot %>%
  left_join(proficiency, by = c('study', 'group', 'participant')) %>%
  filter(proficiency != 'NA') %>%
  filter(type == 'resumption')

# facet labels

groups <- c(`english` = 'ENS', `korean` = 'KLE', `mandarin` = 'MLE',
            `short` = 'short', `long` = 'long', `island` = 'island')

# define data for plots

p1 <- ggplot(data=filter(plot, study == '210510_do'), aes(x=proficiency*100, y=prop))
p2 <- ggplot(data=filter(plot, study == '210510_su'), aes(x=proficiency*100, y=prop))

# generate plot

s <- list(
  geom_smooth(method=lm, col="#785ef0"), 
  geom_point(shape = 1),
  theme_classic(),
  scale_x_continuous(name='proficiency'),
  scale_y_continuous(name="% resumption", limits = c(-5, 100)),
  scale_fill_manual(name='group', values=c("#9b82f3", "#00a78f")),
  theme(text = element_text(size = 12),
        plot.title = element_text(size = 12, hjust = .5), 
        legend.position = "right"),
  facet_grid(environment~group, labeller = as_labeller(groups))
)

# write plots

p1 + s
ggsave("plots/orc/ept_proficiency.png", width=6.5, height=3, dpi=600)

p2 + s
ggsave("plots/src/ept_proficiency.png", width=6.5, height=3, dpi=600)

#------------------------------------------------------------------------------#
#### spr: plots for rt data by participant and by item
#------------------------------------------------------------------------------#

# summarise data for plotting by participant
plot2 <- trim %>%
  mutate(environment = as.factor(environment)) %>%
  mutate(environment = fct_relevel(environment, 'short', 'long', 'island')) %>%
  group_by(run_id, participant, region, dependency, environment, cond) %>%
  summarise(mean_rt = mean(rt, na.rm=T),
            sd = sd(rt, na.rm=T),
            n = n()) %>%
  mutate(se = sd / sqrt(n),
         ci = qt(1 - (0.05 / 2), n - 1) * se) %>%
  ungroup() %>%
  add_row(region = "11", cond = "cond1", dependency = 'gap', environment = 'short') %>%
  add_row(region = "11", cond = "cond2", dependency = 'gap', environment = 'long') %>%
  add_row(region = "11", cond = "cond3", dependency = 'gap', environment = 'island') %>%
  filter(region %in% c(9, 10, 11, 12, 13, 14))

# plot by participant
plot2 <- plot2 %>%
  filter(region %in% c(10, 11, 12, 13)) %>%
  mutate(environment = fct_relevel(environment, 'short', 'long', 'island'))
ggplot(data=plot2, aes(x=region, y=mean_rt, group=dependency, col=dependency, shape=dependency)) +
  annotate("rect", xmin = 3.5, xmax = 5.5, ymin = 200, ymax = 700, alpha = .2) +
  geom_hline(yintercept = 0) +
  geom_line(lwd=1) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin=mean_rt-ci, ymax=mean_rt+ci), width=.5, lwd=1, linetype=1) +
  theme_classic() +
  scale_y_continuous(name="mean reading time (ms)", limits=c(-1000, 2000)) +
  scale_x_discrete(name="region", limits=c("9", "10", "11", "12", "13", "14"), labels=c('9', '10', '11', '12', '13', '14')) +
  scale_colour_manual(name="dependency", values=c('#648fff', '#ffb000'), labels=c('gap', 'RP')) +
  scale_shape_manual(name="dependency", values=c(16, 15), labels=c('gap', 'RP')) +
  theme(text = element_text(size = 12), plot.title = element_text(size = 12, hjust = .5), legend.position = "right", legend.margin = margin(1, 1, 1, -5)) +
  facet_grid(participant~environment)

# save plot
#ggsave("plots/plot_spr_ppt.png", width=5.5, height=30, dpi=600)
ggsave("data/english/plots/spr_ppt.png", width=5.5, height=30, dpi=600)

# summarise data for scatterplot by item
plot <- trim %>%
  filter(region %in% c('12', '13')) %>%
  mutate(item = as.character(item)) %>%
  mutate(item = str_remove_all(item, 'item')) %>%
  mutate(cond = as.factor(cond)) %>%
  group_by(run_id, item, environment, dependency, cond) %>%
  summarise(sum = sum(rt, na.rm=T)) %>%
  ungroup() %>%
  group_by(item, environment, dependency, cond) %>%
  summarise(mean = mean(sum, na.rm=T)) %>%
  ungroup() 

# generate scatterplot by item
ggplot(plot, aes(x=cond, y=mean, fill=dependency, label=item)) + 
  geom_violin() +
  geom_boxplot(width = .1, fill='white') +
  #geom_jitter(shape=1, position = position_jitter(seed=2)) +
  geom_text(size = 2.5, col = "black", position = position_jitter(seed=2)) +
  theme_classic() +
  scale_x_discrete(name="environment", labels=c('short', 'long', 'island', 'short', 'long', 'island')) +
  scale_y_continuous(name="mean reading time (ms)") +
  scale_fill_manual(name='dependency', values=c("#9b82f3", "#00a78f")) +
  theme(text = element_text(size = 12), 
        plot.title = element_text(size = 12, hjust = .5), 
        legend.position = "right")

# save plot
ggsave("plots/plot_spr_item.png", width=5.5, height=2.75, dpi=600)
ggsave("plots/plots_su/plot_spr_item.png", width=5.5, height=2.75, dpi=600)

# summarise data for scatterplot by participant
plot <- trim %>%
  filter(region %in% c('12', '13')) %>%
  mutate(cond = as.factor(cond)) %>%
  group_by(run_id, item, environment, dependency, cond) %>%
  summarise(sum = sum(rt, na.rm=T)) %>%
  ungroup() %>%
  group_by(run_id, environment, dependency, cond) %>%
  summarise(mean = mean(sum, na.rm=T)) %>%
  ungroup() 

# generate plot by participant
ggplot(plot, aes(x=cond, y=mean, fill=dependency, label=run_id)) +
  geom_violin() +
  geom_boxplot(width = .1, fill='white') +
  geom_jitter(shape=1, alpha = .15, position = position_jitter(seed=2, width = .2)) +
  #geom_text(size = 2.5, col = "black", position = position_jitter(seed=2)) +
  theme_classic() +
  scale_x_discrete(name="environment", labels=c('short', 'long', 'island', 'short', 'long', 'island')) +
  scale_y_continuous(name="mean reading time (ms)") +
  scale_fill_manual(name='dependency', values=c("#9b82f3", "#00a78f")) +
  theme(text = element_text(size = 12), 
        plot.title = element_text(size = 12, hjust = .5), 
        legend.position = "right")

# save plot
ggsave("plots/plot_spr_scatter_ppt.png", width=5.5, height=2.75, dpi=600)
ggsave("plots/plots_su/plot_spr_scatter_ppt.png", width=5.5, height=2.75, dpi=600)

#------------------------------------------------------------------------------#
#### spr: plots for accuracy data by participant and by item
#------------------------------------------------------------------------------#

#summarize data for plotting by participant
plot <- ds %>%
  group_by(run_id, participant, dependency, environment) %>%
  filter(region == "1") %>%
  summarise(mean = mean(accuracy, na.rm=T) * 100,
            sd = sd(accuracy, na.rm=T) * 100,
            n = n()) %>%
  mutate(se = sd / sqrt(n),
         ci = qt(1 - (0.05 / 2), n - 1) * se) %>%
  ungroup()

# create plot by participant
ggplot(data=plot, aes(x=environment, y=mean, group=dependency, col=dependency, shape=dependency)) +
  #geom_hline(yintercept=50) +
  geom_line(lwd = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.2, lwd=1, linetype=1) +
  theme_classic() +
  scale_x_discrete(name="environment", limits = c("short", "long", "island"), labels = c("short", "long", "island")) +
  scale_y_continuous(name="% accuracy", limits=c(0, 100), breaks = c(25, 50, 75, 100)) +
  scale_colour_manual(name="dependency", values=c('#648fff', '#ffb000'), labels=c("gap", "RP")) +
  scale_shape_manual(name="dependency", values=c(16, 15), labels=c("gap", "RP")) +
  theme(text = element_text(size = 12), plot.title = element_text(size = 12, hjust = .5), legend.position = "right", legend.margin=margin(1, 1, 1, -5)) +
  facet_wrap(~participant)

# save plot by participant
#ggsave("plots/spr_accuracy_ppt.png", width=3.5, height=2.5, dpi=600)
ggsave("data/objects/snulife/plots/spr_accuracy_ppt.png", width=3.5, height=2.5, dpi=600)

#summarize data for plotting by item
plot <- ds %>%
  group_by(item, dependency, environment) %>%
  filter(region == "1") %>%
  summarise(mean = mean(accuracy, na.rm=T) * 100,
            sd = sd(accuracy, na.rm=T) * 100,
            n = n()) %>%
  mutate(se = sd / sqrt(n),
         ci = qt(1 - (0.05 / 2), n - 1) * se) %>%
  ungroup()

# create interaction plot by item
ggplot(data=plot, aes(x=environment, y=mean, group=dependency, col=dependency, shape=dependency)) +
  #geom_hline(yintercept=50) +
  geom_line(lwd = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.2, lwd=1, linetype=1) +
  theme_classic() +
  scale_x_discrete(name="environment", limits = c("short", "long", "island"), labels = c("short", "long", "island")) +
  scale_y_continuous(name="% accuracy", limits=c(25, 100), breaks = c(25, 50, 75, 100)) +
  scale_colour_manual(name="dependency", values=c('#648fff', '#ffb000'), labels=c("gap", "RP")) +
  scale_shape_manual(name="dependency", values=c(16, 15), labels=c("gap", "RP")) +
  theme(text = element_text(size = 12), plot.title = element_text(size = 12, hjust = .5), legend.position = "right", legend.margin=margin(1, 1, 1, -5)) +
  facet_wrap(~item)

# save plot by participant
ggsave("plots/spr_accuracy_item.png", width=3.5, height=2.5, dpi=600)

# summarise data for scatterplot by item
plot <- ds %>%
  mutate(item = as.character(item)) %>%
  mutate(item = str_remove_all(item, 'item')) %>%
  mutate(cond = as.factor(cond)) %>%
  group_by(item, environment, dependency, cond) %>%
  summarise(mean = mean(accuracy, na.rm=T)) %>%
  ungroup() 

# generate scatterplot by item
ggplot(plot, aes(x=cond, y=mean, fill=dependency, label=item)) + 
  geom_violin() +
  geom_boxplot(width = .1, fill='white') +
  #geom_jitter(shape=1, position = position_jitter(seed=2)) +
  geom_text(size = 2.5, col = "black", position = position_jitter(seed=2)) +
  theme_classic() +
  scale_x_discrete(name="environment", labels=c('short', 'long', 'island', 'short', 'long', 'island')) +
  scale_y_continuous(name="% accuracy") +
  scale_fill_manual(name='dependency', values=c("#9b82f3", "#00a78f")) +
  theme(text = element_text(size = 12), 
        plot.title = element_text(size = 12, hjust = .5), 
        legend.position = "right")

# save plot
ggsave("plots/plot_spr_accuracy_scatter_item.png", width=5.5, height=2.75, dpi=600)

# summarise data for scatterplot by participant
plot <- ds %>%
  mutate(cond = as.factor(cond)) %>%
  group_by(run_id, environment, dependency, cond) %>%
  summarise(mean = mean(accuracy, na.rm=T)) %>%
  ungroup()

# generate plot by participant
ggplot(plot, aes(x=cond, y=mean, fill=dependency, label=run_id)) +
  geom_violin() +
  geom_boxplot(width = .1, fill='white') +
  #geom_jitter(shape=1, position = position_jitter(seed=2)) +
  geom_text(size = 2.5, col = "black", position = position_jitter(seed=2)) +
  theme_classic() +
  scale_x_discrete(name="environment", labels=c('short', 'long', 'island', 'short', 'long', 'island')) +
  scale_y_continuous(name="mean reading time (ms)") +
  scale_fill_manual(name='dependency', values=c("#9b82f3", "#00a78f")) +
  theme(text = element_text(size = 12), 
        plot.title = element_text(size = 12, hjust = .5), 
        legend.position = "right")

# save plot
ggsave("plots/plot_spr_accuracy_scatter_ppt.png", width=5.5, height=2.75, dpi=600)

# clean workspace
rm(check, ds, plot, spr, spr_accuracy, spr_question, spr_stimulus)
