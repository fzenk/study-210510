#==============================================================================#
# ::::: script header ::::: ----
#==============================================================================#

# title: analysis for study no. 210510
# author: fred zenker
# created: 2020-05-11
# updated: 2022-06-10

#==============================================================================#
# ::::: packages ::::: ----
#==============================================================================#

# load packages ...

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
library(emmeans) #  for post-hoc tests
library(performance) # for checking model performance
library(kableExtra) # for tables
library(broom) # for tables
library(ordinal) # for clmm models

#==============================================================================#
# ::::: data ::::: ----
#==============================================================================#

# load data ...

df <- read_csv('data/data.csv', col_types = cols(.default = 'c')) %>%
  select(-audio_data)

ct <- read_csv('data/ctest_scored.csv', col_types = cols(.default = 'f', accuracy = 'l'))

#------------------------------------------------------------------------------#
# + plot number of participants in each task 
#------------------------------------------------------------------------------#

# summarise ...

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

# plot ...

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

# save ...

ggsave("plots/participant_counts.png", width=6.5, height=4.5, dpi=600)

#==============================================================================#
# ::::: exit survey ::::: ----
#==============================================================================#

# prep dataframe ...

exit <- df %>%
  filter(task == 'exit_survey') %>%
  arrange(study, group, participant) %>%
  select_if(function(x){!all(is.na(x))})

#==============================================================================#
# ::::: language survey ::::: ----
#==============================================================================#

# prep dataframe ...

survey <- df %>%
  filter(task == 'language_survey') %>%
  arrange(study, group, participant) %>%
  select_if(function(x){!all(is.na(x))})

# count participants ...

check <- survey %>%
  group_by(study, group, participant) %>%
  summarise() %>%
  ungroup() %>%
  group_by(study, group) %>%
  summarise(n = n()) %>%
  ungroup()

# check age ...

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

# check age of acquisition ...

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

# check length of residence ...

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

#==============================================================================#
# ::::: c-test ::::: ----
#==============================================================================#

# prep dataframe ...

ct <- df %>% filter(task == 'ctest') %>%
  arrange(study, group, participant) %>%
  select_if(function(x){!all(is.na(x))})

ct <- ct %>%
  mutate(response = str_remove_all(response, '[^[:alnum:]]'))

#------------------------------------------------------------------------------#
# score responses ----
#------------------------------------------------------------------------------#

# count participants ...

check <- ct %>%
  group_by(study, group, participant) %>%
  summarise() %>%
  ungroup() %>%
  group_by(study, group) %>%
  summarise(n = n()) %>%
  ungroup()

# drop unneeded columns ...

ct <- ct %>% select(study, group, participant, item, response)

# read in correct answers ...

answers <- read_csv('data/answers.csv', col_types = cols(.default = 'c')) %>%
  select(item, word, onset, exact, acceptable)

# add correct answers to dataframe ...

ct <- ct %>% left_join(answers, by = 'item')

# correct spelling and determine accuracy ...

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

# make list of proficiency scores ...

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

# check incorrect responses ...

check <- ct %>%
  filter(accuracy == FALSE) %>%
  select(study, group, participant, item, word, response, accuracy)

# write to csv ...

write_csv(ct, 'data/ctest_scored.csv')

#------------------------------------------------------------------------------#
# plots ----
#------------------------------------------------------------------------------#

# count participants ...

check <- ct %>%
  group_by(study, group, participant) %>%
  summarise() %>%
  ungroup() %>%
  group_by(study, group) %>%
  summarise(n = n()) %>%
  ungroup()

# summarise for plotting ...

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

# define plot data ...

p1 <- ggplot(data=filter(plot, study == '210510_do'), aes(x=group, y=accuracy*100, fill=group, label=participant))
p2 <- ggplot(data=filter(plot, study == '210510_su'), aes(x=group, y=accuracy*100, fill=group, label=participant))

# define plot styling ...

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

# print and save plots ...

p1 + s
ggsave("plots/orc/ctest.png", width=6, height=2, dpi=600)

p2 + s
ggsave("plots/src/ctest.png", width=6, height=2, dpi=600)

#------------------------------------------------------------------------------#
# modeling ----
#------------------------------------------------------------------------------#

# filter for analysis ...

md <- ct %>%
  filter(study == '210510_do')

# check contrasts ...

contrasts(md$group)

# relevel factor ...

md <- md %>%
  mutate(group = fct_relevel(group, 'korean', 'mandarin', 'english'))

# fit model ...

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

#==============================================================================#
# ::::: elicited production task (ept) ::::: ----
#==============================================================================#

# prep dataframe ...

ep <- df %>%
  filter(task == 'ept') %>%
  arrange(study, group, participant) %>%
  select_if(function(x){!all(is.na(x))})

# create 'wide' version of dataframe

ep2 <- ep %>%
  select(study, group, participant, item, response, condition, target, type, environment) %>%
  filter(is.na(environment) == FALSE) %>%
  filter(type %in% c('gap', 'resumption')) %>%
  mutate(region1 = '', region2 = '', region3 = '', region4 = '', region5 = '', region6 = '')

write_csv(ep2, 'ept_speaking_time_analysis_wide.csv')

# convert to 'long' format

ep3 <- ep2 %>%
  pivot_longer(cols = region1:region6, names_to = 'region', values_to = 'duration')
  

write_csv(ep3, 'ept_speaking_time_analysis_long.csv')

#------------------------------------------------------------------------------#
# export responses to .webm ----
#------------------------------------------------------------------------------#

# filter to ept data ...

audio <- read_csv('data/data.csv', col_types = cols(.default = 'c')) %>%
  filter(task == 'ept') %>%
  arrange(study, group, participant) %>%
  select_if(function(x){!all(is.na(x))})

# define function to write audio data to sound files ...

convert <- function(x) { 
  binary <- base64decode(x["audio_data"])
  myfile <- file(paste0("D:/study210510/audio/all_webm/", x["participant"],"_",x["item"],"_", x["condition"],".webm"), "wb")
  writeBin(binary, myfile)
  close(myfile)
}

# replace empty cells ...

audio <- audio %>%
  mutate(participant = as.character(participant)) %>%
  mutate(condition = case_when(condition == '"' ~ 'null', TRUE ~ condition),
         participant = case_when(participant == '"' ~ 'null', TRUE ~ participant),
         item = case_when(item == '"' ~ 'null', TRUE ~ item),
         condition = case_when(condition == '"' ~ 'null', TRUE ~ condition))

# use apply() to iterate the "convert" function over all the rows in the dataframe ...

apply(X = audio, FUN = convert, MARGIN = 1)

#------------------------------------------------------------------------------#
# inter-rater reliability ----
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
# bar plot critical ----
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

# print and save ...

p1 + s
ggsave("plots/orc/ept_barplot.png", width=6.5, height=2.5, dpi=600)

p2 + s
ggsave("plots/src/ept_barplot.png", width=6.5, height=2.5, dpi=600)

#------------------------------------------------------------------------------#
# bar plot filler ----
#------------------------------------------------------------------------------#

# remove nontarget responses ...

target <- ep %>%
  filter(type != 'nontarget')

# summarise for plotting ...

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

# facet labels ...

groups <- c(`english` = 'ENS', `korean` = 'KLE', `mandarin` = 'MLE')

# define data for plots ...

p1 <- ggplot(data=filter(plot, study == '210510_do'), aes(x=group, y=prop, group=type, fill=type, label=prc))
p2 <- ggplot(data=filter(plot, study == '210510_su'), aes(x=group, y=prop, group=type, fill=type, label=prc))

# add styling ...

s <- list(
  geom_bar(stat = "identity", col = "black", width = .5, alpha=.8),
  geom_text(size = 3, col = "black", position = position_stack(vjust = 0.5)),
  theme_classic(),
  scale_x_discrete(name="group", limits = c('english', 'korean', 'mandarin'), labels = c('ENS', 'KLE', 'MLE')),
  scale_y_continuous(name="% responses", limits=c(0, 101), breaks=c(0, 20, 40, 60, 80, 100)),
  scale_fill_manual(name="dependency", values=c("#648fff", "gray", "#ffb000")),
  theme(text = element_text(size = 12), plot.title = element_text(size = 12, hjust = .5), legend.position = "right", legend.margin=margin(1, 1, 1, 1))
)

# print and save ...

p1 + s
ggsave("plots/orc/ept_barplot_filler.png", width=6.5, height=2.5, dpi=600)

p2 + s
ggsave("plots/src/ept_barplot_filler.png", width=6.5, height=2.5, dpi=600)

#------------------------------------------------------------------------------#
# bar plot critical nontarget ----
#------------------------------------------------------------------------------#

# list participants who only gave nontarget responses ...

temp <- ep %>%
  filter(type != 'nontarget') %>%
  group_by(study, group, participant) %>%
  summarise() %>%
  ungroup()

# summarise for plotting ...

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
  mutate(type = factor(type, levels = c('gap', 'other', 'nontarget', 'resumption')))

# facet labels ...

groups <- c(`english` = 'ENS', `korean` = 'KLE', `mandarin` = 'MLE')

# define data for plots ...

p1 <- ggplot(data=filter(plot, study == '210510_do'), aes(x=condition, y=prop, group=type, fill=type, label=prc))
p2 <- ggplot(data=filter(plot, study == '210510_su'), aes(x=condition, y=prop, group=type, fill=type, label=prc))

# add styling ...

s <- list(
  geom_bar(stat = "identity", col = "black", width = .5, alpha=.8),
  geom_text(size = 3, col = "black", position = position_stack(vjust = 0.5)),
  theme_classic(),
  scale_x_discrete(name="environment", limits = c('cond1', 'cond2', 'cond3'), labels = c('short', 'long', 'island')),
  scale_y_continuous(name="% responses", limits=c(0, 101), breaks=c(0, 20, 40, 60, 80, 100)),
  scale_fill_manual(name="dependency", values=c('#648fff', 'gray85', 'gray60', '#ffb000'), labels = c('gap', 'modified', 'nontarget', 'resumption')),
  theme(text = element_text(size = 12), plot.title = element_text(size = 12, hjust = .5), legend.position = "right", legend.margin=margin(1, 1, 1, 1)),
  facet_wrap(~group, labeller = as_labeller(groups))
)

# print and save ...

p1 + s
ggsave("plots/orc/ept_barplot_with_nontarget.png", width=6.5, height=2.75, dpi=600)

p2 + s
ggsave("plots/src/ept_barplot_with_nontarget.png", width=6.5, height=2.75, dpi=600)

#------------------------------------------------------------------------------#
# scatter plot of proficiency effects ----
#------------------------------------------------------------------------------#

# summarise for plotting ...

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

# facet labels ...

groups <- c(`english` = 'ENS', `korean` = 'KLE', `mandarin` = 'MLE')

# define data for plots ...

p1 <- ggplot(data=filter(plot, study == '210510_do'), aes(x=proficiency*100, y=prop))
p2 <- ggplot(data=filter(plot, study == '210510_su'), aes(x=proficiency*100, y=prop))

# generate plot ...

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

# print and save ...

p1 + s
ggsave("plots/orc/ept_proficiency.png", width=6.5, height=3, dpi=600)

p2 + s
ggsave("plots/src/ept_proficiency.png", width=6.5, height=3, dpi=600)

#------------------------------------------------------------------------------#
# simple linear regression analysis ----
#------------------------------------------------------------------------------#

# filter for analysis ...

md <- plot %>% filter(group == 'korean' & study == '210510_do')

# pearson correlation ...

cor(md$proficiency, md$prop) 

# fit model ...

model <- lm(prop ~ proficiency, data = md)

summary(model)

# doen: ' ' (r-squared = 0.000434, F = 0.03778, p = 0.8463)
# doko: '.' (r-squared = 0.05509, F = 3.848, p = 0.05402)
# dozh: ' ' (r-squared = 0.006488, F = 0.4832, p = 0.4891)
# suen: ' ' (r-squared = 0.005779, F = 0.343, p = 0.5604)
# suko: ' ' (r-squared = 0.03603, F = 2.392, p = 0.1269)
# suzh: ' ' (r-squared = 0.00, F = 0.00, p = 0.997)

#------------------------------------------------------------------------------#
# bar plot critical by-item ----
#------------------------------------------------------------------------------#

# summarise for plotting ...

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

# generate plot ...

ggplot(data=plot, aes(x=condition, y=prop, group=category, fill=category, label=prc)) +
  geom_bar(stat = "identity", col = "black", width = .5, alpha = .8) +
  geom_text(size = 3, col = "black", position = position_stack(vjust = 0.5)) +
  theme_classic() +
  scale_x_discrete(name="environment", limits = c('cond1', 'cond2', 'cond3'), labels = c('short', 'long', 'island')) +
  scale_y_continuous(name="% responses", limits=c(0, 101), breaks=c(0, 20, 40, 60, 80, 100)) +
  scale_fill_manual(name="dependency", values=c('#648fff', 'gray', '#ffb000')) +
  theme(text = element_text(size = 12), plot.title = element_text(size = 12, hjust = .5), legend.position = "right") +
  facet_grid(study~item)

# save plot ...

ggsave("plots/ept_plot_item.png", width=10, height=5, dpi=600)

#------------------------------------------------------------------------------#
# modeling ----
#------------------------------------------------------------------------------#

# remove participants who only gave nontarget responses ...

temp <- ep %>%
  filter(type != 'nontarget') %>%
  group_by(study, group, participant) %>%
  summarise() %>%
  ungroup()

md <- ep %>%
  filter(participant %in% temp$participant)

# code responses as ±resumption ...

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

# view contrasts ...

contrasts(md2$environment)

# fit model ...

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

# alternative models ...

model1 <- glmer(resumption ~ environment + (environment|participant) + (environment|item), 
                data = md2, family = binomial, glmerControl(optimizer = "bobyqa"))
summary(model1)

model2 <- glmer(type ~ environment + (1|participant) + (1|item), 
                data = md2, family = binomial, glmerControl(optimizer = "bobyqa"))
summary(model2)

model3 <- glmer(type ~ environment + (environment|item), 
                data = md2, family = binomial, glmerControl(optimizer = "bobyqa"))
summary(model3)

# ::: filter to participants who used resumption at least once and try again :::

# make list of participants who used resumption at least once ...

check <- md %>%
  filter(type == 'resumption') %>%
  group_by(participant) %>%
  summarise() %>%
  ungroup() %>%
  mutate(resumer = TRUE)

# english resumers: 13/33 = 0.3939394 = 39%
# korean resumers: 13/35 = 0.3428571 = 37%

# add information about who used resumption to dataframe ...

md <- md %>%
  left_join(check, by = 'participant')

# filter to participants who used resumption at least once ...

md2 <- md %>%
  filter(resumer == TRUE)

# summarise for plotting ...

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

# bar plot ...

ggplot(data=plot, aes(x=cond, y=prop, group=category, fill=category, label=prc)) +
  geom_bar(stat = "identity", col = "black", width = .5) +
  geom_text(size = 3.5, col = "black", position = position_stack(vjust = 0.5)) +
  theme_classic() +
  scale_x_discrete(name="environment", limits = c('cond1', 'cond2', 'cond3'), labels = c('short', 'long', 'island')) +
  scale_y_continuous(name="% responses", limits=c(0, 101), breaks=c(0, 20, 40, 60, 80, 100)) +
  scale_fill_manual(name="dependency", values=c("#9b82f3", "#00a78f")) +
  theme(text = element_text(size = 12), plot.title = element_text(size = 12, hjust = .5), legend.position = "right", legend.margin=margin(1, 1, 1, 1)) +
  facet_wrap(~group)

# save plot ...

ggsave("data/plots/ept_binary_resumers.png", width=5.5, height=2.5, dpi=600)

# filter for analysis ...

md3 <- md2 %>%
  filter(group == 'korean') %>%
  #filter(environment != 'NA') %>%
  filter(cond %in% c('cond1', 'cond2', 'cond3')) %>%
  select(group, participant, item, type, resumption, cond, resumer) %>%
  mutate(participant = as.factor(participant),
         item = as.factor(item))

# view contrasts ...

contrasts(md3$environment)

# fit model ...

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
# speaking time - plot - overall ----
#------------------------------------------------------------------------------#

st <- read_excel('data/ept_spoken_analysis.xlt')

# check participants

check <- st %>%
  group_by(study, group, participant) %>%
  summarise() %>%
  summarise(n = n()) %>%
  ungroup()

# filter to doen

st <- st %>%
  filter(study == '210510_do', group == 'english')

# summarise data for plotting by group

plot <- st %>%
  mutate(durations = as.numeric(durations) * 1000) %>%
  rename(dependency = type) %>%
  mutate(environment = as.factor(environment)) %>%
  mutate(environment = fct_relevel(environment, 'short', 'long', 'island')) %>%
  group_by(study, group, dependency, environment) %>%
  summarise(mean = mean(durations, na.rm=T),
            sd = sd(durations, na.rm=T),
            n = n()) %>%
  mutate(se = sd / sqrt(n),
         ci = qt(1 - (0.05 / 2), n - 1) * se) %>%
  ungroup() %>%
  filter(group %in% c('english', 'korean', 'mandarin')) %>%
  mutate(panel = case_when(group == 'english' ~ 'ENS',
                           group == 'korean' ~ 'KLE',
                           group == 'mandarin' ~ 'MLE')) %>%
  mutate(environment = fct_relevel(environment, 'short', 'long', 'island')) %>%
  filter(is.na(mean) == 'FALSE')

# generate plot

p1 <- ggplot(data = filter(plot, study == '210510_do'), aes(x = environment, y = mean, group = dependency, col = dependency, shape = dependency))
p2 <- ggplot(data = filter(plot, study == '210510_su'), aes(x = environment, y = mean, group = dependency, col = dependency, shape = dependency))

s <- list(
  ggtitle('ORC Speaking Times (Overall)'),
  geom_line(lwd=1),
  geom_point(size=2),
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.5, lwd=1, linetype=1),
  theme_classic(),
  scale_y_continuous(name="speaking time (ms)", limits=c(500, 700)),
  scale_x_discrete(name="environment", limits=c('short', 'long', 'island')),
  scale_colour_manual(name="dependency", values=c('#648fff', '#ffb000'), labels=c('gap', 'resumption')),
  scale_shape_manual(name="dependency", values=c(16, 15), labels=c('gap', 'resumption')),
  theme(text = element_text(size = 12),
        legend.position = 'bottom',
        legend.margin = margin(t = -.4, unit = 'cm'),
        plot.title = element_text(size = 12, hjust = .5)),
  facet_grid2(vars(panel), axes = 'all', remove_labels = 'y')
)

p1 + s
p2 + s

ggsave('plots/orc/ept_time_overall.png', width=6.5, height=3.5, dpi=600)

#------------------------------------------------------------------------------#
# speaking time - plot - by region ----
#------------------------------------------------------------------------------#

st <- read_excel('data/ept_spoken_analysis.xlt')

# check participants

check <- st %>%
  group_by(study, group, participant) %>%
  summarise() %>%
  summarise(n = n()) %>%
  ungroup()

# filter to doen

st <- st %>%
  filter(study == '210510_do', group == 'english')

# summarise data for plotting by group

plot <- st %>%
  mutate(durations = as.numeric(durations) * 1000) %>%
  rename(dependency = type) %>%
  mutate(environment = as.factor(environment)) %>%
  mutate(environment = fct_relevel(environment, 'short', 'long', 'island')) %>%
  group_by(study, group, dependency, environment, region) %>%
  summarise(mean = mean(durations, na.rm=T),
            sd = sd(durations, na.rm=T),
            n = n()) %>%
  mutate(se = sd / sqrt(n),
         ci = qt(1 - (0.05 / 2), n - 1) * se) %>%
  ungroup() %>%
  filter(group %in% c('english', 'korean', 'mandarin')) %>%
  mutate(panel = case_when(group == 'english' ~ 'ENS',
                           group == 'korean' ~ 'KLE',
                           group == 'mandarin' ~ 'MLE')) %>%
  mutate(environment = fct_relevel(environment, 'short', 'long', 'island')) %>%
  filter(is.na(mean) == 'FALSE')

# generate plot

p1 <- ggplot(data = filter(plot, study == '210510_do'), aes(x = region, y = mean, group = dependency, col = dependency, shape = dependency))
p2 <- ggplot(data = filter(plot, study == '210510_su'), aes(x = region, y = mean, group = dependency, col = dependency, shape = dependency))

s <- list(
  ggtitle('ORC Speaking Times (by Region)'),
  geom_line(lwd=1),
  geom_point(size=2),
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.5, lwd=1, linetype=1),
  theme_classic(),
  scale_y_continuous(name="speaking time (ms)", limits=c(0, 1300)),
  scale_x_discrete(name="region", 
                   limits=c('region1', 'region2', 'region3', 'region4', 'region5', 'region6', 'region7'),
                   labels=c('1', '2', '3', '4', '5', '6', '7')),
  scale_colour_manual(name="dependency", values=c('#648fff', '#ffb000'), labels=c('gap', 'resumption')),
  scale_shape_manual(name="dependency", values=c(16, 15), labels=c('gap', 'resumption')),
  theme(text = element_text(size = 12),
        legend.position = 'bottom',
        legend.margin = margin(t = -.4, unit = 'cm'),
        plot.title = element_text(size = 12, hjust = .5)),
  facet_grid2(vars(panel), vars(environment), axes = 'all', remove_labels = 'y')
)

p1 + s
p2 + s

ggsave('plots/orc/ept_time_region.png', width=6.5, height=3.5, dpi=600)

#==============================================================================#
# ::::: self-paced reading task (sprt) :::::  ----
#==============================================================================#

# spr = self-paced reading task

# prep dataframe ...

spr <- df %>%
  filter(task == 'sprt') %>%
  arrange(study, group, participant) %>%
  select_if(function(x){!all(is.na(x))})

#------------------------------------------------------------------------------#
# plots for rt data ----
#------------------------------------------------------------------------------#

# filter to critical trials ...

ds <- spr %>%
  filter(!condition %in% c('grammatical', 'ungrammatical')) %>%
  mutate(rt = as.numeric(as.character(rt)),
         participant = as.factor(participant))

# run fct_drop on 'participant' ...

ds <- ds %>%
  mutate(participant = fct_drop(participant))

# check participants ...

check <- ds %>%
  group_by(study, group, participant) %>%
  summarise() %>%
  summarise(n = n()) %>%
  ungroup()

# trim based on rt ...

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

# check participants ...

check <- trim %>%
  group_by(study, group, participant) %>%
  summarise() %>%
  summarise(n = n()) %>%
  ungroup()

plot <- trim %>%
  group_by(study, group, participant, acc_rate) %>%
  summarise() %>%
  ungroup()

# inspect accuracy rates ...

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

# trim based on accuracy ...

trim <- trim %>%
  filter(acc_rate > .5)

# check participants ...

check <- trim %>%
  group_by(study, group, participant) %>%
  summarise() %>%
  summarise(n = n()) %>%
  ungroup()

# summarise for plotting by group ...

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

# generate plot ...

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
# plots for accuracy data ----
#------------------------------------------------------------------------------#

# trim based on accuracy ...

trim <- ds %>%
  group_by(study, group, participant) %>%
  mutate(acc_rate = mean(as.logical(accuracy))) %>%
  ungroup() %>%
  filter(acc_rate >.5)
  
# summarize for plotting by group ...

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

# create plot ...

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
ggsave("plots/orc/spr_accuracy.png", width=6.5, height=2, dpi=600)

p2 + s
ggsave("plots/src/spr_accuracy.png", width=6.5, height=2.5, dpi=600)

#------------------------------------------------------------------------------#
# trim data and calculate RRTs ----
#------------------------------------------------------------------------------#

# filter to critical trials ...

spr_crit <- spr %>%
  filter(!condition %in% c('grammatical', 'ungrammatical')) %>%
  mutate(rt = as.numeric(as.character(rt)),
         participant = as.factor(participant)) %>%
  mutate(participant = fct_drop(participant))

# trim based on rt ...

spr_trim <- spr_crit %>%
  filter(rt <= 3000) %>%
  filter(rt >= 200)

# adjust region numbers ...

spr_trim <- spr_trim %>%
  mutate(region = as.numeric(region)) %>%
  mutate(region2 = case_when(study == '210510_do' ~ region - 11,
                             study == '210510_su' & environment %in% c('short', 'long') ~ region - 8,
                             study == '210510_su' & environment == 'island' ~ region - 10))

# make word length column ...

spr_trim <- spr_trim %>%
  mutate(stimulus = str_trim(tolower(stimulus))) %>%
  mutate(length = nchar(stimulus))
  
# calculate RRTs by participant for each study and add to dataframe ...

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

# replace extreme RRTs ...

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

# calculate accuracy rates on comprehension question by participant ...

spr_trim <- spr_trim %>%
  group_by(study, group, participant) %>%
  mutate(acc_rate = mean(as.logical(accuracy))) %>%
  ungroup()

# check distribution of RRTs ...

hist(spr_trim$rrt)
qqnorm(spr_trim$rrt)

# https://stat.ethz.ch/pipermail/r-help/2008-July/168808.html

# check participants ...

check <- spr_trim %>%
  group_by(study, group, participant) %>%
  summarise() %>%
  summarise(n = n()) %>%
  ungroup()

# inspect accuracy rates ...

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

# trim based on accuracy ...

spr_trim <- spr_trim %>%
  filter(acc_rate > .5)

# inspect accuracy rates ...

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

# check participants ...

check <- spr_trim %>%
  group_by(study, group, participant) %>%
  summarise() %>%
  summarise(n = n()) %>%
  ungroup()

#------------------------------------------------------------------------------#
# plots of raw RTs ----
#------------------------------------------------------------------------------#

# summarise for plotting by group ...

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

# generate plot ...

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
# modeling for raw RTs ----
#------------------------------------------------------------------------------#

# filter data for analysis ...

md <- spr_trim %>%
  filter(region2 == 2,
         study == '210510_do',
         group == 'mandarin')

# check distribution ...

hist(md$rt)
qqnorm(md$rt)

md <- md %>%
  mutate(dependency = as.factor(dependency),
         environment = as.factor(environment)) %>%
  mutate(environment = fct_relevel(environment, 'short', 'long', 'island'))

# view contrasts ...

contrasts(md$dependency)
contrasts(md$environment)

# full model ...

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

# doko region 1
# (skip for now)
saveRDS(mod_spr_1, file='models/orc_spr_rawrt_doko_region1_mod.rds') 

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

# doko region 3
# (skip for now)
saveRDS(mod_spr_1, file='models/orc_spr_rawrt_doko_region3_mod.rds') 

# dozh region 1
# (skip for now)
saveRDS(mod_spr_1, file='models/orc_spr_rawrt_dozh_region1_mod.rds') 

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

# dozh region 3
# (skip for now)
saveRDS(mod_spr_1, file='models/orc_spr_rawrt_dozh_region3_mod.rds') 

# suen region 1
# boundary (singular) fit: see help('isSingular')

saveRDS(mod_spr_1, file='models/src_spr_rawrt_suen_region1_mod.rds') 

# suen region 2
# boundary (singular) fit: see help('isSingular')

saveRDS(mod_spr_1, file='models/src_spr_rawrt_suen_region2_mod.rds') 

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
# plots of residual RTs ----
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
# modeling for residual RTs ----
#------------------------------------------------------------------------------#

temp <- spr_trim %>%
  mutate(dependency = as.factor(dependency),
         environment = as.factor(environment),
         group = as.factor(group)) %>%
  mutate(environment = fct_relevel(environment, 'short', 'long', 'island'))

#------------------------------------------------------------------------------#
# + all - region 1 ----
#------------------------------------------------------------------------------#

# filter data for analysis

md <- temp %>%
  filter(region2 == 1,
         study == '210510_do')

# apply deviation coding ...

contrasts(md$dependency) <- contr.treatment(2) - matrix(rep(1/2, 2), ncol=1)
contrasts(md$environment) <- contr.treatment(3) - matrix(rep(1/3, 6), ncol=2)
contrasts(md$group) <- contr.treatment(3) - matrix(rep(1/3, 6), ncol=2)

# view contrasts ...

contrasts(md$dependency)
contrasts(md$environment)
contrasts(md$group)

#------------------------------------------------------------------------------#
# + + model 1
#------------------------------------------------------------------------------#

# fit model ...

tic()
model1 <- lmer(rrt ~ dependency * environment * group + 
                 (1 | participant) + 
                 (1 | item), 
               data = md,
               control = lmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_doall_rrt_region1_md1.rds')
summary(model1)
toc()
beep()

# 1 sec elapsed
# no warnings
# Fixed effects:
#   Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)                      -22.3596     5.9447  122.7170  -3.761 0.000260 ***
# dependency2                      -20.0404     5.2096 6191.5919  -3.847 0.000121 ***
# environment2                       2.1643     6.3845 6186.4693   0.339 0.734628    
# environment3                       7.8804     6.3747 6186.7806   1.236 0.216433    
# group2                           -30.6595    12.7451  218.2847  -2.406 0.016980 *  
# group3                           -22.6946    12.6735  217.2190  -1.791 0.074732 .  
# dependency2:environment2          -0.4001    12.8039 6200.4348  -0.031 0.975074    
# dependency2:environment3          11.7110    12.7485 6182.7742   0.919 0.358331    
# dependency2:group2                 0.7726    12.5175 6177.8116   0.062 0.950790    
# dependency2:group3                 0.2818    12.4071 6178.7478   0.023 0.981879    
# environment2:group2                7.8668    15.3535 6190.1295   0.512 0.608402    
# environment3:group2                8.5062    15.3300 6189.5383   0.555 0.579003    
# environment2:group3                4.1582    15.2242 6198.1895   0.273 0.784761    
# environment3:group3                7.7825    15.2131 6196.1262   0.512 0.608973    
# dependency2:environment2:group2   29.2000    30.6912 6178.5607   0.951 0.341432    
# dependency2:environment3:group2   84.2242    30.6491 6179.8861   2.748 0.006013 ** 
# dependency2:environment2:group3   21.0141    30.4174 6185.0666   0.691 0.489681    
# dependency2:environment3:group3  113.2952    30.3926 6177.1007   3.728 0.000195 ***

# post-hoc tests ...

pairwise <- model1 %>%
  emmeans(~ group * dependency * environment) %>%
  contrast('pairwise', simple = 'each', combine = TRUE) %>%
  summary(by = NULL, adjust = 'holm')
pairwise

#------------------------------------------------------------------------------#
# + + model 2 (final) ---
#------------------------------------------------------------------------------#

# fit model ...

tic()
model2 <- lmer(rrt ~ dependency * environment * group + 
                 (1 + dependency | participant) + 
                 (1 + dependency | item), 
               data = md,
               control = lmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_doall_rrt_region1_md2.rds')
summary(model2)
toc()
beep()

# 3.03 sec elapsed
# no warnings
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                      -22.29972    5.96054  122.10645  -3.741 0.000280 ***
# dependency2                      -20.15268    6.68490   44.56127  -3.015 0.004236 ** 
# environment2                       1.87012    6.34053 5928.67968   0.295 0.768045    
# environment3                       7.89130    6.32526 5963.25831   1.248 0.212232    
# group2                           -30.69306   12.72676  218.26209  -2.412 0.016706 *  
# group3                           -22.64533   12.65542  217.20326  -1.789 0.074947 .  
# dependency2:environment2          -0.07737   12.70549 5982.36407  -0.006 0.995142    
# dependency2:environment3          11.28673   12.64999 5962.97400   0.892 0.372305    
# dependency2:group2                 0.14212   14.82816  216.70506   0.010 0.992362    
# dependency2:group3                 0.50949   14.71392  214.24947   0.035 0.972410    
# environment2:group2                7.93823   15.23441 5958.58820   0.521 0.602336    
# environment3:group2                8.72847   15.21091 5960.89625   0.574 0.566105    
# environment2:group3                3.93928   15.10521 5973.74175   0.261 0.794264    
# environment3:group3                7.84016   15.09314 5962.82104   0.519 0.603465    
# dependency2:environment2:group2   28.21885   30.45829 5962.49894   0.926 0.354237    
# dependency2:environment3:group2   84.32276   30.41442 5962.54404   2.772 0.005581 ** 
# dependency2:environment2:group3   20.70537   30.19007 5974.43876   0.686 0.492845    
# dependency2:environment3:group3  112.15337   30.16340 5961.99063   3.718 0.000202 ***

# compare models ...

anova(model1, model2, refit = FALSE)

# npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)   
# model1   21 87178 87320 -43568    87136                        
# model2   25 87168 87338 -43559    87118 17.704  4    0.00141 **

#------------------------------------------------------------------------------#
# + all - region 2 ----
#------------------------------------------------------------------------------#

# filter data for analysis

md <- temp %>%
  filter(region2 == 2,
         study == '210510_do')

# apply deviation coding ...

contrasts(md$dependency) <- contr.treatment(2) - matrix(rep(1/2, 2), ncol=1)
contrasts(md$environment) <- contr.treatment(3) - matrix(rep(1/3, 6), ncol=2)
contrasts(md$group) <- contr.treatment(3) - matrix(rep(1/3, 6), ncol=2)

# view contrasts ...

contrasts(md$dependency)
contrasts(md$environment)
contrasts(md$group)

#------------------------------------------------------------------------------#
# + + model 1
#------------------------------------------------------------------------------#

# fit model ...

tic()
model1 <- lmer(rrt ~ dependency * environment * group + 
                 (1 | participant) + 
                 (1 | item), 
               data = md,
               control = lmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_doall_rrt_region2_md1.rds')
summary(model1)
toc()
beep()

# 1.86 sec elapsed
# no warnings
# Fixed effects:
#   Estimate Std. Error       df t value Pr(>|t|)    
# (Intercept)                      -19.446      6.739  210.335  -2.886  0.00431 ** 
#   dependency2                      -30.991      3.967 6097.384  -7.811 6.62e-15 ***
#   environment2                       5.116      4.851 6093.508   1.055  0.29167    
# environment3                      13.896      4.855 6093.301   2.862  0.00422 ** 
#   group2                           -68.419     16.006  220.376  -4.275 2.85e-05 ***
#   group3                           -74.490     15.922  219.571  -4.678 5.05e-06 ***
#   dependency2:environment2         -30.423      9.713 6044.043  -3.132  0.00174 ** 
#   dependency2:environment3         -31.274      9.711 6083.990  -3.220  0.00129 ** 
#   dependency2:group2               -43.602      9.568 6075.866  -4.557 5.29e-06 ***
#   dependency2:group3               -55.437      9.437 6081.473  -5.874 4.47e-09 ***
#   environment2:group2               22.612     11.702 6097.071   1.932  0.05337 .  
# environment3:group2               16.350     11.713 6095.064   1.396  0.16281    
# environment2:group3               24.202     11.547 6098.566   2.096  0.03613 *  
#   environment3:group3               20.645     11.559 6098.439   1.786  0.07413 .  
# dependency2:environment2:group2  -29.496     23.401 6075.904  -1.260  0.20756    
# dependency2:environment3:group2   29.160     23.431 6077.209   1.245  0.21336    
# dependency2:environment2:group3  -10.872     23.088 6087.761  -0.471  0.63773    
# dependency2:environment3:group3   27.866     23.107 6076.344   1.206  0.22788 

#------------------------------------------------------------------------------#
# + + model 2
#------------------------------------------------------------------------------#

# fit model ...

tic()
model2 <- lmer(rrt ~ dependency * environment * group + 
                 (1 + dependency | participant) + 
                 (1 + dependency | item), 
               data = md,
               control = lmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_doall_rrt_region2_md2.rds')
summary(model2)
toc()
beep()

# 2.61 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error       df t value Pr(>|t|)    
# (Intercept)                      -19.521      6.775  213.561  -2.881 0.004364 ** 
# dependency2                      -30.987      4.919   88.098  -6.299 1.15e-08 ***
# environment2                       5.107      4.814 5869.643   1.061 0.288805    
# environment3                      13.925      4.816 5885.105   2.891 0.003849 ** 
# group2                           -68.552     16.022  220.149  -4.279 2.81e-05 ***
# group3                           -74.506     15.939  219.365  -4.675 5.14e-06 ***
# dependency2:environment2         -30.509      9.638 5864.199  -3.166 0.001555 ** 
# dependency2:environment3         -31.485      9.633 5877.537  -3.269 0.001087 ** 
# dependency2:group2               -43.761     11.420  223.141  -3.832 0.000165 ***
# dependency2:group3               -55.205     11.296  217.557  -4.887 1.98e-06 ***
# environment2:group2               22.410     11.609 5892.582   1.930 0.053599 .  
# environment3:group2               15.977     11.619 5889.656   1.375 0.169174    
# environment2:group3               24.437     11.453 5883.736   2.134 0.032913 *  
# environment3:group3               21.058     11.464 5880.139   1.837 0.066280 .  
# dependency2:environment2:group2  -29.730     23.215 5878.198  -1.281 0.200371    
# dependency2:environment3:group2   29.319     23.242 5874.647   1.261 0.207181    
# dependency2:environment2:group3  -11.252     22.900 5876.375  -0.491 0.623205    
# dependency2:environment3:group3   27.197     22.919 5866.996   1.187 0.235416 

# compare models ...

anova(model1, model2, refit = FALSE)

# npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)   
# model1   21 82435 82576 -41196    82393                        
# model2   25 82425 82594 -41187    82375 17.972  4    0.00125 **

#------------------------------------------------------------------------------#
# + + model 3
#------------------------------------------------------------------------------#

# fit model ...

tic()
model3 <- lmer(rrt ~ dependency * environment * group + 
                 (1 + dependency + environment | participant) + 
                 (1 + dependency | item), 
               data = md,
               control = lmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_doall_rrt_region1_md3.rds')
summary(model3)
toc()
beep()

# 12.59 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                      -22.27331    5.95732  122.41323  -3.739 0.000282 ***
# dependency2                      -20.20437    6.68362   44.85955  -3.023 0.004128 ** 
# environment2                       2.11902    7.18402  215.66040   0.295 0.768306    
# environment3                       7.97642    6.41539  891.37219   1.243 0.214075    
# group2                           -30.65197   12.72758  218.27164  -2.408 0.016857 *  
# group3                           -22.50357   12.65648  217.22826  -1.778 0.076798 .  
# dependency2:environment2          -0.05749   12.63585 5778.32152  -0.005 0.996370    
# dependency2:environment3          11.35926   12.57894 5748.48764   0.903 0.366544    
# dependency2:group2                 0.11742   14.82268  217.74769   0.008 0.993687    
# dependency2:group3                 0.22161   14.70748  215.24625   0.015 0.987992    
# environment2:group2                7.70045   17.25350  217.10658   0.446 0.655817    
# environment3:group2                8.59094   15.42631  900.87710   0.557 0.577733    
# environment2:group3                3.43070   17.11826  214.88697   0.200 0.841348    
# environment3:group3                7.63578   15.30879  892.29245   0.499 0.618055    
# dependency2:environment2:group2   28.25507   30.29113 5758.83037   0.933 0.350971    
# dependency2:environment3:group2   84.28112   30.24429 5749.58913   2.787 0.005343 ** 
# dependency2:environment2:group3   21.18051   30.02439 5769.43739   0.705 0.480563    
# dependency2:environment3:group3  112.39879   29.99366 5745.79186   3.747 0.000180 ***

# compare models ...

anova(model2, model3, refit = FALSE)

# npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)
# model2   25 87168 87338 -43559    87118                     
# model3   32 87172 87389 -43554    87108 9.9441  7     0.1918

#------------------------------------------------------------------------------#
# + + model 4
#------------------------------------------------------------------------------#

# fit model ...

tic()
model4 <- lmer(rrt ~ dependency * environment * group + 
                 (1 + dependency * environment | participant) + 
                 (1 + dependency * environment | item), 
               data = md,
               control = lmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_doall_rrt_region1_md4.rds')
summary(model4)
toc()
beep()

# 786.3 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error      df t value Pr(>|t|)    
# (Intercept)                      -19.535      6.814 217.327  -2.867 0.004555 ** 
#   dependency2                      -30.792      5.053  86.605  -6.094 2.96e-08 ***
#   environment2                       4.800      5.517  41.330   0.870 0.389338    
# environment3                      14.052      5.651  85.852   2.487 0.014835 *  
#   group2                           -68.753     16.032 220.161  -4.288 2.69e-05 ***
#   group3                           -74.458     15.949 219.417  -4.668 5.29e-06 ***
#   dependency2:environment2         -29.704     11.085  62.015  -2.680 0.009428 ** 
#   dependency2:environment3         -31.623     11.801  83.305  -2.680 0.008879 ** 
#   dependency2:group2               -43.571     11.544 233.876  -3.774 0.000203 ***
#   dependency2:group3               -55.113     11.422 228.527  -4.825 2.56e-06 ***
#   environment2:group2               22.388     12.338 386.246   1.815 0.070369 .  
# environment3:group2               16.198     12.890 317.708   1.257 0.209822    
# environment2:group3               23.646     12.192 374.876   1.940 0.053184 .  
# environment3:group3               20.990     12.741 309.990   1.647 0.100486    
# dependency2:environment2:group2  -30.120     24.124 319.606  -1.249 0.212741    
# dependency2:environment3:group2   29.387     26.622 285.954   1.104 0.270576    
# dependency2:environment2:group3  -10.691     23.828 309.938  -0.449 0.653969    
# dependency2:environment3:group3   27.456     26.315 279.495   1.043 0.297676 

anova(model4, model2)

#------------------------------------------------------------------------------#
# + + model 5
#------------------------------------------------------------------------------#

# fit model ...

tic()
model5 <- lmer(rrt ~ dependency * environment * group + 
                 (1 + dependency * environment | participant) + 
                 (1 + dependency * environment * group | item), 
               data = md,
               control = lmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_doall_rrt_region1_md5.rds')
summary(model5)
toc()
beep()

# took too long; run overnight

#------------------------------------------------------------------------------#
# + doen - region 1 ----
#------------------------------------------------------------------------------#

# filter data for analysis

md <- temp %>%
  filter(region2 == 1,
         study == '210510_do',
         group == 'english')

# apply deviation coding ...

contrasts(md$dependency) <- contr.treatment(2) - matrix(rep(1/2, 2), ncol=1)
contrasts(md$environment) <- contr.treatment(3) - matrix(rep(1/3, 6), ncol=2)

# view contrasts ...

contrasts(md$dependency)
contrasts(md$environment)

#------------------------------------------------------------------------------#
# + + model 1 
#------------------------------------------------------------------------------#

# fit model ...

tic()
model1 <- lmer(rrt ~ dependency * environment + 
                 (1 + dependency * environment | participant) + 
                 (1 + dependency * environment | item), 
               data = md,
               control = lmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_doen_rrt_region1_md1.rds')
summary(model1)
toc()
beep()

# 458.82 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error      df t value Pr(>|t|)    
# (Intercept)                -4.554      5.400  61.043  -0.843  0.40232    
# environment2               -1.330      8.006  47.834  -0.166  0.86871    
# environment3                2.495      7.542  65.795   0.331  0.74184    
# dependency2               -20.398      6.434  34.017  -3.170  0.00322 ** 
# environment2:dependency2  -15.922     16.526  36.885  -0.963  0.34159    
# environment3:dependency2  -52.934     15.530  89.659  -3.408  0.00098 ***

#------------------------------------------------------------------------------#
# + + model 4
#------------------------------------------------------------------------------#

# fit model ...

tic()
model4 <- lmer(rrt ~ dependency * environment + 
                 (1 + dependency + environment | participant) + 
                 (1 + dependency + environment | item), 
               data = md,
               control = lmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_doen_rrt_region1_md4.rds')
summary(model4)
toc()
beep()

# 20.61 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error       df t value Pr(>|t|)    
# (Intercept)                -4.540      5.385   60.762  -0.843 0.402529    
# environment2               -1.627      7.955   47.956  -0.205 0.838783    
# environment3                2.235      7.410  117.755   0.302 0.763445    
# dependency2               -20.399      6.231   60.674  -3.274 0.001755 ** 
# environment2:dependency2  -16.102     14.086 2261.507  -1.143 0.253103    
# environment3:dependency2  -53.650     14.039 2248.594  -3.822 0.000136 ***

anova(model1, model4, refit = FALSE)

# npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)
# model4   27 33020 33178 -16483    32966                     
# model1   49 33050 33337 -16476    32952 14.468 22     0.8841

#------------------------------------------------------------------------------#
# + + model 5
#------------------------------------------------------------------------------#

# fit model ...

tic()
model5 <- lmer(rrt ~ dependency * environment + 
                 (1 + dependency * environment | participant) + 
                 (1 | item), 
               data = md,
               control = lmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_doen_rrt_region1_md5.rds')
summary(model5)
toc()
beep()

# 1.77 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error       df t value Pr(>|t|)    
# (Intercept)                -4.606      5.381   60.788  -0.856 0.395372    
# environment2               -1.590      7.569  113.184  -0.210 0.833964    
# environment3                2.287      7.284  341.029   0.314 0.753784    
# dependency2               -20.548      6.078   84.402  -3.381 0.001096 ** 
# environment2:dependency2  -16.125     14.119 2310.146  -1.142 0.253553    
# environment3:dependency2  -53.735     14.065 2277.614  -3.820 0.000137 ***

anova(model4, model5, refit = FALSE)

# npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)
# model5   18 33004 33109 -16484    32968                     
# model4   27 33020 33178 -16483    32966 1.6611  9     0.9958

#------------------------------------------------------------------------------#
# + + model 6
#------------------------------------------------------------------------------#

# fit model ...

tic()
model6 <- lmer(rrt ~ dependency * environment + 
                 (1 | participant) + 
                 (1 | item), 
               data = md,
               control = lmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_doen_rrt_region1_md6.rds')
summary(model6)
toc()
beep()

# 0.93 sec elapsed
# no warnings
# Fixed effects:
#   Estimate Std. Error       df t value Pr(>|t|)    
# (Intercept)                -4.587      5.345   60.588  -0.858 0.394184    
# environment2               -1.842      7.089 2456.482  -0.260 0.795014    
# environment3                2.283      7.094 2455.299   0.322 0.747656    
# dependency2               -20.595      5.790 2459.133  -3.557 0.000382 ***
# environment2:dependency2  -15.901     14.232 2477.292  -1.117 0.263974    
# environment3:dependency2  -53.630     14.182 2450.991  -3.781 0.000160 ***
  
anova(model5, model6, refit = FALSE)

# npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)
# model6    9 32993 33046 -16488    32975                     
# model5   18 33004 33109 -16484    32968 7.2218  9      0.614

#------------------------------------------------------------------------------#
# + + model 7
#------------------------------------------------------------------------------#

# fit model ...

tic()
model7 <- lmer(rrt ~ dependency * environment + 
                 (1 + dependency | participant) + 
                 (1 | item), 
               data = md,
               control = lmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_doen_rrt_region1_md7.rds')
summary(model7)
toc()
beep()

# 1.28 sec elapsed
# Fixed effects:
#   Estimate Std. Error       df t value Pr(>|t|)    
# (Intercept)                -4.592      5.350   60.676  -0.858 0.394114    
# environment2               -1.861      7.076 2371.340  -0.263 0.792524    
# environment3                2.299      7.081 2371.155   0.325 0.745453    
# dependency2               -20.599      6.073   86.110  -3.392 0.001051 ** 
# environment2:dependency2  -15.879     14.206 2394.489  -1.118 0.263793    
# environment3:dependency2  -53.589     14.156 2364.211  -3.786 0.000157 ***

anova(model6, model7, refit = FALSE)

# npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)
# model6    9 32993 33046 -16488    32975                     
# model7   11 32996 33061 -16487    32974 0.8463  2      0.655

#------------------------------------------------------------------------------#
# + + model 8
#------------------------------------------------------------------------------#

# fit model ...

tic()
model8 <- lmer(rrt ~ dependency * environment + 
                 (1 + environment | participant) + 
                 (1 | item), 
               data = md,
               control = lmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_doen_rrt_region1_md8.rds')
summary(model8)
toc()
beep()

# 0.91 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error       df t value Pr(>|t|)    
# (Intercept)                -4.602      5.373   60.668  -0.856  0.39515    
# environment2               -1.569      7.585  117.078  -0.207  0.83652    
# environment3                2.272      7.294  357.364   0.312  0.75558    
# dependency2               -20.565      5.756 2375.724  -3.573  0.00036 ***
# environment2:dependency2  -16.141     14.150 2393.763  -1.141  0.25410    
# environment3:dependency2  -53.770     14.096 2364.992  -3.815  0.00014 ***

anova(model6, model8, refit = FALSE)

# npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)
# model6    9 32993 33046 -16488    32975                     
# model8   14 32997 33079 -16485    32969 6.2291  5     0.2846

#------------------------------------------------------------------------------#
# + + model 9 (final) ----
#------------------------------------------------------------------------------#

# fit model ...

tic()
model9 <- lmer(rrt ~ dependency * environment + 
                 (1 + dependency | participant) + 
                 (1 + dependency | item), 
               data = md,
               control = lmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_doen_rrt_region1_md9.rds')
summary(model9)
toc()
beep()

# 1.12 sec elapsed
# no warnings
# Fixed effects:
#   Estimate Std. Error       df t value Pr(>|t|)    
# (Intercept)                -4.576      5.353   60.619  -0.855 0.395971    
# environment2               -1.887      7.075 2359.733  -0.267 0.789731    
# environment3                2.255      7.079 2348.568   0.319 0.750102    
# dependency2               -20.601      6.160   24.744  -3.344 0.002628 ** 
# environment2:dependency2  -15.852     14.202 2380.120  -1.116 0.264462    
# environment3:dependency2  -53.597     14.154 2350.774  -3.787 0.000156 ***

# compare models ...

anova(model6, model9, refit = FALSE)

# npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)
# model6    9 32993 33046 -16488    32975                     
# model9   13 33000 33076 -16487    32974 1.2587  4     0.8683

# post-hoc tests ...

pairwise <- model9 %>%
  emmeans(~ dependency * environment) %>%
  contrast('pairwise', simple = 'each', combine = TRUE) %>%
  summary(by = NULL, adjust = 'holm')
pairwise

# environment dependency contrast       estimate    SE   df t.ratio p.value
# short       .          gap - pronoun     -2.55 10.27  188  -0.248  1.0000
# long        .          gap - pronoun     13.30 10.24  185   1.299  0.7823
# island      .          gap - pronoun     51.05 10.26  186   4.977  <.0001
# .           gap        short - long      -6.04 10.03 2365  -0.602  1.0000
# .           gap        short - island   -29.05  9.99 2347  -2.908  0.0294
# .           gap        long - island    -23.01 10.04 2365  -2.292  0.1319
# .           pronoun    short - long       9.81 10.05 2353   0.976  0.9871
# .           pronoun    short - island    24.54 10.04 2350   2.446  0.1017
# .           pronoun    long - island     14.73 10.03 2359   1.468  0.7108
# Degrees-of-freedom method: kenward-roger 
# P value adjustment: holm method for 9 tests

# tables ...

model9 %>%
  tidy() %>%
  kbl(digits = c(2, 2, 2, 2, 3)) %>%
  remove_column(6)

pairwise %>%
  kbl(digits = c(2, 2, 2, 3)) %>%
  remove_column(6)

#------------------------------------------------------------------------------#
# + doen - region 2 ----
#------------------------------------------------------------------------------#

# filter data for analysis

md <- temp %>%
  filter(region2 == 2,
         study == '210510_do',
         group == 'english')

# apply deviation coding ...

contrasts(md$dependency) <- contr.treatment(2) - matrix(rep(1/2, 2), ncol=1)
contrasts(md$environment) <- contr.treatment(3) - matrix(rep(1/3, 6), ncol=2)

# view contrasts ...

contrasts(md$dependency)
contrasts(md$environment)

#------------------------------------------------------------------------------#
# + + model 1
#------------------------------------------------------------------------------#

# fit model ...

tic()
model1 <- lmer(rrt ~ dependency * environment + 
                 (1 + dependency * environment | participant) + 
                 (1 + dependency * environment | item), 
               data = md,
               control = lmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_doen_rrt_region2_md1.rds')
summary(model1)
toc()
beep()

# 198.35 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error      df t value Pr(>|t|)    
# (Intercept)                28.380      5.720  89.445   4.961 3.32e-06 ***
# dependency2                 2.320      4.956  36.598   0.468 0.642489    
# environment2              -10.263      7.007  42.335  -1.465 0.150402    
# environment3                2.007      6.890  35.534   0.291 0.772513    
# dependency2:environment2  -16.575     12.875  43.034  -1.287 0.204864    
# dependency2:environment3  -50.503     14.281  55.670  -3.536 0.000826 ***

# old analysis ...
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#                                     Estimate Std. Error      df t value Pr(>|t|)    
# (Intercept)                           18.909      7.447  82.527   2.539 0.012985 *  
# dependencypronoun                     24.560      9.479  68.194   2.591 0.011697 *  
# environmentlong                       -1.983      7.723 814.992  -0.257 0.797429    
# environmentisland                     26.983      9.589  47.803   2.814 0.007086 ** 
# environmentlong:dependencypronoun    -16.569     13.173  36.622  -1.258 0.216433    
# environmentisland:dependencypronoun  -50.294     14.149  73.771  -3.555 0.000665 ***

#------------------------------------------------------------------------------#
# + + model 6 (final) ----
#------------------------------------------------------------------------------#

# fit model ...

tic()
model6 <- lmer(rrt ~ dependency * environment + 
                 (1 | participant) + 
                 (1 | item), 
               data = md,
               control = lmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_doen_rrt_region2_md6.rds')
summary(model6)
toc()
beep()

# 0.94 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error       df t value Pr(>|t|)    
# (Intercept)                28.211      5.705   89.248   4.945 3.55e-06 ***
# dependency2                 2.098      4.527 2422.007   0.463   0.6431    
# environment2              -10.443      5.536 2419.827  -1.886   0.0594 .  
# environment3                1.584      5.547 2419.695   0.286   0.7752    
# dependency2:environment2  -17.188     11.073 2419.815  -1.552   0.1207    
# dependency2:environment3  -50.268     11.095 2420.125  -4.531 6.16e-06 ***

# compare models ...

anova(model1, model6, refit = FALSE)

# post-hoc tests ...

pairwise <- model6 %>%
  emmeans(~ dependency * environment) %>%
  contrast('pairwise', simple = 'each', combine = TRUE) %>%
  summary(by = NULL, adjust = 'holm')
pairwise

# environment dependency contrast       estimate   SE   df t.ratio p.value
# short       .          gap - pronoun    -24.58 7.85 2420  -3.131  0.0106 *
# long        .          gap - pronoun     -7.40 7.82 2420  -0.945  1.0000
# island      .          gap - pronoun     25.68 7.86 2421   3.270  0.0076 **
# .           gap        short - long       1.85 7.84 2420   0.236  1.0000
# .           gap        short - island   -26.72 7.81 2401  -3.423  0.0050 **
# .           gap        long - island    -28.57 7.80 2420  -3.661  0.0023 **
# .           pronoun    short - long      19.04 7.83 2419   2.430  0.0607 .
# .           pronoun    short - island    23.55 7.89 2398   2.986  0.0143 *
# .           pronoun    long - island      4.51 7.87 2420   0.573  1.0000
# Degrees-of-freedom method: kenward-roger 
# P value adjustment: holm method for 9 tests 

#------------------------------------------------------------------------------#
# + + model 9
#------------------------------------------------------------------------------#

# fit model ...

tic()
model9 <- lmer(rrt ~ dependency * environment + 
                 (1 + dependency | participant) + 
                 (1 + dependency | item), 
               data = md,
               control = lmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_doen_rrt_region2_md9.rds')
summary(model9)
toc()
beep()

# 1.22 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error       df t value Pr(>|t|)    
# (Intercept)                28.199      5.706   89.209   4.942 3.59e-06 ***
# dependency2                 2.060      4.571 1096.704   0.451   0.6523    
# environment2              -10.434      5.534 2419.797  -1.885   0.0595 .  
# environment3                1.584      5.545 2419.694   0.286   0.7751    
# dependency2:environment2  -17.149     11.069 2419.791  -1.549   0.1214    
# dependency2:environment3  -50.313     11.091 2420.110  -4.536 6.00e-06 ***

anova(model6, model9, refit = FALSE)

# npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)
# model6    9 31049 31101 -15515    31031                     
# model9   13 31055 31131 -15514    31029 1.5229  4     0.8226

#------------------------------------------------------------------------------#
# + doen - region 3 ----
#------------------------------------------------------------------------------#

# filter data for analysis

md <- temp %>%
  filter(region2 == 3,
         study == '210510_do',
         group == 'english')

# apply deviation coding ...

contrasts(md$dependency) <- contr.treatment(2) - matrix(rep(1/2, 2), ncol=1)
contrasts(md$environment) <- contr.treatment(3) - matrix(rep(1/3, 6), ncol=2)

# view contrasts ...

contrasts(md$dependency)
contrasts(md$environment)

#------------------------------------------------------------------------------#
# + + model 1 
#------------------------------------------------------------------------------#

# fit model ...

tic()
model1 <- lmer(rrt ~ dependency * environment + 
                 (1 + dependency * environment | participant) + 
                 (1 + dependency * environment | item), 
               data = md,
               control = lmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_doen_rrt_region3_md1.rds')
summary(model1)
toc()
beep()

model1 <- read_rds('models/spr_doen_rrt_region3_md1.rds')

# 246.92 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error       df t value Pr(>|t|)   
# (Intercept)               22.9638     7.9107  84.6076   2.903  0.00471 **
# dependency2                7.7857     4.8435  58.1534   1.607  0.11337   
# environment2              -4.1610     5.7386  74.5713  -0.725  0.47067   
# environment3               4.6485     5.7947  52.6953   0.802  0.42604   
# dependency2:environment2   0.6994    11.9057  38.2059   0.059  0.95346   
# dependency2:environment3 -16.4908    12.7946  34.1513  -1.289  0.20610 

# old analysis ...
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error       df t value Pr(>|t|)  
# (Intercept)                          16.2765     8.8078  68.0970   1.848   0.0690 .
# dependencypronoun                    13.0496     7.5429  86.1457   1.730   0.0872 .
# environmentlong                      -4.5110     7.6742  67.6572  -0.588   0.5586  
# environmentisland                    12.8944     7.7875  63.8697   1.656   0.1027  
# environmentlong:dependencypronoun     0.6997    11.9051  38.2415   0.059   0.9534  
# environmentisland:dependencypronoun -16.4919    12.7929  34.1701  -1.289   0.2060 

#------------------------------------------------------------------------------#
# + + model 2
#------------------------------------------------------------------------------#

# fit model ...

tic()
model2 <- lmer(rrt ~ dependency * environment + 
                 (1 + dependency * environment | participant) + 
                 (1 + dependency + environment | item), 
               data = md,
               control = lmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_doen_rrt_region3_md2.rds')
summary(model2)
toc()
beep()

# 52.08 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error       df t value Pr(>|t|)   
# (Intercept)               23.0377     7.8870  85.0545   2.921  0.00447 **
# dependency2                7.5110     4.6370  75.3213   1.620  0.10946   
# environment2              -4.1894     5.6590  76.6316  -0.740  0.46138   
# environment3               4.1723     5.5098  37.8782   0.757  0.45358   
# dependency2:environment2   0.2098    10.5504 229.1755   0.020  0.98415   
# dependency2:environment3 -16.5583    10.3624 293.4460  -1.598  0.11113 

# compare models ...

anova(model1, model2, refit = FALSE)

# npar   AIC   BIC logLik deviance Chisq Df Pr(>Chisq)
# model2   38 29872 30092 -14898    29796                    
# model1   49 29885 30169 -14894    29787 8.901 11      0.631

#------------------------------------------------------------------------------#
# + + model 3
#------------------------------------------------------------------------------#

# fit model ...

tic()
model3 <- lmer(rrt ~ dependency * environment + 
                 (1 + dependency * environment | participant) + 
                 (1 + dependency | item), 
               data = md,
               control = lmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_doen_rrt_region3_md3.rds')
summary(model3)
toc()
beep()

# 14.59 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error        df t value Pr(>|t|)   
# (Intercept)               23.01053    7.88478  85.28289   2.918   0.0045 **
# dependency2                7.58828    4.61667  96.35785   1.644   0.1035   
# environment2              -4.16175    5.55564 112.35751  -0.749   0.4554   
# environment3               4.16811    5.38310 127.47759   0.774   0.4402   
# dependency2:environment2   0.01598   10.56116 230.40827   0.002   0.9988   
# dependency2:environment3 -16.77989   10.35566 297.44665  -1.620   0.1062

# compare models ...

anova(model2, model3, refit = FALSE)

# npar   AIC   BIC logLik deviance Chisq Df Pr(>Chisq)
# model3   31 29859 30039 -14899    29797                    
# model2   38 29872 30092 -14898    29796 1.172  7     0.9916

#------------------------------------------------------------------------------#
# + + model 4
#------------------------------------------------------------------------------#

# fit model ...

tic()
model4 <- lmer(rrt ~ dependency * environment + 
                 (1 + dependency + environment | participant) + 
                 (1 + dependency | item), 
               data = md,
               control = lmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_doen_rrt_region3_md4.rds')
summary(model4)
toc()
beep()

# 3.12 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error        df t value Pr(>|t|)   
# (Intercept)                22.9624     7.8898   85.3350   2.910   0.0046 **
# dependency2                 7.4399     4.5714   88.8058   1.627   0.1072   
# environment2               -4.2441     5.5662  103.7851  -0.762   0.4475   
# environment3                4.1137     5.3104  108.2315   0.775   0.4402   
# dependency2:environment2    0.2625    10.1901 2186.0509   0.026   0.9794   
# dependency2:environment3  -17.2382    10.1056 2163.2540  -1.706   0.0882 .

#------------------------------------------------------------------------------#
# + + model 5
#------------------------------------------------------------------------------#

# fit model ...

tic()
model5 <- lmer(rrt ~ dependency * environment + 
                 (1 + dependency | participant) + 
                 (1 + dependency | item), 
               data = md,
               control = lmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_doen_rrt_region3_md5.rds')
summary(model5)
toc()
beep()

# 0.96 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error        df t value Pr(>|t|)   
# (Intercept)                22.9504     7.8813   85.3590   2.912  0.00458 **
# dependency2                 7.2125     4.4799   77.2275   1.610  0.11148   
# environment2               -4.5516     5.0801 2246.4780  -0.896  0.37037   
# environment3                3.8969     5.0848 2250.4352   0.766  0.44353   
# dependency2:environment2    0.3341    10.2404 2272.2180   0.033  0.97398   
# dependency2:environment3  -17.2188    10.1572 2248.3213  -1.695  0.09017 . 

# compare models ...

anova(model4, model5, refit = FALSE)

# npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)
# model5   13 29832 29908 -14903    29806                     
# model4   20 29842 29958 -14901    29802 4.1291  7     0.7648

#------------------------------------------------------------------------------#
# + + model 6
#------------------------------------------------------------------------------#

# fit model ...

tic()
model6 <- lmer(rrt ~ dependency * environment + 
                 (1 | participant) + 
                 (1 | item), 
               data = md,
               control = lmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_doen_rrt_region3_md6.rds')
summary(model6)
toc()
beep()

# 0.7 sec elapsed
# no warnings
# Fixed effects:
#   Estimate Std. Error        df t value Pr(>|t|)   
# (Intercept)                22.9031     7.8853   85.3382   2.905  0.00468 **
# dependency2                 7.1470     4.1663 2328.4621   1.715  0.08640 . 
# environment2               -4.5579     5.0926 2329.5734  -0.895  0.37088   
# environment3                3.8827     5.0974 2329.0999   0.762  0.44631   
# dependency2:environment2    0.1328    10.2638 2339.2031   0.013  0.98968   
# dependency2:environment3  -17.2922    10.1823 2327.1878  -1.698  0.08959 . 

# compare models ...

anova(model5, model6)

# npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)
# model6    9 29859 29911 -14921    29841                     
# model5   13 29866 29941 -14920    29840 1.6613  4     0.7977

#------------------------------------------------------------------------------#
# + + model 7 (final) ----
#------------------------------------------------------------------------------#

# fit model ...

tic()
model7 <- lmer(rrt ~ dependency * environment + 
                 (1 + dependency | participant) + 
                 (1 | item), 
               data = md,
               control = lmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_doen_rrt_region3_md7.rds')
summary(model7)
toc()
beep()

# 0.7 sec elapsed
# no warnings
# Fixed effects:
#   Estimate Std. Error        df t value Pr(>|t|)   
# (Intercept)                22.9262     7.8837   85.4043   2.908  0.00463 **
# dependency2                 7.2026     4.4323   82.9701   1.625  0.10795   
# environment2               -4.5278     5.0803 2246.5497  -0.891  0.37290   
# environment3                3.9020     5.0855 2250.3916   0.767  0.44300   
# dependency2:environment2    0.3245    10.2416 2272.1595   0.032  0.97472   
# dependency2:environment3  -17.2573    10.1585 2248.2642  -1.699  0.08950 . 

# compare models ...

anova(model6, model7, refit = FALSE)

# npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)
# model6    9 29826 29878 -14904    29808                     
# model7   11 29829 29893 -14903    29807 1.1094  2     0.5742

#------------------------------------------------------------------------------#
# + + model 8
#------------------------------------------------------------------------------#

# fit model ...

tic()
model8 <- lmer(rrt ~ dependency * environment + 
                 (1 + dependency + environment | participant) + 
                 (1 | item), 
               data = md,
               control = lmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_doen_rrt_region3_md8.rds')
summary(model8)
toc()
beep()

# boundary (singular) fit: see help('isSingular')

anova(model7, model8, refit = FALSE)

# npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)
# model7   11 29829 29893 -14903    29807                     
# model8   18 29839 29943 -14901    29803 4.1216  7     0.7657

#------------------------------------------------------------------------------#
# + doko - region 1 ----
#------------------------------------------------------------------------------#

# filter data for analysis

md <- temp %>%
  filter(region2 == 1,
         study == '210510_do',
         group == 'korean')

# apply deviation coding ...

contrasts(md$dependency) <- contr.treatment(2) - matrix(rep(1/2, 2), ncol=1)
contrasts(md$environment) <- contr.treatment(3) - matrix(rep(1/3, 6), ncol=2)

# view contrasts ...

contrasts(md$dependency)
contrasts(md$environment)

#------------------------------------------------------------------------------#
# + + model 1 
#------------------------------------------------------------------------------#

# fit model ...

tic()
model1 <- lmer(rrt ~ dependency * environment + 
                 (1 + dependency * environment | participant) + 
                 (1 + dependency * environment | item), 
               data = md,
               control = lmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_doko_rrt_region1_md1.rds')
summary(model1)
toc()
beep()

model1 <- read_rds('models/spr_doko_rrt_region1_md1.rds')

# 197.13 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error      df t value Pr(>|t|)   
# (Intercept)               -35.009     11.997  68.162  -2.918  0.00477 **
# dependency2               -19.048     14.226  45.860  -1.339  0.18718   
# environment2                3.917     15.431  33.101   0.254  0.80119   
# environment3                9.767     13.660  54.713   0.715  0.47766   
# dependency2:environment2   12.443     25.882  90.725   0.481  0.63185   
# dependency2:environment3   31.640     31.101  31.951   1.017  0.31665 

# old anlaysis ...
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error      df t value Pr(>|t|)
# (Intercept)                          -22.700     17.960  46.022  -1.264    0.213
# dependencypronoun                    -33.742     22.493  36.054  -1.500    0.142
# environmentlong                       -2.302     20.502  42.972  -0.112    0.911
# environmentisland                     -6.051     22.236  31.049  -0.272    0.787
# environmentlong:dependencypronoun     12.442     25.884  90.654   0.481    0.632
# environmentisland:dependencypronoun   31.638     31.099  31.953   1.017    0.317

#------------------------------------------------------------------------------#
# + + model 5
#------------------------------------------------------------------------------#

# fit model ...

tic()
model5 <- lmer(rrt ~ dependency * environment + 
                 (1 + dependency + environment | participant) + 
                 (1 | item), 
               data = md,
               control = lmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_doko_rrt_region1_md5.rds')
summary(model5)
toc()
beep()

# 1.49 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error       df t value Pr(>|t|)   
# (Intercept)               -35.160     11.674   62.825  -3.012  0.00374 **
# dependency2               -20.346     12.465   75.960  -1.632  0.10678   
# environment2                6.477     13.751   64.635   0.471  0.63923   
# environment3               11.339     13.245  104.736   0.856  0.39390   
# dependency2:environment2   12.213     24.683 1709.374   0.495  0.62080   
# dependency2:environment3   29.551     24.545 1695.812   1.204  0.22878

anova(model5, model8, refit = FALSE)

# npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)
# model8   13 26081 26153 -13027    26055                     
# model5   18 26084 26184 -13024    26048 6.6299  5     0.2496

#------------------------------------------------------------------------------#
# + + model 6
#------------------------------------------------------------------------------#

# fit model ...

tic()
model6 <- lmer(rrt ~ dependency * environment + 
                 (1 | participant) + 
                 (1 | item), 
               data = md,
               control = lmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_doko_rrt_region1_md6.rds')
summary(model6)
toc()
beep()

# 0.76 sec elapsed
# no warnings
# Fixed effects:
#   Estimate Std. Error       df t value Pr(>|t|)   
# (Intercept)               -35.280     11.701   62.718  -3.015  0.00371 **
# dependency2               -19.662     10.222 1823.693  -1.923  0.05458 . 
# environment2                6.222     12.540 1825.311   0.496  0.61982   
# environment3               11.542     12.500 1825.019   0.923  0.35594   
# dependency2:environment2   12.725     25.134 1830.808   0.506  0.61271   
# dependency2:environment3   30.040     24.998 1815.869   1.202  0.22965 

anova(model1, model6, refit = FALSE)

# npar   AIC   BIC logLik deviance Chisq Df Pr(>Chisq)
# model6    9 26086 26136 -13034    26068                    
# model1   49 26127 26399 -13014    26029 38.93 40     0.5183

#------------------------------------------------------------------------------#
# + + model 7
#------------------------------------------------------------------------------#

# fit model ...

tic()
model7 <- lmer(rrt ~ dependency * environment + 
                 (1 + dependency | participant) + 
                 (1 | item), 
               data = md,
               control = lmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_doko_rrt_region1_md7.rds')
summary(model7)
toc()
beep()

# 0.75 sec elapsed
# no warnings
# Fixed effects:
#   Estimate Std. Error       df t value Pr(>|t|)   
# (Intercept)               -35.334     11.693   62.829  -3.022  0.00363 **
# dependency2               -20.115     12.008   63.928  -1.675  0.09880 . 
# environment2                5.977     12.456 1762.890   0.480  0.63139   
# environment3               11.585     12.416 1762.828   0.933  0.35092   
# dependency2:environment2   12.150     24.965 1768.577   0.487  0.62654   
# dependency2:environment3   29.717     24.827 1751.635   1.197  0.23150 

anova(model6, model7, refit = FALSE)

# npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)  
# model6    9 26086 26136 -13034    26068                       
# model7   11 26082 26143 -13030    26060 7.9287  2    0.01898 *

#------------------------------------------------------------------------------#
# + + model 8 (final) ----
#------------------------------------------------------------------------------#

# fit model ...

tic()
model8 <- lmer(rrt ~ dependency * environment + 
                 (1 + dependency | participant) + 
                 (1 + dependency | item), 
               data = md,
               control = lmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_doko_rrt_region1_md8.rds')
summary(model8)
toc()
beep()

# 0.75 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error      df t value Pr(>|t|)   
# (Intercept)                -35.06      11.89   67.14  -2.949  0.00438 **
# dependency2                -20.61      13.58   40.24  -1.518  0.13687   
# environment2                 5.10      12.44 1761.69   0.410  0.68193   
# environment3                11.69      12.36 1753.33   0.945  0.34464   
# dependency2:environment2    13.33      24.90 1763.64   0.535  0.59250   
# dependency2:environment3    29.32      24.73 1751.26   1.185  0.23598

anova(model7, model8, refit = FALSE)

# npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)  
# model7   11 26082 26143 -13030    26060                       
# model8   13 26081 26153 -13027    26055 5.0919  2     0.0784 .

#------------------------------------------------------------------------------#
# + + model 9
#------------------------------------------------------------------------------#

# fit model ...

tic()
model9 <- lmer(rrt ~ dependency * environment + 
                 (1 + dependency + environment | participant) + 
                 (1 + dependency | item), 
               data = md,
               control = lmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_doko_rrt_region1_md9.rds')
summary(model9)
toc()
beep()

# 2.84 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error       df t value Pr(>|t|)   
# (Intercept)               -35.033     11.434   65.227  -3.064  0.00317 **
# dependency2               -20.289     12.913   34.862  -1.571  0.12517   
# environment2                5.918     13.694   63.482   0.432  0.66710   
# environment3               11.412     13.279  103.579   0.859  0.39209   
# dependency2:environment2   11.686     24.659 1711.477   0.474  0.63563   
# dependency2:environment3   29.328     24.572 1710.018   1.194  0.23282 

anova(model8, model9, refit = FALSE)

# npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)
# model8   13 26081 26153 -13027    26055                     
# model9   20 26089 26200 -13024    26049 6.2309  7     0.5131

#------------------------------------------------------------------------------#
# + doko - region 2 ----
#------------------------------------------------------------------------------#

# filter data for analysis

md <- temp %>%
  filter(region2 == 2,
         study == '210510_do',
         group == 'korean')

# apply deviation coding ...

contrasts(md$dependency) <- contr.treatment(2) - matrix(rep(1/2, 2), ncol=1)
contrasts(md$environment) <- contr.treatment(3) - matrix(rep(1/3, 6), ncol=2)

# view contrasts ...

contrasts(md$dependency)
contrasts(md$environment)

#------------------------------------------------------------------------------#
# + + model 1 
#------------------------------------------------------------------------------#

# fit model ...

tic()
model1 <- lmer(rrt ~ dependency * environment + 
                 (1 + dependency * environment | participant) + 
                 (1 + dependency * environment | item), 
               data = md,
               control = lmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_doko_rrt_region2_md1.rds')
summary(model1)
toc()
beep()

# boundary (singular) fit: see help('isSingular')
# Warning message:
#   Model failed to converge with 2 negative eigenvalues: -5.8e+00 -6.8e+02 

# old analysis ...
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error     df t value Pr(>|t|)  
# (Intercept)                           -40.71      17.18  68.23  -2.370   0.0206 *
# dependencypronoun                     -19.45      14.52  95.38  -1.340   0.1834  
# environmentlong                        35.00      17.41  50.39   2.010   0.0498 *
# environmentisland                      28.40      20.58  61.53   1.380   0.1726  
# environmentlong:dependencypronoun     -46.21      23.52  47.88  -1.965   0.0553 .
# environmentisland:dependencypronoun   -20.75      25.09  55.81  -0.827   0.4118  

#------------------------------------------------------------------------------#
# + doko - region 3 ----
#------------------------------------------------------------------------------#

# filter data for analysis

md <- temp %>%
  filter(region2 == 3,
         study == '210510_do',
         group == 'korean')

# apply deviation coding ...

contrasts(md$dependency) <- contr.treatment(2) - matrix(rep(1/2, 2), ncol=1)
contrasts(md$environment) <- contr.treatment(3) - matrix(rep(1/3, 6), ncol=2)

# view contrasts ...

contrasts(md$dependency)
contrasts(md$environment)

#------------------------------------------------------------------------------#
# + + model 1 
#------------------------------------------------------------------------------#

# fit model ...

tic()
model1 <- lmer(rrt ~ dependency * environment + 
                 (1 + dependency * environment | participant) + 
                 (1 + dependency * environment | item), 
               data = md,
               control = lmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_doko_rrt_region3_md1.rds')
summary(model1)
toc()
beep()

# 190.71 sec elapsed
# boundary (singular) fit: see help('isSingular')
# Warning message:
#   Model failed to converge with 1 negative eigenvalue: -5.2e-02 

# old analysis ...
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error      df t value Pr(>|t|)
# (Intercept)                          -25.335     18.481  57.836  -1.371    0.176
# dependencypronoun                    -30.901     18.613  43.521  -1.660    0.104
# environmentlong                      -20.596     16.682  50.175  -1.235    0.223
# environmentisland                      3.596     18.830  29.555   0.191    0.850
# environmentlong:dependencypronoun      5.751     23.048  96.870   0.250    0.803
# environmentisland:dependencypronoun    8.417     25.537  30.334   0.330    0.744

#------------------------------------------------------------------------------#
# + dozh - region 1 ----
#------------------------------------------------------------------------------#

# filter data for analysis

md <- temp %>%
  filter(region2 == 1,
         study == '210510_do',
         group == 'mandarin')

# apply deviation coding ...

contrasts(md$dependency) <- contr.treatment(2) - matrix(rep(1/2, 2), ncol=1)
contrasts(md$environment) <- contr.treatment(3) - matrix(rep(1/3, 6), ncol=2)

# view contrasts ...

contrasts(md$dependency)
contrasts(md$environment)

#------------------------------------------------------------------------------#
# + + model 1 
#------------------------------------------------------------------------------#

# fit model ...

tic()
model1 <- lmer(rrt ~ dependency * environment + 
                 (1 + dependency * environment | participant) + 
                 (1 + dependency * environment | item), 
               data = md,
               control = lmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_dozh_rrt_region1_md1.rds')
summary(model1)
toc()
beep()

# boundary (singular) fit: see help('isSingular')
# Warning message:
#   Model failed to converge with 1 negative eigenvalue: -8.2e+00 

# old analysis ...
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error      df t value Pr(>|t|)  
# (Intercept)                          -12.773     21.019  50.146  -0.608   0.5461  
# dependencypronoun                    -39.771     23.848  45.319  -1.668   0.1023  
# environmentlong                        4.094     24.508  36.950   0.167   0.8683  
# environmentisland                    -15.724     20.665  89.120  -0.761   0.4487  
# environmentlong:dependencypronoun      4.434     29.625  80.336   0.150   0.8814  
# environmentisland:dependencypronoun   54.108     30.646  39.095   1.766   0.0853 .

#------------------------------------------------------------------------------#
# + dozh - region 2 ----
#------------------------------------------------------------------------------#

# filter data for analysis

md <- temp %>%
  filter(region2 == 2,
         study == '210510_do',
         group == 'mandarin')

# apply deviation coding ...

contrasts(md$dependency) <- contr.treatment(2) - matrix(rep(1/2, 2), ncol=1)
contrasts(md$environment) <- contr.treatment(3) - matrix(rep(1/3, 6), ncol=2)

# view contrasts ...

contrasts(md$dependency)
contrasts(md$environment)

#------------------------------------------------------------------------------#
# + + model 1 
#------------------------------------------------------------------------------#

# fit model ...

tic()
model1 <- lmer(rrt ~ dependency * environment + 
                 (1 + dependency * environment | participant) + 
                 (1 + dependency * environment | item), 
               data = md,
               control = lmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_dozh_rrt_region2_md1.rds')
summary(model1)
toc()
beep()

# 187.42 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error     df t value Pr(>|t|)    
# (Intercept)                -46.93      14.40  67.49  -3.260 0.001749 ** 
# dependency2                -52.25      12.96  48.90  -4.030 0.000194 ***
# environment2                12.13      13.10  32.55   0.925 0.361552    
# environment3                22.37      12.72  40.69   1.759 0.086076 .  
# dependency2:environment2   -26.83      24.06  48.99  -1.115 0.270198    
# dependency2:environment3   -22.34      25.85  46.81  -0.864 0.391900 

# old analysis ...
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error     df t value Pr(>|t|)  
# (Intercept)                           -40.50      17.77  58.03  -2.279   0.0264 *
# dependencypronoun                     -35.86      14.94  65.28  -2.400   0.0193 *
# environmentlong                        25.54      20.19  36.88   1.265   0.2138  
# environmentisland                      33.54      21.17  42.22   1.585   0.1205  
# environmentlong:dependencypronoun     -26.83      24.06  48.97  -1.115   0.2702  
# environmentisland:dependencypronoun   -22.34      25.85  47.17  -0.864   0.3919

#------------------------------------------------------------------------------#
# + dozh - region 3 ----
#------------------------------------------------------------------------------#

# filter data for analysis

md <- temp %>%
  filter(region2 == 3,
         study == '210510_do',
         group == 'mandarin')

# apply deviation coding ...

contrasts(md$dependency) <- contr.treatment(2) - matrix(rep(1/2, 2), ncol=1)
contrasts(md$environment) <- contr.treatment(3) - matrix(rep(1/3, 6), ncol=2)

# view contrasts ...

contrasts(md$dependency)
contrasts(md$environment)

#------------------------------------------------------------------------------#
# + + model 1 
#------------------------------------------------------------------------------#

# fit model ...

tic()
model1 <- lmer(rrt ~ dependency * environment + 
                 (1 + dependency * environment | participant) + 
                 (1 + dependency * environment | item), 
               data = md,
               control = lmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_dozh_rrt_region3_md1.rds')
summary(model1)
toc()
beep()

# 240.9 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error      df t value Pr(>|t|)   
# (Intercept)               -32.899     12.360  64.257  -2.662  0.00981 **
# dependency2               -17.326     11.271  32.598  -1.537  0.13388   
# environment2              -11.864     14.313  32.641  -0.829  0.41319   
# environment3               -1.535     15.010  41.403  -0.102  0.91902   
# dependency2:environment2   10.051     25.457  57.165   0.395  0.69444   
# dependency2:environment3    8.030     29.332  39.325   0.274  0.78570 

# old analysis ...
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error      df t value Pr(>|t|)
# (Intercept)                          -16.755     19.898  53.210  -0.842    0.404
# dependencypronoun                    -23.355     20.605  45.019  -1.133    0.263
# environmentlong                      -16.891     21.333  32.707  -0.792    0.434
# environmentisland                     -5.552     24.676  35.726  -0.225    0.823
# environmentlong:dependencypronoun     10.054     25.456  57.158   0.395    0.694
# environmentisland:dependencypronoun    8.032     29.335  39.327   0.274    0.786

#------------------------------------------------------------------------------#
# + suen - region 1 ----
#------------------------------------------------------------------------------#

# filter data for analysis

md <- temp %>%
  filter(region2 == 1,
         study == '210510_su',
         group == 'english')

# apply deviation coding ...

contrasts(md$dependency) <- contr.treatment(2) - matrix(rep(1/2, 2), ncol=1)
contrasts(md$environment) <- contr.treatment(3) - matrix(rep(1/3, 6), ncol=2)

# view contrasts ...

contrasts(md$dependency)
contrasts(md$environment)

#------------------------------------------------------------------------------#
# + + model 1 
#------------------------------------------------------------------------------#

# fit model ...

tic()
model1 <- lmer(rrt ~ dependency * environment + 
                 (1 + dependency * environment | participant) + 
                 (1 + dependency * environment | item), 
               data = md,
               control = lmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_suen_rrt_region1_md1.rds')
summary(model1)
toc()
beep()

# 614.65 sec elapsed
# boundary (singular) fit: see help('isSingular')
# Warning message:
#   Model failed to converge with 1 negative eigenvalue: -9.7e-01 


# old analysis ...
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error       df t value Pr(>|t|)    
# (Intercept)                         -94.4957    11.5507  52.3598  -8.181 6.28e-11 ***
# dependencypronoun                    35.7479    15.8883  35.0287   2.250   0.0308 *  
# environmentlong                       0.4633    15.2077  68.2516   0.030   0.9758    
# environmentisland                    35.4499    17.8622  44.7862   1.985   0.0533 .  
# environmentlong:dependencypronoun    -5.9114    26.0650  33.3015  -0.227   0.8220    
# environmentisland:dependencypronoun -54.1687    27.3279  28.7903  -1.982   0.0571 . 

#------------------------------------------------------------------------------#
# + suen - region 2 ----
#------------------------------------------------------------------------------#

# filter data for analysis

md <- temp %>%
  filter(region2 == 2,
         study == '210510_su',
         group == 'english')

# apply deviation coding ...

contrasts(md$dependency) <- contr.treatment(2) - matrix(rep(1/2, 2), ncol=1)
contrasts(md$environment) <- contr.treatment(3) - matrix(rep(1/3, 6), ncol=2)

# view contrasts ...

contrasts(md$dependency)
contrasts(md$environment)

#------------------------------------------------------------------------------#
# + + model 1 
#------------------------------------------------------------------------------#

# fit model ...

tic()
model1 <- lmer(rrt ~ dependency * environment + 
                 (1 + dependency * environment | participant) + 
                 (1 + dependency * environment | item), 
               data = md,
               control = lmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_suen_rrt_region2_md1.rds')
summary(model1)
toc()
beep()

# 212.79 sec elapsed
# boundary (singular) fit: see help('isSingular')
# Warning message:
#   Model failed to converge with 1 negative eigenvalue: -3.1e+00

# old analysis ...
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error      df t value Pr(>|t|)    
# (Intercept)                           -55.49      12.68   45.73  -4.377 6.93e-05 ***
# dependencypronoun                      29.40      13.26   57.68   2.216   0.0306 *  
# environmentlong                        13.23      18.61   45.12   0.711   0.4807    
# environmentisland                      94.65      12.18  271.83   7.769 1.63e-13 ***
# environmentlong:dependencypronoun     -33.09      19.92   55.13  -1.661   0.1024    
# environmentisland:dependencypronoun  -109.14      19.11   55.37  -5.711 4.60e-07 ***

#------------------------------------------------------------------------------#
# + suen - region 3 ----
#------------------------------------------------------------------------------#

# filter data for analysis

md <- temp %>%
  filter(region2 == 3,
         study == '210510_su',
         group == 'english')

# apply deviation coding ...

contrasts(md$dependency) <- contr.treatment(2) - matrix(rep(1/2, 2), ncol=1)
contrasts(md$environment) <- contr.treatment(3) - matrix(rep(1/3, 6), ncol=2)

# view contrasts ...

contrasts(md$dependency)
contrasts(md$environment)

#------------------------------------------------------------------------------#
# + + model 1 
#------------------------------------------------------------------------------#

# fit model ...

tic()
model1 <- lmer(rrt ~ dependency * environment + 
                 (1 + dependency * environment | participant) + 
                 (1 + dependency * environment | item), 
               data = md,
               control = lmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_suen_rrt_region3_md1.rds')
summary(model1)
toc()
beep()

# 990.86 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error      df t value Pr(>|t|)    
# (Intercept)               -40.763      7.803  30.442  -5.224 1.19e-05 ***
# dependency2               -10.960      9.180  36.342  -1.194   0.2403    
# environment2               12.098      9.507  81.808   1.273   0.2068    
# environment3              150.072     16.099  45.622   9.322 3.92e-12 ***
# dependency2:environment2  -11.907     22.588  32.683  -0.527   0.6017    
# dependency2:environment3  -74.490     22.512  42.946  -3.309   0.0019 ** 

# old analysis ...
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error      df t value Pr(>|t|)    
# (Intercept)                          -103.74      16.55   28.21  -6.267 8.66e-07 ***
# dependencypronoun                      17.83      15.81   34.41   1.128  0.26721    
# environmentlong                        18.05      15.42   32.72   1.170  0.25034    
# environmentisland                     187.31      23.15   45.43   8.090 2.38e-10 ***
# environmentlong:dependencypronoun     -11.90      22.59   32.70  -0.527  0.60200    
# environmentisland:dependencypronoun   -74.48      22.52   42.96  -3.308  0.00191 ** 

#------------------------------------------------------------------------------#
# + suko - region 1 ----
#------------------------------------------------------------------------------#

# filter data for analysis

md <- temp %>%
  filter(region2 == 1,
         study == '210510_su',
         group == 'korean')

# apply deviation coding ...

contrasts(md$dependency) <- contr.treatment(2) - matrix(rep(1/2, 2), ncol=1)
contrasts(md$environment) <- contr.treatment(3) - matrix(rep(1/3, 6), ncol=2)

# view contrasts ...

contrasts(md$dependency)
contrasts(md$environment)

#------------------------------------------------------------------------------#
# + + model 1 
#------------------------------------------------------------------------------#

# fit model ...

tic()
model1 <- lmer(rrt ~ dependency * environment + 
                 (1 + dependency * environment | participant) + 
                 (1 + dependency * environment | item), 
               data = md,
               control = lmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_suko_rrt_region1_md1.rds')
summary(model1)
toc()
beep()

# 216.6 sec elapsed
# boundary (singular) fit: see help('isSingular')
# Warning message:
#   Model failed to converge with 1 negative eigenvalue: -1.2e+00 
# Fixed effects:
#   Estimate Std. Error      df t value Pr(>|t|)
# (Intercept)                -5.007     10.820  32.885  -0.463    0.647
# dependency2               -25.043     20.354  44.004  -1.230    0.225
# environment2              -22.276     27.092  31.605  -0.822    0.417
# environment3               -6.726     28.390  55.290  -0.237    0.814
# dependency2:environment2  -13.735     54.172  53.609  -0.254    0.801
# dependency2:environment3  -12.533     57.014  38.522  -0.220    0.827

# old analysis ...
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error       df t value Pr(>|t|)
# (Intercept)                          11.8752    26.7665  32.1748   0.444    0.660
# dependencypronoun                   -15.8171    32.4809  63.1945  -0.487    0.628
# environmentlong                     -14.9229    39.0395  35.4573  -0.382    0.705
# environmentisland                     0.8691    39.7102  29.9687   0.022    0.983
# environmentlong:dependencypronoun   -14.2447    54.4191  54.4822  -0.262    0.794
# environmentisland:dependencypronoun -13.4231    56.9894  38.8554  -0.236    0.815

#------------------------------------------------------------------------------#
# + suko - region 2 ----
#------------------------------------------------------------------------------#

# filter data for analysis

md <- temp %>%
  filter(region2 == 2,
         study == '210510_su',
         group == 'korean')

# apply deviation coding ...

contrasts(md$dependency) <- contr.treatment(2) - matrix(rep(1/2, 2), ncol=1)
contrasts(md$environment) <- contr.treatment(3) - matrix(rep(1/3, 6), ncol=2)

# view contrasts ...

contrasts(md$dependency)
contrasts(md$environment)

#------------------------------------------------------------------------------#
# + + model 1 
#------------------------------------------------------------------------------#

# fit model ...

tic()
model1 <- lmer(rrt ~ dependency * environment + 
                 (1 + dependency * environment | participant) + 
                 (1 + dependency * environment | item), 
               data = md,
               control = lmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_suko_rrt_region2_md1.rds')
summary(model1)
toc()
beep()

# 2320.28 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Warning message:
#   Model failed to converge with 4 negative eigenvalues: -4.5e-04 -1.2e-02 -7.7e-02 -2.0e+01 
# Fixed effects:
#   Estimate Std. Error       df t value Pr(>|t|)   
# (Intercept)               -10.507      8.314   35.320  -1.264  0.21458   
# dependency2               -45.397     15.178   73.511  -2.991  0.00378 **
# environment2               22.605     22.126   29.705   1.022  0.31520   
# environment3               -3.938     18.916   40.969  -0.208  0.83612   
# dependency2:environment2 -101.738     35.628   49.523  -2.856  0.00626 **
# dependency2:environment3    5.750     40.879   31.342   0.141  0.88904 

# old analysis ...
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error       df t value Pr(>|t|)   
# (Intercept)                           -9.833     19.532   39.301  -0.503  0.61747   
# dependencypronoun                    -12.304     27.666   40.168  -0.445  0.65888   
# environmentlong                       73.007     29.217   34.140   2.499  0.01744 * 
# environmentisland                     -7.263     26.873   36.802  -0.270  0.78847   
# environmentlong:dependencypronoun   -103.034     35.722   47.964  -2.884  0.00586 **
# environmentisland:dependencypronoun    4.768     41.479   32.796   0.115  0.90918  

#------------------------------------------------------------------------------#
# + suko - region 3 ----
#------------------------------------------------------------------------------#

# filter data for analysis

md <- temp %>%
  filter(region2 == 3,
         study == '210510_su',
         group == 'korean')

# apply deviation coding ...

contrasts(md$dependency) <- contr.treatment(2) - matrix(rep(1/2, 2), ncol=1)
contrasts(md$environment) <- contr.treatment(3) - matrix(rep(1/3, 6), ncol=2)

# view contrasts ...

contrasts(md$dependency)
contrasts(md$environment)

#------------------------------------------------------------------------------#
# + + model 1 
#------------------------------------------------------------------------------#

# fit model ...

tic()
model1 <- lmer(rrt ~ dependency * environment + 
                 (1 + dependency * environment | participant) + 
                 (1 + dependency * environment | item), 
               data = md,
               control = lmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_suko_rrt_region3_md1.rds')
summary(model1)
toc()
beep()

# 286.86 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Warning message:
#   Model failed to converge with 2 negative eigenvalues: -2.6e-04 -1.6e-01 
# Fixed effects:
#   Estimate Std. Error       df t value Pr(>|t|)    
# (Intercept)               -20.610      9.010   48.714  -2.288   0.0265 *  
#   dependency2               -13.111     17.063   36.144  -0.768   0.4473    
# environment2                9.972     20.991   36.348   0.475   0.6376    
# environment3               55.198     23.448   48.396   2.354   0.0227 *  
# dependency2:environment2 -101.692     38.788   43.201  -2.622   0.0120 *  
# dependency2:environment3 -140.361     33.998   73.558  -4.128 9.53e-05 ***

# old analysis ...
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error      df t value Pr(>|t|)    
# (Intercept)                           -76.23      18.03   70.73  -4.228 6.94e-05 ***
# dependencypronoun                      67.74      26.50   51.88   2.556 0.013558 *  
# environmentlong                        60.95      29.51   45.10   2.065 0.044701 *  
# environmentisland                     125.22      27.11   62.84   4.619 1.96e-05 ***
# environmentlong:dependencypronoun    -101.79      39.39   47.10  -2.584 0.012927 *  
# environmentisland:dependencypronoun  -140.36      34.38   81.45  -4.083 0.000103 ***

#------------------------------------------------------------------------------#
# + suzh - region 1 ----
#------------------------------------------------------------------------------#

# filter data for analysis

md <- temp %>%
  filter(region2 == 1,
         study == '210510_su',
         group == 'mandarin')

# apply deviation coding ...

contrasts(md$dependency) <- contr.treatment(2) - matrix(rep(1/2, 2), ncol=1)
contrasts(md$environment) <- contr.treatment(3) - matrix(rep(1/3, 6), ncol=2)

# view contrasts ...

contrasts(md$dependency)
contrasts(md$environment)

#------------------------------------------------------------------------------#
# + + model 1 
#------------------------------------------------------------------------------#

# fit model ...

tic()
model1 <- lmer(rrt ~ dependency * environment + 
                 (1 + dependency * environment | participant) + 
                 (1 + dependency * environment | item), 
               data = md,
               control = lmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_suzh_rrt_region1_md1.rds')
summary(model1)
toc()
beep()

# 258.11 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Warning message:
#   Model failed to converge with 2 negative eigenvalues: -6.2e+00 -1.2e+01 
# Fixed effects:
#   Estimate Std. Error       df t value Pr(>|t|)    
# (Intercept)                23.511     14.962   51.104   1.571  0.12226    
# dependency2               -86.848     18.298   72.047  -4.746 1.02e-05 ***
# environment2               -1.652     21.799   85.273  -0.076  0.93976    
# environment3               49.822     21.117  122.146   2.359  0.01990 *  
# dependency2:environment2 -112.241     47.182   30.836  -2.379  0.02374 *  
# dependency2:environment3 -116.427     43.291   65.706  -2.689  0.00906 ** 

# old analysis ...
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error      df t value Pr(>|t|)    
# (Intercept)                            12.81      23.81   45.28   0.538 0.593192    
# dependencypronoun                     -10.62      29.14   63.91  -0.364 0.716848    
# environmentlong                        54.39      35.07   46.38   1.551 0.127767    
# environmentisland                     108.05      31.21   74.53   3.462 0.000892 ***
# environmentlong:dependencypronoun    -112.29      47.51   39.20  -2.364 0.023153 *  
# environmentisland:dependencypronoun  -116.55      43.54   63.17  -2.677 0.009457 ** 

#------------------------------------------------------------------------------#
# + suzh - region 2 ----
#------------------------------------------------------------------------------#

# filter data for analysis

md <- temp %>%
  filter(region2 == 2,
         study == '210510_su',
         group == 'mandarin')

# apply deviation coding ...

contrasts(md$dependency) <- contr.treatment(2) - matrix(rep(1/2, 2), ncol=1)
contrasts(md$environment) <- contr.treatment(3) - matrix(rep(1/3, 6), ncol=2)

# view contrasts ...

contrasts(md$dependency)
contrasts(md$environment)

#------------------------------------------------------------------------------#
# + + model 1 
#------------------------------------------------------------------------------#

# fit model ...

tic()
model1 <- lmer(rrt ~ dependency * environment + 
                 (1 + dependency * environment | participant) + 
                 (1 + dependency * environment | item), 
               data = md,
               control = lmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_suzh_rrt_region2_md1.rds')
summary(model1)
toc()
beep()

# 269.97 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Warning message:
#   Model failed to converge with 2 negative eigenvalues: -1.2e+00 -2.0e+00 
# Fixed effects:
#   Estimate Std. Error      df t value Pr(>|t|)    
# (Intercept)                -26.51      10.78   56.22  -2.459    0.017 *  
# dependency2                -76.70      14.66   75.85  -5.233 1.44e-06 ***
# environment2                82.99      19.35   76.85   4.289 5.17e-05 ***
# environment3                28.28      25.28   53.17   1.119    0.268    
# dependency2:environment2  -176.72      40.23   68.46  -4.393 3.99e-05 ***
# dependency2:environment3   -50.32      34.25  271.56  -1.469    0.143  


# old analysis ...
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)                          -63.1519    19.2731   79.8946  -3.277  0.00156 ** 
# dependencypronoun                     -0.9302    24.0921  367.5248  -0.039  0.96922    
# environmentlong                      171.1295    31.4737   59.9389   5.437 1.05e-06 ***
# environmentisland                     53.4415    32.3608   58.6065   1.651  0.10400    
# environmentlong:dependencypronoun   -176.4626    40.7114   69.6624  -4.334 4.82e-05 ***
# environmentisland:dependencypronoun  -50.2838    34.2306  265.3421  -1.469  0.14303  

#------------------------------------------------------------------------------#
# + suzh - region 3 ----
#------------------------------------------------------------------------------#

# filter data for analysis

md <- temp %>%
  filter(region2 == 3,
         study == '210510_su',
         group == 'mandarin')

# apply deviation coding ...

contrasts(md$dependency) <- contr.treatment(2) - matrix(rep(1/2, 2), ncol=1)
contrasts(md$environment) <- contr.treatment(3) - matrix(rep(1/3, 6), ncol=2)

# view contrasts ...

contrasts(md$dependency)
contrasts(md$environment)

#------------------------------------------------------------------------------#
# + + model 1 
#------------------------------------------------------------------------------#

# fit model ...

tic()
model1 <- lmer(rrt ~ dependency * environment + 
                 (1 + dependency * environment | participant) + 
                 (1 + dependency * environment | item), 
               data = md,
               control = lmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_suzh_rrt_region3_md1.rds')
summary(model1)
toc()
beep()

# 199.67 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error      df t value Pr(>|t|)
# (Intercept)                 2.690     13.045  57.953   0.206    0.837
# dependency2               -26.262     16.755  47.568  -1.567    0.124
# environment2               29.517     24.590  31.078   1.200    0.239
# environment3              -30.667     34.518  61.991  -0.888    0.378
# dependency2:environment2   -3.342     52.106  39.342  -0.064    0.949
# dependency2:environment3  -13.162     40.061  56.108  -0.329    0.744

# old analysis ...
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error      df t value Pr(>|t|)
# (Intercept)                           13.256     33.098  47.181   0.401    0.691
# dependencypronoun                    -20.730     32.301  38.533  -0.642    0.525
# environmentlong                       31.380     37.616  36.211   0.834    0.410
# environmentisland                    -23.822     39.817  55.114  -0.598    0.552
# environmentlong:dependencypronoun     -3.404     52.579  40.549  -0.065    0.949
# environmentisland:dependencypronoun  -13.256     40.077  54.968  -0.331    0.742

#------------------------------------------------------------------------------#
# modeling for accuracy data ----
#------------------------------------------------------------------------------#

# prep dataframe ...

temp <- spr_trim %>%
  group_by(study, group, participant, item, dependency, environment, acc_rate, accuracy) %>%
  summarise() %>%
  ungroup() %>%
  mutate(dependency = fct_drop(dependency),
         environment = fct_drop(environment),
         accuracy = as.logical(accuracy),
         participant = as.factor(participant),
         item = as.factor(item)) %>%
  mutate(environment = fct_relevel(environment, 'short', 'long', 'island'))

# count participants ...

check <- temp %>%
  group_by(study, group, participant) %>%
  summarise() %>%
  ungroup() %>%
  group_by(study, group) %>%
  summarise(n = n()) %>%
  ungroup()

# generate plot ...

plot <- temp %>%
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
  facet_grid(study~group)

#------------------------------------------------------------------------------#
# + doen ----
#------------------------------------------------------------------------------#

# doen = ORC study (do) + ENS group (en)

# filter for modeling ...

md <- temp %>% 
  filter(study == '210510_do', group == 'english')

# count participants ...

check <- md %>%
  group_by(study, group, participant) %>%
  summarise()

# apply deviation coding ...

contrasts(md$dependency) <- contr.treatment(2) - matrix(rep(1/2, 2), ncol=1)
contrasts(md$environment) <- contr.treatment(3) - matrix(rep(1/3, 6), ncol=2)

# view contrasts ...

contrasts(md$dependency)
contrasts(md$environment)

#------------------------------------------------------------------------------#
# + + model 1 (maximal) 
#------------------------------------------------------------------------------#

# fit model ...

tic()
model1 <- glmer(accuracy ~ dependency * environment + 
                  (dependency * environment | participant) + 
                  (dependency * environment | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_doen_acc_md1.rds')
summary(model1)
toc()
beep()

model1 <- read_rds('models/spr_doen_acc_md1.rds')

# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)               4.32597    0.34719  12.460   <2e-16 ***
# dependency2               0.84492    0.76115   1.110    0.267    
# environment2              0.13692    1.02568   0.133    0.894    
# environment3              0.49225    0.90789   0.542    0.588    
# dependency2:environment2  1.11743    2.22829   0.501    0.616    
# dependency2:environment3 -0.04087    1.81099  -0.023    0.982 

#------------------------------------------------------------------------------#
# + + model 3
#------------------------------------------------------------------------------#

tic()
model3 <- glmer(accuracy ~ dependency * environment + 
                  (dependency * environment | participant) + 
                  (dependency + environment | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_doen_acc_md3.rds')
summary(model3)
toc()
beep()

model3 <- read_rds('models/spr_doen_acc_md3.rds')

# 2435.58 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)               3.92513    0.24601  15.955   <2e-16 ***
# dependency2               0.18935    0.40465   0.468    0.640    
# environment2             -0.14796    0.51738  -0.286    0.775    
# environment3              0.17597    0.55967   0.314    0.753    
# dependency2:environment2  0.50081    0.93346   0.537    0.592    
# dependency2:environment3 -0.07302    0.98115  -0.074    0.941 

# compare models ...

anova(model1, model3)

# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model3   37 940.65 1159.0 -433.33   866.65                     
# model1   48 956.88 1240.1 -430.44   860.88 5.7667 11     0.8885

#------------------------------------------------------------------------------#
# + + model 4
#------------------------------------------------------------------------------#

tic()
model4 <- glmer(accuracy ~ dependency * environment + 
                  (dependency + environment | participant) + 
                  (dependency + environment | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_doen_acc_md4.rds')
summary(model4)
toc()
beep()

model4 <- read_rds('models/spr_doen_acc_md4.rds')

# 265.82 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)               3.86811    0.22755  16.999   <2e-16 ***
# dependency2               0.21421    0.33822   0.633    0.527    
# environment2             -0.15598    0.46740  -0.334    0.739    
# environment3              0.09027    0.51528   0.175    0.861    
# dependency2:environment2  0.03805    0.60071   0.063    0.949    
# dependency2:environment3 -0.22013    0.58596  -0.376    0.707 

# compare models ...

anova(model3, model4)

# npar    AIC  BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model4   26 919.52 1073 -433.76   867.52                     
# model3   37 940.65 1159 -433.33   866.65 0.8711 11          1

#------------------------------------------------------------------------------#
# + + model 5
#------------------------------------------------------------------------------#

tic()
model5 <- glmer(accuracy ~ dependency * environment + 
                  (dependency + environment | participant) + 
                  (1 | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_doen_acc_md5.rds')
summary(model5)
toc()
beep()

model5 <- read_rds('models/spr_doen_acc_md5.rds')

# 12.76 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)               3.72472    0.20712  17.984   <2e-16 ***
# dependency2               0.29404    0.31706   0.927    0.354    
# environment2             -0.14602    0.37317  -0.391    0.696    
# environment3              0.01555    0.40119   0.039    0.969    
# dependency2:environment2  0.26222    0.55428   0.473    0.636    
# dependency2:environment3  0.12010    0.59372   0.202    0.840 

# compare models ...

anova(model4, model5)

# npar    AIC    BIC  logLik deviance Chisq Df Pr(>Chisq)
# model5   17 907.46 1007.8 -436.73   873.46                    
# model4   26 919.52 1073.0 -433.76   867.52 5.934  9     0.7465

#------------------------------------------------------------------------------#
# + + model 6 (final) ----
#------------------------------------------------------------------------------#

tic()
model6 <- glmer(accuracy ~ dependency * environment + 
                  (1 | participant) + 
                  (1 | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_doen_acc_md6.rds')
summary(model6)
toc()
beep()

model6 <- read_rds('models/spr_doen_acc_md6.rds')

# 1.91 sec elapsed
# no warnings
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)               3.75380    0.20777  18.067  < 2e-16 ***
# dependency2               0.85728    0.21179   4.048 5.17e-05 ***
# environment2             -0.10776    0.26048  -0.414    0.679    
# environment3             -0.11300    0.25861  -0.437    0.662    
# dependency2:environment2  0.20103    0.52183   0.385    0.700    
# dependency2:environment3  0.05514    0.51836   0.106    0.915 

# compare models ...

anova(model5, model6)

# npar    AIC     BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model6    8 896.21  943.41 -440.10   880.21                     
# model5   17 907.46 1007.77 -436.73   873.46 6.7506  9     0.6631

anova(model1, model6)

# npar    AIC     BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model6    8 896.21  943.41 -440.10   880.21                     
# model1   48 956.88 1240.13 -430.44   860.88 19.322 40     0.9976

# check assumptions ...

check_model(model6)
ggsave('plots/check_model.png', width=10, height=10, dpi=600)

#------------------------------------------------------------------------------#
# + doko ----
#------------------------------------------------------------------------------#

# doko = ORC study (do) + KLE group (ko)

# filter for modeling ...

md <- temp %>% 
  filter(study == '210510_do', group == 'korean')

# count participants ...

check <- md %>%
  group_by(study, group, participant) %>%
  summarise()

# apply deviation coding ...

contrasts(md$dependency) <- contr.treatment(2) - matrix(rep(1/2, 2), ncol=1)
contrasts(md$environment) <- contr.treatment(3) - matrix(rep(1/3, 6), ncol=2)

# view contrasts ...

contrasts(md$dependency)
contrasts(md$environment)

#------------------------------------------------------------------------------#
# + + model 1
#------------------------------------------------------------------------------#

# fit model ...

tic()
model1 <- glmer(accuracy ~ dependency * environment + 
                  (dependency * environment | participant) + 
                  (dependency * environment | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_doko_acc_md1.rds')
summary(model1)
toc()
beep()

model1 <- read_rds('models/spr_doko_acc_md1.rds')

# 4618.69 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                2.3364     0.1959  11.925  < 2e-16 ***
# dependency2                1.5085     0.2777   5.431  5.6e-08 ***
# environment2              -0.3198     0.3392  -0.943    0.346    
# environment3              -0.3216     0.3169  -1.015    0.310    
# dependency2:environment2  -0.4101     0.7079  -0.579    0.562    
# dependency2:environment3  -0.7649     0.7295  -1.049    0.294 

#------------------------------------------------------------------------------#
# + + model 5
#------------------------------------------------------------------------------#

# fit model ...

tic()
model5 <- glmer(accuracy ~ dependency * environment + 
                  (dependency + environment | participant) + 
                  (1 | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_doko_acc_md5.rds')
summary(model5)
toc()
beep()

model5 <- read_rds('models/spr_doko_acc_md5.rds')

# 16.36 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                2.1794     0.1683  12.953  < 2e-16 ***
# dependency2                1.4001     0.1966   7.123 1.05e-12 ***
# environment2              -0.1640     0.2427  -0.676    0.499    
# environment3              -0.1136     0.2300  -0.494    0.621    
# dependency2:environment2  -0.1571     0.3886  -0.404    0.686    
# dependency2:environment3  -0.1784     0.3853  -0.463    0.643 

# compare models ...

anova(model1, model5)

# npar    AIC    BIC  logLik deviance Chisq Df Pr(>Chisq)
# model5   17 1551.3 1646.4 -758.66   1517.3                    
# model1   48 1592.0 1860.4 -748.03   1496.0 21.27 31     0.9046

#------------------------------------------------------------------------------#
# + + model 6 (final) ----
#------------------------------------------------------------------------------#

# fit model ...

tic()
model6 <- glmer(accuracy ~ dependency * environment + 
                  (1 | participant) + 
                  (1 | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_doko_acc_md6.rds')
summary(model6)
toc()
beep()

model6 <- read_rds('models/spr_doko_acc_md6.rds')

# 1.71 sec elapsed
# no warnings
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)               2.14936    0.16093  13.356   <2e-16 ***
# dependency2               1.40744    0.14654   9.604   <2e-16 ***
# environment2             -0.19510    0.17597  -1.109    0.268    
# environment3             -0.04670    0.17985  -0.260    0.795    
# dependency2:environment2 -0.03271    0.35363  -0.092    0.926    
# dependency2:environment3 -0.06833    0.35921  -0.190    0.849 

# compare models ...

anova(model1, model6)

# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model6    8 1537.2 1582.0 -760.63   1521.2                     
# model1   48 1592.0 1860.4 -748.03   1496.0 25.201 40     0.9672

anova(model5, model6)

# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model6    8 1537.2 1582.0 -760.63   1521.2                     
# model5   17 1551.3 1646.4 -758.66   1517.3 3.9306  9     0.9159

# check assumptions ...

check_model(model6)
ggsave('plots/check_model.png', width=10, height=10, dpi=600)

#------------------------------------------------------------------------------#
# + dozh ----
#------------------------------------------------------------------------------#

# dozh = ORC study (do) + MLE group (zh)

# filter for modeling ...

md <- temp %>% 
  filter(study == '210510_do', group == 'mandarin')

# count participants ...

check <- md %>%
  group_by(study, group, participant) %>%
  summarise()

# apply deviation coding ...

contrasts(md$dependency) <- contr.treatment(2) - matrix(rep(1/2, 2), ncol=1)
contrasts(md$environment) <- contr.treatment(3) - matrix(rep(1/3, 6), ncol=2)

# view contrasts ...

contrasts(md$dependency)
contrasts(md$environment)

#------------------------------------------------------------------------------#
# + + model 1 (maximal)
#------------------------------------------------------------------------------#

# fit model ...

tic()
model1 <- glmer(accuracy ~ dependency * environment + 
                  (dependency * environment | participant) + 
                  (dependency * environment | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_dozh_acc_md1.rds')
summary(model1)
toc()
beep()

model1 <- read_rds('models/spr_dozh_acc_md1.rds')

# 1717.86 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                3.8007     0.3166  12.006   <2e-16 ***
# dependency2                1.1720     0.4939   2.373   0.0177 *  
# environment2              -0.1428     0.6234  -0.229   0.8189    
# environment3              -0.5478     0.6444  -0.850   0.3953    
# dependency2:environment2   1.0256     1.1382   0.901   0.3675    
# dependency2:environment3   1.5550     1.0640   1.462   0.1439 

#------------------------------------------------------------------------------#
# + + model 3
#------------------------------------------------------------------------------#

# fit model ...

tic()
model3 <- glmer(accuracy ~ dependency * environment + 
                  (dependency + environment | participant) + 
                  (dependency + environment | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_dozh_acc_md3.rds')
summary(model3)
toc()
beep()

model3 <- read_rds('models/spr_dozh_acc_md3.rds')

# 488.4 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)               3.55710    0.28596  12.439   <2e-16 ***
# dependency2               0.81722    0.33417   2.446   0.0145 *  
# environment2             -0.35603    0.57372  -0.621   0.5349    
# environment3             -0.85667    0.57475  -1.491   0.1361    
# dependency2:environment2  0.07305    0.56159   0.130   0.8965    
# dependency2:environment3  0.67998    0.59071   1.151   0.2497 

# compare models ...

anova(model3, model1)

# npar    AIC    BIC  logLik deviance Chisq Df Pr(>Chisq)
# model3   26 1014.7 1160.5 -481.35   962.70                    
# model1   48 1048.9 1318.0 -476.45   952.91 9.797 22     0.9881

#------------------------------------------------------------------------------#
# + + model 4
#------------------------------------------------------------------------------#

# fit model ...

tic()
model4 <- glmer(accuracy ~ dependency * environment + 
                  (dependency * environment | participant) + 
                  (1 | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_dozh_acc_md4.rds')
summary(model4)
toc()
beep()

model4 <- read_rds('models/spr_dozh_acc_md4.rds')

# 69.7 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)               3.517461   0.285402  12.325   <2e-16 ***
# dependency2               0.771873   0.375679   2.055   0.0399 *  
# environment2              0.005219   0.545209   0.010   0.9924    
# environment3             -0.638777   0.474521  -1.346   0.1783    
# dependency2:environment2  0.686489   0.902823   0.760   0.4470    
# dependency2:environment3  0.666644   0.777452   0.857   0.3912 

# compare models ...

anova(model1, model4)

# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model4   28 1020.4 1177.4 -482.22   964.43                     
# model1   48 1048.9 1318.0 -476.45   952.91 11.525 20     0.9314

anova(model3, model4)

# npar    AIC    BIC  logLik deviance Chisq Df Pr(>Chisq)
# model3   26 1014.7 1160.5 -481.35   962.70                    
# model4   28 1020.4 1177.4 -482.22   964.43     0  2          1

#------------------------------------------------------------------------------#
# + + model 5 (final) ----
#------------------------------------------------------------------------------#

# fit model ...

tic()
model5 <- glmer(accuracy ~ dependency * environment + 
                  (dependency + environment | participant) + 
                  (1 | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_dozh_acc_md5.rds')
summary(model5)
toc()
beep()

model5 <- read_rds('models/spr_dozh_acc_md5.rds')

# 14.53 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)               3.46372    0.27446  12.620  < 2e-16 ***
# dependency2               0.79196    0.30610   2.587  0.00967 ** 
# environment2             -0.04924    0.51008  -0.097  0.92310    
# environment3             -0.61506    0.46842  -1.313  0.18916    
# dependency2:environment2  0.16803    0.54197   0.310  0.75653    
# dependency2:environment3  0.75679    0.56992   1.328  0.18422

# compare models ...

anova(model4, model5)

# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model5   17 1001.2 1096.5 -483.62   967.24                     
# model4   28 1020.4 1177.4 -482.22   964.43 2.8109 11      0.993

#------------------------------------------------------------------------------#
# + + model 6
#------------------------------------------------------------------------------#

# fit model ...

tic()
model6 <- glmer(accuracy ~ dependency * environment + 
                  (1 | participant) + 
                  (1 | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_dozh_acc_md6.rds')
summary(model6)
toc()
beep()

model6 <- read_rds('models/spr_dozh_acc_md6.rds')

# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)               3.26369    0.24570  13.283  < 2e-16 ***
# dependency2               0.98170    0.19136   5.130  2.9e-07 ***
# environment2             -0.23260    0.23434  -0.993    0.321    
# environment3             -0.14076    0.24246  -0.581    0.562    
# dependency2:environment2 -0.03142    0.45959  -0.068    0.945    
# dependency2:environment3  0.57751    0.47463   1.217    0.224 

# compare models ...

anova(model6, model5)

# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)  
# model6    8 1000.0 1044.9 -492.00   984.01                       
# model5   17 1001.2 1096.5 -483.62   967.24 16.767  9     0.0525 .

#------------------------------------------------------------------------------#
# + suen ----
#------------------------------------------------------------------------------#

# suen = SRC study (su) + ENS group (en)

# filter for modeling ...

md <- temp %>% 
  filter(study == '210510_su', group == 'english')

# count participants ...

check <- md %>%
  group_by(study, group, participant) %>%
  summarise()

# apply deviation coding ...

contrasts(md$dependency) <- contr.treatment(2) - matrix(rep(1/2, 2), ncol=1)
contrasts(md$environment) <- contr.treatment(3) - matrix(rep(1/3, 6), ncol=2)

# view contrasts ...

contrasts(md$dependency)
contrasts(md$environment)

#------------------------------------------------------------------------------#
# + + model 1 (maximal)
#------------------------------------------------------------------------------#

# fit model ...

tic()
model1 <- glmer(accuracy ~ dependency * environment + 
                  (dependency * environment | participant) + 
                  (dependency * environment | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_suen_acc_md1.rds')
summary(model1)
toc()
beep()

model1 <- read_rds('models/spr_suen_acc_md1.rds')

# 1105.91 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)               3.09121    0.24818  12.455  < 2e-16 ***
# dependency2               1.33220    0.38692   3.443 0.000575 ***
# environment2             -0.00791    0.47680  -0.017 0.986764    
# environment3             -0.40507    0.49115  -0.825 0.409521    
# dependency2:environment2  0.54681    0.93946   0.582 0.560536    
# dependency2:environment3  1.02199    0.93589   1.092 0.274834 

#------------------------------------------------------------------------------#
# + + model 3
#------------------------------------------------------------------------------#

# fit model ...

tic()
model3 <- glmer(accuracy ~ dependency * environment + 
                  (dependency * environment | participant) + 
                  (dependency + environment | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_suen_acc_md3.rds')
summary(model3)
toc()
beep()

# skip for now

#------------------------------------------------------------------------------#
# + + model 4
#------------------------------------------------------------------------------#

# fit model ...

tic()
model4 <- glmer(accuracy ~ dependency * environment + 
                  (dependency + environment | participant) + 
                  (dependency + environment | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_suen_acc_md4.rds')
summary(model4)
toc()
beep()

# 76.96 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)               3.03336    0.24319  12.473  < 2e-16 ***
# dependency2               1.27208    0.35871   3.546 0.000391 ***
# environment2             -0.03007    0.40248  -0.075 0.940437    
# environment3             -0.45754    0.37687  -1.214 0.224735    
# dependency2:environment2  0.07569    0.56524   0.134 0.893479    
# dependency2:environment3  0.99261    0.56357   1.761 0.078190 .  

# compare models ...

anova(model3, model4)

npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
model4   26 1055.3 1198.6 -501.66   1003.3
model3   37 1076.2 1280.1 -501.12   1002.2 1.0889 11     0.9999

#------------------------------------------------------------------------------#
# + + model 5
#------------------------------------------------------------------------------#

tic()
model5 <- glmer(accuracy ~ dependency * environment + 
                  (1 + dependency + environment | participant) + 
                  (1 | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_suen_acc_md5.rds')
summary(model5)
toc()
beep()

model5 <- read_rds('models/spr_suen_acc_md5.rds')

# 7.9 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)               2.95094    0.22589  13.064  < 2e-16 ***
# dependency2               1.14172    0.30432   3.752 0.000176 ***
# environment2             -0.02452    0.33693  -0.073 0.941976    
# environment3             -0.50700    0.30082  -1.685 0.091909 .  
# dependency2:environment2  0.09075    0.51881   0.175 0.861136    
# dependency2:environment3  0.89985    0.50233   1.791 0.073239 . 

#------------------------------------------------------------------------------#
# + + model 6 (final) ----
#------------------------------------------------------------------------------#

tic()
model6 <- glmer(accuracy ~ dependency * environment + 
                  (1 | participant) + 
                  (1 | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_suen_acc_md6.rds')
summary(model6)
toc()
beep()

model6 <- read_rds('models/spr_suen_acc_md6.rds')

# 1.56 sec elapsed
# no warnings
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                2.8552     0.2028  14.080  < 2e-16 ***
# dependency2                0.9083     0.1879   4.834 1.34e-06 ***
# environment2              -0.2913     0.2357  -1.236   0.2164    
# environment3              -0.5806     0.2326  -2.496   0.0126 *  
# dependency2:environment2   0.1031     0.4708   0.219   0.8266    
# dependency2:environment3   0.9431     0.4658   2.025   0.0429 * 

anova(model6, model5)

# npar    AIC    BIC  logLik deviance Chisq Df Pr(>Chisq)
# model6    8 1025.4 1069.5 -504.70   1009.4                    
# model5   17 1039.8 1133.5 -502.92   1005.8 3.557  9     0.9381

anova(model4, model6)

# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model6    8 1025.4 1069.5 -504.70   1009.4                     
# model4   26 1055.3 1198.6 -501.66   1003.3 6.0642 18     0.9959

anova(model1, model6)

# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model6    8 1025.4 1069.5 -504.70  1009.39                     
# model1   48 1093.4 1357.9 -498.69   997.39 12.004 40          1

# post-hoc tests ...

pairwise1 <- model6 %>%
  emmeans(~ dependency * environment) %>%
  contrast('pairwise', by = 'environment') %>%
  summary(by = NULL, adjust = 'holm')
pairwise1

# contrast      environment estimate    SE  df z.ratio p.value
# gap - pronoun short         -0.560 0.350 Inf  -1.598  0.1099
# gap - pronoun long          -0.663 0.315 Inf  -2.103  0.0710 .
# gap - pronoun island        -1.503 0.307 Inf  -4.891  <.0001 ***
# Results are given on the log odds ratio (not the response) scale.
# P value adjustment: holm method for 3 tests

#------------------------------------------------------------------------------#
# + suko ----
#------------------------------------------------------------------------------#

# suko = SRC study (su) + KLE group (ko)

# filter for modeling ...

md <- temp %>% 
  filter(study == '210510_su', group == 'korean')

# count participants ...

check <- md %>%
  group_by(study, group, participant) %>%
  summarise()

# apply deviation coding ...

contrasts(md$dependency) <- contr.treatment(2) - matrix(rep(1/2, 2), ncol=1)
contrasts(md$environment) <- contr.treatment(3) - matrix(rep(1/3, 6), ncol=2)

# view contrasts ...

contrasts(md$dependency)
contrasts(md$environment)

#------------------------------------------------------------------------------#
# + + model 1 (maximal)
#------------------------------------------------------------------------------#

# fit model ...

tic()
model1 <- glmer(accuracy ~ dependency * environment + 
                  (dependency * environment | participant) + 
                  (dependency * environment | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_suko_acc_md1.rds')
summary(model1)
toc()
beep()

model1 <- read_rds('models/spr_suko_acc_md1.rds')

# 3775.56 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)               3.30218    0.31797  10.385  < 2e-16 ***
# dependency2               1.65354    0.56411   2.931  0.00338 ** 
# environment2             -1.37605    0.78757  -1.747  0.08060 .  
# environment3             -1.82990    0.73873  -2.477  0.01325 *  
# dependency2:environment2 -0.66746    1.66759  -0.400  0.68897    
# dependency2:environment3 -0.07874    1.54553  -0.051  0.95937  

#------------------------------------------------------------------------------#
# + + model 4
#------------------------------------------------------------------------------#

# fit model ...

tic()
model4 <- glmer(accuracy ~ dependency * environment + 
                  (dependency + environment | participant) + 
                  (dependency + environment | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_suko_acc_md4.rds')
summary(model4)
toc()
beep()

model4 <- read_rds('models/spr_suko_acc_md4.rds')

# 117.45 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)               3.30218    0.31797  10.385  < 2e-16 ***
# dependency2               1.65354    0.56411   2.931  0.00338 ** 
# environment2             -1.37605    0.78757  -1.747  0.08060 .  
# environment3             -1.82990    0.73873  -2.477  0.01325 *  
# dependency2:environment2 -0.66746    1.66759  -0.400  0.68897    
# dependency2:environment3 -0.07874    1.54553  -0.051  0.95937  

# compare models ...

anova(model1, model4)

# npar    AIC    BIC  logLik deviance Chisq Df Pr(>Chisq)
# model4   26 1181.9 1326.1 -564.97   1129.9                    
# model1   48 1205.2 1471.4 -554.61   1109.2 20.71 22     0.5387

#------------------------------------------------------------------------------#
# + + model 5
#------------------------------------------------------------------------------#

# fit model ...

tic()
model5 <- glmer(accuracy ~ dependency * environment + 
                  (1 + dependency + environment | participant) + 
                  (1 | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_suko_acc_md5.rds')
summary(model5)
toc()
beep()

model5 <- read_rds('models/spr_suko_acc_md5.rds')

# 5.78 sec elapsed
# Model failed to converge with max|grad| = 0.169075 (tol = 0.002, component 1)
# Model is nearly unidentifiable: very large eigenvalue
# - Rescale variables?
  
#------------------------------------------------------------------------------#
# + + model 6 (final) ----
#------------------------------------------------------------------------------#

# fit model ...

tic()
model6 <- glmer(accuracy ~ dependency * environment + 
                  (1 | participant) + 
                  (1 | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_suko_acc_md6.rds')
summary(model6)
toc()
beep()

model6 <- read_rds('models/spr_suko_acc_md6.rds')

# 1.36 sec elapsed
# no warnings
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                2.7622     0.2021  13.670  < 2e-16 ***
# dependency2                1.1325     0.1770   6.399 1.57e-10 ***
# environment2              -0.8526     0.2288  -3.727 0.000194 ***
# environment3              -1.0365     0.2238  -4.631 3.63e-06 ***
# dependency2:environment2   0.5178     0.4566   1.134 0.256780    
# dependency2:environment3   0.5981     0.4506   1.327 0.184411  

# compare models ...

anova(model4, model6)

# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model6    8 1153.7 1198.0 -568.83   1137.7                     
# model4   26 1181.9 1326.1 -564.97   1129.9 7.7169 18     0.9826

anova(model6, model1)

# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model6    8 1153.7 1198.0 -568.83   1137.7                     
# model1   48 1205.2 1471.4 -554.61   1109.2 28.427 40     0.9144

#------------------------------------------------------------------------------#
# + suzh ----
#------------------------------------------------------------------------------#

# suzh = SRC study (su) + MLE group (zh)

# filter for modeling ...

md <- temp %>% 
  filter(study == '210510_su', group == 'mandarin')

# count participants ...

check <- md %>%
  group_by(study, group, participant) %>%
  summarise()

# apply deviation coding ...

contrasts(md$dependency) <- contr.treatment(2) - matrix(rep(1/2, 2), ncol=1)
contrasts(md$environment) <- contr.treatment(3) - matrix(rep(1/3, 6), ncol=2)

# view contrasts ...

contrasts(md$dependency)
contrasts(md$environment)

#------------------------------------------------------------------------------#
# + + model 1 (maximal)
#------------------------------------------------------------------------------#

# fit model ...

tic()
model1 <- glmer(accuracy ~ dependency * environment + 
                  (dependency * environment | participant) + 
                  (dependency * environment | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_suzh_acc_md1.rds')
summary(model1)
toc()
beep()

model1 <- read_rds('models/spr_suzh_acc_md1.rds')

# 1747.23 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                3.0974     0.2409  12.857  < 2e-16 ***
# dependency2                1.3350     0.3822   3.493 0.000478 ***
# environment2              -0.9075     0.5640  -1.609 0.107586    
# environment3              -1.0045     0.5111  -1.965 0.049360 *  
# dependency2:environment2   0.7882     1.0811   0.729 0.465980    
# dependency2:environment3  -0.1669     1.0064  -0.166 0.868303 

#------------------------------------------------------------------------------#
# + + model 3 
#------------------------------------------------------------------------------#

# fit model ...

tic()
model3 <- glmer(accuracy ~ dependency * environment + 
                  (dependency + environment | participant) + 
                  (dependency + environment | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_suzh_acc_md3.rds')
summary(model3)
toc()
beep()

# 99.14 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                2.9171     0.2162  13.493  < 2e-16 ***
# dependency2                0.9761     0.2665   3.662  0.00025 ***
# environment2              -1.0376     0.4414  -2.351  0.01874 *  
# environment3              -1.0614     0.4193  -2.531  0.01136 *  
# dependency2:environment2   0.8669     0.4895   1.771  0.07655 .  
# dependency2:environment3   0.3725     0.4786   0.778  0.43639  

# compare models ...

anova(model1, model3)

# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model3   26 1280.3 1426.8 -614.17   1228.3                     
# model1   48 1311.2 1581.7 -607.60   1215.2 13.128 22     0.9295

#------------------------------------------------------------------------------#
# + + model 4
#------------------------------------------------------------------------------#

# fit model ...

tic()
model4 <- glmer(accuracy ~ dependency * environment + 
                  (dependency * environment | participant) + 
                  (1 | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_suzh_acc_md4.rds')
summary(model4)
toc()
beep()

# 58.47 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                2.9229     0.2135  13.688  < 2e-16 ***
# dependency2                1.0638     0.3089   3.444 0.000573 ***
# environment2              -0.4359     0.4144  -1.052 0.292853    
# environment3              -0.4856     0.3727  -1.303 0.192616    
# dependency2:environment2   1.4062     0.7992   1.760 0.078469 .  
# dependency2:environment3   0.6208     0.7241   0.857 0.391262 

# compare models ...

anova(model3, model4)

# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model3   26 1280.3 1426.8 -614.17   1228.3                     
# model4   28 1282.5 1440.3 -613.26   1226.5 1.8186  2     0.4028

#------------------------------------------------------------------------------#
# + + model 5
#------------------------------------------------------------------------------#

# fit model ...

tic()
model5 <- glmer(accuracy ~ dependency * environment + 
                   (dependency + environment | participant) + 
                   (1 | item), 
                 data = md, family = binomial, 
                 control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_suzh_acc_md5.rds')
summary(model5)
toc()
beep()

model5 <- read_rds('models/spr_suzh_acc_md5.rds')

# 15.28 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                2.7996     0.1989  14.078  < 2e-16 ***
# dependency2                0.9387     0.2444   3.842 0.000122 ***
# environment2              -0.6705     0.3303  -2.030 0.042364 *  
# environment3              -0.7110     0.3354  -2.120 0.034040 *  
# dependency2:environment2   0.9930     0.4632   2.144 0.032038 *  
# dependency2:environment3   0.4440     0.4572   0.971 0.331508

# compare models ...

anova(model4, model5)

# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model5   17 1271.2 1367.0 -618.58   1237.2                     
# model4   28 1282.5 1440.3 -613.26   1226.5 10.635 11     0.4743

#------------------------------------------------------------------------------#
# + + model 6 (final) ----
#------------------------------------------------------------------------------#

# fit model ...

tic()
model6 <- glmer(accuracy ~ dependency * environment + 
                  (1 | participant) + 
                  (1 | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_suzh_acc_md6.rds')
summary(model6)
toc()
beep()

model6 <- read_rds('models/spr_suzh_acc_md6.rds')

# 1.32 sec elapsed
# no warnings
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                2.7050     0.1816  14.899  < 2e-16 ***
# dependency2                0.9649     0.1640   5.885 3.99e-09 ***
# environment2              -0.5033     0.2093  -2.405 0.016181 *  
# environment3              -0.6909     0.1990  -3.471 0.000518 ***
# dependency2:environment2   1.2225     0.4172   2.930 0.003390 ** 
# dependency2:environment3   0.5130     0.3961   1.295 0.195320   

# compare models ...

anova(model6, model5)

# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model6    8 1260.2 1305.2 -622.08   1244.2                     
# model5   17 1271.2 1367.0 -618.58   1237.2 6.9998  9     0.6371

anova(model1, model6)

# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model6    8 1260.2 1305.2 -622.08   1244.2                     
# model1   48 1311.2 1581.7 -607.60   1215.2 28.945 40     0.9025

# post-hoc tests

pairwise1 <- model6 %>%
  emmeans(~ dependency * environment) %>%
  contrast('pairwise', by = 'environment') %>%
  summary(by = NULL, adjust = 'holm')
pairwise1

# contrast      environment estimate    SE  df z.ratio p.value
# gap - pronoun short         -0.386 0.307 Inf  -1.257  0.2089
# gap - pronoun long          -1.609 0.285 Inf  -5.646  <.0001 ***
# gap - pronoun island        -0.899 0.250 Inf  -3.593  0.0007 ***
#   Results are given on the log odds ratio (not the response) scale.
# P value adjustment: holm method for 3 tests

#------------------------------------------------------------------------------#
# scatter plot of proficiency effects for accuracy data ----
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

#==============================================================================#
# ::::: acceptability judgment task (ajt) ::::: ----
#==============================================================================#

# ajt = acceptability judgment task

# create dataframe for ajt data ...

ajt <- df %>%
  filter(task %in% c('english_ajt', 'korean_ajt', 'mandarin_ajt')) %>%
  arrange(study, group, participant) %>%
  select_if(function(x){!all(is.na(x))})

check <- ajt %>%
  group_by(study, group, task, participant) %>%
  summarise() %>%
  ungroup() %>%
  group_by(study, group, task) %>%
  summarise(n = n()) %>%
  ungroup()

# check 'NA' responses from skipped trials ...

check <- ajt %>%
  filter(!response %in% c(1, 2, 3, 4, 5, 6))

# remove 'NA' responses ...

ajt <- ajt %>%
  filter(is.na(response) == FALSE)

# add binary accept/reject column ...

ajt <- ajt %>%
  mutate(acceptance = case_when(response > 3.5 ~ TRUE,
                                response < 3.5 ~ FALSE))

# calculate z-scores for each task by participant on ratings for all trials ...

ajt <- ajt %>%
  mutate(response = as.numeric(response)) %>%
  group_by(study, group, task, participant) %>%
  mutate(zscore = (response - mean(response, na.rm=T)) / sd(response, na.rm = T)) %>%
  ungroup()

check <- ajt %>%
  mutate(condition = as.factor(condition))

summary(check$condition)

# calculate filler accuracy by participant ...

temp <- ajt %>%
  filter(condition %in% c('grammatical', 'ungrammatical')) %>%
  mutate(accuracy = as.logical(accuracy)) %>%
  group_by(study, group, task, participant) %>%
  summarise(acc_rate = mean(accuracy, na.rm = T)) %>%
  ungroup()

check <- temp %>%
  group_by(study, group, task) %>%
  summarise(n = n()) %>%
  ungroup()

check <- temp %>%
  filter(acc_rate <= .5)

# inspect accuracy rates ...

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

# trim based on accuracy rate ...

ajt <- ajt %>%
  left_join(temp, by = c('study', 'group', 'task', 'participant')) %>%
  filter(acc_rate > .5)

# inspect accuracy rates by participant ...

plot <- ajt %>%
  group_by(group, task, participant, acc_rate) %>%
  summarise() %>%
  ungroup()

# inspect accuracy rates ...

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

# check number of participants ...

check <- ajt %>%
  group_by(study, task, group, participant) %>%
  summarise() %>%
  ungroup() %>%
  group_by(study, task, group) %>%
  summarise(n = n()) %>%
  ungroup()

#------------------------------------------------------------------------------#
# check for yes-bias and no-bias participants ----
#------------------------------------------------------------------------------#

# summarise ...

plot <- ajt %>%
  group_by(study, group, participant) %>%
  summarise(mean = mean(response, na.rm = TRUE)) %>%
  ungroup()

# check outliers ...

outliers_list <- check_outliers(plot$mean)

outliers <- plot[!outliers_list, ]

outliers_list

check_outliers(plot)

# define data for plots

p <- ggplot(data=plot, aes(x=group, y=mean))

# generate plot

s <- list(
  geom_hline(yintercept=mean(mean)),
  geom_violin(),
  geom_boxplot(width = .1, fill='white'),
  geom_jitter(shape = 1, size = 1, position = position_jitter(seed = 2, width = .2)),
  theme_classic(),
  scale_x_discrete(name="group", 
                   limits = c('english', 'korean', 'mandarin'),
                   labels = c('ENS', 'KLE', 'MLE')),
  scale_y_continuous(name="mean rating", 
                     limits=c(0, 6)),
  theme(text = element_text(size = 12), 
        plot.title = element_text(size = 12, hjust = .5), 
        legend.position = "hide"),
  facet_wrap(~study)
)

# write and save ...

p + s
ggsave("plots/orc/spr_accuracy_proficiency_effect.png", width=6.5, height=3, dpi=600)


#------------------------------------------------------------------------------#
# interaction plots - critical trials - z-scores ----
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
# interaction plots - critical trials - raw ratings ----
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

ggsave("plots/orc/ajt_crit_rating.png", width=6.5, height=3, dpi=600)

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
# interaction plots - critical trials - acceptance rates ----
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
# scatter plots by item and by participant ----
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
# modeling - clmm - critical ----
#------------------------------------------------------------------------------#

# prep data

temp <- ajt %>%
  filter(!condition %in% c('grammatical', 'ungrammatical')) %>%
  mutate(environment = fct_relevel(environment, 'short', 'long', 'island'),
         dependency = as.factor(dependency))

#------------------------------------------------------------------------------#
# + all ----
#------------------------------------------------------------------------------#

md <- temp %>% 
  mutate(panel = case_when(task == 'english_ajt' & group == 'english' ~ 'enen',
                           task == 'english_ajt' & group == 'korean' ~ 'enko',
                           task == 'english_ajt' & group == 'mandarin' ~ 'enzh',
                           task == 'korean_ajt' & group == 'korean' ~ 'koko',
                           task == 'mandarin_ajt' & group == 'mandarin' ~ 'zhzh')) %>%
  filter(study == '210510_do')

# convert response and panel to factors

md <- md %>%
  mutate(response = ordered(response)) %>%
  mutate(panel = factor(panel))

# apply deviation coding ...

contrasts(md$dependency) <- contr.treatment(2) - matrix(rep(1/2, 2), ncol = 1)
contrasts(md$environment) <- contr.treatment(3) - matrix(rep(1/3, 6), ncol = 2)
contrasts(md$panel) <- contr.treatment(5) - matrix(rep(1/5, 20), ncol = 4)

# view contrasts ...

contrasts(md$dependency)
contrasts(md$environment)
contrasts(md$panel)

#------------------------------------------------------------------------------#
# + + model 1 
#------------------------------------------------------------------------------#

# fit model ...

tic()
model1 <- clmm(response ~ dependency * environment * panel + 
                 (1 | participant) + 
                 (1 | item), 
               data = md, control = clmm.control(grtol = 1e6)) %>%
  write_rds('models/ajt_all_clmm_md1.rds')
summary(model1)
toc()
beep()

# 274.33 sec elapsed
# no warnings
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# dependency2                     -1.81769    0.04010 -45.331  < 2e-16 ***
# environment2                    -0.32292    0.04712  -6.853 7.25e-12 ***
# environment3                    -0.49513    0.04686 -10.566  < 2e-16 ***
# panel2                           0.23305    0.17484   1.333  0.18254    
# panel3                           0.55941    0.17015   3.288  0.00101 ** 
# panel4                           0.16746    0.17419   0.961  0.33636    
# panel5                          -0.43686    0.16904  -2.584  0.00976 ** 
# dependency2:environment2         1.29088    0.09451  13.658  < 2e-16 ***
# dependency2:environment3         2.45558    0.09517  25.803  < 2e-16 ***
# dependency2:panel2               1.33580    0.12468  10.714  < 2e-16 ***
# dependency2:panel3               1.82873    0.12093  15.123  < 2e-16 ***
# dependency2:panel4               1.40997    0.12151  11.604  < 2e-16 ***
# dependency2:panel5               2.61988    0.11636  22.515  < 2e-16 ***
# environment2:panel2              0.46848    0.15448   3.033  0.00242 ** 
# environment3:panel2              1.63459    0.15194  10.758  < 2e-16 ***
# environment2:panel3              0.29057    0.14963   1.942  0.05215 .  
# environment3:panel3              1.05762    0.14693   7.198 6.10e-13 ***
# environment2:panel4             -0.12568    0.15276  -0.823  0.41066    
# environment3:panel4              0.92319    0.15044   6.137 8.43e-10 ***
# environment2:panel5              0.78617    0.14299   5.498 3.84e-08 ***
# environment3:panel5              1.01944    0.14042   7.260 3.87e-13 ***
# dependency2:environment2:panel2 -0.50574    0.30881  -1.638  0.10149    
# dependency2:environment3:panel2 -2.27970    0.30433  -7.491 6.84e-14 ***
# dependency2:environment2:panel3 -0.77192    0.29893  -2.582  0.00981 ** 
# dependency2:environment3:panel3 -3.22313    0.29507 -10.923  < 2e-16 ***
# dependency2:environment2:panel4  1.77597    0.30628   5.799 6.69e-09 ***
# dependency2:environment3:panel4 -0.34056    0.30183  -1.128  0.25918    
# dependency2:environment2:panel5 -0.63643    0.28582  -2.227  0.02597 *  
# dependency2:environment3:panel5 -2.93943    0.28203 -10.423  < 2e-16 ***

# post-hoc tests ...

pairwise <- model1 %>%
  emmeans(~ dependency * environment * panel) %>%
  contrast('pairwise', simple = 'each', combine = TRUE) %>%
  summary(by = NULL, adjust = 'holm')
pairwise

#------------------------------------------------------------------------------#
# + + model 2
#------------------------------------------------------------------------------#

# fit model ...

tic()
model2 <- clmm(response ~ dependency * environment * panel + 
                 (1 + dependency * environment | participant) + 
                 (1 + dependency * environment * panel | item), 
               data = md, control = clmm.control(grtol = 1e6)) %>%
  write_rds('models/ajt_all_clmm_md2.rds')
summary(model2)
toc()
beep()

#------------------------------------------------------------------------------#
# + + model 3
#------------------------------------------------------------------------------#

# fit model ...

tic()
model3 <- clmm(response ~ dependency * environment * panel + 
                 (1 + dependency | participant) + 
                 (1 | item), 
               data = md, control = clmm.control(grtol = 1e6)) %>%
  write_rds('models/ajt_all_clmm_md3.rds')
summary(model3)
toc()
beep()

# 774.3 sec elapsed
# no warnings
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# dependency2                     -2.22157    0.14395 -15.433  < 2e-16 ***
# environment2                    -0.34752    0.04858  -7.154 8.42e-13 ***
# environment3                    -0.53518    0.04881 -10.965  < 2e-16 ***
# panel2                           0.28574    0.20001   1.429 0.153111    
# panel3                           0.66475    0.19471   3.414 0.000640 ***
# panel4                           0.20997    0.19991   1.050 0.293565    
# panel5                          -0.42894    0.19392  -2.212 0.026971 *  
# dependency2:environment2         1.42208    0.09748  14.589  < 2e-16 ***
# dependency2:environment3         2.69486    0.09905  27.208  < 2e-16 ***
# dependency2:panel2               1.55978    0.34216   4.559 5.15e-06 ***
# dependency2:panel3               2.21698    0.33315   6.655 2.84e-11 ***
# dependency2:panel4               1.71359    0.34198   5.011 5.42e-07 ***
# dependency2:panel5               3.04635    0.33173   9.183  < 2e-16 ***
# environment2:panel2              0.54230    0.15723   3.449 0.000562 ***
# environment3:panel2              1.82834    0.15697  11.648  < 2e-16 ***
# environment2:panel3              0.33418    0.15267   2.189 0.028608 *  
# environment3:panel3              1.18105    0.15177   7.782 7.14e-15 ***
# environment2:panel4             -0.16189    0.15980  -1.013 0.311011    
# environment3:panel4              1.03200    0.15953   6.469 9.85e-11 ***
# environment2:panel5              0.89238    0.14809   6.026 1.68e-09 ***
# environment3:panel5              1.12342    0.14749   7.617 2.60e-14 ***
# dependency2:environment2:panel2 -0.57626    0.31442  -1.833 0.066835 .  
# dependency2:environment3:panel2 -2.60530    0.31341  -8.313  < 2e-16 ***
# dependency2:environment2:panel3 -0.82914    0.30534  -2.715 0.006618 ** 
# dependency2:environment3:panel3 -3.56386    0.30398 -11.724  < 2e-16 ***
# dependency2:environment2:panel4  2.08331    0.31997   6.511 7.47e-11 ***
# dependency2:environment3:panel4 -0.23844    0.31879  -0.748 0.454496    
# dependency2:environment2:panel5 -0.68587    0.29598  -2.317 0.020487 *  
# dependency2:environment3:panel5 -3.26738    0.29533 -11.063  < 2e-16 ***

anova(model1, model3)

# no.par   AIC logLik LR.stat df Pr(>Chisq)    
# model1     36 31392 -15660                          
# model3     38 29973 -14949  1422.4  2  < 2.2e-16 ***

#------------------------------------------------------------------------------#
# + + model 4
#------------------------------------------------------------------------------#

# fit model ...

tic()
model4 <- clmm(response ~ dependency * environment * panel + 
                 (1 + dependency | participant) + 
                 (1 + dependency | item), 
               data = md, control = clmm.control(grtol = 1e6)) %>%
  write_rds('models/ajt_all_clmm_md4.rds')
summary(model4)
toc()
beep()

# 1454.01 sec elapsed
# no warnings
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# dependency2                     -2.22492    0.14667 -15.170  < 2e-16 ***
# environment2                    -0.34846    0.04865  -7.162 7.96e-13 ***
# environment3                    -0.53862    0.04890 -11.014  < 2e-16 ***
# panel2                           0.28468    0.20039   1.421 0.155411    
# panel3                           0.66182    0.19509   3.392 0.000693 ***
# panel4                           0.21215    0.20029   1.059 0.289515    
# panel5                          -0.42997    0.19429  -2.213 0.026895 *  
# dependency2:environment2         1.42626    0.09767  14.603  < 2e-16 ***
# dependency2:environment3         2.70093    0.09923  27.220  < 2e-16 ***
# dependency2:panel2               1.56303    0.34247   4.564 5.02e-06 ***
# dependency2:panel3               2.22429    0.33348   6.670 2.56e-11 ***
# dependency2:panel4               1.71190    0.34230   5.001 5.70e-07 ***
# dependency2:panel5               3.05033    0.33204   9.187  < 2e-16 ***
# environment2:panel2              0.54070    0.15736   3.436 0.000590 ***
# environment3:panel2              1.82478    0.15709  11.616  < 2e-16 ***
# environment2:panel3              0.33973    0.15295   2.221 0.026337 *  
# environment3:panel3              1.19387    0.15209   7.850 4.16e-15 ***
# environment2:panel4             -0.17416    0.16033  -1.086 0.277369    
# environment3:panel4              1.03850    0.15974   6.501 7.97e-11 ***
# environment2:panel5              0.87437    0.14850   5.888 3.91e-09 ***
# environment3:panel5              1.12416    0.14760   7.616 2.61e-14 ***
# dependency2:environment2:panel2 -0.57518    0.31475  -1.827 0.067632 .  
# dependency2:environment3:panel2 -2.59553    0.31367  -8.275  < 2e-16 ***
# dependency2:environment2:panel3 -0.84612    0.30593  -2.766 0.005679 ** 
# dependency2:environment3:panel3 -3.59358    0.30478 -11.791  < 2e-16 ***
# dependency2:environment2:panel4  2.09743    0.32086   6.537 6.28e-11 ***
# dependency2:environment3:panel4 -0.24936    0.31921  -0.781 0.434710    
# dependency2:environment2:panel5 -0.65340    0.29670  -2.202 0.027651 *  
# dependency2:environment3:panel5 -3.26539    0.29564 -11.045  < 2e-16 ***

anova(model3, model4)

# no.par   AIC logLik LR.stat df Pr(>Chisq)  
# model3     38 29973 -14949                        
# model4     40 29972 -14946  4.8397  2    0.08894 .

#------------------------------------------------------------------------------#
# + + model 5
#------------------------------------------------------------------------------#

# fit model ...

tic()
model5 <- clmm(response ~ dependency * environment * panel + 
                 (1 + dependency * environment | participant) + 
                 (1 + dependency * environment | item), 
               data = md, control = clmm.control(grtol = 1e6)) %>%
  write_rds('models/ajt_all_clmm_md5.rds')
summary(model5)
toc()
beep()

#------------------------------------------------------------------------------#
# + allen ----
#------------------------------------------------------------------------------#

md <- temp %>% 
  filter(study == '210510_do',
         task == 'english_ajt')

# convert response to factor

md <- md %>%
  mutate(response = ordered(response)) %>%
  mutate(group = factor(group))

# apply deviation coding ...

contrasts(md$dependency) <- contr.treatment(2) - matrix(rep(1/2, 2), ncol=1)
contrasts(md$environment) <- contr.treatment(3) - matrix(rep(1/3, 6), ncol=2)
contrasts(md$group) <- contr.treatment(3) - matrix(rep(1/3, 6), ncol=2)

# view contrasts ...

contrasts(md$dependency)
contrasts(md$environment)
contrasts(md$group)

#------------------------------------------------------------------------------#
# + + model 1 
#------------------------------------------------------------------------------#

# fit model ...

tic()
model1 <- clmm(response ~ dependency * environment * group + 
                 (1 | participant) + 
                 (1 | item), 
               data = md, control=clmm.control(grtol=1e6)) %>%
  write_rds('models/ajt_allen_clmm_md1.rds')
summary(model1)
toc()
beep()

# 79.03 sec elapsed
# no warnings
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# dependency2                     -2.13847    0.05363 -39.874  < 2e-16 ***
# environment2                    -0.33928    0.06154  -5.514 3.52e-08 ***
# environment3                    -0.51053    0.06086  -8.388  < 2e-16 ***
# group2                           0.20765    0.17816   1.166 0.243805    
# group3                           0.59244    0.17388   3.407 0.000656 ***
# dependency2:environment2         0.87851    0.12314   7.134 9.73e-13 ***
# dependency2:environment3         2.31111    0.12332  18.741  < 2e-16 ***
# dependency2:group2               1.31272    0.12393  10.593  < 2e-16 ***
# dependency2:group3               1.79179    0.12050  14.870  < 2e-16 ***
# environment2:group2              0.47927    0.15326   3.127 0.001766 ** 
# environment3:group2              1.61083    0.15102  10.666  < 2e-16 ***
# environment2:group3              0.29365    0.14835   1.980 0.047759 *  
# environment3:group3              1.02722    0.14578   7.046 1.84e-12 ***
# dependency2:environment2:group2 -0.52010    0.30647  -1.697 0.089685 .  
# dependency2:environment3:group2 -2.24969    0.30220  -7.444 9.74e-14 ***
# dependency2:environment2:group3 -0.75177    0.29667  -2.534 0.011277 *  
# dependency2:environment3:group3 -3.13786    0.29317 -10.703  < 2e-16 ***

#------------------------------------------------------------------------------#
# + + model 2
#------------------------------------------------------------------------------#

# fit model ...

tic()
model2 <- clmm(response ~ dependency * environment * group + 
                 (1 + dependency * environment | participant) + 
                 (1 + dependency * environment | item), 
               data = md, control=clmm.control(grtol=1e6)) %>%
  write_rds('models/ajt_allen_clmm_md2.rds')
summary(model2)
toc()
beep()

# Error: optimizer nlminb failed to converge

#------------------------------------------------------------------------------#
# + doenen ----
#------------------------------------------------------------------------------#

# doenen = ORC study (do) + ENS group (en) + English AJT (en)

# load package ...

library(ordinal)

# filter for analysis ...

md <- temp %>% 
  filter(study == '210510_do', 
         group == 'english',
         task == 'english_ajt')

check <- md %>% 
  group_by(study, group, participant) %>%
  summarise() %>%
  ungroup()

# convert response to factor

md <- md %>%
  mutate(response = as.factor(response, ordered = TRUE))

# apply deviation coding ...

contrasts(md$dependency) <- contr.treatment(2) - matrix(rep(1/2, 2), ncol=1)
contrasts(md$environment) <- contr.treatment(3) - matrix(rep(1/3, 6), ncol=2)

# view contrasts ...

contrasts(md$dependency)
contrasts(md$environment)

#------------------------------------------------------------------------------#
# + + model 1 (final) ----
#------------------------------------------------------------------------------#

# fit model ...

tic()
model1 <- clmm(response ~ dependency * environment + 
                 (1 + dependency * environment | participant) + 
                 (1 + dependency * environment | item), 
               data = md, control=clmm.control(grtol=1e6)) %>%
  write_rds('models/ajt_doenen_clmm_md1.rds')
summary(model1)
toc()
beep()

model1 <- read_rds('models/ajt_doenen_clmm_md1.rds')

# 7096.41 sec elapsed
# no warnings?
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# dependency2               -5.8542     0.3791 -15.443  < 2e-16 ***
# environment2              -0.5718     0.2566  -2.229 0.025834 *  
# environment3              -2.1130     0.3543  -5.963 2.47e-09 ***
# dependency2:environment2   1.6434     0.4507   3.646 0.000266 ***
# dependency2:environment3   6.7497     0.5432  12.425  < 2e-16 ***

# post-hoc tests ...

pairwise <- model1 %>%
  emmeans(~ dependency * environment) %>%
  contrast('pairwise', simple = 'each', combine = TRUE) %>%
  summary(by = NULL, adjust = 'holm')
pairwise

# environment dependency contrast       estimate    SE  df z.ratio p.value
# short       .          gap - pronoun      8.65 0.548 Inf  15.786  <.0001
# long        .          gap - pronoun      7.01 0.483 Inf  14.508  <.0001
# island      .          gap - pronoun      1.90 0.375 Inf   5.077  <.0001
# .           gap        short - long       1.39 0.424 Inf   3.283  0.0041
# .           gap        short - island     5.49 0.500 Inf  10.971  <.0001
# .           gap        long - island      4.09 0.414 Inf   9.884  <.0001
# .           pronoun    short - long      -0.25 0.230 Inf  -1.085  0.2779
# .           pronoun    short - island    -1.26 0.385 Inf  -3.276  0.0041
# .           pronoun    long - island     -1.01 0.374 Inf  -2.707  0.0136
# P value adjustment: holm method for 9 tests

# tables ...

model1 %>%
  tidy() %>%
  kbl(digits = c(2, 2, 2, 2, 3)) %>%
  remove_column(6)

pairwise %>%
  kbl(digits = c(2, 2, 2, 3)) %>%
  remove_column(6)

#------------------------------------------------------------------------------#
# + + model 2
#------------------------------------------------------------------------------#

# fit model ...

tic()
model2 <- clmm(response ~ dependency * environment + 
                 (1 + dependency * environment | participant) + 
                 (1 + dependency + environment | item), 
               data = md, control=clmm.control(grtol=1e6), Hess = TRUE) %>%
  write_rds('models/ajt_doenen_clmm_md2.rds')
summary(model2)
toc()
beep()

# 3963.31 sec elapsed
# no warnings?
#   Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# dependency2               -5.8438     0.3796 -15.393  < 2e-16 ***
# environment2              -0.5614     0.2533  -2.217 0.026648 *  
# environment3              -2.0912     0.3523  -5.936 2.92e-09 ***
# dependency2:environment2   1.6465     0.4475   3.680 0.000234 ***
# dependency2:environment3   6.7519     0.5331  12.666  < 2e-16 ***
  
# compare models ...

anova(model1, model2)

# no.par    AIC  logLik LR.stat df Pr(>Chisq)
# model2     41 5533.3 -2725.7                      
# model1     52 5553.3 -2724.7  2.0167 11     0.9984

install.packages("RVAideMemoire") 
library(RVAideMemoire) 
install.packages("car") 
library(car)

Anova(model1, type="II")

#------------------------------------------------------------------------------#
# + + model 3
#------------------------------------------------------------------------------#

# fit model ...

tic()
model3 <- clmm(response ~ dependency * environment + 
                 (1 + dependency * environment | participant) + 
                 (1 | item), 
               data = md, control=clmm.control(grtol=1e6), Hess = TRUE) %>%
  write_rds('models/ajt_doenen_clmm_md3.rds')
summary(model3)
toc()
beep()

class(model3)

# 807.76 sec elapsed (13 min)
# no warnings?
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# dependency2               -5.7936     0.3702 -15.650  < 2e-16 ***
# environment2              -0.5868     0.2478  -2.369 0.017860 *  
# environment3              -2.0901     0.3503  -5.966 2.43e-09 ***
# dependency2:environment2   1.6515     0.4448   3.713 0.000205 ***
# dependency2:environment3   6.7123     0.5298  12.669  < 2e-16 ***

# compare models ...

anova(model3, model6)

# no.par    AIC  logLik LR.stat df Pr(>Chisq)    
# model6     12 6232.5 -3104.3                          
# model3     32 5520.2 -2728.1   752.3 20  < 2.2e-16 ***

anova(model2, model3)

# no.par    AIC  logLik LR.stat df Pr(>Chisq)
# model3     32 5520.2 -2728.1                      
# model2     41 5533.3 -2725.7  4.8992  9      0.843

# post-hoc tests ...

model3 %>%
  emmeans(~ dependency * environment) %>%
  contrast('pairwise', simple = 'each', combine = TRUE) %>%
  summary(by = NULL, adjust = 'holm')

# environment dependency contrast       estimate    SE  df z.ratio p.value
# short       .          gap - pronoun     8.582 0.542 Inf  15.845  <.0001
# long        .          gap - pronoun     6.930 0.473 Inf  14.644  <.0001
# island      .          gap - pronoun     1.869 0.356 Inf   5.252  <.0001
# .           gap        short - long      1.413 0.414 Inf   3.415  0.0026
# .           gap        short - island    5.446 0.493 Inf  11.049  <.0001
# .           gap        long - island     4.034 0.407 Inf   9.909  <.0001
# .           pronoun    short - long     -0.239 0.225 Inf  -1.063  0.2879
# .           pronoun    short - island   -1.266 0.378 Inf  -3.350  0.0026
# .           pronoun    long - island    -1.027 0.360 Inf  -2.851  0.0087
# P value adjustment: holm method for 9 tests 

#------------------------------------------------------------------------------#
# + + model 4
#------------------------------------------------------------------------------#

# fit model ...

tic()
model4 <- clmm(response ~ dependency * environment + 
                 (1 + dependency + environment | participant) + 
                 (1 + dependency + environment | item), 
               data = md, control=clmm.control(grtol=1e6), Hess = TRUE) %>%
  write_rds('models/ajt_doenen_clmm_md4.rds')
summary(model4)
toc()
beep()

# 952.84 sec elapsed
# no warnings?
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# dependency2               -5.4166     0.3405 -15.907  < 2e-16 ***
# environment2              -0.6767     0.1758  -3.849 0.000119 ***
# environment3              -2.0481     0.3306  -6.195 5.81e-10 ***
# dependency2:environment2   1.5568     0.2719   5.725 1.03e-08 ***
# dependency2:environment3   5.6509     0.2888  19.568  < 2e-16 ***

anova(model3, model4)

# no.par    AIC  logLik LR.stat df Pr(>Chisq)    
# model4     30 5608.2 -2774.1                          
# model3     32 5520.2 -2728.1   91.91  2  < 2.2e-16 ***

#------------------------------------------------------------------------------#
# + + model 5
#------------------------------------------------------------------------------#

# fit model ...

tic()
model5 <- clmm(response ~ dependency * environment + 
                 (1 + dependency + environment | participant) + 
                 (1 | item), 
               data = md, control=clmm.control(grtol=1e6), Hess = TRUE) %>%
  write_rds('models/ajt_doenen_clmm_md5.rds')
summary(model5)
toc()
beep()

tic()
model5 <- clmm(response ~ dependency * environment +
                 (1 + dependency + environment | participant) +
                 (1 | item),
               data = md)
toc()
beep()

# does not converge

#------------------------------------------------------------------------------#
# + + model 6
#------------------------------------------------------------------------------#

# fit model ...

tic()
model6 <- clmm(response ~ dependency * environment + 
                  (1 | participant) + 
                  (1 | item), 
               data = md, Hess = TRUE) %>%
  write_rds('models/ajt_doenen_clmm_md6.rds')
summary(model6)
toc()
beep()

model6 <- read_rds('models/ajt_doenen_clmm_md6.rds')

# 19.08 sec elapsed
# no warnings
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# dependency2               -3.8823     0.1114 -34.848  < 2e-16 ***
# environment2              -0.6673     0.1109  -6.017 1.78e-09 ***
# environment3              -1.6386     0.1102 -14.871  < 2e-16 ***
# dependency2:environment2   1.4595     0.2222   6.570 5.04e-11 ***
# dependency2:environment3   4.9034     0.2297  21.349  < 2e-16 ***

# compare models ...

anova(model5, model6)

# pairwise comparisons ...

model6 %>%
  emmeans(~ dependency * environment) %>%
  contrast('pairwise', simple = 'each', combine = TRUE) %>%
  summary(by = NULL, adjust = 'holm')

# environment dependency contrast       estimate    SE  df z.ratio p.value
# short       .          gap - pronoun    6.0033 0.202 Inf  29.756  <.0001
# long        .          gap - pronoun    4.5437 0.166 Inf  27.433  <.0001
# island      .          gap - pronoun    1.0999 0.129 Inf   8.529  <.0001
# .           gap        short - long     1.3971 0.178 Inf   7.867  <.0001
# .           gap        short - island   4.0903 0.182 Inf  22.498  <.0001
# .           gap        long - island    2.6932 0.148 Inf  18.224  <.0001
# .           pronoun    short - long    -0.0625 0.133 Inf  -0.469  0.6391
# .           pronoun    short - island  -0.8131 0.133 Inf  -6.128  <.0001
# .           pronoun    long - island   -0.7506 0.131 Inf  -5.738  <.0001
# P value adjustment: holm method for 9 tests 

# visualize ...

emmip(model6, dependency ~ environment)

#------------------------------------------------------------------------------#
# + dokoen ----
#------------------------------------------------------------------------------#

# doenen = ORC study (do) + KLE group (ko) + English AJT (en)

# filter for analysis ...

md <- temp %>% 
  filter(study == '210510_do', 
         group == 'korean',
         task == 'english_ajt')

check <- md %>% 
  group_by(study, group, task, participant) %>%
  summarise() %>%
  ungroup()

# convert response to factor

md <- md %>%
  mutate(response = ordered(response))

# apply deviation coding ...

contrasts(md$dependency) <- contr.treatment(2) - matrix(rep(1/2, 2), ncol=1)
contrasts(md$environment) <- contr.treatment(3) - matrix(rep(1/3, 6), ncol=2)

# view contrasts ...

contrasts(md$dependency)
contrasts(md$environment)

#------------------------------------------------------------------------------#
# + + model 1
#------------------------------------------------------------------------------#

# fit model ...

tic()
model1 <- clmm(response ~ dependency * environment + 
                 (1 + dependency * environment | participant) + 
                 (1 + dependency * environment | item), 
               data = md, control=clmm.control(grtol=1e6), Hess = TRUE) %>%
  write_rds('models/ajt_dokoen_clmm_md1.rds')
summary(model1)
toc()
beep()

# Error: optimizer nlminb failed to converge
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# dependency2               -5.8542     0.3791 -15.443  < 2e-16 ***
# environment2              -0.5718     0.2566  -2.229 0.025834 *  
# environment3              -2.1130     0.3543  -5.963 2.47e-09 ***
# dependency2:environment2   1.6434     0.4507   3.646 0.000266 ***
# dependency2:environment3   6.7497     0.5432  12.425  < 2e-16 ***

#------------------------------------------------------------------------------#
# + + model 2
#------------------------------------------------------------------------------#

# fit model ...

tic()
model2 <- clmm(response ~ dependency * environment + 
                 (1 + dependency * environment | participant) + 
                 (1 + dependency + environment | item), 
               data = md, control=clmm.control(grtol=1e6), Hess = TRUE) %>%
  write_rds('models/ajt_dokoen_clmm_md2.rds')
summary(model2)
toc()
beep()

# 263.66 sec elapsed
# Error: optimizer nlminb failed to converge

#------------------------------------------------------------------------------#
# + + model 3
#------------------------------------------------------------------------------#

# fit model ...

tic()
model3 <- clmm(response ~ dependency * environment + 
                 (1 + dependency * environment | participant) + 
                 (1 | item), 
               data = md, control=clmm.control(grtol=1e6), Hess = TRUE) %>%
  write_rds('models/ajt_dokoen_clmm_md3.rds')
summary(model3)
toc()
beep()

# 91.1 sec elapsed
# Error: optimizer nlminb failed to converge

#------------------------------------------------------------------------------#
# + + model 4 (final) ----
#------------------------------------------------------------------------------#

# fit model ...

tic()
model4 <- clmm(response ~ dependency * environment + 
                 (1 + dependency + environment | participant) + 
                 (1 + dependency + environment | item), 
               data = md, control=clmm.control(grtol=1e6), Hess = TRUE) %>%
  write_rds('models/ajt_dokoen_clmm_md4.rds')
summary(model4)
toc()
beep()

model4 <- read_rds('models/ajt_dokoen_clmm_md4.rds')

# 422.53 sec elapsed
# no warnings?
# Coefficients:
# Estimate Std. Error z value Pr(>|z|)    
# dependency2               -3.1573     0.6242  -5.058 4.23e-07 ***
# environment2              -0.2709     0.1937  -1.399    0.162    
# environment3               0.1804     0.2380   0.758    0.448    
# dependency2:environment2   1.0374     0.2507   4.138 3.50e-05 ***
# dependency2:environment3   2.4998     0.2597   9.625  < 2e-16 ***

# post-hoc tests ...

pairwise <- model4 %>%
  emmeans(~ dependency * environment) %>%
  contrast('pairwise', simple = 'each', combine = TRUE) %>%
  summary(by = NULL, adjust = 'holm')
pairwise

# environment dependency contrast       estimate    SE  df z.ratio p.value
# short       .          gap - pronoun     4.336 0.647 Inf   6.702  <.0001
# long        .          gap - pronoun     3.299 0.639 Inf   5.163  <.0001
# island      .          gap - pronoun     1.837 0.637 Inf   2.885  0.0117
# .           gap        short - long      0.790 0.239 Inf   3.307  0.0038
# .           gap        short - island    1.069 0.278 Inf   3.852  0.0006
# .           gap        long - island     0.280 0.239 Inf   1.172  0.4826
# .           pronoun    short - long     -0.248 0.222 Inf  -1.114  0.4826
# .           pronoun    short - island   -1.430 0.264 Inf  -5.408  <.0001
# .           pronoun    long - island    -1.183 0.236 Inf  -5.013  <.0001

# tables ...

model4 %>%
  tidy() %>%
  kbl(digits = c(2, 2, 2, 2, 3)) %>%
  remove_column(6)

pairwise %>%
  kbl(digits = c(2, 2, 2, 2, 2, 2, 2, 3)) %>%
  remove_column(6)

#------------------------------------------------------------------------------#
# + + model 5
#------------------------------------------------------------------------------#

# fit model ...

tic()
model5 <- clmm(response ~ dependency * environment + 
                 (1 + dependency + environment | participant) + 
                 (1 | item), 
               data = md, control=clmm.control(grtol=1e6), Hess = TRUE) %>%
  write_rds('models/ajt_dokoen_clmm_md5.rds')
summary(model5)
toc()
beep()

# 256.72 sec elapsed
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# dependency2               -3.1415     0.6184  -5.080 3.78e-07 ***
# environment2              -0.2640     0.1872  -1.411    0.158    
# environment3               0.1792     0.2351   0.762    0.446    
# dependency2:environment2   1.0262     0.2489   4.123 3.73e-05 ***
# dependency2:environment3   2.4868     0.2578   9.647  < 2e-16 ***

# compare models ...

anova(model4, model5)

# model5     21 4750.9 -2354.4                      
# model4     30 4767.3 -2353.7  1.5345  9     0.9969

#------------------------------------------------------------------------------#
# + + model 6
#------------------------------------------------------------------------------#

# fit model ...

tic()
model6 <- clmm(response ~ dependency * environment + 
                 (1 | participant) + 
                 (1 | item), 
               data = md, control=clmm.control(grtol=1e6), Hess = TRUE) %>%
  write_rds('models/ajt_dokoen_clmm_md6.rds')
summary(model6)
toc()
beep()

# 21.58 sec elapsed
# no warnings?
#   Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# dependency2              -1.61107    0.09002 -17.897  < 2e-16 ***
# environment2             -0.10419    0.10652  -0.978  0.32797    
# environment3              0.18816    0.10602   1.775  0.07593 .  
# dependency2:environment2  0.68207    0.21317   3.200  0.00138 ** 
# dependency2:environment3  1.61348    0.21335   7.563 3.95e-14 *** 

# compare models ...

anova(model5, model6)

# no.par    AIC  logLik LR.stat df Pr(>Chisq)    
# model6     12 5744.0 -2860.0                          
# model5     21 4750.9 -2354.4  1011.2  9  < 2.2e-16 ***

#------------------------------------------------------------------------------#
# + dozhen ----
#------------------------------------------------------------------------------#

# doenen = ORC study (do) + MLE group (zh) + English AJT (en)

# filter for analysis ...

md <- temp %>% 
  filter(study == '210510_do', 
         group == 'mandarin',
         task == 'english_ajt')

check <- md %>% 
  group_by(study, group, participant) %>%
  summarise() %>%
  ungroup()

# convert response to factor

md <- md %>%
  mutate(response = ordered(response))

# apply deviation coding ...

contrasts(md$dependency) <- contr.treatment(2) - matrix(rep(1/2, 2), ncol=1)
contrasts(md$environment) <- contr.treatment(3) - matrix(rep(1/3, 6), ncol=2)

# view contrasts ...

contrasts(md$dependency)
contrasts(md$environment)

#------------------------------------------------------------------------------#
# + + model 1 (final) ----
#------------------------------------------------------------------------------#

# fit model ...

tic()
model1 <- clmm(response ~ dependency * environment + 
                 (1 + dependency * environment | participant) + 
                 (1 + dependency * environment | item), 
               data = md, control=clmm.control(grtol=1e6), Hess = TRUE) %>%
  write_rds('models/ajt_dozhen_clmm_md1.rds')
summary(model1)
toc()
beep()

model1 <- read_rds('models/ajt_dozhen_clmm_md1.rds')

# 3857.24 sec elapsed
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# dependency2               -2.2364     0.4093  -5.464 4.66e-08 ***
# environment2              -0.4381     0.2507  -1.747 0.080600 .  
# environment3              -0.5533     0.2351  -2.354 0.018593 *  
# dependency2:environment2   0.7095     0.3366   2.108 0.035060 *  
# dependency2:environment3   1.4366     0.3980   3.610 0.000306 ***

# post-hoc tests ...

pairwise <- model1 %>%
  emmeans(~ dependency * environment) %>%
  contrast('pairwise', simple = 'each', combine = TRUE) %>%
  summary(by = NULL, adjust = 'holm')
pairwise

# environment dependency contrast       estimate    SE  df z.ratio p.value
# short       .          gap - pronoun    2.9517 0.506 Inf   5.837  <.0001
# long        .          gap - pronoun    2.2423 0.437 Inf   5.135  <.0001
# island      .          gap - pronoun    1.5151 0.426 Inf   3.556  0.0023
# .           gap        short - long     0.7928 0.339 Inf   2.340  0.0965
# .           gap        short - island   1.2716 0.346 Inf   3.670  0.0017
# .           gap        long - island    0.4787 0.297 Inf   1.612  0.4283
# .           pronoun    short - long     0.0834 0.260 Inf   0.321  1.0000
# .           pronoun    short - island  -0.1651 0.264 Inf  -0.625  1.0000
# .           pronoun    long - island   -0.2484 0.207 Inf  -1.199  0.6912
# P value adjustment: holm method for 9 tests

# tables ...

model1 %>%
  tidy() %>%
  kbl(digits = c(2, 2, 2, 2, 3)) %>%
  remove_column(6)

pairwise %>%
  kbl(digits = c(2, 2, 2, 2, 2, 2, 2, 3)) %>%
  remove_column(6)

#------------------------------------------------------------------------------#
# + dokoko ----
#------------------------------------------------------------------------------#

# doenen = ORC study (do) + KLE group (ko) + Korean AJT (ko)

# filter for analysis ...

md <- temp %>% 
  filter(study == '210510_do', 
         group == 'korean',
         task == 'korean_ajt')

check <- md %>% 
  group_by(study, group, participant) %>%
  summarise() %>%
  ungroup()

# convert response to factor

md <- md %>%
  mutate(response = ordered(response))

# apply deviation coding ...

contrasts(md$dependency) <- contr.treatment(2) - matrix(rep(1/2, 2), ncol=1)
contrasts(md$environment) <- contr.treatment(3) - matrix(rep(1/3, 6), ncol=2)

# view contrasts ...

contrasts(md$dependency)
contrasts(md$environment)

#------------------------------------------------------------------------------#
# + + model 1 (final) ----
#------------------------------------------------------------------------------#

# fit model ...

tic()
model1 <- clmm(response ~ dependency * environment + 
                 (1 + dependency * environment | participant) + 
                 (1 + dependency * environment | item), 
               data = md, control=clmm.control(grtol=1e6), Hess = TRUE) %>%
  write_rds('models/ajt_dokoko_clmm_md1.rds')
summary(model1)
toc()
beep()

model1 <- read_rds('models/ajt_dokoko_clmm_md1.rds')

class(model1)

# 5593.59 sec elapsed (93 min)
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# dependency2               -3.4746     0.3705  -9.377  < 2e-16 ***
# environment2              -0.8452     0.2924  -2.890  0.00385 ** 
# environment3              -0.3697     0.3025  -1.222  0.22172    
# dependency2:environment2   5.5128     0.5947   9.270  < 2e-16 ***
# dependency2:environment3   6.9773     0.6469  10.785  < 2e-16 ***

# post-hoc tests ...

pairwise <- model1 %>%
  emmeans(~ dependency * environment) %>%
  contrast('pairwise', simple = 'each', combine = TRUE) %>%
  summary(by = NULL, adjust = 'holm')
pairwise

# environment dependency contrast       estimate    SE  df z.ratio p.value
# short       .          gap - pronoun     7.638 0.683 Inf  11.187  <.0001
# long        .          gap - pronoun     2.125 0.363 Inf   5.850  <.0001
# island      .          gap - pronoun     0.661 0.322 Inf   2.049  0.0809
# .           gap        short - long      3.602 0.460 Inf   7.831  <.0001
# .           gap        short - island    3.858 0.476 Inf   8.108  <.0001
# .           gap        long - island     0.257 0.249 Inf   1.033  0.3017
# .           pronoun    short - long     -1.911 0.369 Inf  -5.176  <.0001
# .           pronoun    short - island   -3.119 0.407 Inf  -7.658  <.0001
# .           pronoun    long - island    -1.208 0.290 Inf  -4.162  0.0001
# P value adjustment: holm method for 9 tests 

# tables ...

model1 %>%
  tidy() %>%
  kbl(digits = c(2, 2, 2, 2, 3)) %>%
  remove_column(6)

pairwise %>%
  kbl(digits = c(2, 2, 2, 2, 2, 2, 2, 3)) %>%
  remove_column(6)

#------------------------------------------------------------------------------#
# + dozhzh ----
#------------------------------------------------------------------------------#

# doenen = ORC study (do) + MLE group (zh) + Mandarin AJT (zh)

# filter for analysis ...

md <- temp %>% 
  filter(study == '210510_do', 
         group == 'mandarin',
         task == 'mandarin_ajt')

check <- md %>% 
  group_by(study, group, participant) %>%
  summarise() %>%
  ungroup()

# convert response to factor

md <- md %>%
  mutate(response = ordered(response))

# apply deviation coding ...

contrasts(md$dependency) <- contr.treatment(2) - matrix(rep(1/2, 2), ncol=1)
contrasts(md$environment) <- contr.treatment(3) - matrix(rep(1/3, 6), ncol=2)

# view contrasts ...

contrasts(md$dependency)
contrasts(md$environment)

#------------------------------------------------------------------------------#
# + + model 1 (final) ----
#------------------------------------------------------------------------------#

# fit model ...

tic()
model1 <- clmm(response ~ dependency * environment + 
                 (1 + dependency * environment | participant) + 
                 (1 + dependency * environment | item), 
               data = md, control=clmm.control(grtol=1e6), Hess = TRUE) %>%
  write_rds('models/ajt_dozhzh_clmm_md1.rds')
summary(model1)
toc()
beep()

model1 <- read_rds('models/ajt_dozhzh_clmm_md1.rds')

# 3246.32 sec elapsed (54 min)
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# dependency2               -1.1400     0.2311  -4.934 8.05e-07 ***
# environment2               0.1582     0.2162   0.732 0.464400    
# environment3              -0.7796     0.2396  -3.254 0.001137 ** 
# dependency2:environment2   1.1921     0.3198   3.728 0.000193 ***
# dependency2:environment3   2.1916     0.3269   6.704 2.02e-11 ***

# post-hoc tests ...

pairwise <- model1 %>%
  emmeans(~ dependency * environment) %>%
  contrast('pairwise', simple = 'each', combine = TRUE) %>%
  summary(by = NULL, adjust = 'holm')
pairwise

# environment dependency contrast       estimate    SE  df z.ratio p.value
# short       .          gap - pronoun    2.2680 0.372 Inf   6.094  <.0001
# long        .          gap - pronoun    1.0758 0.283 Inf   3.797  0.0009
# island      .          gap - pronoun    0.0764 0.209 Inf   0.366  0.7146
# .           gap        short - long     0.4379 0.305 Inf   1.434  0.4548
# .           gap        short - island   1.8754 0.322 Inf   5.818  <.0001
# .           gap        long - island    1.4376 0.256 Inf   5.607  <.0001
# .           pronoun    short - long    -0.7543 0.227 Inf  -3.327  0.0044
# .           pronoun    short - island  -0.3162 0.254 Inf  -1.247  0.4548
# .           pronoun    long - island    0.4381 0.233 Inf   1.883  0.2387

# tables ...

model1 %>%
  tidy() %>%
  kbl(digits = c(2, 2, 2, 2, 3)) %>%
  remove_column(6)

pairwise %>%
  kbl(digits = c(2, 2, 2, 2, 2, 2, 2, 3)) %>%
  remove_column(6)

#------------------------------------------------------------------------------#
# + suenen ----
#------------------------------------------------------------------------------#

# suenen = SRC study (su) + ENS group (en) + English AJT (en)

# load package ...

library(ordinal)

# filter for analysis ...

md <- temp %>% 
  filter(study == '210510_su', 
         group == 'english',
         task == 'english_ajt')

check <- md %>% 
  group_by(study, group, participant) %>%
  summarise() %>%
  ungroup()

# convert response to factor

md <- md %>%
  mutate(response = ordered(response))

# apply deviation coding ...

contrasts(md$dependency) <- contr.treatment(2) - matrix(rep(1/2, 2), ncol=1)
contrasts(md$environment) <- contr.treatment(3) - matrix(rep(1/3, 6), ncol=2)

# view contrasts ...

contrasts(md$dependency)
contrasts(md$environment)

#------------------------------------------------------------------------------#
# + + model 1 
#------------------------------------------------------------------------------#

# fit model ...

tic()
model1 <- clmm(response ~ dependency * environment + 
                 (1 + dependency * environment | participant) + 
                 (1 + dependency * environment | item), 
               data = md, control=clmm.control(grtol=1e6)) %>%
  write_rds('models/ajt_suenen_clmm_md1.rds')
summary(model1)
toc()
beep()

model1 <- read_rds('models/ajt_suenen_clmm_md1.rds')

# 8168.43 sec elapsed
# In summary.clmm(model1) :
#   Variance-covariance matrix of the parameters is not defined

#------------------------------------------------------------------------------#
# + + model 2
#------------------------------------------------------------------------------#

# fit model ...

tic()
model2 <- clmm(response ~ dependency * environment + 
                 (1 + dependency * environment | participant) + 
                 (1 + dependency + environment | item), 
               data = md, control=clmm.control(grtol=1e6)) %>%
  write_rds('models/ajt_suenen_clmm_md2.rds')
summary(model2)
toc()
beep()

# 3191.14 sec elapsed
# Warning message:
#   In summary.clmm(model2) :
#   Variance-covariance matrix of the parameters is not defined

#------------------------------------------------------------------------------#
# + + model 3
#------------------------------------------------------------------------------#

# fit model ...

tic()
model3 <- clmm(response ~ dependency * environment + 
                 (1 + dependency * environment | participant) + 
                 (1 | item), 
               data = md, control=clmm.control(grtol=1e6)) %>%
  write_rds('models/ajt_suenen_clmm_md3.rds')
summary(model3)
toc()
beep()

# 381.79 sec elapsed
# In summary.clmm(model3) :
#   Variance-covariance matrix of the parameters is not defined

#------------------------------------------------------------------------------#
# + + model 4 (final?) ----
#------------------------------------------------------------------------------#

# fit model ...

tic()
model4 <- clmm(response ~ dependency * environment + 
                 (1 + dependency + environment | participant) + 
                 (1 + dependency + environment | item), 
               data = md, control=clmm.control(grtol=1e6)) %>%
  write_rds('models/ajt_suenen_clmm_md4.rds')
summary(model4)
toc()
beep()

# 439.31 sec elapsed
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# dependency2               -4.5712     0.2841 -16.092  < 2e-16 ***
# environment2              -1.0337     0.2080  -4.969 6.73e-07 ***
# environment3              -3.9264     0.3461 -11.345  < 2e-16 ***
# dependency2:environment2   2.9205     0.3919   7.452 9.18e-14 ***
# dependency2:environment3  10.1551     0.4700  21.604  < 2e-16 ***

# post-hoc tests ...

pairwise <- model4 %>%
  emmeans(~ dependency * environment) %>%
  contrast('pairwise', simple = 'each', combine = TRUE) %>%
  summary(by = NULL, adjust = 'holm')
pairwise

# environment dependency contrast       estimate    SE  df z.ratio p.value
# short       .          gap - pronoun     8.930 0.448 Inf  19.949  <.0001
# long        .          gap - pronoun     6.009 0.343 Inf  17.496  <.0001
# island      .          gap - pronoun    -1.225 0.301 Inf  -4.072  0.0002
# .           gap        short - long      2.494 0.319 Inf   7.807  <.0001
# .           gap        short - island    9.004 0.477 Inf  18.888  <.0001
# .           gap        long - island     6.510 0.362 Inf  17.986  <.0001
# .           pronoun    short - long     -0.426 0.248 Inf  -1.722  0.0850
# .           pronoun    short - island   -1.151 0.350 Inf  -3.285  0.0031
# .           pronoun    long - island    -0.725 0.309 Inf  -2.349  0.0377
# P value adjustment: holm method for 9 tests

# tables ...

model4 %>%
  tidy() %>%
  kbl(digits = c(2, 2, 2, 2, 3)) %>%
  remove_column(6)

pairwise %>%
  kbl(digits = c(2, 2, 2, 2, 2, 2, 2, 3)) %>%
  remove_column(6)

#------------------------------------------------------------------------------#
# + + model 5
#------------------------------------------------------------------------------#

# fit model ...

tic()
model5 <- clmm(response ~ dependency * environment + 
                 (1 + dependency + environment | participant) + 
                 (1 | item), 
               data = md, control=clmm.control(grtol=1e6)) %>%
  write_rds('models/ajt_suenen_clmm_md5.rds')
summary(model5)
toc()
beep()

# 217.5 sec elapsed
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# dependency2               -4.5563     0.2853 -15.970  < 2e-16 ***
# environment2              -1.0206     0.2024  -5.042 4.61e-07 ***
# environment3              -3.9052     0.3437 -11.362  < 2e-16 ***
# dependency2:environment2   2.9390     0.3850   7.635 2.27e-14 ***
# dependency2:environment3  10.1142     0.4660  21.704  < 2e-16 ***

# compare models ...

anova(model4, model5)

#        no.par    AIC  logLik LR.stat df Pr(>Chisq)
# model5     21 3206.1 -1582.0                      
# model4     30 3221.8 -1580.9  2.3157  9     0.9854

#------------------------------------------------------------------------------#
# + + model 6
#------------------------------------------------------------------------------#

# fit model ...

tic()
model6 <- clmm(response ~ dependency * environment + 
                 (1 | participant) + 
                 (1 | item), 
               data = md, control=clmm.control(grtol=1e6)) %>%
  write_rds('models/ajt_suenen_clmm_md6.rds')
summary(model6)
toc()
beep()

# 12.73 sec elapsed
# no warnings
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# dependency2               -4.0739     0.1621  -25.14  < 2e-16 ***
# environment2              -1.0084     0.1573   -6.41 1.46e-10 ***
# environment3              -3.4756     0.1798  -19.33  < 2e-16 ***
# dependency2:environment2   3.0050     0.3170    9.48  < 2e-16 ***
# dependency2:environment3   9.7638     0.3899   25.04  < 2e-16 ***

# compare models ...

anova(model5, model6)

# no.par    AIC  logLik LR.stat df Pr(>Chisq)    
# model6     12 3307.8 -1641.9                          
# model5     21 3206.1 -1582.0  119.74  9  < 2.2e-16 ***
  
#------------------------------------------------------------------------------#
# + sukoen ----
#------------------------------------------------------------------------------#

# sukoen = SRC study (su) + KLE group (ko) + English AJT (en)

# filter for analysis ...

md <- temp %>% 
  filter(study == '210510_su', 
         group == 'korean',
         task == 'english_ajt')

check <- md %>% 
  group_by(study, group, participant) %>%
  summarise() %>%
  ungroup()

# convert response to factor

md <- md %>%
  mutate(response = ordered(response))

# apply deviation coding ...

contrasts(md$dependency) <- contr.treatment(2) - matrix(rep(1/2, 2), ncol=1)
contrasts(md$environment) <- contr.treatment(3) - matrix(rep(1/3, 6), ncol=2)

# view contrasts ...

contrasts(md$dependency)
contrasts(md$environment)

#------------------------------------------------------------------------------#
# + + model 1 (final) ----
#------------------------------------------------------------------------------#

# fit model ...

tic()
model1 <- clmm(response ~ dependency * environment + 
                 (1 + dependency * environment | participant) + 
                 (1 + dependency * environment | item), 
               data = md, control=clmm.control(grtol=1e6)) %>%
  write_rds('models/ajt_sukoen_clmm_md1.rds')
summary(model1)
toc()
beep()

model1 <- read_rds('models/ajt_sukoen_clmm_md1.rds')

# 10246.95 sec elapsed (171 min)
# no warnings
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# dependency2               -2.0987     0.4301  -4.880 1.06e-06 ***
#   environment2              -1.0019     0.3453  -2.901  0.00372 ** 
#   environment3              -1.6287     0.3585  -4.544 5.53e-06 ***
#   dependency2:environment2   5.2669     0.6599   7.981 1.45e-15 ***
#   dependency2:environment3   9.1706     0.8962  10.232  < 2e-16 ***
  
# post-hoc tests ...

pairwise <- model1 %>%
  emmeans(~ dependency * environment) %>%
  contrast('pairwise', simple = 'each', combine = TRUE) %>%
  summary(by = NULL, adjust = 'holm')
pairwise

# environment dependency contrast       estimate    SE  df z.ratio p.value
# short       .          gap - pronoun      6.91 0.756 Inf   9.143  <.0001
# long        .          gap - pronoun      1.64 0.591 Inf   2.782  0.0054
# island      .          gap - pronoun     -2.26 0.410 Inf  -5.505  <.0001
# .           gap        short - long       3.64 0.553 Inf   6.580  <.0001
# .           gap        short - island     6.21 0.650 Inf   9.565  <.0001
# .           gap        long - island      2.58 0.459 Inf   5.617  <.0001
# .           pronoun    short - long      -1.63 0.389 Inf  -4.199  0.0001
# .           pronoun    short - island    -2.96 0.486 Inf  -6.079  <.0001
# .           pronoun    long - island     -1.33 0.245 Inf  -5.416  <.0001
# P value adjustment: holm method for 9 tests 

# tables ...

model1 %>%
  tidy() %>%
  kbl(digits = c(2, 2, 2, 2, 3)) %>%
  remove_column(6)

pairwise %>%
  kbl(digits = c(2, 2, 2, 2, 2, 2, 2, 3)) %>%
  remove_column(6)

#------------------------------------------------------------------------------#
# + suzhen ----
#------------------------------------------------------------------------------#

# suzhen = SRC study (su) + MLE group (zh) + English AJT (en)

# filter for analysis ...

md <- temp %>% 
  filter(study == '210510_su', 
         group == 'mandarin',
         task == 'english_ajt')

check <- md %>% 
  group_by(study, group, participant) %>%
  summarise() %>%
  ungroup()

# convert response to factor

md <- md %>%
  mutate(response = ordered(response))

# apply deviation coding ...

contrasts(md$dependency) <- contr.treatment(2) - matrix(rep(1/2, 2), ncol=1)
contrasts(md$environment) <- contr.treatment(3) - matrix(rep(1/3, 6), ncol=2)

# view contrasts ...

contrasts(md$dependency)
contrasts(md$environment)

#------------------------------------------------------------------------------#
# + + model 1 (final) ----
#------------------------------------------------------------------------------#

# fit model ...

tic()
model1 <- clmm(response ~ dependency * environment + 
                 (1 + dependency * environment | participant) + 
                 (1 + dependency * environment | item), 
               data = md, control=clmm.control(grtol=1e6)) %>%
  write_rds('models/ajt_suzhen_clmm_md1.rds')
summary(model1)
toc()
beep()

model1 <- read_rds('models/ajt_suzhen_clmm_md1.rds')

# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# dependency2                0.7984     0.2491   3.205 0.001351 ** 
# environment2              -1.0650     0.2870  -3.711 0.000206 ***
# environment3              -2.1160     0.2853  -7.417 1.19e-13 ***
# dependency2:environment2   4.3907     0.4721   9.300  < 2e-16 ***
# dependency2:environment3   5.9950     0.7094   8.451  < 2e-16 ***

# post-hoc tests ...

pairwise <- model1 %>%
  emmeans(~ dependency * environment) %>%
  contrast('pairwise', simple = 'each', combine = TRUE) %>%
  summary(by = NULL, adjust = 'holm')
pairwise

# environment dependency contrast       estimate    SE  df z.ratio p.value
# short       .          gap - pronoun     2.663 0.403 Inf   6.611  <.0001
# long        .          gap - pronoun    -1.727 0.395 Inf  -4.376  <.0001
# island      .          gap - pronoun    -3.331 0.474 Inf  -7.030  <.0001
# .           gap        short - long      3.260 0.411 Inf   7.927  <.0001
# .           gap        short - island    5.114 0.498 Inf  10.265  <.0001
# .           gap        long - island     1.853 0.380 Inf   4.878  <.0001
# .           pronoun    short - long     -1.130 0.327 Inf  -3.455  0.0016
# .           pronoun    short - island   -0.881 0.408 Inf  -2.162  0.0612
# .           pronoun    long - island     0.249 0.387 Inf   0.643  0.5203
# P value adjustment: holm method for 9 tests 

#------------------------------------------------------------------------------#
# + sukoko ----
#------------------------------------------------------------------------------#

# sukoko = SRC study (su) + KLE group (ko) + Korean AJT (ko)

# filter for analysis ...

md <- temp %>% 
  filter(study == '210510_su', 
         group == 'korean',
         task == 'korean_ajt')

check <- md %>% 
  group_by(study, group, participant) %>%
  summarise() %>%
  ungroup()

# convert response to factor

md <- md %>%
  mutate(response = ordered(response))

# apply deviation coding ...

contrasts(md$dependency) <- contr.treatment(2) - matrix(rep(1/2, 2), ncol=1)
contrasts(md$environment) <- contr.treatment(3) - matrix(rep(1/3, 6), ncol=2)

# view contrasts ...

contrasts(md$dependency)
contrasts(md$environment)

#------------------------------------------------------------------------------#
# + + model 1 
#------------------------------------------------------------------------------#

# fit model ...

tic()
model1 <- clmm(response ~ dependency * environment + 
                 (1 + dependency * environment | participant) + 
                 (1 + dependency * environment | item), 
               data = md, control=clmm.control(grtol=1e6)) %>%
  write_rds('models/ajt_sukoko_clmm_md1.rds')
summary(model1)
toc()
beep()

# Error: optimizer nlminb failed to converge

#------------------------------------------------------------------------------#
# + + model 2
#------------------------------------------------------------------------------#

# fit model ...

tic()
model2 <- clmm(response ~ dependency * environment + 
                 (1 + dependency * environment | participant) + 
                 (1 + dependency + environment | item), 
               data = md, control=clmm.control(grtol=1e6)) %>%
  write_rds('models/ajt_sukoko_clmm_md2.rds')
summary(model2)
toc()
beep()

# 339.42 sec elapsed
# Error: optimizer nlminb failed to converge
# Warning message:
#   In summary.clmm(model2) :
#   Variance-covariance matrix of the parameters is not defined

#------------------------------------------------------------------------------#
# + + model 3
#------------------------------------------------------------------------------#

# fit model ...

tic()
model3 <- clmm(response ~ dependency * environment + 
                 (1 + dependency * environment | participant) + 
                 (1 | item), 
               data = md, control=clmm.control(grtol=1e6)) %>%
  write_rds('models/ajt_sukoko_clmm_md3.rds')
summary(model3)
toc()
beep()

# 114.56 sec elapsed
# Warning message:
#   In summary.clmm(model3) :
#   Variance-covariance matrix of the parameters is not defined

#------------------------------------------------------------------------------#
# + + model 4 (final?) ----
#------------------------------------------------------------------------------#

# fit model ...

tic()
model4 <- clmm(response ~ dependency * environment + 
                 (1 + dependency + environment | participant) + 
                 (1 + dependency + environment | item), 
               data = md, control=clmm.control(grtol=1e6)) %>%
  write_rds('models/ajt_sukoko_clmm_md4.rds')
summary(model4)
toc()
beep()

# Error: optimizer nlminb failed to converge
# 277.03 sec elapsed
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# dependency2               -4.5712     0.2841 -16.092  < 2e-16 ***
# environment2              -1.0337     0.2080  -4.969 6.73e-07 ***
# environment3              -3.9264     0.3461 -11.345  < 2e-16 ***
# dependency2:environment2   2.9205     0.3919   7.452 9.18e-14 ***
# dependency2:environment3  10.1551     0.4700  21.604  < 2e-16 ***

#------------------------------------------------------------------------------#
# + + model 5
#------------------------------------------------------------------------------#

# fit model ...

tic()
model5 <- clmm(response ~ dependency * environment + 
                 (1 + dependency + environment | participant) + 
                 (1 | item), 
               data = md, control=clmm.control(grtol=1e6)) %>%
  write_rds('models/ajt_sukoko_clmm_md5.rds')
summary(model5)
toc()
beep()

# 328.39 sec elapsed
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# dependency2              -3.59565    0.32156 -11.182   <2e-16 ***
# environment2             -0.08796    0.40177  -0.219    0.827    
# environment3             -0.38573    0.41238  -0.935    0.350    
# dependency2:environment2  5.62644    0.30213  18.623   <2e-16 ***
# dependency2:environment3  5.76669    0.30450  18.938   <2e-16 ***

#------------------------------------------------------------------------------#
# + + model 6
#------------------------------------------------------------------------------#

# fit model ...

tic()
model6 <- clmm(response ~ dependency * environment + 
                 (1 | participant) + 
                 (1 | item), 
               data = md, control=clmm.control(grtol=1e6)) %>%
  write_rds('models/ajt_sukoko_clmm_md6.rds')
summary(model6)
toc()
beep()

# 12.78 sec elapsed
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# dependency2               -2.6548     0.1107 -23.978  < 2e-16 ***
# environment2              -0.3936     0.1298  -3.032 0.002431 ** 
# environment3              -0.4908     0.1297  -3.783 0.000155 ***
# dependency2:environment2   5.3694     0.2761  19.444  < 2e-16 ***
# dependency2:environment3   5.4392     0.2761  19.703  < 2e-16 ***
  
# compare models ...

anova(model5, model6)

# no.par    AIC  logLik LR.stat df Pr(>Chisq)    
# model6     12 5198.3 -2587.2                          
# model5     21 4728.2 -2343.1  488.16  9  < 2.2e-16 ***

#------------------------------------------------------------------------------#
# + suzhzh ----
#------------------------------------------------------------------------------#

# suzhzh = SRC study (su) + MLE group (zh) + Mandarin AJT (zh)

# filter for analysis ...

md <- temp %>% 
  filter(study == '210510_su', 
         group == 'mandarin',
         task == 'mandarin_ajt')

check <- md %>% 
  group_by(study, group, participant) %>%
  summarise() %>%
  ungroup()

# convert response to factor

md <- md %>%
  mutate(response = ordered(response))

# apply deviation coding ...

contrasts(md$dependency) <- contr.treatment(2) - matrix(rep(1/2, 2), ncol=1)
contrasts(md$environment) <- contr.treatment(3) - matrix(rep(1/3, 6), ncol=2)

# view contrasts ...

contrasts(md$dependency)
contrasts(md$environment)

#------------------------------------------------------------------------------#
# + + model 1 (final?) ----
#------------------------------------------------------------------------------#

# fit model ...

tic()
model1 <- clmm(response ~ dependency * environment + 
                 (1 + dependency * environment | participant) + 
                 (1 + dependency * environment | item), 
               data = md, control=clmm.control(grtol=1e6)) %>%
  write_rds('models/ajt_suzhzh_clmm_md1.rds')
summary(model1)
toc()
beep()

model1 <- read_rds('models/ajt_suzhzh_clmm_md1.rds')

# 7180.05 sec elapsed
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# dependency2               -1.3274     0.1853  -7.162 7.95e-13 ***
# environment2              -0.6532     0.3016  -2.166   0.0303 *  
# environment3              -1.5109     0.3393  -4.453 8.46e-06 ***
# dependency2:environment2   3.5096     0.4161   8.434  < 2e-16 ***
# dependency2:environment3   4.9257     0.4953   9.945  < 2e-16 ***

# post-hoc tests ...

pairwise <- model1 %>%
  emmeans(~ dependency * environment) %>%
  contrast('pairwise', simple = 'each', combine = TRUE) %>%
  summary(by = NULL, adjust = 'holm')
pairwise

# environment dependency contrast       estimate    SE  df z.ratio p.value
# short       .          gap - pronoun     4.139 0.420 Inf   9.857  <.0001
# long        .          gap - pronoun     0.630 0.198 Inf   3.180  0.0059
# island      .          gap - pronoun    -0.786 0.238 Inf  -3.300  0.0048
# .           gap        short - long      2.408 0.361 Inf   6.665  <.0001
# .           gap        short - island    3.974 0.430 Inf   9.243  <.0001
# .           gap        long - island     1.566 0.379 Inf   4.136  0.0002
# .           pronoun    short - long     -1.102 0.371 Inf  -2.966  0.0091
# .           pronoun    short - island   -0.952 0.410 Inf  -2.322  0.0404
# .           pronoun    long - island     0.150 0.284 Inf   0.527  0.5979
# P value adjustment: holm method for 9 tests 

# tables ...

model1 %>%
  tidy() %>%
  kbl(digits = c(2, 2, 2, 2, 3)) %>%
  remove_column(6)

pairwise %>%
  kbl(digits = c(2, 2, 2, 2, 2, 2, 2, 3)) %>%
  remove_column(6)

#------------------------------------------------------------------------------#
# modeling - glmer - critical ----
#------------------------------------------------------------------------------#

# prep data

temp <- ajt %>%
  filter(!condition %in% c('grammatical', 'ungrammatical')) %>%
  mutate(environment = fct_relevel(environment, 'short', 'long', 'island'),
         dependency = as.factor(dependency))

#------------------------------------------------------------------------------#
# + doenen ----
#------------------------------------------------------------------------------#

# doenen = ORC study (do) + ENS group (en) + English AJT (en)

# filter for analysis ...

md <- temp %>% 
  filter(study == '210510_do', 
         group == 'english',
         task == 'english_ajt')

check <- md %>% 
  group_by(study, group, participant) %>%
  summarise() %>%
  ungroup()

# apply deviation coding ...

contrasts(md$dependency) <- contr.treatment(2) - matrix(rep(1/2, 2), ncol=1)
contrasts(md$environment) <- contr.treatment(3) - matrix(rep(1/3, 6), ncol=2)

# view contrasts ...

contrasts(md$dependency)
contrasts(md$environment)

#------------------------------------------------------------------------------#
# + + model 1 (maximal model)
#------------------------------------------------------------------------------#

# fit model ...

tic()
model1 <- glmer(acceptance ~ dependency * environment + 
                  (1 + dependency * environment | participant) + 
                  (1 + dependency * environment | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl=list(maxfun = 1e6))) %>%
  write_rds('models/ajt_doenen_acc_md1.rds')
summary(model1)
toc()
beep()

model1 <- read_rds('models/ajt_doenen_acc_md1.rds')
  
# 1172.58 sec elapsed (19 min)
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#                          Estimate Std. Error z value Pr(>|z|)    
# (Intercept)               -0.2264     0.2909  -0.778    0.436    
# dependency2               -5.7529     0.4877 -11.796  < 2e-16 ***
# environment2              -0.3750     0.5423  -0.692    0.489    
# environment3              -2.2321     0.5450  -4.096 4.21e-05 ***
# dependency2:environment2   0.3864     1.0592   0.365    0.715    
# dependency2:environment3   5.2693     0.9606   5.485 4.12e-08 ***

#------------------------------------------------------------------------------#
# + + model 2
#------------------------------------------------------------------------------#

# fit model ...

tic()
model2 <- glmer(acceptance ~ dependency * environment + 
                  (1 + dependency * environment | participant) + 
                  (1 + dependency + environment | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl=list(maxfun = 1e6))) %>%
  write_rds('models/ajt_doenen_acc_md2.rds')
summary(model2)
toc()
beep()

# 486.71 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)               -0.1741     0.2819  -0.618    0.537    
# dependency2               -5.7319     0.4773 -12.009  < 2e-16 ***
# environment2              -0.3469     0.5124  -0.677    0.498    
# environment3              -2.1449     0.5000  -4.290 1.79e-05 ***
# dependency2:environment2   0.3507     0.9950   0.352    0.724    
# dependency2:environment3   5.5125     0.8607   6.405 1.50e-10 ***

# compare models ...

anova(model2, model1)

# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model2   37 1686.7 1904.6 -806.37   1612.7                     
# model1   48 1706.4 1989.0 -805.22   1610.4 2.3102 11     0.9971

anova(model2, model7)

# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)    
# model7    8 1817.1 1864.2 -900.55   1801.1                         
# model2   37 1686.7 1904.6 -806.37   1612.7 188.36 29  < 2.2e-16 ***

#------------------------------------------------------------------------------#
# + + model 3
#------------------------------------------------------------------------------#

# fit model ...

tic()
model3 <- glmer(acceptance ~ dependency * environment + 
                  (1 + dependency + environment | participant) + 
                  (1 + dependency + environment | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl=list(maxfun = 1e6))) %>%
  write_rds('models/ajt_doenen_acc_md3.rds')
summary(model3)
toc()
beep()

# 81.66 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)               -0.2878     0.2560  -1.124  0.26097    
# dependency2               -5.4659     0.4041 -13.525  < 2e-16 ***
# environment2              -0.8106     0.3119  -2.599  0.00936 ** 
# environment3              -2.2534     0.4132  -5.453 4.95e-08 ***
# dependency2:environment2   0.8108     0.6278   1.292  0.19650    
# dependency2:environment3   5.1341     0.5750   8.928  < 2e-16 ***

# compare models ...

anova (model3, model2)

# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)  
# model3   26 1685.3 1838.4 -816.64   1633.3                       
# model2   37 1686.7 1904.6 -806.37   1612.7 20.537 11     0.0385 *

#------------------------------------------------------------------------------#
# + + model 4 (final) ----
#------------------------------------------------------------------------------#

# fit model ...

tic()
model4 <- glmer(acceptance ~ dependency * environment + 
                  (1 + dependency * environment | participant) + 
                  (1 | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl=list(maxfun = 1e6))) %>%
  write_rds('models/ajt_doenen_acc_md4.rds')
summary(model4)
toc()
beep()

# 62.87 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)               -0.2266     0.2728  -0.831    0.406    
# dependency2               -5.5815     0.4465 -12.500  < 2e-16 ***
# environment2              -0.4215     0.4944  -0.853    0.394    
# environment3              -2.0467     0.4858  -4.213 2.52e-05 ***
# dependency2:environment2   0.4817     0.9571   0.503    0.615    
# dependency2:environment3   5.4253     0.8499   6.383 1.73e-10 ***

# compare models ...

anova(model2, model4)

# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model4   28 1673.2 1838.1 -808.61   1617.2                     
# model2   37 1686.7 1904.6 -806.37   1612.7 4.4896  9     0.8763

# check assumptions ...

check_model(model4)
ggsave('plots/check_model.png', width=10, height=10, dpi=600)

# pairwise comparisons ...

model4 %>%
  emmeans(~ dependency * environment) %>%
  contrast('pairwise', by = 'environment') %>%
  summary(by = NULL, adjust = 'holm')

# contrast      environment estimate    SE  df z.ratio p.value
# gap - pronoun short           7.45 0.577 Inf  12.911  <.0001
# gap - pronoun long            6.64 0.557 Inf  11.922  <.0001
# gap - pronoun island          2.31 0.426 Inf   5.433  <.0001
# Results are given on the log odds ratio (not the response) scale. 
# P value adjustment: holm method for 3 tests 

#------------------------------------------------------------------------------#
# + + model 5 
#------------------------------------------------------------------------------#

# fit model ...

tic()
model5 <- glmer(acceptance ~ dependency * environment + 
                  (1 + dependency + environment | participant) + 
                  (1 | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl=list(maxfun = 1e6))) %>%
  write_rds('models/ajt_doenen_acc_md5.rds')
summary(model5)
toc()
beep()

model5 <- read_rds('models/ajt_doenen_acc_md5.rds')

# 16.03 sec elapsed
# no warnings
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)               -0.2812     0.2534  -1.110  0.26714    
# dependency2               -5.3886     0.3840 -14.033  < 2e-16 ***
# environment2              -0.8124     0.3049  -2.664  0.00771 ** 
# environment3              -2.1991     0.3981  -5.524 3.31e-08 ***
# dependency2:environment2   0.8285     0.6215   1.333  0.18249    
# dependency2:environment3   5.1405     0.5717   8.991  < 2e-16 ***

# compare models ...

anova(model5, model2)

# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model5   17 1669.5 1769.6 -817.76   1635.5                     
# model2   37 1686.7 1904.6 -806.37   1612.7 22.788 20     0.2993

anova(model5, model4)

# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)  
# model5   17 1669.5 1769.6 -817.76   1635.5                       
# model4   28 1673.2 1838.1 -808.61   1617.2 18.298 11    0.07492 .

#------------------------------------------------------------------------------#
# + + model 6
#------------------------------------------------------------------------------#

# fit model ...

tic()
model6 <- glmer(acceptance ~ dependency * environment + 
                  (1 | participant) + 
                  (1 | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl=list(maxfun = 1e6))) %>%
  write_rds('models/ajt_doenen_acc_md6.rds')
summary(model6)
toc()
beep()

# 2.83 sec elapsed
# Model failed to converge with max|grad| = 0.0507521 (tol = 0.002, component 1)
# Model is nearly unidentifiable: very large eigenvalue
# - Rescale variables?

#------------------------------------------------------------------------------#
# + dokoen ----
#------------------------------------------------------------------------------#

# dokoen = ORC study (do) + KLE group (ko) + English AJT (en)

# filter for analysis ...

md <- temp %>% 
  filter(study == '210510_do', 
         group == 'korean',
         task == 'english_ajt')

check <- md %>% 
  group_by(study, group, participant) %>%
  summarise() %>%
  ungroup()

# apply deviation coding ...

contrasts(md$dependency) <- contr.treatment(2) - matrix(rep(1/2, 2), ncol=1)
contrasts(md$environment) <- contr.treatment(3) - matrix(rep(1/3, 6), ncol=2)

# view contrasts ...

contrasts(md$dependency)
contrasts(md$environment)

#------------------------------------------------------------------------------#
# + + model 1 (maximal model)
#------------------------------------------------------------------------------#

# fit model ...

tic()
model1 <- glmer(acceptance ~ dependency * environment + 
                  (1 + dependency * environment | participant) + 
                  (1 + dependency * environment | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl=list(maxfun = 1e6))) %>%
  write_rds('models/ajt_dokoen_acc_md1.rds')
summary(model1)
toc()
beep()

# 782 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                0.4604     0.2188   2.104   0.0354 *  
# dependency2               -3.2167     0.7122  -4.516 6.29e-06 ***
# environment2              -0.5779     0.4026  -1.435   0.1512    
# environment3               0.1551     0.4141   0.375   0.7080    
# dependency2:environment2   1.7584     0.7861   2.237   0.0253 *  
# dependency2:environment3   3.9142     0.8510   4.600 4.23e-06 ***

#------------------------------------------------------------------------------#
# + + model 2 
#------------------------------------------------------------------------------#

# fit model ...

tic()
model2 <- glmer(acceptance ~ dependency * environment + 
                  (1 + dependency * environment | participant) + 
                  (1 + dependency + environment | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl=list(maxfun = 1e6))) %>%
  write_rds('models/ajt_dokoen_acc_md2.rds')
summary(model2)
toc()
beep()

# 307.75 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                0.4421     0.2120   2.085   0.0371 *  
# dependency2               -3.1671     0.6985  -4.534 5.79e-06 ***
# environment2              -0.5421     0.3882  -1.396   0.1626    
# environment3               0.1800     0.3940   0.457   0.6478    
# dependency2:environment2   1.6780     0.7375   2.275   0.0229 *  
# dependency2:environment3   3.8450     0.8066   4.767 1.87e-06 ***

# compare models ...

anova(model1, model2)

# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model2   37 1770.9 1978.2 -848.43   1696.9                     
# model1   48 1788.7 2057.6 -846.35   1692.7 4.1682 11     0.9647

#------------------------------------------------------------------------------#
# + + model 3 (final) ----
#------------------------------------------------------------------------------#

# fit model ...

tic()
model3 <- glmer(acceptance ~ dependency * environment + 
                  (1 + dependency * environment | participant) + 
                  (1 | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl=list(maxfun = 1e6))) %>%
  write_rds('models/ajt_dokoen_acc_md3.rds')
summary(model3)
toc()
beep()

# 46.16 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                0.4382     0.2109   2.078   0.0377 *  
# dependency2               -3.1518     0.6949  -4.536 5.74e-06 ***
# environment2              -0.5605     0.3820  -1.467   0.1423    
# environment3               0.1751     0.3924   0.446   0.6554    
# dependency2:environment2   1.7251     0.7301   2.363   0.0181 *  
# dependency2:environment3   3.8479     0.8081   4.762 1.92e-06 ***

# compare models ...

anova(model1, model3)

# npar    AIC    BIC  logLik deviance Chisq Df Pr(>Chisq)
# model3   28 1753.5 1910.4 -848.75   1697.5                    
# model1   48 1788.7 2057.6 -846.35   1692.7 4.817 20     0.9998

anova(model2, model3)

# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model3   28 1753.5 1910.4 -848.75   1697.5                     
# model2   37 1770.9 1978.2 -848.43   1696.9 0.6488  9     0.9999

# check assumptions ...

check_model(model3)
ggsave('plots/check_model.png', width=10, height=10, dpi=600)

# pairwise comparisons ...

model3 %>%
  emmeans(~ dependency * environment) %>%
  contrast('pairwise', by = 'environment') %>%
  summary(by = NULL, adjust = 'holm')

# contrast      environment estimate    SE  df z.ratio p.value
# gap - pronoun short           5.01 0.911 Inf   5.500  <.0001 ***
# gap - pronoun long            3.28 0.821 Inf   4.001  0.0001 ***
# gap - pronoun island          1.16 0.673 Inf   1.725  0.0844 .
# Results are given on the log odds ratio (not the response) scale. 
# P value adjustment: holm method for 3 tests 

#------------------------------------------------------------------------------#
# + + model 4 
#------------------------------------------------------------------------------#

# fit model ...

tic()
model4 <- glmer(acceptance ~ dependency * environment + 
                  (1 + dependency + environment | participant) + 
                  (1 + dependency + environment | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl=list(maxfun = 1e6))) %>%
  write_rds('models/ajt_dokoen_acc_md4.rds')
summary(model4)
toc()
beep()

# 113.87 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                0.4116     0.1786   2.305   0.0212 *  
# dependency2               -2.7511     0.6248  -4.403 1.07e-05 ***
# environment2              -0.3941     0.2782  -1.417   0.1565    
# environment3               0.1761     0.3031   0.581   0.5613    
# dependency2:environment2   1.5287     0.3798   4.025 5.69e-05 ***
# dependency2:environment3   3.4847     0.4039   8.627  < 2e-16 ***

# compare models ...

anova(model4, model3)

# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)    
# model4   26 1791.8 1937.5 -869.91   1739.8                         
# model3   28 1753.5 1910.4 -848.75   1697.5 42.317  2   6.47e-10 ***
  
#------------------------------------------------------------------------------#
# + + model 5
#------------------------------------------------------------------------------#
  
# fit model ...
  
tic()
model5 <- glmer(acceptance ~ dependency * environment + 
                  (1 + dependency + environment | participant) + 
                  (1 | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl=list(maxfun = 1e6))) %>%
  write_rds('models/ajt_dokoen_acc_md5.rds')
summary(model5)
toc()
beep()

# 13.24 sec elapsed
# no warnings
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                0.4127     0.1797   2.297   0.0216 *  
# dependency2               -2.7405     0.6211  -4.412 1.02e-05 ***
# environment2              -0.4097     0.2690  -1.523   0.1277    
# environment3               0.1685     0.3000   0.562   0.5743    
# dependency2:environment2   1.4945     0.3732   4.004 6.22e-05 ***
# dependency2:environment3   3.4629     0.3969   8.724  < 2e-16 ***

# compare models ...

anova(model1, model5)

# npar    AIC    BIC  logLik deviance Chisq Df Pr(>Chisq)  
# model5   17 1774.0 1869.3 -870.02   1740.0                      
# model1   48 1788.7 2057.6 -846.35   1692.7 47.34 31    0.03041 *

anova(model3, model5)

# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)    
# model5   17 1774.0 1869.3 -870.02   1740.0                         
# model3   28 1753.5 1910.4 -848.75   1697.5 42.523 11  1.315e-05 ***
  
#------------------------------------------------------------------------------#
# + + model 6 
#------------------------------------------------------------------------------#
  
# fit model ...
  
tic()
model6 <- glmer(acceptance ~ dependency * environment + 
                  (1 | participant) + 
                  (1 | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl=list(maxfun = 1e6))) %>%
  write_rds('models/ajt_dokoen_acc_md6.rds')
summary(model6)
toc()
beep()

# 1.39 sec elapsed
# no warnings
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                0.2784     0.1108   2.513  0.01199 *  
# dependency2               -1.5165     0.1046 -14.494  < 2e-16 ***
# environment2              -0.1055     0.1280  -0.824  0.40966    
# environment3               0.1722     0.1258   1.369  0.17111    
# dependency2:environment2   0.7723     0.2579   2.995  0.00274 ** 
# dependency2:environment3   1.7963     0.2528   7.106  1.2e-12 ***

anova(model1, model6)

# npar    AIC    BIC   logLik deviance  Chisq Df Pr(>Chisq)    
# model6    8 2411.2 2456.1 -1197.62   2395.2                         
# model1   48 1788.7 2057.6  -846.35   1692.7 702.55 40  < 2.2e-16 ***

anova(model3, model6)

# npar    AIC    BIC   logLik deviance  Chisq Df Pr(>Chisq)    
# model6    8 2411.2 2456.1 -1197.62   2395.2                         
# model3   28 1753.5 1910.4  -848.75   1697.5 697.73 20  < 2.2e-16 ***

#------------------------------------------------------------------------------#
# + dozhen ----
#------------------------------------------------------------------------------#

# dozhen = ORC study (do) + MLE group (zh) + English AJT (en)

# filter for analysis ...

md <- temp %>% 
  filter(study == '210510_do', 
         group == 'mandarin',
         task == 'english_ajt')

check <- md %>% 
  group_by(study, group, participant) %>%
  summarise() %>%
  ungroup()

# apply deviation coding ...

contrasts(md$dependency) <- contr.treatment(2) - matrix(rep(1/2, 2), ncol=1)
contrasts(md$environment) <- contr.treatment(3) - matrix(rep(1/3, 6), ncol=2)

# view contrasts ...

contrasts(md$dependency)
contrasts(md$environment)

#------------------------------------------------------------------------------#
# + + model 1 (maximal model) 
#------------------------------------------------------------------------------#

# fit model ...

tic()
model1 <- glmer(acceptance ~ dependency * environment + 
                  (1 + dependency * environment | participant) + 
                  (1 + dependency * environment | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl=list(maxfun = 1e6))) %>%
  write_rds('models/ajt_dozhen_acc_md1.rds')
summary(model1)
toc()
beep()

model1 <- read_rds('models/ajt_dozhen_acc_md1.rds')

# 2625.09 sec elapsed (43 min)
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                1.2125     0.2305   5.260 1.44e-07 ***
# dependency2               -2.2188     0.4530  -4.898 9.69e-07 ***
# environment2              -0.5965     0.3399  -1.755   0.0792 .  
# environment3              -0.6193     0.3540  -1.749   0.0802 .  
# dependency2:environment2   1.1327     0.6118   1.851   0.0641 .  
# dependency2:environment3   1.5160     0.6886   2.202   0.0277 *

#------------------------------------------------------------------------------#
# + + model 2 
#------------------------------------------------------------------------------#

# fit model ...

tic()
model2 <- glmer(acceptance ~ dependency * environment + 
                  (1 + dependency * environment | participant) + 
                  (1 + dependency + environment | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl=list(maxfun = 1e6))) %>%
  write_rds('models/ajt_dozhen_acc_md2.rds')
summary(model2)
toc()
beep()

model2 <- read_rds('models/ajt_dozhen_acc_md2.rds')

# 249.57 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                1.1647     0.2229   5.225 1.74e-07 ***
# dependency2               -2.1251     0.4407  -4.822 1.42e-06 ***
# environment2              -0.5357     0.3251  -1.648   0.0993 .  
# environment3              -0.6232     0.3183  -1.958   0.0502 .  
# dependency2:environment2   1.0018     0.5691   1.761   0.0783 .  
# dependency2:environment3   1.5091     0.6025   2.505   0.0123 * 

# compare models ...

anova(model2, model1)

# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model2   37 2051.3 2261.9 -988.68   1977.3                     
# model1   48 2064.7 2337.8 -984.34   1968.7 8.6751 11     0.6519

#------------------------------------------------------------------------------#
# + + model 3 
#------------------------------------------------------------------------------#

# fit model ...

tic()
model3 <- glmer(acceptance ~ dependency * environment + 
                  (1 + dependency * environment | participant) + 
                  (1 | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl=list(maxfun = 1e6))) %>%
  write_rds('models/ajt_dozhen_acc_md3.rds')
summary(model3)
toc()
beep()

model3 <- read_rds('models/ajt_dozhen_acc_md3.rds')

# Model failed to converge with max|grad| = 0.149796 (tol = 0.002, component 1)
# Model is nearly unidentifiable: very large eigenvalue
# - Rescale variables?
#   Model is nearly unidentifiable: large eigenvalue ratio
# - Rescale variables?

#------------------------------------------------------------------------------#
# + + model 4 
#------------------------------------------------------------------------------#

# fit model ...

tic()
model4 <- glmer(acceptance ~ dependency * environment + 
                  (1 + dependency + environment | participant) + 
                  (1 + dependency + environment | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl=list(maxfun = 1e6))) %>%
  write_rds('models/ajt_dozhen_acc_md4.rds')
summary(model4)
toc()
beep()

model4 <- read_rds('models/ajt_dozhen_acc_md4.rds')

# 95.75 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                1.0859     0.2144   5.064 4.11e-07 ***
# dependency2               -1.9536     0.4199  -4.652 3.29e-06 ***
# environment2              -0.3666     0.2649  -1.384   0.1664    
# environment3              -0.5057     0.2518  -2.008   0.0446 *  
# dependency2:environment2   0.6642     0.3653   1.818   0.0690 .  
# dependency2:environment3   1.3859     0.3474   3.989 6.63e-05 ***

# compare models ...

anova(model4, model2)

# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model4   26 2043.6 2191.5 -995.78   1991.6                     
# model2   37 2051.3 2261.9 -988.68   1977.3 14.215 11     0.2213

anova(model4, model1)

# npar    AIC    BIC  logLik deviance Chisq Df Pr(>Chisq)
# model4   26 2043.6 2191.5 -995.78   1991.6                    
# model1   48 2064.7 2337.8 -984.34   1968.7 22.89 22      0.408

#------------------------------------------------------------------------------#
# + + model 5 (final) ----
#------------------------------------------------------------------------------#

# fit model ...

tic()
model5 <- glmer(acceptance ~ dependency * environment + 
                  (1 + dependency + environment | participant) + 
                  (1 | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl=list(maxfun = 1e6))) %>%
  write_rds('models/ajt_dozhen_acc_md5.rds')
summary(model5)
toc()
beep()

model5 <- read_rds('models/ajt_dozhen_acc_md5.rds')

# 10.61 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                1.0746     0.2113   5.086 3.66e-07 ***
# dependency2               -1.9265     0.4133  -4.661 3.14e-06 ***
# environment2              -0.3514     0.2442  -1.439   0.1501    
# environment3              -0.4757     0.2214  -2.148   0.0317 *  
# dependency2:environment2   0.6225     0.3575   1.741   0.0816 .  
# dependency2:environment3   1.3521     0.3417   3.957 7.60e-05 ***

# compare models ...

anova(model5, model4)

# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model5   17 2028.1 2124.8 -997.04   1994.1                     
# model4   26 2043.6 2191.5 -995.78   1991.6 2.5195  9     0.9804

anova(model3, model5)

# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model5   17 2028.1 2124.8 -997.04   1994.1                     
# model3   28 2034.3 2193.6 -989.15   1978.3 15.787 11     0.1492

# check assumptions ...

check_model(model5)
ggsave('plots/check_model.png', width=10, height=10, dpi=600)

# pairwise comparisons ...

model5 %>%
  emmeans(~ dependency * environment) %>%
  contrast('pairwise', by = 'environment') %>%
  summary(by = NULL, adjust = 'holm')

# contrast      environment estimate    SE  df z.ratio p.value
# gap - pronoun short           2.58 0.471 Inf   5.492  <.0001 ***
# gap - pronoun long            1.96 0.460 Inf   4.263  <.0001 ***
# gap - pronoun island          1.23 0.444 Inf   2.776  0.0055 **
# Results are given on the log odds ratio (not the response) scale. 
# P value adjustment: holm method for 3 tests 

#------------------------------------------------------------------------------#
# + + model 6 
#------------------------------------------------------------------------------#

# fit model ...

tic()
model6 <- glmer(acceptance ~ dependency * environment + 
                  (1 | participant) + 
                  (1 | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl=list(maxfun = 1e6))) %>%
  write_rds('models/ajt_dozhen_acc_md6.rds')
summary(model6)
toc()
beep()

model6 <- read_rds('models/ajt_dozhen_acc_md6.rds')

# 1.76 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                0.9219     0.1634   5.643 1.68e-08 ***
# dependency2               -1.5927     0.1114 -14.298  < 2e-16 ***
# environment2              -0.2882     0.1345  -2.142  0.03218 *  
# environment3              -0.4140     0.1326  -3.122  0.00179 ** 
# dependency2:environment2   0.5891     0.2691   2.189  0.02857 *  
# dependency2:environment3   1.0564     0.2654   3.980 6.89e-05 ***

# model comparison ...

anova(model6, model5)

# npar    AIC    BIC   logLik deviance  Chisq Df Pr(>Chisq)    
# model6    8 2334.2 2379.7 -1159.08   2318.2                         
# model5   17 2028.1 2124.8  -997.04   1994.1 324.08  9  < 2.2e-16 ***

anova(model6, model4)

# npar    AIC    BIC   logLik deviance Chisq Df Pr(>Chisq)    
# model6    8 2334.2 2379.7 -1159.08   2318.2                        
# model4   26 2043.6 2191.5  -995.78   1991.6 326.6 18  < 2.2e-16 ***

#------------------------------------------------------------------------------#
# + + model 5b
#------------------------------------------------------------------------------#

# fit model ...

tic()
model5b <- glmer(acceptance ~ dependency * environment + 
                  (1 + dependency + environment || participant) + 
                  (1 | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl=list(maxfun = 1e6))) %>%
  write_rds('models/ajt_dozhen_acc_md5b.rds')
summary(model5b)
toc()
beep()

model5b <- read_rds('models/ajt_dozhen_acc_md5b.rds')

# 13.83 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                1.0747     0.2100   5.118 3.09e-07 ***
# dependency2               -1.9212     0.4118  -4.666 3.07e-06 ***
# environment2              -0.3179     0.2399  -1.325   0.1852    
# environment3              -0.4611     0.2198  -2.098   0.0359 *  
# dependency2:environment2   0.5748     0.3532   1.627   0.1037    
# dependency2:environment3   1.3334     0.3364   3.964 7.38e-05 ***

anova(model5b, model5)

# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model5b   17 2029.0 2125.8 -997.52   1995.0                     
# model5    17 2028.1 2124.8 -997.04   1994.1 0.9447  0 

#------------------------------------------------------------------------------#
# + dokoko ----
#------------------------------------------------------------------------------#

# dokoko = ORC study (do) + KLE group (ko) + Korean AJT (ko)

# filter for analysis ...

md <- temp %>% 
  filter(study == '210510_do', 
         group == 'korean',
         task == 'korean_ajt')

check <- md %>% 
  group_by(study, group, participant) %>%
  summarise() %>%
  ungroup()

# apply deviation coding ...

contrasts(md$dependency) <- contr.treatment(2) - matrix(rep(1/2, 2), ncol=1)
contrasts(md$environment) <- contr.treatment(3) - matrix(rep(1/3, 6), ncol=2)

# view contrasts ...

contrasts(md$dependency)
contrasts(md$environment)

#------------------------------------------------------------------------------#
# + + model 1 (maximal model) 
#------------------------------------------------------------------------------#

# fit model ...

tic()
model1 <- glmer(acceptance ~ dependency * environment + 
                  (1 + dependency * environment | participant) + 
                  (1 + dependency * environment | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl=list(maxfun = 1e6))) %>%
  write_rds('models/ajt_dokoko_acc_md1.rds')
summary(model1)
toc()
beep()

model1 <- read_rds('models/ajt_dokoko_acc_md1.rds')

missing

#------------------------------------------------------------------------------#
# + + model 3 
#------------------------------------------------------------------------------#

# fit model ...

tic()
model3 <- glmer(acceptance ~ dependency * environment + 
                  (1 + dependency + environment | participant) + 
                  (1 + dependency + environment | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl=list(maxfun = 1e6))) %>%
  write_rds('models/ajt_dokoko_acc_md3.rds')
summary(model3)
toc()
beep()

model3 <- read_rds('models/ajt_dokoko_acc_md3.rds')

# 244.86 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                0.4714     0.2988   1.578 0.114574    
# dependency2               -3.3795     0.3788  -8.922  < 2e-16 ***
# environment2              -1.5673     0.4316  -3.631 0.000282 ***
# environment3              -1.0878     0.4164  -2.613 0.008982 ** 
# dependency2:environment2   4.9090     0.7378   6.653 2.86e-11 ***
# dependency2:environment3   6.9606     0.7173   9.704  < 2e-16 ***

#------------------------------------------------------------------------------#
# + + model 4
#------------------------------------------------------------------------------#

tic()
model4 <- glmer(acceptance ~ dependency * environment + 
                  (1 + dependency * environment | participant) + 
                  (1 | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl=list(maxfun = 1e6))) %>%
  write_rds('models/ajt_dokoko_acc_md4.rds')
summary(model4)
toc()
beep()

# 243.84 sec elapsed
# no warnings
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                0.8563     0.4827   1.774 0.076095 .  
# dependency2               -4.5496     0.8412  -5.408 6.36e-08 ***
# environment2              -2.4650     1.2164  -2.026 0.042724 *  
# environment3              -2.1469     1.2013  -1.787 0.073909 .  
# dependency2:environment2   8.4309     2.4190   3.485 0.000492 ***
# dependency2:environment3  10.5262     2.3954   4.394 1.11e-05 ***

#------------------------------------------------------------------------------#
# + + model 5
#------------------------------------------------------------------------------#

tic()
model5 <- glmer(acceptance ~ dependency * environment + 
                  (1 + dependency * environment || participant) + 
                  (1 | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl=list(maxfun = 1e6))) %>%
  write_rds('models/ajt_dokoko_acc_md5.rds')
summary(model5)
toc()
beep()

# Model failed to converge: degenerate  Hessian with 5 negative eigenvalues

#------------------------------------------------------------------------------#
# + + model 6 ----
#------------------------------------------------------------------------------#

tic()
model6 <- glmer(acceptance ~ dependency * environment + 
                  (1 + dependency + environment | participant) + 
                  (1 | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl=list(maxfun = 1e6))) %>%
  write_rds('models/ajt_dokoko_acc_md6.rds')
summary(model6)
toc()
beep()

# 14.12 sec elapsed
# no warnings
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                0.4665     0.2932   1.591 0.111634    
# dependency2               -3.2932     0.3425  -9.614  < 2e-16 ***
# environment2              -1.4717     0.3928  -3.746 0.000179 ***
# environment3              -1.0543     0.3875  -2.721 0.006517 ** 
# dependency2:environment2   4.8643     0.6997   6.952 3.59e-12 ***
# dependency2:environment3   6.7890     0.6828   9.944  < 2e-16 ***

anova(model6, model3)

       npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
model6   17 1624.8 1720.1 -795.42   1590.8
model3   26 1635.9 1781.7 -791.95   1583.9 6.9319  9     0.6442

# check assumptions ...

check_model(model6)
ggsave('plots/check_model.png', width=10, height=10, dpi=600)

# pairwise comparisons ...

model6 %>%
  emmeans(~ dependency * environment) %>%
  contrast('pairwise', by = 'environment') %>%
  summary(by = NULL, adjust = 'holm')

# contrast      environment estimate    SE  df z.ratio p.value
# gap - pronoun short          7.178 0.685 Inf  10.477  <.0001 ***
# gap - pronoun long           2.313 0.377 Inf   6.141  <.0001 ***
# gap - pronoun island         0.389 0.325 Inf   1.196  0.2316
# Results are given on the log odds ratio (not the response) scale. 
# P value adjustment: holm method for 3 tests

#------------------------------------------------------------------------------#
# + dozhzh ----
#------------------------------------------------------------------------------#

# dozhzh = ORC study (do) + MLE group (zh) + Mandarin AJT (zh)

# filter for analysis ...

md <- temp %>% 
  filter(study == '210510_do', 
         group == 'mandarin',
         task == 'mandarin_ajt')

check <- md %>% 
  group_by(study, group, participant) %>%
  summarise() %>%
  ungroup()

# apply deviation coding ...

contrasts(md$dependency) <- contr.treatment(2) - matrix(rep(1/2, 2), ncol=1)
contrasts(md$environment) <- contr.treatment(3) - matrix(rep(1/3, 6), ncol=2)

# view contrasts ...

contrasts(md$dependency)
contrasts(md$environment)

#------------------------------------------------------------------------------#
# + + model 1 (maximal model) 
#------------------------------------------------------------------------------#

# fit model ...

tic()
model1 <- glmer(acceptance ~ dependency * environment + 
                  (1 + dependency * environment | participant) + 
                  (1 + dependency * environment | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl=list(maxfun = 1e6))) %>%
  write_rds('models/ajt_dozhzh_acc_md1.rds')
summary(model1)
toc()
beep()

model1 <- read_rds('models/ajt_dozhzh_acc_md1.rds')

# 1290.69 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)               -0.4334     0.2915  -1.487 0.137078    
# dependency2               -0.9881     0.2287  -4.321 1.55e-05 ***
# environment2               0.3739     0.3124   1.197 0.231264    
# environment3              -0.8169     0.2987  -2.735 0.006243 ** 
# dependency2:environment2   1.5354     0.4195   3.660 0.000252 ***
# dependency2:environment3   2.6887     0.4684   5.740 9.44e-09 ***

#------------------------------------------------------------------------------#
# + + model 2
#------------------------------------------------------------------------------#

# fit model ...

tic()
model2 <- glmer(acceptance ~ dependency * environment + 
                  (dependency * environment | participant) + 
                  (dependency + environment | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl=list(maxfun = 1e6))) %>%
  write_rds('models/ajt_dozhzh_acc_md2.rds')
summary(model2)
toc()
beep()

model2 <- read_rds('models/ajt_dozhzh_acc_md2.rds')

# 688.68 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)               -0.4353     0.2884  -1.509  0.13123    
# dependency2               -0.9786     0.2226  -4.395 1.11e-05 ***
# environment2               0.4045     0.3061   1.321  0.18636    
# environment3              -0.7740     0.2862  -2.705  0.00684 ** 
# dependency2:environment2   1.5288     0.3906   3.914 9.07e-05 ***
# dependency2:environment3   2.6534     0.4568   5.809 6.30e-09 ***

# compare models ...

anova(model1, model2)

# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model2   37 2217.6 2429.1 -1071.8   2143.6                     
# model1   48 2235.2 2509.6 -1069.6   2139.2 4.3718 11     0.9578

#------------------------------------------------------------------------------#
# + + model 3 (final) ----
#------------------------------------------------------------------------------#

# fit model ...

tic()
model3 <- glmer(acceptance ~ dependency * environment + 
                  (dependency + environment | participant) + 
                  (dependency + environment | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl=list(maxfun = 1e6))) %>%
  write_rds('models/ajt_dozhzh_acc_md3.rds')
summary(model3)
toc()
beep()

model3 <- read_rds('models/ajt_dozhzh_acc_md3.rds')

# 257.71 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)               -0.4120     0.2806  -1.468 0.142034    
# dependency2               -0.9008     0.2056  -4.381 1.18e-05 ***
# environment2               0.3019     0.2790   1.082 0.279210    
# environment3              -0.9267     0.2739  -3.383 0.000716 ***
# dependency2:environment2   1.2474     0.3246   3.842 0.000122 ***
# dependency2:environment3   2.3292     0.3367   6.917 4.60e-12 ***

# compare models ...

anova(model2, model3)

# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model3   26 2212.5 2361.2 -1080.3   2160.5
# model2   37 2217.6 2429.1 -1071.8   2143.6 16.977 11     0.1086

# pairwise comparisons ...

model3 %>%
  emmeans(~ dependency * environment) %>%
  contrast('pairwise', by = 'environment') %>%
  summary(by = NULL, adjust = 'holm')

# contrast      environment estimate    SE  df z.ratio p.value
# gap - pronoun short          2.093 0.287 Inf   7.283  <.0001 ***
# gap - pronoun long           0.846 0.269 Inf   3.144  0.0033 **
# gap - pronoun island        -0.236 0.281 Inf  -0.840  0.4009

#------------------------------------------------------------------------------#
# + + model 4
#------------------------------------------------------------------------------#

# fit model ...

tic()
model4 <- glmer(acceptance ~ dependency * environment + 
                  (dependency * environment | participant) + 
                  (1 | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl=list(maxfun = 1e6))) %>%
  write_rds('models/ajt_dozhzh_acc_md4.rds')
summary(model4)
toc()
beep()

model4 <- read_rds('models/ajt_dozhzh_acc_md4.rds')

# 112.33 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)               -0.4463     0.2821  -1.582 0.113596    
# dependency2               -0.9273     0.2128  -4.357 1.32e-05 ***
# environment2               0.3884     0.2529   1.536 0.124589    
# environment3              -0.7676     0.2656  -2.890 0.003852 ** 
# dependency2:environment2   1.2775     0.3650   3.500 0.000466 ***
# dependency2:environment3   2.4263     0.4300   5.643 1.68e-08 ***

# compare models ...

anova(model1, model4)

# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model4   28 2218.5 2378.5 -1081.2   2162.5
# model1   48 2235.2 2509.6 -1069.6   2139.2 23.295 20     0.2745

anova(model3, model4)

# npar    AIC    BIC  logLik deviance Chisq Df Pr(>Chisq)
# model3   26 2212.5 2361.2 -1080.3   2160.5
# model4   28 2218.5 2378.5 -1081.2   2162.5     0  2          1

#------------------------------------------------------------------------------#
# + + model 5
#------------------------------------------------------------------------------#

# fit model ...

tic()
model5 <- glmer(acceptance ~ dependency * environment + 
                  (dependency + environment | participant) + 
                  (1 | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl=list(maxfun = 1e6))) %>%
  write_rds('models/ajt_dozhzh_acc_md5.rds')
summary(model5)
toc()
beep()

model5 <- read_rds('models/ajt_dozhzh_acc_md5.rds')

# 24.87 sec elapsed
# no warnings
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)               -0.4260     0.2763  -1.542 0.123156    
# dependency2               -0.8603     0.1979  -4.346 1.39e-05 ***
# environment2               0.2817     0.2302   1.224 0.221136    
# environment3              -0.9214     0.2626  -3.509 0.000450 ***
# dependency2:environment2   1.0653     0.3119   3.415 0.000638 ***
# dependency2:environment3   2.1691     0.3225   6.726 1.74e-11 ***

# compare models ...

anova(model1, model5)

# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model5   17 2212.6 2309.7 -1089.3   2178.6
# model1   48 2235.2 2509.6 -1069.6   2139.2 39.356 31     0.1442

anova(model4, model5)

# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model5   17 2212.6 2309.7 -1089.3   2178.6
# model4   28 2218.5 2378.5 -1081.2   2162.5 16.061 11     0.1389

anova(model3, model5)

# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model5   17 2212.6 2309.7 -1089.3   2178.6
# model3   26 2212.5 2361.2 -1080.3   2160.5 18.008  9    0.03509 *

#------------------------------------------------------------------------------#
# + + model 6
#------------------------------------------------------------------------------#

# fit model ...

tic()
model6 <- glmer(acceptance ~ dependency * environment + 
                  (1 | participant) + 
                  (1 | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl=list(maxfun = 1e6))) %>%
  write_rds('models/ajt_dozhzh_acc_md6.rds')
summary(model6)
toc()
beep()

model6 <- read_rds('models/ajt_dozhzh_acc_md6.rds')

# 1.72 sec elapsed
# no warnings
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)               -0.3257     0.2365  -1.377   0.1684    
# dependency2               -0.7868     0.1101  -7.146 8.92e-13 ***
# environment2               0.3056     0.1340   2.280   0.0226 *  
# environment3              -0.5689     0.1363  -4.175 2.98e-05 ***
# dependency2:environment2   1.1340     0.2662   4.260 2.05e-05 ***
# dependency2:environment3   1.9809     0.2732   7.252 4.12e-13 ***

# compare models ...

anova(model1, model6)

# npar    AIC    BIC  logLik deviance Chisq Df Pr(>Chisq)
# model6    8 2293.8 2339.5 -1138.9   2277.8
# model1   48 2235.2 2509.6 -1069.6   2139.2 138.6 40  8.481e-13 ***

anova(model5, model6)

# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model6    8 2293.8 2339.5 -1138.9   2277.8
# model5   17 2212.6 2309.7 -1089.3   2178.6 99.243  9  < 2.2e-16 ***

anova(model3, model6)

# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)    
# model6    8 2293.8 2339.5 -1138.9   2277.8                         
# model3   26 2212.5 2361.2 -1080.3   2160.5 117.25 18  < 2.2e-16 ***

#------------------------------------------------------------------------------#
# + suenen ----
#------------------------------------------------------------------------------#

# suenen = SRC study (su) + ENS group (en) + English AJT (en)

# filter for analysis ...

md <- temp %>% 
  filter(study == '210510_su', 
         group == 'english',
         task == 'english_ajt')

check <- md %>% 
  group_by(study, group, participant) %>%
  summarise() %>%
  ungroup()

# apply deviation coding ...

contrasts(md$dependency) <- contr.treatment(2) - matrix(rep(1/2, 2), ncol=1)
contrasts(md$environment) <- contr.treatment(3) - matrix(rep(1/3, 6), ncol=2)

# view contrasts ...

contrasts(md$dependency)
contrasts(md$environment)

#------------------------------------------------------------------------------#
# + + model 1 (final) ----
#------------------------------------------------------------------------------#

# fit model ...

tic()
model1 <- glmer(acceptance ~ dependency * environment + 
                  (1 + dependency * environment | participant) + 
                  (1 + dependency * environment | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl=list(maxfun = 1e6))) %>%
  write_rds('models/ajt_suenen_acc_md1.rds')
summary(model1)
toc()
beep()

model1 <- read_rds('models/ajt_suenen_acc_md1.rds')

# 29465.95 sec elapsed (8 hours)
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                          17.616890   0.002214    7957   <2e-16 ***
# dependencypronoun                   -24.162444   0.002335  -10350   <2e-16 ***
# environmentlong                     -12.782311   0.002392   -5343   <2e-16 ***
# environmentisland                   -34.776498   0.002335  -14896   <2e-16 ***
# dependencypronoun:environmentlong    15.395926   0.002293    6715   <2e-16 ***
# dependencypronoun:environmentisland  38.085710   0.002335   16314   <2e-16 ***

# pairwise comparisons ...

model1 %>%
  emmeans(~ dependency * environment) %>%
  contrast('pairwise', by = 'environment') %>%
  summary(by = NULL, adjust = 'holm')

# contrast      environment estimate        SE  df   z.ratio p.value
# gap - pronoun short        24.1624 0.0023346 Inf 10349.850  <.0001
# gap - pronoun long          8.7665 0.0032720 Inf  2679.232  <.0001
# gap - pronoun island      -13.9233 0.0033015 Inf -4217.253  <.0001

#------------------------------------------------------------------------------#
# + + model 2
#------------------------------------------------------------------------------#

# fit model ...

tic()
model2 <- glmer(acceptance ~ dependency * environment + 
                  (dependency * environment | participant) + 
                  (dependency + environment | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl=list(maxfun = 1e6))) %>%
  write_rds('models/ajt_suenen_acc_md2.rds')
summary(model2)
toc()
beep()

5199.58 sec elapsed (86 min)
optimizer (bobyqa) convergence code: 0 (OK)
boundary (singular) fit: see help('isSingular')
Fixed effects:
  Estimate Std. Error
(Intercept)                            8.320      1.426
dependencypronoun                    -15.607      2.318
environmentlong                       -3.523      1.831
environmentisland                    -14.887      2.164
dependencypronoun:environmentlong      6.179      2.629
dependencypronoun:environmentisland   18.915      2.982
z value Pr(>|z|)
(Intercept)                           5.834 5.41e-09 ***
  dependencypronoun                    -6.732 1.67e-11 ***
  environmentlong                      -1.923   0.0544 .
environmentisland                    -6.880 5.98e-12 ***
  dependencypronoun:environmentlong     2.351   0.0187 *
  dependencypronoun:environmentisland   6.343 2.26e-10 ***

# compare models ...

anova(model1, model2)

npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
model2   37 802.68 1006.3 -364.34   728.68
model1   48 804.68 1068.8 -354.34   708.68 19.993 11    0.04543 *

#------------------------------------------------------------------------------#
# + + model 3
#------------------------------------------------------------------------------#

# fit model ...

tic()
model3 <- glmer(acceptance ~ dependency * environment + 
                  (dependency + environment | participant) + 
                  (dependency + environment | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl=list(maxfun = 1e6))) %>%
  write_rds('models/ajt_suenen_acc_md3.rds')
summary(model3)
toc()
beep()

model3 <- read_rds('models/ajt_suenen_acc_md3.rds')

# 128.85 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)               -0.9966     0.4037  -2.469  0.01355 *  
# dependency2               -5.4468     0.5792  -9.405  < 2e-16 ***
# environment2              -1.1272     0.5137  -2.194  0.02820 *  
# environment3              -5.2881     0.8619  -6.135  8.5e-10 ***
# dependency2:environment2   4.1446     1.4652   2.829  0.00467 ** 
# dependency2:environment3  13.8977     1.7035   8.158  3.4e-16 ***

# compare models ...

anova(model1, model3)

# npar    AIC     BIC  logLik deviance  Chisq Df Pr(>Chisq)    
# model3   26 819.65  962.71 -383.83   767.65                         
# model1   48 804.68 1068.79 -354.34   708.68 58.972 22  3.168e-05 ***

#------------------------------------------------------------------------------#
# + + model 4
#------------------------------------------------------------------------------#

# fit model ...

tic()
model4 <- glmer(acceptance ~ dependency * environment + 
                  (dependency * environment | participant) + 
                  (1 | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl=list(maxfun = 1e6))) %>%
  write_rds('models/ajt_suenen_acc_md4.rds')
summary(model4)
toc()
beep()

# 118.24 sec elapsed
# Model failed to converge with max|grad| = 0.0424997 (tol = 0.002, component 1)

#------------------------------------------------------------------------------#
# + + model 5 (maybe?)
#------------------------------------------------------------------------------#

# fit model ...

tic()
model5 <- glmer(acceptance ~ dependency * environment + 
                  (dependency + environment | participant) + 
                  (1 | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl=list(maxfun = 1e6))) %>%
  write_rds('models/ajt_suenen_acc_md5.rds')
summary(model5)
toc()
beep()

model5 <- read_rds('models/ajt_suenen_acc_md5.rds')

# 50.84 sec elapsed
# no warnings
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)               -0.9332     0.3992  -2.338 0.019400 *  
# dependency2               -5.3905     0.5562  -9.691  < 2e-16 ***
# environment2              -0.9651     0.4726  -2.042 0.041145 *  
# environment3              -5.2713     0.8109  -6.500 8.01e-11 ***
# dependency2:environment2   5.1344     1.4094   3.643 0.000269 ***
# dependency2:environment3  14.2322     1.6855   8.444  < 2e-16 ***

# compare models ...

anova(model1, model5)

# npar    AIC     BIC  logLik deviance  Chisq Df Pr(>Chisq)    
# model5   17 805.80  899.34 -385.90   771.80                         
# model1   48 804.68 1068.79 -354.34   708.68 63.119 31  0.0005698 ***

anova(model3, model5)

# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model5   17 805.80 899.34 -385.90   771.80                     
# model3   26 819.65 962.71 -383.83   767.65 4.1477  9     0.9014
  
#------------------------------------------------------------------------------#
# + + model 6
#------------------------------------------------------------------------------#

# fit model ...

tic()
model6 <- glmer(acceptance ~ dependency * environment + 
                  (1 | participant) + 
                  (1 | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl=list(maxfun = 1e6))) %>%
  write_rds('models/ajt_suenen_acc_md6.rds')
summary(model6)
toc()
beep()

model6 <- read_rds('models/ajt_suenen_acc_md6.rds')

# 2.03 sec elapsed
# no warnings
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)               -0.6943     0.3028  -2.293  0.02187 *  
# dependency2               -5.0061     0.3465 -14.447  < 2e-16 ***
# environment2              -1.0421     0.3740  -2.786  0.00533 ** 
# environment3              -3.8746     0.4045  -9.579  < 2e-16 ***
# dependency2:environment2   3.4680     0.7511   4.617 3.89e-06 ***
# dependency2:environment3  12.1821     0.8887  13.707  < 2e-16 ***

# compare models ...

anova(model5, model6)

# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)    
# model6    8 819.54 863.55 -401.77   803.54                         
# model5   17 805.80 899.34 -385.90   771.80 31.735  9  0.0002212 ***

anova(model3, model6)

# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)   
# model6    8 819.54 863.55 -401.77   803.54                        
# model3   26 819.65 962.71 -383.83   767.65 35.883 18   0.007304 **

#------------------------------------------------------------------------------#
# + sukoen ----
#------------------------------------------------------------------------------#

# sukoen = SRC study (su) + KLE group (ko) + English AJT (en)

# filter for analysis ...

md <- temp %>% 
  filter(study == '210510_su', 
         group == 'korean',
         task == 'english_ajt')

check <- md %>% 
  group_by(study, group, participant) %>%
  summarise() %>%
  ungroup()

# apply deviation coding ...

contrasts(md$dependency) <- contr.treatment(2) - matrix(rep(1/2, 2), ncol=1)
contrasts(md$environment) <- contr.treatment(3) - matrix(rep(1/3, 6), ncol=2)

# view contrasts ...

contrasts(md$dependency)
contrasts(md$environment)

#------------------------------------------------------------------------------#
# + + model 1 (maximal model) 
#------------------------------------------------------------------------------#

# fit model ...

tic()
model1 <- glmer(acceptance ~ dependency * environment + 
                  (1 + dependency * environment | participant) + 
                  (1 + dependency * environment | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl=list(maxfun = 1e6))) %>%
  write_rds('models/ajt_sukoen_acc_md1.rds')
summary(model1)
toc()
beep()

model1 <- read_rds('models/ajt_sukoen_acc_md1.rds')

1828.64 sec elapsed (30 min)
optimizer (bobyqa) convergence code: 0 (OK)
boundary (singular) fit: see help('isSingular')
Fixed effects:
  Estimate Std. Error z value Pr(>|z|)
(Intercept)                            8.097      2.444   3.314 0.000921 ***
dependencypronoun                    -11.656      2.601  -4.481 7.44e-06 ***
environmentlong                       -6.985      2.463  -2.836 0.004570 **
environmentisland                    -10.189      2.494  -4.085 4.40e-05 ***
dependencypronoun:environmentlong      9.287      2.597   3.576 0.000348 ***
dependencypronoun:environmentisland   14.150      2.667   5.306 1.12e-07 ***

#------------------------------------------------------------------------------#
# + + model 2
#------------------------------------------------------------------------------#

# fit model ...

tic()
model2 <- glmer(acceptance ~ dependency * environment + 
                  (1 + dependency * environment | participant) + 
                  (1 + dependency + environment | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl=list(maxfun = 1e6))) %>%
  write_rds('models/ajt_sukoen_acc_md2.rds')
summary(model2)
toc()
beep()

446.64 sec elapsed (7 min)
optimizer (bobyqa) convergence code: 0 (OK)
boundary (singular) fit: see help('isSingular')
Fixed effects:
  Estimate Std. Error z value Pr(>|z|)
(Intercept)                            6.118      1.979   3.091 0.001992 **
dependencypronoun                     -9.606      2.169  -4.429 9.45e-06 ***
environmentlong                       -4.982      2.016  -2.471 0.013461 *
environmentisland                     -8.154      2.048  -3.982 6.84e-05 ***
dependencypronoun:environmentlong      7.227      2.172   3.328 0.000875 ***
dependencypronoun:environmentisland   12.040      2.255   5.338 9.39e-08 ***

#------------------------------------------------------------------------------#
# + + model 3
#------------------------------------------------------------------------------#

# fit model ...

tic()
model3 <- glmer(acceptance ~ dependency * environment + 
                  (1 + dependency + environment | participant) + 
                  (1 + dependency + environment | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl=list(maxfun = 1e6))) %>%
  write_rds('models/ajt_sukoen_acc_md3.rds')
summary(model3)
toc()
beep()

45.31 sec elapsed
optimizer (bobyqa) convergence code: 0 (OK)
boundary (singular) fit: see help('isSingular')
Fixed effects:
  Estimate Std. Error z value Pr(>|z|)
(Intercept)                           5.0599     0.5928   8.536  < 2e-16 ***
dependencypronoun                    -7.8789     0.7520 -10.477  < 2e-16 ***
environmentlong                      -4.2566     0.5560  -7.656 1.93e-14 ***
environmentisland                    -6.7924     0.6427 -10.568  < 2e-16 ***
dependencypronoun:environmentlong     6.0719     0.6548   9.272  < 2e-16 ***
dependencypronoun:environmentisland  10.1546     0.7258  13.990  < 2e-16 ***

# compare models ...

anova(model1, model3)

npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
model3   26 1752.6 1898.0 -850.31   1700.6
model1   48 1701.6 1969.9 -802.79   1605.6 95.035 22  4.698e-11 ***

#------------------------------------------------------------------------------#
# + + model 4 ----
#------------------------------------------------------------------------------#

# fit model ...

tic()
model4 <- glmer(acceptance ~ dependency * environment + 
                  (1 + dependency * environment | participant) + 
                  (1 | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl=list(maxfun = 1e6))) %>%
  write_rds('models/ajt_sukoen_acc_md4.rds')
summary(model4)
toc()
beep()

78.66 sec elapsed
no warnings
Fixed effects:
  Estimate Std. Error z value Pr(>|z|)
(Intercept)                            5.931      1.904   3.115 0.001842 **
dependencypronoun                     -9.382      2.089  -4.490 7.11e-06 ***
environmentlong                       -4.826      1.944  -2.483 0.013044 *
environmentisland                     -7.949      1.971  -4.033 5.51e-05 ***
dependencypronoun:environmentlong      7.040      2.092   3.365 0.000766 ***
dependencypronoun:environmentisland   11.800      2.176   5.423 5.86e-08 ***

# compare models ...

anova(model1, model4)

npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
model4   28 1667.5 1824.1 -805.77   1611.5
model1   48 1701.6 1969.9 -802.79   1605.6 5.9504 20      0.999

anova(model2, model4)

npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
model4   28 1667.5 1824.1 -805.77   1611.5
model2   37 1684.6 1891.5 -805.30   1610.6 0.9349  9     0.9996

anova(model3, model4)

npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
model3   26 1752.6 1898.0 -850.31   1700.6
model4   28 1667.5 1824.1 -805.77   1611.5 89.084  2  < 2.2e-16 ***

# pairwise comparisons ...

model4 %>%
  emmeans(~ dependency * environment) %>%
  contrast('pairwise', by = 'environment') %>%
  summary(by = NULL, adjust = 'holm')

# generate table ...

kbl(summary(model4)$coefficients, digits = c(2, 2, 2, 3))

#------------------------------------------------------------------------------#
# + + model 5
#------------------------------------------------------------------------------#

# fit model ...

tic()
model5 <- glmer(acceptance ~ dependency * environment + 
                  (1 + dependency + environment | participant) + 
                  (1 | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl=list(maxfun = 1e6))) %>%
  write_rds('models/ajt_sukoen_acc_md5.rds')
summary(model5)
toc()
beep()

10.41 sec elapsed
no warnings
Fixed effects:
  Estimate Std. Error z value Pr(>|z|)
(Intercept)                           5.0444     0.5873   8.590  < 2e-16 ***
dependencypronoun                    -7.8500     0.7426 -10.571  < 2e-16 ***
environmentlong                      -4.2437     0.5523  -7.683 1.55e-14 ***
environmentisland                    -6.7752     0.6384 -10.613  < 2e-16 ***
dependencypronoun:environmentlong     6.0457     0.6449   9.374  < 2e-16 ***
dependencypronoun:environmentisland  10.1235     0.7168  14.124  < 2e-16 ***

# compare models ...

anova(model1, model5)

npar    AIC    BIC  logLik deviance Chisq Df Pr(>Chisq)
model5   17 1734.8 1829.8 -850.40   1700.8
model1   48 1701.6 1969.9 -802.79   1605.6 95.22 31  1.894e-08 ***

#------------------------------------------------------------------------------#
# + + model 6
#------------------------------------------------------------------------------#

# fit model ...

tic()
model6 <- glmer(acceptance ~ dependency * environment + 
                  (1 | participant) + 
                  (1 | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl=list(maxfun = 1e6))) %>%
  write_rds('models/ajt_sukoen_acc_md6.rds')
summary(model6)
toc()
beep()

2.12 sec elapsed
optimizer (bobyqa) convergence code: 0 (OK)
boundary (singular) fit: see help('isSingular')
Fixed effects:
  Estimate Std. Error z value Pr(>|z|)
(Intercept)                           3.0997     0.2730  11.355   <2e-16 ***
dependencypronoun                    -4.6805     0.2907 -16.100   <2e-16 ***
environmentlong                      -2.5546     0.2706  -9.439   <2e-16 ***
environmentisland                    -4.1934     0.2817 -14.888   <2e-16 ***
dependencypronoun:environmentlong     3.4429     0.3346  10.289   <2e-16 ***
dependencypronoun:environmentisland   6.0653     0.3493  17.362   <2e-16 ***

# compare models ...

anova(model1, model6)

npar    AIC    BIC   logLik deviance  Chisq Df Pr(>Chisq)
model6    8 2053.6 2098.3 -1018.80   2037.6
model1   48 1701.6 1969.9  -802.79   1605.6 432.02 40  < 2.2e-16 ***

#------------------------------------------------------------------------------#
# + suzhen ----
#------------------------------------------------------------------------------#

# suzhen = SRC study (su) + MLE group (zh) + English AJT (en)

# filter for analysis ...

md <- temp %>% 
  filter(study == '210510_su', 
         group == 'mandarin',
         task == 'english_ajt')

check <- md %>% 
  group_by(study, group, participant) %>%
  summarise() %>%
  ungroup()

# apply deviation coding ...

contrasts(md$dependency) <- contr.treatment(2) - matrix(rep(1/2, 2), ncol=1)
contrasts(md$environment) <- contr.treatment(3) - matrix(rep(1/3, 6), ncol=2)

# view contrasts ...

contrasts(md$dependency)
contrasts(md$environment)

#------------------------------------------------------------------------------#
# + + model 1 (maximal model) 
#------------------------------------------------------------------------------#

# fit model ...

tic()
model1 <- glmer(acceptance ~ dependency * environment + 
                  (1 + dependency * environment | participant) + 
                  (1 + dependency * environment | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl=list(maxfun = 1e6))) %>%
  write_rds('models/ajt_suzhen_acc_md1.rds')
summary(model1)
toc()
beep()

model1 <- read_rds('models/ajt_suzhen_acc_md1.rds')

541.27 sec elapsed
optimizer (bobyqa) convergence code: 0 (OK)
boundary (singular) fit: see help('isSingular')
Fixed effects:
  Estimate Std. Error z value Pr(>|z|)
(Intercept)                           3.3386     0.5540   6.027 1.68e-09 ***
dependencypronoun                    -2.9492     0.6260  -4.711 2.46e-06 ***
environmentlong                      -3.6073     0.5756  -6.267 3.69e-10 ***
environmentisland                    -5.3472     0.6617  -8.081 6.44e-16 ***
dependencypronoun:environmentlong     5.0827     0.6939   7.325 2.39e-13 ***
dependencypronoun:environmentisland   6.3267     0.8115   7.797 6.35e-15 ***

#------------------------------------------------------------------------------#
# + + model 2
#------------------------------------------------------------------------------#

# fit model ...

tic()
model2 <- glmer(acceptance ~ dependency * environment + 
                  (1 + dependency * environment | participant) + 
                  (1 + dependency + environment | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl=list(maxfun = 1e6))) %>%
  write_rds('models/ajt_suzhen_acc_md2.rds')
summary(model2)
toc()
beep()

169.15 sec elapsed
optimizer (bobyqa) convergence code: 0 (OK)
boundary (singular) fit: see help('isSingular')
Fixed effects:
  Estimate Std. Error z value Pr(>|z|)
(Intercept)                           3.3012     0.5361   6.158 7.35e-10 ***
dependencypronoun                    -2.9123     0.6085  -4.786 1.70e-06 ***
environmentlong                      -3.5766     0.5666  -6.313 2.74e-10 ***
environmentisland                    -5.2864     0.6478  -8.161 3.32e-16 ***
dependencypronoun:environmentlong     5.0150     0.6724   7.458 8.78e-14 ***
dependencypronoun:environmentisland   6.2694     0.7996   7.840 4.50e-15 ***

# compare models ...

anova(model1, model2)

npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
model2   37 2056.3 2265.3 -991.16   1982.3
model1   48 2077.3 2348.4 -990.63   1981.3 1.0652 11     0.9999

#------------------------------------------------------------------------------#
# + + model 3
#------------------------------------------------------------------------------#

# fit model ...

tic()
model3 <- glmer(acceptance ~ dependency * environment + 
                  (1 + dependency + environment | participant) + 
                  (1 + dependency + environment | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl=list(maxfun = 1e6))) %>%
  write_rds('models/ajt_suzhen_acc_md3.rds')
summary(model3)
toc()
beep()

50.09 sec elapsed
optimizer (bobyqa) convergence code: 0 (OK)
boundary (singular) fit: see help('isSingular')
Fixed effects:
  Estimate Std. Error z value Pr(>|z|)
(Intercept)                           2.9350     0.3327   8.821  < 2e-16 ***
dependencypronoun                    -2.5634     0.3607  -7.106 1.19e-12 ***
environmentlong                      -3.1900     0.3800  -8.394  < 2e-16 ***
environmentisland                    -4.7980     0.4093 -11.722  < 2e-16 ***
dependencypronoun:environmentlong     4.4065     0.3787  11.636  < 2e-16 ***
dependencypronoun:environmentisland   5.5696     0.4096  13.598  < 2e-16 ***

# compare models ...

anova(model2, model3)

npar    AIC    BIC   logLik deviance  Chisq Df Pr(>Chisq)
model3   26 2110.8 2257.6 -1029.38   2058.8
model2   37 2056.3 2265.3  -991.16   1982.3 76.429 11  7.205e-12 ***

#------------------------------------------------------------------------------#
# + + model 4 ----
#------------------------------------------------------------------------------#

# fit model ...

tic()
model4 <- glmer(acceptance ~ dependency * environment + 
                  (1 + dependency * environment | participant) + 
                  (1 | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl=list(maxfun = 1e6))) %>%
  write_rds('models/ajt_suzhen_acc_md4.rds')
summary(model4)
toc()
beep()

34.38 sec elapsed
no warnings
Fixed effects:
  Estimate Std. Error z value Pr(>|z|)
(Intercept)                           3.3139     0.5358   6.185 6.23e-10 ***
dependencypronoun                    -2.9216     0.6075  -4.809 1.51e-06 ***
environmentlong                      -3.5764     0.5572  -6.418 1.38e-10 ***
environmentisland                    -5.2409     0.6373  -8.224  < 2e-16 ***
dependencypronoun:environmentlong     5.0092     0.6696   7.481 7.38e-14 ***
dependencypronoun:environmentisland   6.2246     0.7950   7.830 4.88e-15 ***

# compare models ...

anova(model1, model4)

npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
model4   28 2041.9 2200.1 -992.94   1985.9
model1   48 2077.3 2348.4 -990.63   1981.3 4.6243 20     0.9998

anova(model2, model4)

npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
model4   28 2041.9 2200.1 -992.94   1985.9
model2   37 2056.3 2265.3 -991.16   1982.3 3.5592  9      0.938

anova(model5, model4)

npar    AIC    BIC   logLik deviance  Chisq Df Pr(>Chisq)
model5   17 2096.0 2192.0 -1030.98   2062.0
model4   28 2041.9 2200.1  -992.94   1985.9 76.079 11  8.411e-12 ***

# pairwise comparisons ...

model4 %>%
  emmeans(~ dependency * environment) %>%
  contrast('pairwise', by = 'environment') %>%
  summary(by = NULL, adjust = 'holm')

#------------------------------------------------------------------------------#
# + + model 5
#------------------------------------------------------------------------------#

# fit model ...

tic()
model5 <- glmer(acceptance ~ dependency * environment + 
                  (1 + dependency + environment | participant) + 
                  (1 | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl=list(maxfun = 1e6))) %>%
  write_rds('models/ajt_suzhen_acc_md5.rds')
summary(model5)
toc()
beep()

8.73 sec elapsed
no warnings
Fixed effects:
  Estimate Std. Error z value Pr(>|z|)
(Intercept)                           2.9267     0.3308   8.847  < 2e-16 ***
dependencypronoun                    -2.5554     0.3561  -7.175 7.22e-13 ***
environmentlong                      -3.1733     0.3743  -8.479  < 2e-16 ***
environmentisland                    -4.7520     0.4015 -11.834  < 2e-16 ***
dependencypronoun:environmentlong     4.3842     0.3766  11.641  < 2e-16 ***
dependencypronoun:environmentisland   5.5197     0.4045  13.645  < 2e-16 ***

# compare models ...

anova(model1, model5)
  
npar    AIC    BIC   logLik deviance  Chisq Df Pr(>Chisq)
model5   17 2096.0 2192.0 -1030.98   2062.0
model1   48 2077.3 2348.4  -990.63   1981.3 80.704 31  2.634e-06 ***

#------------------------------------------------------------------------------#
# + + model 6
#------------------------------------------------------------------------------#

# fit model ...

tic()
model6 <- glmer(acceptance ~ dependency * environment + 
                  (1 | participant) + 
                  (1 | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl=list(maxfun = 1e6))) %>%
  write_rds('models/ajt_suzhen_acc_md6.rds')
summary(model6)
toc()
beep()

1.5 sec elapsed
no warnings
Fixed effects:
  Estimate Std. Error z value Pr(>|z|)
(Intercept)                           2.5102     0.2287   10.98   <2e-16 ***
dependencypronoun                    -2.2151     0.2212  -10.01   <2e-16 ***
environmentlong                      -2.7001     0.2234  -12.09   <2e-16 ***
environmentisland                    -3.9516     0.2405  -16.43   <2e-16 ***
dependencypronoun:environmentlong     3.5348     0.2879   12.28   <2e-16 ***
dependencypronoun:environmentisland   4.5614     0.2996   15.23   <2e-16 ***

# compare models ...

anova(model1, model6)
  
npar    AIC    BIC   logLik deviance  Chisq Df Pr(>Chisq)
model6    8 2239.2 2284.4 -1111.58   2223.2
model1   48 2077.3 2348.4  -990.63   1981.3 241.91 40  < 2.2e-16 ***

#------------------------------------------------------------------------------#
# + sukoko ----
#------------------------------------------------------------------------------#

# sukoko = SRC study (su) + KLE group (ko) + Korean AJT (ko)

# filter for analysis ...

md <- temp %>% 
  filter(study == '210510_su', 
         group == 'korean',
         task == 'korean_ajt')

check <- md %>% 
  group_by(study, group, participant) %>%
  summarise() %>%
  ungroup()

# apply deviation coding ...

contrasts(md$dependency) <- contr.treatment(2) - matrix(rep(1/2, 2), ncol=1)
contrasts(md$environment) <- contr.treatment(3) - matrix(rep(1/3, 6), ncol=2)

# view contrasts ...

contrasts(md$dependency)
contrasts(md$environment)

#------------------------------------------------------------------------------#
# + + model 1 (maximal model) 
#------------------------------------------------------------------------------#

# fit model ...

tic()
model1 <- glmer(acceptance ~ dependency * environment + 
                  (1 + dependency * environment | participant) + 
                  (1 + dependency * environment | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl=list(maxfun = 1e6))) %>%
  write_rds('models/ajt_sukoko_acc_md1.rds')
summary(model1)
toc()
beep()

1618.44 sec elapsed
Model failed to converge with max|grad| = 0.0540659 (tol = 0.002, component 1)
Model is nearly unidentifiable: very large eigenvalue
- Rescale variables?
  Model is nearly unidentifiable: large eigenvalue ratio
- Rescale variables?

#------------------------------------------------------------------------------#
# + + model 3
#------------------------------------------------------------------------------#

# fit model ...

tic()
model3 <- glmer(acceptance ~ dependency * environment + 
                  (1 + dependency + environment | participant) + 
                  (1 + dependency + environment | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl=list(maxfun = 1e6))) %>%
  write_rds('models/ajt_sukoko_acc_md3.rds')
summary(model3)
toc()
beep()

38.71 sec elapsed
Model failed to converge with max|grad| = 0.13897 (tol = 0.002, component 1)
Model is nearly unidentifiable: very large eigenvalue
- Rescale variables?

#------------------------------------------------------------------------------#
# + + model 4
#------------------------------------------------------------------------------#

# fit model ...

tic()
model4 <- glmer(acceptance ~ dependency * environment + 
                  (1 + dependency * environment | participant) + 
                  (1 | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl=list(maxfun = 1e6))) %>%
  write_rds('models/ajt_sukoko_acc_md4.rds')
summary(model4)
toc()
beep()

56.32 sec elapsed
Model failed to converge with max|grad| = 0.131685 (tol = 0.002, component 1)
Model is nearly unidentifiable: very large eigenvalue
- Rescale variables?

#------------------------------------------------------------------------------#
# + + model 5 ----
#------------------------------------------------------------------------------#

# fit model ...

tic()
model5 <- glmer(acceptance ~ dependency * environment + 
                  (1 + dependency + environment | participant) + 
                  (1 | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl=list(maxfun = 1e6))) %>%
  write_rds('models/ajt_sukoko_acc_md5.rds')
summary(model5)
toc()
beep()

16 sec elapsed
no warnings
Fixed effects:
  Estimate Std. Error z value Pr(>|z|)
(Intercept)                           5.5694     0.6828   8.157 3.44e-16 ***
dependencypronoun                    -8.3793     0.8100 -10.345  < 2e-16 ***
environmentlong                      -3.4099     0.7583  -4.497 6.89e-06 ***
environmentisland                    -3.9077     0.7722  -5.060 4.19e-07 ***
dependencypronoun:environmentlong     6.3096     0.8070   7.819 5.32e-15 ***
dependencypronoun:environmentisland   6.7195     0.8131   8.264  < 2e-16 ***

# compare models ...

anova(model5, model6)

npar    AIC    BIC  logLik deviance Chisq Df Pr(>Chisq)
model6    8 1640.0 1684.7 -811.98   1624.0
model5   17 1473.9 1568.9 -719.93   1439.9 184.1  9  < 2.2e-16 ***

# check assumptions ...

check_model(model5)
ggsave('plots/check_model.png', width=10, height=10, dpi=600)

# pairwise comparisons ...

model5 %>%
  emmeans(~ dependency * environment) %>%
  contrast('pairwise', by = 'environment') %>%
  summary(by = NULL, adjust = 'holm')

# generate table ...

kbl(summary(model5)$coefficients, digits = c(2, 2, 2, 3))

#------------------------------------------------------------------------------#
# + + model 6
#------------------------------------------------------------------------------#

# fit model ...

tic()
model6 <- glmer(acceptance ~ dependency * environment + 
                  (1 | participant) + 
                  (1 | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl=list(maxfun = 1e6))) %>%
  write_rds('models/ajt_sukoko_acc_md6.rds')
summary(model6)
toc()
beep()

1.77 sec elapsed
no warnings
Fixed effects:
  Estimate Std. Error z value Pr(>|z|)
(Intercept)                           5.7806     0.6001   9.633  < 2e-16 ***
dependencypronoun                    -8.2492     0.6113 -13.495  < 2e-16 ***
environmentlong                      -4.3531     0.5815  -7.486 7.12e-14 ***
environmentisland                    -4.6260     0.5824  -7.943 1.97e-15 ***
dependencypronoun:environmentlong     6.9052     0.6311  10.942  < 2e-16 ***
dependencypronoun:environmentisland   7.0871     0.6319  11.216  < 2e-16 ***

#------------------------------------------------------------------------------#
# + suzhzh ----
#------------------------------------------------------------------------------#

# suzhzh = SRC study (su) + MLE group (zh) + Mandarin AJT (zh)

# filter for analysis ...

md <- temp %>% 
  filter(study == '210510_su', 
         group == 'mandarin',
         task == 'mandarin_ajt')

check <- md %>% 
  group_by(study, group, participant) %>%
  summarise() %>%
  ungroup()

# apply deviation coding ...

contrasts(md$dependency) <- contr.treatment(2) - matrix(rep(1/2, 2), ncol=1)
contrasts(md$environment) <- contr.treatment(3) - matrix(rep(1/3, 6), ncol=2)

# view contrasts ...

contrasts(md$dependency)
contrasts(md$environment)

#------------------------------------------------------------------------------#
# + + model 1 (maximal model) 
#------------------------------------------------------------------------------#

# fit model ...

tic()
model1 <- glmer(acceptance ~ dependency * environment + 
                  (1 + dependency * environment | participant) + 
                  (1 + dependency * environment | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl=list(maxfun = 1e6))) %>%
  write_rds('models/ajt_suzhzh_acc_md1.rds')
summary(model1)
toc()
beep()

model1 <- read_rds('models/ajt_suzhzh_acc_md1.rds')

569.18 sec elapsed
optimizer (bobyqa) convergence code: 0 (OK)
boundary (singular) fit: see help('isSingular')
Fixed effects:
  Estimate Std. Error z value Pr(>|z|)
(Intercept)                           4.6374     0.8093   5.730 1.00e-08 ***
dependencypronoun                    -5.1865     0.8298  -6.250 4.10e-10 ***
environmentlong                      -3.0617     0.8998  -3.403 0.000667 ***
environmentisland                    -4.7561     0.8867  -5.364 8.16e-08 ***
dependencypronoun:environmentlong     4.6731     0.8738   5.348 8.88e-08 ***
dependencypronoun:environmentisland   6.2212     0.8490   7.327 2.35e-13 ***

#------------------------------------------------------------------------------#
# + + model 5
#------------------------------------------------------------------------------#

# fit model ...

tic()
model5 <- glmer(acceptance ~ dependency * environment + 
                  (1 + dependency + environment | participant) + 
                  (1 | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl=list(maxfun = 1e6))) %>%
  write_rds('models/ajt_suzhzh_acc_md5.rds')
summary(model5)
toc()
beep()

14.05 sec elapsed
no warnings
Fixed effects:
  Estimate Std. Error z value Pr(>|z|)
(Intercept)                           4.4561     0.4587   9.714  < 2e-16 ***
dependencypronoun                    -4.9133     0.4511 -10.891  < 2e-16 ***
environmentlong                      -2.9448     0.5747  -5.125 2.98e-07 ***
environmentisland                    -4.5467     0.5466  -8.318  < 2e-16 ***
dependencypronoun:environmentlong     4.5429     0.5027   9.036  < 2e-16 ***
dependencypronoun:environmentisland   5.9150     0.4929  12.000  < 2e-16 ***

# compare models ...

anova()

#------------------------------------------------------------------------------#
# + + model 6
#------------------------------------------------------------------------------#

# fit model ...

tic()
model6 <- glmer(acceptance ~ dependency * environment + 
                  (1 | participant) + 
                  (1 | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl=list(maxfun = 1e6))) %>%
  write_rds('models/ajt_suzhzh_acc_md6.rds')
summary(model6)
toc()
beep()

1.81 sec elapsed
no warnings
Fixed effects:
  Estimate Std. Error z value Pr(>|z|)
(Intercept)                           3.7410     0.3206  11.670   <2e-16 ***
dependencypronoun                    -4.1345     0.3112 -13.286   <2e-16 ***
environmentlong                      -2.7422     0.3070  -8.932   <2e-16 ***
environmentisland                    -3.8508     0.3090 -12.461   <2e-16 ***
dependencypronoun:environmentlong     3.7957     0.3563  10.654   <2e-16 ***
dependencypronoun:environmentisland   4.8649     0.3594  13.537   <2e-16 ***

# compare models ...

anova(model1, model6)
  
npar    AIC    BIC   logLik deviance  Chisq Df Pr(>Chisq)
model6    8 2250.7 2296.1 -1117.35   2234.7
model1   48 2057.8 2330.4  -980.92   1961.8 272.87 40  < 2.2e-16 ***

#------------------------------------------------------------------------------#
# interaction plots for fillers ----
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
# density plots for ajt critical trials ----
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
# bar plot of rating distributions ----
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
# corrections to files 
#------------------------------------------------------------------------------#

files <- fs::dir_ls('data/subjects/korean/raw', regexp = 'task5.*\\.csv$')

for (file in files) {
  x <- read_csv(file, col_types = cols(.default = 'c')) %>%
    rename(response = responses)
  write_csv(x, file)
}

#------------------------------------------------------------------------------#
# check submissions 
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


#==============================================================================#
# ::::: ajt data vs. spr data ::::: ----
#==============================================================================#

#------------------------------------------------------------------------------#
# relationship between spr and ajt data (all ORC environments) ----
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
# relationship between spr and ajt data (ORC island environment) ----
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
# relationship between spr and ajt data (ORC long environment) ----
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
# relationship between spr and ajt data (ORC short environment) ----
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

#==============================================================================#
# ::::: ajt data vs. c-test data ::::: ----
#==============================================================================#

#------------------------------------------------------------------------------#
# proficiency effects - all environments - raw ratings ----
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
# proficiency effects - island environment - raw ratings ----
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
# proficiency effects - island environment - binary ratings ----
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

#==============================================================================#
# ::::: key ::::: ----
#==============================================================================#

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

#==============================================================================#
# ::::: chopping block ::::: ----
#==============================================================================#

#------------------------------------------------------------------------------#
# ept: complex bar plot
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
# ept: interaction plot
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
# ept: scatter plot of resumption rate against proficiency scores (by environment)
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
# plots for rt data by participant and by item
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
# plots for accuracy data by participant and by item
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

#------------------------------------------------------------------------------#
# modeling for accuracy data, lmer ----
#------------------------------------------------------------------------------#

# prep dataframe

temp <- spr_trim %>%
  group_by(study, group, participant, item, dependency, environment, acc_rate, accuracy) %>%
  summarise() %>%
  ungroup() %>%
  mutate(dependency = fct_drop(dependency),
         environment = fct_drop(environment),
         accuracy = as.logical(accuracy),
         participant = as.factor(participant),
         item = as.factor(item)) %>%
  mutate(environment = fct_relevel(environment, 'short', 'long', 'island'))

check <- temp %>%
  group_by(study, group, participant) %>%
  summarise() %>%
  ungroup() %>%
  group_by(study, group) %>%
  summarise(n = n()) %>%
  ungroup()
# doen 90, doko 66, dozh 67, suen 61, suko 63, suzh 69

# calculate accuracy rates by condition for participant

temp <- temp %>%
  group_by(study, group, participant, dependency, environment) %>%
  summarise(accuracy = mean(accuracy, na.rm = TRUE)) %>%
  ungroup()

# check severity of ceiling effects by calculating proportion of participant with perfect scores by group and condition

check <- temp %>%
  mutate(ceiling = case_when(accuracy == 1 ~ TRUE, TRUE ~ FALSE)) %>%
  group_by(study, group, dependency, environment) %>%
  summarise(ceiling = mean(ceiling, na.rm = TRUE)) %>%
  ungroup()

# check frequency of least frequent outcome (i.e., incorrect responses; there should be at least 10 per cell for logistic regression)

check <- spr_trim %>%
  mutate(accuracy = as.logical(accuracy)) %>%
  mutate(check = case_when(accuracy == FALSE ~ TRUE, TRUE ~ FALSE)) %>%
  group_by(study, group, dependency, environment) %>%
  summarise(check = sum(check)) %>%
  ungroup()

# generate plot

plot <- temp %>%
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
  facet_grid(study~group)

# filter for modeling

md <- temp %>% 
  filter(study == '210510_do', group == 'english')

check <- md %>%
  group_by(study, group, participant) %>%
  summarise() %>%
  ungroup() %>%
  group_by(study, group) %>%
  summarise(n = n()) %>%
  ungroup()

# view contrasts

contrasts(md$dependency)
contrasts(md$environment)

# construct model

model1 <- lmer(accuracy ~ dependency*environment + (1|participant), data = md)
summary(model1)
beep(1)

# check model performance

performance::check_model(model1) # perform checks
ggsave('plots/check_model.png', width=10, height=10, dpi=600) # save output
performance::model_performance(model1) # check model performance
lattice::qqmath(model1, id = 0.05) # check for normality of residuals (identified outliers)
car::leveneTest(residuals(model1) ~ md$environment * md$dependency) # check homogeneity of residual variance (should not be significant)

# doen
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                           0.946667   0.011324 384.508828  83.597   <2e-16 ***
# dependencypronoun                     0.026667   0.013600 445.000024   1.961   0.0505 .  
# environmentlong                      -0.011111   0.013600 445.000024  -0.817   0.4144    
# environmentisland                    -0.006667   0.013600 445.000024  -0.490   0.6242    
# dependencypronoun:environmentlong     0.011111   0.019233 445.000024   0.578   0.5638    
# dependencypronoun:environmentisland   0.004444   0.019233 445.000024   0.231   0.8174  

# doko
# Fixed effects:
#   Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)                           0.77879    0.02336 255.70147  33.336  < 2e-16 ***
# dependencypronoun                     0.14849    0.02716 325.00000   5.467 9.15e-08 ***
# environmentlong                      -0.03030    0.02716 325.00000  -1.116    0.265    
# environmentisland                    -0.00303    0.02716 325.00000  -0.112    0.911    
# dependencypronoun:environmentlong     0.01515    0.03841 325.00000   0.394    0.694    
# dependencypronoun:environmentisland  -0.00303    0.03841 325.00000  -0.079    0.937 

# dozh
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                          9.015e-01  1.920e-02  2.590e+02  46.945   <2e-16 ***
# dependencypronoun                    4.776e-02  2.231e-02  3.300e+02   2.141    0.033 *  
# environmentlong                     -8.955e-03  2.231e-02  3.300e+02  -0.401    0.688    
# environmentisland                   -2.985e-02  2.231e-02  3.300e+02  -1.338    0.182    
# dependencypronoun:environmentlong    6.361e-15  3.155e-02  3.300e+02   0.000    1.000    
# dependencypronoun:environmentisland  3.881e-02  3.155e-02  3.300e+02   1.230    0.220 

# suen
# Fixed effects:
#   Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)                           0.91803    0.01755 263.09736  52.301  < 2e-16 ***
# dependencypronoun                     0.03279    0.02119 300.00000   1.547  0.12283    
# environmentlong                      -0.02623    0.02119 300.00000  -1.238  0.21672    
# environmentisland                    -0.10164    0.02119 300.00000  -4.797 2.54e-06 ***
# dependencypronoun:environmentlong     0.01311    0.02997 300.00000   0.438  0.66194    
# dependencypronoun:environmentisland   0.09508    0.02997 300.00000   3.173  0.00166 ** 

# suko
# Fixed effects:
#   Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)                           0.92063    0.02185 291.88005  42.140  < 2e-16 ***
# dependencypronoun                     0.04127    0.02704 310.00000   1.526   0.1279    
# environmentlong                      -0.10794    0.02704 310.00000  -3.992 8.17e-05 ***
# environmentisland                    -0.13651    0.02704 310.00000  -5.049 7.58e-07 ***
# dependencypronoun:environmentlong     0.08254    0.03823 310.00000   2.159   0.0316 *  
# dependencypronoun:environmentisland   0.09841    0.03823 310.00000   2.574   0.0105 *  

# suzh
# Fixed effects:
#   Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)                           0.92174    0.02006 339.20397  45.956  < 2e-16 ***
# dependencypronoun                     0.02029    0.02535 339.99726   0.800 0.424005    
# environmentlong                      -0.10725    0.02535 339.99726  -4.231    3e-05 ***
# environmentisland                    -0.08696    0.02535 339.99726  -3.431 0.000677 ***
# dependencypronoun:environmentlong     0.11304    0.03585 339.99726   3.153 0.001757 ** 
# dependencypronoun:environmentisland   0.06377    0.03585 339.99726   1.779 0.076152 .  

# post-hoc tests

pairwise1 <- model1 %>%
  emmeans(~ dependency * environment) %>%
  contrast('pairwise', by = 'environment') %>%
  summary(by = NULL, adjust = 'holm')
pairwise1

# suen
# contrast      environment estimate     SE  df t.ratio p.value
# gap - pronoun short        -0.0328 0.0212 300  -1.547  0.1228
# gap - pronoun long         -0.0459 0.0212 300  -2.166  0.0621 .
# gap - pronoun island       -0.1279 0.0212 300  -6.035  <.0001 ***
# Degrees-of-freedom method: kenward-roger 
# P value adjustment: holm method for 3 tests 

# suko
# contrast      environment estimate    SE  df t.ratio p.value
# gap - pronoun short        -0.0413 0.027 310  -1.526  0.1279
# gap - pronoun long         -0.1238 0.027 310  -4.579  <.0001 ***
# gap - pronoun island       -0.1397 0.027 310  -5.167  <.0001 ***
# Degrees-of-freedom method: kenward-roger 
# P value adjustment: holm method for 3 tests 

# suzh
# contrast      environment estimate     SE  df t.ratio p.value
# gap - pronoun short        -0.0203 0.0253 340  -0.800  0.4240
# gap - pronoun long         -0.1333 0.0253 340  -5.260  <.0001 ***
# gap - pronoun island       -0.0841 0.0253 340  -3.316  0.0020 **
# Degrees-of-freedom method: kenward-roger 
# P value adjustment: holm method for 3 tests 

#------------------------------------------------------------------------------#
# modeling for reading times at critical region ----
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
# modeling for reading times at RP region ----
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

