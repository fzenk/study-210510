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
library(emmeans) #  for post-hoc tests; see https://marissabarlaz.github.io/portfolio/contrastcoding/
library(performance) # for checking model performance

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

#------------------------------------------------------------------------------#
# plots of raw RTs ----
#------------------------------------------------------------------------------#

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

# check participants ...

check <- spr_trim %>%
  group_by(study, group, participant) %>%
  summarise() %>%
  summarise(n = n()) %>%
  ungroup()

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

# filter data for analysis

md <- spr_trim %>%
  filter(region2 == 3,
         study == '210510_do',
         group == 'english')

# check distribution

hist(md$rrt)
qqnorm(md$rrt)

md <- md %>%
  mutate(dependency = as.factor(dependency),
         environment = as.factor(environment)) %>%
  mutate(environment = fct_relevel(environment, 'short', 'long', 'island'))

# apply deviation coding ...

contrasts(md$dependency) <- contr.treatment(2) - matrix(rep(1/2, 2), ncol=1)
contrasts(md$environment) <- contr.treatment(3) - matrix(rep(1/3, 6), ncol=2)

# view contrasts ...

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
# dependencypronoun                      2.508     11.716  93.254   0.214  0.83097   
# environmentlong                        6.518     12.629  40.439   0.516  0.60859   
# environmentisland                     28.809     11.464  65.225   2.513  0.01446 * 
# environmentlong:dependencypronoun    -15.898     17.741  44.225  -0.896  0.37507   
# environmentisland:dependencypronoun  -52.826     16.303 113.487  -3.240  0.00157 **
saveRDS(mod_spr_1, file='models/orc_spr_doen_region1_mod.rds') 
cat(capture.output(summary(mod_spr_1)), file = paste('models/orc_spr_doen_region1_mod.txt', sep = ''), sep='\n')
# mod_spr_1 <- readRDS('models/orc_spr_doen_region1_mod.rds')
# https://www.r-bloggers.com/2012/04/a-better-way-of-saving-and-loading-objects-in-r/

# doen region 2
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#                                     Estimate Std. Error      df t value Pr(>|t|)    
# (Intercept)                           18.909      7.447  82.527   2.539 0.012985 *  
# dependencypronoun                     24.560      9.479  68.194   2.591 0.011697 *  
# environmentlong                       -1.983      7.723 814.992  -0.257 0.797429    
# environmentisland                     26.983      9.589  47.803   2.814 0.007086 ** 
# environmentlong:dependencypronoun    -16.569     13.173  36.622  -1.258 0.216433    
# environmentisland:dependencypronoun  -50.294     14.149  73.771  -3.555 0.000665 ***
saveRDS(mod_spr_1, file='models/orc_spr_doen_region2_mod.rds') 
cat(capture.output(summary(mod_spr_1)), file = paste('models/orc_spr_doen_region1_mod.txt', sep = ''), sep='\n')
# mod_spr_1 <- readRDS('models/orc_spr_doen_region2_mod.rds')

# doen region 3
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error       df t value Pr(>|t|)  
# (Intercept)                          16.2765     8.8078  68.0970   1.848   0.0690 .
# dependencypronoun                    13.0496     7.5429  86.1457   1.730   0.0872 .
# environmentlong                      -4.5110     7.6742  67.6572  -0.588   0.5586  
# environmentisland                    12.8944     7.7875  63.8697   1.656   0.1027  
# environmentlong:dependencypronoun     0.6997    11.9051  38.2415   0.059   0.9534  
# environmentisland:dependencypronoun -16.4919    12.7929  34.1701  -1.289   0.2060 
saveRDS(mod_spr_1, file='models/orc_spr_doen_region3_mod.rds') 
cat(capture.output(summary(mod_spr_1)), file = paste('models/orc_spr_doen_region3_mod.txt', sep = ''), sep='\n')

# doko region 1
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error      df t value Pr(>|t|)
# (Intercept)                          -22.700     17.960  46.022  -1.264    0.213
# dependencypronoun                    -33.742     22.493  36.054  -1.500    0.142
# environmentlong                       -2.302     20.502  42.972  -0.112    0.911
# environmentisland                     -6.051     22.236  31.049  -0.272    0.787
# environmentlong:dependencypronoun     12.442     25.884  90.654   0.481    0.632
# environmentisland:dependencypronoun   31.638     31.099  31.953   1.017    0.317
saveRDS(mod_spr_1, file='models/orc_spr_doko_region1_mod.rds') 
cat(capture.output(summary(mod_spr_1)), file = paste('models/orc_spr_doko_region1_mod.txt', sep = ''), sep='\n')

# doko region 2
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error     df t value Pr(>|t|)  
# (Intercept)                           -40.71      17.18  68.23  -2.370   0.0206 *
# dependencypronoun                     -19.45      14.52  95.38  -1.340   0.1834  
# environmentlong                        35.00      17.41  50.39   2.010   0.0498 *
# environmentisland                      28.40      20.58  61.53   1.380   0.1726  
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
# dependencypronoun                    -30.901     18.613  43.521  -1.660    0.104
# environmentlong                      -20.596     16.682  50.175  -1.235    0.223
# environmentisland                      3.596     18.830  29.555   0.191    0.850
# environmentlong:dependencypronoun      5.751     23.048  96.870   0.250    0.803
# environmentisland:dependencypronoun    8.417     25.537  30.334   0.330    0.744
saveRDS(mod_spr_1, file='models/orc_spr_doko_region3_mod.rds') 
cat(capture.output(summary(mod_spr_1)), file = paste('models/orc_spr_doko_region3_mod.txt', sep = ''), sep='\n')

# dozh region 1
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error      df t value Pr(>|t|)  
# (Intercept)                          -12.773     21.019  50.146  -0.608   0.5461  
# dependencypronoun                    -39.771     23.848  45.319  -1.668   0.1023  
# environmentlong                        4.094     24.508  36.950   0.167   0.8683  
# environmentisland                    -15.724     20.665  89.120  -0.761   0.4487  
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
# dependencypronoun                     -35.86      14.94  65.28  -2.400   0.0193 *
# environmentlong                        25.54      20.19  36.88   1.265   0.2138  
# environmentisland                      33.54      21.17  42.22   1.585   0.1205  
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
# dependencypronoun                    -23.355     20.605  45.019  -1.133    0.263
# environmentlong                      -16.891     21.333  32.707  -0.792    0.434
# environmentisland                     -5.552     24.676  35.726  -0.225    0.823
# environmentlong:dependencypronoun     10.054     25.456  57.158   0.395    0.694
# environmentisland:dependencypronoun    8.032     29.335  39.327   0.274    0.786
saveRDS(mod_spr_1, file='models/orc_spr_dozh_region3_mod.rds') 
cat(capture.output(summary(mod_spr_1)), file = paste('models/orc_spr_dozh_region3_mod.txt', sep = ''), sep='\n')

# suen region 1
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error       df t value Pr(>|t|)    
# (Intercept)                         -94.4957    11.5507  52.3598  -8.181 6.28e-11 ***
# dependencypronoun                    35.7479    15.8883  35.0287   2.250   0.0308 *  
# environmentlong                       0.4633    15.2077  68.2516   0.030   0.9758    
# environmentisland                    35.4499    17.8622  44.7862   1.985   0.0533 .  
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
# dependencypronoun                      29.40      13.26   57.68   2.216   0.0306 *  
# environmentlong                        13.23      18.61   45.12   0.711   0.4807    
# environmentisland                      94.65      12.18  271.83   7.769 1.63e-13 ***
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
# dependencypronoun                      17.83      15.81   34.41   1.128  0.26721    
# environmentlong                        18.05      15.42   32.72   1.170  0.25034    
# environmentisland                     187.31      23.15   45.43   8.090 2.38e-10 ***
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
# dependencypronoun                   -15.8171    32.4809  63.1945  -0.487    0.628
# environmentlong                     -14.9229    39.0395  35.4573  -0.382    0.705
# environmentisland                     0.8691    39.7102  29.9687   0.022    0.983
# environmentlong:dependencypronoun   -14.2447    54.4191  54.4822  -0.262    0.794
# environmentisland:dependencypronoun -13.4231    56.9894  38.8554  -0.236    0.815
saveRDS(mod_spr_1, file='models/src_spr_suko_region1_mod.rds') 
cat(capture.output(summary(mod_spr_1)), file = paste('models/src_spr_suko_region1_mod.txt', sep = ''), sep='\n')

# suko region 2
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error       df t value Pr(>|t|)   
# (Intercept)                           -9.833     19.532   39.301  -0.503  0.61747   
# dependencypronoun                    -12.304     27.666   40.168  -0.445  0.65888   
# environmentlong                       73.007     29.217   34.140   2.499  0.01744 * 
# environmentisland                     -7.263     26.873   36.802  -0.270  0.78847   
# environmentlong:dependencypronoun   -103.034     35.722   47.964  -2.884  0.00586 **
# environmentisland:dependencypronoun    4.768     41.479   32.796   0.115  0.90918  
saveRDS(mod_spr_1, file='models/src_spr_suko_region2_mod.rds') 
cat(capture.output(summary(mod_spr_1)), file = paste('models/src_spr_suko_region2_mod.txt', sep = ''), sep='\n')

# suko region 3
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error      df t value Pr(>|t|)    
# (Intercept)                           -76.23      18.03   70.73  -4.228 6.94e-05 ***
# dependencypronoun                      67.74      26.50   51.88   2.556 0.013558 *  
# environmentlong                        60.95      29.51   45.10   2.065 0.044701 *  
# environmentisland                     125.22      27.11   62.84   4.619 1.96e-05 ***
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
# dependencypronoun                     -10.62      29.14   63.91  -0.364 0.716848    
# environmentlong                        54.39      35.07   46.38   1.551 0.127767    
# environmentisland                     108.05      31.21   74.53   3.462 0.000892 ***
# environmentlong:dependencypronoun    -112.29      47.51   39.20  -2.364 0.023153 *  
# environmentisland:dependencypronoun  -116.55      43.54   63.17  -2.677 0.009457 ** 
saveRDS(mod_spr_1, file='models/src_spr_suzh_region1_mod.rds') 
cat(capture.output(summary(mod_spr_1)), file = paste('models/src_spr_suzh_region1_mod.txt', sep = ''), sep='\n')

# suzh region 2
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)                          -63.1519    19.2731   79.8946  -3.277  0.00156 ** 
# dependencypronoun                     -0.9302    24.0921  367.5248  -0.039  0.96922    
# environmentlong                      171.1295    31.4737   59.9389   5.437 1.05e-06 ***
# environmentisland                     53.4415    32.3608   58.6065   1.651  0.10400    
# environmentlong:dependencypronoun   -176.4626    40.7114   69.6624  -4.334 4.82e-05 ***
# environmentisland:dependencypronoun  -50.2838    34.2306  265.3421  -1.469  0.14303  
saveRDS(mod_spr_1, file='models/src_spr_suzh_region2_mod.rds') 
cat(capture.output(summary(mod_spr_1)), file = paste('models/src_spr_suzh_region2_mod.txt', sep = ''), sep='\n')

# suzh region 3
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error      df t value Pr(>|t|)
# (Intercept)                           13.256     33.098  47.181   0.401    0.691
# dependencypronoun                    -20.730     32.301  38.533  -0.642    0.525
# environmentlong                       31.380     37.616  36.211   0.834    0.410
# environmentisland                    -23.822     39.817  55.114  -0.598    0.552
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
# gap - pronoun island         
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

write_rds(model1, 'models/spr_doen_acc_md1.rds')

model1 <- read_rds('models/spr_doen_acc_md1.rds')

optimizer (bobyqa) convergence code: 0 (OK)
boundary (singular) fit: see help('isSingular')
Fixed effects:
  Estimate Std. Error z value Pr(>|z|)
(Intercept)                           3.8458     0.5919   6.497 8.17e-11 ***
dependencypronoun                     0.8593     1.1411   0.753    0.451
environmentlong                      -0.3853     0.7153  -0.539    0.590
environmentisland                     0.3546     0.9122   0.389    0.697
environmentlong:dependencypronoun     0.8629     1.6303   0.529    0.597
environmentisland:dependencypronoun  -0.3207     1.5887  -0.202    0.840

#------------------------------------------------------------------------------#
# + + model 2 
#------------------------------------------------------------------------------#

tic()
model2 <- glmer(accuracy ~ dependency * environment + 
                  (dependency * environment || participant) + 
                  (dependency * environment || item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_doen_acc_md2.rds')
summary(model2)
toc()
beep()

# skip for now

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

# skip for now

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

# skip for now

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

#------------------------------------------------------------------------------#
# + + model 6 (final?) ----
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

doko (everybody)
boundary (singular) fit: see help('isSingular')
Fixed effects:
  Estimate Std. Error z value Pr(>|z|)
(Intercept)                          1.60016    0.25551   6.263 3.78e-10 ***
dependencypronoun                    1.89983    0.57708   3.292 0.000994 ***
environmentlong                     -0.11455    0.31752  -0.361 0.718264
environmentisland                    0.06303    0.36812   0.171 0.864053
dependencypronoun:environmentlong   -0.40906    0.70812  -0.578 0.563487
dependencypronoun:environmentisland -0.76532    0.72946  -1.049 0.294104

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

# 6.42 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                2.1255     0.1808  11.758  < 2e-16 ***
# dependency2                1.4563     0.1824   7.986  1.4e-15 ***
# environment2              -0.1617     0.2204  -0.734    0.463    
# environment3              -0.1188     0.2101  -0.565    0.572    
# dependency2:environment2  -0.1473     0.3634  -0.405    0.685    
# dependency2:environment3  -0.2733     0.3584  -0.762    0.446 

#------------------------------------------------------------------------------#
# + + model 6 (final?) ----
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

# 1.71 sec elapsed
# no warnings
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)               2.07619    0.17413  11.923   <2e-16 ***
# dependency2               1.36374    0.14114   9.662   <2e-16 ***
# environment2             -0.20931    0.16932  -1.236    0.216    
# environment3             -0.08972    0.17142  -0.523    0.601    
# dependency2:environment2 -0.06965    0.34038  -0.205    0.838    
# dependency2:environment3 -0.19870    0.34262  -0.580    0.562 

# compare models ...

anova(model5, model6)

# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model6    8 1622.6 1667.5 -803.29   1606.6                     
# model5   17 1637.1 1732.7 -801.56   1603.1 3.4623  9     0.9431

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

dozh (everybody)
boundary (singular) fit: see help('isSingular')
Fixed effects:
  Estimate Std. Error z value Pr(>|z|)
(Intercept)                          3.999463   0.705482   5.669 1.44e-08 ***
dependencypronoun                   -0.001692   0.855696  -0.002   0.9984
environmentlong                     -0.825229   0.760581  -1.085   0.2779
environmentisland                   -1.461149   0.772294  -1.892   0.0585 .
dependencypronoun:environmentlong    1.438693   1.210922   1.188   0.2348
dependencypronoun:environmentisland  2.191246   1.307525   1.676   0.0938 .

# post-hoc tests ...

pairwise1 <- model1 %>%
  emmeans(~ dependency * environment) %>%
  contrast('pairwise', by = 'environment') %>%
  summary(by = NULL, adjust = 'holm')
pairwise1

contrast      environment estimate    SE  df z.ratio p.value
gap - pronoun short        0.00169 0.856 Inf   0.002  0.9984
gap - pronoun long        -1.43700 0.864 Inf  -1.662  0.1929
gap - pronoun island      -2.18955 0.977 Inf  -2.242  0.0749 .

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

137.08 sec elapsed
optimizer (bobyqa) convergence code: 0 (OK)
boundary (singular) fit: see help('isSingular')
Fixed effects:
  Estimate Std. Error z value Pr(>|z|)
(Intercept)                           3.6951     0.5466   6.760 1.38e-11 ***
dependencypronoun                     0.5366     0.5069   1.059    0.290
environmentlong                      -0.3856     0.5860  -0.658    0.511
environmentisland                    -1.2327     0.5910  -2.086    0.037 *
dependencypronoun:environmentlong     0.0604     0.5123   0.118    0.906
dependencypronoun:environmentisland   0.7486     0.5842   1.281    0.200

# compare models ...

anova(model3, model1)

npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
model3   26 1014.5 1160.3 -481.27   962.54
model1   48 1047.8 1316.8 -475.88   951.76 10.777 22     0.9778

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

308.89 sec elapsed
optimizer (bobyqa) convergence code: 0 (OK)
boundary (singular) fit: see help('isSingular')
Fixed effects:
  Estimate Std. Error z value Pr(>|z|)
(Intercept)                          3.67842    0.59441   6.188 6.08e-10 ***
dependencypronoun                    0.06087    0.67276   0.090   0.9279
environmentlong                     -0.48737    0.65749  -0.741   0.4585
environmentisland                   -1.10440    0.64765  -1.705   0.0881 .
dependencypronoun:environmentlong    1.21006    1.07537   1.125   0.2605
dependencypronoun:environmentisland  1.04034    0.88038   1.182   0.2373

# compare models ...

anova(model1, model4)

npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
model4   28 1019.3 1176.3 -481.66   963.32
model1   48 1047.8 1316.8 -475.88   951.76 11.566 20     0.9302

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
  write_rds('models/spr_dozh_acc_md5.rds')
summary(model5)
toc()
beep()

# 19.94 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)               3.30133    0.32232  10.243  < 2e-16 ***
# dependency2               0.96351    0.27773   3.469 0.000522 ***
# environment2             -0.08774    0.42868  -0.205 0.837829    
# environment3             -0.42185    0.41807  -1.009 0.312952    
# dependency2:environment2  0.21721    0.46229   0.470 0.638457    
# dependency2:environment3  0.37280    0.46768   0.797 0.425382  

#------------------------------------------------------------------------------#
# + + model 6 (final?) ----
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

# 2.48 sec elapsed
# no warnings
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)               3.13304    0.29789  10.517  < 2e-16 ***
# dependency2               0.98950    0.17299   5.720 1.07e-08 ***
# environment2             -0.28222    0.21074  -1.339    0.181    
# environment3             -0.16226    0.21438  -0.757    0.449    
# dependency2:environment2  0.03125    0.41351   0.076    0.940    
# dependency2:environment3  0.34034    0.41902   0.812    0.417 

# compare models ...

anova(model6, model5)

# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model6    8 1195.0 1240.4 -589.51   1179.0                     
# model5   17 1199.1 1295.6 -582.56   1165.1 13.898  9      0.126

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

518.55 sec elapsed (8 min)
optimizer (bobyqa) convergence code: 0 (OK)
boundary (singular) fit: see help('isSingular')
Fixed effects:
  Estimate Std. Error z value Pr(>|z|)
(Intercept)                           2.8242     0.3615   7.812 5.63e-15 ***
dependencypronoun                     0.8093     0.6346   1.275    0.202
environmentlong                      -0.2813     0.4743  -0.593    0.553
environmentisland                    -0.9161     0.4414  -2.075    0.038 *
dependencypronoun:environmentlong     0.5468     0.9395   0.582    0.561
dependencypronoun:environmentisland   1.0220     0.9360   1.092    0.275

#------------------------------------------------------------------------------#
# + + model 2
#------------------------------------------------------------------------------#

# fit model ...

tic()
model2 <- glmer(accuracy ~ dependency * environment + 
                  (dependency * environment || participant) + 
                  (dependency * environment || item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_suen_acc_md2.rds')
summary(model2)
toc()
beep()

# skip for now

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

265.64 sec elapsed (4 min)
optimizer (bobyqa) convergence code: 0 (OK)
boundary (singular) fit: see help('isSingular')
Fixed effects:
  Estimate Std. Error z value Pr(>|z|)
(Intercept)                           2.7315     0.3246   8.415   <2e-16 ***
dependencypronoun                     0.9171     0.6135   1.495   0.1349
environmentlong                      -0.1659     0.4189  -0.396   0.6922
environmentisland                    -0.8856     0.3963  -2.234   0.0255 *
dependencypronoun:environmentlong     0.3010     0.8507   0.354   0.7235
dependencypronoun:environmentisland   0.9085     0.8296   1.095   0.2734

# compare models ...

anova (model1, model3)

       npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
model3   37 1076.2 1280.1 -501.12  1002.24
model1   48 1093.4 1357.9 -498.69   997.39 4.8506 11     0.9382

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

74.5 sec elapsed
optimizer (bobyqa) convergence code: 0 (OK)
boundary (singular) fit: see help('isSingular')
Fixed effects:
  Estimate Std. Error z value Pr(>|z|)
(Intercept)                          2.73790    0.32135   8.520   <2e-16 ***
dependencypronoun                    0.91598    0.50162   1.826   0.0678 .
environmentlong                     -0.06792    0.42060  -0.161   0.8717
environmentisland                   -0.95384    0.37264  -2.560   0.0105 *
dependencypronoun:environmentlong    0.07569    0.56525   0.134   0.8935
dependencypronoun:environmentisland  0.99261    0.56357   1.761   0.0782 .

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
# + + model 6 (final?) ----
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

optimizer (bobyqa) convergence code: 0 (OK)
boundary (singular) fit: see help('isSingular')
Fixed effects:
  Estimate Std. Error z value Pr(>|z|)
(Intercept)                          3.41970    0.52114   6.562 5.31e-11 ***
dependencypronoun                    1.90213    1.45392   1.308 0.190781
environmentlong                     -1.04233    0.61448  -1.696 0.089833 .
environmentisland                   -1.79053    0.53594  -3.341 0.000835 ***
dependencypronoun:environmentlong   -0.66729    1.66860  -0.400 0.689222
dependencypronoun:environmentisland -0.07859    1.54661  -0.051 0.959476

#------------------------------------------------------------------------------#
# + + model 2
#------------------------------------------------------------------------------#

tic()
model2 <- glmer(accuracy ~ dependency * environment + 
                  (dependency * environment || participant) + 
                  (dependency * environment || item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_suko_acc_md2.rds')
summary(model2)
toc()
beep()

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

88.19 sec elapsed
optimizer (bobyqa) convergence code: 0 (OK)
boundary (singular) fit: see help('isSingular')
Fixed effects:
  Estimate Std. Error z value Pr(>|z|)
(Intercept)                           3.1201     0.3884   8.034 9.47e-16 ***
dependencypronoun                     1.0071     0.5273   1.910 0.056122 .
environmentlong                      -0.9997     0.4339  -2.304 0.021216 *
environmentisland                    -1.4318     0.3997  -3.582 0.000341 ***
dependencypronoun:environmentlong     0.1397     0.5711   0.245 0.806767
dependencypronoun:environmentisland   0.3615     0.5373   0.673 0.501059

# compare models ...

anova(model1, model4)

npar    AIC    BIC  logLik deviance Chisq Df Pr(>Chisq)
model4   26 1181.9 1326.1 -564.97   1129.9
model1   48 1205.2 1471.4 -554.61   1109.2 20.71 22     0.5387

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

# 5.78 sec elapsed
# Model failed to converge with max|grad| = 0.169075 (tol = 0.002, component 1)
# Model is nearly unidentifiable: very large eigenvalue
# - Rescale variables?
  
#------------------------------------------------------------------------------#
# + + model 6 ----
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

# 1.25 sec elapsed
# no warnings
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                2.6151     0.2219  11.787  < 2e-16 ***
# dependency2                1.0341     0.1606   6.437 1.22e-10 ***
# environment2              -0.7558     0.2028  -3.727 0.000194 ***
# environment3              -0.8408     0.2006  -4.192 2.76e-05 ***
# dependency2:environment2   0.5713     0.4023   1.420 0.155519    
# dependency2:environment3   0.7688     0.4031   1.907 0.056509 .  

# compare models ...

anova(model6, model1)

# post-hoc tests ...

pairwise1 <- model6 %>%
  emmeans(~ dependency * environment) %>%
  contrast('pairwise', by = 'environment') %>%
  summary(by = NULL, adjust = 'holm')
pairwise1

# contrast      environment estimate    SE  df z.ratio p.value
# gap - pronoun short         -0.587 0.311 Inf  -1.887  0.0591
# gap - pronoun long          -1.159 0.259 Inf  -4.479  <.0001
# gap - pronoun island        -1.356 0.255 Inf  -5.325  <.0001
# Results are given on the log odds ratio (not the response) scale. 
# P value adjustment: holm method for 3 tests 

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

suzh (everybody)
boundary (singular) fit: see help('isSingular')
Fixed effects:
  Estimate Std. Error z value Pr(>|z|)
(Intercept)                           3.1708     0.4394   7.216 5.37e-13 ***
dependencypronoun                     1.1282     0.8210   1.374  0.16941
environmentlong                      -1.3016     0.4795  -2.714  0.00664 **
environmentisland                    -0.9211     0.5113  -1.801  0.07163 .
dependencypronoun:environmentlong     0.7879     1.0814   0.729  0.46628
dependencypronoun:environmentisland  -0.1671     1.0067  -0.166  0.86816

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

85.54 sec elapsed
optimizer (bobyqa) convergence code: 0 (OK)
boundary (singular) fit: see help('isSingular')
Fixed effects:
  Estimate Std. Error z value Pr(>|z|)
(Intercept)                           3.3426     0.4341   7.700 1.36e-14 ***
dependencypronoun                     0.5528     0.4417   1.251  0.21081
environmentlong                      -1.4831     0.4627  -3.205  0.00135 **
environmentisland                    -1.2545     0.4447  -2.821  0.00479 **
dependencypronoun:environmentlong     0.8905     0.5113   1.742  0.08156 .
dependencypronoun:environmentisland   0.3846     0.4852   0.793  0.42795

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

50.31 sec elapsed
optimizer (bobyqa) convergence code: 0 (OK)
boundary (singular) fit: see help('isSingular')
Fixed effects:
  Estimate Std. Error z value Pr(>|z|)
(Intercept)                           3.0360     0.3704   8.197 2.45e-16 ***
dependencypronoun                     0.3881     0.4957   0.783  0.43369
environmentlong                      -1.1391     0.4015  -2.837  0.00456 **
environmentisland                    -0.7959     0.4551  -1.749  0.08028 .
dependencypronoun:environmentlong     1.4062     0.7992   1.760  0.07847 .
dependencypronoun:environmentisland   0.6208     0.7241   0.857  0.39128

#------------------------------------------------------------------------------#
# + + model 5 ----
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
  write_rds('models/spr_suzh_acc_md6.rds')
summary(model6)
toc()
beep()

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

anova (model6, model5)

# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model6    8 1260.2 1305.2 -622.08   1244.2                     
# model5   17 1271.2 1367.0 -618.58   1237.2 6.9998  9     0.6371

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
# modeling - glmer - critical (2022-06-08) ----
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
# gap - pronoun short           5.01 0.911 Inf   5.500  <.0001
# gap - pronoun long            3.28 0.821 Inf   4.001  0.0001
# gap - pronoun island          1.16 0.673 Inf   1.725  0.0844
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

2625.09 sec elapsed (43 min)
optimizer (bobyqa) convergence code: 0 (OK)
boundary (singular) fit: see help('isSingular')
Fixed effects:
  Estimate Std. Error z value Pr(>|z|)
(Intercept)                           3.1582     0.5518   5.724 1.04e-08 ***
dependencypronoun                    -3.0914     0.7053  -4.383 1.17e-05 ***
environmentlong                      -1.1486     0.5835  -1.968   0.0490 *
environmentisland                    -1.3680     0.6389  -2.141   0.0323 *
dependencypronoun:environmentlong     1.1183     0.6164   1.814   0.0696 .
dependencypronoun:environmentisland   1.5071     0.6880   2.190   0.0285 *

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

249.57 sec elapsed
optimizer (bobyqa) convergence code: 0 (OK)
boundary (singular) fit: see help('isSingular')
Fixed effects:
  Estimate Std. Error z value Pr(>|z|)
(Intercept)                           3.0426     0.5113   5.951 2.67e-09 ***
dependencypronoun                    -2.9732     0.6732  -4.416 1.00e-05 ***
environmentlong                      -1.0593     0.5354  -1.979   0.0479 *
environmentisland                    -1.3660     0.5644  -2.420   0.0155 *
dependencypronoun:environmentlong     1.0256     0.5754   1.782   0.0747 .
dependencypronoun:environmentisland   1.5005     0.6237   2.406   0.0161 *

# compare models ...

anova(model2, model1)

       npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
model2   37 2047.9 2258.4 -986.94   1973.9
model1   48 2064.7 2337.7 -984.33   1968.7 5.2306 11     0.9195

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

60.1 sec elapsed
optimizer (bobyqa) convergence code: 0 (OK)
boundary (singular) fit: see help('isSingular')
Fixed effects:
  Estimate Std. Error z value Pr(>|z|)
(Intercept)                           3.0386     0.5101   5.957 2.57e-09 ***
dependencypronoun                    -2.9689     0.6720  -4.418 9.96e-06 ***
environmentlong                      -1.0752     0.5265  -2.042   0.0411 *
environmentisland                    -1.3959     0.5495  -2.540   0.0111 *
dependencypronoun:environmentlong     1.0339     0.5721   1.807   0.0708 .
dependencypronoun:environmentisland   1.5400     0.6187   2.489   0.0128 *

# compare models ...

anova(model3, model2)

npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
model3   28 2033.6 2192.9 -988.81   1977.6
model2   37 2047.9 2258.4 -986.94   1973.9 3.7429  9     0.9275

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

95.75 sec elapsed
optimizer (bobyqa) convergence code: 0 (OK)
boundary (singular) fit: see help('isSingular')
Fixed effects:
  Estimate Std. Error z value Pr(>|z|)
(Intercept)                           2.6811     0.3338   8.032 9.62e-16 ***
dependencypronoun                    -2.6248     0.4824  -5.441 5.28e-08 ***
environmentlong                      -0.6775     0.3412  -1.986 0.047074 *
environmentisland                    -1.1724     0.3278  -3.577 0.000347 ***
dependencypronoun:environmentlong     0.6468     0.3647   1.774 0.076141 .
dependencypronoun:environmentisland   1.3548     0.3482   3.891 9.99e-05 ***

# compare models ...

anova(model4, model3)

npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
model4   26 2041.0 2188.9 -994.49   1989.0
model3   28 2033.6 2192.9 -988.81   1977.6 11.348  2   0.003434 **

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
  write_rds('models/ajt_dozhen_acc_md5.rds')
summary(model5)
toc()
beep()

10.61 sec elapsed
optimizer (bobyqa) convergence code: 0 (OK)
boundary (singular) fit: see help('isSingular')
Fixed effects:
  Estimate Std. Error z value Pr(>|z|)
(Intercept)                           2.6426     0.3243   8.150 3.65e-16 ***
dependencypronoun                    -2.5847     0.4707  -5.491 3.99e-08 ***
environmentlong                      -0.6627     0.3243  -2.044 0.040996 *
environmentisland                    -1.1517     0.2999  -3.840 0.000123 ***
dependencypronoun:environmentlong     0.6225     0.3575   1.741 0.081626 .
dependencypronoun:environmentisland   1.3521     0.3418   3.956 7.61e-05 ***

# compare models ...

anova(model5, model4)

npar    AIC    BIC  logLik deviance Chisq Df Pr(>Chisq)
model5   17 2028.1 2124.8 -997.04   1994.1
model4   26 2041.0 2188.9 -994.49   1989.0 5.111  9     0.8245

anova(model3, model5)

npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
model5   17 2028.1 2124.8 -997.04   1994.1
model3   28 2033.6 2192.9 -988.81   1977.6 16.459 11     0.1249

# check assumptions ...

check_model(model5)
ggsave('plots/check_model.png', width=10, height=10, dpi=600)

# pairwise comparisons ...

model5 %>%
  emmeans(~ dependency * environment) %>%
  contrast('pairwise', by = 'environment') %>%
  summary(by = NULL, adjust = 'holm')

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

1.76 sec elapsed
optimizer (bobyqa) convergence code: 0 (OK)
boundary (singular) fit: see help('isSingular')
Fixed effects:
  Estimate Std. Error z value Pr(>|z|)
(Intercept)                           2.2266     0.2237   9.952  < 2e-16 ***
dependencypronoun                    -2.1412     0.2018 -10.613  < 2e-16 ***
environmentlong                      -0.5828     0.2091  -2.786  0.00533 **
environmentisland                    -0.9422     0.2043  -4.611 4.00e-06 ***
dependencypronoun:environmentlong     0.5891     0.2691   2.189  0.02858 *
dependencypronoun:environmentisland   1.0564     0.2654   3.980 6.89e-05 ***

# model comparison ...

anova(model6, model5)

npar    AIC    BIC   logLik deviance  Chisq Df Pr(>Chisq)
model6    8 2334.2 2379.7 -1159.08   2318.2
model5   17 2028.1 2124.8  -997.04   1994.1 324.08  9  < 2.2e-16 ***

anova(model6, model4)

npar    AIC    BIC   logLik deviance  Chisq Df Pr(>Chisq)
model6    8 2334.2 2379.7 -1159.08   2318.2
model4   26 2041.0 2188.9  -994.49   1989.0 329.19 18  < 2.2e-16 ***

#------------------------------------------------------------------------------#
# + + model 7 
#------------------------------------------------------------------------------#

# fit model ...

tic()
model7 <- glmer(acceptance ~ dependency * environment + 
                  (1 + dependency + environment || participant) + 
                  (1 | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl=list(maxfun = 1e6))) %>%
  write_rds('models/ajt_dozhen_acc_md7.rds')
summary(model7)
toc()
beep()

13.83 sec elapsed
optimizer (bobyqa) convergence code: 0 (OK)
boundary (singular) fit: see help('isSingular')
Fixed effects:
  Estimate Std. Error z value Pr(>|z|)
(Intercept)                           2.6130     0.3159   8.272  < 2e-16 ***
dependencypronoun                    -2.5573     0.4674  -5.472 4.46e-08 ***
environmentlong                      -0.6053     0.3169  -1.910  0.05616 .
environmentisland                    -1.1278     0.2932  -3.847  0.00012 ***
dependencypronoun:environmentlong     0.5748     0.3532   1.627  0.10365
dependencypronoun:environmentisland   1.3334     0.3364   3.964 7.38e-05 ***

anova(model7, model5)

        npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
model7   17 2029.0 2125.8 -997.52   1995.0
model5   17 2028.1 2124.8 -997.04   1994.1 0.9447  0

#------------------------------------------------------------------------------#
# + dokoko ----
#------------------------------------------------------------------------------#

# dokoko = ORC study (do) + KLE group (ko) + Korean AJT (ko)

# filter for analysis ...

md <- md %>% 
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

fails to converge

#------------------------------------------------------------------------------#
# + + model 2 
#------------------------------------------------------------------------------#

# fit model ...

tic()
model2 <- glmer(acceptance ~ dependency * environment + 
                  (1 + dependency * environment || participant) + 
                  (1 + dependency * environment || item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl=list(maxfun = 1e6))) %>%
  write_rds('models/ajt_dokoko_acc_md2.rds')
summary(model2)
toc()
beep()

fails to converge

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

optimizer (bobyqa) convergence code: 0 (OK)
boundary (singular) fit: see help('isSingular')
Fixed effects:
  Estimate Std. Error z value Pr(>|z|)
(Intercept)                           5.0245     0.6146   8.176 2.94e-16 ***
  dependencypronoun                    -7.3361     0.7319 -10.024  < 2e-16 ***
  environmentlong                      -4.0218     0.6541  -6.149 7.79e-10 ***
  environmentisland                    -4.5681     0.6433  -7.101 1.24e-12 ***
  dependencypronoun:environmentlong     4.9090     0.7377   6.654 2.84e-11 ***
  dependencypronoun:environmentisland   6.9606     0.7172   9.705  < 2e-16 ***

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

Model failed to converge with max|grad| = 0.0709582 (tol = 0.002, component 1)
Model is nearly unidentifiable: very large eigenvalue

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

optimizer (bobyqa) convergence code: 0 (OK)
boundary (singular) fit: see help('isSingular')
Fixed effects:
  Estimate Std. Error z value Pr(>|z|)
(Intercept)                            7.828      2.312   3.386 0.000709 ***
  dependencypronoun                    -10.869      2.407  -4.516 6.31e-06 ***
  environmentlong                       -6.681      2.343  -2.852 0.004347 **
  environmentisland                     -7.410      2.321  -3.193 0.001408 **
  dependencypronoun:environmentlong      8.431      2.423   3.479 0.000503 ***
  dependencypronoun:environmentisland   10.527      2.400   4.386 1.15e-05 ***

anova(model5, model3)

       npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
model3   26 1635.9 1781.7 -791.95   1583.9
model5   38 1643.0 1856.0 -783.50   1567.0 16.907 12     0.1531

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
  write_rds('models/ajt_doko_acc_md6.rds')
summary(model6)
toc()
beep()

no warnings
Fixed effects:
  Estimate Std. Error z value Pr(>|z|)
(Intercept)                           4.8973     0.5763   8.498  < 2e-16 ***
  dependencypronoun                    -7.1777     0.6847 -10.482  < 2e-16 ***
  environmentlong                      -3.9038     0.6071  -6.431 1.27e-10 ***
  environmentisland                    -4.4488     0.6042  -7.363 1.80e-13 ***
  dependencypronoun:environmentlong     4.8642     0.6993   6.956 3.51e-12 ***
  dependencypronoun:environmentisland   6.7890     0.6824   9.948  < 2e-16 ***

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

485.11 sec elapsed (8 min)
optimizer (bobyqa) convergence code: 0 (OK)
boundary (singular) fit: see help('isSingular')
Fixed effects:
  Estimate Std. Error z value Pr(>|z|)
(Intercept)                           0.9123     0.3970   2.298 0.021548 *
dependencypronoun                    -2.3960     0.4079  -5.875 4.24e-09 ***
environmentlong                      -0.3938     0.3776  -1.043 0.297026
environmentisland                    -2.1612     0.3792  -5.699 1.20e-08 ***
dependencypronoun:environmentlong     1.5354     0.4195   3.660 0.000252 ***
dependencypronoun:environmentisland   2.6886     0.4683   5.741 9.40e-09 ***

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

160.57 sec elapsed
optimizer (bobyqa) convergence code: 0 (OK)
boundary (singular) fit: see help('isSingular')
Fixed effects:
  Estimate Std. Error z value Pr(>|z|)
(Intercept)                           0.8742     0.3682   2.374   0.0176 *
dependencypronoun                    -2.3727     0.3887  -6.103 1.04e-09 ***
environmentlong                      -0.3600     0.3378  -1.066   0.2866
environmentisland                    -2.1006     0.3621  -5.801 6.61e-09 ***
dependencypronoun:environmentlong     1.5288     0.3906   3.915 9.06e-05 ***
dependencypronoun:environmentisland   2.6534     0.4568   5.809 6.28e-09 ***

#------------------------------------------------------------------------------#
# + + model 3 ----
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

65.69 sec elapsed
optimizer (bobyqa) convergence code: 0 (OK)
boundary (singular) fit: see help('isSingular')
Fixed effects:
  Estimate Std. Error z value Pr(>|z|)
(Intercept)                           0.8427     0.3499   2.408 0.016026 *
dependencypronoun                    -2.0930     0.2874  -7.283 3.26e-13 ***
environmentlong                      -0.3218     0.3216  -1.000 0.317107
environmentisland                    -2.0913     0.3280  -6.376 1.82e-10 ***
dependencypronoun:environmentlong     1.2474     0.3246   3.842 0.000122 ***
dependencypronoun:environmentisland   2.3292     0.3367   6.917 4.61e-12 ***

# compare models ...

anova(model2, model3)

npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
model3   26 2212.5 2361.2 -1080.3   2160.5
model2   37 2217.6 2429.1 -1071.8   2143.6 16.977 11     0.1086

# pairwise comparisons ...

model3 %>%
  emmeans(~ dependency * environment) %>%
  contrast('pairwise', by = 'environment') %>%
  summary(by = NULL, adjust = 'holm')

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

47.97 sec elapsed
optimizer (bobyqa) convergence code: 0 (OK)
boundary (singular) fit: see help('isSingular')
Fixed effects:
  Estimate Std. Error z value Pr(>|z|)
(Intercept)                           0.7610     0.3025   2.516 0.011877 *
dependencypronoun                    -2.1619     0.3529  -6.126 9.01e-10 ***
environmentlong                      -0.2504     0.2918  -0.858 0.390881
environmentisland                    -1.9807     0.3382  -5.856 4.74e-09 ***
dependencypronoun:environmentlong     1.2775     0.3650   3.500 0.000466 ***
dependencypronoun:environmentisland   2.4263     0.4300   5.642 1.68e-08 ***

# compare models ...

anova(model1, model4)

npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
model4   28 2218.5 2378.5 -1081.2   2162.5
model1   48 2235.2 2509.6 -1069.6   2139.2 23.295 20     0.2745

anova(model3, model4)

npar    AIC    BIC  logLik deviance Chisq Df Pr(>Chisq)
model3   26 2212.5 2361.2 -1080.3   2160.5
model4   28 2218.5 2378.5 -1081.2   2162.5     0  2          1

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

14.41 sec elapsed
no warnings
Fixed effects:
  Estimate Std. Error z value Pr(>|z|)
(Intercept)                           0.7564     0.2973   2.544 0.010957 *
dependencypronoun                    -1.9384     0.2687  -7.215 5.41e-13 ***
environmentlong                      -0.2510     0.2757  -0.910 0.362651
environmentisland                    -2.0060     0.3135  -6.399 1.57e-10 ***
dependencypronoun:environmentlong     1.0653     0.3120   3.415 0.000638 ***
dependencypronoun:environmentisland   2.1691     0.3225   6.726 1.75e-11 ***

# compare models ...

anova(model1, model5)

npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
model5   17 2212.6 2309.7 -1089.3   2178.6
model1   48 2235.2 2509.6 -1069.6   2139.2 39.356 31     0.1442

anova(model4, model5)

npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
model5   17 2212.6 2309.7 -1089.3   2178.6
model4   28 2218.5 2378.5 -1081.2   2162.5 16.061 11     0.1389

anova(model3, model5)

npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
model5   17 2212.6 2309.7 -1089.3   2178.6
model3   26 2212.5 2361.2 -1080.3   2160.5 18.008  9    0.03509 *

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

1.6 sec elapsed
no warnings
Fixed effects:
  Estimate Std. Error z value Pr(>|z|)
(Intercept)                           0.6746     0.2655   2.540   0.0111 *
dependencypronoun                    -1.8251     0.1961  -9.305  < 2e-16 ***
environmentlong                      -0.2614     0.1859  -1.406   0.1597
environmentisland                    -1.5593     0.1932  -8.070 7.04e-16 ***
dependencypronoun:environmentlong     1.1340     0.2662   4.260 2.05e-05 ***
dependencypronoun:environmentisland   1.9809     0.2732   7.252 4.12e-13 ***

# compare models ...

anova(model1, model6)

npar    AIC    BIC  logLik deviance Chisq Df Pr(>Chisq)
model6    8 2293.8 2339.5 -1138.9   2277.8
model1   48 2235.2 2509.6 -1069.6   2139.2 138.6 40  8.481e-13 ***

anova(model5, model6)

npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
model6    8 2293.8 2339.5 -1138.9   2277.8
model5   17 2212.6 2309.7 -1089.3   2178.6 99.243  9  < 2.2e-16 ***

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
# + + model 1 ----
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

6983.77 sec elapsed (116 min)
optimizer (bobyqa) convergence code: 0 (OK)
boundary (singular) fit: see help('isSingular')
Fixed effects:
  Estimate Std. Error z value Pr(>|z|)
(Intercept)                          17.616890   0.002214    7957   <2e-16 ***
dependencypronoun                   -24.162444   0.002335  -10350   <2e-16 ***
environmentlong                     -12.782311   0.002392   -5343   <2e-16 ***
environmentisland                   -34.776498   0.002335  -14896   <2e-16 ***
dependencypronoun:environmentlong    15.395926   0.002293    6715   <2e-16 ***
dependencypronoun:environmentisland  38.085710   0.002335   16314   <2e-16 ***

# pairwise comparisons ...

model1 %>%
  emmeans(~ dependency * environment) %>%
  contrast('pairwise', by = 'environment') %>%
  summary(by = NULL, adjust = 'holm')

#------------------------------------------------------------------------------#
# + + model 1b
#------------------------------------------------------------------------------#

# fit model ...

tic()
model1b <- glmer(acceptance ~ dependency * environment + 
                  (1 + dependency * environment || participant) + 
                  (1 + dependency * environment || item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl=list(maxfun = 1e6))) %>%
  write_rds('models/ajt_suenen_acc_md1b.rds')
summary(model1b)
toc()
beep()

# run later

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

175.41 sec elapsed
optimizer (bobyqa) convergence code: 0 (OK)
boundary (singular) fit: see help('isSingular')
Fixed effects:
  Estimate Std. Error z value Pr(>|z|)
(Intercept)                           6.8722     1.0619   6.471 9.72e-11 ***
dependencypronoun                   -11.4609     1.3806  -8.302  < 2e-16 ***
environmentlong                      -3.1995     0.9523  -3.360 0.000781 ***
environmentisland                   -12.2373     1.5048  -8.132 4.22e-16 ***
dependencypronoun:environmentlong     4.1445     1.4662   2.827 0.004701 **
dependencypronoun:environmentisland  13.8977     1.7049   8.151 3.59e-16 ***

# compare models ...

anova(model1, model3)

npar    AIC     BIC  logLik deviance  Chisq Df Pr(>Chisq)
model3   26 819.65  962.71 -383.83   767.65
model1   48 804.68 1068.79 -354.34   708.68 58.971 22  3.169e-05 ***

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

148.22 sec elapsed
Model failed to converge: degenerate  Hessian with 3 negative eigenvalues

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
  write_rds('models/ajt_suenen_acc_md5.rds')
summary(model5)
toc()
beep()

8.43 sec elapsed
optimizer (bobyqa) convergence code: 0 (OK)
boundary (singular) fit: see help('isSingular')
Fixed effects:
  Estimate Std. Error z value Pr(>|z|)
(Intercept)                          6.604e+00  7.311e-04    9032   <2e-16 ***
dependencypronoun                   -1.109e+01  7.311e-04  -15174   <2e-16 ***
environmentlong                     -3.019e+00  7.311e-04   -4129   <2e-16 ***
environmentisland                   -1.200e+01  7.312e-04  -16413   <2e-16 ***
dependencypronoun:environmentlong    4.271e+00  7.311e-04    5842   <2e-16 ***
dependencypronoun:environmentisland  1.345e+01  7.311e-04   18396   <2e-16 ***

# compare models ...

anova(model1, model5)

npar    AIC     BIC  logLik deviance  Chisq Df Pr(>Chisq)
model5   17 806.39  899.92 -386.19   772.39
model1   48 804.68 1068.79 -354.34   708.68 63.703 31  0.0004831 ***

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

1.83 sec elapsed
no warnings
Fixed effects:
  Estimate Std. Error z value Pr(>|z|)
(Intercept)                           6.0560     0.6974   8.684  < 2e-16 ***
dependencypronoun                   -10.2228     0.7826 -13.063  < 2e-16 ***
environmentlong                      -2.7761     0.6444  -4.308 1.65e-05 ***
environmentisland                    -9.9656     0.7682 -12.973  < 2e-16 ***
dependencypronoun:environmentlong     3.4680     0.7511   4.617 3.89e-06 ***
dependencypronoun:environmentisland  12.1821     0.8888  13.707  < 2e-16 ***

# compare models ...

anova(model1, model6)

npar    AIC     BIC  logLik deviance  Chisq Df Pr(>Chisq)
model6    8 819.54  863.55 -401.77   803.54
model1   48 804.68 1068.79 -354.34   708.68 94.855 40  2.372e-06 ***

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
# + + model 1b
#------------------------------------------------------------------------------#

# fit model ...

tic()
model1b <- glmer(acceptance ~ dependency * environment + 
                  (1 + dependency * environment || participant) + 
                  (1 + dependency * environment || item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl=list(maxfun = 1e6))) %>%
  write_rds('models/ajt_sukoen_acc_md1b.rds')
summary(model1b)
toc()
beep()

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
# modeling - glmer - critical ----
#------------------------------------------------------------------------------#

# prep data

md <- crit %>%
  mutate(environment = fct_relevel(environment, 'short', 'long', 'island'),
         dependency = as.factor(dependency)) %>%
  mutate(acceptance = case_when(response > 3.5 ~ TRUE,
                                response < 3.5 ~ FALSE)) %>%
  filter(study == '210510_do', 
         group == 'korean',
         task == 'korean_ajt')

check <- md %>% 
  group_by(study, group, participant) %>%
  summarise() %>%
  ungroup()

# view contrasts

contrasts(md$dependency)
contrasts(md$environment)

class(md$dependency)
class(md$environment)

# model 1 (maximal model)

tic()
model1 <- glmer(acceptance ~ dependency * environment + 
                (1 + dependency * environment | participant) + 
                (1 + dependency * environment | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa",
                                       optCtrl=list(maxfun = 1e6)))
summary(model1)
toc()
beep(1)

# doen-ejt (old)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                           4.4571     0.6719   6.634 3.27e-11 ***
# dependencypronoun                    -7.6782     0.8221  -9.340  < 2e-16 ***
# environmentlong                      -0.5082     0.9037  -0.562    0.574    
# environmentisland                    -4.8227     0.7766  -6.210 5.31e-10 ***
# environmentlong:dependencypronoun     0.3287     1.0535   0.312    0.755    
# environmentisland:dependencypronoun   5.2213     0.9500   5.496 3.88e-08 ***
saveRDS(model1, file = 'models/ajt_doen_acc_md1.rds') 
saveRDS(model1, file = 'models/ajt_doen_acc_maxfun_md1.rds') 

model1 <- readRDS('models/ajt_doen_acc_md1.rds') 
summary(model1)

# doko-ejt
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                           2.9764     0.6280   4.739 2.15e-06 ***
#   dependencypronoun                    -4.9261     0.9046  -5.446 5.16e-08 ***
#   environmentlong                      -1.3425     0.5440  -2.468   0.0136 *  
#   environmentisland                    -1.6203     0.6397  -2.533   0.0113 *  
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
#   dependencypronoun                    -3.0913     0.7052  -4.383 1.17e-05 ***
#   environmentlong                      -1.1486     0.5834  -1.969   0.0490 *  
#   environmentisland                    -1.3680     0.6388  -2.141   0.0322 *  
#   environmentlong:dependencypronoun     1.1183     0.6163   1.814   0.0696 .  
# environmentisland:dependencypronoun   1.5071     0.6879   2.191   0.0285 * 

# dozh-zjt
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                           0.9123     0.3970   2.298 0.021557 *  
#   dependencypronoun                    -2.3961     0.4080  -5.873 4.28e-09 ***
#   environmentlong                      -0.3938     0.3776  -1.043 0.297049    
# environmentisland                    -2.1612     0.3792  -5.699 1.21e-08 ***
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

# dozh-ejt (kjt???)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                           5.0245     0.6145   8.177 2.92e-16 ***
# dependencypronoun                    -7.3361     0.7318 -10.025  < 2e-16 ***
# environmentlong                      -4.0218     0.6540  -6.150 7.76e-10 ***
# environmentisland                    -4.5681     0.6432  -7.102 1.23e-12 ***
# environmentlong:dependencypronoun     4.9090     0.7376   6.655 2.83e-11 ***
# environmentisland:dependencypronoun   6.9606     0.7171   9.706  < 2e-16 ***

# dozh-zjt
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                           0.8427     0.3499   2.408 0.016025 *  
#   dependencypronoun                    -2.0930     0.2874  -7.283 3.26e-13 ***
#   environmentlong                      -0.3218     0.3216  -1.000 0.317101    
# environmentisland                    -2.0913     0.3280  -6.376 1.81e-10 ***
#   environmentlong:dependencypronoun     1.2474     0.3246   3.842 0.000122 ***
#   environmentisland:dependencypronoun   2.3292     0.3367   6.917 4.60e-12 ***

# post-hoc tests: pairwise comparisons of estimated marginal means

pairs(emmeans(model1, "dependency", by = "environment"))

model1 %>%
  emmeans(~ dependency * environment) %>%
  contrast('pairwise', by = 'environment') %>%
  summary(by = NULL, adjust = 'holm')
beep(1)

# doen-ejt
# environment = short:
#   contrast      estimate    SE  df z.ratio p.value
# gap - pronoun     7.68 0.822 Inf   9.340  <.0001 ***
# environment = long:
#   contrast      estimate    SE  df z.ratio p.value
# gap - pronoun     7.35 0.804 Inf   9.136  <.0001 ***
# environment = island:
#   contrast      estimate    SE  df z.ratio p.value
# gap - pronoun     2.46 0.609 Inf   4.034  0.0001 ***
# Results are given on the log odds ratio (not the response) scale.
# --------------------------------------------------------------------- #
# doen-ejt (new on 220608)
# contrast      environment estimate         SE  df   z.ratio p.value
# gap - pronoun short         7.5144 0.00068940 Inf 10900.016  <.0001
# gap - pronoun long          7.2759 0.00097497 Inf  7462.756  <.0001
# gap - pronoun island        2.3419 0.00097495 Inf  2402.098  <.0001
# Results are given on the log odds ratio (not the response) scale. 
# P value adjustment: holm method for 3 tests 

# doko-ejt
# environment = short:
#   contrast      estimate    SE  df z.ratio p.value
# gap - pronoun     4.93 0.905 Inf   5.446  <.0001 ***
# environment = long:
#   contrast      estimate    SE  df z.ratio p.value
# gap - pronoun     3.27 0.817 Inf   3.999  0.0001 ***
# environment = island:
#   contrast      estimate    SE  df z.ratio p.value
# gap - pronoun     1.20 0.704 Inf   1.711  0.0871 .
# Results are given on the log odds ratio (not the response) scale. 

# doko-kjt
# (see below)

# dozh-ejt
# environment = short:
#   contrast      estimate    SE  df z.ratio p.value
# gap - pronoun     3.09 0.705 Inf   4.383  <.0001 ***
# environment = long:
#   contrast      estimate    SE  df z.ratio p.value
# gap - pronoun     1.97 0.473 Inf   4.170  <.0001 ***
# environment = island:
#   contrast      estimate    SE  df z.ratio p.value
# gap - pronoun     1.58 0.505 Inf   3.139  0.0017 **
# Results are given on the log odds ratio (not the response) scale. 

# dozh-zjt
# environment = short:
#   contrast      estimate    SE  df z.ratio p.value
# gap - pronoun    2.396 0.408 Inf   5.873  <.0001 ***
# environment = long:
#   contrast      estimate    SE  df z.ratio p.value
# gap - pronoun    0.861 0.289 Inf   2.975  0.0029 **
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
# modeling - old ----
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