#==============================================================================#
# ::::: header :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: ----
#==============================================================================#

# title: analysis for study no. 210510
# author: fred zenker
# created: 2020-05-11
# updated: 2022-06-10

#==============================================================================#
# ::::: packages :::::::::::::::::::::::::::::::::::::::::::::::::::::::::: ----
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
# ::::: data :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: ----
#==============================================================================#

# load data ...

df <- read_csv('data/data.csv', col_types = cols(.default = 'c')) %>%
  select(-audio_data)

ct <- read_csv('data/ctest_scored.csv', col_types = cols(.default = 'f', accuracy = 'l'))

proficiency <- ct %>%
  group_by(study, group, participant) %>%
  summarise(proficiency = sum(accuracy, na.rm=T)) %>%
  ungroup()

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
# ::::: exit survey ::::::::::::::::::::::::::::::::::::::::::::::::::::::: ----
#==============================================================================#

# prep dataframe ...

exit <- df %>%
  filter(task == 'exit_survey') %>%
  arrange(study, group, participant) %>%
  select_if(function(x){!all(is.na(x))})

#==============================================================================#
# ::::: language survey ::::::::::::::::::::::::::::::::::::::::::::::::::: ----
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
# ::::: c-test :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: ----
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
  geom_violin(col = NA),
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
ggsave("plots/orc/ctest.png", width=6.5, height=2, dpi=600)

p2 + s
ggsave("plots/src/ctest.png", width=6.5, height=2, dpi=600)

#------------------------------------------------------------------------------#
# dispersion ----
#------------------------------------------------------------------------------#

check <- ct %>%
  group_by(study, group, participant) %>%
  summarise(accuracy = mean(accuracy)) %>%
  ungroup() %>%
  group_by(study, group) %>%
  summarise(mean = mean(accuracy),
            sd = sd(accuracy),
            iqr = IQR(accuracy)) %>%
  ungroup()

#------------------------------------------------------------------------------#
# modeling ----
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# + orc w/ ref = ens ----
#------------------------------------------------------------------------------#

# filter for analysis ...

md <- ct %>%
  filter(study == '210510_do')

# set contrasts for deviation coding ...

contrasts(md$group) <- contr.treatment(3) - matrix(rep(1/3, 6), ncol=2)

# check contrasts ...

contrasts(md$group)

# fit model ...

tic()
model1 <- glmer(accuracy ~ group + (1 | participant) + (1 + group | item), 
                data = md, family = binomial, control = glmerControl(optimizer = "bobyqa")) %>%
  write_rds('models/ctest_doall_glmer_refens_md1.rds')
summary(model1)
toc()
beep()

# 34.72 sec elapsed
# no warnings
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)   1.3629     0.2464   5.531 3.18e-08 ***
# group2       -2.4521     0.2881  -8.511  < 2e-16 ***
# group3       -2.5051     0.2943  -8.513  < 2e-16 ***



#------------------------------------------------------------------------------#
# + orc w/ ref = kle ----
#------------------------------------------------------------------------------#

# relevel factor ...

md <- md %>%
  mutate(group = fct_relevel(group, 'korean', 'mandarin', 'english'))

# fit model ...

tic()
model1 <- glmer(accuracy ~ group + (1 | participant) + (1 + group | item), 
                data = md, family = binomial, control = glmerControl(optimizer = "bobyqa")) %>%
  write_rds('models/ctest_doall_glmer_refkle_md1.rds')
summary(model1)
toc()
beep()

# reference level = korean 
# 23.28 sec elapsed
# no warnings
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  1.36277    0.24643   5.530  3.2e-08 ***
# group2      -0.05295    0.18685  -0.283    0.777    
# group3       2.45182    0.28810   8.510  < 2e-16 ***

#------------------------------------------------------------------------------#
# + src w/ ref = ens ----
#------------------------------------------------------------------------------#

# filter for analysis ...

md <- ct %>%
  filter(study == '210510_su')

# set contrasts for deviation coding ...

contrasts(md$group) <- contr.treatment(3) - matrix(rep(1/3, 6), ncol=2)

# check contrasts ...

contrasts(md$group)

# fit model ...

tic()
model1 <- glmer(accuracy ~ group + (1 | participant) + (1 + group | item), 
                data = md, family = binomial, control = glmerControl(optimizer = "bobyqa")) %>%
  write_rds('models/ctest_suall_glmer_refens_md1.rds')
summary(model1)
toc()
beep()

# 25.79 sec elapsed
# no warnings
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)   1.5141     0.2463   6.147 7.91e-10 ***
# group2       -2.1268     0.3072  -6.924 4.39e-12 ***
# group3       -2.5014     0.3253  -7.689 1.49e-14 ***

#------------------------------------------------------------------------------#
# + src w/ ref = kle ----
#------------------------------------------------------------------------------#

# relevel factor ...

md <- md %>%
  mutate(group = fct_relevel(group, 'korean', 'mandarin', 'english'))

# fit model ...

tic()
model1 <- glmer(accuracy ~ group + (1 | participant) + (1 + group | item), 
                data = md, family = binomial, control = glmerControl(optimizer = "bobyqa")) %>%
  write_rds('models/ctest_suall_glmer_refkle_md1.rds')
summary(model1)
toc()
beep()

#==============================================================================#
# ::::: elicited production task (ept) :::::::::::::::::::::::::::::::::::: ----
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

check <- temp %>%
  group_by(study) %>%
  summarise(irr = mean(agree, na.rm = TRUE)) %>%
  ungroup()

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
# bar plot critical w/ nontarget ----
#------------------------------------------------------------------------------#

# list participants excluding those who only gave nontarget responses ...

temp1 <- ep %>%
  filter(type != 'nontarget') %>%
  group_by(study, group, participant) %>%
  summarise() %>%
  ungroup()

# remove participants who only gave nontarget responses ...

temp2 <- ep %>%
  filter(participant %in% temp1$participant)

# check rate of passive responses ...

check <- temp2 %>%
  mutate(passive = case_when(str_detect(subtype, 'passive') == TRUE ~ TRUE, TRUE ~ FALSE)) %>%
  group_by(group, environment) %>%
  summarise(passive = mean(passive, na.rm = TRUE) * 100)
  
# summarise for plotting ...
  
plot <- temp %>%
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
  geom_bar(stat = "identity", col = NA, width = .5, alpha=.8),
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
# plot: 'pronoun' resumption vs. 'other' resumption ----
#------------------------------------------------------------------------------#

# check participants ...

check <- ep %>%
  group_by(study, group, participant) %>%
  summarise() %>%
  ungroup() %>% 
  group_by(study, group) %>%
  summarise(n = n()) %>%
  ungroup()

# remove participants who only gave nontarget responses ...

temp <- ep %>%
  filter(type != 'nontarget') %>%
  group_by(study, group, participant) %>%
  summarise() %>%
  ungroup()

ep2 <- ep %>%
  filter(participant %in% temp$participant)

# check participants ...

check <- ep2 %>%
  group_by(study, group, participant) %>%
  summarise() %>%
  ungroup() %>% 
  group_by(study, group) %>%
  summarise(n = n()) %>%
  ungroup()

# recode responses ...

ep3 <- ep2 %>%
  mutate(pronoun = case_when(str_detect(subtype, 'pronoun') == TRUE ~ TRUE,
                             TRUE ~ FALSE),
         fullNP = case_when(str_detect(subtype, 'fullNP') == TRUE ~ TRUE,
                            TRUE ~ FALSE)) %>%
  pivot_longer(cols = c('pronoun', 'fullNP'), names_to = 'dependency', values_to = 'response2') %>%
  filter(group %in% c('english', 'korean', 'mandarin')) %>%
  mutate(group = fct_drop(group)) %>%
  mutate(environment = fct_relevel(environment, 'short', 'long', 'island'))

# check counts per variable

check <- ep3 %>%
  filter(environment %in% c('short', 'long', 'island')) %>%
  group_by(study, group, environment, dependency) %>%
  summarise(count = sum(response2)) %>%
  ungroup()

check <- check %>%
  filter(study == '210510_do')

# summarise to create count data ...

ep4 <- ep3 %>%
  filter(environment %in% c('short', 'long', 'island')) %>%
  group_by(study, group, participant, environment, dependency) %>%
  summarise(count = sum(response2))

# plot total counts ...

plot <- ep4 %>%
  group_by(study, group, environment, dependency) %>%
  summarise(count = sum(count)) %>%
  ungroup() 

p1 <- ggplot(data = filter(plot, study == '210510_do'))
p2 <- ggplot(data = filter(plot, study == '210510_su'))

s <- list(
  aes(x = environment, y = count, group = dependency, col = dependency, shape = dependency, label = count),
  geom_line(lwd = 1, position = position_dodge(width = .4)),
  geom_point(size = 3, position = position_dodge(width = .4)),
  geom_text(size = 2.5, col = "black", hjust = .5, vjust = -1, position = position_dodge(width = .4)),
  theme_classic(),
  scale_y_continuous(name='total count'),
  scale_colour_manual(name='subtype', 
                      values=c('#ffb000', 'darkorange'),
                      breaks = c('pronoun', 'fullNP'),
                      labels = c('pronoun', 'full NP')),
  scale_shape_manual(name='subtype', 
                     values=c(15, 17),
                     breaks = c('pronoun', 'fullNP'),
                     labels = c('pronoun', 'full NP')),
  theme(text = element_text(size = 12),
        legend.position = 'right',
        legend.margin = margin(t = -.4, unit = 'cm'),
        plot.title = element_text(size = 12, hjust = .5)),
  facet_wrap(~group, labeller = as_labeller(c(`english` = 'ENS', `korean` = 'KLE', `mandarin` = 'MLE')))
)

plot <- plot %>%
  complete(study, nesting(group, environment, dependency), 
           fill = list(mean = 0, ci = 0))

p1 + s
ggsave("plots/orc/ept_resumption_type_total.png", width=6.5, height=2, dpi=600)

p2 + s
ggsave("plots/src/ept_resumption_type_total.png", width=6.5, height=2, dpi=600)

# plot mean counts ...

plot <- ep4 %>%
  group_by(study, group, environment, dependency) %>%
  summarise(mean = mean(count, na.rm=T),
            sd = sd(count, na.rm=T),
            n = n()) %>%
  mutate(se = sd / sqrt(n),
         ci = qt(1 - (0.05 / 2), n - 1) * se) %>%
  ungroup() 

plot <- plot %>%
  complete(study, nesting(group, environment, dependency), 
           fill = list(mean = 0, ci = 0))

p1 <- ggplot(data = filter(plot, study == '210510_do'))
p2 <- ggplot(data = filter(plot, study == '210510_su'))

s <- list(
  aes(x = environment, y = mean, group = dependency, col = dependency, shape = dependency),
  geom_line(lwd = 1, position = position_dodge(width = .4)),
  geom_point(size = 3, position = position_dodge(width = .4)),
  geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci), 
                width = 0, lwd = 5, linetype = 1, alpha = .5,
                position = position_dodge(width = .4)),
  theme_classic(),
  scale_y_continuous(name="mean count"),
  scale_colour_manual(name='subtype', 
                      values=c('#ffb000', 'darkorange'),
                      breaks = c('pronoun', 'fullNP'),
                      labels = c('pronoun', 'full NP')),
  scale_shape_manual(name='subtype', 
                     values=c(15, 17),
                     breaks = c('pronoun', 'fullNP'),
                     labels = c('pronoun', 'full NP')),
  theme(text = element_text(size = 12),
        legend.position = 'right',
        legend.margin = margin(t = -.4, unit = 'cm'),
        plot.title = element_text(size = 12, hjust = .5)),
  facet_wrap(~group, labeller = as_labeller(c(`english` = 'ENS', `korean` = 'KLE', `mandarin` = 'MLE')))
)

p1 + s
ggsave("plots/orc/ept_resumption_type_count.png", width=6.5, height=2, dpi=600)

p2 + s
ggsave("plots/src/ept_resumption_type_count.png", width=6.5, height=2, dpi=600)

#------------------------------------------------------------------------------#
# plot: type == 'gap'/'resumption' vs. subtype == 'gap'/'resumption' ----
#------------------------------------------------------------------------------#

# check participants ...

check <- ep %>%
  group_by(study, group, participant) %>%
  summarise() %>%
  ungroup() %>% 
  group_by(study, group) %>%
  summarise(n = n()) %>%
  ungroup()

# remove participants who only gave nontarget responses ...

temp <- ep %>%
  filter(type != 'nontarget') %>%
  group_by(study, group, participant) %>%
  summarise() %>%
  ungroup()

ep2 <- ep %>%
  filter(participant %in% temp$participant)

# check participants ...

check <- ep2 %>%
  group_by(study, group, participant) %>%
  summarise() %>%
  ungroup() %>% 
  group_by(study, group) %>%
  summarise(n = n()) %>%
  ungroup()

# recode responses ...

ep3 <- ep2 %>%
  mutate(resumption = case_when(type == 'resumption' ~ TRUE,
                                type == 'other' & str_detect(subtype, 'resumption') == TRUE ~ TRUE,
                                TRUE ~ FALSE),
         gap = case_when(type == 'gap' ~ TRUE,
                        type == 'other' & str_detect(subtype, 'resumption') == FALSE ~ TRUE,
                         TRUE ~ FALSE)) %>%
  pivot_longer(cols = c('resumption', 'gap'), names_to = 'dependency', values_to = 'response2') %>%
  filter(group %in% c('english', 'korean', 'mandarin')) %>%
  mutate(group = fct_drop(group)) %>%
  mutate(environment = fct_relevel(environment, 'short', 'long', 'island')) %>%
  mutate(set = case_when(type %in% c('gap', 'resumption') ~ 'type',
                         type == 'other' ~ 'subtype')) %>%
  mutate(set = fct_relevel(set, 'type', 'subtype')) %>%
  filter(set %in% c('type', 'subtype'))

# check counts per variable

check <- ep3 %>%
  filter(environment %in% c('short', 'long', 'island')) %>%
  group_by(study, group, environment, dependency) %>%
  summarise(count = sum(response2)) %>%
  ungroup()

check <- check %>%
  filter(study == '210510_do')

# summarise to create count data ...

ep4 <- ep3 %>%
  filter(environment %in% c('short', 'long', 'island')) %>%
  group_by(study, group, participant, environment, dependency, set) %>%
  summarise(count = sum(response2))

# plot total counts ...

plot <- ep4 %>%
  group_by(study, group, environment, dependency, set) %>%
  summarise(count = sum(count)) %>%
  ungroup() 

p1 <- ggplot(data = filter(plot, study == '210510_do'))
p2 <- ggplot(data = filter(plot, study == '210510_su'))

s <- list(
  aes(x = environment, y = count, group = dependency, col = dependency, shape = dependency, label = count),
  geom_line(lwd = 1, position = position_dodge(width = .4)),
  geom_point(size = 3, position = position_dodge(width = .4)),
  geom_text(size = 2.5, col = "black", hjust = .5, vjust = -1, position = position_dodge(width = .4)),
  theme_classic(),
  scale_y_continuous(name='total count'),
  scale_colour_manual(name='dependency', 
                      values=c('#648fff', '#ffb000'), 
                      labels=c('gap', 'resumption')),
  scale_shape_manual(name='dependency', 
                     values=c(16, 15), 
                     labels=c('gap', 'resumption')),
  theme(text = element_text(size = 12),
        legend.position = 'right',
        legend.margin = margin(t = -.4, unit = 'cm'),
        plot.title = element_text(size = 12, hjust = .5)),
  facet_grid2(vars(set), vars(group), axes = 'all', remove_labels = FALSE,
              labeller = as_labeller(c(`type` = 'main', `subtype` = 'other', `english` = 'ENS', `korean` = 'KLE', `mandarin` = 'MLE')))
)

p1 + s
#ggsave("plots/orc/ept_total.png", width=6.5, height=2, dpi=600)

p2 + s
#ggsave("plots/src/ept_total.png", width=6.5, height=2, dpi=600)

# plot mean counts ...

plot <- ep4 %>%
  group_by(study, group, environment, dependency, set) %>%
  summarise(mean = mean(count, na.rm=T),
            sd = sd(count, na.rm=T),
            n = n()) %>%
  mutate(se = sd / sqrt(n),
         ci = qt(1 - (0.05 / 2), n - 1) * se) %>%
  ungroup() 

p1 <- ggplot(data = filter(plot, study == '210510_do'))
p2 <- ggplot(data = filter(plot, study == '210510_su'))

s <- list(
  aes(x = environment, y = mean, group = dependency, col = dependency, shape = dependency),
  geom_line(lwd = 1, position = position_dodge(width = .4)),
  geom_point(size = 3, position = position_dodge(width = .4)),
  geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci), 
                width = 0, lwd = 5, linetype = 1, alpha = .5,
                position = position_dodge(width = .4)),
  theme_classic(),
  scale_y_continuous(name="count", breaks = c(0, 1, 2, 3, 4, 5)),
  scale_colour_manual(name='dependency', 
                      values=c('#648fff', '#ffb000'), 
                      labels=c('gap', 'resumption')),
  scale_shape_manual(name='dependency', 
                     values=c(16, 15), 
                     labels=c('gap', 'resumption')),
  theme(text = element_text(size = 12),
        legend.position = 'right',
        legend.margin = margin(t = -.4, unit = 'cm'),
        plot.title = element_text(size = 12, hjust = .5)),
  facet_grid2(vars(set), vars(group), axes = 'all', remove_labels = FALSE,
              labeller = as_labeller(c(`type` = 'main', `subtype` = 'other', `english` = 'ENS', `korean` = 'KLE', `mandarin` = 'MLE')))
)

p1 + s
# ggsave("plots/orc/ept_count.png", width=6.5, height=2, dpi=600)

p2 + s
# ggsave("plots/src/ept_count.png", width=6.5, height=2, dpi=600)

#------------------------------------------------------------------------------#
# plot: w/ 'other' gaps & 'other' resumption ----
#------------------------------------------------------------------------------#

# check participants ...

check <- ep %>%
  group_by(study, group, participant) %>%
  summarise() %>%
  ungroup() %>% 
  group_by(study, group) %>%
  summarise(n = n()) %>%
  ungroup()

# remove participants who only gave nontarget responses ...

temp <- ep %>%
  filter(type != 'nontarget') %>%
  group_by(study, group, participant) %>%
  summarise() %>%
  ungroup()

ep2 <- ep %>%
  filter(participant %in% temp$participant)

# check participants ...

check <- ep2 %>%
  group_by(study, group, participant) %>%
  summarise() %>%
  ungroup() %>% 
  group_by(study, group) %>%
  summarise(n = n()) %>%
  ungroup()

# recode responses ...

ep3 <- ep2 %>%
  mutate(resumption = case_when(type == 'resumption' ~ TRUE,
                                type == 'other' & str_detect(subtype, 'resumption') == TRUE ~ TRUE,
                                TRUE ~ FALSE),
         gap = case_when(type == 'gap' ~ TRUE,
                         type == 'other' & str_detect(subtype, 'resumption') == FALSE ~ TRUE,
                         TRUE ~ FALSE)) %>%
  pivot_longer(cols = c('resumption', 'gap'), names_to = 'dependency', values_to = 'response2') %>%
  filter(group %in% c('english', 'korean', 'mandarin')) %>%
  mutate(group = fct_drop(group)) %>%
  mutate(environment = fct_relevel(environment, 'short', 'long', 'island'))

# check counts per variable

check <- ep3 %>%
  filter(environment %in% c('short', 'long', 'island')) %>%
  group_by(study, group, environment, dependency) %>%
  summarise(count = sum(response2)) %>%
  ungroup()

check <- check %>%
  filter(study == '210510_do')

# summarise to create count data ...

ep4 <- ep3 %>%
  filter(environment %in% c('short', 'long', 'island')) %>%
  group_by(study, group, participant, environment, dependency) %>%
  summarise(count = sum(response2))

# plot total counts ...

plot <- ep4 %>%
  group_by(study, group, environment, dependency) %>%
  summarise(count = sum(count)) %>%
  ungroup() 

p1 <- ggplot(data = filter(plot, study == '210510_do'))
p2 <- ggplot(data = filter(plot, study == '210510_su'))

s <- list(
  aes(x = environment, y = count, group = dependency, col = dependency, shape = dependency, label = count),
  geom_line(lwd = 1, position = position_dodge(width = .4)),
  geom_point(size = 3, position = position_dodge(width = .4)),
  geom_text(size = 2.5, col = "black", hjust = .5, vjust = -1, position = position_dodge(width = .4)),
  theme_classic(),
  scale_y_continuous(name='total count'),
  scale_colour_manual(name='dependency', 
                      values=c('#648fff', '#ffb000'), 
                      labels=c('gap', 'resumption')),
  scale_shape_manual(name='dependency', 
                     values=c(16, 15), 
                     labels=c('gap', 'resumption')),
  theme(text = element_text(size = 12),
        legend.position = 'right',
        legend.margin = margin(t = -.4, unit = 'cm'),
        plot.title = element_text(size = 12, hjust = .5)),
  facet_wrap(~group, labeller = as_labeller(c(`english` = 'ENS', `korean` = 'KLE', `mandarin` = 'MLE')))
)

p1 + s
#ggsave("plots/orc/ept_total.png", width=6.5, height=2, dpi=600)

p2 + s
#ggsave("plots/src/ept_total.png", width=6.5, height=2, dpi=600)

# plot mean counts ...

plot <- ep4 %>%
  group_by(study, group, environment, dependency) %>%
  summarise(mean = mean(count, na.rm=T),
            sd = sd(count, na.rm=T),
            n = n()) %>%
  mutate(se = sd / sqrt(n),
         ci = qt(1 - (0.05 / 2), n - 1) * se) %>%
  ungroup() 

p1 <- ggplot(data = filter(plot, study == '210510_do'))
p2 <- ggplot(data = filter(plot, study == '210510_su'))

s <- list(
  aes(x = environment, y = mean, group = dependency, col = dependency, shape = dependency),
  geom_line(lwd = 1, position = position_dodge(width = .4)),
  geom_point(size = 3, position = position_dodge(width = .4)),
  geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci), 
                width = 0, lwd = 5, linetype = 1, alpha = .5,
                position = position_dodge(width = .4)),
  theme_classic(),
  scale_y_continuous(name="count", limits = c(-.05, 5), breaks = c(0, 1, 2, 3, 4, 5)),
  scale_colour_manual(name='dependency', 
                      values=c('#648fff', '#ffb000'), 
                      labels=c('gap', 'resumption')),
  scale_shape_manual(name='dependency', 
                     values=c(16, 15), 
                     labels=c('gap', 'resumption')),
  theme(text = element_text(size = 12),
        legend.position = 'right',
        legend.margin = margin(t = -.4, unit = 'cm'),
        plot.title = element_text(size = 12, hjust = .5)),
  facet_wrap(~group, labeller = as_labeller(c(`english` = 'ENS', `korean` = 'KLE', `mandarin` = 'MLE')))
)

p1 + s
# ggsave("plots/orc/ept_count.png", width=6.5, height=2, dpi=600)

p2 + s
# ggsave("plots/src/ept_count.png", width=6.5, height=2, dpi=600)

#------------------------------------------------------------------------------#
# proficiency effects ----
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# + scatter plot ~ overall ----
#------------------------------------------------------------------------------#

# list participants excluding those who only gave nontarget responses ...

temp <- ep %>%
  filter(type != 'nontarget') %>%
  group_by(study, group, participant) %>%
  summarise() %>%
  ungroup()

# summarise for plotting ...

plot <- ep %>%
  filter(participant %in% temp$participant) %>%
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

p1 <- ggplot(data=filter(plot, study == '210510_do', group %in% c('korean', 'mandarin')), aes(x=scale(proficiency, scale = FALSE), y=prop))
p2 <- ggplot(data=filter(plot, study == '210510_su', group %in% c('korean', 'mandarin')), aes(x=scale(proficiency, scale = FALSE), y=prop))

# generate plot ...

s <- list(
  geom_hline(yintercept = 50),
  geom_vline(xintercept = 0),
  geom_smooth(method=lm, col='#ffb000', fill = alpha('#ffb000', .5)), 
  geom_point(shape = 16, col = alpha('#ffb000', .5), size = 2),
  theme_classic(),
  scale_x_continuous(name='mean-centered proficiency'),
  scale_y_continuous(name="resumption rate", limits = c(-5, 100)),
  theme(text = element_text(size = 12),
        plot.title = element_text(size = 12, hjust = .5), 
        legend.position = "right"),
  facet_wrap(~group, labeller = as_labeller(groups))
)

# print and save ...

p1 + s
ggsave("plots/orc/ept_proficiency.png", width=6.5, height=2, dpi=600)

p2 + s
ggsave("plots/src/ept_proficiency.png", width=6.5, height=2, dpi=600)

#------------------------------------------------------------------------------#
# + scatter plot ~ by environment ----
#------------------------------------------------------------------------------#

# exclude participants who gave only nontarget responses ...

temp <- ep %>%
  filter(type != 'nontarget') %>%
  group_by(study, group, participant) %>%
  summarise() %>%
  ungroup()

check <- ep %>%
  group_by(study, group, participant) %>%
  summarise() %>%
  ungroup() %>%
  filter(!participant %in% temp$participant)

ep2 <- ep %>%
  filter(participant %in% temp$participant)

ep2 <- ep2 %>%
  left_join(proficiency, by = c('study', 'group', 'participant'))

check <- ep2 %>%
  group_by(study, group, participant, proficiency) %>%
  summarise() %>%
  ungroup() %>%
  filter(is.na(proficiency) == TRUE)

ep_clean <- ep2 %>%
  filter(is.na(proficiency) == FALSE)

ep_clean <- ep_clean %>%
  mutate(resumption = case_when(type == 'resumption' ~ TRUE, TRUE ~ FALSE))

ep_clean <- ep_clean %>%
  mutate(environment = factor(environment)) %>%
  filter(environment %in% c('short', 'long', 'island')) %>%
  mutate(environment = fct_relevel(environment, 'short', 'long', 'island'))

plot <- ep_clean %>%
  group_by(study, group, environment, proficiency, participant) %>%
  summarise(resumption = mean(resumption)) %>%
  ungroup()

summary(plot)

# summarise for plotting ...

plot <- plot %>%
  filter(participant %in% temp$participant) %>%
  filter(condition %in% c('cond1', 'cond2', 'cond3')) %>%
  mutate(type = case_when(type == 'gap' ~ 'gap',
                          type == 'resumption' ~ 'resumption',
                          TRUE ~ 'other')) %>%
  mutate(type = as.character(type)) %>%
  group_by(study, group, condition, participant) %>%
  count(type) %>%
  ungroup() %>%
  group_by(study, group, condition, participant) %>%
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

groups <- c(`english` = 'ENS', `korean` = 'KLE', `mandarin` = 'MLE',
            `short` = 'short', `long` = 'long', `island` = 'island')

# define data for plots ...

p1 <- ggplot(data=filter(plot, study == '210510_do', group %in% c('korean', 'mandarin')), aes(x=scale(proficiency, scale = FALSE), y=resumption*100))
p2 <- ggplot(data=filter(plot, study == '210510_su', group %in% c('korean', 'mandarin')), aes(x=scale(proficiency, scale = FALSE), y=resumption*100))

# generate plot ...

s <- list(
  geom_hline(yintercept = 50),
  geom_vline(xintercept = 0),
  geom_smooth(method=lm, col='#ffb000', fill = alpha('#ffb000', .5)), 
  geom_point(shape = 16, col = alpha('#ffb000', .5), size = 2),
  theme_classic(),
  scale_x_continuous(name='mean-centered proficiency score'),
  scale_y_continuous(name='resumption rate'),
  theme(text = element_text(size = 12),
        plot.title = element_text(size = 12, hjust = .5), 
        legend.position = "right"),
  facet_grid2(vars(group), vars(environment), labeller = as_labeller(groups),
              axes = 'all', remove_labels = 'y')
)

# print and save ...

p1 + s
ggsave("plots/orc/ept_proficiency.png", width=6.5, height=2, dpi=600)

p2 + s
ggsave("plots/src/ept_proficiency.png", width=6.5, height=2, dpi=600)

#------------------------------------------------------------------------------#
# + modeling: glmer ----
#------------------------------------------------------------------------------#

# adjust column classes ...

md <- ep_clean %>%
  filter(group %in% c('korean', 'mandarin')) %>%
  mutate_at(c('study', 'group', 'environment', 'participant', 'item'), as.factor) %>%
  mutate(resumption = as.logical(resumption)) %>%
  mutate(proficiency = scale(proficiency, scale = FALSE)) %>%
  filter(environment %in% c('short', 'long', 'island'))

str(md)

# apply deviation coding ...

contrasts(md$group) <- contr.treatment(2) - matrix(rep(1/2, 2), ncol=1)
contrasts(md$environment) <- contr.treatment(3) - matrix(rep(1/3, 6), ncol=2)

# view contrasts ...

contrasts(md$group)
contrasts(md$environment)

#------------------------------------------------------------------------------#
# + + orc ----
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# + + + model 1
#------------------------------------------------------------------------------#

# fit model ...

tic()
model1 <- glmer(resumption ~ proficiency * group * environment + 
                  (1 | participant) + 
                  (1 | item),
                data = filter(md, study == '210510_do'),
                family = binomial,
                control = glmerControl(optimizer = 'bobyqa', 
                                       optCtrl = list(maxfun = 1e6))) %>%
  write_rds('models/ept_orc_glmer_md1.rds')
summary(model1)
toc()
beep()

# 22.08 sec elapsed
# no warnings
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                     -6.47696    0.82214  -7.878 3.32e-15 ***
#  proficiency                     -0.16705    0.05374  -3.109  0.00188 ** 
# group2                           0.27474    0.86192   0.319  0.74992    
# environment2                     2.97083    0.65507   4.535 5.76e-06 ***
# environment3                     4.16141    0.65923   6.313 2.75e-10 ***
# proficiency:group2               0.08895    0.10650   0.835  0.40362    
# proficiency:environment2         0.16713    0.07583   2.204  0.02753 *  
# proficiency:environment3         0.18887    0.07341   2.573  0.01008 *  
# group2:environment2             -0.48716    1.28739  -0.378  0.70513    
# group2:environment3             -0.91777    1.29345  -0.710  0.47798    
# proficiency:group2:environment2 -0.07179    0.15087  -0.476  0.63419    
# proficiency:group2:environment3 -0.14570    0.14652  -0.994  0.32005 

test(emtrends(model, ~ group * environment, var = 'proficiency', adjust = 'mvt'))

#------------------------------------------------------------------------------#
# + + + model 2 (final?) ----
#------------------------------------------------------------------------------#

tic()
model2 <- glmer(resumption ~ proficiency * group * environment + 
                (1 + environment | participant) + 
                (1 | item),
                data = filter(md, study == '210510_do'),
                family = binomial,
                control = glmerControl(optimizer = 'bobyqa', 
                                       optCtrl = list(maxfun = 1e6))) %>%
  write_rds('models/ept_orc_glmer_md2.rds')
summary(model2)
toc()
beep()

model2 <- read_rds('models/ept_orc_glmer_md2.rds')

# 72.11 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#                                 Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                     -7.65066    1.09721  -6.973 3.11e-12 ***
# proficiency                     -0.17807    0.06639  -2.682  0.00731 ** 
# group2                           0.41869    0.99071   0.423  0.67257    
# environment2                     6.24357    2.44611   2.552  0.01070 *  
# environment3                     5.11054    2.60560   1.961  0.04984 *  
# proficiency:group2               0.11250    0.12529   0.898  0.36925    
# proficiency:environment2         0.24674    0.12137   2.033  0.04205 *  
# proficiency:environment3         0.21772    0.12885   1.690  0.09107 .  
# group2:environment2             -1.29077    1.70230  -0.758  0.44830    
# group2:environment3             -1.96666    1.84681  -1.065  0.28692    
# proficiency:group2:environment2 -0.14976    0.20754  -0.722  0.47055    
# proficiency:group2:environment3 -0.19141    0.22265  -0.860  0.38994  

lme.dscore(model2, data = filter(md, study == '210510_do'), type = 'lme4')

#                                          t       df           d
# proficiency                     -2.5952664 131.9977 -0.45178190 *
# group2                          -0.8560837 131.9977 -0.14902636
# environment2                     4.4350491 131.8542  0.77246965 **
# environment3                     5.1738948 131.3253  0.90297000 **
# proficiency:group2               1.4447159 131.9977  0.25149498 *
# proficiency:environment2         0.2456786 132.3366  0.04271273
# proficiency:environment3         0.3101656 131.7265  0.05404892
# group2:environment2             -0.4795665 132.1954 -0.08342009
# group2:environment3             -0.5601580 131.5764 -0.09766783
# proficiency:group2:environment2 -0.6996357 131.9539 -0.12181221
# proficiency:group2:environment3 -1.0488595 131.3894 -0.18300675

trends <- emtrends(model2, ~ group * environment, var = 'proficiency', adjust = 'mvt')
test(trends)

# group    environment proficiency.trend     SE  df z.ratio p.value
# korean   short                 -0.4460 0.1755 Inf  -2.541  0.0603 .
# mandarin short                 -0.2198 0.1804 Inf  -1.219  0.7354
# korean   long                  -0.1244 0.0698 Inf  -1.782  0.3393
# mandarin long                  -0.0479 0.0637 Inf  -0.752  0.9608
# korean   island                -0.1326 0.1004 Inf  -1.320  0.6632
# mandarin island                -0.0978 0.0946 Inf  -1.034  0.8503
# P value adjustment: mvt method for 6 tests 

eff_size(trends, sigma = sigma(model2), edf = Inf, method = 'identity')

# contrast        effect.size     SE  df asymp.LCL asymp.UCL
# korean short        -0.4460 0.1755 Inf    -0.790   -0.1020 *
# mandarin short      -0.2198 0.1804 Inf    -0.573    0.1337 *
# korean long         -0.1244 0.0698 Inf    -0.261    0.0124
# mandarin long       -0.0479 0.0637 Inf    -0.173    0.0769
# korean island       -0.1326 0.1004 Inf    -0.329    0.0643
# mandarin island     -0.0978 0.0946 Inf    -0.283    0.0876

# plot ...

plot <- emmip(model2, environment ~ proficiency | group, cov.reduce = range, CIs = TRUE, plotit = FALSE)

ggplot(plot, aes(x = xvar, y = yvar, group = environment, col = environment, linetype = environment)) +
  theme_classic() +
  geom_ribbon(aes(ymin = LCL, ymax = UCL, fill = environment), alpha = .1, col = NA) +
  geom_line(lwd = .75) +
  scale_y_continuous(name='log odds ratio') +
  scale_x_continuous(name='mean-centered proficiency score') +
  scale_linetype_manual(values = c('longdash', 'solid', 'twodash')) +
  scale_color_manual(values = c('#619CFF', '#00BA38', '#F8766D')) +
  scale_fill_manual(values = c('#619CFF', '#00BA38', '#F8766D')) +
  theme(legend.key.size = unit(2,"line")) +
  facet_wrap(~group, labeller = as_labeller(c(`korean` = 'KLE', `mandarin` = 'MLE')))

ggsave("plots/orc/ept_resumption_proficiency_emmeans.png", width=6.5, height=2, dpi=600)

anova(model1, model2)

# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)    
# model1   14 724.52 803.21 -348.26   696.52                         
# model2   19 710.37 817.17 -336.19   672.37 24.144  5  0.0002037 ***

#------------------------------------------------------------------------------#
# + + model 3
#------------------------------------------------------------------------------#

tic()
model3 <- glmer(resumption ~ proficiency * group * environment + 
                  (1 + environment | participant) + 
                  (1 + environment | item),
                data = filter(md, study == '210510_do'),
                family = binomial,
                control = glmerControl(optimizer = 'bobyqa', 
                                       optCtrl = list(maxfun = 1e6))) %>%
  write_rds('models/ept_orc_glmer_md3.rds')
summary(model3)
toc()
beep()

# 199.92 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                     -8.06685    1.25253  -6.440 1.19e-10 ***
# proficiency                     -0.18206    0.07157  -2.544   0.0110 *  
# group2                           0.44388    1.03547   0.429   0.6682    
# environment2                     6.33516    3.13668   2.020   0.0434 *  
# environment3                     5.53846    3.20392   1.729   0.0839 .  
# proficiency:group2               0.11647    0.13245   0.879   0.3792    
# proficiency:environment2         0.26048    0.13778   1.891   0.0587 .  
# proficiency:environment3         0.23585    0.14539   1.622   0.1048    
# group2:environment2             -1.21061    1.82450  -0.664   0.5070    
# group2:environment3             -2.01777    1.97317  -1.023   0.3065    
# proficiency:group2:environment2 -0.15261    0.23052  -0.662   0.5079    
# proficiency:group2:environment3 -0.19865    0.24330  -0.816   0.4142 

anova(model2, model3)

# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model2   19 710.37 817.17 -336.19   672.37                     
# model3   24 715.65 850.55 -333.83   667.65 4.7211  5     0.4509

#------------------------------------------------------------------------------#
# + + model 4
#------------------------------------------------------------------------------#

tic()
model4 <- glmer(resumption ~ proficiency * group * environment + 
                  (1 + environment | participant) + 
                  (1 + group | item),
                data = filter(md, study == '210510_do'),
                family = binomial,
                control = glmerControl(optimizer = 'bobyqa', 
                                       optCtrl = list(maxfun = 1e6))) %>%
  write_rds('models/ept_orc_glmer_md4.rds')
summary(model4)
toc()
beep()

citation('emmeans')

anova(model2, model4)



#------------------------------------------------------------------------------#
# + + src ----
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# + + + model 1
#------------------------------------------------------------------------------#

# fit model ...

tic()
model1 <- glmer(resumption ~ proficiency * group * environment + 
                  (1 | participant) + 
                  (1 | item),
                data = filter(md, study == '210510_su'),
                family = binomial,
                control = glmerControl(optimizer = 'bobyqa', 
                                       optCtrl = list(maxfun = 1e6))) %>%
  write_rds('models/ept_src_glmer_md1.rds')
summary(model1)
toc()
beep()

# 14.33 sec elapsed
# Model failed to converge: degenerate  Hessian with 2 negative eigenvalues

#------------------------------------------------------------------------------#
# + + + model 2 ----
#------------------------------------------------------------------------------#

# fit model ...

tic()
model2 <- glmer(resumption ~ proficiency * group * environment + 
                  (1 | participant) + 
                  (1 | item),
                data = filter(md, study == '210510_su', environment %in% c('long', 'island')),
                family = binomial,
                control = glmerControl(optimizer = 'bobyqa', 
                                       optCtrl = list(maxfun = 1e6))) %>%
  write_rds('models/ept_src_glmer_md2.rds')
summary(model2)
toc()
beep

# 5.88 sec elapsed
# no warnings
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                          -1.54703    0.48481  -3.191 0.001418 ** 
# proficiency                           0.05577    0.05671   0.983 0.325398    
# group2                                3.03668    0.95603   3.176 0.001491 ** 
# environmentisland                     0.76593    0.22809   3.358 0.000785 ***
# proficiency:group2                    0.13745    0.11255   1.221 0.221983    
# proficiency:environmentisland         0.09934    0.03049   3.258 0.001121 ** 
# group2:environmentisland             -1.21704    0.45606  -2.669 0.007617 ** 
# proficiency:group2:environmentisland -0.26094    0.06147  -4.245 2.19e-05 ***

test(emtrends(model2, ~ group * environment, var = 'proficiency', adjust = 'mvt'))

# group    environment proficiency.trend     SE  df z.ratio p.value
# korean   long                  -0.0129 0.0817 Inf  -0.158  0.9990
# mandarin long                   0.1245 0.0780 Inf   1.596  0.2882
# korean   island                 0.2169 0.0851 Inf   2.547  0.0336 *
# mandarin island                 0.0934 0.0778 Inf   1.200  0.5254
# P value adjustment: mvt method for 4 tests 

trend <- emtrends(model2, ~ group * environment, var = 'proficiency', adjust = 'mvt')
eff_size(trend, sigma = sigma(model2), edf = df.residual(model2), method = 'identity')

# contrast        effect.size     SE  df asymp.LCL asymp.UCL
# korean long         -0.0129 0.0817 Inf   -0.1732     0.147
# mandarin long        0.1245 0.0780 Inf   -0.0284     0.277
# korean island        0.2169 0.0851 Inf    0.0500     0.384 *
# mandarin island      0.0934 0.0778 Inf   -0.0592     0.246

# plot ...

plot <- emmip(model2, environment ~ proficiency | group, cov.reduce = range, CIs = TRUE, plotit = FALSE)

ggplot(plot, aes(x = xvar, y = yvar, group = environment, col = environment, linetype = environment)) +
  theme_classic() +
  geom_ribbon(aes(ymin = LCL, ymax = UCL, fill = environment), alpha = .1, col = NA) +
  geom_line(lwd = .75) +
  scale_y_continuous(name='log odds ratio') +
  scale_x_continuous(name='mean-centered proficiency score') +
  scale_linetype_manual(values = c('solid', 'twodash')) +
  scale_color_manual(values = c('#00BA38', '#F8766D')) +
  scale_fill_manual(values = c('#00BA38', '#F8766D')) +
  theme(legend.key.size = unit(2,"line")) +
  facet_wrap(~group, labeller = as_labeller(c(`korean` = 'KLE', `mandarin` = 'MLE')))

ggsave("plots/src/ept_resumption_proficiency_emmeans.png", width=6.5, height=2, dpi=600)


#------------------------------------------------------------------------------#
# + + model 3
#------------------------------------------------------------------------------#

# fit model ...

model3 <- lmer(resumption ~ proficiency * group * environment + 
                 (1 + environment | participant) + 
                 (1 + environment | item),
               data = filter(md, study == '210510_su'))
summary(model3)

# optimizer (nloptwrap) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                       0.290711   0.025345 130.360764  11.470  < 2e-16 ***
# proficiency                       0.005090   0.002978 126.710555   1.709   0.0899 .  
# group2                            0.125192   0.049632 126.715680   2.522   0.0129 *  
# environment2                      0.368794   0.039606 123.998812   9.312 5.84e-16 ***
# environment3                      0.437093   0.041606 124.273558  10.506  < 2e-16 ***
# proficiency:group2               -0.001231   0.005957 126.749247  -0.207   0.8366    
# proficiency:environment2          0.006409   0.004658 126.354653   1.376   0.1713    
# proficiency:environment3          0.012587   0.004900 126.360895   2.569   0.0114 *  
# group2:environment2               0.176195   0.077624 126.368309   2.270   0.0249 *  
# group2:environment3               0.066877   0.081657 126.371951   0.819   0.4143    
# proficiency:group2:environment2   0.010859   0.009318 126.459266   1.165   0.2461    
# proficiency:group2:environment3  -0.007086   0.009802 126.445757  -0.723   0.4710 

anova(model2, model3, refit = FALSE)

# npar    AIC    BIC  logLik deviance Chisq Df Pr(>Chisq)
# model2   20 366.50 478.16 -163.25   326.50                    
# model3   25 370.55 510.13 -160.28   320.55 5.947  5     0.3114

#------------------------------------------------------------------------------#
# + + model 4
#------------------------------------------------------------------------------#

# fit model ...

model4 <- lmer(resumption ~ proficiency * group * environment + 
                 (1 + environment | participant) + 
                 (1 + group | item),
               data = filter(md, study == '210510_su'))
summary(model4)

# optimizer (nloptwrap) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                       0.290705   0.025270 130.237963  11.504   <2e-16 ***
# proficiency                       0.005093   0.002979 126.996447   1.710   0.0898 .  
# group2                            0.125148   0.050034 128.791116   2.501   0.0136 *  
# environment2                      0.368858   0.038802 126.159076   9.506   <2e-16 ***
# environment3                      0.437126   0.040838 126.232205  10.704   <2e-16 ***
# proficiency:group2               -0.001214   0.005958 126.996447  -0.204   0.8389    
# proficiency:environment2          0.006381   0.004658 126.243797   1.370   0.1731    
# proficiency:environment3          0.012578   0.004902 126.311477   2.566   0.0115 *  
# group2:environment2               0.176368   0.077605 126.166545   2.273   0.0247 *  
# group2:environment3               0.066841   0.081678 126.239960   0.818   0.4147    
# proficiency:group2:environment2   0.010771   0.009317 126.280149   1.156   0.2498    
# proficiency:group2:environment3  -0.007070   0.009805 126.348956  -0.721   0.4722 

anova(model2, model4, refit = FALSE)

# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model2   20 366.50 478.16 -163.25   326.50                     
# model4   22 366.71 489.55 -161.36   322.71 3.7832  2     0.1508

#------------------------------------------------------------------------------#
# + lm ~ orc ~ overall ----
#------------------------------------------------------------------------------#

# adjust column classes ...

md <- plot %>%
  filter(group %in% c('korean', 'mandarin')) %>%
  mutate(group = factor(group)) %>%
  mutate(proficiency = scale(proficiency, scale = FALSE))

str(md)

# apply deviation coding ...

contrasts(md$group) <- contr.treatment(2) - matrix(rep(1/2, 2), ncol=1)

# view contrasts ...

contrasts(md$group)
  
# fit model ...

model <- lm(prop ~ proficiency, data = filter(md, group == 'korean'))
summary(model)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  17.2932     2.2308   7.752 2.45e-12 ***
# proficiency   0.0347     0.2757   0.126      0.9    
# Residual standard error: 25.17 on 128 degrees of freedom
# Multiple R-squared:  0.0001238,	Adjusted R-squared:  -0.007688 
# F-statistic: 0.01585 on 1 and 128 DF,  p-value: 0.9

model <- lm(prop ~ proficiency, data = filter(md, group == 'mandarin'))
summary(model)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 20.53422    2.36000   8.701    1e-14 ***
# proficiency -0.04527    0.28646  -0.158    0.875    
# Residual standard error: 27.38 on 135 degrees of freedom
# Multiple R-squared:  0.000185,	Adjusted R-squared:  -0.007221 
# F-statistic: 0.02498 on 1 and 135 DF,  p-value: 0.8747

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
# modeling: glmmTMB ~ all response types ----
#------------------------------------------------------------------------------#

# check participants ...

check <- ep %>%
  group_by(study, group, participant) %>%
  summarise() %>%
  ungroup() %>% 
  group_by(study, group) %>%
  summarise(n = n()) %>%
  ungroup()

# remove participants who only gave nontarget responses ...

temp <- ep %>%
  filter(type != 'nontarget') %>%
  group_by(study, group, participant) %>%
  summarise() %>%
  ungroup()

md <- ep %>%
  filter(participant %in% temp$participant)

# check participants ...

check <- md %>%
  group_by(study, group, participant) %>%
  summarise() %>%
  ungroup() %>% 
  group_by(study, group) %>%
  summarise(n = n()) %>%
  ungroup()

# check response types ...

check <- md %>%
  mutate(type = factor(type))

summary(check$type)

# recode responses ...

md <- md %>%
  mutate(resumption = case_when(type == 'resumption' ~ TRUE,
                                TRUE ~ FALSE),
         gap = case_when(type == 'gap' ~ TRUE,
                         TRUE ~ FALSE),
         modified = case_when(type == 'other' ~ TRUE,
                              TRUE ~ FALSE),
         nontarget = case_when(type == 'nontarget' ~ TRUE,
                               TRUE ~ FALSE)) %>%
  pivot_longer(cols = c('resumption', 'gap', 'modified', 'nontarget'), names_to = 'dependency', values_to = 'response2') %>%
  filter(group %in% c('english', 'korean', 'mandarin')) %>%
  mutate(group = fct_drop(group)) %>%
  mutate(environment = fct_relevel(environment, 'short', 'long', 'island'))

# check counts per variable

check <- md %>%
  filter(environment %in% c('short', 'long', 'island')) %>%
  group_by(study, group, environment, dependency) %>%
  summarise(count = sum(response2)) %>%
  ungroup()

# summarise to create count data ...

md2 <- md %>%
  filter(environment %in% c('short', 'long', 'island')) %>%
  group_by(study, group, participant, environment, dependency) %>%
  summarise(count = sum(response2))

# plot total counts ...

plot <- md2 %>%
  group_by(study, group, environment, dependency) %>%
  summarise(count = sum(count)) %>%
  ungroup() 

p1 <- ggplot(data = filter(plot, study == '210510_do'))
p2 <- ggplot(data = filter(plot, study == '210510_su'))

s <- list(
  aes(x = environment, y = count, group = dependency, col = dependency, shape = dependency, label = count),
  geom_line(lwd = 1, position = position_dodge(width = .4)),
  geom_point(size = 3, position = position_dodge(width = .4)),
  geom_text(size = 2.5, col = "black", hjust = .5, vjust = -1, position = position_dodge(width = .4)),
  theme_classic(),
  scale_y_continuous(name='total count', limits = c(0, 400)),
  scale_colour_manual(name='dependency', 
                      values=c('#648fff', 'gray85', 'gray60', '#ffb000')),
  scale_shape_manual(name='dependency', 
                     values=c(16, 18, 17, 15)),
  theme(text = element_text(size = 12),
        legend.position = 'right',
        legend.margin = margin(t = -.4, unit = 'cm'),
        plot.title = element_text(size = 12, hjust = .5)),
  facet_wrap(~group, labeller = as_labeller(c(`english` = 'ENS', `korean` = 'KLE', `mandarin` = 'MLE')))
)

p1 + s
ggsave("plots/orc/ept_total_all.png", width=6.5, height=2, dpi=600)

p2 + s
ggsave("plots/src/ept_total_all.png", width=6.5, height=2, dpi=600)

# plot mean counts ...

plot <- md2 %>%
  group_by(study, group, environment, dependency) %>%
  summarise(mean = mean(count, na.rm=T),
            sd = sd(count, na.rm=T),
            n = n()) %>%
  mutate(se = sd / sqrt(n),
         ci = qt(1 - (0.05 / 2), n - 1) * se) %>%
  ungroup() 

p1 <- ggplot(data = filter(plot, study == '210510_do'))
p2 <- ggplot(data = filter(plot, study == '210510_su'))

s <- list(
  aes(x = environment, y = mean, group = dependency, col = dependency, shape = dependency),
  geom_line(lwd = 1, position = position_dodge(width = .4)),
  geom_point(size = 3, position = position_dodge(width = .4)),
  geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci), 
                width = 0, lwd = 5, linetype = 1, alpha = .5,
                position = position_dodge(width = .4)),
  theme_classic(),
  scale_y_continuous(name="count", limits = c(-.05, 5), breaks = c(0, 1, 2, 3, 4, 5)),
  scale_colour_manual(name='dependency', 
                      values=c('#648fff', 'gray85', 'gray60', '#ffb000')),
  scale_shape_manual(name='dependency', 
                     values=c(16, 18, 17, 15)),
  theme(text = element_text(size = 12),
        legend.position = 'right',
        legend.margin = margin(t = -.4, unit = 'cm'),
        plot.title = element_text(size = 12, hjust = .5)),
  facet_wrap(~group, labeller = as_labeller(c(`english` = 'ENS', `korean` = 'KLE', `mandarin` = 'MLE')))
)

p1 + s
ggsave("plots/orc/ept_count_all.png", width=6.5, height=2, dpi=600)

p2 + s
ggsave("plots/src/ept_count_all.png", width=6.5, height=2, dpi=600)

# modeling ...

md2 <- md2 %>%
  mutate_at(c('study', 'group', 'participant', 'dependency', 'environment'), factor)

summary(md2)

# apply deviation coding ...

contrasts(md2$dependency) <- contr.treatment(4) - matrix(rep(1/4, 12), ncol=3)
contrasts(md2$environment) <- contr.treatment(3) - matrix(rep(1/3, 6), ncol=2)
contrasts(md2$group) <- contr.treatment(3) - matrix(rep(1/3, 6), ncol=2)

# view contrasts ...

contrasts(md2$dependency)
contrasts(md2$environment)
contrasts(md2$group)

# load package ...

library(glmmTMB) # (https://fcorowe.github.io/countdata_modelling/)

# fit poisson regression model ...

model1 <- glmmTMB(count ~ dependency * environment * group + (1 | participant), 
                  data = filter(md2, study == '210510_do'),
                  ziformula = ~0,
                  family = poisson)
summary(model1)

# no errors

check_overdispersion(model1)

# Overdispersion detected.

model1 <- glmmTMB(count ~ dependency * environment * group + (1 | participant), 
                  data = filter(md2, study == '210510_su'),
                  ziformula = ~0,
                  family = poisson)
summary(model1)

# no errors

check_overdispersion(model1)

# Overdispersion detected.

# fit zero-inflated poisson regression model ...

model1 <- glmmTMB(count ~ dependency * environment * group + (1 | participant), 
                  data = filter(md2, study == '210510_do'),
                  ziformula = ~1,
                  family = poisson)
summary(model1)

# no errors

check_overdispersion(model1)

# Overdispersion detected.

model1 <- glmmTMB(count ~ dependency * environment * group + (1 | participant), 
                  data = filter(md2, study == '210510_su'),
                  ziformula = ~1,
                  family = poisson)
summary(model1)

# Model convergence problem

# fit negative binomial regression model ...

# + model 1 (final) ----

model1 <- glmmTMB(count ~ dependency * environment * group + (1 | participant),
                  data = filter(md2, study == '210510_do'),
                  ziformula = ~0,
                  family = nbinom2)
summary(model1)

model1 %>% write_rds('models/ept_orc_count_all_glmmtmb.rds')

model1 <- read_rds('models/ept_orc_count_all_glmmtmb.rds')

# no errors
# Conditional model:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                     -0.228739   0.036983  -6.185 6.21e-10 ***
# dependency2                     -1.118095   0.082649 -13.528  < 2e-16 ***
# dependency3                     -1.893907   0.092653 -20.441  < 2e-16 ***
# dependency4                     -2.207199   0.110224 -20.025  < 2e-16 ***
# environment2                     0.118230   0.096008   1.231 0.218152    
# environment3                     0.399858   0.091585   4.366 1.27e-05 ***
# group2                           0.381720   0.091642   4.165 3.11e-05 ***
# group3                           0.278564   0.092227   3.020 0.002524 ** 
# dependency2:environment2        -0.822963   0.200714  -4.100 4.13e-05 ***
# dependency3:environment2        -0.088113   0.229356  -0.384 0.700850    
# dependency4:environment2         0.702865   0.297398   2.363 0.018109 *  
# dependency2:environment3        -0.530776   0.199803  -2.657 0.007896 ** 
# dependency3:environment3         0.288344   0.227132   1.269 0.204263    
# dependency4:environment3         2.104808   0.277818   7.576 3.56e-14 ***
# dependency2:group2               0.930341   0.202944   4.584 4.56e-06 ***
# dependency3:group2               0.502497   0.226151   2.222 0.026287 *  
# dependency4:group2               0.902121   0.274591   3.285 0.001019 ** 
# dependency2:group3               0.858407   0.198973   4.314 1.60e-05 ***
# dependency3:group3               0.401533   0.221871   1.810 0.070334 .  
# dependency4:group3               0.497112   0.280854   1.770 0.076727 .  
# environment2:group2             -0.020728   0.240543  -0.086 0.931329    
# environment3:group2             -0.518462   0.226144  -2.293 0.021870 *  
# environment2:group3             -0.009302   0.242382  -0.038 0.969386    
# environment3:group3             -0.557179   0.228998  -2.433 0.014969 *  
# dependency2:environment2:group2  0.442226   0.493862   0.895 0.370550    
# dependency3:environment2:group2 -0.499532   0.564326  -0.885 0.376057    
# dependency4:environment2:group2 -0.107482   0.748704  -0.144 0.885850    
# dependency2:environment3:group2 -0.624536   0.489071  -1.277 0.201608    
# dependency3:environment3:group2 -0.796025   0.553404  -1.438 0.150317    
# dependency4:environment3:group2 -2.517266   0.689355  -3.652 0.000261 ***
# dependency2:environment2:group3 -0.245856   0.484333  -0.508 0.611722    
# dependency3:environment2:group3 -0.673704   0.547620  -1.230 0.218607    
# dependency4:environment2:group3 -0.187394   0.770365  -0.243 0.807809    
# dependency2:environment3:group3 -1.259686   0.477474  -2.638 0.008334 ** 
# dependency3:environment3:group3 -1.451034   0.547927  -2.648 0.008092 ** 
# dependency4:environment3:group3 -2.598193   0.709425  -3.662 0.000250 ***

table <- summary(model1)$coefficients$cond %>%
  as.data.frame() %>%
  rownames_to_column('Parameter') %>%
  rename('SE' = 'Std. Error', 'z' = 'z value', 'p' = 'Pr(>|z|)') %>%
  mutate_at(vars('p'), round, 3) %>%
  mutate_at(vars('Estimate', 'SE', 'z'), round, 2) %>%
  mutate_if(is.character, str_replace_all, 
            c('dependency2' = 'dependency(modified)', 'dependency3' = 'dependency(nontarget)', 'dependency4' = 'dependency(resumption)',
              'environment2' = 'environment(long)', 'environment3' = 'environment(island)',
              'group2' = 'group(KLE)', 'group3' = 'group(KLE)')) %>%
  rename('*p*' = 'p', '*z*' = 'z')

table %>% write_rds('tables/ept_orc_glmmtmb_primary.rds')

# check for overdispersion ...

check_overdispersion(model1)

# Overdispersion detected.

# pairwise comparisons ...

pairs <- pairs(emmeans(model1, ~ dependency * environment * group), adjust = 'mvt', simple = 'each', combine = TRUE, reverse = TRUE)
pairs

tibble <- as_tibble(pairs)

table <- tibble %>%
  mutate_at(vars('p.value'), round, 3) %>%
  mutate_at(vars('estimate':'t.ratio'), round, 2) %>%
  rename('*p*' = 'p.value', '*t*' = 't.ratio',
         'Environment' = 'environment', 'Group' = 'group', 'Dependency' = 'dependency', 
         'Contrast' = 'contrast', 'Estimate' = 'estimate') %>%
  mutate_if(is.character, str_replace_all, 
            c('english' = 'ENS', 'korean' = 'KLE', 'mandarin' = 'MLE'))

table %>% write_rds('tables/ept_orc_glmmtmb_pairwise.rds')

# tibble2 <- tibble %>%
#   mutate(sig = case_when(p.value < .05 & p.value >= .01 ~ ' *', 
#                          p.value < .01 & p.value >= .001 ~ ' **',
#                          p.value < .001 ~ ' ***',
#                          TRUE ~ '')) %>%
#   mutate_at(vars('p.value'), sprintf, fmt = "%0.3f") %>%
#   mutate_at(vars('estimate', 'SE', 't.ratio'), sprintf, fmt = "%0.2f") %>%
#   rename(p = p.value, t = t.ratio) %>%
#   mutate(p = str_c(p, sig)) %>%
#   mutate_if(is.character, str_replace_all, 
#             c('english' = 'ENS', 'korean' = 'KLE', 'mandarin' = 'MLE')) %>%
#   select(-sig) %>%
#   mutate_all(as.character)

pairs %>%
  kbl(digits = c(2, 2, 2, 2, 2, 2, 2, 3)) %>%
  remove_column(6)

# plot ...

plot <- plot(emmeans(model1, ~ dependency * environment * group), combine = TRUE, comparisons = TRUE, adjust = 'mvt', plotit = FALSE)

ggplot(plot, aes(x = environment, y = the.emmean, group = dependency, col = dependency, shape = dependency)) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0, lwd = 5, alpha = .5, position = position_dodge(width = .4)) +
  geom_errorbar(aes(ymin = case_when(is.na(lcmpl)~the.emmean, TRUE ~ lcmpl), ymax = case_when(is.na(rcmpl)~the.emmean, TRUE ~ rcmpl)), width = 0, lwd = 1, position = position_dodge(width = .4)) +
  geom_line(lwd = 1, position = position_dodge(width = .4)) +
  geom_point(size = 3, position = position_dodge(width = .4)) +
  theme_classic() +
  scale_y_continuous(name="log count", limits = c(-4, 2)) +
  scale_colour_manual(name="dependency", values=c('#648fff', 'gray85', 'gray60', '#ffb000')) +
  scale_shape_manual(name="dependency", values=c(16, 18, 17, 15)) +
  theme(text = element_text(size = 12), 
        legend.margin=margin(0, 0, 0, -5)) +
  facet_wrap(~group, labeller = as_labeller(c(`english` = 'ENS', `korean` = 'KLE', `mandarin` = 'MLE')))

ggsave("plots/orc/ept_count_all_emmeans.png", width=6.5, height=2, dpi=600)

model1 <- glmmTMB(count ~ dependency * environment * group + (1 | participant),
                  data = filter(md2, study == '210510_su'),
                  ziformula = ~0,
                  family = nbinom2)
summary(model1)

model1 <- lmer(count ~ dependency * environment * group + (1 | participant),
               data = filter(md2, study == '210510_su'))
summary(model1)

# Conditional model:
#   Estimate Std. Error z value Pr(>|z|)
# (Intercept)                     -4.099e+00  2.452e+04   0.000    1.000
# dependency2                     -3.491e+00  4.876e+04   0.000    1.000
# environment2                     8.130e+00  7.240e+04   0.000    1.000
# environment3                     4.006e+00  7.314e+04   0.000    1.000
# group2                           2.941e+00  7.314e+04   0.000    1.000
# group3                           7.750e+00  1.177e+04   0.001    0.999
# dependency2:environment2         1.785e+01  1.448e+05   0.000    1.000
# dependency2:environment3         2.951e+01  1.471e+05   0.000    1.000
# dependency2:group2              -1.109e+01  1.471e+05   0.000    1.000
# dependency2:group3              -2.697e-01  2.355e+04   0.000    1.000
# environment2:group2              6.996e+00  2.172e+05   0.000    1.000
# environment3:group2              1.842e+01  2.207e+05   0.000    1.000
# environment2:group3             -8.030e+00  1.756e+03  -0.005    0.996
# environment3:group3              3.071e+00  3.532e+04   0.000    1.000
# dependency2:environment2:group2  1.566e+01  4.344e+05   0.000    1.000
# dependency2:environment3:group2 -1.555e+01  4.388e+05   0.000    1.000
# dependency2:environment2:group3 -1.338e+01  3.512e+03  -0.004    0.997
# dependency2:environment3:group3 -4.477e+01  7.064e+04  -0.001    0.999

# fit model with more complex random-effects structure ...

tic()
model2 <- glmmTMB(count ~ dependency * environment * group + 
                    (1 + environment | participant),
                  data = filter(md2, study == '210510_do'),
                  ziformula = ~0,
                  family = nbinom2)
summary(model2)
toc()
beep()

# 38.34 sec elapsed
# Model convergence problem


#------------------------------------------------------------------------------#
# modeling: glmmTMB ~ gap vs. resumption ----
#------------------------------------------------------------------------------#

# check participants ...

check <- ep %>%
  group_by(study, group, participant) %>%
  summarise() %>%
  ungroup() %>% 
  group_by(study, group) %>%
  summarise(n = n()) %>%
  ungroup()

# remove participants who only gave nontarget responses ...

temp <- ep %>%
  filter(type != 'nontarget') %>%
  group_by(study, group, participant) %>%
  summarise() %>%
  ungroup()

md <- ep %>%
  filter(participant %in% temp$participant)

# check participants ...

check <- md %>%
  group_by(study, group, participant) %>%
  summarise() %>%
  ungroup() %>% 
  group_by(study, group) %>%
  summarise(n = n()) %>%
  ungroup()

# recode responses ...

md <- md %>%
  mutate(resumption = case_when(type == 'resumption' ~ TRUE,
                                TRUE ~ FALSE),
         gap = case_when(type == 'gap' ~ TRUE,
                         TRUE ~ FALSE)) %>%
  pivot_longer(cols = c('resumption', 'gap'), names_to = 'dependency', values_to = 'response2') %>%
  filter(group %in% c('english', 'korean', 'mandarin')) %>%
  mutate(group = fct_drop(group)) %>%
  mutate(environment = fct_relevel(environment, 'short', 'long', 'island'))

# check counts per variable

check <- md %>%
  filter(condition %in% c('cond1', 'cond2', 'cond3')) %>%
  group_by(study, group, environment, dependency) %>%
  summarise(count = sum(response2)) %>%
  ungroup()

check <- check %>%
  filter(study == '210510_do')

kbl(check)

# summarise to create count data ...

md2 <- md %>%
  filter(condition %in% c('cond1', 'cond2', 'cond3')) %>%
  group_by(study, group, participant, environment, dependency) %>%
  summarise(count = sum(response2))

# plot total counts ...

plot <- md2 %>%
  group_by(study, group, environment, dependency) %>%
  summarise(count = sum(count)) %>%
  ungroup() 

p1 <- ggplot(data = filter(plot, study == '210510_do'))
p2 <- ggplot(data = filter(plot, study == '210510_su'))

s <- list(
  aes(x = environment, y = count, group = dependency, col = dependency, shape = dependency, label = count),
  geom_line(lwd = 1, position = position_dodge(width = .4)),
  geom_point(size = 3, position = position_dodge(width = .4)),
  geom_text(size = 2.5, col = "black", hjust = .5, vjust = -1, position = position_dodge(width = .4)),
  theme_classic(),
  scale_y_continuous(name='total count', limits = c(0, 400)),
  scale_colour_manual(name='dependency', 
                      values=c('#648fff', '#ffb000'), 
                      labels=c('gap', 'resumption')),
  scale_shape_manual(name='dependency', 
                     values=c(16, 15), 
                     labels=c('gap', 'resumption')),
  theme(text = element_text(size = 12),
        legend.position = 'right',
        legend.margin = margin(t = -.4, unit = 'cm'),
        plot.title = element_text(size = 12, hjust = .5)),
  facet_wrap(~group, labeller = as_labeller(c(`english` = 'ENS', `korean` = 'KLE', `mandarin` = 'MLE')))
)

p1 + s
ggsave("plots/orc/ept_total.png", width=6.5, height=2, dpi=600)

p2 + s
ggsave("plots/src/ept_total.png", width=6.5, height=2, dpi=600)

# plot mean counts ...

plot <- md2 %>%
  group_by(study, group, environment, dependency) %>%
  summarise(mean = mean(count, na.rm=T),
            sd = sd(count, na.rm=T),
            n = n()) %>%
  mutate(se = sd / sqrt(n),
         ci = qt(1 - (0.05 / 2), n - 1) * se) %>%
  ungroup() 

p1 <- ggplot(data = filter(plot, study == '210510_do'))
p2 <- ggplot(data = filter(plot, study == '210510_su'))

s <- list(
  aes(x = environment, y = mean, group = dependency, col = dependency, shape = dependency),
  geom_line(lwd = 1, position = position_dodge(width = .4)),
  geom_point(size = 3, position = position_dodge(width = .4)),
  geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci), 
                width = 0, lwd = 5, linetype = 1, alpha = .5,
                position = position_dodge(width = .4)),
  theme_classic(),
  scale_y_continuous(name="count", limits = c(-.05, 5), breaks = c(0, 1, 2, 3, 4, 5)),
  scale_colour_manual(name='dependency', 
                      values=c('#648fff', '#ffb000'), 
                      labels=c('gap', 'resumption')),
  scale_shape_manual(name='dependency', 
                     values=c(16, 15), 
                     labels=c('gap', 'resumption')),
  theme(text = element_text(size = 12),
        legend.position = 'right',
        legend.margin = margin(t = -.4, unit = 'cm'),
        plot.title = element_text(size = 12, hjust = .5)),
  facet_wrap(~group, labeller = as_labeller(c(`english` = 'ENS', `korean` = 'KLE', `mandarin` = 'MLE')))
)

p1 + s
ggsave("plots/orc/ept_count.png", width=6.5, height=2, dpi=600)

p2 + s
ggsave("plots/src/ept_count.png", width=6.5, height=2, dpi=600)

# modeling ...

md2 <- md2 %>%
  mutate_at(c('study', 'group', 'participant', 'dependency', 'environment'), factor)

summary(md2)

# apply deviation coding ...

contrasts(md2$dependency) <- contr.treatment(2) - matrix(rep(1/2, 2), ncol=1)
contrasts(md2$environment) <- contr.treatment(3) - matrix(rep(1/3, 6), ncol=2)
contrasts(md2$group) <- contr.treatment(3) - matrix(rep(1/3, 6), ncol=2)

# view contrasts ...

contrasts(md2$dependency)
contrasts(md2$environment)
contrasts(md2$group)

# load package ...

library(glmmTMB) # (https://fcorowe.github.io/countdata_modelling/)

# fit poisson regression model ...

model1 <- glmmTMB(count ~ dependency * environment * group + (1 | participant), 
                  data = filter(md2, study == '210510_do'),
                  ziformula = ~0,
                  family = poisson)
summary(model1)

# no errors

check_overdispersion(model1)

# dispersion ratio = 1.615
# Pearson's Chi-Squared = 2149.197
# p-value = < 0.001
# Overdispersion detected.

model1 <- glmmTMB(count ~ dependency * environment * group + (1 | participant), 
                  data = filter(md2, study == '210510_su'),
                  ziformula = ~0,
                  family = poisson)
summary(model1)

# no errors

check_overdispersion(model1)

# dispersion ratio =    1.430
# Pearson's Chi-Squared = 1619.668
#                 p-value =  < 0.001
# Overdispersion detected.

# fit zero-inflated poisson regression model ...

model1 <- glmmTMB(count ~ dependency * environment * group + (1 | participant), 
                  data = filter(md2, study == '210510_do'),
                  ziformula = ~1,
                  family = poisson)
summary(model1)

# no errors

check_overdispersion(model1)

# dispersion ratio =    1.454
# Pearson's Chi-Squared = 1933.875
#                 p-value =  < 0.001
# Overdispersion detected.

model1 <- glmmTMB(count ~ dependency * environment * group + (1 | participant), 
                  data = filter(md2, study == '210510_su'),
                  ziformula = ~1,
                  family = poisson)
summary(model1)

# Model convergence problem

# fit negative binomial regression model ...

# + model 1 (final) ----

model1 <- glmmTMB(count ~ dependency * environment * group + (1 | participant),
                  data = filter(md2, study == '210510_do'),
                  ziformula = ~0,
                  family = nbinom2)
summary(model1)

model1 %>% write_rds('models/ept_orc_count_glmmtmb.rds')

# Conditional model:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                     -0.15578    0.06344  -2.455 0.014071 *  
# dependency2                     -2.26413    0.09652 -23.457  < 2e-16 ***
# environment2                     0.53821    0.13104   4.107 4.01e-05 ***
# environment3                     1.01351    0.11992   8.452  < 2e-16 ***
# group2                           0.14149    0.14981   0.944 0.344943    
# group3                           0.01186    0.15143   0.078 0.937551    
# dependency2:environment2         0.67984    0.26207   2.594 0.009483 ** 
# dependency2:environment3         2.11128    0.23989   8.801  < 2e-16 ***
# dependency2:group2               0.88802    0.23925   3.712 0.000206 ***
# dependency2:group3               0.47953    0.24790   1.934 0.053066 .  
# environment2:group2             -0.01639    0.33301  -0.049 0.960736    
# environment3:group2             -0.78280    0.29927  -2.616 0.008904 ** 
# environment2:group3              0.20108    0.34721   0.579 0.562500    
# environment3:group3             -0.50374    0.31309  -1.609 0.107624    
# dependency2:environment2:group2 -0.11497    0.66611  -0.173 0.862970    
# dependency2:environment3:group2 -2.55872    0.59928  -4.270 1.96e-05 ***
# dependency2:environment2:group3 -0.20948    0.69438  -0.302 0.762891    
# dependency2:environment3:group3 -2.64641    0.62675  -4.222 2.42e-05 *** 

# table ...

summary(model1)$coefficients$cond %>%
  kbl(digits = c(2, 2, 2, 3)) 

# check for overdispersion ...

check_overdispersion(model1)

# dispersion ratio =    1.008
# Pearson's Chi-Squared = 1340.388
#                 p-value =    0.415
# No overdispersion detected.

# pairwise comparisons ...

pairs <- pairs(emmeans(model1, ~ dependency * environment * group), adjust = 'mvt', simple = 'each', combine = TRUE, reverse = TRUE)
pairs

# plot ...

plot <- plot(emmeans(model1, ~ dependency * environment * group), combine = TRUE, comparisons = TRUE, adjust = 'mvt', plotit = FALSE)

ggplot(plot, aes(x = environment, y = the.emmean, group = dependency, col = dependency, shape = dependency)) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0, lwd = 5, alpha = .5, position = position_dodge(width = .4)) +
  geom_errorbar(aes(ymin = case_when(is.na(lcmpl)~the.emmean, TRUE ~ lcmpl), ymax = case_when(is.na(rcmpl)~the.emmean, TRUE ~ rcmpl)), width = 0, lwd = 1, position = position_dodge(width = .4)) +
  geom_line(lwd = 1, position = position_dodge(width = .4)) +
  geom_point(size = 3, position = position_dodge(width = .4)) +
  theme_classic() +
  scale_y_continuous(name="log count", limits = c(-4, 2)) +
  scale_colour_manual(name="dependency", values=c('#648fff', '#ffb000'), labels=c("gap", "resumption")) +
  scale_shape_manual(name="dependency", values=c(16, 15), labels=c("gap", "resumption")) +
  theme(text = element_text(size = 12), 
        legend.margin=margin(0, 0, 0, -5)) +
  facet_wrap(~group, labeller = as_labeller(c(`english` = 'ENS', `korean` = 'KLE', `mandarin` = 'MLE')))

ggsave("plots/orc/ept_count_emmeans.png", width=6.5, height=2, dpi=600)

model1 <- glmmTMB(count ~ dependency * environment * group + (1 | participant),
                  data = filter(md2, study == '210510_su'),
                  ziformula = ~0,
                  family = nbinom2)
summary(model1)

model1 <- lmer(count ~ dependency * environment * group + (1 | participant),
                  data = filter(md2, study == '210510_su'))
summary(model1)

# Conditional model:
#   Estimate Std. Error z value Pr(>|z|)
# (Intercept)                     -4.099e+00  2.452e+04   0.000    1.000
# dependency2                     -3.491e+00  4.876e+04   0.000    1.000
# environment2                     8.130e+00  7.240e+04   0.000    1.000
# environment3                     4.006e+00  7.314e+04   0.000    1.000
# group2                           2.941e+00  7.314e+04   0.000    1.000
# group3                           7.750e+00  1.177e+04   0.001    0.999
# dependency2:environment2         1.785e+01  1.448e+05   0.000    1.000
# dependency2:environment3         2.951e+01  1.471e+05   0.000    1.000
# dependency2:group2              -1.109e+01  1.471e+05   0.000    1.000
# dependency2:group3              -2.697e-01  2.355e+04   0.000    1.000
# environment2:group2              6.996e+00  2.172e+05   0.000    1.000
# environment3:group2              1.842e+01  2.207e+05   0.000    1.000
# environment2:group3             -8.030e+00  1.756e+03  -0.005    0.996
# environment3:group3              3.071e+00  3.532e+04   0.000    1.000
# dependency2:environment2:group2  1.566e+01  4.344e+05   0.000    1.000
# dependency2:environment3:group2 -1.555e+01  4.388e+05   0.000    1.000
# dependency2:environment2:group3 -1.338e+01  3.512e+03  -0.004    0.997
# dependency2:environment3:group3 -4.477e+01  7.064e+04  -0.001    0.999

# fit model with more complex random-effects structure ...

model2 <- glmmTMB(count ~ dependency * environment * group + 
                    (1 + dependency | participant),
                  data = filter(md2, study == '210510_do'),
                  ziformula = ~0,
                  family = nbinom2)
summary(model2)

# 38.34 sec elapsed
# Model convergence problem; non-positive-definite Hessian matrix.

#------------------------------------------------------------------------------#
# modeling: mblogit ----
#------------------------------------------------------------------------------#

# check participants ...

check <- ep %>%
  group_by(study, group, participant) %>%
  summarise() %>%
  ungroup() %>% 
  group_by(study, group) %>%
  summarise(n = n()) %>%
  ungroup()

# remove participants who only gave nontarget responses ...

temp <- ep %>%
  filter(type != 'nontarget') %>%
  group_by(study, group, participant) %>%
  summarise() %>%
  ungroup()

md <- ep %>%
  filter(participant %in% temp$participant)

# check participants ...

check <- md %>%
  group_by(study, group, participant) %>%
  summarise() %>%
  ungroup() %>% 
  group_by(study, group) %>%
  summarise(n = n()) %>%
  ungroup()

# recode responses ...

md <- md %>%
  mutate(outcome = factor(case_when(type == 'resumption' ~ 'resumption',
                             type == 'gap' ~ 'gap',
                             TRUE ~ 'other')))

# plot total counts ...

plot <- md %>%
  filter(environment %in% c('short', 'long', 'island')) %>%
  group_by(study, group, environment, outcome) %>%
  summarise(count = n()) %>%
  ungroup()

p1 <- ggplot(data = filter(plot, study == '210510_do'))
p2 <- ggplot(data = filter(plot, study == '210510_su'))

s <- list(
  aes(x = environment, y = count, group = outcome, col = outcome, shape = outcome, label = count),
  geom_line(lwd = 1, position = position_dodge(width = .4)),
  geom_point(size = 3, position = position_dodge(width = .4)),
  geom_text(size = 2.5, col = "black", hjust = .5, vjust = -1, position = position_dodge(width = .4)),
  theme_classic(),
  scale_y_continuous(name='total count', limits = c(0, 400)),
  scale_x_discrete(limits = c('short', 'long', 'island')),
  scale_colour_manual(name='outcome', 
                      values=c('#648fff', 'gray', '#ffb000')),
  theme(text = element_text(size = 12),
        legend.position = 'right',
        legend.margin = margin(t = -.4, unit = 'cm'),
        plot.title = element_text(size = 12, hjust = .5)),
  facet_wrap(~group, labeller = as_labeller(c(`english` = 'ENS', `korean` = 'KLE', `mandarin` = 'MLE')))
)

p1 + s
ggsave("plots/orc/ept_total_mclogit.png", width=6.5, height=2, dpi=600)

p2 + s
ggsave("plots/src/ept_totalmclogit.png", width=6.5, height=2, dpi=600)

# plot mean counts ...

plot <- md %>%
  mutate(resumption = case_when(outcome == 'resumption' ~ TRUE, TRUE ~ FALSE),
         gap = case_when(outcome == 'gap' ~ TRUE, TRUE ~ FALSE),
         other = case_when(outcome == 'other' ~ TRUE, TRUE ~ FALSE)) %>%
  pivot_longer(cols = c('resumption', 'gap', 'other'), names_to = 'category', values_to = 'value')

plot <- plot %>%
  filter(environment %in% c('short', 'long', 'island')) %>%
  group_by(study, group, environment, category) %>%
  summarise(mean = mean(value, na.rm=T),
            sd = sd(value, na.rm=T),
            n = n()) %>%
  mutate(se = sd / sqrt(n),
         ci = qt(1 - (0.05 / 2), n - 1) * se) %>%
  ungroup() 

p1 <- ggplot(data = filter(plot, study == '210510_do'))
p2 <- ggplot(data = filter(plot, study == '210510_su'))

s <- list(
  aes(x = environment, y = mean, group = category, col = category, shape = category),
  geom_line(lwd = 1, position = position_dodge(width = .4)),
  geom_point(size = 3, position = position_dodge(width = .4)),
  geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci), 
                width = 0, lwd = 5, linetype = 1, alpha = .5,
                position = position_dodge(width = .4)),
  theme_classic(),
  scale_y_continuous(name="proportion", limits = c(0, 1)),
  scale_x_discrete(limits = c('short', 'long', 'island')),
  scale_colour_manual(values=c('#648fff', 'gray', '#ffb000')),
  theme(text = element_text(size = 12),
        legend.position = 'right',
        legend.margin = margin(t = -.4, unit = 'cm'),
        plot.title = element_text(size = 12, hjust = .5)),
  facet_wrap(~group, labeller = as_labeller(c(`english` = 'ENS', `korean` = 'KLE', `mandarin` = 'MLE')))
)

p1 + s
ggsave("plots/orc/ept_prop.png", width=6.5, height=2, dpi=600)

p2 + s
ggsave("plots/src/ept_prop.png", width=6.5, height=2, dpi=600)

s <- list(
  aes(x = environment, y = mean*100, group = category, col = category, shape = category, label = round(mean*100, digits = 0)),
  geom_line(lwd = 1, position = position_dodge(width = .4)),
  geom_point(size = 3, position = position_dodge(width = .4)),
  geom_errorbar(aes(ymin = mean*100 - ci*100, ymax = mean*100 + ci*100), 
                width = 0, lwd = 5, linetype = 1, alpha = .5,
                position = position_dodge(width = .4)),
  geom_text(size = 2.5, col = 'black', hjust = .5, vjust = -1, position = position_dodge(width = .4)),
  theme_classic(),
  scale_y_continuous(name="proportion", limits = c(0, 100)),
  scale_x_discrete(limits = c('short', 'long', 'island')),
  scale_colour_manual(values=c('#648fff', 'gray', '#ffb000')),
  theme(text = element_text(size = 12),
        legend.position = 'right',
        legend.margin = margin(t = -.4, unit = 'cm'),
        plot.title = element_text(size = 12, hjust = .5)),
  facet_wrap(~group, labeller = as_labeller(c(`english` = 'ENS', `korean` = 'KLE', `mandarin` = 'MLE')))
)

p1 + s
ggsave("plots/orc/ept_prc.png", width=6.5, height=2, dpi=600)

p2 + s
ggsave("plots/src/ept_prc.png", width=6.5, height=2, dpi=600)

# modeling ...

md2 <- md %>%
  filter(is.na(environment) == FALSE) %>%
  select(study, group, participant, item, environment, outcome) %>%
  mutate_at(c('study', 'group', 'participant', 'item', 'environment', 'outcome'), factor) %>%
  mutate(environment = fct_relevel(environment, 'short', 'long', 'island'),
         outcome = fct_relevel(outcome, 'gap', 'resumption', 'other'))

summary(md2)

# apply deviation coding ...

contrasts(md2$environment) <- contr.treatment(3) - matrix(rep(1/3, 6), ncol=2)
contrasts(md2$group) <- contr.treatment(3) - matrix(rep(1/3, 6), ncol=2)

# view contrasts ...

contrasts(md2$environment)
contrasts(md2$group)

# load package ...

library(mclogit)

# fit mixed conditional logit model ...

model1 <- mblogit(outcome ~ environment * group, 
                  random = ~ 1 | participant,
                  data = filter(md2, study == '210510_do'))
summary(model1)

dispersion(model1, method="Afroz")
dispersion(model1, method="Deviance")

pairs(emmeans(model1, ~ environment * group))

pairs <- pairs(emmeans(model1, ~ environment * group), adjust = 'mvt', simple = 'each', combine = TRUE, reverse = TRUE)
pairs

# error

warnings()

#------------------------------------------------------------------------------#
# modeling: glmer ~ resumption ----
#------------------------------------------------------------------------------#

# check participants ...

check <- ep %>%
  group_by(study, group, participant) %>%
  summarise() %>%
  ungroup() %>% 
  group_by(study, group) %>%
  summarise(n = n()) %>%
  ungroup()

# remove participants who only gave nontarget responses ...

temp <- ep %>%
  filter(type != 'nontarget') %>%
  group_by(study, group, participant) %>%
  summarise() %>%
  ungroup()

md <- ep %>%
  filter(participant %in% temp$participant)

# check participants ...

check <- md %>%
  group_by(study, group, participant) %>%
  summarise() %>%
  ungroup() %>% 
  group_by(study, group) %>%
  summarise(n = n()) %>%
  ungroup()

# code responses as ±resumption ...

md <- md %>%
  mutate(resumption = case_when(type == 'resumption' ~ TRUE,
                                TRUE ~ FALSE)) %>%
  filter(group %in% c('english', 'korean', 'mandarin')) %>%
  mutate(group = fct_drop(group)) %>%
  mutate(environment = fct_relevel(environment, 'short', 'long', 'island'))

# plot count data ...

plot <- md %>%
  filter(condition %in% c('cond1', 'cond2', 'cond3')) %>%
  group_by(study, group, participant, environment) %>%
  summarise(count = sum(resumption))

plot <- plot %>%
  group_by(study, group, environment) %>%
  summarise(mean = mean(count, na.rm=T),
            sd = sd(count, na.rm=T),
            n = n()) %>%
  mutate(se = sd / sqrt(n),
         ci = qt(1 - (0.05 / 2), n - 1) * se) %>%
  ungroup() %>%
  mutate(dependency = 'resumption')

p1 <- ggplot(data = filter(plot, study == '210510_do'))
p2 <- ggplot(data = filter(plot, study == '210510_su'))

s <- list(
  aes(x = environment, y = mean, group = group, col = dependency),
  geom_line(lwd=1),
  geom_point(size=2, shape = 15),
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.5, lwd=1, linetype=1),
  theme_classic(),
  scale_y_continuous(name="count", breaks=c(0, 1, 2, 3, 4, 5)),
  #scale_x_discrete(name="environment", limits=c('short', 'long', 'island')),
  scale_colour_manual(name="dependency", values='#ffb000'),
  #scale_shape_manual(name="dependency", values=c(16, 15), labels=c('gap', 'resumption')),
  theme(text = element_text(size = 12),
        legend.position = 'none',
        #legend.margin = margin(t = -.4, unit = 'cm'),
        plot.title = element_text(size = 12, hjust = .5)),
  facet_wrap(~group)
)

p1 + s
p2 + s

# modeling ...

md2 <- md2 %>%
  mutate_at(c('study', 'group', 'participant', 'environment'), factor)

summary(md2)

# apply deviation coding ...

contrasts(md2$environment) <- contr.treatment(3) - matrix(rep(1/3, 6), ncol=2)
contrasts(md2$group) <- contr.treatment(3) - matrix(rep(1/3, 6), ncol=2)

# view contrasts ...

contrasts(md2$environment)
contrasts(md2$group)

#------------------------------------------------------------------------------#
# + orc model 1 ----
#------------------------------------------------------------------------------#

md2 <- md %>%
  filter(is.na(condition) == FALSE) %>%
  mutate(condition = fct_drop(condition))

md2 <- md2 %>%
  mutate_at(c('environment', 'group', 'participant', 'item'), as.factor)

summary(md2)

# md2 <- md2 %>%
#   filter(type %in% c('gap', 'resumption'))

# apply deviation coding ...

contrasts(md2$environment) <- contr.treatment(3) - matrix(rep(1/3, 6), ncol=2)
contrasts(md2$group) <- contr.treatment(3) - matrix(rep(1/3, 6), ncol=2)

# view contrasts ...

contrasts(md2$environment)
contrasts(md2$group)

#------------------------------------------------------------------------------#
# + + model 2 - orc
#------------------------------------------------------------------------------#

# fit model ...

tic()
model2 <- glmer(resumption ~ environment * group +
                  (1 | participant) + 
                  (1 | item), 
                data = filter(md2, study == '210510_do'), family = binomial, 
                control = glmerControl(optimizer = 'bobyqa', optCtrl = list(maxfun = 1e6))) %>%
  write_rds('models/ajt_doall_glmer_md2.rds')
summary(model2)
toc()
beep()

model2 <- read_rds('models/ajt_doall_glmer_md2.rds')

# 6.91 sec elapsed
# no warnings
# Fixed effects:
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept)         -5.242666   0.394286 -13.297  < 2e-16 ***
# environment2         1.711611   0.345890   4.948 7.48e-07 ***
# environment3         3.966361   0.350131  11.328  < 2e-16 ***
# group2              -0.002109   0.667432  -0.003  0.99748    
# group3              -0.113479   0.650661  -0.174  0.86155    
# environment2:group2  0.551321   0.830234   0.664  0.50666    
# environment3:group2 -2.667690   0.821993  -3.245  0.00117 ** 
# environment2:group3  0.743056   0.890715   0.834  0.40415    
# environment3:group3 -2.709075   0.872339  -3.106  0.00190 ** 

#------------------------------------------------------------------------------#
# + + model 2 - src
#------------------------------------------------------------------------------#

# check ...

check <- md2 %>%
  group_by(study, group, environment) %>%
  summarise(count = sum(resumption)) %>%
  ungroup()

# fit model ...

tic()
model2 <- glmer(resumption ~ environment * group +
                  (1 | participant) + 
                  (1 | item), 
                data = filter(md2, study == '210510_su'), family = binomial, 
                control = glmerControl(optimizer = 'bobyqa', optCtrl = list(maxfun = 1e6))) %>%
  write_rds('models/ajt_src_glmer_md2.rds')
summary(model2)
toc()
beep()

# 4.57 sec elapsed
# no warnings
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)
# (Intercept)          -7.3868     5.1895  -1.423    0.155
# environment2         14.7634    15.5440   0.950    0.342
# environment3         18.8882    15.5421   1.215    0.224
# group2                0.4982    14.0875   0.035    0.972
# group3                7.7666     8.4076   0.924    0.356
# environment2:group2   6.3650    42.2051   0.151    0.880
# environment3:group2  -2.0430    42.2033  -0.048    0.961
# environment2:group3 -10.9856    25.1419  -0.437    0.662
# environment3:group3 -21.0977    25.1315  -0.839    0.401


#------------------------------------------------------------------------------#
# + + model 3
#------------------------------------------------------------------------------#

# fit model ...

tic()
model3 <- glmer(resumption ~ environment * group +
                  (1 + environment | participant) + 
                  (1 + group | item), 
                data = md2, family = binomial, 
                control = glmerControl(optimizer = 'bobyqa', optCtrl = list(maxfun = 1e6))) %>%
  write_rds('models/ajt_doall_glmer_md3.rds')
summary(model3)
toc()
beep()
# 
# 109.08 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)         -7.18699    0.92807  -7.744 9.63e-15 ***
# environment2         5.12139    2.02440   2.530 0.011412 *  
# environment3         7.75504    2.16426   3.583 0.000339 ***
# group2              -0.01080    0.93216  -0.012 0.990753    
# group3               0.02393    0.92522   0.026 0.979366    
# environment2:group2  0.85515    1.15822   0.738 0.460314    
# environment3:group2 -3.04159    1.53828  -1.977 0.048012 *  
# environment2:group3  0.53493    1.29338   0.414 0.679174    
# environment3:group3 -4.00926    1.60311  -2.501 0.012387 * 

anova(model3, model5)

# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model5   16 1277.3 1375.3 -622.66   1245.3                     
# model3   21 1286.4 1415.0 -622.20   1244.4 0.9046  5     0.9699

#------------------------------------------------------------------------------#
# + + model 4
#------------------------------------------------------------------------------#

# fit model ...

tic()
model4 <- glmer(resumption ~ environment * group +
                  (1 + environment | participant) + 
                  (1 + environment | item), 
                data = md2, family = binomial, 
                control = glmerControl(optimizer = 'bobyqa', optCtrl = list(maxfun = 1e6))) %>%
  write_rds('models/ajt_doall_glmer_md4.rds')
summary(model4)
toc()
beep()

model4 <- read_rds('models/ajt_doall_glmer_md4.rds')

# old
# 205.05 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)          -5.8201     1.1843  -4.914 8.90e-07 ***
# environment2         -4.8788     3.4479  -1.415   0.1571    
# environment3          1.7361     0.7991   2.173   0.0298 *  
# group2                0.8271     1.0444   0.792   0.4284    
# group3                0.5232     1.0301   0.508   0.6115    
# environment2:group2   0.5897     1.6960   0.348   0.7281    
# environment3:group2  -5.2938     0.9220  -5.741 9.39e-09 ***
# environment2:group3   0.6738     1.8497   0.364   0.7157    
# environment3:group3  -5.8088     0.9264  -6.270 3.60e-10 ***

# post-hoc tests ...

pairwise <- model4 %>%
  emmeans(~ environment * group) %>%
  contrast('pairwise', simple = 'each', combine = TRUE) %>%
  summary(by = NULL, adjust = 'holm')
pairwise

# 109.93 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)         -7.63700    1.09097  -7.000 2.56e-12 ***
# environment2         5.81498    2.72401   2.135  0.03278 *  
# environment3         8.70456    2.76447   3.149  0.00164 ** 
# group2               0.01286    1.00004   0.013  0.98974    
# group3               0.15807    0.97815   0.162  0.87162    
# environment2:group2  0.85524    1.42007   0.602  0.54701    
# environment3:group2 -3.12660    1.76549  -1.771  0.07657 .  
# environment2:group3  0.35462    1.52802   0.232  0.81648    
# environment3:group3 -4.28170    1.81432  -2.360  0.01828 *

anova(model4, model5)

# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model5   16 1277.3 1375.3 -622.66   1245.3                     
# model4   21 1282.4 1411.0 -620.21   1240.4 4.8943  5     0.4289

#------------------------------------------------------------------------------#
# + + model 5 (final) ----
#------------------------------------------------------------------------------#

# fit model ...

tic()
model5 <- glmer(resumption ~ environment * group +
                  (1 + environment | participant) + 
                  (1 | item), 
                data = md2, family = binomial, 
                control = glmerControl(optimizer = 'bobyqa', optCtrl = list(maxfun = 1e6))) %>%
  write_rds('models/ajt_doall_glmer_md5.rds')
summary(model5)
toc()
beep()

model5 <- read_rds('models/ajt_doall_glmer_md5.rds')

tic()
model5b <- glmer(resumption ~ environment * group +
                   (1 + environment | participant) + 
                   (1 | item), 
                 data = md2, family = binomial, 
                 control = glmerControl(optimizer = 'bobyqa', optCtrl = list(maxfun = 1e6)))
summary(model5b)
toc()
beep()

# 31.06 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)         -7.10416    0.91103  -7.798 6.29e-15 ***
# environment2         5.04945    2.22179   2.273 0.023045 *  
# environment3         7.66577    2.23591   3.428 0.000607 ***
# group2               0.02424    0.93761   0.026 0.979373    
# group3               0.10818    0.91391   0.118 0.905770    
# environment2:group2  0.80797    1.35736   0.595 0.551676    
# environment3:group2 -3.08800    1.65422  -1.867 0.061938 .  
# environment2:group3  0.45457    1.43478   0.317 0.751378    
# environment3:group3 -4.04956    1.67972  -2.411 0.015915 *  

anova(model2, model5)

# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)    
# model2   11 1312.5 1379.8 -645.23   1290.5                         
# model5   16 1277.3 1375.3 -622.66   1245.3 45.153  5   1.35e-08 ***

# post-hoc tests ...

pairs <- pairs(emmeans(model5, ~ environment * group), adjust = 'mvt', simple = 'each', combine = TRUE, reverse = TRUE)
pairs

# group    environment contrast           estimate    SE  df z.ratio p.value
# english  .           long - short          4.629 2.530 Inf   1.830  0.5173
# english  .           island - short       10.045 2.568 Inf   3.911  0.0015 *
# english  .           island - long         5.416 1.203 Inf   4.502  0.0001 *
# korean   .           long - short          5.437 2.426 Inf   2.241  0.2538
# korean   .           island - short        6.957 2.495 Inf   2.788  0.0675 .
# korean   .           island - long         1.520 1.108 Inf   1.372  0.8268
# mandarin .           long - short          5.083 2.089 Inf   2.433  0.1672
# mandarin .           island - short        5.995 2.196 Inf   2.731  0.0794 .
# mandarin .           island - long         0.912 1.015 Inf   0.898  0.9817
# .        short       korean - english      0.784 1.753 Inf   0.447  0.9999
# .        short       mandarin - english    1.307 1.759 Inf   0.743  0.9946
# .        short       mandarin - korean     0.522 1.633 Inf   0.320  1.0000
# .        long        korean - english      1.592 0.923 Inf   1.724  0.5934
# .        long        mandarin - english    1.761 0.908 Inf   1.939  0.4387
# .        long        mandarin - korean     0.169 0.833 Inf   0.203  1.0000
# .        island      korean - english     -2.304 0.729 Inf  -3.159  0.0222 * 
# .        island      mandarin - english   -2.743 0.717 Inf  -3.827  0.0021 *
# .        island      mandarin - korean    -0.439 0.739 Inf  -0.594  0.9988
# Results are given on the log odds ratio (not the response) scale. 
# P value adjustment: mvt method for 18 tests 

# effect sizes ...

as_tibble(summary(model5)$coefficients) %>%
  mutate(OR = exp(Estimate)) %>%
  kbl(digits = c(2, 2, 2, 3, 2))

pairs %>% as_tibble() %>%
  mutate(OR = exp(estimate)) %>%
  kbl(digits = c(2, 2, 2, 2, 2, 2, 2, 3))

# tables ...

summary(model5)$coefficients %>%
  kbl(digits = c(2, 2, 2, 3))

pairs %>%
  kbl(digits = c(2, 2, 2, 2, 2, 2, 2, 3)) %>%
  remove_column(6)

# plot ...

plot <- plot(emmeans(model5, ~ environment | group), combine = TRUE, comparisons = TRUE, adjust = 'mvt', plotit = FALSE)

ggplot(plot, aes(x = environment, y = the.emmean, group = group)) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0, lwd = 5, col = '#ffb000', alpha = .5, position = position_dodge(width = .4)) +
  geom_errorbar(aes(ymin = case_when(is.na(lcmpl)~the.emmean, TRUE ~ lcmpl), ymax = case_when(is.na(rcmpl)~the.emmean, TRUE ~ rcmpl)), width = 0, lwd = 1, col = '#ffb000', position = position_dodge(width = .4)) +
  geom_line(lwd = 1, col = '#ffb000', position = position_dodge(width = .4)) +
  geom_point(size = 3, col = '#ffb000', shape = 15, position = position_dodge(width = .4)) +
  theme_classic() +
  scale_y_continuous(name="log odds ratio", limits = c(-20, 0)) +
  scale_x_discrete(limits = c('short', 'long', 'island')) +
  theme(text = element_text(size = 12), 
        legend.margin=margin(0, 0, 0, -5)) +
  facet_wrap(~group, labeller = as_labeller(c(`english` = 'ENS', `korean` = 'KLE', `mandarin` = 'MLE')))

ggsave("plots/orc/ept_rate_emmeans.png", width=6.5, height=2, dpi=600)



#------------------------------------------------------------------------------#
# speaking time - plot - overall ----
#------------------------------------------------------------------------------#

st <- read_excel('data/speaking_analysis.xlsx')

# check participants

check <- st %>%
  group_by(study, group, participant) %>%
  summarise() %>%
  summarise(n = n()) %>%
  ungroup()

# summarise data for plotting by group

plot <- st %>%
  mutate(duration = as.numeric(duration) * 1000) %>%
  rename(dependency = type) %>%
  mutate(environment = as.factor(environment)) %>%
  mutate(environment = fct_relevel(environment, 'short', 'long', 'island')) %>%
  group_by(study, group, dependency, environment) %>%
  summarise(mean = mean(duration, na.rm=T),
            sd = sd(duration, na.rm=T),
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

confint(lm(count ~ 1, md2), level = .95)

# generate plot

p1 <- ggplot(data = filter(plot, study == '210510_do'), aes(x = environment, y = mean, group = dependency, col = dependency, shape = dependency))
p2 <- ggplot(data = filter(plot, study == '210510_su'), aes(x = environment, y = mean, group = dependency, col = dependency, shape = dependency))

s <- list(
  ggtitle('ORC Speaking Times (Overall)'),
  geom_line(lwd=1),
  geom_point(size=2),
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.5, lwd=1, linetype=1),
  theme_classic(),
  scale_y_continuous(name="speaking time (ms)", limits=c(500, 1000)),
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
ggsave('plots/orc/ept_time_overall.png', width=6.5, height=3.5, dpi=600)

p2 + s
ggsave('plots/src/ept_time_overall.png', width=6.5, height=3.5, dpi=600)

#------------------------------------------------------------------------------#
# speaking time - plot - by region ----
#------------------------------------------------------------------------------#

st <- read_excel('data/speaking_analysis.xlsx')

# check participants

check <- st %>%
  group_by(study, group, participant) %>%
  summarise() %>%
  summarise(n = n()) %>%
  ungroup()

# summarise data for plotting by group

plot <- st %>%
  mutate(duration = as.numeric(duration) * 1000) %>%
  rename(dependency = type) %>%
  mutate(environment = as.factor(environment)) %>%
  mutate(environment = fct_relevel(environment, 'short', 'long', 'island')) %>%
  group_by(study, group, dependency, environment, region) %>%
  summarise(mean = mean(duration, na.rm=T),
            sd = sd(duration, na.rm=T),
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
  scale_y_continuous(name="speaking time (ms)", limits=c(0, 1600)),
  scale_x_discrete(name="region", 
                   limits=c('region0', 'region1', 'region2', 'region3', 'region4', 'region5', 'region6', 'region7'),
                   labels=c('0', '1', '2', '3', '4', '5', '6', '7')),
  scale_colour_manual(name="dependency", values=c('#648fff', '#ffb000'), labels=c('gap', 'resumption')),
  scale_shape_manual(name="dependency", values=c(16, 15), labels=c('gap', 'resumption')),
  theme(text = element_text(size = 12),
        legend.position = 'bottom',
        legend.margin = margin(t = -.4, unit = 'cm'),
        plot.title = element_text(size = 12, hjust = .5)),
  facet_grid2(vars(panel), vars(environment), axes = 'all', remove_labels = 'y')
)

p1 + s
ggsave('plots/orc/ept_time_region.png', width=6.5, height=3.5, dpi=600)

p2 + s
ggsave('plots/src/ept_time_region.png', width=6.5, height=3.5, dpi=600)

#==============================================================================#
# ::::: self-paced reading task (sprt) :::::::::::::::::::::::::::::::::::  ----
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
  geom_line(lwd = 1, position = position_dodge(width = .4)),
  geom_point(size = 3, position = position_dodge(width = .4)),
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=0, lwd=5, linetype=1, alpha = .5, position = position_dodge(width = .4)),
  theme_classic(),
  scale_x_discrete(name="environment", limits = c("short", "long", "island"), labels = c("short", "long", "island")),
  scale_y_continuous(name="% accuracy", limits=c(70, 100)),
  scale_colour_manual(name="dependency", values=c('#648fff', '#ffb000'), labels=c("gap", "resumption")),
  scale_shape_manual(name="dependency", values=c(16, 15), labels=c("gap", "resumption")),
  theme(text = element_text(size = 12), 
        axis.title.y = element_text(margin=margin(r=-3)),
        legend.margin=margin(0, 0, 0, -5)),
  facet_wrap(~panel)
)

p1 + s
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
  annotate('rect', xmin = 0.6, xmax = 1.4, ymin = -150, ymax = 150, alpha = .1),
  annotate('rect', xmin = 1.6, xmax = 2.4, ymin = -150, ymax = 150, alpha = .1),
  annotate('rect', xmin = 2.6, xmax = 3.4, ymin = -150, ymax = 150, alpha = .1),
  geom_hline(yintercept = 0),
  geom_vline(xintercept = 0),
  geom_line(lwd=1, position = position_dodge(width = .4)),
  geom_point(size=3, position = position_dodge(width = .4)),
  geom_errorbar(aes(ymin=mean_rrt-ci, ymax=mean_rrt+ci), width=0, lwd=5, linetype=1, alpha = .5, position = position_dodge(width = .4)),
  theme_classic(),
  scale_y_continuous(name="RRT (ms)", breaks = seq(-150, 150, by = 50), limits=c(-150, 150)),
  scale_x_continuous(name="region", limits=c(-3.25, 4.25), breaks = c(-3, -2, -1, 0, 1, 2, 3, 4)),
  scale_colour_manual(name="dependency", values=c('#648fff', '#ffb000'), labels=c('gap', 'resumption')),
  scale_shape_manual(name="dependency", values=c(16, 15), labels=c('gap', 'resumption')),
  theme(text = element_text(size = 12),
        legend.position = 'bottom',
        legend.margin = margin(t = -.4, unit = 'cm'),
        plot.title = element_text(size = 12, hjust = .5)),
  facet_grid2(vars(panel), vars(environment), axes = 'all', remove_labels = 'y')
)

# s <- list(
#   annotate('rect', xmin = 0.6, xmax = 1.4, ymin = -150, ymax = 150, alpha = .1),
#   annotate('rect', xmin = 1.6, xmax = 2.4, ymin = -150, ymax = 150, alpha = .1),
#   annotate('rect', xmin = 2.6, xmax = 3.4, ymin = -150, ymax = 150, alpha = .1),
#   geom_hline(yintercept = 0),
#   geom_vline(xintercept = 0),
#   geom_line(lwd=1, position = position_dodge(width = 0.2)),
#   geom_point(size=2, position = position_dodge(width = 0.2)),
#   geom_errorbar(aes(ymin=mean_rrt-ci, ymax=mean_rrt+ci), width=0, lwd=2, linetype=1, alpha=.5, position = position_dodge(width = 0.2)),
#   theme_classic(),
#   scale_y_continuous(name="residual reading time (ms)", limits=c(-150, 150)),
#   scale_x_continuous(name="region", limits=c(-3.25, 4.25), breaks = c(-3, -2, -1, 0, 1, 2, 3, 4)),
#   scale_colour_manual(name="dependency", values=c('#648fff', '#ffb000'), labels=c('gap', 'resumption')),
#   scale_shape_manual(name="dependency", values=c(16, 15), labels=c('gap', 'resumption')),
#   theme(text = element_text(size = 12),
#         legend.position = 'bottom',
#         legend.margin = margin(t = -.4, unit = 'cm'),
#         plot.title = element_text(size = 12, hjust = .5)),
#   facet_grid2(vars(panel), vars(environment), axes = 'all', remove_labels = 'y')
# )

# s <- list(
#   annotate('rect', xmin = 0.6, xmax = 1.4, ymin = -180, ymax = 180, alpha = .15),
#   annotate('rect', xmin = 1.6, xmax = 2.4, ymin = -180, ymax = 180, alpha = .15),
#   annotate('rect', xmin = 2.6, xmax = 3.4, ymin = -180, ymax = 180, alpha = .15),
#   geom_hline(yintercept = 0),
#   geom_vline(xintercept = 0),
#   geom_line(lwd=1),
#   geom_point(size=2),
#   geom_errorbar(aes(ymin=mean_rrt-ci, ymax=mean_rrt+ci), width=.5, lwd=1, linetype=1),
#   theme_classic(),
#   scale_y_continuous(name="residual reading time (ms)", limits=c(-200, 200)),
#   scale_x_continuous(name="region", limits=c(-3.25, 4.25), breaks = c(-3, -2, -1, 0, 1, 2, 3, 4)),
#   scale_colour_manual(name="dependency", values=c('#648fff', '#ffb000'), labels=c('gap', 'resumption')),
#   scale_shape_manual(name="dependency", values=c(16, 15), labels=c('gap', 'resumption')),
#   theme(text = element_text(size = 12),
#         legend.position = 'bottom',
#         legend.margin = margin(t = -.4, unit = 'cm'),
#         plot.title = element_text(size = 12, hjust = .5)),
#   facet_grid2(vars(panel), vars(environment), axes = 'all', remove_labels = 'y')
# )

p1 + s

# p1 + s + 
#   geom_text(data = filter(plot, environment == 'island', panel == 'ENS'), mapping = aes(x = 1, y = 185, label = '*'), col = 'black') +
#   geom_text(data = filter(plot, environment == 'short', panel == 'ENS'), mapping = aes(x = 2, y = 185, label = '*'), col = 'black') +
#   geom_text(data = filter(plot, environment == 'island', panel == 'ENS'), mapping = aes(x = 2, y = 185, label = '*'), col = 'black') +
#   geom_text(data = filter(plot, environment == 'long', panel == 'KLE'), mapping = aes(x = 2, y = 185, label = '*'), col = 'black') +
#   geom_text(data = filter(plot, environment == 'island', panel == 'KLE'), mapping = aes(x = 2, y = 195, label = '·'), col = 'black') +
#   geom_text(data = filter(plot, environment == 'short', panel == 'MLE'), mapping = aes(x = 2, y = 185, label = '*'), col = 'black') +
#   geom_text(data = filter(plot, environment == 'long', panel == 'MLE'), mapping = aes(x = 2, y = 185, label = '*'), col = 'black') +
#   geom_text(data = filter(plot, environment == 'island', panel == 'MLE'), mapping = aes(x = 2, y = 185, label = '*'), col = 'black')

ggsave('plots/orc/spr_rrt.png', width=6.5, height=4.5, dpi=600)

p2 + s

# p2 + s +
#   geom_text(data = filter(plot, environment == 'short', panel == 'ENS'), mapping = aes(x = 2, y = 195, label = '·'), col = 'black') +
#   geom_text(data = filter(plot, environment == 'island', panel == 'ENS'), mapping = aes(x = 2, y = 185, label = '*'), col = 'black') +
#   geom_text(data = filter(plot, environment == 'island', panel == 'ENS'), mapping = aes(x = 3, y = 185, label = '*'), col = 'black') +
#   geom_text(data = filter(plot, environment == 'long', panel == 'KLE'), mapping = aes(x = 2, y = 185, label = '*'), col = 'black') +
#   geom_text(data = filter(plot, environment == 'short', panel == 'KLE'), mapping = aes(x = 3, y = 185, label = '*'), col = 'black') +
#   geom_text(data = filter(plot, environment == 'island', panel == 'KLE'), mapping = aes(x = 3, y = 185, label = '*'), col = 'black') +
#   geom_text(data = filter(plot, environment == 'long', panel == 'MLE'), mapping = aes(x = 1, y = 185, label = '*'), col = 'black') +
#   geom_text(data = filter(plot, environment == 'long', panel == 'MLE'), mapping = aes(x = 2, y = 185, label = '*'), col = 'black') +
#   geom_text(data = filter(plot, environment == 'island', panel == 'MLE'), mapping = aes(x = 1, y = 185, label = '*'), col = 'black') +
#   geom_text(data = filter(plot, environment == 'island', panel == 'MLE'), mapping = aes(x = 2, y = 195, label = '·'), col = 'black')

ggsave('plots/src/spr_rrt.png', width=6.5, height=4.5, dpi=600)

#------------------------------------------------------------------------------#
# modeling for residual RTs ----
#------------------------------------------------------------------------------#

spr_crit_clean <- spr_trim

write_csv(spr_crit_clean, 'data/spr_crit_clean.csv')

#------------------------------------------------------------------------------#
# + doall - region 1 ----
#------------------------------------------------------------------------------#

# read in data

spr_crit_clean <- read_csv('data/spr_crit_clean.csv')

# filter data for analysis

md <- spr_crit_clean %>%
  filter(region2 == 1,
         study == '210510_do')

# set class of columns ...

md <- md %>%
  mutate(dependency = as.factor(dependency),
         environment = as.factor(environment),
         group = as.factor(group),
         rrt = as.numeric(rrt)) %>%
  mutate(environment = fct_relevel(environment, 'short', 'long', 'island'))

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

model1 <- read_rds('models/spr_doall_rrt_region1_md1.rds')

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
# + + model 2
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

model2 <- read_rds('models/spr_doall_rrt_region1_md2.rds')

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
# + + model 4 
#------------------------------------------------------------------------------#

# fit model ...

tic()
model4 <- lmer(rrt ~ dependency * environment * group + 
                 (1 + dependency + environment | participant) + 
                 (1 + dependency | item), 
               data = md,
               control = lmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_doall_rrt_region1_md4.rds')
summary(model4)
toc()
beep()

# 11.75 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                      -22.27331    5.95732  122.41333  -3.739 0.000282 ***
# dependency2                      -20.20437    6.68362   44.85961  -3.023 0.004128 ** 
# environment2                       2.11902    7.18402  215.66039   0.295 0.768306    
# environment3                       7.97642    6.41539  891.37897   1.243 0.214075    
# group2                           -30.65197   12.72757  218.27176  -2.408 0.016857 *  
# group3                           -22.50357   12.65648  217.22837  -1.778 0.076798 .  
# dependency2:environment2          -0.05749   12.63585 5778.32169  -0.005 0.996370    
# dependency2:environment3          11.35926   12.57894 5748.48777   0.903 0.366544    
# dependency2:group2                 0.11742   14.82268  217.74771   0.008 0.993687    
# dependency2:group3                 0.22161   14.70748  215.24626   0.015 0.987992    
# environment2:group2                7.70045   17.25350  217.10657   0.446 0.655817    
# environment3:group2                8.59094   15.42631  900.88394   0.557 0.577733    
# environment2:group3                3.43070   17.11826  214.88696   0.200 0.841348    
# environment3:group3                7.63578   15.30879  892.29922   0.499 0.618054    
# dependency2:environment2:group2   28.25507   30.29113 5758.83051   0.933 0.350971    
# dependency2:environment3:group2   84.28112   30.24429 5749.58925   2.787 0.005343 ** 
# dependency2:environment2:group3   21.18052   30.02439 5769.43750   0.705 0.480563    
# dependency2:environment3:group3  112.39878   29.99366 5745.79197   3.747 0.000180 ***

anova(model2, model4, refit = FALSE)

# npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)
# model2   25 87168 87338 -43559    87118                     
# model4   32 87172 87389 -43554    87108 9.9441  7     0.1918

#------------------------------------------------------------------------------#
# + + model 6 (final) ----
#------------------------------------------------------------------------------#

# fit model ...

tic()
model6 <- lmer(rrt ~ dependency * environment * group + 
                 (1 + dependency | participant) + 
                 (1 + dependency + group | item), 
               data = md,
               control = lmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_doall_rrt_region1_md6.rds')
summary(model6)
toc()
beep()

model6 <- read_rds('models/spr_doall_rrt_region1_md6.rds')

# 20.19 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                      -22.33544    6.02869  117.60814  -3.705 0.000324 ***
# dependency2                      -20.25898    6.62276   70.57696  -3.059 0.003138 ** 
# environment2                       3.50220    6.34489 5927.93823   0.552 0.580988    
# environment3                       8.72143    6.33157 5945.09753   1.377 0.168424    
# group2                           -30.71040   12.79927  210.55031  -2.399 0.017294 *  
# group3                           -22.61648   14.04528  132.05722  -1.610 0.109730    
# dependency2:environment2           0.51761   12.66737 5932.64736   0.041 0.967408    
# dependency2:environment3          11.22891   12.61144 5945.35791   0.890 0.373301    
# dependency2:group2                -0.03562   14.82360  215.86928  -0.002 0.998085    
# dependency2:group3                 0.39592   14.71359  213.63638   0.027 0.978558    
# environment2:group2                8.59093   15.18987 5954.09276   0.566 0.571708    
# environment3:group2                9.55488   15.16586 5953.32428   0.630 0.528702    
# environment2:group3                8.51149   15.14173 5942.23896   0.562 0.574055    
# environment3:group3               10.42303   15.13515 5934.93980   0.689 0.491062    
# dependency2:environment2:group2   28.45557   30.37533 5911.14284   0.937 0.348901    
# dependency2:environment3:group2   83.91341   30.31764 5943.46915   2.768 0.005661 ** 
# dependency2:environment2:group3   19.76364   30.10114 5941.65984   0.657 0.511480    
# dependency2:environment3:group3  111.96606   30.07698 5940.44717   3.723 0.000199 ***

anova(model2, model6, refit = FALSE)

# npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)  
# model2   25 87168 87338 -43559    87118                       
# model6   32 87166 87383 -43551    87102 16.038  7    0.02478 *

# post-hoc tests ...

pairwise <- model6 %>%
  emmeans(~ dependency * environment * group) %>%
  contrast('pairwise', simple = 'dependency', combine = TRUE) %>%
  summary(by = NULL, adjust = 'mvt')
pairwise

pairwise <- model6 %>%
  emmeans(~ dependency * environment * group) %>%
  contrast('pairwise', simple = 'each', combine = TRUE) %>%
  summary(by = NULL, adjust = 'mvt')
pairwise

# tables ...

summary(model6)$coefficients %>%
  kbl(digits = c(2, 2, 2, 3)) %>%
  remove_column(4)

pairwise %>%
  kbl(digits = c(2, 2, 2, 3)) %>%
  remove_column(7)

# plot ...

plot <- plot(emmeans(model6, ~ dependency * environment * group), comparisons = TRUE, adjust = 'mvt', plotit = FALSE)

ggplot(plot, aes(x = environment, y = the.emmean, group = dependency, col = dependency, shape = dependency)) +
  geom_hline(yintercept = 0) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0, lwd = 5, alpha = .5, position = position_dodge(width = .4)) +
  geom_errorbar(aes(ymin = case_when(is.na(lcmpl)~the.emmean, TRUE ~ lcmpl), ymax = case_when(is.na(rcmpl)~the.emmean, TRUE ~ rcmpl)), width = 0, lwd = 1, position = position_dodge(width = .4)) +
  geom_line(lwd = 1, position = position_dodge(width = .4)) +
  geom_point(size = 3, position = position_dodge(width = .4)) +
  theme_classic() +
  scale_y_continuous(name="RRT (ms)", breaks = seq(-100, 75, by = 25), limits = c(-110, 85)) +
  scale_colour_manual(name="dependency", values=c('#648fff', '#ffb000'), labels=c("gap", "resumption")) +
  scale_shape_manual(name="dependency", values=c(16, 15), labels=c("gap", "resumption")) +
  theme(text = element_text(size = 12), 
        legend.margin=margin(0, 0, 0, -5)) +
  facet_wrap(~group, labeller = as_labeller(c(`english` = 'ENS', `korean` = 'KLE', `mandarin` = 'MLE')))

ggsave("plots/orc/spr_rrt_region1_emmeans.png", width=6.5, height=2, dpi=600)

#------------------------------------------------------------------------------#
# + + model 7
#------------------------------------------------------------------------------#

# fit model ...

tic()
model7 <- lmer(rrt ~ dependency * environment * group + 
                 (1 + dependency + environment | participant) + 
                 (1 + dependency + group | item), 
               data = md,
               control = lmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_doall_rrt_region1_md7.rds')
summary(model7)
toc()
beep()

# 57.5 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                      -22.30030    6.02634  118.04770  -3.700 0.000328 ***
# dependency2                      -20.29828    6.73532   45.88280  -3.014 0.004193 ** 
# environment2                       3.64175    7.12934  218.24785   0.511 0.609999    
# environment3                       8.77985    6.41616  964.56842   1.368 0.171507    
# group2                           -30.67661   12.81140  208.32721  -2.394 0.017530 *  
# group3                           -22.48016   14.01547  131.88038  -1.604 0.111116    
# dependency2:environment2           0.47468   12.60320 5741.50920   0.038 0.969957    
# dependency2:environment3          11.28829   12.54404 5724.34473   0.900 0.368215    
# dependency2:group2                -0.06092   14.82250  216.98408  -0.004 0.996724    
# dependency2:group3                 0.10288   14.71110  214.68511   0.007 0.994427    
# environment2:group2                8.30201   17.06569  217.61706   0.486 0.627121    
# environment3:group2                9.39448   15.36616  964.38714   0.611 0.541096    
# environment2:group3                7.87359   17.00727  218.46046   0.463 0.643858    
# environment3:group3               10.10557   15.33651  969.73027   0.659 0.510102    
# dependency2:environment2:group2   28.40326   30.21900 5702.24450   0.940 0.347301    
# dependency2:environment3:group2   83.90674   30.15694 5727.05547   2.782 0.005415 ** 
# dependency2:environment2:group3   20.30941   29.94646 5737.10809   0.678 0.497678    
# dependency2:environment3:group3  112.21615   29.91820 5726.07257   3.751 0.000178 ***

# compare models ...

anova(model6, model7, refit = FALSE)

# npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)
# model6   32 87166 87383 -43551    87102                     
# model7   39 87171 87435 -43547    87093 9.1865  7     0.2395

#------------------------------------------------------------------------------#
# + + model 8
#------------------------------------------------------------------------------#

# fit model ...

tic()
model8 <- lmer(rrt ~ dependency * environment * group + 
                 (1 + dependency | participant) + 
                 (1 + dependency + environment + group | item), 
               data = md,
               control = lmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_doall_rrt_region1_md8.rds')
summary(model8)
toc()
beep()

# 102.08 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)                      -22.3409     6.0448  117.7827  -3.696 0.000334 ***
# dependency2                      -19.7951     6.6713   66.3354  -2.967 0.004176 ** 
# environment2                       3.3847     7.7985   34.6282   0.434 0.666968    
# environment3                       8.9573     6.5850  115.2728   1.360 0.176403    
# group2                           -30.9029    12.8034  210.3689  -2.414 0.016650 *  
# group3                           -22.7150    14.0105  134.8037  -1.621 0.107293    
# dependency2:environment2           1.1574    12.6428 5835.7043   0.092 0.927061    
# dependency2:environment3          11.4053    12.5960 5895.3493   0.905 0.365253    
# dependency2:group2                 0.1399    14.7857  215.2373   0.009 0.992460    
# dependency2:group3                 0.2153    14.6761  213.0301   0.015 0.988309    
# environment2:group2                7.7246    15.1644 5935.4009   0.509 0.610499    
# environment3:group2                9.0324    15.1388 5923.7328   0.597 0.550770    
# environment2:group3                8.1102    15.0988 5857.4071   0.537 0.591189    
# environment3:group3               11.2934    15.1260 5904.0067   0.747 0.455321    
# dependency2:environment2:group2   28.5497    30.3190 5890.3845   0.942 0.346413    
# dependency2:environment3:group2   83.8724    30.2602 5918.2667   2.772 0.005594 ** 
# dependency2:environment2:group3   19.9059    30.0476 5916.2259   0.662 0.507691    
# dependency2:environment3:group3  112.8895    30.0247 5915.4994   3.760 0.000172 ***

# compare models ...

anova(model6, model8, refit = FALSE)

# npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)
# model6   32 87166 87383 -43551    87102                     
# model8   43 87179 87470 -43546    87093 9.7023 11     0.5573

#------------------------------------------------------------------------------#
# + + model 9
#------------------------------------------------------------------------------#

# fit model ...

tic()
model9 <- lmer(rrt ~ dependency * environment * group + 
                 (1 + dependency | participant) + 
                 (1 + dependency * group | item), 
               data = md,
               control = lmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_doall_rrt_region1_md9.rds')
summary(model9)
toc()
beep()

# 116.18 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)                      -22.2688     6.0464  118.6968  -3.683 0.000348 ***
# dependency2                      -20.4751     7.3592   49.2578  -2.782 0.007632 ** 
# environment2                       3.4020     6.3337 5901.5785   0.537 0.591198    
# environment3                       8.7533     6.3171 5927.3382   1.386 0.165909    
# group2                           -30.3764    12.8636  205.6623  -2.361 0.019139 *  
# group3                           -22.5484    14.0620  131.6989  -1.603 0.111220    
# dependency2:environment2           0.9292    12.6669 5883.4660   0.073 0.941523    
# dependency2:environment3          10.5722    12.6133 5881.9970   0.838 0.401961    
# dependency2:group2                -0.2481    16.8966   57.1078  -0.015 0.988334    
# dependency2:group3                 0.6713    15.8275   81.9044   0.042 0.966271    
# environment2:group2                7.1850    15.1872 5847.4650   0.473 0.636161    
# environment3:group2                9.6447    15.1254 5921.5300   0.638 0.523727    
# environment2:group3                8.3668    15.1026 5914.9613   0.554 0.579604    
# environment3:group3               10.3548    15.1000 5917.4898   0.686 0.492898    
# dependency2:environment2:group2   28.7381    30.3100 5882.3199   0.948 0.343097    
# dependency2:environment3:group2   83.8021    30.2444 5913.0977   2.771 0.005609 ** 
# dependency2:environment2:group3   19.6360    30.1284 5830.4441   0.652 0.514591    
# dependency2:environment3:group3  110.4031    30.1106 5822.3346   3.667 0.000248 ***

anova(model6, model9, refit = FALSE)

# npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)
# model6   32 87166 87383 -43551    87102                     
# model9   43 87177 87468 -43546    87091 11.062 11     0.4381

#------------------------------------------------------------------------------#
# + doall - region 2 ----
#------------------------------------------------------------------------------#

# filter data for analysis

md <- spr_crit_clean %>%
  filter(region2 == 2,
         study == '210510_do')

# set class of columns ...

md <- md %>%
  mutate(dependency = as.factor(dependency),
         environment = as.factor(environment),
         group = as.factor(group),
         rrt = as.numeric(rrt)) %>%
  mutate(environment = fct_relevel(environment, 'short', 'long', 'island'))

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

model1 <- read_rds('models/spr_doall_rrt_region2_md1.rds')

# 1.86 sec elapsed
# no warnings
# Fixed effects:
#   Estimate Std. Error       df t value Pr(>|t|)    
# (Intercept)                      -19.446      6.739  210.335  -2.886  0.00431 ** 
#  dependency2                      -30.991      3.967 6097.384  -7.811 6.62e-15 ***
# environment2                       5.116      4.851 6093.508   1.055  0.29167    
# environment3                      13.896      4.855 6093.301   2.862  0.00422 ** 
# group2                           -68.419     16.006  220.376  -4.275 2.85e-05 ***
# group3                           -74.490     15.922  219.571  -4.678 5.05e-06 ***
# dependency2:environment2         -30.423      9.713 6044.043  -3.132  0.00174 ** 
# dependency2:environment3         -31.274      9.711 6083.990  -3.220  0.00129 ** 
# dependency2:group2               -43.602      9.568 6075.866  -4.557 5.29e-06 ***
# dependency2:group3               -55.437      9.437 6081.473  -5.874 4.47e-09 ***
# environment2:group2               22.612     11.702 6097.071   1.932  0.05337 .  
# environment3:group2               16.350     11.713 6095.064   1.396  0.16281    
# environment2:group3               24.202     11.547 6098.566   2.096  0.03613 *  
# environment3:group3               20.645     11.559 6098.439   1.786  0.07413 .  
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

model2 <- read_rds('models/spr_doall_rrt_region2_md2.rds')

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
  write_rds('models/spr_doall_rrt_region2_md3.rds')
summary(model3)
toc()
beep()

model3 <- read_rds('models/spr_doall_rrt_region2_md3.rds')

# 10.68 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error       df t value Pr(>|t|)    
# (Intercept)                      -19.527      6.780  213.789  -2.880 0.004381 ** 
# dependency2                      -30.941      4.988   90.351  -6.204 1.63e-08 ***
# environment2                       4.969      4.928  751.354   1.008 0.313619    
# environment3                      13.849      5.512  301.286   2.513 0.012508 *  
# group2                           -68.729     16.029  220.160  -4.288 2.70e-05 ***
# group3                           -74.572     15.946  219.384  -4.677 5.10e-06 ***
# dependency2:environment2         -30.497      9.555 5657.588  -3.192 0.001422 ** 
# dependency2:environment3         -31.375      9.550 5681.232  -3.285 0.001024 ** 
# dependency2:group2               -43.717     11.563  234.723  -3.781 0.000198 ***
# dependency2:group3               -55.300     11.438  229.044  -4.835 2.44e-06 ***
# environment2:group2               22.092     11.879  766.302   1.860 0.063308 .  
# environment3:group2               15.820     13.280  306.793   1.191 0.234472    
# environment2:group3               24.272     11.724  743.984   2.070 0.038773 *  
# environment3:group3               21.417     13.126  299.631   1.632 0.103793    
# dependency2:environment2:group2  -30.178     23.016 5668.306  -1.311 0.189843    
# dependency2:environment3:group2   29.580     23.044 5685.275   1.284 0.199315    
# dependency2:environment2:group3  -11.119     22.701 5665.971  -0.490 0.624288    
# dependency2:environment3:group3   26.598     22.721 5669.404   1.171 0.241801

# compare models ...

anova(model1, model2, model3, refit = FALSE)

# npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)    
# model1   21 82435 82576 -41196    82393                         
# model2   25 82425 82594 -41187    82375 17.972  4  0.0012498 ** 
# model3   32 82411 82627 -41174    82347 27.334  7  0.0002902 ***

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
  write_rds('models/spr_doall_rrt_region2_md4.rds')
summary(model4)
toc()
beep()

model4 <- read_rds('models/spr_doall_rrt_region2_md4.rds')

# 786.3 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error      df t value Pr(>|t|)    
# (Intercept)                      -19.535      6.814 217.327  -2.867 0.004555 ** 
# dependency2                      -30.792      5.053  86.605  -6.094 2.96e-08 ***
# environment2                       4.800      5.517  41.330   0.870 0.389338    
# environment3                      14.052      5.651  85.852   2.487 0.014835 *  
# group2                           -68.753     16.032 220.161  -4.288 2.69e-05 ***
# group3                           -74.458     15.949 219.417  -4.668 5.29e-06 ***
# dependency2:environment2         -29.704     11.085  62.015  -2.680 0.009428 ** 
# dependency2:environment3         -31.623     11.801  83.305  -2.680 0.008879 ** 
# dependency2:group2               -43.571     11.544 233.876  -3.774 0.000203 ***
# dependency2:group3               -55.113     11.422 228.527  -4.825 2.56e-06 ***
# environment2:group2               22.388     12.338 386.246   1.815 0.070369 .  
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
  write_rds('models/spr_doall_rrt_region2_md5.rds')
summary(model5)
toc()
beep()

# took too long; run overnight

#------------------------------------------------------------------------------#
# + + model 6
#------------------------------------------------------------------------------#

# fit model ...

tic()
model6 <- lmer(rrt ~ dependency * environment * group + 
                 (1 + dependency + environment | participant) + 
                 (1 + dependency + environment + group | item), 
               data = md,
               control = lmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_doall_rrt_region2_md6.rds')
summary(model6)
toc()
beep()

# 1378.64 sec elapsed
# boundary (singular) fit: see help('isSingular')
# Warning message:
#   Model failed to converge with 2 negative eigenvalues: -2.9e+01 -2.9e+02 
# Fixed effects:
#   Estimate Std. Error       df t value Pr(>|t|)    
# (Intercept)                      -19.486      6.853  216.286  -2.843 0.004892 ** 
# dependency2                      -30.838      5.012  102.414  -6.153 1.49e-08 ***
# environment2                       4.739      5.294   35.756   0.895 0.376657    
# environment3                      14.328      5.620  160.939   2.550 0.011721 *  
# group2                           -68.595     16.480  217.664  -4.162 4.54e-05 ***
# group3                           -74.475     16.183  219.410  -4.602 7.08e-06 ***
# dependency2:environment2         -30.852      9.519 5544.586  -3.241 0.001197 ** 
# dependency2:environment3         -31.540      9.513 5619.082  -3.316 0.000921 ***
# dependency2:group2               -43.637     11.586  233.004  -3.766 0.000210 ***
# dependency2:group3               -55.568     11.457  227.231  -4.850 2.29e-06 ***
# environment2:group2               21.636     11.833  775.287   1.828 0.067874 .  
# environment3:group2               15.925     13.285  300.363   1.199 0.231580    
# environment2:group3               24.220     11.722  758.134   2.066 0.039146 *  
# environment3:group3               22.749     13.158  294.874   1.729 0.084876 .  
# dependency2:environment2:group2  -31.981     23.018 5532.792  -1.389 0.164770    
# dependency2:environment3:group2   29.680     22.951 5633.159   1.293 0.196004    
# dependency2:environment2:group3  -10.794     22.604 5604.471  -0.478 0.633011    
# dependency2:environment3:group3   26.805     22.631 5622.948   1.184 0.236283 

anova(model1, model2, model3, model6, refit = FALSE)

# npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)    
# model1   21 82435 82576 -41196    82393                         
# model2   25 82425 82594 -41187    82375 17.972  4  0.0012498 ** 
# model3   32 82411 82627 -41174    82347 27.334  7  0.0002902 ***
# model6   50 82431 82768 -41165    82331 16.517 18  0.5565197  

#------------------------------------------------------------------------------#
# + + model 7 
#------------------------------------------------------------------------------#

# fit model ...

tic()
model7 <- lmer(rrt ~ dependency * environment * group + 
                 (1 + dependency | participant) + 
                 (1 + dependency + group | item), 
               data = md,
               control = lmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_doall_rrt_region2_md7.rds')
summary(model7)
toc()
beep()

model7 <- read_rds('models/spr_doall_rrt_region2_md7.rds')

# 21.94 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error       df t value Pr(>|t|)    
# (Intercept)                      -19.506      6.855  216.132  -2.845 0.004861 ** 
#   dependency2                      -31.102      4.960   99.163  -6.270 9.45e-09 ***
#   environment2                       4.990      4.816 5844.350   1.036 0.300143    
# environment3                      14.284      4.818 5849.567   2.965 0.003044 ** 
#   group2                           -68.471     16.460  217.389  -4.160 4.58e-05 ***
#   group3                           -74.440     16.186  219.515  -4.599 7.17e-06 ***
#   dependency2:environment2         -30.846      9.610 5820.025  -3.210 0.001336 ** 
#   dependency2:environment3         -31.297      9.600 5849.201  -3.260 0.001120 ** 
#   dependency2:group2               -43.812     11.460  222.320  -3.823 0.000171 ***
#   dependency2:group3               -55.387     11.332  216.575  -4.888 1.98e-06 ***
#   environment2:group2               21.940     11.572 5855.365   1.896 0.058021 .  
# environment3:group2               15.871     11.584 5854.401   1.370 0.170701    
# environment2:group3               24.239     11.459 5776.200   2.115 0.034449 *  
#   environment3:group3               22.331     11.473 5760.980   1.946 0.051666 .  
# dependency2:environment2:group2  -31.425     23.229 5758.703  -1.353 0.176153    
# dependency2:environment3:group2   29.589     23.162 5845.846   1.277 0.201481    
# dependency2:environment2:group3  -11.013     22.816 5833.438  -0.483 0.629337    
# dependency2:environment3:group3   27.393     22.844 5841.920   1.199 0.230517 

anova(model1, model2, model3, model6, model7, refit = FALSE)

#------------------------------------------------------------------------------#
# + + model 8 
#------------------------------------------------------------------------------#

# fit model ...

tic()
model8 <- lmer(rrt ~ dependency * environment * group + 
                 (1 + dependency + environment | participant) + 
                 (1 + dependency + group | item), 
               data = md,
               control = lmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_doall_rrt_region2_md8.rds')
summary(model8)
toc()
beep()

model8 <- read_rds('models/spr_doall_rrt_region2_md8.rds')

# 48.49 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error       df t value Pr(>|t|)    
# (Intercept)                      -19.481      6.857  216.228  -2.841  0.00493 ** 
# dependency2                      -31.039      4.996  102.572  -6.213 1.13e-08 ***
# environment2                       4.808      5.056  456.146   0.951  0.34211    
# environment3                      14.407      5.360  306.205   2.688  0.00759 ** 
# group2                           -68.627     16.483  217.857  -4.163 4.52e-05 ***
# group3                           -74.467     16.196  219.590  -4.598 7.20e-06 ***
# dependency2:environment2         -30.939      9.515 5631.597  -3.252  0.00115 ** 
# dependency2:environment3         -31.213      9.502 5651.793  -3.285  0.00103 ** 
# dependency2:group2               -43.743     11.576  232.569  -3.779  0.00020 ***
# dependency2:group3               -55.500     11.447  226.695  -4.849 2.31e-06 ***
# environment2:group2               21.347     12.145  460.127   1.758  0.07946 .  
# environment3:group2               16.001     12.877  309.246   1.243  0.21495    
# environment2:group3               24.137     12.031  451.799   2.006  0.04543 *  
# environment3:group3               22.932     12.764  304.411   1.797  0.07338 .  
# dependency2:environment2:group2  -32.373     23.001 5570.358  -1.407  0.15934    
# dependency2:environment3:group2   29.895     22.929 5654.948   1.304  0.19235    
# dependency2:environment2:group3  -10.752     22.584 5634.385  -0.476  0.63402    
# dependency2:environment3:group3   26.978     22.609 5641.709   1.193  0.23283  

anova(model3, model8, refit = FALSE)

# npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)   
# model3   32 82411 82627 -41174    82347                        
# model8   39 82406 82670 -41164    82328 18.904  7   0.008494 **

#------------------------------------------------------------------------------#
# + + model 9 (final) ----
#------------------------------------------------------------------------------#

# fit model ...

tic()
model9 <- lmer(rrt ~ dependency * environment * group + 
                 (1 + dependency * environment | participant) + 
                 (1 + dependency + group | item), 
               data = md,
               control = lmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_doall_rrt_region2_md9.rds')
summary(model9)
toc()
beep()

model9 <- read_rds('models/spr_doall_rrt_region2_md9.rds')

# 191.29 sec elapsed
# boundary (singular) fit: see help('isSingular')
# Warning message:
#   Model failed to converge with 1 negative eigenvalue: -3.9e+01 # but did it really fail to converge?
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
# Estimate Std. Error      df t value Pr(>|t|)    
# (Intercept)                      -19.533      6.842 216.524  -2.855 0.004726 ** 
# dependency2                      -30.966      4.981 109.495  -6.217 9.42e-09 ***
# environment2                       4.672      5.125 389.177   0.912 0.362542    
# environment3                      14.147      5.362 314.761   2.638 0.008748 ** 
# group2                           -68.616     16.451 218.507  -4.171 4.38e-05 ***
# group3                           -74.408     16.206 219.740  -4.592 7.40e-06 ***
# dependency2:environment2         -30.518      9.976 320.728  -3.059 0.002406 ** 
# dependency2:environment3         -31.115     11.037 279.021  -2.819 0.005161 ** 
# dependency2:group2               -43.483     11.574 231.843  -3.757 0.000218 ***
# dependency2:group3               -55.470     11.446 226.171  -4.846 2.34e-06 ***
# environment2:group2               21.573     12.304 392.890   1.753 0.080334 .  
# environment3:group2               15.783     12.877 317.713   1.226 0.221236    
# environment2:group3               23.697     12.201 385.225   1.942 0.052849 .  
# environment3:group3               22.488     12.770 312.858   1.761 0.079220 .  
# dependency2:environment2:group2  -32.409     24.090 328.920  -1.345 0.179441    
# dependency2:environment3:group2   29.243     26.586 284.076   1.100 0.272278    
# dependency2:environment2:group3  -10.829     23.696 316.112  -0.457 0.647984    
# dependency2:environment3:group3   27.156     26.273 277.430   1.034 0.302216  

anova(model8, model9, refit = FALSE)

# npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)    
# model8   39 82406 82670 -41164    82328                         
# model9   50 82380 82718 -41140    82280 48.379 11  1.222e-06 ***

# post-hoc tests ...

pairwise <- model9 %>%
  emmeans(~ dependency * environment * group) %>%
  contrast('pairwise', simple = 'dependency', combine = TRUE) %>%
  summary(by = NULL, adjust = 'holm')
pairwise

# pairwise <- model9 %>%
#   emmeans(~ dependency * environment * group) %>%
#   contrast('pairwise', simple = 'each', combine = TRUE) %>%
#   summary(by = NULL, adjust = 'holm')
# pairwise

# tables ...

summary(model9)$coefficients %>%
  kbl(digits = c(2, 2, 2, 2, 3)) %>%
  remove_column(4)

pairwise %>%
  kbl(digits = c(2, 2, 2, 2, 2, 2, 2, 2, 3)) %>%
  remove_column(7)

# plot ...

plot <- plot(emmeans(model9, ~ dependency * environment * group), comparisons = TRUE, adjust = 'mvt', plotit = FALSE)

ggplot(plot, aes(x = environment, y = the.emmean, group = dependency, col = dependency, shape = dependency)) +
  geom_hline(yintercept = 0) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0, lwd = 5, alpha = .5, position = position_dodge(width = .4)) +
  geom_errorbar(aes(ymin = case_when(is.na(lcmpl)~the.emmean, TRUE ~ lcmpl), ymax = case_when(is.na(rcmpl)~the.emmean, TRUE ~ rcmpl)), width = 0, lwd = 1, position = position_dodge(width = .4)) +
  geom_line(lwd = 1, position = position_dodge(width = .4)) +
  geom_point(size = 3, position = position_dodge(width = .4)) +
  theme_classic() +
  scale_y_continuous(name="RRT (ms)", breaks = seq(-100, 100, by = 25), limits = c(-110, 85)) +
  scale_colour_manual(name="dependency", values=c('#648fff', '#ffb000'), labels=c("gap", "resumption")) +
  scale_shape_manual(name="dependency", values=c(16, 15), labels=c("gap", "resumption")) +
  theme(text = element_text(size = 12), 
        legend.margin=margin(0, 0, 0, -5)) +
  facet_wrap(~group, labeller = as_labeller(c(`english` = 'ENS', `korean` = 'KLE', `mandarin` = 'MLE')))

ggsave("plots/orc/spr_rrt_region2_emmeans.png", width=6.5, height=2, dpi=600)

#------------------------------------------------------------------------------#
# + + model 10
#------------------------------------------------------------------------------#

# fit model ...

tic()
model10 <- lmer(rrt ~ dependency * environment * group + 
                 (1 + dependency * environment | participant) + 
                 (1 + dependency * group | item), 
               data = md,
               control = lmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_doall_rrt_region2_md10.rds')
summary(model10)
toc()
beep()

model10 <- read_rds('models/spr_doall_rrt_region2_md10.rds')

# 855.9 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error      df t value Pr(>|t|)    
# (Intercept)                      -19.527      6.839 217.573  -2.855  0.00472 ** 
# dependency2                      -31.027      5.310  68.289  -5.843 1.58e-07 ***
# environment2                       4.819      5.104 392.891   0.944  0.34562    
# environment3                      14.366      5.362 311.051   2.679  0.00777 ** 
# group2                           -68.471     16.503 220.080  -4.149 4.77e-05 ***
# group3                           -74.545     16.185 219.581  -4.606 6.96e-06 ***
# dependency2:environment2         -31.195      9.978 321.051  -3.126  0.00193 ** 
# dependency2:environment3         -32.422     11.028 282.862  -2.940  0.00355 ** 
# dependency2:group2               -43.914     12.987  70.605  -3.382  0.00118 ** 
# dependency2:group3               -55.420     12.951  59.417  -4.279 6.92e-05 ***
# environment2:group2               21.800     12.267 397.763   1.777  0.07631 .  
# environment3:group2               16.110     12.871 313.683   1.252  0.21165    
# environment2:group3               24.028     12.148 389.113   1.978  0.04865 *  
# environment3:group3               22.668     12.772 309.600   1.775  0.07692 .  
# dependency2:environment2:group2  -30.820     24.021 326.045  -1.283  0.20039    
# dependency2:environment3:group2   28.990     26.493 285.442   1.094  0.27477    
# dependency2:environment2:group3  -13.831     23.732 317.316  -0.583  0.56044    
# dependency2:environment3:group3   23.732     26.277 282.000   0.903  0.36722 

anova(model9, model10, refit = FALSE)

# npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)
# model9    50 82380 82718 -41140    82280                     
# model10   61 82386 82798 -41132    82264 15.593 11     0.1569

#------------------------------------------------------------------------------#
# + + model 11
#------------------------------------------------------------------------------#

# fit model ...

tic()
model11 <- lmer(rrt ~ dependency * environment * group + 
                  (1 + dependency * environment | participant) + 
                  (1 + dependency + environment + group | item), 
                data = md,
                control = lmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_doall_rrt_region2_md11.rds')
summary(model11)
toc()
beep()

# 1093.7 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error      df t value Pr(>|t|)    
# (Intercept)                      -19.529      6.840 216.589  -2.855 0.004719 ** 
# dependency2                      -30.760      4.989 106.480  -6.165 1.28e-08 ***
# environment2                       4.648      5.495  39.982   0.846 0.402651    
# environment3                      14.108      5.417 192.240   2.605 0.009920 ** 
# group2                           -68.565     16.450 218.436  -4.168 4.43e-05 ***
# group3                           -74.396     16.199 219.749  -4.593 7.37e-06 ***
# dependency2:environment2         -30.381      9.970 321.318  -3.047 0.002500 ** 
# dependency2:environment3         -31.354     11.036 278.141  -2.841 0.004829 ** 
# dependency2:group2               -43.367     11.565 231.779  -3.750 0.000223 ***
# dependency2:group3               -55.593     11.438 226.213  -4.860 2.19e-06 ***
# environment2:group2               21.597     12.306 389.931   1.755 0.080047 .  
# environment3:group2               15.785     12.873 316.534   1.226 0.221037    
# environment2:group3               23.684     12.207 382.462   1.940 0.053086 .  
# environment3:group3               22.319     12.755 310.726   1.750 0.081141 .  
# dependency2:environment2:group2  -31.993     24.078 329.817  -1.329 0.184852    
# dependency2:environment3:group2   29.172     26.582 283.000   1.097 0.273379    
# dependency2:environment2:group3  -10.803     23.686 316.925  -0.456 0.648647    
# dependency2:environment3:group3   27.167     26.268 276.356   1.034 0.301931 

anova(model9, model11, refit = FALSE)

# npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)
# model9    50 82380 82718 -41140    82280                     
# model11   61 82400 82812 -41139    82278 1.5788 11     0.9995

#------------------------------------------------------------------------------#
# + doall - region 3 ----
#------------------------------------------------------------------------------#

# filter data for analysis

md <- spr_crit_clean %>%
  filter(region2 == 3,
         study == '210510_do')

# set class of columns ...

md <- md %>%
  mutate(dependency = as.factor(dependency),
         environment = as.factor(environment),
         group = as.factor(group),
         rrt = as.numeric(rrt)) %>%
  mutate(environment = fct_relevel(environment, 'short', 'long', 'island'))

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
  write_rds('models/spr_doall_rrt_region3_md1.rds')
summary(model1)
toc()
beep()

# 1.66 sec elapsed
# no warnings
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                      -17.96669    6.95651  167.29061  -2.583 0.010658 *  
# dependency2                      -12.90109    4.20085 5821.66445  -3.071 0.002143 ** 
# environment2                     -11.39543    5.14979 5826.60051  -2.213 0.026950 *  
# environment3                       3.55052    5.14878 5823.88475   0.690 0.490483    
# group2                           -67.51487   14.82302  220.98039  -4.555 8.67e-06 ***
# group3                           -55.18508   14.68992  217.17601  -3.757 0.000221 ***
# dependency2:environment2           6.99024   10.32401 5837.98272   0.677 0.498379    
# dependency2:environment3          -0.02287   10.29104 5818.19175  -0.002 0.998227    
# dependency2:group2               -33.73824   10.20178 5818.44876  -3.307 0.000948 ***
# dependency2:group3               -25.42901    9.86670 5814.96074  -2.577 0.009983 ** 
# environment2:group2              -12.08862   12.48534 5824.81932  -0.968 0.332972    
# environment3:group2                4.29578   12.50524 5824.50759   0.344 0.731221    
# environment2:group3               -8.37503   12.11960 5829.05326  -0.691 0.489573    
# environment3:group3               -5.27878   12.10482 5827.65790  -0.436 0.662788    
# dependency2:environment2:group2    2.32425   24.94538 5815.51617   0.093 0.925769    
# dependency2:environment3:group2   23.33136   25.00058 5820.22038   0.933 0.350738    
# dependency2:environment2:group3   13.46667   24.19955 5819.48786   0.556 0.577901    
# dependency2:environment3:group3   27.21359   24.16396 5813.31036   1.126 0.260125 

#------------------------------------------------------------------------------#
# + + model 2
#------------------------------------------------------------------------------#

# fit model ...

tic()
model2 <- lmer(rrt ~ dependency * environment * group + 
                 (1 + dependency | participant) + 
                 (1 | item), 
               data = md,
               control = lmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_doall_rrt_region3_md2.rds')
summary(model2)
toc()
beep()

# 1.38 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                     -1.798e+01  6.957e+00  1.675e+02  -2.584 0.010630 *  
# dependency2                     -1.280e+01  4.210e+00  4.620e+03  -3.040 0.002375 ** 
# environment2                    -1.138e+01  5.149e+00  5.826e+03  -2.211 0.027102 *  
# environment3                     3.571e+00  5.148e+00  5.823e+03   0.694 0.487933    
# group2                          -6.749e+01  1.483e+01  2.209e+02  -4.551  8.8e-06 ***
# group3                          -5.520e+01  1.470e+01  2.171e+02  -3.756 0.000222 ***
# dependency2:environment2         6.934e+00  1.032e+01  5.838e+03   0.672 0.501760    
# dependency2:environment3        -5.947e-03  1.029e+01  5.818e+03  -0.001 0.999539    
# dependency2:group2              -3.369e+01  1.022e+01  4.681e+03  -3.296 0.000988 ***
# dependency2:group3              -2.533e+01  9.889e+00  4.528e+03  -2.561 0.010461 *  
# environment2:group2             -1.206e+01  1.248e+01  5.824e+03  -0.966 0.333995    
# environment3:group2              4.302e+00  1.250e+01  5.824e+03   0.344 0.730799    
# environment2:group3             -8.365e+00  1.212e+01  5.829e+03  -0.690 0.490047    
# environment3:group3             -5.297e+00  1.210e+01  5.827e+03  -0.438 0.661647    
# dependency2:environment2:group2  2.312e+00  2.494e+01  5.815e+03   0.093 0.926135    
# dependency2:environment3:group2  2.336e+01  2.500e+01  5.820e+03   0.934 0.350093    
# dependency2:environment2:group3  1.344e+01  2.420e+01  5.819e+03   0.556 0.578491    
# dependency2:environment3:group3  2.718e+01  2.416e+01  5.813e+03   1.125 0.260723

anova(model1, model2, refit = FALSE)

# npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)
# model1   21 79401 79542 -39680    79359                     
# model2   23 79405 79559 -39679    79359 0.9138  2     0.6332

#------------------------------------------------------------------------------#
# + + model 3
#------------------------------------------------------------------------------#

# fit model ...

tic()
model3 <- lmer(rrt ~ dependency * environment * group + 
                 (1 + environment | participant) + 
                 (1 | item), 
               data = md,
               control = lmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_doall_rrt_region3_md3.rds')
summary(model3)
toc()
beep()

# 2.38 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                     -1.789e+01  6.952e+00  1.671e+02  -2.573 0.010963 *  
# dependency2                     -1.286e+01  4.192e+00  5.644e+03  -3.067 0.002172 ** 
# environment2                    -1.147e+01  5.280e+00  4.601e+02  -2.172 0.030369 *  
# environment3                     3.752e+00  5.364e+00  2.463e+02   0.699 0.484972    
# group2                          -6.749e+01  1.481e+01  2.210e+02  -4.559 8.53e-06 ***
# group3                          -5.522e+01  1.467e+01  2.172e+02  -3.763 0.000216 ***
# dependency2:environment2         6.936e+00  1.030e+01  5.667e+03   0.673 0.500866    
# dependency2:environment3        -7.197e-03  1.027e+01  5.655e+03  -0.001 0.999441    
# dependency2:group2              -3.376e+01  1.018e+01  5.648e+03  -3.316 0.000919 ***
# dependency2:group3              -2.545e+01  9.846e+00  5.628e+03  -2.585 0.009765 ** 
# environment2:group2             -1.216e+01  1.279e+01  4.738e+02  -0.950 0.342357    
# environment3:group2              4.260e+00  1.302e+01  2.547e+02   0.327 0.743726    
# environment2:group3             -8.527e+00  1.243e+01  4.439e+02  -0.686 0.493143    
# environment3:group3             -5.219e+00  1.262e+01  2.368e+02  -0.413 0.679652    
# dependency2:environment2:group2  2.086e+00  2.490e+01  5.655e+03   0.084 0.933238    
# dependency2:environment3:group2  2.311e+01  2.495e+01  5.669e+03   0.926 0.354293    
# dependency2:environment2:group3  1.351e+01  2.415e+01  5.635e+03   0.559 0.575923    
# dependency2:environment3:group3  2.710e+01  2.411e+01  5.633e+03   1.124 0.261094 

anova(model1, model3, refit = FALSE)

# npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)
# model1   21 79401 79542 -39680    79359                     
# model3   26 79405 79579 -39676    79353 6.7489  5       0.24

#------------------------------------------------------------------------------#
# + + model 4
#------------------------------------------------------------------------------#

# fit model ...

tic()
model4 <- lmer(rrt ~ dependency * environment * group + 
                 (1 | participant) + 
                 (1 + dependency | item), 
               data = md,
               control = lmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_doall_rrt_region3_md4.rds')
summary(model4)
toc()
beep()

# 1.45 sec elapsed
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                      -17.96817    6.95729  167.23098  -2.583 0.010661 *  
# dependency2                      -12.89983    4.21126   28.67833  -3.063 0.004728 ** 
# environment2                     -11.39854    5.14985 5750.57821  -2.213 0.026911 *  
# environment3                       3.54651    5.14876 5807.91892   0.689 0.490971    
# group2                           -67.51455   14.82333  220.98114  -4.555 8.67e-06 ***
# group3                           -55.18681   14.69025  217.17767  -3.757 0.000221 ***
# dependency2:environment2           7.00347   10.32401 5837.80965   0.678 0.497566    
# dependency2:environment3          -0.01124   10.29107 5817.72669  -0.001 0.999128    
# dependency2:group2               -33.72515   10.20164 5796.15470  -3.306 0.000953 ***
# dependency2:group3               -25.43155    9.86658 5790.37907  -2.578 0.009975 ** 
# environment2:group2              -12.07239   12.48519 5804.01389  -0.967 0.333616    
# environment3:group2                4.30535   12.50513 5804.88193   0.344 0.730643    
# environment2:group3               -8.35941   12.11959 5825.99932  -0.690 0.490383    
# environment3:group3               -5.25045   12.10473 5811.00262  -0.434 0.664485    
# dependency2:environment2:group2    2.28679   24.94519 5812.82230   0.092 0.926961    
# dependency2:environment3:group2   23.26252   25.00041 5818.97376   0.930 0.352158    
# dependency2:environment2:group3   13.49760   24.19964 5818.31364   0.558 0.577029    
# dependency2:environment3:group3   27.25428   24.16400 5811.62121   1.128 0.259414  

anova(model1, model4, refit = FALSE)

# npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)
# model1   21 79401 79542 -39680    79359                     
# model4   23 79405 79560 -39680    79359 0.0494  2     0.9756

#------------------------------------------------------------------------------#
# + + model 5
#------------------------------------------------------------------------------#

# fit model ...

tic()
model5 <- lmer(rrt ~ dependency * environment * group + 
                 (1 | participant) + 
                 (1 + environment | item), 
               data = md,
               control = lmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_doall_rrt_region3_md5.rds')
summary(model5)
toc()
beep()

# 4.91 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)                      -17.9809     6.9676  166.5656  -2.581 0.010724 *  
# dependency2                      -12.9448     4.1977 5785.4267  -3.084 0.002053 ** 
# environment2                     -11.3320     5.5601   39.5108  -2.038 0.048276 *  
# environment3                       3.6445     6.1175   29.3047   0.596 0.555917    
# group2                           -67.4114    14.8261  221.0193  -4.547 8.97e-06 ***
# group3                           -55.2685    14.6935  217.2450  -3.761 0.000217 ***
# dependency2:environment2           7.2346    10.3180 5777.5451   0.701 0.483229    
# dependency2:environment3           0.5463    10.3056 5752.7024   0.053 0.957725    
# dependency2:group2               -33.7263    10.1924 5796.8966  -3.309 0.000942 ***
# dependency2:group3               -25.3970     9.8579 5792.1996  -2.576 0.010011 *  
# environment2:group2              -12.2311    12.4757 5809.1796  -0.980 0.326931    
# environment3:group2                4.3845    12.4962 5808.1672   0.351 0.725700    
# environment2:group3               -8.2013    12.1121 5812.9600  -0.677 0.498361    
# environment3:group3               -5.0763    12.1004 5814.6284  -0.420 0.674855    
# dependency2:environment2:group2    2.4029    24.9215 5792.0788   0.096 0.923190    
# dependency2:environment3:group2   23.5333    24.9769 5796.8184   0.942 0.346128    
# dependency2:environment2:group3   13.2354    24.1792 5799.2809   0.547 0.584135    
# dependency2:environment3:group3   26.5362    24.1569 5810.1093   1.098 0.272034 

anova(model1, model5, refit = FALSE)

# npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)
# model1   21 79401 79542 -39680    79359                     
# model5   26 79409 79584 -39679    79357 2.0877  5     0.8369

#------------------------------------------------------------------------------#
# + + model 6 (final) ----
#------------------------------------------------------------------------------#

# fit model ...

tic()
model6 <- lmer(rrt ~ dependency * environment * group + 
                 (1 | participant) + 
                 (1 + group | item), 
               data = md,
               control = lmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_doall_rrt_region3_md6.rds')
summary(model6)
toc()
beep()

model6 <- read_rds('models/spr_doall_rrt_region3_md6.rds')

# 7.9 sec elapsed
# no warnings
# Fixed effects:
#   Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)                      -17.9530     6.9296  167.9486  -2.591 0.010418 *  
# dependency2                      -12.8432     4.1914 5806.2099  -3.064 0.002193 ** 
# environment2                     -11.6817     5.1336 5708.5843  -2.276 0.022909 *  
# environment3                       3.2519     5.1337 5704.8465   0.633 0.526470    
# group2                           -67.4056    14.8135  220.9881  -4.550 8.84e-06 ***
# group3                           -55.1383    15.3253  201.0850  -3.598 0.000404 ***
# dependency2:environment2           6.3165    10.3149 5814.8962   0.612 0.540317    
# dependency2:environment3          -0.3549    10.2633 5801.5191  -0.035 0.972420    
# dependency2:group2               -33.9668    10.1772 5790.7296  -3.338 0.000851 ***
# dependency2:group3               -25.3831     9.8414 5804.0411  -2.579 0.009927 ** 
# environment2:group2              -12.0754    12.4626 5796.9142  -0.969 0.332618    
# environment3:group2                4.5777    12.4835 5795.7124   0.367 0.713858    
# environment2:group3               -8.9257    12.0830 5473.3605  -0.739 0.460123    
# environment3:group3               -5.7003    12.0681 5479.6322  -0.472 0.636702    
# dependency2:environment2:group2    2.3069    24.8860 5787.1201   0.093 0.926145    
# dependency2:environment3:group2   23.5703    24.9418 5792.5291   0.945 0.344691    
# dependency2:environment2:group3   13.9834    24.1627 5794.0354   0.579 0.562802    
# dependency2:environment3:group3   27.0650    24.0995 5803.9102   1.123 0.261460 

anova(model1, model6, refit = FALSE)

# npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)   
# model1   21 79401 79542 -39680    79359                        
# model6   26 79392 79566 -39670    79340 19.863  5   0.001326 **

# post-hoc tests ...

pairwise <- model6 %>%
  emmeans(~ dependency * environment * group) %>%
  contrast('pairwise', simple = 'dependency', combine = TRUE) %>%
  summary(by = NULL, adjust = 'holm')
pairwise

# pairwise <- model6 %>%
#   emmeans(~ dependency * environment * group) %>%
#   contrast('pairwise', simple = 'each', combine = TRUE) %>%
#   summary(by = NULL, adjust = 'holm')
# pairwise

# tables ...

summary(model6)$coefficients %>%
  kbl(digits = c(2, 2, 2, 2, 3)) %>%
  remove_column(4)

pairwise %>%
  kbl(digits = c(2, 2, 2, 2, 2, 2, 2, 2, 3)) %>%
  remove_column(7)

# plot ...

plot <- plot(emmeans(model6, ~ dependency * environment * group), comparisons = TRUE, adjust = 'mvt', plotit = FALSE)

ggplot(plot, aes(x = environment, y = the.emmean, group = dependency, col = dependency, shape = dependency)) +
  geom_hline(yintercept = 0) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0, lwd = 5, alpha = .5, position = position_dodge(width = .4)) +
  geom_errorbar(aes(ymin = case_when(is.na(lcmpl)~the.emmean, TRUE ~ lcmpl), ymax = case_when(is.na(rcmpl)~the.emmean, TRUE ~ rcmpl)), width = 0, lwd = 1, position = position_dodge(width = .4)) +
  geom_line(lwd = 1, position = position_dodge(width = .4)) +
  geom_point(size = 3, position = position_dodge(width = .4)) +
  theme_classic() +
  scale_y_continuous(name="RRT (ms)", breaks = seq(-100, 100, by = 25), limits = c(-110, 85)) +
  scale_colour_manual(name="dependency", values=c('#648fff', '#ffb000'), labels=c("gap", "resumption")) +
  scale_shape_manual(name="dependency", values=c(16, 15), labels=c("gap", "resumption")) +
  theme(text = element_text(size = 12), 
        legend.margin=margin(0, 0, 0, -5)) +
  facet_wrap(~group, labeller = as_labeller(c(`english` = 'ENS', `korean` = 'KLE', `mandarin` = 'MLE')))

ggsave("plots/orc/spr_rrt_region3_emmeans.png", width=6.5, height=2, dpi=600)

#------------------------------------------------------------------------------#
# + + model 7
#------------------------------------------------------------------------------#

# fit model ...

tic()
model7 <- lmer(rrt ~ dependency * environment * group + 
                 (1 + dependency + environment | participant) + 
                 (1 + dependency + environment + group | item), 
               data = md,
               control = lmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_doall_rrt_region3_md7.rds')
summary(model7)
toc()
beep()

# 226.36 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)                      -17.8704     6.9345  167.9071  -2.577 0.010825 *  
# dependency2                      -12.7331     4.4638   46.3692  -2.853 0.006459 ** 
# environment2                     -11.8557     5.7232   34.6225  -2.072 0.045827 *  
# environment3                       3.4629     6.2773   35.8673   0.552 0.584611    
# group2                           -67.4237    14.9106  216.6751  -4.522 1.01e-05 ***
# group3                           -55.1574    15.3415  202.7968  -3.595 0.000407 ***
# dependency2:environment2           6.3637    10.2771 5536.9765   0.619 0.535803    
# dependency2:environment3           0.0508    10.2421 5515.6154   0.005 0.996043    
# dependency2:group2               -33.9451    10.1571 4585.5024  -3.342 0.000838 ***
# dependency2:group3               -25.3356     9.8221 4446.0063  -2.579 0.009928 ** 
# environment2:group2              -11.8883    12.8198  291.0068  -0.927 0.354519    
# environment3:group2                4.6697    13.0702  228.5533   0.357 0.721215    
# environment2:group3               -9.0003    12.4389  272.4772  -0.724 0.469956    
# environment3:group3               -5.7610    12.6633  212.3148  -0.455 0.649621    
# dependency2:environment2:group2    2.9237    24.8159 5499.6713   0.118 0.906218    
# dependency2:environment3:group2   23.7068    24.8428 5565.6638   0.954 0.339988    
# dependency2:environment2:group3   14.4981    24.0725 5531.3383   0.602 0.547021    
# dependency2:environment3:group3   26.6954    24.0147 5535.5414   1.112 0.266345  

anova(model6, model7, refit = FALSE)

# npar   AIC   BIC logLik deviance Chisq Df Pr(>Chisq)
# model6   26 79392 79566 -39670    79340                    
# model7   50 79427 79763 -39664    79327  12.5 24     0.9737

#------------------------------------------------------------------------------#
# + + model 8
#------------------------------------------------------------------------------#

# fit model ...

tic()
model8 <- lmer(rrt ~ dependency * environment * group + 
                 (1 + dependency * environment | participant) + 
                 (1 + dependency * environment * group | item), 
               data = md,
               control = lmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_doall_rrt_region3_md8.rds')
summary(model8)
toc()
beep()

# skip for now

#------------------------------------------------------------------------------#
# proficiency - rrt - region 2 ----
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# + d-score ----
#------------------------------------------------------------------------------#

# read in data ...

spr2 <- read_csv('data/spr_crit_clean.csv')

# filter for analysis ...

spr2 <- spr2 %>%
  filter(region2 == '2',
         group %in% c('korean', 'mandarin'))

# select columns ...

spr2 <- spr2 %>%
  select(study, group, participant, item, dependency, environment, rrt)

# add proficiency scores

spr2 <- spr2 %>%
  left_join(proficiency, by = c('study', 'group', 'participant')) %>%
  mutate(proficiency = scale(proficiency, scale = FALSE))

# set column classes ...

spr2 <- spr2 %>%
  mutate_at(c('study', 'dependency', 'environment', 'group', 'participant', 'item'), as.factor) %>%
  mutate_at(c('proficiency', 'rrt'), as.numeric) %>%
  mutate(environment = fct_relevel(environment, 'short', 'long', 'island'))
         
summary(spr_prof)

# summarise ...

spr3 <- spr2 %>%
  group_by(study, group, participant, dependency, environment, proficiency) %>%
  summarise(rrt = mean(rrt)) %>%
  ungroup() %>%
  group_by(study, group, participant, environment, proficiency) %>%
  summarise(dif = rrt[dependency == 'gap'] - rrt[dependency == 'pronoun']) %>%
  ungroup()

#------------------------------------------------------------------------------#
# + + plot - type 1 ----
#------------------------------------------------------------------------------#

# plot ...

p1 <- ggplot(data=filter(spr3, study == '210510_do'), aes(x=proficiency, y=dif, group = environment, col = environment, fill = environment, linetype = environment))
p2 <- ggplot(data=filter(spr3, study == '210510_su'), aes(x=proficiency, y=dif, group = environment, col = environment, fill = environment, linetype = environment))

s <- list(
  geom_point(alpha = .1, shape = 16, size = 2),
  geom_smooth(method=lm, lwd = 1, alpha = .2), 
  theme_classic(),
  scale_x_continuous(name='proficiency'),
  scale_y_continuous(name='RP advantage (ms)'),
  scale_linetype_manual(values = c('longdash', 'solid', 'twodash')),
  scale_color_manual(values = c('#619CFF', '#00BA38', '#F8766D')),
  scale_fill_manual(values = c('#619CFF', '#00BA38', '#F8766D')),
  guides(color = guide_legend(override.aes = list(shape = NA))),
  theme(text = element_text(size = 12),
        plot.title = element_text(size = 12, hjust = .5), 
        legend.key.size = unit(2,"line"),
        legend.position = 'right'),
  facet_wrap(~group, labeller = as_labeller(c(`korean` = 'KLE', `mandarin` = 'MLE')))
)

p1 + s
ggsave("plots/orc/spr_rrt_dscore_proficiency_effect_type1.png", width=6.5, height=2, dpi=600)

p2 + s
ggsave("plots/src/spr_rrt_dscore_proficiency_effect_type1.png", width=6.5, height=2, dpi=600)

#------------------------------------------------------------------------------#
# + + plot - type 2 ----
#------------------------------------------------------------------------------#

# plot ...

p1 <- ggplot(data=filter(spr3, study == '210510_do'), aes(x=proficiency, y=dif, group = environment, col = environment, fill = environment, linetype = environment))
p2 <- ggplot(data=filter(spr3, study == '210510_su'), aes(x=proficiency, y=dif, group = environment, col = environment, fill = environment, linetype = environment))

s <- list(
  geom_point(alpha = .3, shape = 16),
  geom_smooth(method=lm, lwd = .75, alpha = .1), 
  theme_classic(),
  scale_x_continuous(name='proficiency'),
  scale_y_continuous(name='RP advantage (ms)'),
  scale_linetype_manual(values = c('longdash', 'solid', 'twodash')),
  scale_color_manual(values = c('#619CFF', '#00BA38', '#F8766D')),
  scale_fill_manual(values = c('#619CFF', '#00BA38', '#F8766D')),
  theme(text = element_text(size = 12),
        plot.title = element_text(size = 12, hjust = .5), 
        legend.key.size = unit(2,"line"),
        legend.position = 'none'),
  facet_grid2(vars(group), vars(environment), axes = 'all', remove_labels = 'y',
              labeller = as_labeller(c(`short` = 'short', `long` = 'long', `island` = 'island', `korean` = 'KLE', `mandarin` = 'MLE')))
)

p1 + s
ggsave("plots/orc/spr_rrt_dscore_proficiency_effect_type2.png", width=6.5, height=3, dpi=600)

p2 + s
ggsave("plots/src/spr_rrt_dscore_proficiency_effect_type2.png", width=6.5, height=3, dpi=600)

#------------------------------------------------------------------------------#
# + + modeling: orc ----
#------------------------------------------------------------------------------#

# apply deviation coding ...

contrasts(spr3$environment) <- contr.treatment(3) - matrix(rep(1/3, 6), ncol=2)
contrasts(spr3$group) <- contr.treatment(2) - matrix(rep(1/2, 2), ncol=1)

# view contrasts ...

contrasts(md$environment)
contrasts(md$group)

# fit models ...

model1 <- lmer(dif ~ proficiency * environment * group + 
                 (1 | participant),
               data = filter(spr3, study == '210510_do')) 
summary(model1)

# Fixed effects:
#   Estimate Std. Error       df t value Pr(>|t|)    
# (Intercept)                      45.5634     7.3950 129.0000   6.161 8.53e-09 ***
# proficiency                      -1.1095     0.9564 129.0000  -1.160   0.2482    
# environment2                     37.5107    15.0685 258.0000   2.489   0.0134 *  
# environment3                     22.3705    15.0685 258.0000   1.485   0.1389    
# group2                           11.0272    14.7900 129.0000   0.746   0.4573    
# proficiency:environment2         -0.4581     1.9489 258.0000  -0.235   0.8143    
# proficiency:environment3         -0.5832     1.9489 258.0000  -0.299   0.7650    
# proficiency:group2                2.1756     1.9129 129.0000   1.137   0.2575    
# environment2:group2             -20.9343    30.1371 258.0000  -0.695   0.4879    
# environment3:group2              -0.4185    30.1371 258.0000  -0.014   0.9889    
# proficiency:environment2:group2  -1.4538     3.8978 258.0000  -0.373   0.7095    
# proficiency:environment3:group2  -5.4659     3.8978 258.0000  -1.402   0.1620  

model2 <- lmer(dif ~ proficiency * environment * group + 
                 (1 + environment | participant),
               data = filter(spr3, study == '210510_do')) 
summary(model2)

# does not converge

trends <- emtrends(model1, ~ group * environment, var = 'proficiency', adjust = 'mvt')
test(trends)

# group    environment proficiency.trend   SE  df t.ratio p.value
# korean   short                  -3.004 2.11 374  -1.425  0.6299
# mandarin short                   1.479 2.07 374   0.715  0.9781
# korean   long                   -2.735 2.11 374  -1.298  0.7226
# mandarin long                    0.294 2.07 374   0.142  1.0000
# korean   island                 -0.854 2.11 374  -0.405  0.9990
# mandarin island                 -1.837 2.07 374  -0.888  0.9381
# Degrees-of-freedom method: kenward-roger 
# P value adjustment: mvt method for 6 tests 

# plot ...

plot <- emmip(model1, environment ~ proficiency | group, cov.reduce = range, CIs = TRUE, plotit = FALSE)

ggplot(plot, aes(x = proficiency, y = yvar, group = environment, col = environment, linetype = environment)) +
  theme_classic() +
  geom_ribbon(aes(ymin = LCL, ymax = UCL, fill = environment), alpha = .1, col = NA) +
  geom_line(lwd = .75) +
  scale_y_continuous(name='RP advantage (ms)') +
  scale_x_continuous(name='mean-centered proficiency score') +
  scale_linetype_manual(values = c('longdash', 'solid', 'twodash')) +
  scale_color_manual(values = c('#619CFF', '#00BA38', '#F8766D')) +
  scale_fill_manual(values = c('#619CFF', '#00BA38', '#F8766D')) +
  theme(legend.key.size = unit(2,"line")) +
  facet_wrap(~group, labeller = as_labeller(c(`korean` = 'KLE', `mandarin` = 'MLE')))

ggsave("plots/orc/spr_rrt_dscore_proficiency_emmeans_md1.png", width=6.5, height=3, dpi=600)

#------------------------------------------------------------------------------#
# + + modeling: src ----
#------------------------------------------------------------------------------#

# apply deviation coding ...

contrasts(spr3$environment) <- contr.treatment(3) - matrix(rep(1/3, 6), ncol=2)
contrasts(spr3$group) <- contr.treatment(2) - matrix(rep(1/2, 2), ncol=1)

# view contrasts ...

contrasts(md$environment)
contrasts(md$group)

# fit models ...

model1 <- lmer(dif ~ proficiency * environment * group + 
                 (1 | participant),
               data = filter(spr3, study == '210510_su')) 
summary(model1)

# optimizer (nloptwrap) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error       df t value Pr(>|t|)    
# (Intercept)                      63.6215    10.3792 380.0000   6.130 2.21e-09 ***
# proficiency                       0.3824     1.2144 380.0000   0.315   0.7530    
# environment2                    122.6877    25.4503 380.0000   4.821 2.07e-06 ***
# environment3                     17.7967    25.4503 380.0000   0.699   0.4848    
# group2                           25.1412    20.7584 380.0000   1.211   0.2266    
# proficiency:environment2          3.9876     2.9763 380.0000   1.340   0.1811    
# proficiency:environment3          7.6532     2.9763 380.0000   2.571   0.0105 *  
# proficiency:group2                0.4140     2.4289 380.0000   0.170   0.8647    
# environment2:group2              93.5430    50.9006 380.0000   1.838   0.0669 .  
# environment3:group2              95.7858    50.9006 380.0000   1.882   0.0606 .  
# proficiency:environment2:group2 -11.2422     5.9526 380.0000  -1.889   0.0597 .  
# proficiency:environment3:group2  -4.3285     5.9526 380.0000  -0.727   0.4676  

trends <- emtrends(model1, ~ group * environment, var = 'proficiency', adjust = 'mvt')
test(trends)

# group    environment proficiency.trend   SE  df t.ratio p.value
# korean   short                  -6.300 3.16 380  -1.994  0.2494
# mandarin short                  -0.696 2.79 380  -0.249  0.9999
# korean   long                    3.309 3.14 380   1.053  0.8739
# mandarin long                   -2.329 2.79 380  -0.835  0.9549
# korean   island                  3.517 3.14 380   1.120  0.8393
# mandarin island                  4.793 2.79 380   1.717  0.4184
# Degrees-of-freedom method: kenward-roger 
# P value adjustment: mvt method for 6 tests 

# plot ...

plot <- emmip(model1, environment ~ proficiency | group, cov.reduce = range, CIs = TRUE, plotit = FALSE)

ggplot(plot, aes(x = proficiency, y = yvar, group = environment, col = environment, linetype = environment)) +
  theme_classic() +
  geom_ribbon(aes(ymin = LCL, ymax = UCL, fill = environment), alpha = .1, col = NA) +
  geom_line(lwd = .75) +
  scale_y_continuous(name='RP advantage (ms)') +
  scale_x_continuous(name='mean-centered proficiency score') +
  scale_linetype_manual(values = c('longdash', 'solid', 'twodash')) +
  scale_color_manual(values = c('#619CFF', '#00BA38', '#F8766D')) +
  scale_fill_manual(values = c('#619CFF', '#00BA38', '#F8766D')) +
  theme(legend.key.size = unit(2,"line")) +
  facet_wrap(~group, labeller = as_labeller(c(`korean` = 'KLE', `mandarin` = 'MLE')))

ggsave("plots/src/spr_rrt_dscore_proficiency_emmeans_md1.png", width=6.5, height=3, dpi=600)

#------------------------------------------------------------------------------#
# + lmer - rrt ----
#------------------------------------------------------------------------------#

# read in data

spr_crit_clean <- read_csv('data/spr_crit_clean.csv')

# filter data for analysis

md <- spr_crit_clean %>%
  filter(region2 == 2,
         group %in% c('korean', 'mandarin'),
         study == '210510_do') %>%
  select(study, group, rrt, dependency, environment, participant, item)

# add proficiency scores

md <- md %>%
  left_join(proficiency, by = c('study', 'group', 'participant')) %>%
  mutate(proficiency = scale(proficiency, scale = FALSE))

# add proficiency scores

md <- md %>%
  left_join(proficiency, by = c('study', 'group', 'participant')) %>%
  mutate(proficiency = scale(proficiency, scale = FALSE))

# set class of columns ...

md <- md %>%
  mutate(dependency = as.factor(dependency),
         environment = as.factor(environment),
         group = as.factor(group),
         proficiency = as.numeric(proficiency),
         rrt = as.numeric(rrt)) %>%
  mutate(environment = fct_relevel(environment, 'short', 'long', 'island'))

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
# + + model 1
#------------------------------------------------------------------------------#

# fit model ...

tic()
model1 <- lmer(rrt ~ proficiency * dependency * environment * group + 
                 (1 | participant) + 
                 (1 | item), 
               data = md,
               control = lmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_do_rrt_proficiency_region2_md1.rds')
summary(model1)
toc()
beep()

pairwise <- model1 %>%
  emmeans(~ proficiency * dependency * environment * group) %>%
  contrast('pairwise', simple = 'all', combine = TRUE) %>%
  summary(by = NULL, adjust = 'holm')
pairwise

trends <- emtrends(model1, ~ group * dependency * environment , var = 'proficiency', adjust = 'mvt')
test(trends)


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
# + orc ----
#------------------------------------------------------------------------------#

# filter data for analysis

md <- temp %>%
        filter(study == '210510_do')

# set class of columns ...

md <- md %>%
  mutate(dependency = as.factor(dependency),
         environment = as.factor(environment),
         group = as.factor(group),
         accuracy = as.logical(accuracy)) %>%
  mutate(environment = fct_relevel(environment, 'short', 'long', 'island'))

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
model1 <- glmer(accuracy ~ dependency * environment * group + 
                  (1 | participant) + 
                  (1 | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_doall_acc_md1.rds')
summary(model1)
toc()
beep()

# 22.45 sec elapsed
# no warnings
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                      3.03467    0.12761  23.780  < 2e-16 ***
# dependency2                      1.08778    0.10718  10.149  < 2e-16 ***
# environment2                    -0.17436    0.13017  -1.339  0.18043    
# environment3                    -0.09116    0.13198  -0.691  0.48973    
# group2                          -1.58795    0.23635  -6.719 1.83e-11 ***
# group3                          -0.67823    0.24556  -2.762  0.00575 ** 
# dependency2:environment2         0.01200    0.26026   0.046  0.96323    
# dependency2:environment3         0.17525    0.26378   0.664  0.50644    
# dependency2:group2               0.57736    0.25970   2.223  0.02620 *  
# dependency2:group3               0.10430    0.28358   0.368  0.71303    
# environment2:group2             -0.09482    0.31824  -0.298  0.76573    
# environment3:group2              0.08877    0.31866   0.279  0.78058    
# environment2:group3             -0.10784    0.34677  -0.311  0.75580    
# environment3:group3              0.02838    0.35068   0.081  0.93550    
# dependency2:environment2:group2 -0.27735    0.63581  -0.436  0.66268    
# dependency2:environment3:group2 -0.12824    0.63669  -0.201  0.84038    
# dependency2:environment2:group3 -0.22829    0.69175  -0.330  0.74139    
# dependency2:environment3:group3  0.53032    0.69884   0.759  0.44794 

pairwise <- model1 %>%
  emmeans(~ dependency * environment * group) %>%
  contrast('pairwise', simple = 'dependency', combine = TRUE) %>%
  summary(by = NULL, adjust = 'holm')
pairwise

# contrast      environment group    estimate    SE  df z.ratio p.value
# gap - pronoun short       english    -0.787 0.374 Inf  -2.101  0.0651 .
# gap - pronoun long        english    -0.967 0.366 Inf  -2.641  0.0413 *
# gap - pronoun island      english    -0.828 0.359 Inf  -2.305  0.0651 .
# gap - pronoun short       korean     -1.499 0.267 Inf  -5.623  <.0001 ***
# gap - pronoun long        korean     -1.402 0.248 Inf  -5.649  <.0001 ***
# gap - pronoun island      korean     -1.412 0.260 Inf  -5.430  <.0001 ***
# gap - pronoun short       mandarin   -0.790 0.329 Inf  -2.403  0.0651 .
# gap - pronoun long        mandarin   -0.743 0.310 Inf  -2.391  0.0651 .
# gap - pronoun island      mandarin   -1.362 0.336 Inf  -4.052  0.0003 ***
# Results are given on the log odds ratio (not the response) scale. 
# P value adjustment: holm method for 9 tests 

#------------------------------------------------------------------------------#
# + + model 2 
#------------------------------------------------------------------------------#

# fit model ...

tic()
model2 <- glmer(accuracy ~ dependency * environment * group + 
                  (1 + dependency | participant) + 
                  (1 | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_doall_acc_md2.rds')
summary(model2)
toc()
beep()

# 29.88 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                      2.99920    0.12608  23.788  < 2e-16 ***
# dependency2                      0.88354    0.14937   5.915 3.32e-09 ***
# environment2                    -0.17488    0.12975  -1.348  0.17771    
# environment3                    -0.09244    0.13160  -0.702  0.48242    
# group2                          -1.59434    0.22978  -6.939 3.96e-12 ***
# group3                          -0.68706    0.23879  -2.877  0.00401 ** 
# dependency2:environment2         0.01947    0.25951   0.075  0.94019    
# dependency2:environment3         0.18166    0.26309   0.690  0.48989    
# dependency2:group2               0.67168    0.26707   2.515  0.01190 *  
# dependency2:group3               0.11202    0.28607   0.392  0.69535    
# environment2:group2             -0.08939    0.31741  -0.282  0.77823    
# environment3:group2              0.09299    0.31789   0.293  0.76989    
# environment2:group3             -0.10449    0.34572  -0.302  0.76248    
# environment3:group3              0.02738    0.34977   0.078  0.93760    
# dependency2:environment2:group2 -0.26424    0.63424  -0.417  0.67695    
# dependency2:environment3:group2 -0.13109    0.63534  -0.206  0.83654    
# dependency2:environment2:group3 -0.21819    0.68993  -0.316  0.75181    
# dependency2:environment3:group3  0.53551    0.69765   0.768  0.44273 

anova(model1, model2)

# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model1   20 3416.3 3552.5 -1688.2   3376.3                     
# model2   22 3416.7 3566.5 -1686.3   3372.7 3.6103  2     0.1645

#------------------------------------------------------------------------------#
# + + model 3 
#------------------------------------------------------------------------------#

# fit model ...

tic()
model3 <- glmer(accuracy ~ dependency * environment * group + 
                  (1 + environment | participant) + 
                  (1 | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_doall_acc_md3.rds')
summary(model3)
toc()
beep()

# 52.41 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                      3.06852    0.13044  23.525  < 2e-16 ***
# dependency2                      1.09646    0.10778  10.173  < 2e-16 ***
# environment2                    -0.06433    0.20412  -0.315  0.75265    
# environment3                    -0.22264    0.19197  -1.160  0.24614    
# group2                          -1.59664    0.23804  -6.707 1.98e-11 ***
# group3                          -0.66897    0.24747  -2.703  0.00687 ** 
# dependency2:environment2         0.04293    0.26523   0.162  0.87141    
# dependency2:environment3         0.14394    0.26474   0.544  0.58666    
# dependency2:group2               0.58910    0.26068   2.260  0.02383 *  
# dependency2:group3               0.10577    0.28468   0.372  0.71023    
# environment2:group2             -0.16107    0.35070  -0.459  0.64602    
# environment3:group2              0.14131    0.33939   0.416  0.67714    
# environment2:group3             -0.10361    0.37481  -0.276  0.78221    
# environment3:group3              0.03953    0.36694   0.108  0.91420    
# dependency2:environment2:group2 -0.27099    0.64254  -0.422  0.67321    
# dependency2:environment3:group2 -0.15917    0.63541  -0.250  0.80221    
# dependency2:environment2:group3 -0.22702    0.69930  -0.325  0.74546    
# dependency2:environment3:group3  0.51010    0.69679   0.732  0.46412

anova(model1, model3)

# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model1   20 3416.3 3552.5 -1688.2   3376.3                     
# model3   25 3420.6 3590.8 -1685.3   3370.6 5.6928  5     0.3373

#------------------------------------------------------------------------------#
# + + model 4 (final) ----
#------------------------------------------------------------------------------#

# fit model ...

tic()
model4 <- glmer(accuracy ~ dependency * environment * group + 
                  (1 | participant) + 
                  (1 + dependency | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_doall_acc_md4.rds')
summary(model4)
toc()
beep()

model4 <- read_rds('models/spr_doall_acc_md4.rds')

# 42.08 sec elapsed
# no warnings
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                      3.047831   0.130720  23.316  < 2e-16 ***
# dependency2                      1.132730   0.121568   9.318  < 2e-16 ***
# environment2                    -0.177957   0.130539  -1.363  0.17280    
# environment3                    -0.091970   0.132260  -0.695  0.48682    
# group2                          -1.588793   0.236340  -6.723 1.79e-11 ***
# group3                          -0.680527   0.245515  -2.772  0.00557 ** 
# dependency2:environment2         0.004902   0.260962   0.019  0.98501    
# dependency2:environment3         0.171064   0.264416   0.647  0.51766    
# dependency2:group2               0.563371   0.260320   2.164  0.03045 *  
# dependency2:group3               0.095099   0.284066   0.335  0.73779    
# environment2:group2             -0.094149   0.319038  -0.295  0.76792    
# environment3:group2              0.093188   0.319472   0.292  0.77052    
# environment2:group3             -0.105302   0.347745  -0.303  0.76203    
# environment3:group3              0.034625   0.351480   0.099  0.92153    
# dependency2:environment2:group2 -0.270645   0.637517  -0.425  0.67118    
# dependency2:environment3:group2 -0.109145   0.638595  -0.171  0.86429    
# dependency2:environment2:group3 -0.253209   0.694185  -0.365  0.71529    
# dependency2:environment3:group3  0.534426   0.701271   0.762  0.44601  

anova(model1, model4)

# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model1   20 3416.3 3552.5 -1688.2   3376.3                     
# model4   22 3418.8 3568.6 -1687.4   3374.8 1.5056  2      0.471

pairwise <- model4 %>%
  emmeans(~ dependency * environment * group) %>%
  contrast('pairwise', simple = 'dependency', combine = TRUE) %>%
  summary(by = NULL, adjust = 'mvt')
pairwise

# contrast      environment group    estimate    SE  df z.ratio p.value
# gap - pronoun short       english    -0.844 0.381 Inf  -2.215  0.2161
# gap - pronoun long        english    -1.023 0.372 Inf  -2.749  0.0525 .
# gap - pronoun island      english    -0.873 0.364 Inf  -2.398  0.1387
# gap - pronoun short       korean     -1.534 0.272 Inf  -5.630  <.0001 ***
# gap - pronoun long        korean     -1.442 0.255 Inf  -5.660  <.0001 ***
# gap - pronoun island      korean     -1.454 0.267 Inf  -5.450  <.0001 ***
# gap - pronoun short       mandarin   -0.845 0.336 Inf  -2.517  0.1014
# gap - pronoun long        mandarin   -0.771 0.315 Inf  -2.447  0.1222
# gap - pronoun island      mandarin   -1.409 0.342 Inf  -4.118  0.0003 ***
# Results are given on the log odds ratio (not the response) scale. 
# P value adjustment: mvt method for 9 tests 

pairwise <- model4 %>%
  emmeans(~ dependency * group) %>%
  contrast('pairwise', simple = 'each', combine = TRUE) %>%
  summary(by = NULL, adjust = 'mvt')
pairwise


pairwise <- model4 %>%
  emmeans(~ dependency * environment * group) %>%
  contrast('pairwise', simple = 'each', combine = TRUE) %>%
  summary(by = NULL, adjust = 'mvt')
pairwise

# environment group    dependency contrast           estimate    SE  df z.ratio p.value
# short       english  .          gap - pronoun      -0.84363 0.381 Inf  -2.215  0.7219
# long        english  .          gap - pronoun      -1.02315 0.372 Inf  -2.749  0.1915
# island      english  .          gap - pronoun      -0.87294 0.364 Inf  -2.398  0.4613
# short       korean   .          gap - pronoun      -1.53360 0.272 Inf  -5.630  <.0001
# long        korean   .          gap - pronoun      -1.44247 0.255 Inf  -5.660  <.0001
# island      korean   .          gap - pronoun      -1.45376 0.267 Inf  -5.450  <.0001
# short       mandarin .          gap - pronoun      -0.84499 0.336 Inf  -2.517  0.3548
# long        mandarin .          gap - pronoun      -0.77130 0.315 Inf  -2.447  0.4175
# island      mandarin .          gap - pronoun      -1.40872 0.342 Inf  -4.118  0.0015

joint_tests(model4)

# tables ...

summary(model4)$coefficients %>%
  kbl(digits = c(2, 2, 2, 3))

pairwise %>%
  kbl(digits = c(2, 2, 2, 2, 2, 2, 2, 2, 3)) %>%
  remove_column(7)

# plot ...

plot <- plot(emmeans(model4, ~ dependency * environment * group), comparisons = TRUE, adjust = 'mvt', plotit = FALSE)

ggplot(plot, aes(x = environment, y = the.emmean, group = dependency, col = dependency, shape = dependency)) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0, lwd = 5, alpha = .5, position = position_dodge(width = .4)) +
  geom_errorbar(aes(ymin = case_when(is.na(lcmpl)~the.emmean, TRUE ~ lcmpl), ymax = case_when(is.na(rcmpl)~the.emmean, TRUE ~ rcmpl)), width = 0, lwd = 1, position = position_dodge(width = .4)) +
  geom_line(lwd = 1, position = position_dodge(width = .4)) +
  geom_point(size = 3, position = position_dodge(width = .4)) +
  theme_classic() +
  scale_y_continuous(name="log odds ratio") +
  scale_colour_manual(name="dependency", values=c('#648fff', '#ffb000'), labels=c("gap", "resumption")) +
  scale_shape_manual(name="dependency", values=c(16, 15), labels=c("gap", "resumption")) +
  theme(text = element_text(size = 12), 
        legend.margin=margin(0, 0, 0, -5)) +
  facet_wrap(~group, labeller = as_labeller(c(`english` = 'ENS', `korean` = 'KLE', `mandarin` = 'MLE')))

ggsave("plots/orc/spr_accuracy_emmeans.png", width=6.5, height=2, dpi=600)

#------------------------------------------------------------------------------#
# + + model 5 
#------------------------------------------------------------------------------#

# fit model ...

tic()
model5 <- glmer(accuracy ~ dependency * environment * group + 
                  (1 | participant) + 
                  (1 + environment | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_doall_acc_md5.rds')
summary(model5)
toc()
beep()

# 65.8 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                      3.04922    0.12803  23.817  < 2e-16 ***
# dependency2                      1.09235    0.10749  10.162  < 2e-16 ***
# environment2                    -0.16354    0.15069  -1.085  0.27781    
# environment3                    -0.06643    0.15614  -0.425  0.67048    
# group2                          -1.59307    0.23738  -6.711 1.93e-11 ***
# group3                          -0.67819    0.24658  -2.750  0.00595 ** 
# dependency2:environment2         0.02161    0.26077   0.083  0.93396    
# dependency2:environment3         0.18808    0.26479   0.710  0.47753    
# dependency2:group2               0.58066    0.26032   2.231  0.02571 *  
# dependency2:group3               0.10602    0.28427   0.373  0.70919    
# environment2:group2             -0.10417    0.31896  -0.327  0.74397    
# environment3:group2              0.07584    0.31947   0.237  0.81237    
# environment2:group3             -0.11084    0.34729  -0.319  0.74960    
# environment3:group3              0.02691    0.35148   0.077  0.93897    
# dependency2:environment2:group2 -0.28086    0.63729  -0.441  0.65943    
# dependency2:environment3:group2 -0.12274    0.63900  -0.192  0.84768    
# dependency2:environment2:group3 -0.24164    0.69299  -0.349  0.72732    
# dependency2:environment3:group3  0.52827    0.70158   0.753  0.45147 

anova(model1, model5)

# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model1   20 3416.3 3552.5 -1688.2   3376.3                     
# model5   25 3422.7 3592.9 -1686.3   3372.7 3.6602  5     0.5993

#------------------------------------------------------------------------------#
# + + model 6 
#------------------------------------------------------------------------------#

# fit model ...

tic()
model6 <- glmer(accuracy ~ dependency * environment * group + 
                  (1 | participant) + 
                  (1 + group | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_doall_acc_md6.rds')
summary(model6)
toc()
beep()

# 78.67 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                      3.038509   0.128162  23.708  < 2e-16 ***
# dependency2                      1.088360   0.107295  10.144  < 2e-16 ***
# environment2                    -0.180742   0.130658  -1.383   0.1666    
# environment3                    -0.096034   0.132549  -0.725   0.4687    
# group2                          -1.577590   0.239527  -6.586 4.51e-11 ***
# group3                          -0.632270   0.253002  -2.499   0.0125 *  
# dependency2:environment2         0.018358   0.260703   0.070   0.9439    
# dependency2:environment3         0.176465   0.263927   0.669   0.5037    
# dependency2:group2               0.573594   0.259493   2.210   0.0271 *  
# dependency2:group3               0.108949   0.283995   0.384   0.7013    
# environment2:group2             -0.095262   0.317791  -0.300   0.7644    
# environment3:group2              0.085705   0.318279   0.269   0.7877    
# environment2:group3             -0.128212   0.348497  -0.368   0.7129    
# environment3:group3              0.008234   0.352622   0.023   0.9814    
# dependency2:environment2:group2 -0.278178   0.635898  -0.437   0.6618    
# dependency2:environment3:group2 -0.127073   0.636358  -0.200   0.8417    
# dependency2:environment2:group3 -0.227275   0.693736  -0.328   0.7432    
# dependency2:environment3:group3  0.530603   0.700238   0.758   0.4486 

anova(model1, model6)

# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model1   20 3416.3 3552.5 -1688.2   3376.3                     
# model6   25 3425.3 3595.5 -1687.7   3375.3 1.0016  5     0.9624

#------------------------------------------------------------------------------#
# + src ----
#------------------------------------------------------------------------------#

# filter data for analysis

md <- temp %>%
  filter(study == '210510_su')

# set class of columns ...

md <- md %>%
  mutate(dependency = as.factor(dependency),
         environment = as.factor(environment),
         group = as.factor(group),
         accuracy = as.logical(accuracy)) %>%
  mutate(environment = fct_relevel(environment, 'short', 'long', 'island'))

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
model1 <- glmer(accuracy ~ dependency * environment * group + 
                  (1 | participant) + 
                  (1 | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_suall_acc_md1.rds')
summary(model1)
toc()
beep()

# 13.81 sec elapsed
# no warnings
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                      2.76600    0.13120  21.082  < 2e-16 ***
# dependency2                      0.99880    0.10020   9.968  < 2e-16 ***
# environment2                    -0.54249    0.12765  -4.250 2.14e-05 ***
# environment3                    -0.76718    0.12428  -6.173 6.70e-10 ***
# group2                          -0.13053    0.22710  -0.575  0.56543    
# group3                          -0.13156    0.22122  -0.595  0.55206    
# dependency2:environment2         0.61892    0.25542   2.423  0.01539 *  
# dependency2:environment3         0.69001    0.24918   2.769  0.00562 ** 
# dependency2:group2               0.23999    0.25169   0.954  0.34032    
# dependency2:group3               0.09373    0.24334   0.385  0.70011    
# environment2:group2             -0.55881    0.32053  -1.743  0.08127 .  
# environment3:group2             -0.47305    0.31520  -1.501  0.13341    
# environment2:group3             -0.23086    0.30856  -0.748  0.45436    
# environment3:group3             -0.15684    0.29921  -0.524  0.60015    
# dependency2:environment2:group2  0.42914    0.64103   0.669  0.50321    
# dependency2:environment3:group2 -0.34605    0.63182  -0.548  0.58390    
# dependency2:environment2:group3  1.12692    0.61553   1.831  0.06713 .  
# dependency2:environment3:group3 -0.48257    0.59876  -0.806  0.42027 

#------------------------------------------------------------------------------#
# + + model 2
#------------------------------------------------------------------------------#

# fit model ...

tic()
model2 <- glmer(accuracy ~ dependency * environment * group + 
                  (1 + dependency | participant) + 
                  (1 | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_suall_acc_md2.rds')
summary(model2)
toc()
beep()

# 15.24 sec elapsed
# no warnings
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                      2.78202    0.13416  20.737  < 2e-16 ***
# dependency2                      1.03488    0.14092   7.344 2.08e-13 ***
# environment2                    -0.54289    0.12755  -4.256 2.08e-05 ***
# environment3                    -0.76788    0.12422  -6.182 6.34e-10 ***
# group2                          -0.13141    0.22855  -0.575  0.56531    
# group3                          -0.13331    0.22263  -0.599  0.54931    
# dependency2:environment2         0.61669    0.25520   2.417  0.01567 *  
# dependency2:environment3         0.68696    0.24914   2.757  0.00583 ** 
# dependency2:group2               0.23475    0.26173   0.897  0.36976    
# dependency2:group3               0.08451    0.25335   0.334  0.73871    
# environment2:group2             -0.55793    0.32021  -1.742  0.08145 .  
# environment3:group2             -0.47520    0.31499  -1.509  0.13140    
# environment2:group3             -0.22801    0.30831  -0.740  0.45958    
# environment3:group3             -0.15884    0.29912  -0.531  0.59542    
# dependency2:environment2:group2  0.43401    0.64048   0.678  0.49800    
# dependency2:environment3:group2 -0.34618    0.63130  -0.548  0.58344    
# dependency2:environment2:group3  1.13570    0.61501   1.847  0.06480 .  
# dependency2:environment3:group3 -0.48077    0.59829  -0.804  0.42164 

anova(model1, model2)

# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model1   20 3412.9 3546.2 -1686.5   3372.9                     
# model2   22 3416.5 3563.1 -1686.2   3372.5 0.4238  2     0.8091

#------------------------------------------------------------------------------#
# + + model 3
#------------------------------------------------------------------------------#

# fit model ...

tic()
model3 <- glmer(accuracy ~ dependency * environment * group + 
                  (1 + dependency + environment | participant) + 
                  (1 | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_suall_acc_md3.rds')
summary(model3)
toc()
beep()

# 128.85 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                      2.82858    0.13961  20.260  < 2e-16 ***
# dependency2                      1.08650    0.15785   6.883 5.86e-12 ***
# environment2                    -0.57729    0.19830  -2.911   0.0036 ** 
# environment3                    -0.82769    0.19208  -4.309 1.64e-05 ***
# group2                          -0.12934    0.23629  -0.547   0.5841    
# group3                          -0.13468    0.22995  -0.586   0.5581    
# dependency2:environment2         0.40986    0.28772   1.425   0.1543    
# dependency2:environment3         0.54012    0.27803   1.943   0.0521 .  
# dependency2:group2               0.25483    0.28512   0.894   0.3715    
# dependency2:group3               0.09461    0.27609   0.343   0.7318    
# environment2:group2             -0.56168    0.35819  -1.568   0.1169    
# environment3:group2             -0.47042    0.33947  -1.386   0.1658    
# environment2:group3             -0.23067    0.34547  -0.668   0.5043    
# environment3:group3             -0.14971    0.32323  -0.463   0.6432    
# dependency2:environment2:group2  0.43343    0.67312   0.644   0.5196    
# dependency2:environment3:group2 -0.35325    0.66075  -0.535   0.5929    
# dependency2:environment2:group3  1.15706    0.64771   1.786   0.0740 .  
# dependency2:environment3:group3 -0.47433    0.62765  -0.756   0.4498  

anova(model2, model3)

# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model2   22 3416.5 3563.1 -1686.2   3372.5                     
# model3   29 3424.3 3617.6 -1683.2   3366.3 6.1645  7     0.5207

#------------------------------------------------------------------------------#
# + + model 4 (final) ----
#------------------------------------------------------------------------------#

# fit model ...

tic()
model4 <- glmer(accuracy ~ dependency * environment * group + 
                  (1 + dependency | participant) + 
                  (1 + dependency | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_suall_acc_md4.rds')
summary(model4)
toc()
beep()

model4 <- read_rds('models/spr_suall_acc_md4.rds')

# 43.81 sec elapsed
# no warnings
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                      2.79050    0.13687  20.388  < 2e-16 ***
# dependency2                      1.06727    0.14980   7.125 1.04e-12 ***
# environment2                    -0.54150    0.12740  -4.250 2.14e-05 ***
# environment3                    -0.76640    0.12409  -6.176 6.57e-10 ***
# group2                          -0.13294    0.22865  -0.581  0.56097    
# group3                          -0.13217    0.22274  -0.593  0.55292    
# dependency2:environment2         0.61382    0.25497   2.407  0.01607 *  
# dependency2:environment3         0.68232    0.24890   2.741  0.00612 ** 
# dependency2:group2               0.23367    0.26144   0.894  0.37144    
# dependency2:group3               0.08416    0.25312   0.333  0.73951    
# environment2:group2             -0.55801    0.31975  -1.745  0.08096 .  
# environment3:group2             -0.47458    0.31455  -1.509  0.13137    
# environment2:group3             -0.23018    0.30804  -0.747  0.45492    
# environment3:group3             -0.16445    0.29915  -0.550  0.58250    
# dependency2:environment2:group2  0.42819    0.63993   0.669  0.50342    
# dependency2:environment3:group2 -0.35099    0.63052  -0.557  0.57776    
# dependency2:environment2:group3  1.12680    0.61487   1.833  0.06686 .  
# dependency2:environment3:group3 -0.49306    0.59847  -0.824  0.41002  

anova(model4, model2)

# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model2   22 3416.5 3563.1 -1686.2   3372.5                     
# model4   24 3420.0 3580.0 -1686.0   3372.0 0.4451  2     0.8005

pairwise <- model4 %>%
  emmeans(~ dependency * environment * group) %>%
  contrast('pairwise', simple = 'each', combine = TRUE) %>%
  summary(by = NULL, adjust = 'holm')
pairwise

# tables ...

summary(model4)$coefficients %>%
  kbl(digits = c(2, 2, 2, 2, 3)) %>%
  remove_column(4)

pairwise %>%
  kbl(digits = c(2, 2, 2, 2, 2, 2, 2, 2, 3)) %>%
  remove_column(7)

# visualize ...

emmip <- emmip(model4, dependency ~ environment | group, CIs = TRUE)

groups <- c(`english` = 'ENS', `korean` = 'KLE', `mandarin` = 'MLE')

emmip + 
  geom_hline(yintercept = 0) +
  theme_classic() +
  scale_color_manual(values = c('#648fff', '#ffb000')) + 
  facet_wrap(~group, labeller = as_labeller(groups))

ggsave('plots/src/spr_rrt_region3_emmip.png', width=6.5, height=3, dpi=600)

# interaction plot with significance lines

plot <- plot(emmeans(model4, ~ dependency * environment * group), comparisons = TRUE, adjust = 'mvt', plotit = FALSE)

ggplot(plot, aes(x = environment, y = the.emmean, group = dependency, col = dependency, shape = dependency)) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0, lwd = 2, alpha = .3, position = position_dodge(width = .1)) +
  geom_errorbar(aes(ymin = case_when(is.na(lcmpl)~the.emmean, TRUE ~ lcmpl), ymax = case_when(is.na(rcmpl)~the.emmean, TRUE ~ rcmpl)), width = 0, lwd = .75, position = position_dodge(width = .1)) +
  geom_line(lwd = 1, position = position_dodge(width = .1)) +
  geom_point(size = 2, position = position_dodge(width = .1)) +
  theme_classic() +
  scale_y_continuous(name="log odds ratio") +
  scale_colour_manual(name="dependency", values=c('#648fff', '#ffb000'), labels=c("gap", "resumption")) +
  scale_shape_manual(name="dependency", values=c(16, 15), labels=c("gap", "resumption")) +
  theme(text = element_text(size = 12), 
        legend.margin=margin(0, 0, 0, -5)) +
  facet_wrap(~group, labeller = as_labeller(c(`english` = 'ENS', `korean` = 'KLE', `mandarin` = 'MLE')))

ggsave("plots/src/spr_accuracy_emmeans.png", width=6.5, height=2, dpi=600)

#------------------------------------------------------------------------------#
# + + model 5
#------------------------------------------------------------------------------#

# fit model ...

tic()
model5 <- glmer(accuracy ~ dependency * environment * group + 
                  (1 + dependency | participant) + 
                  (1 + dependency + environment | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_suall_acc_md5.rds')
summary(model5)
toc()
beep()

# 93.15 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                      2.82363    0.13929  20.272  < 2e-16 ***
# dependency2                      1.09092    0.15302   7.129 1.01e-12 ***
# environment2                    -0.57983    0.17473  -3.318 0.000905 ***
# environment3                    -0.81536    0.14269  -5.714 1.10e-08 ***
# group2                          -0.13271    0.22968  -0.578 0.563404    
# group3                          -0.13659    0.22388  -0.610 0.541794    
# dependency2:environment2         0.57653    0.26053   2.213 0.026904 *  
# dependency2:environment3         0.64807    0.25076   2.584 0.009755 ** 
# dependency2:group2               0.23898    0.26163   0.913 0.361013    
# dependency2:group3               0.08155    0.25350   0.322 0.747690    
# environment2:group2             -0.55908    0.31922  -1.751 0.079881 .  
# environment3:group2             -0.47404    0.31359  -1.512 0.130621    
# environment2:group3             -0.23038    0.30819  -0.748 0.454753    
# environment3:group3             -0.15856    0.29887  -0.531 0.595741    
# dependency2:environment2:group2  0.42542    0.63916   0.666 0.505673    
# dependency2:environment3:group2 -0.35779    0.62857  -0.569 0.569215    
# dependency2:environment2:group3  1.15629    0.61594   1.877 0.060479 .  
# dependency2:environment3:group3 -0.45819    0.59725  -0.767 0.442981  

anova(model4, model5)

# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model4   24 3420.0 3580.0 -1686.0   3372.0                     
# model5   31 3428.4 3634.9 -1683.2   3366.4 5.6549  7     0.5806

#------------------------------------------------------------------------------#
# + + model 6
#------------------------------------------------------------------------------#

# fit model ...

tic()
model6 <- glmer(accuracy ~ dependency * environment * group + 
                  (1 + dependency | participant) + 
                  (1 + dependency + group | item), 
                data = md, family = binomial, 
                control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_suall_acc_md6.rds')
summary(model6)
toc()
beep()

# 140.39 sec elapsed
# optimizer (bobyqa) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                      2.81391    0.13794  20.400  < 2e-16 ***
# dependency2                      1.07103    0.15289   7.005 2.46e-12 ***
# environment2                    -0.54542    0.12745  -4.279 1.87e-05 ***
# environment3                    -0.77123    0.12415  -6.212 5.23e-10 ***
# group2                          -0.14925    0.24455  -0.610  0.54164    
# group3                          -0.16529    0.24578  -0.673  0.50126    
# dependency2:environment2         0.62183    0.25481   2.440  0.01467 *  
# dependency2:environment3         0.69274    0.24904   2.782  0.00541 ** 
# dependency2:group2               0.20831    0.26530   0.785  0.43233    
# dependency2:group3               0.08046    0.25826   0.312  0.75539    
# environment2:group2             -0.56144    0.31981  -1.756  0.07917 .  
# environment3:group2             -0.46515    0.31460  -1.479  0.13926    
# environment2:group3             -0.22658    0.30750  -0.737  0.46122    
# environment3:group3             -0.14514    0.29882  -0.486  0.62718    
# dependency2:environment2:group2  0.42287    0.63960   0.661  0.50852    
# dependency2:environment3:group2 -0.34032    0.63126  -0.539  0.58981    
# dependency2:environment2:group3  1.11579    0.61399   1.817  0.06918 .  
# dependency2:environment3:group3 -0.48274    0.59747  -0.808  0.41911 

anova(model4, model6)

# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# model4   24 3420.0 3580.0 -1686.0   3372.0                     
# model6   31 3429.3 3635.9 -1683.7   3367.3 4.7241  7     0.6936

#------------------------------------------------------------------------------#
# scatter plot of proficiency effects for accuracy data ----
#------------------------------------------------------------------------------#

# trim based on accuracy

trim <- spr_crit_clean %>%
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
  filter(proficiency != 'NA') %>%
  group_by(study) %>%
  mutate(proficiency = scale(proficiency, scale = FALSE)) %>%
  ungroup()

# facet labels

groups <- c(`english` = 'ENS', `korean` = 'KLE', `mandarin` = 'MLE')

# define data for plots

p1 <- ggplot(data=filter(plot, study == '210510_do'), aes(x=proficiency, y=acc_rate*100))
p2 <- ggplot(data=filter(plot, study == '210510_su'), aes(x=proficiency, y=acc_rate*100))

# generate plot

s <- list(
  geom_smooth(method=lm, col="#785ef0"), 
  geom_point(shape = 1),
  theme_classic(),
  scale_x_continuous(name='proficiency'),
  scale_y_continuous(name="% accuracy", limits = c(0, 100)),
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
# proficiency - accuracy ----
#------------------------------------------------------------------------------#

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
# + plot ----
#------------------------------------------------------------------------------#

plot <- md %>%
  group_by(study, group, dependency, environment, participant, proficiency) %>%
  summarise(accuracy = mean(accuracy)*100) %>%
  ungroup()

p1 <- ggplot(data=filter(plot, study == '210510_do'), aes(x=proficiency, y=accuracy, group = environment, col = environment, fill = environment, linetype = environment))
p2 <- ggplot(data=filter(plot, study == '210510_su'), aes(x=proficiency, y=accuracy, group = environment, col = environment, fill = environment, linetype = environment))

# generate plot

s <- list(
  geom_point(alpha = .1, shape = 16, size = 2),
  geom_smooth(method=lm, lwd = 1, alpha = .2), 
  theme_classic(),
  scale_x_continuous(name='proficiency'),
  scale_y_continuous(name='accuracy rate'),
  scale_linetype_manual(values = c('longdash', 'solid', 'twodash')),
  scale_color_manual(values = c('#619CFF', '#00BA38', '#F8766D')),
  scale_fill_manual(values = c('#619CFF', '#00BA38', '#F8766D')),
  guides(color = guide_legend(override.aes = list(shape = NA))),
  theme(text = element_text(size = 12),
        plot.title = element_text(size = 12, hjust = .5), 
        legend.key.size = unit(2,"line"),
        legend.position = 'right'),
  facet_grid2(vars(dependency), vars(group), axes = 'all', remove_labels = 'y',
              labeller = as_labeller(c(`gap` = 'gap', `pronoun` = 'RP', `korean` = 'KLE', `mandarin` = 'MLE')))
)

p1 + s
ggsave("plots/orc/spr_accuracy_proficiency_effect.png", width=6.5, height=3.5, dpi=600)

p2 + s
ggsave("plots/src/spr_accuracy_proficiency_effect.png", width=6.5, height=3.5, dpi=600)

#------------------------------------------------------------------------------#
# + lmer - accuracy rate ----
#------------------------------------------------------------------------------#

model1 <- lmer(accuracy ~ proficiency * dependency * environment * group + 
                  (1 | participant),
                data = filter(plot, study == '210510_do')) 
summary(model1)

# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)
# (Intercept)                                  9.122e-01  1.097e-02  1.290e+02  83.183  < 2e-16
# proficiency                                  6.085e-03  1.188e-03  1.290e+02   5.121 1.08e-06 ***
# dependency2                                  8.333e-02  1.201e-02  6.450e+02   6.941 9.53e-12 ***
# environment2                                -6.632e-03  1.470e-02  6.450e+02  -0.451 0.652115
# environment3                                -2.378e-03  1.470e-02  6.450e+02  -0.162 0.871549
# group2                                       6.682e-02  2.193e-02  1.290e+02   3.047 0.002806 **
# proficiency:dependency2                     -4.496e-03  1.301e-03  6.450e+02  -3.456 0.000584 ***
# proficiency:environment2                     2.114e-03  1.593e-03  6.450e+02   1.327 0.185120
# proficiency:environment3                     1.069e-03  1.593e-03  6.450e+02   0.671 0.502517
# dependency2:environment2                     1.580e-02  2.941e-02  6.450e+02   0.537 0.591383
# dependency2:environment3                     3.628e-03  2.941e-02  6.450e+02   0.123 0.901845
# proficiency:group2                          -1.816e-03  2.377e-03  1.290e+02  -0.764 0.446301
# dependency2:group2                          -9.081e-02  2.401e-02  6.450e+02  -3.782 0.000170 ***
# environment2:group2                         -1.644e-02  2.941e-02  6.450e+02  -0.559 0.576240
# environment3:group2                         -5.101e-02  2.941e-02  6.450e+02  -1.735 0.083280 .
# proficiency:dependency2:environment2         1.066e-03  3.187e-03  6.450e+02   0.335 0.737995
# proficiency:dependency2:environment3        -2.962e-03  3.187e-03  6.450e+02  -0.929 0.353065
# proficiency:dependency2:group2               1.868e-04  2.602e-03  6.450e+02   0.072 0.942796
# proficiency:environment2:group2             -6.297e-03  3.187e-03  6.450e+02  -1.976 0.048586 *
# proficiency:environment3:group2             -8.860e-03  3.187e-03  6.450e+02  -2.780 0.005589 **
# dependency2:environment2:group2             -5.782e-03  5.882e-02  6.450e+02  -0.098 0.921727
# dependency2:environment3:group2              3.716e-02  5.882e-02  6.450e+02   0.632 0.527771
# proficiency:dependency2:environment2:group2  3.028e-03  6.373e-03  6.450e+02   0.475 0.634832
# proficiency:dependency2:environment3:group2 -1.319e-04  6.373e-03  6.450e+02  -0.021 0.983490

trends <- emtrends(model1, ~ group * environment * dependency, var = 'proficiency', adjust = 'mvt')
test(trends)

# group    environment dependency proficiency.trend      SE  df t.ratio p.value
# korean   short       gap                 0.005144 0.00268 556   1.919  0.4610
# mandarin short       gap                 0.008770 0.00263 556   3.332  0.0108 *
# korean   long        gap                 0.010630 0.00268 556   3.966  0.0010 **
# mandarin long        gap                 0.006445 0.00263 556   2.449  0.1524
# korean   island      gap                 0.012091 0.00268 556   4.511  0.0001 ***
# mandarin island      gap                 0.006923 0.00263 556   2.630  0.0952
# korean   short       pronoun             0.001668 0.00268 556   0.622  0.9998
# mandarin short       pronoun             0.004516 0.00263 556   1.716  0.6225
# korean   long        pronoun             0.006707 0.00268 556   2.502  0.1333
# mandarin long        pronoun             0.004771 0.00263 556   1.813  0.5444
# korean   island      pronoun             0.005720 0.00268 556   2.134  0.3101
# mandarin island      pronoun            -0.000359 0.00263 556  -0.136  1.0000
# Degrees-of-freedom method: kenward-roger 
# P value adjustment: mvt method for 12 tests

# plot ...

plot <- emmip(model1, environment ~ proficiency | group ~ dependency, cov.reduce = range, CIs = TRUE, plotit = FALSE) %>%
  mutate(yvar = yvar *100,
         LCL = LCL * 100,
         UCL = UCL * 100)

ggplot(plot, aes(x = proficiency, y = yvar, group = environment, col = environment, linetype = environment)) +
  theme_classic() +
  geom_ribbon(aes(ymin = LCL, ymax = UCL, fill = environment), alpha = .1, col = NA) +
  geom_line(lwd = .75) +
  scale_y_continuous(name='accuracy rate') +
  scale_x_continuous(name='mean-centered proficiency score') +
  scale_linetype_manual(values = c('longdash', 'solid', 'twodash')) +
  scale_color_manual(values = c('#619CFF', '#00BA38', '#F8766D')) +
  scale_fill_manual(values = c('#619CFF', '#00BA38', '#F8766D')) +
  theme(legend.key.size = unit(2,"line")) +
  facet_grid2(vars(dependency), vars(group), axes = 'all', remove_labels = 'y',
              labeller = as_labeller(c(`gap` = 'gap', `pronoun` = 'RP', `korean` = 'KLE', `mandarin` = 'MLE')))

ggsave("plots/orc/spr_accuracy_proficiency_emmeans_md1.png", width=6.5, height=3, dpi=600)

#------------------------------------------------------------------------------#
# + glmer - accuracy ----
#------------------------------------------------------------------------------#

# fit models ...

tic()
model1 <- glmer(accuracy ~ proficiency * group + 
                  (1 | participant) + 
                  (1 | item), 
                data = filter(md, study == '210510_do'), family = binomial, 
                control = glmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_orc_proficiency_acc_md1.rds')
summary(model1)
toc()

# 71.232 sec elapsed
# Fixed effects:
#                    Estimate Std. Error z value Pr(>|z|)    
# (Intercept)         3.71321    0.22463  16.530  < 2e-16 ***
# proficiency         0.11224    0.02216   5.064 4.11e-07 ***
# group2              1.34763    0.41397   3.255  0.00113 ** 
# proficiency:group2 -0.01808    0.04412  -0.410  0.68196 

test(emtrends(model1, ~ group, var = 'proficiency', adjust = 'mvt'))

# group    proficiency.trend     SE  df z.ratio p.value
# korean               0.121 0.0313 Inf   3.869  0.0002 ***
# mandarin             0.103 0.0312 Inf   3.308  0.0019 **

tic()
model2 <- glmer(accuracy ~ proficiency * environment * group + 
                  (1 | participant) + 
                  (1 | item), 
                data = filter(md, study == '210510_do', dependency == 'gap'), family = binomial, 
                control = glmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_orc_proficiency_acc_md2.rds')
summary(model2)
toc()

# 461.832 sec elapsed
# Model failed to converge with max|grad| = 0.00278253 (tol = 0.002, component 1)

tic()
model3 <- glmer(accuracy ~ proficiency * dependency * environment * group + 
                  (1 | participant) + 
                  (1 | item), 
                data = filter(md, study == '210510_do'), family = binomial, 
                control = glmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=1e6))) %>%
  write_rds('models/spr_orc_proficiency_acc_md3.rds')
summary(model3)
toc()

# 3406.738 sec elapsed (56.78 min)
# Model failed to converge with max|grad| = 0.00625195 (tol = 0.002, component 1)

#==============================================================================#
# ::::: acceptability judgment task (ajt) ::::::::::::::::::::::::::::::::: ----
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
  mutate(panel = case_when(group == 'english' & task == 'english_ajt' ~ 'EE',
                           group == 'korean' & task == 'english_ajt' ~ 'KE',
                           group == 'korean' & task == 'korean_ajt' ~ 'KK',
                           group == 'mandarin' & task == 'english_ajt' ~ 'ME',
                           group == 'mandarin' & task == 'mandarin_ajt' ~ 'MM')) %>%
  mutate(panel = factor(panel, levels = c('EE', 
                                          'KE', 
                                          'ME', 
                                          'KK', 
                                          'MM')))

p1 <- ggplot(data=filter(plot, study == '210510_do'), 
             aes(x=environment, y=mean, group=dependency, col=dependency, shape=dependency))

p2 <- ggplot(data=filter(plot, study == '210510_su'), 
             aes(x=environment, y=mean, group=dependency, col=dependency, shape=dependency))

design <- "
 ABC
 #DE
"

s <- list(
  annotate("rect", 
           xmin = 0, xmax = 4, 
           ymin = 1+(5/3), 
           ymax = 1+(5/3)+(5/3), 
           alpha = .1),
  geom_hline(yintercept = 3.5),
  geom_line(lwd = 1, position = position_dodge(width = .4)),
  geom_point(size = 3, position = position_dodge(width = .4)),
  geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci), 
                width = 0, lwd = 5, linetype = 1, alpha = .5,
                position = position_dodge(width = .4)),
  theme_classic(),
  scale_x_discrete(name='environment', 
                   limits = c('short', 'long', 'island'), 
                   expand = c(0, 0)),
  scale_y_continuous(name='rating', 
                     limits=c(1, 6), breaks = c(1, 2, 3, 4, 5, 6)),
  scale_colour_manual(name='dependency', 
                      values=c('#648fff', '#ffb000'), 
                      labels=c('gap', 'resumption')),
  scale_shape_manual(name='dependency', 
                     values=c(16, 15), 
                     labels=c('gap', 'resumption')),
  theme(text = element_text(size = 12),
        legend.position = c(.15, .22),
        legend.margin = margin(-10, 0, 0, 0),
        axis.title.y = element_text(hjust = .88),
        plot.title = element_text(size = 12, hjust = .5)),
  facet_manual(~panel, design = design, axes = 'all', remove_labels = FALSE,
               labeller = as_labeller(c(`EE` = 'ENS in English', 
                                        `KE` = 'KLE in English', 
                                        `ME` = 'MLE in English', 
                                        `KK` = 'KLE in Korean', 
                                        `MM` = 'MLE in Mandarin')))
)

p1 + s
ggsave("plots/orc/ajt_crit_rating.png", width=6.5, height=3.5, dpi=600)

p2 + s
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

ajt_crit <- ajt %>%
  filter(!condition %in% c('grammatical', 'ungrammatical'))

# write to csv

write_csv(ajt_crit, 'data/ajt_crit.csv')

# read in data ...

ajt_crit <- read_csv('data/ajt_crit.csv')

# create panel factor ...

temp <- ajt_crit %>% 
  mutate(panel = case_when(task == 'english_ajt' & group == 'english' ~ 'EE',
                           task == 'english_ajt' & group == 'korean' ~ 'KE',
                           task == 'english_ajt' & group == 'mandarin' ~ 'ME',
                           task == 'korean_ajt' & group == 'korean' ~ 'KK',
                           task == 'mandarin_ajt' & group == 'mandarin' ~ 'MM')) %>%
  mutate(panel = fct_relevel(panel, 'EE', 'KE', 'ME', 'KK', 'MM'))

# adjust column classes ...

temp <- temp %>%
  mutate(environment = fct_relevel(environment, 'short', 'long', 'island'),
         dependency = factor(dependency)) %>%
  mutate(response = ordered(response)) %>%
  mutate(panel = factor(panel))

#------------------------------------------------------------------------------#
# + orc ----
#------------------------------------------------------------------------------#

# filter for analysis ...

md <- temp %>%
  filter(study == '210510_do')

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

# 274.33 sec elapsed or 5 min (vs. 433.928 or 7 min on AWS)
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
  summary(by = NULL, adjust = 'mvt')
pairwise

# plot ...

plot <- plot(emmeans(model1, ~ dependency * environment * panel), comparisons = TRUE, adjust = 'mvt', plotit = FALSE)

ggplot(plot, aes(x = environment, y = the.emmean, group = dependency, col = dependency, shape = dependency)) +
  geom_hline(yintercept = 0) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0, lwd = 5, alpha = .5, position = position_dodge(width = .4)) +
  geom_errorbar(aes(ymin = case_when(is.na(lcmpl)~the.emmean, TRUE ~ lcmpl), ymax = case_when(is.na(rcmpl)~the.emmean, TRUE ~ rcmpl)), width = 0, lwd = 1, position = position_dodge(width = .4)) +
  geom_line(lwd = 1, position = position_dodge(width = .4)) +
  geom_point(size = 3, position = position_dodge(width = .4)) +
  theme_classic() +
  scale_y_continuous(name="log odds ratio") +
  scale_colour_manual(name="dependency", values=c('#648fff', '#ffb000'), labels=c("gap", "resumption")) +
  scale_shape_manual(name="dependency", values=c(16, 15), labels=c("gap", "resumption")) +
  theme(text = element_text(size = 12), 
        legend.position = c(.15, .22),
        legend.margin = margin(-10, 0, 0, 0),
        axis.title.y = element_text(hjust = .99)) +
  facet_manual(~panel, design = design, axes = 'all', remove_labels = FALSE,
               labeller = as_labeller(c(`enen` = 'ENS on English AJT', `enko` = 'KLE on English AJT', `enzh` = 'MLE on English AJT', `koko` = 'KLE on Korean AJT', `zhzh` = 'MLE on Mandarin AJT')))

ggsave("plots/orc/ajt_clmm_emmeans_md1.png", width=6.5, height=3.5, dpi=600)

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

# 1454.01 sec elapsed (24 min)
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

# 28112.36 sec elapsed (7.8 hours)
# Warning message:
#   In summary.clmm(model5) :
#   Variance-covariance matrix of the parameters is not defined
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)
# dependency2                      -2.4046        NaN     NaN      NaN
# environment2                     -0.3681        NaN     NaN      NaN
# environment3                     -0.6062        NaN     NaN      NaN
# panel2                            0.2915        NaN     NaN      NaN
# panel3                            0.6861        NaN     NaN      NaN
# panel4                            0.2288        NaN     NaN      NaN
# panel5                           -0.4738        NaN     NaN      NaN
# dependency2:environment2          1.6645        NaN     NaN      NaN
# dependency2:environment3          3.1379        NaN     NaN      NaN
# dependency2:panel2                1.7121        NaN     NaN      NaN
# dependency2:panel3                2.3946        NaN     NaN      NaN
# dependency2:panel4                1.7669        NaN     NaN      NaN
# dependency2:panel5                3.2831        NaN     NaN      NaN
# environment2:panel2               0.5630        NaN     NaN      NaN
# environment3:panel2               1.9527        NaN     NaN      NaN
# environment2:panel3               0.3020        NaN     NaN      NaN
# environment3:panel3               1.1691        NaN     NaN      NaN
# environment2:panel4              -0.2482        NaN     NaN      NaN
# environment3:panel4               1.0691        NaN     NaN      NaN
# environment2:panel5               0.8917        NaN     NaN      NaN
# environment3:panel5               1.1052        NaN     NaN      NaN
# dependency2:environment2:panel2  -0.7221        NaN     NaN      NaN
# dependency2:environment3:panel2  -2.7847        NaN     NaN      NaN
# dependency2:environment2:panel3  -1.0849        NaN     NaN      NaN
# dependency2:environment3:panel3  -4.0947        NaN     NaN      NaN
# dependency2:environment2:panel4   2.2912        NaN     NaN      NaN
# dependency2:environment3:panel4  -0.2114        NaN     NaN      NaN
# dependency2:environment2:panel5  -0.8185        NaN     NaN      NaN
# dependency2:environment3:panel5  -3.6346        NaN     NaN      NaN

anova(model4, model5)

#        no.par   AIC logLik LR.stat df Pr(>Chisq)    
# model4     40 29972 -14946                          
# model5     76 29683 -14765  361.71 36  < 2.2e-16 ***

#------------------------------------------------------------------------------#
# + + model 6 (final?) ----
#------------------------------------------------------------------------------#

# fit model ...

tic()
model6 <- clmm(response ~ dependency * environment * panel + 
                 (1 + dependency + environment | participant) + 
                 (1 + dependency + environment + panel | item), 
               data = md, control = clmm.control(grtol = 1e6)) %>%
  write_rds('models/ajt_all_clmm_md6.rds')
summary(model6)
toc()
beep()

model6 <- read_rds('models/ajt_all_clmm_md6.rds')

# 10731.28 sec elapsed (3 hrs)
# no warnings
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# dependency2                     -2.28126    0.15198 -15.011  < 2e-16 ***
# environment2                    -0.34102    0.07187  -4.745 2.08e-06 ***
# environment3                    -0.55578    0.10166  -5.467 4.57e-08 ***
# panel2                           0.34309    0.20636   1.663 0.096400 .  
# panel3                           0.72471    0.20152   3.596 0.000323 ***
# panel4                           0.26209    0.20628   1.271 0.203887    
# panel5                          -0.40738    0.20066  -2.030 0.042341 *  
# dependency2:environment2         1.41178    0.10141  13.922  < 2e-16 ***
# dependency2:environment3         2.68990    0.10290  26.142  < 2e-16 ***
# dependency2:panel2               1.58180    0.35465   4.460 8.19e-06 ***
# dependency2:panel3               2.25319    0.34535   6.524 6.83e-11 ***
# dependency2:panel4               1.72373    0.35465   4.860 1.17e-06 ***
# dependency2:panel5               3.08311    0.34400   8.963  < 2e-16 ***
# environment2:panel2              0.55655    0.18675   2.980 0.002880 ** 
# environment3:panel2              1.95069    0.25006   7.801 6.14e-15 ***
# environment2:panel3              0.30553    0.18303   1.669 0.095051 .  
# environment3:panel3              1.12355    0.24323   4.619 3.85e-06 ***
# environment2:panel4             -0.19053    0.18980  -1.004 0.315453    
# environment3:panel4              1.10825    0.25166   4.404 1.06e-05 ***
# environment2:panel5              0.83265    0.17877   4.658 3.20e-06 ***
# environment3:panel5              1.03602    0.23991   4.318 1.57e-05 ***
# dependency2:environment2:panel2 -0.51623    0.32355  -1.596 0.110598    
# dependency2:environment3:panel2 -2.58791    0.32335  -8.003 1.21e-15 ***
# dependency2:environment2:panel3 -0.78281    0.31547  -2.481 0.013087 *  
# dependency2:environment3:panel3 -3.54231    0.31494 -11.248  < 2e-16 ***
# dependency2:environment2:panel4  2.29248    0.33013   6.944 3.81e-12 ***
# dependency2:environment3:panel4 -0.05329    0.33049  -0.161 0.871900    
# dependency2:environment2:panel5 -0.52933    0.30639  -1.728 0.084058 .  
# dependency2:environment3:panel5 -3.18627    0.30599 -10.413  < 2e-16 ***

anova(model4, model6)

# no.par   AIC logLik LR.stat df Pr(>Chisq)    
# model4     40 29972 -14946                          
# model6     65 29810 -14840  212.54 25  < 2.2e-16 ***

# tables ...

summary(model6)$coefficients %>%
  kbl(digits = c(2, 2, 2, 3))

pairwise %>%
  kbl(digits = c(2, 2, 2, 2, 2, 2, 2, 2, 3)) %>%
  remove_column(7)

# plot ...

plot <- plot(emmeans(model6, ~ dependency * environment * panel), comparisons = TRUE, adjust = 'mvt', plotit = FALSE)

ggplot(plot, aes(x = environment, y = the.emmean, group = dependency, col = dependency, shape = dependency)) +
  geom_hline(yintercept = 0) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0, lwd = 5, alpha = .5, position = position_dodge(width = .4)) +
  geom_errorbar(aes(ymin = case_when(is.na(lcmpl)~the.emmean, TRUE ~ lcmpl), ymax = case_when(is.na(rcmpl)~the.emmean, TRUE ~ rcmpl)), width = 0, lwd = 1, position = position_dodge(width = .4)) +
  geom_line(lwd = 1, position = position_dodge(width = .4)) +
  geom_point(size = 3, position = position_dodge(width = .4)) +
  theme_classic() +
  scale_y_continuous(name="log odds ratio") +
  scale_colour_manual(name="dependency", values=c('#648fff', '#ffb000'), labels=c("gap", "resumption")) +
  scale_shape_manual(name="dependency", values=c(16, 15), labels=c("gap", "resumption")) +
  theme(text = element_text(size = 12), 
        legend.position = c(.15, .22),
        legend.margin = margin(-10, 0, 0, 0),
        axis.title.y = element_text(hjust = .99)) +
  facet_manual(~panel, design = design, axes = 'all', remove_labels = FALSE,
               labeller = as_labeller(c(`enen` = 'ENS in English', `enko` = 'KLE in English', `enzh` = 'MLE in English', `koko` = 'KLE in Korean', `zhzh` = 'MLE in Mandarin')))

ggsave("plots/orc/ajt_clmm_emmeans_md6.png", width=6.5, height=3.5, dpi=600)

#------------------------------------------------------------------------------#
# + + model 7
#------------------------------------------------------------------------------#

# fit model ...

tic()
model7 <- clmm(response ~ dependency * environment * panel + 
                 (1 + dependency * environment | participant) + 
                 (1 + dependency * environment * panel | item), 
               data = md, control = clmm.control(grtol = 1e6)) %>%
  write_rds('models/ajt_all_clmm_md7.rds')
summary(model7)
toc()
beep()

# 33003.27 sec elapsed
# no warnings
# Fixed effects:
#   Estimate Std. Error       df t value Pr(>|t|)   
# (Intercept)               -35.334     11.693   62.829  -3.022  0.00363 **
# dependency2               -20.115     12.008   63.928  -1.675  0.09880 . 
# environment2                5.977     12.456 1762.890   0.480  0.63139   
# environment3               11.585     12.416 1762.828   0.933  0.35092   
# dependency2:environment2   12.150     24.965 1768.577   0.487  0.62654   
# dependency2:environment3   29.717     24.827 1751.635   1.197  0.23150 
# (stopped it part-way through because it was taking too long)

#------------------------------------------------------------------------------#
# + src ----
#------------------------------------------------------------------------------#

# filter for analysis ...

md <- temp %>%
  filter(study == '210510_su')

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
  write_rds('models/ajt_suall_clmm_md1.rds')
summary(model1)
toc()
beep()

# 309.92 sec elapsed (5.17 min)
# no warnings
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# dependency2                     -1.42085    0.04374 -32.483  < 2e-16 ***
# environment2                    -0.63089    0.05367 -11.754  < 2e-16 ***
# environment3                    -1.35344    0.05474 -24.726  < 2e-16 ***
# panel2                           0.47379    0.20433   2.319  0.02041 *  
# panel3                           1.06206    0.20069   5.292 1.21e-07 ***
# panel4                           0.93949    0.20470   4.590 4.44e-06 ***
# panel5                           1.18149    0.20019   5.902 3.59e-09 ***
# dependency2:environment2         3.11433    0.10927  28.502  < 2e-16 ***
# dependency2:environment3         5.05002    0.11454  44.088  < 2e-16 ***
# dependency2:panel2               2.01759    0.15019  13.434  < 2e-16 ***
# dependency2:panel3               3.71111    0.15017  24.712  < 2e-16 ***
# dependency2:panel4               1.07206    0.15127   7.087 1.37e-12 ***
# dependency2:panel5               2.40435    0.14602  16.466  < 2e-16 ***
# environment2:panel2              0.28609    0.18756   1.525  0.12718    
# environment3:panel2              1.84679    0.19050   9.695  < 2e-16 ***
# environment2:panel3              0.02192    0.18393   0.119  0.90511    
# environment3:panel3              1.24511    0.18739   6.645 3.04e-11 ***
# environment2:panel4              0.59403    0.19211   3.092  0.00199 ** 
# environment3:panel4              2.43975    0.19530  12.493  < 2e-16 ***
# environment2:panel5              0.42504    0.18172   2.339  0.01933 *  
# environment3:panel5              1.86025    0.18472  10.071  < 2e-16 ***
# dependency2:environment2:panel2  0.34970    0.37570   0.931  0.35196    
# dependency2:environment3:panel2 -2.63469    0.38475  -6.848 7.50e-12 ***
# dependency2:environment2:panel3  0.49224    0.36844   1.336  0.18154    
# dependency2:environment3:panel3 -3.93647    0.37914 -10.383  < 2e-16 ***
# dependency2:environment2:panel4  1.86856    0.38554   4.847 1.26e-06 ***
# dependency2:environment3:panel4 -3.45240    0.39366  -8.770  < 2e-16 ***
# dependency2:environment2:panel5 -0.24296    0.36389  -0.668  0.50434    
# dependency2:environment3:panel5 -4.74176    0.37385 -12.684  < 2e-16 ***

#------------------------------------------------------------------------------#
# + + model 2
#------------------------------------------------------------------------------#

# fit model ...

tic()
model2 <- clmm(response ~ dependency * environment * panel + 
                 (1 + dependency | participant) + 
                 (1 | item), 
               data = md, control = clmm.control(grtol = 1e6)) %>%
  write_rds('models/ajt_suall_clmm_md2.rds')
summary(model2)
toc()
beep()

# 833.28 sec elapsed (13.89 min)
# no warnings
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# dependency2                     -1.53351    0.09556 -16.048  < 2e-16 ***
# environment2                    -0.63585    0.05444 -11.679  < 2e-16 ***
# environment3                    -1.37708    0.05643 -24.405  < 2e-16 ***
# panel2                           0.56680    0.21442   2.643  0.00821 ** 
# panel3                           1.17387    0.21068   5.572 2.52e-08 ***
# panel4                           1.06030    0.21497   4.932 8.13e-07 ***
# panel5                           1.30106    0.21024   6.189 6.07e-10 ***
# dependency2:environment2         3.23537    0.11077  29.207  < 2e-16 ***
# dependency2:environment3         5.27068    0.11778  44.752  < 2e-16 ***
# dependency2:panel2               2.16471    0.25742   8.409  < 2e-16 ***
# dependency2:panel3               3.98515    0.25497  15.630  < 2e-16 ***
# dependency2:panel4               1.18050    0.25851   4.567 4.96e-06 ***
# dependency2:panel5               2.61264    0.25216  10.361  < 2e-16 ***
# environment2:panel2              0.24935    0.18910   1.319  0.18731    
# environment3:panel2              1.83066    0.19543   9.367  < 2e-16 ***
# environment2:panel3             -0.01697    0.18563  -0.091  0.92715    
# environment3:panel3              1.19832    0.19233   6.231 4.65e-10 ***
# environment2:panel4              0.58691    0.19449   3.018  0.00255 ** 
# environment3:panel4              2.44910    0.20076  12.199  < 2e-16 ***
# environment2:panel5              0.39933    0.18362   2.175  0.02965 *  
# environment3:panel5              1.84517    0.18986   9.719  < 2e-16 ***
# dependency2:environment2:panel2  0.51794    0.37853   1.368  0.17122    
# dependency2:environment3:panel2 -2.51225    0.39129  -6.420 1.36e-10 ***
# dependency2:environment2:panel3  0.57068    0.37150   1.536  0.12451    
# dependency2:environment3:panel3 -4.00218    0.38560 -10.379  < 2e-16 ***
# dependency2:environment2:panel4  2.03307    0.38978   5.216 1.83e-07 ***
# dependency2:environment3:panel4 -3.42534    0.40119  -8.538  < 2e-16 ***
# dependency2:environment2:panel5 -0.20752    0.36744  -0.565  0.57222    
# dependency2:environment3:panel5 -4.80077    0.38075 -12.609  < 2e-16 ***

anova(model1, model2)

# model1     36 26983 -13455                          
# model2     38 26585 -13254  402.28  2  < 2.2e-16 ***

#------------------------------------------------------------------------------#
# + + model 3
#------------------------------------------------------------------------------#

# fit model ...

tic()
model3 <- clmm(response ~ dependency * environment * panel + 
                 (1 + environment | participant) + 
                 (1 | item), 
               data = md, control = clmm.control(grtol = 1e6)) %>%
  write_rds('models/ajt_suall_clmm_md3.rds')
summary(model3)
toc()
beep()

# 1680.64 sec elapsed (28.01 min)
# no warnings
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# dependency2                     -1.47214    0.04551 -32.350  < 2e-16 ***
# environment2                    -0.66275    0.09691  -6.839 7.97e-12 ***
# environment3                    -1.47833    0.10914 -13.545  < 2e-16 ***
# panel2                           0.51697    0.21507   2.404   0.0162 *  
# panel3                           1.15565    0.21135   5.468 4.55e-08 ***
# panel4                           0.99597    0.21536   4.625 3.75e-06 ***
# panel5                           1.27559    0.21084   6.050 1.45e-09 ***
# dependency2:environment2         3.18963    0.11546  27.625  < 2e-16 ***
# dependency2:environment3         5.24891    0.11984  43.800  < 2e-16 ***
# dependency2:panel2               2.08084    0.15524  13.404  < 2e-16 ***
# dependency2:panel3               3.84965    0.15584  24.703  < 2e-16 ***
# dependency2:panel4               1.10332    0.15579   7.082 1.42e-12 ***
# dependency2:panel5               2.48384    0.15123  16.425  < 2e-16 ***
# environment2:panel2              0.23619    0.27065   0.873   0.3828    
# environment3:panel2              1.89744    0.29914   6.343 2.25e-10 ***
# environment2:panel3             -0.04880    0.26666  -0.183   0.8548    
# environment3:panel3              1.24324    0.29501   4.214 2.51e-05 ***
# environment2:panel4              0.64246    0.27312   2.352   0.0187 *  
# environment3:panel4              2.60413    0.30202   8.622  < 2e-16 ***
# environment2:panel5              0.42990    0.26459   1.625   0.1042    
# environment3:panel5              1.93794    0.29262   6.623 3.53e-11 ***
# dependency2:environment2:panel2  0.44571    0.39685   1.123   0.2614    
# dependency2:environment3:panel2 -2.75968    0.39701  -6.951 3.62e-12 ***
# dependency2:environment2:panel3  0.69617    0.39189   1.776   0.0757 .  
# dependency2:environment3:panel3 -4.05907    0.39321 -10.323  < 2e-16 ***
# dependency2:environment2:panel4  2.00849    0.40433   4.968 6.78e-07 ***
# dependency2:environment3:panel4 -3.70916    0.40497  -9.159  < 2e-16 ***
# dependency2:environment2:panel5 -0.08499    0.38661  -0.220   0.8260    
# dependency2:environment3:panel5 -4.89667    0.38698 -12.653  < 2e-16 ***

anova(model1, model3)

# model1     36 26983 -13455                          
# model3     41 26633 -13275  360.17  5  < 2.2e-16 ***

#------------------------------------------------------------------------------#
# + + model 4
#------------------------------------------------------------------------------#

# fit model ...

tic()
model4 <- clmm(response ~ dependency * environment * panel + 
                 (1 | participant) + 
                 (1 + dependency | item), 
               data = md, control = clmm.control(grtol = 1e6)) %>%
  write_rds('models/ajt_suall_clmm_md4.rds')
summary(model4)
toc()
beep()

# 2017.05 sec elapsed (33.62 min)
# no warnings
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# dependency2                     -1.42285    0.04752 -29.943  < 2e-16 ***
# environment2                    -0.63290    0.05371 -11.783  < 2e-16 ***
# environment3                    -1.35637    0.05479 -24.754  < 2e-16 ***
# panel2                           0.47523    0.20477   2.321  0.02030 *  
# panel3                           1.06110    0.20111   5.276 1.32e-07 ***
# panel4                           0.93903    0.20514   4.578 4.70e-06 ***
# panel5                           1.18150    0.20061   5.889 3.88e-09 ***
# dependency2:environment2         3.11889    0.10938  28.514  < 2e-16 ***
# dependency2:environment3         5.05718    0.11472  44.081  < 2e-16 ***
# dependency2:panel2               2.01773    0.15025  13.429  < 2e-16 ***
# dependency2:panel3               3.71094    0.15020  24.706  < 2e-16 ***
# dependency2:panel4               1.07040    0.15135   7.072 1.52e-12 ***
# dependency2:panel5               2.40391    0.14607  16.457  < 2e-16 ***
# environment2:panel2              0.28301    0.18772   1.508  0.13164    
# environment3:panel2              1.84963    0.19062   9.703  < 2e-16 ***
# environment2:panel3              0.01527    0.18407   0.083  0.93387    
# environment3:panel3              1.23967    0.18745   6.613 3.76e-11 ***
# environment2:panel4              0.59164    0.19229   3.077  0.00209 ** 
# environment3:panel4              2.44323    0.19539  12.505  < 2e-16 ***
# environment2:panel5              0.42430    0.18182   2.334  0.01961 *  
# environment3:panel5              1.85903    0.18479  10.060  < 2e-16 ***
# dependency2:environment2:panel2  0.35329    0.37603   0.940  0.34745    
# dependency2:environment3:panel2 -2.63994    0.38498  -6.857 7.01e-12 ***
# dependency2:environment2:panel3  0.50509    0.36879   1.370  0.17081    
# dependency2:environment3:panel3 -3.93006    0.37931 -10.361  < 2e-16 ***
# dependency2:environment2:panel4  1.88760    0.38605   4.890 1.01e-06 ***
# dependency2:environment3:panel4 -3.45347    0.39386  -8.768  < 2e-16 ***
# dependency2:environment2:panel5 -0.24011    0.36406  -0.660  0.50955    
# dependency2:environment3:panel5 -4.74304    0.37398 -12.683  < 2e-16 ***

anova(model1, model4)

# no.par   AIC logLik LR.stat df Pr(>Chisq)
# model1     36 26983 -13455                      
# model4     38 26984 -13454  2.6226  2     0.2695

#------------------------------------------------------------------------------#
# + + model 5
#------------------------------------------------------------------------------#

# fit model ...

tic()
model5 <- clmm(response ~ dependency * environment * panel + 
                 (1 | participant) + 
                 (1 + environment | item), 
               data = md, control = clmm.control(grtol = 1e6)) %>%
  write_rds('models/ajt_suall_clmm_md5.rds')
summary(model5)
toc()
beep()

# 1941.85 sec elapsed (32.36 min)
# no warnings
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# dependency2                     -1.42408    0.04382 -32.501  < 2e-16 ***
# environment2                    -0.63041    0.05722 -11.017  < 2e-16 ***
# environment3                    -1.35344    0.06680 -20.261  < 2e-16 ***
# panel2                           0.47633    0.20463   2.328  0.01992 *  
# panel3                           1.06372    0.20101   5.292 1.21e-07 ***
# panel4                           0.94215    0.20501   4.596 4.31e-06 ***
# panel5                           1.18308    0.20048   5.901 3.61e-09 ***
# dependency2:environment2         3.12161    0.10958  28.486  < 2e-16 ***
# dependency2:environment3         5.06011    0.11485  44.059  < 2e-16 ***
# dependency2:panel2               2.01364    0.15028  13.400  < 2e-16 ***
# dependency2:panel3               3.71515    0.15027  24.722  < 2e-16 ***
# dependency2:panel4               1.07272    0.15140   7.086 1.38e-12 ***
# dependency2:panel5               2.40483    0.14608  16.462  < 2e-16 ***
# environment2:panel2              0.28569    0.18773   1.522  0.12805    
# environment3:panel2              1.85112    0.19063   9.710  < 2e-16 ***
# environment2:panel3              0.02363    0.18416   0.128  0.89789    
# environment3:panel3              1.24674    0.18753   6.648 2.96e-11 ***
# environment2:panel4              0.58986    0.19232   3.067  0.00216 ** 
# environment3:panel4              2.43678    0.19543  12.469  < 2e-16 ***
# environment2:panel5              0.42370    0.18184   2.330  0.01980 *  
# environment3:panel5              1.85753    0.18481  10.051  < 2e-16 ***
# dependency2:environment2:panel2  0.35731    0.37633   0.949  0.34240    
# dependency2:environment3:panel2 -2.63032    0.38521  -6.828 8.59e-12 ***
# dependency2:environment2:panel3  0.49101    0.36883   1.331  0.18310    
# dependency2:environment3:panel3 -3.94519    0.37946 -10.397  < 2e-16 ***
# dependency2:environment2:panel4  1.89026    0.38639   4.892 9.98e-07 ***
# dependency2:environment3:panel4 -3.42012    0.39475  -8.664  < 2e-16 ***
# dependency2:environment2:panel5 -0.23587    0.36419  -0.648  0.51720    
# dependency2:environment3:panel5 -4.74090    0.37407 -12.674  < 2e-16 ***

anova(model1, model5)

# no.par   AIC logLik LR.stat df Pr(>Chisq)
# model1     36 26983 -13455                      
# model5     41 26989 -13454   3.669  5      0.598

#------------------------------------------------------------------------------#
# + + model 6
#------------------------------------------------------------------------------#

# fit model ...

tic()
model6 <- clmm(response ~ dependency * environment * panel + 
                 (1 | participant) + 
                 (1 + panel | item), 
               data = md, control = clmm.control(grtol = 1e6)) %>%
  write_rds('models/ajt_suall_clmm_md6.rds')
summary(model6)
toc()
beep()

# 1830.93 sec elapsed (30.52 min)
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# dependency2                     -1.42961    0.04395 -32.530  < 2e-16 ***
# environment2                    -0.63156    0.05384 -11.730  < 2e-16 ***
# environment3                    -1.35660    0.05493 -24.697  < 2e-16 ***
# panel2                           0.47843    0.20653   2.317  0.02053 *  
# panel3                           1.06986    0.20554   5.205 1.94e-07 ***
# panel4                           0.94708    0.20795   4.554 5.26e-06 ***
# panel5                           1.19183    0.20821   5.724 1.04e-08 ***
# dependency2:environment2         3.12635    0.10965  28.511  < 2e-16 ***
# dependency2:environment3         5.07254    0.11500  44.109  < 2e-16 ***
# dependency2:panel2               2.03345    0.15097  13.469  < 2e-16 ***
# dependency2:panel3               3.73765    0.15092  24.765  < 2e-16 ***
# dependency2:panel4               1.08185    0.15207   7.114 1.13e-12 ***
# dependency2:panel5               2.41288    0.14681  16.436  < 2e-16 ***
# environment2:panel2              0.29864    0.18821   1.587  0.11257    
# environment3:panel2              1.85992    0.19108   9.734  < 2e-16 ***
# environment2:panel3              0.02611    0.18476   0.141  0.88761    
# environment3:panel3              1.26041    0.18840   6.690 2.23e-11 ***
# environment2:panel4              0.60876    0.19263   3.160  0.00158 ** 
# environment3:panel4              2.46237    0.19593  12.568  < 2e-16 ***
# environment2:panel5              0.44841    0.18252   2.457  0.01402 *  
# environment3:panel5              1.87799    0.18543  10.128  < 2e-16 ***
# dependency2:environment2:panel2  0.31650    0.37710   0.839  0.40130    
# dependency2:environment3:panel2 -2.66958    0.38623  -6.912 4.78e-12 ***
# dependency2:environment2:panel3  0.47642    0.36981   1.288  0.19765    
# dependency2:environment3:panel3 -3.96899    0.38076 -10.424  < 2e-16 ***
# dependency2:environment2:panel4  1.85267    0.38699   4.787 1.69e-06 ***
# dependency2:environment3:panel4 -3.47748    0.39527  -8.798  < 2e-16 ***
# dependency2:environment2:panel5 -0.26516    0.36538  -0.726  0.46802    
# dependency2:environment3:panel5 -4.77313    0.37552 -12.711  < 2e-16 ***

anova(model1, model6)

# no.par   AIC logLik LR.stat df Pr(>Chisq)
# model1     36 26983 -13455                      
# model6     50 26993 -13447  17.682 14     0.2216

#------------------------------------------------------------------------------#
# + + model 7 (final?) ----
#------------------------------------------------------------------------------#

# fit model ...

tic()
model7 <- clmm(response ~ dependency * environment * panel + 
                 (1 + dependency + environment | participant) + 
                 (1 + dependency + environment + panel | item), 
               data = md, control = clmm.control(grtol = 1e6)) %>%
  write_rds('models/ajt_suall_clmm_md7.rds')
summary(model7)
toc()
beep()

# 23904.05 sec elapsed (6.64 hours)
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# dependency2                     -1.61025    0.10660 -15.106  < 2e-16 ***
# environment2                    -0.66637    0.10577  -6.300 2.98e-10 ***
# environment3                    -1.52287    0.12411 -12.271  < 2e-16 ***
# panel2                           0.61877    0.22717   2.724  0.00645 ** 
# panel3                           1.26828    0.22827   5.556 2.76e-08 ***
# panel4                           1.12877    0.22986   4.911 9.08e-07 ***
# panel5                           1.40765    0.22933   6.138 8.35e-10 ***
# dependency2:environment2         3.31736    0.11900  27.876  < 2e-16 ***
# dependency2:environment3         5.46688    0.12550  43.562  < 2e-16 ***
# dependency2:panel2               2.26788    0.27295   8.309  < 2e-16 ***
# dependency2:panel3               4.20352    0.27049  15.540  < 2e-16 ***
# dependency2:panel4               1.23790    0.27415   4.515 6.32e-06 ***
# dependency2:panel5               2.73294    0.26744  10.219  < 2e-16 ***
# environment2:panel2              0.23639    0.28303   0.835  0.40360    
# environment3:panel2              1.93791    0.31685   6.116 9.59e-10 ***
# environment2:panel3             -0.05653    0.27932  -0.202  0.83960    
# environment3:panel3              1.23569    0.31254   3.954 7.69e-05 ***
# environment2:panel4              0.66753    0.28626   2.332  0.01971 *  
# environment3:panel4              2.65741    0.32027   8.298  < 2e-16 ***
# environment2:panel5              0.46966    0.27720   1.694  0.09021 .  
# environment3:panel5              1.98157    0.31007   6.391 1.65e-10 ***
# dependency2:environment2:panel2  0.68100    0.40216   1.693  0.09039 .  
# dependency2:environment3:panel2 -2.53735    0.40675  -6.238 4.43e-10 ***
# dependency2:environment2:panel3  0.87486    0.39668   2.205  0.02742 *  
# dependency2:environment3:panel3 -3.98220    0.40270  -9.889  < 2e-16 ***
# dependency2:environment2:panel4  2.34233    0.41257   5.677 1.37e-08 ***
# dependency2:environment3:panel4 -3.46841    0.41742  -8.309  < 2e-16 ***
# dependency2:environment2:panel5 -0.01723    0.39174  -0.044  0.96491    
# dependency2:environment3:panel5 -4.86149    0.39659 -12.258  < 2e-16 *** 

anova(model1, model7)

# no.par   AIC logLik LR.stat df Pr(>Chisq)    
# model1     36 26983 -13455                          
# model7     80 26248 -13044  822.72 44  < 2.2e-16 ***

# plot ...

plot <- plot(emmeans(model7, ~ dependency * environment * panel), comparisons = TRUE, adjust = 'mvt', plotit = FALSE)

ggplot(plot, aes(x = environment, y = the.emmean, group = dependency, col = dependency, shape = dependency)) +
  geom_hline(yintercept = 0) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0, lwd = 5, alpha = .5, position = position_dodge(width = .4)) +
  geom_errorbar(aes(ymin = case_when(is.na(lcmpl)~the.emmean, TRUE ~ lcmpl), ymax = case_when(is.na(rcmpl)~the.emmean, TRUE ~ rcmpl)), width = 0, lwd = 1, position = position_dodge(width = .4)) +
  geom_line(lwd = 1, position = position_dodge(width = .4)) +
  geom_point(size = 3, position = position_dodge(width = .4)) +
  theme_classic() +
  scale_y_continuous(name="log odds ratio") +
  scale_colour_manual(name="dependency", values=c('#648fff', '#ffb000'), labels=c("gap", "resumption")) +
  scale_shape_manual(name="dependency", values=c(16, 15), labels=c("gap", "resumption")) +
  theme(text = element_text(size = 12), 
        legend.position = c(.15, .22),
        legend.margin = margin(-10, 0, 0, 0),
        axis.title.y = element_text(hjust = .99)) +
  facet_manual(~panel, design = design, axes = 'all', remove_labels = FALSE,
               labeller = as_labeller(c(`enen` = 'ENS on English AJT', `enko` = 'KLE on English AJT', `enzh` = 'MLE on English AJT', `koko` = 'KLE on Korean AJT', `zhzh` = 'MLE on Mandarin AJT')))

ggsave("plots/src/ajt_clmm_emmeans_md7.png", width=6.5, height=3.5, dpi=600)

#------------------------------------------------------------------------------#
# interaction plots for fillers ----
#------------------------------------------------------------------------------#

# filter to filler trials ...

fill <- ajt %>% filter(condition %in% c('grammatical', 'ungrammatical'))

# create 'set' factor ...

fill <- fill %>%
  mutate(set = case_when(item %in% c('item31', 'item32', 'item33', 'item34', 'item35', 'item36', 'item37') ~ 'set1',
                         item %in% c('item38', 'item39', 'item40', 'item41', 'item42', 'item43', 'item44') ~ 'set2',
                         item %in% c('item45', 'item46', 'item47', 'item48', 'item49', 'item50', 'item51') ~ 'set3',
                         item %in% c('item52', 'item53', 'item54', 'item55', 'item56', 'item57', 'item58') ~ 'set4',
                         item %in% c('item59', 'item60', 'item61', 'item62', 'item63', 'item64', 'item65') ~ 'set5',
                         item %in% c('item66', 'item67', 'item68', 'item69', 'item70', 'item71', 'item72') ~ 'set6'))

# create 'superset' factor ...

fill <- fill %>%
  mutate(superset = case_when(set %in% c('set1', 'set2') ~ 'superset1',
                              set %in% c('set3', 'set4') ~ 'superset2',
                              set == 'set5' ~ 'superset3',
                              set == 'set6' ~ 'superset4'))

# summarize for plotting ...

plot <- fill %>%
  mutate(condition = fct_drop(condition),
         set = fct_drop(superset)) %>%
  group_by(study, group, task, superset, condition) %>%
  summarise(mean = mean(response, na.rm=T),
            sd = sd(response, na.rm=T),
            n = n()) %>%
  mutate(se = sd / sqrt(n),
         ci = qt(1 - (0.05 / 2), n - 1) * se) %>%
  ungroup() %>%
  filter(condition != 'NA') %>%
  mutate(panel = case_when(group == 'english' & task == 'english_ajt' ~ 'ENS on English AJT',
                           group == 'korean' & task == 'english_ajt' ~ 'KLE on English AJT',
                           group == 'korean' & task == 'korean_ajt' ~ 'KLE on Korean AJT',
                           group == 'mandarin' & task == 'english_ajt' ~ 'MLE on English AJT',
                           group == 'mandarin' & task == 'mandarin_ajt' ~ 'MLE on Mandarin AJT')) %>%
  mutate(panel = fct_relevel(panel, 'ENS on English AJT', 'KLE on English AJT', 'MLE on English AJT', 'KLE on Korean AJT', 'MLE on Mandarin AJT'))

# generate plot ...

p1 <- ggplot(data=filter(plot, study == '210510_do'), aes(x=superset, y=mean, group=condition, col=condition, shape=condition))
p2 <- ggplot(data=filter(plot, study == '210510_su'), aes(x=superset, y=mean, group=condition, col=condition, shape=condition))

design <- "
 ABC
 #DE
"

s <- list(
  annotate('rect', 
           xmin = .5, xmax = 4.5, 
           ymin = 1+(5/3), 
           ymax = 1+(5/3)+(5/3), 
           alpha = .1),
  geom_hline(yintercept = 3.5),
  geom_line(lwd = 1, position = position_dodge(width = .4)),
  geom_point(size = 3, position = position_dodge(width = .4)),
  geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci), 
                width = 0, lwd = 5, linetype = 1, alpha = .5,
                position = position_dodge(width = .4)),
  theme_classic(),
  scale_x_discrete(name="type", 
                   limits = c("superset1", "superset2", "superset3", "superset4"), 
                   labels = c("F1", "F2", "F3", "F4"),
                   expand = c(0, 0)),
  scale_y_continuous(name='rating', 
                     limits=c(1, 6), breaks = c(1, 2, 3, 4, 5, 6)),
  scale_colour_manual(name='condition', 
                      values=c('#40B0A6', '#E1BE6A'), 
                      labels=c('grammatical', 'ungrammatical')),
  scale_shape_manual(name='condition', 
                     values=c(17, 18), 
                     labels=c('grammatical', 'ungrammatical')),
  theme(text = element_text(size = 12),
        legend.position = c(.15, .22),
        legend.margin = margin(-10, 0, 0, 0),
        axis.title.y = element_text(hjust = .88),
        plot.title = element_text(size = 12, hjust = .5)),
  facet_manual(~panel, design = design, axes = 'all', remove_labels = FALSE)
)

p1 + s
ggsave("plots/orc/ajt_filler_rating.png", width=6.5, height=3.5, dpi=600)

p2 + s
ggsave("plots/src/ajt_filler_rating.png", width=6.5, height=3.5, dpi=600)

#------------------------------------------------------------------------------#
# modeling - clmm - filler ----
#------------------------------------------------------------------------------#

# prep data ...

ajt_filler <- ajt %>%
  filter(condition %in% c('grammatical', 'ungrammatical'))

# create 'panel' factor ...

ajt_filler <- ajt_filler %>% 
  mutate(panel = case_when(task == 'english_ajt' & group == 'english' ~ 'EE',
                           task == 'english_ajt' & group == 'korean' ~ 'KE',
                           task == 'english_ajt' & group == 'mandarin' ~ 'ME',
                           task == 'korean_ajt' & group == 'korean' ~ 'KK',
                           task == 'mandarin_ajt' & group == 'mandarin' ~ 'MM'))

# create 'set' factor ...

ajt_filler <- ajt_filler %>%
  mutate(set = case_when(item %in% c('item31', 'item32', 'item33', 'item34', 'item35', 'item36', 'item37') ~ 'set1',
                         item %in% c('item38', 'item39', 'item40', 'item41', 'item42', 'item43', 'item44') ~ 'set2',
                         item %in% c('item45', 'item46', 'item47', 'item48', 'item49', 'item50', 'item51') ~ 'set3',
                         item %in% c('item52', 'item53', 'item54', 'item55', 'item56', 'item57', 'item58') ~ 'set4',
                         item %in% c('item59', 'item60', 'item61', 'item62', 'item63', 'item64', 'item65') ~ 'set5',
                         item %in% c('item66', 'item67', 'item68', 'item69', 'item70', 'item71', 'item72') ~ 'set6'))

# create 'superset' factor ...

ajt_filler <- ajt_filler %>%
  mutate(superset = case_when(set %in% c('set1', 'set2') ~ 'superset1',
                              set %in% c('set3', 'set4') ~ 'superset2',
                              set == 'set5' ~ 'superset3',
                              set == 'set6' ~ 'superset4'))

# write to csv ...

write_csv(ajt_filler, 'data/ajt_filler.csv')

# read in data ...

ajt_filler <- read_csv('data/ajt_filler.csv')

# adjust column classes ...

ajt_filler <- ajt_filler %>%
  mutate_at(c('condition', 'superset', 'panel', 'participant', 'item'), as.factor) %>%
  mutate(response = ordered(response)) %>%
  mutate(panel = fct_relevel(panel, 'EE', 'KE', 'ME', 'KK', 'MM'))

class(ajt_filler$condition)

#------------------------------------------------------------------------------#
# + orc ----
#------------------------------------------------------------------------------#

# filter for analysis ...

md <- ajt_filler %>%
  filter(study == '210510_do',
         superset %in% c('superset1', 'superset2')) %>%
  mutate(superset = fct_drop(superset))

# apply deviation coding ...

contrasts(md$condition) <- contr.treatment(2) - matrix(rep(1/2, 2), ncol = 1)
contrasts(md$superset) <- contr.treatment(2) - matrix(rep(1/2, 2), ncol = 1)
contrasts(md$panel) <- contr.treatment(5) - matrix(rep(1/5, 20), ncol = 4)

# view contrasts ...

contrasts(md$condition)
contrasts(md$superset)
contrasts(md$panel)

#------------------------------------------------------------------------------#
# + + model 1 
#------------------------------------------------------------------------------#

# fit model ...

tic()
model1 <- clmm(response ~ condition * superset * panel + 
                 (1 | participant) + 
                 (1 | item), 
               data = md, control = clmm.control(grtol = 1e6)) %>%
  write_rds('models/ajt_orc_filler_clmm_md1.rds')
summary(model1)
toc()
beep()

# 142.26 sec elapsed
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# condition2                  -3.75820    0.12447 -30.193  < 2e-16 ***
# superset2                    0.77001    0.11976   6.430 1.28e-10 ***
# panel2                       0.85312    0.15960   5.346 9.02e-08 ***
# panel3                       1.03728    0.15682   6.614 3.73e-11 ***
# panel4                      -0.85141    0.15970  -5.331 9.75e-08 ***
# panel5                      -0.10191    0.15453  -0.660 0.509574    
# condition2:superset2         0.97008    0.23944   4.052 5.09e-05 ***
# condition2:panel2            3.26849    0.16652  19.629  < 2e-16 ***
# condition2:panel3            3.14597    0.16679  18.862  < 2e-16 ***
# condition2:panel4            0.97805    0.16761   5.835 5.37e-09 ***
# condition2:panel5            1.51144    0.15999   9.447  < 2e-16 ***
# superset2:panel2             0.80421    0.15364   5.234 1.65e-07 ***
# superset2:panel3             0.97867    0.15436   6.340 2.30e-10 ***
# superset2:panel4            -0.54265    0.15532  -3.494 0.000476 ***
# superset2:panel5            -0.95350    0.14768  -6.457 1.07e-10 ***
# condition2:superset2:panel2  1.76502    0.30729   5.744 9.26e-09 ***
# condition2:superset2:panel3  0.56217    0.30801   1.825 0.067979 .  
# condition2:superset2:panel4 -0.70559    0.31063  -2.271 0.023119 *  
# condition2:superset2:panel5  0.08883    0.29513   0.301 0.763416 

# plot ...

plot <- plot(emmeans(model1, ~ condition * superset * panel), comparisons = TRUE, adjust = 'mvt', plotit = FALSE)

ggplot(plot, aes(x = superset, y = the.emmean, group = condition, col = condition, shape = condition)) +
  geom_hline(yintercept = 0) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0, lwd = 5, alpha = .5, position = position_dodge(width = .4)) +
  geom_errorbar(aes(ymin = case_when(is.na(lcmpl)~the.emmean, TRUE ~ lcmpl), ymax = case_when(is.na(rcmpl)~the.emmean, TRUE ~ rcmpl)), width = 0, lwd = 1, position = position_dodge(width = .4)) +
  geom_line(lwd = 1, position = position_dodge(width = .4)) +
  geom_point(size = 3, position = position_dodge(width = .4)) +
  theme_classic() +
  scale_y_continuous(name="log odds ratio") +
  scale_colour_manual(name="condition", values=c('#40B0A6', '#E1BE6A'), labels=c("grammatical", "ungrammatical")) +
  scale_shape_manual(name="condition", values=c(17, 18), labels=c("grammatical", "ungrammatical")) +
  theme(text = element_text(size = 12), 
        legend.position = c(.15, .22),
        legend.margin = margin(-10, 0, 0, 0),
        axis.title.y = element_text(hjust = .99)) +
  facet_manual(~panel, design = design, axes = 'all', remove_labels = FALSE,
               labeller = as_labeller(c(`EE` = 'ENS on English AJT', `KE` = 'KLE on English AJT', `ME` = 'MLE on English AJT', `KK` = 'KLE on Korean AJT', `MM` = 'MLE on Mandarin AJT')))

ggsave("plots/orc/ajt_filler_clmm_emmeans_md1.png", width=6.5, height=3.5, dpi=600)

#------------------------------------------------------------------------------#
# density plots for ajt critical trials ----
#------------------------------------------------------------------------------#

# density plot of z-score data ...

density <- crit %>%
  filter(!condition %in% c('grammatical', 'ungrammatical')) %>%
  mutate(condition = fct_drop(condition),
         environment = fct_relevel(environment, 'short', 'long', 'island')) %>%
  mutate(panel = case_when(group == 'english' & task == 'english_ajt' ~ 'ENS\nEnglish AJT',
                           group == 'korean' & task == 'english_ajt' ~ 'KLE\nEnglish AJT',
                           group == 'mandarin' & task == 'english_ajt' ~ 'MLE\nEnglish AJT',
                           group == 'korean' & task == 'korean_ajt' ~ 'KLE\nKorean AJT',
                           group == 'mandarin' & task == 'mandarin_ajt' ~ 'MLE\nMandarin AJT')) %>%
  mutate(panel = fct_relevel(panel, 'ENS\nEnglish AJT', 'KLE\nEnglish AJT', 'MLE\nEnglish AJT', 'KLE\nKorean AJT', 'MLE\nMandarin AJT'))

p1 <- ggplot(data = filter(density, study == '210510_do'), aes(x = zscore, fill = dependency))
p2 <- ggplot(data = filter(density, study == '210510_su'), aes(x = zscore, fill = dependency))

s <- list(
  geom_vline(xintercept=0),
  geom_density(alpha=.7, col = NA),
  theme_classic(),
  scale_x_continuous(name="z-score", limits = c(-3, 3)),
  scale_fill_manual(name="dependency", values=c('#648fff', '#ffb000'), labels=c("gap", "RP")),
  theme(text = element_text(size = 12), 
        plot.title = element_text(size = 12, hjust = .5), 
        legend.position = "bottom", 
        legend.margin=margin(0, 0, 0, 0),
        legend.box.margin = margin(-10, -10, 0, -10)),
  facet_grid(environment ~ panel)
)

p1 + s
ggsave('plots/orc/ajt_density_crit_zscore.png', width=6.5, height=3.5, dpi=600)

p2 + s
ggsave('plots/orc/ajt_density_crit_zscore.png', width=6.5, height=3.5, dpi=600)



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

# summarise ...

plot <- ajt %>%
  mutate(panel = case_when(group == 'english' & task == 'english_ajt' ~ 'ENS\nEnglish AJT',
                           group == 'korean' & task == 'english_ajt' ~ 'KLE\nEnglish AJT',
                           group == 'korean' & task == 'korean_ajt' ~ 'KLE\nKorean AJT',
                           group == 'mandarin' & task == 'english_ajt' ~ 'MLE\nEnglish AJT',
                           group == 'mandarin' & task == 'mandarin_ajt' ~ 'MLE\nMandarin AJT'))

plot2 <- plot %>%
  group_by(study, panel, response) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  group_by(study, panel) %>%
  mutate(sum = sum(n)) %>%
  ungroup() %>%
  mutate(percent = n / sum * 100)

# plot ...

ggplot(data = filter(plot2, study == '210510_do'), aes(x = response, y = percent)) +
  geom_bar(stat = 'identity', col = 'black', fill = NA, width = 1) +
  theme_classic() +
  scale_x_continuous(name="rating", breaks = c(1, 2, 3, 4, 5, 6)) +
  scale_y_continuous(name="% responses", limits = c(0, 60)) +
  theme(text = element_text(size = 12),
      plot.title = element_text(size = 12, hjust = .5),
      legend.position = "bottom", 
      legend.margin=margin(0, 0, 0, 0),
      legend.box.margin = margin(-10, -10, 0, -10)) +
  facet_wrap(~panel, ncol = 5)

ggsave("plots/orc/ajt_rating_distribution.png", width=6.5, height=2, dpi=600)

ggplot(data = filter(plot2, study == '210510_su'), aes(x = response, y = percent)) +
  geom_bar(stat = 'identity', col = 'black', fill = NA, width = 1) +
  theme_classic() +
  scale_x_continuous(name="rating", breaks = c(1, 2, 3, 4, 5, 6)) +
  scale_y_continuous(name="% responses", limits = c(0, 60)) +
  theme(text = element_text(size = 12),
        plot.title = element_text(size = 12, hjust = .5),
        legend.position = "bottom", 
        legend.margin=margin(0, 0, 0, 0),
        legend.box.margin = margin(-10, -10, 0, -10)) +
  facet_wrap(~panel, ncol = 5)

ggsave("plots/src/ajt_rating_distribution.png", width=6.5, height=2, dpi=600)

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

#------------------------------------------------------------------------------#
# rating distribution plot ----
#------------------------------------------------------------------------------#

# read in data ...

ajt_crit <- read_csv('data/ajt_crit.csv')

# create panel factor ...

temp <- ajt_crit %>% 
  mutate(panel = case_when(task == 'english_ajt' & group == 'english' ~ 'EE',
                           task == 'english_ajt' & group == 'korean' ~ 'KE',
                           task == 'english_ajt' & group == 'mandarin' ~ 'ME',
                           task == 'korean_ajt' & group == 'korean' ~ 'KK',
                           task == 'mandarin_ajt' & group == 'mandarin' ~ 'MM')) %>%
  mutate(panel = fct_relevel(panel, 'EE', 'KE', 'ME', 'KK', 'MM'))

# select columns ...

temp <- temp %>%
  select(study, group, panel, participant, item, dependency, environment, response)

# adjust column classes ...

temp <- temp %>%
  mutate(environment = fct_relevel(environment, 'short', 'long', 'island')) %>%
  mutate_at(c('dependency', 'item', 'panel', 'participant', 'group', 'study'), as.factor) %>%
  mutate(response = as.numeric(response)) %>%
  mutate(response_fct = factor(response))

summary(temp)

plot <- temp %>%
  group_by(study, panel, participant, dependency, environment) %>%
  summarise(mean = mean(response, na.rm = TRUE),
            sd = sd(response, na.rm = TRUE)) %>%
  ungroup()

# plot by dependency

p1 <- ggplot(data=filter(plot, study == '210510_do'), aes(x=mean, y=sd, group = dependency, col = dependency, fill = dependency, linetype = dependency))
p2 <- ggplot(data=filter(plot, study == '210510_su'), aes(x=mean, y=sd, group = dependency, col = dependency, fill = dependency, linetype = dependency))

s <- list(
  geom_point(alpha = .1, shape = 16, size = 2),
  stat_smooth(method = "loess", formula = y ~ x, size = 1),
  theme_classic(),
  scale_x_continuous(name='mean rating', breaks = c(1, 3.5, 6)),
  scale_y_continuous(name='standard deviation'),
  scale_linetype_manual(values = c('longdash', 'solid')),
  scale_color_manual(values = c('#648fff', '#ffb000')),
  scale_fill_manual(values = c('#648fff', '#ffb000')),
  guides(color = guide_legend(override.aes = list(shape = NA))),
  theme(text = element_text(size = 12),
        plot.title = element_text(size = 12, hjust = .5), 
        legend.key.size = unit(2,"line"),
        legend.position = 'right'),
  facet_grid2(vars(environment), vars(panel), axes = 'all', remove_labels = FALSE,
              labeller = as_labeller(c(`short` = 'short', `long` = 'long', `island` = 'island',
                                       `EE` = 'ENS in\nEnglish', `KE` = 'KLE in\nEnglish', `ME` = 'MLE in\nEnglish', `KK` = 'KLE in\nKorean', `MM` = 'MLE in\nMandarin')))
)

p1 + s
ggsave("plots/orc/ajt_orc_rating_sd_environment.png", width=6.5, height=3.5, dpi=600)

p2 + s
ggsave("plots/src/ajt_orc_rating_sd_environment.png", width=6.5, height=3.5, dpi=600)


#------------------------------------------------------------------------------#
# standard deviation plot ----
#------------------------------------------------------------------------------#

# read in data ...

ajt_crit <- read_csv('data/ajt_crit.csv')

# create panel factor ...

temp <- ajt_crit %>% 
  mutate(panel = case_when(task == 'english_ajt' & group == 'english' ~ 'EE',
                           task == 'english_ajt' & group == 'korean' ~ 'KE',
                           task == 'english_ajt' & group == 'mandarin' ~ 'ME',
                           task == 'korean_ajt' & group == 'korean' ~ 'KK',
                           task == 'mandarin_ajt' & group == 'mandarin' ~ 'MM')) %>%
  mutate(panel = fct_relevel(panel, 'EE', 'KE', 'ME', 'KK', 'MM'))

# select columns ...

temp <- temp %>%
  select(study, group, panel, participant, item, dependency, environment, response)

# adjust column classes ...

temp <- temp %>%
  mutate(environment = fct_relevel(environment, 'short', 'long', 'island')) %>%
  mutate_at(c('dependency', 'item', 'panel', 'participant', 'group', 'study'), as.factor) %>%
  mutate(response = as.numeric(response)) %>%
  mutate(response_fct = factor(response))

summary(temp)

plot <- temp %>%
  group_by(study, panel, participant, dependency, environment) %>%
  summarise(mean = mean(response, na.rm = TRUE),
            sd = sd(response, na.rm = TRUE)) %>%
  ungroup()

# plot by dependency

p1 <- ggplot(data=filter(plot, study == '210510_do'), aes(x=mean, y=sd, group = dependency, col = dependency, fill = dependency, linetype = dependency))
p2 <- ggplot(data=filter(plot, study == '210510_su'), aes(x=mean, y=sd, group = dependency, col = dependency, fill = dependency, linetype = dependency))

s <- list(
  geom_point(alpha = .1, shape = 16, size = 2),
  stat_smooth(method = "loess", formula = y ~ x, size = 1),
  theme_classic(),
  scale_x_continuous(name='mean rating', breaks = c(1, 3.5, 6)),
  scale_y_continuous(name='standard deviation'),
  scale_linetype_manual(values = c('longdash', 'solid')),
  scale_color_manual(values = c('#648fff', '#ffb000')),
  scale_fill_manual(values = c('#648fff', '#ffb000')),
  guides(color = guide_legend(override.aes = list(shape = NA))),
  theme(text = element_text(size = 12),
        plot.title = element_text(size = 12, hjust = .5), 
        legend.key.size = unit(2,"line"),
        legend.position = 'right'),
  facet_grid2(vars(environment), vars(panel), axes = 'all', remove_labels = FALSE,
              labeller = as_labeller(c(`short` = 'short', `long` = 'long', `island` = 'island',
                                       `EE` = 'ENS in\nEnglish', `KE` = 'KLE in\nEnglish', `ME` = 'MLE in\nEnglish', `KK` = 'KLE in\nKorean', `MM` = 'MLE in\nMandarin')))
)

p1 + s
ggsave("plots/orc/ajt_orc_rating_sd_environment.png", width=6.5, height=3.5, dpi=600)

p2 + s
ggsave("plots/src/ajt_orc_rating_sd_environment.png", width=6.5, height=3.5, dpi=600)

# plot by dependency

p1 <- ggplot(data=filter(plot, study == '210510_do'), aes(x=mean, y=sd, group = environment, col = environment, fill = environment, linetype = environment))
p2 <- ggplot(data=filter(plot, study == '210510_su'), aes(x=mean, y=sd, group = environment, col = environment, fill = environment, linetype = environment))

s <- list(
  geom_point(alpha = .1, shape = 16, size = 2),
  stat_smooth(method = "loess", formula = y ~ x, size = 1),
  #geom_smooth(method=lm, lwd = 1, alpha = .2), 
  theme_classic(),
  scale_x_continuous(name='rating'),
  scale_y_continuous(name='standard deviation'),
  scale_linetype_manual(values = c('longdash', 'solid', 'twodash')),
  scale_color_manual(values = c('#619CFF', '#00BA38', '#F8766D')),
  scale_fill_manual(values = c('#619CFF', '#00BA38', '#F8766D')),
  guides(color = guide_legend(override.aes = list(shape = NA))),
  theme(text = element_text(size = 12),
        plot.title = element_text(size = 12, hjust = .5), 
        legend.key.size = unit(2,"line"),
        legend.position = 'right'),
  facet_grid2(vars(dependency), vars(panel), axes = 'all', remove_labels = FALSE,
              labeller = as_labeller(c(`gap` = 'gap', `pronoun` = 'RP', 
                                       `EE` = 'ENS in\nEnglish', `KE` = 'KLE in\nEnglish', `ME` = 'MLE in\nEnglish', `KK` = 'KLE in\nKorean', `MM` = 'MLE in\nMandarin')))
)

p1 + s
ggsave("plots/orc/ajt_orc_rating_sd_dependency.png", width=6.5, height=3, dpi=600)

p2 + s
ggsave("plots/src/ajt_orc_rating_sd_dependency.png", width=6.5, height=3, dpi=600)

# plot of rating distributions by environment

plot <- temp %>%
  group_by(study, panel, participant, response, environment) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  group_by(study, panel, participant, response, environment) %>%
  mutate(sum = sum(n)) %>%
  ungroup() %>%
  mutate(percent = n / sum * 100)

p1 <- ggplot(data=filter(plot, study == '210510_do'), aes(x=response, y=sum, group = environment, col = environment, fill = environment, linetype = environment))
p2 <- ggplot(data=filter(plot, study == '210510_su'), aes(x=response, y=sum, group = environment, col = environment, fill = environment, linetype = environment))

s <- list(
  #geom_point(alpha = .1, shape = 16, size = 2),
  stat_smooth(method = "loess", formula = y ~ x, size = 1),
  theme_classic(),
  scale_x_continuous(name='rating'),
  scale_y_continuous(name='count'),
  scale_linetype_manual(values = c('longdash', 'solid', 'twodash')),
  scale_color_manual(values = c('#619CFF', '#00BA38', '#F8766D')),
  scale_fill_manual(values = c('#619CFF', '#00BA38', '#F8766D')),
  guides(color = guide_legend(override.aes = list(shape = NA))),
  theme(text = element_text(size = 12),
        plot.title = element_text(size = 12, hjust = .5), 
        legend.key.size = unit(2,"line"),
        legend.position = 'right'),
  facet_wrap(~panel, ncol = 5,
             labeller = as_labeller(c(`EE` = 'ENS in\nEnglish', `KE` = 'KLE in\nEnglish', `ME` = 'MLE in\nEnglish', `KK` = 'KLE in\nKorean', `MM` = 'MLE in\nMandarin')))
)

p1 + s
ggsave("plots/orc/ajt_orc_rating_distribution_environment.png", width=6.5, height=2, dpi=600)

p2 + s
ggsave("plots/src/ajt_orc_rating_distribution_environment.png", width=6.5, height=2, dpi=600)

# plot of rating distributions overall (averaged by participant)

plot <- temp %>%
  group_by(study, panel, participant) %>%
  mutate(tot = n()) %>%
  ungroup() %>%
  group_by(study, panel, participant, response, tot) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  group_by(study, panel, participant, response) %>%
  mutate(sum = sum(n),
         percent = sum(n) / tot * 100) %>%
  ungroup()

p1 <- ggplot(data=filter(plot, study == '210510_do'), aes(x=response, y=percent))
p2 <- ggplot(data=filter(plot, study == '210510_su'), aes(x=response, y=percent))

s <- list(
  stat_smooth(method = "loess", formula = y ~ x, size = 1),
  theme_classic(),
  scale_x_continuous(name='rating', breaks = c(1, 2, 3, 4, 5, 6)),
  scale_y_continuous(name='% responses'),
  guides(color = guide_legend(override.aes = list(shape = NA))),
  theme(text = element_text(size = 12),
        plot.title = element_text(size = 12, hjust = .5), 
        legend.key.size = unit(2,"line"),
        legend.position = 'right'),
  facet_wrap(~panel, ncol = 5,
             labeller = as_labeller(c(`EE` = 'ENS in\nEnglish', `KE` = 'KLE in\nEnglish', `ME` = 'MLE in\nEnglish', `KK` = 'KLE in\nKorean', `MM` = 'MLE in\nMandarin')))
)

p1 + s
ggsave("plots/orc/ajt_orc_rating_distribution.png", width=6.5, height=2, dpi=600)

p2 + s
ggsave("plots/src/ajt_orc_rating_distribution.png", width=6.5, height=2, dpi=600)

# plot of rating distributions overall

plot <- temp %>%
  group_by(study, panel) %>%
  mutate(tot = n()) %>%
  ungroup() %>%
  group_by(study, panel, response, tot) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  group_by(study, panel, response) %>%
  mutate(sum = sum(n),
         percent = sum(n) / tot * 100) %>%
  ungroup()

p1 <- ggplot(data=filter(temp, study == '210510_do'), aes(x=response, y=n))
p2 <- ggplot(data=filter(temp, study == '210510_su'), aes(x=response, y=percent))

s <- list(
  stat_smooth(method = "loess", formula = y ~ x, size = 1),
  theme_classic(),
  scale_x_continuous(name='rating', breaks = c(1, 2, 3, 4, 5, 6)),
  scale_y_continuous(name='percent'),
  guides(color = guide_legend(override.aes = list(shape = NA))),
  theme(text = element_text(size = 12),
        plot.title = element_text(size = 12, hjust = .5), 
        legend.key.size = unit(2,"line"),
        legend.position = 'right'),
  facet_wrap(~panel, ncol = 5,
             labeller = as_labeller(c(`EE` = 'ENS in\nEnglish', `KE` = 'KLE in\nEnglish', `ME` = 'MLE in\nEnglish', `KK` = 'KLE in\nKorean', `MM` = 'MLE in\nMandarin')))
)

p1 + s
ggsave("plots/orc/ajt_orc_rating_distribution_overall.png", width=6.5, height=2, dpi=600)

p2 + s
ggsave("plots/src/ajt_orc_rating_distribution_overall.png", width=6.5, height=2, dpi=600)


#==============================================================================#
# ::::: ajt data vs. ept data ::::::::::::::::::::::::::::::::::::::::::::: ----
#==============================================================================#

# insert here

#==============================================================================#
# ::::: ajt data vs. spr data ::::::::::::::::::::::::::::::::::::::::::::: ----
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
# ::::: ajt data vs. c-test data :::::::::::::::::::::::::::::::::::::::::: ----
#==============================================================================#

#------------------------------------------------------------------------------#
# modeling: proficiency - clmm - critical ----
#------------------------------------------------------------------------------#

# read in data ...

ajt_crit <- read_csv('data/ajt_crit.csv')

# create panel factor ...

md <- ajt_crit %>% 
  mutate(panel = case_when(task == 'english_ajt' & group == 'english' ~ 'EE',
                           task == 'english_ajt' & group == 'korean' ~ 'KE',
                           task == 'english_ajt' & group == 'mandarin' ~ 'ME',
                           task == 'korean_ajt' & group == 'korean' ~ 'KK',
                           task == 'mandarin_ajt' & group == 'mandarin' ~ 'MM')) %>%
  mutate(panel = fct_relevel(panel, 'EE', 'KE', 'ME', 'KK', 'MM'))

# add proficiency scores

md <- md %>%
  left_join(proficiency, by = c('study', 'group', 'participant')) %>%
  mutate(proficiency = scale(proficiency, scale = FALSE))

# select columns ...

md <- md %>%
  select(study, group, panel, participant, proficiency, item, dependency, environment, response)

# filter ...

md <- md %>%
  filter(panel %in% c('KE', 'ME')) %>%
  mutate(panel = fct_drop(panel))

# adjust column classes ...

md <- md %>%
  mutate(environment = fct_relevel(environment, 'short', 'long', 'island')) %>%
  mutate_at(c('dependency', 'item', 'panel', 'participant', 'group', 'study'), as.factor) %>%
  mutate(response = ordered(response))

summary(md)

#------------------------------------------------------------------------------#
# + plot ratings ----
#------------------------------------------------------------------------------#

plot <- md %>%
  group_by(study, panel, participant, dependency, environment, proficiency) %>%
  summarise(response = mean(as.numeric(response), na.rm = TRUE))

p1 <- ggplot(data=filter(plot, study == '210510_do'), aes(x=proficiency, y=response, group = environment, col = environment, fill = environment, linetype = environment))
p2 <- ggplot(data=filter(plot, study == '210510_su'), aes(x=proficiency, y=response, group = environment, col = environment, fill = environment, linetype = environment))

s <- list(
  geom_point(alpha = .1, shape = 16, size = 2),
  geom_smooth(method=lm, lwd = 1, alpha = .2), 
  theme_classic(),
  scale_x_continuous(name='mean-centered proficiency score'),
  scale_y_continuous(name='rating'),
  scale_linetype_manual(values = c('longdash', 'solid', 'twodash')),
  scale_color_manual(values = c('#619CFF', '#00BA38', '#F8766D')),
  scale_fill_manual(values = c('#619CFF', '#00BA38', '#F8766D')),
  guides(color = guide_legend(override.aes = list(shape = NA))),
  theme(text = element_text(size = 12),
        plot.title = element_text(size = 12, hjust = .5), 
        legend.key.size = unit(2,"line"),
        legend.position = 'right'),
  facet_grid2(vars(dependency), vars(panel), axes = 'all', remove_labels = FALSE,
              labeller = as_labeller(c(`gap` = 'gap', `pronoun` = 'RP', `KE` = 'KLE', `ME` = 'MLE')))
)

p1 + s
ggsave("plots/orc/ajt_orc_rating_proficiency_effect.png", width=6.5, height=2, dpi=600)

p2 + s
ggsave("plots/src/ajt_orc_rating_proficiency_effect.png", width=6.5, height=2, dpi=600)

#------------------------------------------------------------------------------#
# + clmm: orc ratings ----
#------------------------------------------------------------------------------#

# apply deviation coding ...

contrasts(md$dependency) <- contr.treatment(2) - matrix(rep(1/2, 2), ncol = 1)
contrasts(md$environment) <- contr.treatment(3) - matrix(rep(1/3, 6), ncol = 2)
contrasts(md$panel) <- contr.treatment(2) - matrix(rep(1/2, 2), ncol = 1)

# view contrasts ...

contrasts(md$dependency)
contrasts(md$environment)
contrasts(md$panel)

#------------------------------------------------------------------------------#
# + + model 1 
#------------------------------------------------------------------------------#

# fit model ...

tic()
model1 <- clmm(response ~ proficiency * environment * panel + 
                 (1 | participant) + 
                 (1 | item), 
               data = filter(md, study == '210510_do', dependency == 'pronoun'), 
               control = clmm.control(grtol = 1e6)) %>%
  write_rds('models/ajt_orc_proficiency_clmm_md1.rds')
summary(model1)
toc()
beep()

# 45.2 sec elapsed
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# proficiency                     -0.063650   0.025074  -2.538  0.01113 *  
# environment2                     0.211151   0.125213   1.686  0.09173 .  
# environment3                     0.968910   0.128425   7.545 4.54e-14 ***
# panel2                           1.090366   0.372909   2.924  0.00346 ** 
# proficiency:environment2         0.008346   0.014276   0.585  0.55882    
# proficiency:environment3         0.021507   0.014604   1.473  0.14083    
# proficiency:panel2               0.027460   0.044813   0.613  0.54004    
# environment2:panel2             -0.459063   0.249562  -1.839  0.06585 .  
# environment3:panel2             -1.686510   0.255371  -6.604 4.00e-11 ***
# proficiency:environment2:panel2 -0.010658   0.028131  -0.379  0.70479    
# proficiency:environment3:panel2 -0.075003   0.028803  -2.604  0.00921 ** 

#------------------------------------------------------------------------------#
# + + model 2 (final) ----
#------------------------------------------------------------------------------#

# fit model ...

tic()
model2 <- clmm(response ~ proficiency * environment * panel + 
                 (1 + environment | participant) + 
                 (1 | item), 
               data = filter(md, study == '210510_do', dependency == 'pronoun'), 
               control = clmm.control(grtol = 1e6)) %>%
  write_rds('models/ajt_orc_proficiency_clmm_md2.rds')
summary(model2)
toc()
beep()

# 173.83 sec elapsed
# no warnings
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# proficiency                     -0.069666   0.027398  -2.543  0.01100 *  
# environment2                     0.237384   0.194893   1.218  0.22322    
# environment3                     1.101228   0.221959   4.961 7.00e-07 ***
# panel2                           1.213751   0.470890   2.578  0.00995 ** 
# proficiency:environment2         0.007872   0.021555   0.365  0.71497    
# proficiency:environment3         0.021903   0.025019   0.875  0.38134    
# proficiency:panel2               0.031109   0.054658   0.569  0.56925    
# environment2:panel2             -0.570797   0.382258  -1.493  0.13538    
# environment3:panel2             -1.949877   0.438649  -4.445 8.78e-06 ***
# proficiency:environment2:panel2 -0.018321   0.042637  -0.430  0.66742    
# proficiency:environment3:panel2 -0.091164   0.049775  -1.832  0.06702 . 

anova(model1, model2)

# no.par    AIC  logLik LR.stat df Pr(>Chisq)    
# model1     18 5477.1 -2720.5                          
# model2     23 5425.5 -2689.7  61.576  5  5.738e-12 ***

trends <- emtrends(model2, ~ panel * environment, var = 'proficiency', adjust = 'mvt')
test(trends)

# panel environment proficiency.trend     SE  df z.ratio p.value
# KE    short                 -0.1134 0.0464 Inf  -2.445  0.0643 .
# ME    short                 -0.0458 0.0435 Inf  -1.053  0.7506
# KE    long                  -0.0964 0.0428 Inf  -2.252  0.1035
# ME    long                  -0.0471 0.0404 Inf  -1.166  0.6732
# KE    island                -0.0459 0.0417 Inf  -1.100  0.7190
# ME    island                -0.0695 0.0401 Inf  -1.733  0.3023
# P value adjustment: mvt method for 6 tests

# plot ...

plot <- emmip(model2, environment ~ proficiency | panel, cov.reduce = range, CIs = TRUE, plotit = FALSE)

ggplot(plot, aes(x = proficiency, y = yvar, group = environment, col = environment, linetype = environment)) +
  theme_classic() +
  geom_ribbon(aes(ymin = LCL, ymax = UCL, fill = environment), alpha = .1, col = NA) +
  geom_line(lwd = .75) +
  scale_y_continuous(name='log odds ratio') +
  scale_x_continuous(name='mean-centered proficiency score') +
  scale_linetype_manual(values = c('longdash', 'solid', 'twodash')) +
  scale_color_manual(values = c('#619CFF', '#00BA38', '#F8766D')) +
  scale_fill_manual(values = c('#619CFF', '#00BA38', '#F8766D')) +
  theme(legend.key.size = unit(2,"line")) +
  facet_wrap(~panel, labeller = as_labeller(c(`ME` = 'MLE', `KE` = 'KLE')))

ggsave("plots/orc/ajt_rp_rating_proficiency_emmeans_md2.png", width=6.5, height=2, dpi=600)

#------------------------------------------------------------------------------#
# + + model 3
#------------------------------------------------------------------------------#

# fit model ...

tic()
model3 <- clmm(response ~ proficiency * environment * panel + 
                 (1 + environment | participant) + 
                 (1 + environment | item), 
               data = filter(md, study == '210510_do', dependency == 'pronoun'), 
               control = clmm.control(grtol = 1e6)) %>%
  write_rds('models/ajt_orc_proficiency_clmm_md3.rds')
summary(model3)
toc()
beep()

# 439.66 sec elapsed
# singular fit, despite no warning
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# proficiency                     -0.069694   0.027438  -2.540   0.0111 *  
# environment2                     0.238037   0.195294   1.219   0.2229    
# environment3                     1.101656   0.225304   4.890 1.01e-06 ***
# panel2                           1.212026   0.471584   2.570   0.0102 *  
# proficiency:environment2         0.007897   0.021588   0.366   0.7145    
# proficiency:environment3         0.022097   0.025059   0.882   0.3779    
# proficiency:panel2               0.030967   0.054734   0.566   0.5715    
# environment2:panel2             -0.570555   0.382838  -1.490   0.1361    
# environment3:panel2             -1.962995   0.439710  -4.464 8.03e-06 ***
# proficiency:environment2:panel2 -0.018288   0.042701  -0.428   0.6685    
# proficiency:environment3:panel2 -0.091666   0.049826  -1.840   0.0658 . 

anova(model2, model3)

# no.par    AIC  logLik LR.stat df Pr(>Chisq)
# model2     23 5425.5 -2689.7                      
# model3     28 5435.1 -2689.5  0.3886  5     0.9956

#------------------------------------------------------------------------------#
# + + model 1 w/ 'dependency'
#------------------------------------------------------------------------------#

# fit model ...

tic()
model1 <- clmm(response ~ proficiency * dependency * environment * panel + 
                 (1 | participant) + 
                 (1 | item), 
               data = filter(md, study == '210510_do'), 
               control = clmm.control(grtol = 1e6)) %>%
  write_rds('models/ajt_orc_proficiency_clmm_full_md1.rds')
summary(model1)
toc()
beep()

# 133.41 sec elapsed
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# proficiency                                  0.003019   0.010926   0.276 0.782315    
# dependency2                                 -1.884259   0.071453 -26.371  < 2e-16 ***
# environment2                                -0.235993   0.084128  -2.805 0.005029 ** 
# environment3                                -0.182199   0.083037  -2.194 0.028222 *  
# panel2                                       0.238446   0.187679   1.271 0.203906  
# proficiency:dependency2                     -0.098535   0.007895 -12.481  < 2e-16 ***
# proficiency:environment2                    -0.009470   0.009571  -0.989 0.322430    
# proficiency:environment3                    -0.028423   0.009539  -2.980 0.002887 ** 
# dependency2:environment2                     0.733247   0.168354   4.355 1.33e-05 ***
# dependency2:environment3                     1.653619   0.166811   9.913  < 2e-16 ***
# proficiency:panel2                          -0.030378   0.021859  -1.390 0.164614    
# dependency2:panel2                           0.879427   0.136606   6.438 1.21e-10 ***
# environment2:panel2                         -0.201291   0.168180  -1.197 0.231354    
# environment3:panel2                         -0.471279   0.166103  -2.837 0.004550 ** 
# proficiency:dependency2:environment2         0.025422   0.019136   1.328 0.184017    
# proficiency:dependency2:environment3         0.074731   0.019045   3.924 8.71e-05 ***
# proficiency:dependency2:panel2               0.086638   0.015618   5.547 2.90e-08 ***
# proficiency:environment2:panel2             -0.009265   0.019141  -0.484 0.628349    
# proficiency:environment3:panel2              0.007641   0.019071   0.401 0.688653    
# dependency2:environment2:panel2             -0.179203   0.336353  -0.533 0.594185    
# dependency2:environment3:panel2             -1.249653   0.332090  -3.763 0.000168 ***
# proficiency:dependency2:environment2:panel2  0.023038   0.038276   0.602 0.547237    
# proficiency:dependency2:environment3:panel2 -0.092777   0.038075  -2.437 0.014822 *
 
trends <- emtrends(model1, ~ panel * dependency * environment, var = 'proficiency', adjust = 'mvt')
test(trends)

# panel dependency environment proficiency.trend     SE  df z.ratio p.value
# KE    gap        short                0.123999 0.0214 Inf   5.796  <.0001 ***
# ME    gap        short                0.039220 0.0200 Inf   1.956  0.3613
# KE    pronoun    short               -0.062862 0.0200 Inf  -3.138  0.0179 *
# ME    pronoun    short               -0.037757 0.0190 Inf  -1.989  0.3405
# KE    gap        long                 0.112210 0.0204 Inf   5.492  <.0001 ***
# ME    gap        long                 0.006647 0.0192 Inf   0.347  1.0000
# KE    pronoun    long                -0.060748 0.0199 Inf  -3.053  0.0232 *
# ME    pronoun    long                -0.033389 0.0187 Inf  -1.787  0.4786
# KE    gap        island               0.031195 0.0198 Inf   1.576  0.6387
# ME    gap        island               0.000446 0.0191 Inf   0.023  1.0000
# KE    pronoun    island              -0.034546 0.0199 Inf  -1.732  0.5191
# ME    pronoun    island              -0.048188 0.0188 Inf  -2.557  0.0969 .
# P value adjustment: mvt method for 12 tests

#------------------------------------------------------------------------------#
# + + model 2 w/ 'dependency'
#------------------------------------------------------------------------------#

# fit model ...

tic()
model2 <- clmm(response ~ proficiency * dependency * environment * panel + 
                 (1 + dependency | participant) + 
                 (1 | item), 
               data = filter(md, study == '210510_do'), 
               control = clmm.control(grtol = 1e6)) %>%
  write_rds('models/ajt_orc_proficiency_clmm_full_md2.rds')
summary(model2)
toc()
beep()

# 873.17 sec elapsed
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# proficiency                                  0.006762   0.014466   0.467 0.640203    
# dependency2                                 -2.963256   0.347320  -8.532  < 2e-16 ***
#   environment2                                -0.276590   0.094404  -2.930 0.003391 ** 
#   environment3                                -0.204519   0.094876  -2.156 0.031111 *  
#   panel2                                       0.417834   0.251342   1.662 0.096431 . 
# proficiency:dependency2                     -0.140508   0.039710  -3.538 0.000403 ***
#   proficiency:environment2                    -0.011732   0.010474  -1.120 0.262678    
# proficiency:environment3                    -0.038704   0.010618  -3.645 0.000267 ***
#   dependency2:environment2                     0.973123   0.189038   5.148 2.64e-07 ***
#   dependency2:environment3                     2.369132   0.191699  12.359  < 2e-16 ***
#   proficiency:panel2                          -0.033293   0.028851  -1.154 0.248502    
# dependency2:panel2                           1.387020   0.685809   2.022 0.043129 *  
#   environment2:panel2                         -0.273861   0.188599  -1.452 0.146479    
# environment3:panel2                         -0.648957   0.189387  -3.427 0.000611 ***
#   proficiency:dependency2:environment2         0.040294   0.020951   1.923 0.054451 .  
# proficiency:dependency2:environment3         0.120408   0.021259   5.664 1.48e-08 ***
#   proficiency:dependency2:panel2               0.122370   0.079243   1.544 0.122533    
# proficiency:environment2:panel2             -0.016772   0.020943  -0.801 0.423234    
# proficiency:environment3:panel2              0.005905   0.021199   0.279 0.780590    
# dependency2:environment2:panel2             -0.360201   0.377158  -0.955 0.339557    
# dependency2:environment3:panel2             -2.116774   0.379287  -5.581 2.39e-08 ***
#   proficiency:dependency2:environment2:panel2  0.012567   0.041890   0.300 0.764175    
# proficiency:dependency2:environment3:panel2 -0.162880   0.042446  -3.837 0.000124 ***

anova(model1, model2)

# no.par   AIC  logLik LR.stat df Pr(>Chisq)    
# model1     30 11858 -5899.0                          
# model2     32 10487 -5211.7  1374.8  2  < 2.2e-16 ***

#------------------------------------------------------------------------------#
# + + model 3 w/ 'dependency'
#------------------------------------------------------------------------------#

# fit model ...

tic()
model3 <- clmm(response ~ proficiency * dependency * environment * panel + 
                 (1 + dependency + environment | participant) + 
                 (1 | item), 
               data = filter(md, study == '210510_do'), 
               control = clmm.control(grtol = 1e6)) %>%
  write_rds('models/ajt_orc_proficiency_clmm_full_md3.rds')
summary(model3)
toc()
beep()

# 1168.68 sec elapsed
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# proficiency                                  0.008767   0.015404   0.569 0.569248    
# dependency2                                 -3.164194   0.370699  -8.536  < 2e-16 ***
#   environment2                                -0.399455   0.165712  -2.411 0.015929 *  
#   environment3                                -0.308124   0.165420  -1.863 0.062507 .  
# panel2                                       0.417016   0.267930   1.556 0.119605
# proficiency:dependency2                     -0.150122   0.042333  -3.546 0.000391 ***
#   proficiency:environment2                    -0.015642   0.017859  -0.876 0.381105    
# proficiency:environment3                    -0.048693   0.018167  -2.680 0.007357 ** 
#   dependency2:environment2                     1.046320   0.206102   5.077 3.84e-07 ***
#   dependency2:environment3                     2.518649   0.206934  12.171  < 2e-16 ***
#   proficiency:panel2                          -0.039247   0.030738  -1.277 0.201668    
# dependency2:panel2                           1.507380   0.730961   2.062 0.039190 *  
#   environment2:panel2                         -0.221398   0.318165  -0.696 0.486517    
# environment3:panel2                         -0.583920   0.320635  -1.821 0.068586 .  
# proficiency:dependency2:environment2         0.041308   0.022439   1.841 0.065641 .  
# proficiency:dependency2:environment3         0.127351   0.022536   5.651 1.60e-08 ***
#   proficiency:dependency2:panel2               0.134002   0.084478   1.586 0.112684    
# proficiency:environment2:panel2             -0.009134   0.035692  -0.256 0.798012    
# proficiency:environment3:panel2              0.018980   0.036276   0.523 0.600825    
# dependency2:environment2:panel2             -0.493176   0.402968  -1.224 0.221005    
# dependency2:environment3:panel2             -2.313160   0.402369  -5.749 8.99e-09 ***
#   proficiency:dependency2:environment2:panel2 -0.010148   0.044674  -0.227 0.820299    
# proficiency:dependency2:environment3:panel2 -0.192535   0.044957  -4.283 1.85e-05 ***
  
anova(model2, model3)

# no.par   AIC  logLik LR.stat df Pr(>Chisq)    
# model2     32 10487 -5211.7                          
# model3     39 10391 -5156.3  110.66  7  < 2.2e-16 ***

#------------------------------------------------------------------------------#
# + + model 4 w/ 'dependency'
#------------------------------------------------------------------------------#

# fit model ...

tic()
model4 <- clmm(response ~ proficiency * dependency * environment * panel + 
                 (1 + dependency * environment | participant) + 
                 (1 | item), 
               data = filter(md, study == '210510_do'), 
               control = clmm.control(grtol = 1e6)) %>%
  write_rds('models/ajt_orc_proficiency_clmm_full_md4.rds')
summary(model4)
toc()
beep()

# 2948.36 sec elapsed (49.14 min)
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# proficiency                                  0.010748   0.016297   0.659 0.509596    
# dependency2                                 -3.406502   0.390935  -8.714  < 2e-16 ***
# environment2                                -0.437655   0.196766  -2.224 0.026132 *  
# environment3                                -0.388038   0.194971  -1.990 0.046565 *  
# panel2                                       0.434982   0.282226   1.541 0.123255 
# proficiency:dependency2                     -0.163106   0.044714  -3.648 0.000265 ***
# proficiency:environment2                    -0.015546   0.019323  -0.805 0.421072    
# proficiency:environment3                    -0.051875   0.019505  -2.660 0.007826 ** 
# dependency2:environment2                     1.360681   0.320804   4.241 2.22e-05 ***
# dependency2:environment3                     3.052787   0.389471   7.838 4.57e-15 ***
# proficiency:panel2                          -0.041736   0.032471  -1.285 0.198668    
# dependency2:panel2                           1.641946   0.764767   2.147 0.031794 *  
# environment2:panel2                         -0.251413   0.340788  -0.738 0.460672    
# environment3:panel2                         -0.662698   0.343437  -1.930 0.053656 .  
# proficiency:dependency2:environment2         0.056670   0.027899   2.031 0.042229 *  
# proficiency:dependency2:environment3         0.153915   0.038579   3.990 6.62e-05 ***
# proficiency:dependency2:panel2               0.151224   0.089159   1.696 0.089863 .  
# proficiency:environment2:panel2             -0.009889   0.038001  -0.260 0.794675    
# proficiency:environment3:panel2              0.018838   0.038508   0.489 0.624706    
# dependency2:environment2:panel2             -0.794971   0.495051  -1.606 0.108310    
# dependency2:environment3:panel2             -2.761289   0.678779  -4.068 4.74e-05 ***
# proficiency:dependency2:environment2:panel2 -0.034529   0.054616  -0.632 0.527253    
# proficiency:dependency2:environment3:panel2 -0.234475   0.076207  -3.077 0.002092 ** 

anova(model3, model4)

# no.par   AIC  logLik LR.stat df Pr(>Chisq)    
# model3     39 10391 -5156.3                          
# model4     50 10333 -5116.3  80.086 11   1.42e-12 ***

trends <- emtrends(model4, ~ panel * dependency * environment, var = 'proficiency', adjust = 'mvt')
test(trends)

# panel dependency environment proficiency.trend     SE  df z.ratio p.value
# KE    gap        short                 0.23245 0.0528 Inf   4.406  0.0001 ***
# ME    gap        short                 0.06729 0.0464 Inf   1.451  0.7308
# KE    pronoun    short                -0.12129 0.0480 Inf  -2.528  0.1027
# ME    pronoun    short                -0.04557 0.0449 Inf  -1.015  0.9539
# KE    gap        long                  0.18489 0.0443 Inf   4.176  0.0003 ***
# ME    gap        long                  0.02709 0.0404 Inf   0.670  0.9977
# KE    pronoun    long                 -0.09493 0.0435 Inf  -2.181  0.2324
# ME    pronoun    long                 -0.04635 0.0412 Inf  -1.125  0.9169
# KE    gap        island                0.03558 0.0401 Inf   0.887  0.9806
# ME    gap        island                0.00649 0.0382 Inf   0.170  1.0000
# KE    pronoun    island               -0.04701 0.0427 Inf  -1.101  0.9262
# ME    pronoun    island               -0.06968 0.0411 Inf  -1.695  0.5486
# P value adjustment: mvt method for 12 tests 

# plot ...

plot <- emmip(model4, environment ~ proficiency | panel ~ dependency, cov.reduce = range, CIs = TRUE, plotit = FALSE)

ggplot(plot, aes(x = proficiency, y = yvar, group = environment, col = environment, linetype = environment)) +
  theme_classic() +
  geom_ribbon(aes(ymin = LCL, ymax = UCL, fill = environment), alpha = .1, col = NA) +
  geom_line(lwd = .75) +
  scale_y_continuous(name='log odds ratio') +
  scale_x_continuous(name='mean-centered proficiency score') +
  scale_linetype_manual(values = c('longdash', 'solid', 'twodash')) +
  scale_color_manual(values = c('#619CFF', '#00BA38', '#F8766D')) +
  scale_fill_manual(values = c('#619CFF', '#00BA38', '#F8766D')) +
  theme(legend.key.size = unit(2,"line")) +
  facet_grid2(vars(dependency), vars(panel), axes = 'all', remove_labels = FALSE,
              labeller = as_labeller(c(`gap` = 'gap', `pronoun` = 'RP', `ME` = 'MLE', `KE` = 'KLE')))

ggsave("plots/orc/ajt_rating_proficiency_emmeans_md4.png", width=6.5, height=3, dpi=600)

#------------------------------------------------------------------------------#
# + + model 5 w/ 'dependency'
#------------------------------------------------------------------------------#

# fit model ...

tic()
model5 <- clmm(response ~ proficiency * dependency * environment * panel + 
                 (1 | participant) + 
                 (1 + panel | item), 
               data = filter(md, study == '210510_do'), 
               control = clmm.control(grtol = 1e6)) %>%
  write_rds('models/ajt_orc_proficiency_clmm_full_md5.rds')
summary(model5)
toc()
beep()

# 244.09 sec elapsed
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# proficiency                                  0.002960   0.010928   0.271 0.786470    
# dependency2                                 -1.883828   0.071446 -26.367  < 2e-16 ***
# environment2                                -0.235687   0.084119  -2.802 0.005082 ** 
# environment3                                -0.182175   0.083028  -2.194 0.028225 *  
# panel2                                       0.238444   0.187717   1.270 0.204001
# proficiency:dependency2                     -0.098496   0.007895 -12.476  < 2e-16 ***
# proficiency:environment2                    -0.009447   0.009570  -0.987 0.323569    
# proficiency:environment3                    -0.028422   0.009539  -2.980 0.002886 ** 
# dependency2:environment2                     0.731865   0.168336   4.348 1.38e-05 ***
# dependency2:environment3                     1.651997   0.166791   9.905  < 2e-16 ***
# proficiency:panel2                          -0.030354   0.021863  -1.388 0.165037    
# dependency2:panel2                           0.878727   0.136594   6.433 1.25e-10 ***
# environment2:panel2                         -0.201238   0.168161  -1.197 0.231424    
# environment3:panel2                         -0.471190   0.166085  -2.837 0.004553 ** 
# proficiency:dependency2:environment2         0.025342   0.019134   1.324 0.185368    
# proficiency:dependency2:environment3         0.074644   0.019044   3.920 8.87e-05 ***
# proficiency:dependency2:panel2               0.086585   0.015617   5.544 2.95e-08 ***
# proficiency:environment2:panel2             -0.009263   0.019139  -0.484 0.628399    
# proficiency:environment3:panel2              0.007643   0.019069   0.401 0.688568    
# dependency2:environment2:panel2             -0.178550   0.336320  -0.531 0.595493    
# dependency2:environment3:panel2             -1.248115   0.332056  -3.759 0.000171 ***
# proficiency:dependency2:environment2:panel2  0.023059   0.038273   0.602 0.546846    
# proficiency:dependency2:environment3:panel2 -0.092744   0.038072  -2.436 0.014850 *

anova(model1, model5)

# no.par   AIC logLik LR.stat df Pr(>Chisq)
# model1     30 11858  -5899                      
# model5     32 11862  -5899  -0.001  2          1

#------------------------------------------------------------------------------#
# + + model 6 w/ 'dependency'
#------------------------------------------------------------------------------#

# fit model ...

tic()
model6 <- clmm(response ~ proficiency * dependency * environment * panel + 
                 (1 | participant) + 
                 (1 + dependency | item), 
               data = filter(md, study == '210510_do'), 
               control = clmm.control(grtol = 1e6)) %>%
  write_rds('models/ajt_orc_proficiency_clmm_full_md6.rds')
summary(model6)
toc()
beep()

# 252.62 sec elapsed
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# proficiency                                  0.003047   0.010926   0.279 0.780358    
# dependency2                                 -1.884499   0.071455 -26.373  < 2e-16 ***
# environment2                                -0.236059   0.084132  -2.806 0.005019 ** 
# environment3                                -0.182227   0.083041  -2.194 0.028205 * 
# proficiency:dependency2                     -0.098535   0.007895 -12.480  < 2e-16 ***
# proficiency:environment2                    -0.009474   0.009571  -0.990 0.322211    
# proficiency:environment3                    -0.028425   0.009540  -2.980 0.002886 ** 
# dependency2:environment2                     0.733831   0.168362   4.359 1.31e-05 ***
# dependency2:environment3                     1.654072   0.166819   9.915  < 2e-16 ***
# proficiency:panel2                          -0.030386   0.021860  -1.390 0.164520    
# dependency2:panel2                           0.879606   0.136610   6.439 1.20e-10 ***
# environment2:panel2                         -0.201452   0.168187  -1.198 0.231000    
# environment3:panel2                         -0.471098   0.166111  -2.836 0.004568 ** 
# proficiency:dependency2:environment2         0.025447   0.019136   1.330 0.183588    
# proficiency:dependency2:environment3         0.074755   0.019046   3.925 8.67e-05 ***
# proficiency:dependency2:panel2               0.086640   0.015619   5.547 2.90e-08 ***
# proficiency:environment2:panel2             -0.009268   0.019141  -0.484 0.628237    
# proficiency:environment3:panel2              0.007648   0.019071   0.401 0.688388    
# dependency2:environment2:panel2             -0.179326   0.336370  -0.533 0.593950    
# dependency2:environment3:panel2             -1.250111   0.332108  -3.764 0.000167 ***
# proficiency:dependency2:environment2:panel2  0.023004   0.038277   0.601 0.547850    
# proficiency:dependency2:environment3:panel2 -0.092819   0.038076  -2.438 0.014780 *

anova(model1, model6)

# no.par   AIC logLik LR.stat df Pr(>Chisq)
# model1     30 11858  -5899                      
# model6     32 11862  -5899   1e-04  2          1

#------------------------------------------------------------------------------#
# + + model 7 w/ 'dependency'
#------------------------------------------------------------------------------#

# fit model ...

tic()
model7 <- clmm(response ~ proficiency * dependency * environment * panel + 
                 (1 | participant) + 
                 (1 + environment | item), 
               data = filter(md, study == '210510_do'), 
               control = clmm.control(grtol = 1e6)) %>%
  write_rds('models/ajt_orc_proficiency_clmm_full_md7.rds')
summary(model7)
toc()
beep()

# 326.22 sec elapsed
# Coefficients:
# Estimate Std. Error z value Pr(>|z|)    
# proficiency                                  0.003040   0.010932   0.278 0.780970    
# dependency2                                 -1.880941   0.072065 -26.101  < 2e-16 ***
# environment2                                -0.235954   0.084698  -2.786 0.005339 ** 
# environment3                                -0.182136   0.083866  -2.172 0.029874 *  
# panel2                                       0.238078   0.187795   1.268 0.204887 
# proficiency:dependency2                     -0.098300   0.007922 -12.409  < 2e-16 ***
# proficiency:environment2                    -0.009482   0.009573  -0.991 0.321918    
# proficiency:environment3                    -0.028379   0.009547  -2.972 0.002954 ** 
# dependency2:environment2                     0.732236   0.168656   4.342 1.41e-05 ***
# dependency2:environment3                     1.658690   0.167517   9.902  < 2e-16 ***
# proficiency:panel2                          -0.030410   0.021871  -1.390 0.164394    
# dependency2:panel2                           0.877945   0.136700   6.422 1.34e-10 ***
# environment2:panel2                         -0.202147   0.168212  -1.202 0.229466    
# environment3:panel2                         -0.472230   0.166568  -2.835 0.004582 ** 
# proficiency:dependency2:environment2         0.025139   0.019169   1.311 0.189722    
# proficiency:dependency2:environment3         0.074938   0.019119   3.920 8.87e-05 ***
# proficiency:dependency2:panel2               0.086611   0.015628   5.542 2.99e-08 ***
# proficiency:environment2:panel2             -0.009334   0.019145  -0.488 0.625885    
# proficiency:environment3:panel2              0.007547   0.019084   0.395 0.692511    
# dependency2:environment2:panel2             -0.179073   0.336386  -0.532 0.594488    
# dependency2:environment3:panel2             -1.255246   0.332429  -3.776 0.000159 ***
# proficiency:dependency2:environment2:panel2  0.023033   0.038287   0.602 0.547439    
# proficiency:dependency2:environment3:panel2 -0.093353   0.038132  -2.448 0.014359 *

anova(model1, model7)

# no.par   AIC  logLik LR.stat df Pr(>Chisq)
# model1     30 11858 -5899.0                      
# model7     35 11868 -5898.9  0.2005  5     0.9991

#------------------------------------------------------------------------------#
# + + model 8 w/ 'dependency'
#------------------------------------------------------------------------------#

# fit model ...

tic()
model8 <- clmm(response ~ proficiency * dependency * environment * panel + 
                 (1 + dependency * environment | participant) + 
                 (1 + dependency * environment * panel | item), 
               data = filter(md, study == '210510_do'), 
               control = clmm.control(grtol = 1e6)) %>%
  write_rds('models/ajt_orc_proficiency_clmm_full_md8.rds')
summary(model8)
toc()
beep()

#------------------------------------------------------------------------------#
# + plot and model difference scores ----
#------------------------------------------------------------------------------#

plot <- md %>%
  mutate(response = as.numeric(response)) %>%
  group_by(study, panel, participant, environment, proficiency) %>%
  summarise(dscore = mean(response[dependency == 'gap'], na.rm = TRUE) - mean(response[dependency == 'pronoun'], na.rm = TRUE)) %>%
  ungroup()

p1 <- ggplot(data=filter(plot, study == '210510_do'), aes(x=proficiency, y=dscore, group = environment, col = environment, fill = environment, linetype = environment))
p2 <- ggplot(data=filter(plot, study == '210510_su'), aes(x=proficiency, y=dscore, group = environment, col = environment, fill = environment, linetype = environment))

s <- list(
  geom_hline(yintercept = 0),
  geom_vline(xintercept = 0),
  geom_point(alpha = .1, shape = 16, size = 2),
  geom_smooth(method=lm, lwd = 1, alpha = .2), 
  theme_classic(),
  scale_x_continuous(name='mean-centered proficiency score'),
  scale_y_continuous(name='gap - RP'),
  scale_linetype_manual(values = c('longdash', 'solid', 'twodash')),
  scale_color_manual(values = c('#619CFF', '#00BA38', '#F8766D')),
  scale_fill_manual(values = c('#619CFF', '#00BA38', '#F8766D')),
  guides(color = guide_legend(override.aes = list(shape = NA))),
  theme(text = element_text(size = 12),
        plot.title = element_text(size = 12, hjust = .5), 
        legend.key.size = unit(2,"line"),
        legend.position = 'right'),
  facet_wrap(~panel,
              labeller = as_labeller(c(`KE` = 'KLE', `ME` = 'MLE')))
)

p1 + s
ggsave("plots/orc/ajt_orc_rating_proficiency_effect_dscore.png", width=6.5, height=2, dpi=600)

p2 + s
ggsave("plots/src/ajt_src_rating_proficiency_effect_dscore.png", width=6.5, height=2, dpi=600)


contrasts(plot$environment) <- contr.treatment(3) - matrix(rep(1/3, 6), ncol = 2)
contrasts(plot$panel) <- contr.treatment(2) - matrix(rep(1/2, 2), ncol = 1)

contrasts(plot$environment)
contrasts(plot$panel)

tic()
model1 <- lmer(dscore ~ proficiency * environment * panel + 
                 (1 | participant),
               data = filter(plot, study == '210510_do')) %>%
  write_rds('models/ajt_orc_proficiency_lmer_dscore_md1.rds')
summary(model1)
toc()
beep()

# 0.62 sec elapsed
# no warnings
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                       1.654813   0.186417 135.999999   8.877 3.54e-15 ***
# proficiency                       0.081634   0.021748 135.999999   3.754 0.000257 ***
# environment2                     -0.499810   0.155529 272.000000  -3.214 0.001469 ** 
# environment3                     -1.278707   0.155529 272.000000  -8.222 8.30e-15 ***
# panel2                           -0.531251   0.372834 135.999999  -1.425 0.156477    
# proficiency:environment2         -0.008895   0.018144 272.000000  -0.490 0.624348    
# proficiency:environment3         -0.046088   0.018144 272.000000  -2.540 0.011639 *  
# proficiency:panel2               -0.048871   0.043495 135.999999  -1.124 0.263164    
# environment2:panel2               0.100610   0.311058 272.000000   0.323 0.746607    
# environment3:panel2               1.017400   0.311058 272.000000   3.271 0.001211 ** 
# proficiency:environment2:panel2  -0.035042   0.036288 272.000000  -0.966 0.335072    
# proficiency:environment3:panel2   0.057850   0.036288 272.000000   1.594 0.112055  

trends <- emtrends(model1, ~ panel * environment, var = 'proficiency', adjust = 'mvt')
test(trends)

# panel environment proficiency.trend     SE  df t.ratio p.value
# KE    short                  0.1282 0.0347 201   3.699  0.0014 **
# ME    short                  0.0717 0.0336 201   2.134  0.1442
# KE    long                   0.1368 0.0347 201   3.948  0.0006 ***
# ME    long                   0.0453 0.0336 201   1.348  0.5615
# KE    island                 0.0532 0.0347 201   1.534  0.4348
# ME    island                 0.0546 0.0336 201   1.624  0.3791

# plot ...

plot <- emmip(model1, environment ~ proficiency | panel, cov.reduce = range, CIs = TRUE, plotit = FALSE)

ggplot(plot, aes(x = proficiency, y = yvar, group = environment, col = environment, linetype = environment)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  theme_classic() +
  geom_ribbon(aes(ymin = LCL, ymax = UCL, fill = environment), alpha = .1, col = NA) +
  geom_line(lwd = 1) +
  scale_y_continuous(name='gap - RP') +
  scale_x_continuous(name='mean-centered proficiency score') +
  scale_linetype_manual(values = c('longdash', 'solid', 'twodash')) +
  scale_color_manual(values = c('#619CFF', '#00BA38', '#F8766D')) +
  scale_fill_manual(values = c('#619CFF', '#00BA38', '#F8766D')) +
  theme(legend.key.size = unit(2,"line"),
        text = element_text(size = 12)) +
  facet_wrap(~panel, labeller = as_labeller(c(`ME` = 'MLE', `KE` = 'KLE')))

ggsave("plots/orc/ajt_rating_proficiency_dscore_emmeans_md1.png", width=6.5, height=2, dpi=600)


tic()
model2 <- lmer(dscore ~ proficiency * environment * panel + 
                 (1 + environment | participant),
               data = filter(plot, study == '210510_do')) %>%
  write_rds('models/ajt_orc_proficiency_lmer_dscore_md2.rds')
summary(model2)
toc()
beep()

# does not converge

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
# ::::: key ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::: ----
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
# ::::: chopping block :::::::::::::::::::::::::::::::::::::::::::::::::::: ----
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

#------------------------------------------------------------------------------#
# modeling: ept ~ rate ~ glmer ~ 220622 ----
#------------------------------------------------------------------------------#

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
# modeling: spr ~ raw rt ~ lmer ----
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

