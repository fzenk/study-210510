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

# fit models ...

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



