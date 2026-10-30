##### TOSCA Big Five and Major Life Goal (Co-)Development

# work space setup #############################################################

# clear console, environment
cat('\014')
rm(list = ls())

# load packages
library(apaTables)
library(dplyr)
library(effectsize)
library(ggplot2)
library(here)
library(lavaan)
library(lavaan.mi)
library(openxlsx)
library(psych)
library(semTools)
library(tibble)
library(tidyr)
library(tools)

# load custom functions
source('tosca-custom-functions.R')

# load data (this is the 3rd and latest file i received from Sven)
data <- readRDS('TOSCA-data-080625.RDS') # 3023 obs. 750 vars.

names(data)

# note. data is in wide/multivariate format.

# data preparation #############################################################

# quick: need for demographic info
# highest household education level
table(data$pa_bil_z)
describe(data$pa_bil_z) 
median(data$pa_bil_z, na.rm = TRUE) # 6

# remove vars we don't need
data <- data %>% 
  select(-idsch,    # school id
         -abino1,   # grade
         -aiq_mle,  # cognitive
         -abuch,    # books
         -aspraelt) # language

# 3023 obs. 744 vars.names

## rename variables

names(data)

# demographics
data <- data %>%
  rename('id' = 'idstud',
         'age.1' = 'aalter',
         'birthyr.1' = 'agebjahr',
         'birthmo.1' = 'agebmon',
         'birthyr.2' = 'bgebja',
         'birthmo.2' = 'bgebmo',
         'age.2' = 'balter',
         'birthyr.3' = 'cgebja',
         'birthmo.3' = 'cgebmo',
         'birthyr.4' = 'dgebja',
         'birthmo.4' = 'dgebmo',
         'gender' = 'asex',
         'parent.edu' = 'pa_bil_z')

names(data)

## compute age variables

# check age.1 (feb, 2002)
data <- data %>% 
  mutate(age.1.check = (2002 + (2 - 1)/12) - (birthyr.1 + (birthmo.1 - 1)/12),
         age.1.check = as.numeric(substring(age.1.check, 3))) %>% 
  relocate(age.1.check, .after = age.1) # ages match

# check age.2 (mar, 2004)
data <- data %>% 
  mutate(age.2.check = (2004 + (3 - 1)/12) - (birthyr.1 + (birthmo.1 - 1)/12),
         age.2.check = as.numeric(substring(age.2.check, 3))) %>% 
  relocate(age.2.check, .after = age.2) # ages match

#> note.
#> T1 was assessed in February of 2002, and T2 in March of 2004. 
#> for T3 and following, we'll assume they took place in February. 

# remove age.1.check
data$age.1.check <- NULL
data$age.2.check <- NULL

# compute ages T3-T8 (2006-2020)
data <- data %>% 
  mutate(age.3 = age.1 + 4, # until T5, they assessed every 2 years
         age.4 = age.1 + 6,
         age.5 = age.1 + 8,
         age.6 = age.1 + 10, # from T6, they assessed every 4 years
         age.7 = age.1 + 14,
         age.8 = age.1 + 18)

# relocate age variables
data <- data %>% 
  relocate(age.2, .after = age.1) %>% 
  relocate(age.3, .after = age.2) %>% 
  relocate(age.4, .after = age.3) %>% 
  relocate(age.5, .after = age.4) %>% 
  relocate(age.6, .after = age.5) %>% 
  relocate(age.7, .after = age.6) %>% 
  relocate(age.8, .after = age.7)

# check age data
lapply(data[, paste0('age.', 1:8)], describe) # from mean age 19.55 (T1) to 37.55 (T8)

# remove variables we no longer need
data <- data %>% 
  select(-starts_with('birth')) # birthyr, birthmo

## back to renaming key variables

# big five
data <- data %>%
  rename_with(~ sub('conc$',  'cons', .), ends_with('conc')) %>%
  rename_with(~ sub('aggr$',  'agre', .), ends_with('aggr')) %>%
  rename_with(~ sub('neuro$', 'neur', .), ends_with('neuro'))
  
# life goals
data <- data %>%
  rename_with(~ sub('aiw$', 'pros', .), ends_with('aiw')) %>%
  rename_with(~ sub('air$', 'fame', .), ends_with('air')) %>%
  rename_with(~ sub('aia$', 'attr', .), ends_with('aia')) %>%
  rename_with(~ sub('aip$', 'pers', .), ends_with('aip')) %>%
  rename_with(~ sub('aib$', 'rela', .), ends_with('aib')) %>%
  rename_with(~ sub('aig$', 'soci', .), ends_with('aig')) %>%
  rename_with(~ sub('aif$', 'phys', .), ends_with('aif')) %>%
  rename_with(~ sub('aih$', 'hedo', .), ends_with('aih')) %>%
  rename_with(~ sub('aiv$', 'libe', .), ends_with('aiv')) %>%
  rename_with(~ sub('ais$', 'secu', .), ends_with('ais')) %>%
  rename_with(~ sub('aik$', 'conf', .), ends_with('aik')) %>%
  rename_with(~ sub('ait$', 'reli', .), ends_with('ait'))

names(data)

# key variables (traits, goals)
key.vars <- c('extr', 'agre', 'cons', 'neur', 'open',
              'pros', 'fame', 'attr', 'pers', 'rela', 
              'soci', 'phys', 'hedo', 'libe', 'secu', 
              'conf', 'reli')

# filter out items
data <- data %>%
  select('id', 'gender', 'parent.edu',
         starts_with('age'),
         starts_with('birthyr'),
         starts_with('birthmo'),
         ends_with(key.vars)) # 3023 obs. 102 vars

names(data)

# rename wave label
for (i in 1:8) {
  for (var in key.vars) {
    old <- paste0(letters[i], var)
    new <- paste0(var, '.', i)
    
    names(data)[names(data) == old] <- new
  }
  
  rm(i, var, new, old)
}

names(data) # e.g., aextr is now extr.1

## key variable notation and summary

# big five traits
  # extr - extraversion
  # agre - agreeableness
  # cons - conscientiousness
  # neur - neuroticism
  # open - openness

# major life goals
  # pros - prosperity
  # fame
  # attr - attractiveness
  # pers - personal growth
  # rela - relationships
  # soci - society
  # phys - physical health
  # hedo - hedonism
  # libe - liberal values
  # secu - security
  # conf - conformism
  # reli - religiosity

# wave completions
  # T1-T8 (8 waves) - big five, prosperity, fame, attractiveness
  # T1-T4 (4 waves) - personal growth, relationships, society, physical health, hedonism
  # T1    (1 wave)  - liberal values
  # T3-T4 (2 waves) - security, conformism, religiosity/spirituality

## correlations ################################################################

names(data)

# group traits and goals
traits <- data %>% 
  select(matches('^(extr|agre|cons|neur|open)')) %>% 
  names()
goals  <- data %>% 
  select(matches('^(pros|fame|attr|pers|rela|
                    soci|phys|hedo|libe|secu|
                    conf|reli)')) %>% 
  names()

# re-order
traits <- traits[order(as.numeric(sub('.*\\.(\\d+)$', '\\1', traits)))]
goals  <- goals[order(as.numeric(sub('.*\\.(\\d+)$', '\\1', goals)))]

# init corr matrix
corr.matrix <- matrix(NA, 
                     nrow = length(goals), 
                     ncol = length(traits),
                     dimnames = list(goals, traits))

# loop through over all trait-goal pairs
for(t in seq_along(traits)){
  for(g in seq_along(goals)){
    
    # grab cor (and round up)
    corr.results <- cor.test(data[[traits[t]]], 
                             data[[goals[g]]], 
                             use = 'pairwise.complete.obs')
    corr <- round(corr.results$estimate, 2)
    pval <- corr.results$p.value
    
    # grab star if significant
    if(pval < 0.05) { corr <- paste0(corr, '*') }
    
    corr.matrix[g, t] <- corr
  }
  
  rm(t, g, corr.results, corr, pval)
}

# convert to df and add goal names
corr.df <- cbind(Goals = rownames(corr.matrix), 
                  as.data.frame(corr.matrix))

# export as excel
corr.wb <- createWorkbook()
addWorksheet(corr.wb, 'Intercorrelations')
writeData(corr.wb, 'Intercorrelations', corr.df)
saveWorkbook(corr.wb, 
             here('Output/Primary/Descriptives/correlations.xlsx'), 
             overwrite = TRUE)

# clean
rm(corr.df, corr.matrix, corr.wb, goals, traits)

## descriptives ################################################################

# group variables by wave
for (i in 1:8) {
  wave <- paste0('wave.', i)
  vars <- paste0(key.vars, '.', i)
  
  assign(wave, 
         data %>% select(any_of(vars)))
  
  rm(i, wave, vars)
}

# check random
names(get(paste0('wave.', sample(1:8, 1))))

# explore data
for (i in 1:8) {
  wave <- paste0('wave.', i)
  wave.data <- get(wave)
  
  print(describe(wave.data))
  
  rm(i, wave, wave.data)
}

# init desc list
all.desc <- list()

# grab descriptives across all waves
for (i in 1:8) {
  wave <- get(paste0('wave.', i))
  
  desc <- describe(wave) %>%
    as.data.frame() %>%
    rownames_to_column('variable')
  
  desc$mean <- round(desc$mean, 2)
  desc$sd   <- round(desc$sd, 2)
  desc$mean_sd <- paste0(desc$mean, ' (', desc$sd, ')')
  desc$variable <- sub('\\..*$', '', desc$variable)
  desc$wave <- i
  
  all.desc[[i]] <- desc
  
  rm(i, wave, desc)
}

# prepare descriptives table
desc_table <- bind_rows(all.desc) %>%
  select(variable, wave, mean_sd) %>%
  pivot_wider(names_from = wave,
              values_from = mean_sd,
              names_glue = 'wave{wave}')

# export descriptives table
write.xlsx(desc_table,
           here('Output/Primary/descriptives', 
                'Descriptives.xlsx'))

# clean environment
rm(list = ls(pattern = '^wave.'),
   all.desc, desc_table)

## attrition ###################################################################

names(data)

## people who participated in T1

grep('\\.1$', names(data), value = TRUE)

  # "age.1"  
  # "extr.1" "agre.1" "cons.1" "neur.1" "open.1" 
  # "pros.1" "fame.1" "attr.1" 
  # "pers.1" "rela.1" "soci.1" "phys.1" "hedo.1" 
  # "libe.1"

T1.participated <- data %>%
  filter(rowSums(!is.na(across(all_of(
    c('extr.1', 'agre.1', 'cons.1', 'open.1', 'neur.1',
      'pros.1', 'fame.1', 'attr.1',
      'pers.1', 'rela.1', 'soci.1', 'phys.1', 'hedo.1', 
      'libe.1'))))) > 0) # 2971 obs.

describe(T1.participated$age.1) # m=19.54, sd=0.77

## people who participated in T2

grep('\\.2$', names(data), value = TRUE)

  # "age.2"  
  # "extr.2" "agre.2" "cons.2" "neur.2" "open.2" 
  # "pros.2" "fame.2" "attr.2" 
  # "pers.2" "rela.2" "soci.2" "phys.2" "hedo.2"

T2.participated <- data %>%
  filter(rowSums(!is.na(across(all_of(
    c('extr.2', 'agre.2', 'cons.2', 'open.2', 'neur.2',
      'pros.2', 'fame.2', 'attr.2',
      'pers.2', 'rela.2', 'soci.2', 'phys.2', 'hedo.2'))))) > 0) # 2308 obs.

describe(T2.participated$age.2) # m=21.49, sd=0.77

## people who participated in T3

grep('\\.3$', names(data), value = TRUE)

  # "age.3"  
  # "extr.3" "agre.3" "cons.3" "neur.3" "open.3" 
  # "pros.3" "fame.3" "attr.3" 
  # "pers.3" "rela.3" "soci.3" "phys.3" "hedo.3" 
  # "secu.3" "conf.3" "reli.3"

T3.participated <- data %>%
  filter(rowSums(!is.na(across(all_of(
    c('extr.3', 'agre.3', 'cons.3', 'open.3', 'neur.3',
      'pros.3', 'fame.3', 'attr.3',
      'pers.3', 'rela.3', 'soci.3', 'phys.3', 'hedo.3', 
      'secu.3', 'conf.3', 'reli.3'))))) > 0) # 1907 obs.

describe(T3.participated$age.3) # m=23.5, sd=0.78

## people who participated in T4

grep('\\.4$', names(data), value = TRUE)

  # "age.4"  
  # "extr.4" "agre.4" "cons.4" "neur.4" "open.4" 
  # "pros.4" "fame.4" "attr.4" 
  # "pers.4" "rela.4" "soci.4" "phys.4" "hedo.4" 
  # "secu.4" "conf.4" "reli.4"

T4.participated <- data %>%
  filter(rowSums(!is.na(across(all_of(
    c('extr.4', 'agre.4', 'cons.4', 'open.4', 'neur.4',
      'pros.4', 'fame.4', 'attr.4',
      'pers.4', 'rela.4', 'soci.4', 'phys.4', 'hedo.4', 
      'secu.4', 'conf.4', 'reli.4'))))) > 0) # 1865 obs.

describe(T4.participated$age.4) # m=25.5, sd=0.79

## people who participated in T5

grep('\\.5$', names(data), value = TRUE)

  # "age.5"
  # "extr.5" "agre.5" "cons.5" "neur.5" "open.5" 
  # "pros.5" "fame.5" "attr.5"

T5.participated <- data %>%
  filter(rowSums(!is.na(across(all_of(
    c('extr.5', 'agre.5', 'cons.5', 'open.5', 'neur.5',
      'pros.5', 'fame.5', 'attr.5'))))) > 0) # 1682 obs.

describe(T5.participated$age.5) # m=27.49, sd=0.79

## people who participated in T6

grep('\\.6$', names(data), value = TRUE)

  # "age.6"
  # "extr.6" "agre.6" "cons.6" "neur.6" "open.6" 
  # "pros.6" "fame.6" "attr.6"

T6.participated <- data %>%
  filter(rowSums(!is.na(across(all_of(
    c('extr.6', 'agre.6', 'cons.6', 'open.6', 'neur.6',
      'pros.6', 'fame.6', 'attr.6'))))) > 0) # 1405 obs.

describe(T6.participated$age.6) # m=29.5, sd=0.82

## people who participated in T7

grep('\\.7$', names(data), value = TRUE)

  # "age.7"
  # "extr.7" "agre.7" "cons.7" "neur.7" "open.7" 
  # "pros.7" "fame.7" "attr.7"

T7.participated <- data %>%
  filter(rowSums(!is.na(across(all_of(
    c('extr.7', 'agre.7', 'cons.7', 'open.7', 'neur.7',
      'pros.7', 'fame.7', 'attr.7'))))) > 0) # 1376 obs.

describe(T7.participated$age.7) # m=33.48, sd=0.59

## people who participated in T8

grep('\\.8$', names(data), value = TRUE)

  # "age.8"  
  # "extr.8" "agre.8" "cons.8" "neur.8" "open.8" 
  # "pros.8" "fame.8" "attr.8"

T8.participated <- data %>%
  filter(rowSums(!is.na(across(all_of(
    c('extr.8', 'agre.8', 'cons.8', 'open.8', 'neur.8',
      'pros.8', 'fame.8', 'attr.8'))))) > 0) # 1214 obs.

describe(T8.participated$age.8) # m=37.5, sd=0.61
table(T8.participated$gender) # 2=730 female; 60%

## people who DID NOT participate in T8

T8.nonparticipated <- data %>%
  filter(rowSums(!is.na(across(all_of(
    c('extr.8', 'agre.8', 'cons.8', 'open.8', 'neur.8',
      'pros.8', 'fame.8', 'attr.8'))))) == 0) # 1809 obs.

# cohen's d for T8 participated vs. not participated
cohens_d(T8.nonparticipated$extr.1, T8.participated$extr.1, 
         pooled_sd = TRUE, paired = FALSE) # 0.05 [-0.02, 0.13]
cohens_d(T8.nonparticipated$agre.1, T8.participated$agre.1, 
         pooled_sd = TRUE, paired = FALSE) # -0.05 [-0.13, 0.03]
cohens_d(T8.nonparticipated$cons.1, T8.participated$cons.1, 
         pooled_sd = TRUE, paired = FALSE) # -0.18 [-0.26, -0.10] *
cohens_d(T8.nonparticipated$open.1, T8.participated$open.1, 
         pooled_sd = TRUE, paired = FALSE) # 0.09 [0.02, 0.17] *
cohens_d(T8.nonparticipated$neur.1, T8.participated$neur.1, 
         pooled_sd = TRUE, paired = FALSE) # -0.02 [-0.09, 0.06]
cohens_d(T8.nonparticipated$pros.1, T8.participated$pros.1, 
         pooled_sd = TRUE, paired = FALSE) # 0.08 [0.00, 0.15]
cohens_d(T8.nonparticipated$fame.1, T8.participated$fame.1, 
         pooled_sd = TRUE, paired = FALSE) # 0.07 [0.00, 0.15]
cohens_d(T8.nonparticipated$attr.1, T8.participated$attr.1, 
         pooled_sd = TRUE, paired = FALSE) # 0.05 [-0.03, 0.12]
cohens_d(T8.nonparticipated$pers.1, T8.participated$pers.1, 
         pooled_sd = TRUE, paired = FALSE) # 0.08 [0.00, 0.16]
cohens_d(T8.nonparticipated$rela.1, T8.participated$rela.1, 
         pooled_sd = TRUE, paired = FALSE) # -7.98e-03 [-0.08, 0.07]
cohens_d(T8.nonparticipated$soci.1, T8.participated$soci.1, 
         pooled_sd = TRUE, paired = FALSE) # -0.01 [-0.09, 0.06]
cohens_d(T8.nonparticipated$phys.1, T8.participated$phys.1, 
         pooled_sd = TRUE, paired = FALSE) # -3.50e-03 [-0.08, 0.07]
cohens_d(T8.nonparticipated$hedo.1, T8.participated$hedo.1, 
         pooled_sd = TRUE, paired = FALSE) # 0.16 [0.09, 0.24] *
cohens_d(T8.nonparticipated$libe.1, T8.participated$libe.1, 
         pooled_sd = TRUE, paired = FALSE) # -0.01 [-0.09, 0.06]
cohens_d(T8.nonparticipated$secu.3, T8.participated$secu.3, 
         pooled_sd = TRUE, paired = FALSE) # 0.04 [-0.05, 0.13]
cohens_d(T8.nonparticipated$conf.3, T8.participated$conf.3, 
         pooled_sd = TRUE, paired = FALSE) # -0.15 [-0.24, -0.06] *
cohens_d(T8.nonparticipated$reli.3, T8.participated$reli.3, 
         pooled_sd = TRUE, paired = FALSE) # 0.03 [-0.06, 0.12]
cohens_d(T8.nonparticipated$parent.edu, T8.participated$parent.edu, 
         pooled_sd = TRUE, paired = FALSE) # -0.01 [-0.09, 0.06]

describe(T8.participated$extr.1)    # m=2.85
describe(T8.nonparticipated$extr.1) # m=2.87

describe(T8.participated$agre.1)    # m=2.94
describe(T8.nonparticipated$agre.1) # m=2.92

#> note.
#>  d is in the non-participation direction:
#> +d means people who did not participate had higher levels.
#> -d means people who did not participate had lower levels.

# compute p-values
t.test(T8.nonparticipated$cons.1, T8.participated$cons.1) # 2.992e-06
t.test(T8.nonparticipated$open.1, T8.participated$open.1) # 0.01818
t.test(T8.nonparticipated$hedo.1, T8.participated$hedo.1) # 3.421e-05
t.test(T8.nonparticipated$conf.3, T8.participated$conf.3) # 0.001234

# copy data
attrition <- data %>% 
  select(id, gender)

# mark non- vs. participated
attrition$T8participated <- ifelse(
  attrition$id %in% T8.participated$id, 'yes', 'no')

# sub-table by gender
attrition.gender <- table(attrition$T8participated, attrition$gender)

print(attrition.gender) # 1=male, 2=female; 6 people didn't report gender (NAs)

# chi-sq of independence
chisq.test(attrition.gender) # p = 0.1193

# clean
rm(list = ls(pattern = 'participated'))

# analysis 1. rank-order stability #############################################

# remove libe because there's only one wave
key.vars <- setdiff(key.vars, 'libe')

#> note.
#> safe to remove libe because we'll be removing key.vars after this code block.

# group variables by construct
for (var in key.vars) {
  all.var <- paste0('all.', var)
  
  assign(all.var,
         data %>% select(starts_with(var)))
  
  rm(var, all.var)
}

# check random
names(get(paste0('all.', sample(key.vars, 1))))

# export corr and cohen's d matrices
for (var in key.vars) {
  all.var <- get(paste0('all.', var))
  vars <- colnames(all.var)
  
  # create wave pairs
  pairs <- cbind(vars[-length(vars)], vars[-1])         # all adjacent waves
  if (length(vars) >= 4) {                              # T1-T4
    pairs <- rbind(pairs, c(vars[1], vars[4]))
  }
  pairs <- rbind(pairs, c(vars[1], vars[length(vars)])) # T1-T8
  
  # compute correlation and Cohen's d from raw scores
  table <- sapply(1:nrow(pairs), function(i) {
    
    x <- pairs[i, 1]
    y <- pairs[i, 2]
    
    x_data <- all.var[[x]]
    y_data <- all.var[[y]]
    
    # remove cases with missing data in either wave
    keep <- complete.cases(x_data, y_data)
    x_data <- x_data[keep]
    y_data <- y_data[keep]
    
    # correlation
    r <- cor(x_data, y_data, use = 'pairwise.complete.obs')
    
    # Cohen's d from raw scores
    d <- (mean(y_data) - mean(x_data)) / sd(c(x_data, y_data))
    
    # format correlation
    r_str <- sub('^(-?)0\\.', '\\1.', sprintf('%.2f', r))
    d_str <- sub('^(-?)0\\.', '\\1.', sprintf('%.2f', round(d, 2)))
    
    paste0(r_str, ' (', d_str, ')')
  })
  
  names(table) <- apply(pairs, 1, paste, collapse = ' — ')
  table <- as.data.frame(as.list(table), stringsAsFactors = FALSE)
  
  # export
  write.xlsx(table, 
             here('Output/Primary/Analysis 1/', 
                  paste0('rank_order_stability_', var, '.xlsx')),
             rowNames = FALSE)
  
  rm(var, vars, pairs, table)
}

# clean environment
rm(list = ls(pattern = '^all\\.'))

# analysis 2. mean-level changes ###############################################

# initiate df for logging best-fitting univariate models
univariate <- data.frame(var = key.vars,
                         '1:8' = NA,
                         '1:4' = NA,
                         '4:8' = NA,
                         check.names = FALSE)

# clean environment
rm(list = ls(pattern = '^all.'), key.vars)

## extraversion ################################################################

## 8 waves (T1-8)

## no change

# script
extr.nochng.script <- nochng.script('extr', 1:8)
cat(extr.nochng.script)

# fit model
extr.nochng <- sem(extr.nochng.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(extr.nochng, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## linear

# script
extr.linear.script <- linear.script('extr', 1:8)
cat(extr.linear.script)

# fit model
extr.linear <- sem(extr.linear.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(extr.linear, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## quadratic

# script
extr.quadra.script <- quadra.script('extr', 1:8)
cat(extr.quadra.script)

# fit model
extr.quadra <- sem(extr.quadra.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(extr.quadra, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## latent basis

# script
extr.latent.script <- latent.script('extr', 1:8)
cat(extr.latent.script)

# fit model
extr.latent <- sem(extr.latent.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(extr.latent, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.univariate.results(extr.nochng,
                          extr.linear,
                          extr.quadra,
                          extr.latent,
                          NA, 1:8)

## model comparisons

# compare model fits
summary(compareFit(extr.nochng, extr.linear))             # linear fits better
summary(compareFit(extr.linear, extr.quadra))             # quadra fits better
summary(compareFit(extr.linear, extr.latent))             # latent fits better
summary(compareFit(extr.quadra, extr.latent, nested = F)) # quadra fits best

# update summary
univariate[univariate$var == 'extr', '1:8'] <- 'quadra'

# clean environment
rm(list = setdiff(ls(pattern = '^extr'), 'extr.quadra'))

## plot

# average trajectory
extr.average.plot <- plot.average(extr.quadra, 1:8)
print(extr.average.plot)

# if looks good, save plot
ggsave(filename = here('Output/Primary/Analysis 2/Plots/extr_average.png'), 
       plot = extr.average.plot,
       width = 6, height = 4.5, scale = 1.5)

# individual trajectories
extr.spaghetti.plot <- plot.spaghetti(extr.quadra, 1:8)
print(extr.spaghetti.plot)

# if looks good, save plot
ggsave(filename = here('Output/Primary/Analysis 2/Plots/extr_spaghetti.png'), 
       plot = extr.spaghetti.plot,
       width = 6, height = 4.5, scale = 1.5)

# combined (average + spaghetti)
extr.combined.plot <- plot.combined(extr.quadra, 
                                    extr.average.data, 
                                    extr.spaghetti.data, 
                                    1:8)
print(extr.combined.plot)

# if looks good, save plot
ggsave(filename = here('Output/Primary/Analysis 2/Plots/extr_combined.png'), 
       plot = extr.combined.plot,
       width = 6, height = 4.5, scale = 1.5)

# clean environment
rm(list = ls(pattern = '^extr\\.|rows|^time'))

## 4 waves (T1-4)

## no change

# script
extr.nochng.script <- nochng.script('extr', 1:4)
cat(extr.nochng.script)

# fit model
extr.nochng <- sem(extr.nochng.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(extr.nochng, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## linear

# script
extr.linear.script <- linear.script('extr', 1:4)
cat(extr.linear.script)

# fit model
extr.linear <- sem(extr.linear.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(extr.linear, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## quadratic

# script
extr.quadra.script <- quadra.script('extr', 1:4)
cat(extr.quadra.script)

# fit model
extr.quadra <- sem(extr.quadra.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(extr.quadra, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## latent basis

# script
extr.latent.script <- latent.script('extr', 1:4)
cat(extr.latent.script)

# fit model
extr.latent <- sem(extr.latent.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(extr.latent, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.univariate.results(extr.nochng,
                          extr.linear,
                          extr.quadra,
                          extr.latent,
                          NA, 1:4)

## model comparisons

# compare model fits
summary(compareFit(extr.nochng, extr.linear))             # linear fits better
summary(compareFit(extr.linear, extr.quadra))             # quadra fits better
summary(compareFit(extr.linear, extr.latent))             # latent fits better
summary(compareFit(extr.quadra, extr.latent, nested = F)) # quadra fits best

# update summary
univariate[univariate$var == 'extr', '1:4'] <- 'quadra'

# clean environment
rm(list = setdiff(ls(pattern = '^extr'), 'extr.quadra'))

## plot

# average trajectory
extr.average.plot <- plot.average(extr.quadra, 1:4)
print(extr.average.plot)

# individual trajectories
extr.spaghetti.plot <- plot.spaghetti(extr.quadra, 1:4)
print(extr.spaghetti.plot)

# combined (average + spaghetti)
extr.combined.plot <- plot.combined(extr.quadra, 
                                    extr.average.data, 
                                    extr.spaghetti.data,
                                    1:4)
print(extr.combined.plot)

# clean environment
rm(list = ls(pattern = '^extr\\.|rows|^time'))

## 4 waves (T4-8)

## no change

# script
extr.nochng.script <- nochng.script('extr', 4:8)
cat(extr.nochng.script)

# fit model
extr.nochng <- sem(extr.nochng.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(extr.nochng, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## linear

# script
extr.linear.script <- linear.script('extr', 4:8)
cat(extr.linear.script)

# fit model
extr.linear <- sem(extr.linear.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(extr.linear, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## quadratic

# script
extr.quadra.script <- quadra.script('extr', 4:8)
cat(extr.quadra.script)

# fit model
extr.quadra <- sem(extr.quadra.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(extr.quadra, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## latent basis

# script
extr.latent.script <- latent.script('extr', 4:8)
cat(extr.latent.script)

# fit model
extr.latent <- sem(extr.latent.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(extr.latent, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.univariate.results(extr.nochng,
                          extr.linear,
                          extr.quadra,
                          extr.latent,
                          NA, 4:8)

## model comparisons

# compare model fits
summary(compareFit(extr.nochng, extr.linear))                 # linear fits better
summary(compareFit(extr.linear, extr.quadra))                 # quadra fits better
summary(compareFit(extr.linear, extr.latent))                 # latent fits better
summary(compareFit(extr.quadra, extr.latent, nested = FALSE)) # latent fits best

# update summary
univariate[univariate$var == 'extr', '4:8'] <- 'latent'

# clean environment
rm(list = setdiff(ls(pattern = '^extr'), 'extr.latent'))

## plot

# average trajectory
extr.average.plot <- plot.average(extr.latent, 4:8)
print(extr.average.plot)

# individual trajectories
extr.spaghetti.plot <- plot.spaghetti(extr.latent, 4:8)
print(extr.spaghetti.plot)

# combined (average + spaghetti)
extr.combined.plot <- plot.combined(extr.latent, 
                                    extr.average.data, 
                                    extr.spaghetti.data, 
                                    4:8)
print(extr.combined.plot)

# clean environment
rm(list = ls(pattern = '^extr\\.|rows|^time'))

## agreeableness ###############################################################

## 8 waves (T1-8)

## no change

# script
agre.nochng.script <- nochng.script('agre', 1:8)
cat(agre.nochng.script)

# fit model
agre.nochng <- sem(agre.nochng.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(agre.nochng, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## linear

# script
agre.linear.script <- linear.script('agre', 1:8)
cat(agre.linear.script)

# fit model
agre.linear <- sem(agre.linear.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(agre.linear, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## quadratic

# script
agre.quadra.script <- quadra.script('agre', 1:8)
cat(agre.quadra.script)

# fit model
agre.quadra <- sem(agre.quadra.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(agre.quadra, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## latent basis

# script
agre.latent.script <- latent.script('agre', 1:8)
cat(agre.latent.script)

# fit model
agre.latent <- sem(agre.latent.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(agre.latent, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.univariate.results(agre.nochng,
                          agre.linear,
                          agre.quadra,
                          agre.latent,
                          NA, 1:8)

## model comparisons

# compare model fits
summary(compareFit(agre.nochng, agre.linear))             # linear fits better
summary(compareFit(agre.linear, agre.quadra))             # quadra fits better
summary(compareFit(agre.linear, agre.latent))             # latent fits better
summary(compareFit(agre.quadra, agre.latent, nested = F)) # latent fits best

# update summary
univariate[univariate$var == 'agre', '1:8'] <- 'latent'

# clean environment
rm(list = setdiff(ls(pattern = '^agre'), 'agre.latent'))

## plot

# average trajectory
agre.average.plot <- plot.average(agre.latent, 1:8)
print(agre.average.plot)

# if looks good, save plot
ggsave(filename = here('Output/Primary/Analysis 2/Plots/agre_average.png'), 
       plot = agre.average.plot,
       width = 6, height = 4.5, scale = 1.5)

# individual trajectories
agre.spaghetti.plot <- plot.spaghetti(agre.latent, 1:8)
print(agre.spaghetti.plot)

# if looks good, save plot
ggsave(filename = here('Output/Primary/Analysis 2/Plots/agre_spaghetti.png'), 
       plot = agre.spaghetti.plot,
       width = 6, height = 4.5, scale = 1.5)

# combined (average + spaghetti)
agre.combined.plot <- plot.combined(agre.latent, 
                                    agre.average.data, 
                                    agre.spaghetti.data,
                                    1:8)
print(agre.combined.plot)

# if looks good, save plot
ggsave(filename = here('Output/Primary/Analysis 2/Plots/agre_combined.png'), 
       plot = agre.combined.plot,
       width = 6, height = 4.5, scale = 1.5)

# clean environment
rm(list = ls(pattern = '^agre\\.|rows|^time'))

## 4 waves (T1-4)

## no change

# script
agre.nochng.script <- nochng.script('agre', 1:4)
cat(agre.nochng.script)

# fit model
agre.nochng <- sem(agre.nochng.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(agre.nochng, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## linear

# script
agre.linear.script <- linear.script('agre', 1:4)
cat(agre.linear.script)

# fit model
agre.linear <- sem(agre.linear.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(agre.linear, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## quadratic

# script
agre.quadra.script <- quadra.script('agre', 1:4)
cat(agre.quadra.script)

# fit model
agre.quadra <- sem(agre.quadra.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(agre.quadra, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## latent basis

# script
agre.latent.script <- latent.script('agre', 1:4)
cat(agre.latent.script)

# fit model
agre.latent <- sem(agre.latent.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(agre.latent, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.univariate.results(agre.nochng,
                          agre.linear,
                          agre.quadra,
                          agre.latent,
                          NA, 1:4)

## model comparisons

# compare model fits
summary(compareFit(agre.nochng, agre.linear))             # linear fits better
summary(compareFit(agre.linear, agre.quadra))             # quadra fits better
summary(compareFit(agre.linear, agre.latent))             # latent fits better
summary(compareFit(agre.quadra, agre.latent, nested = F)) # latent fits best

# update summary
univariate[univariate$var == 'agre', '1:4'] <- 'latent'

# clean environment
rm(list = setdiff(ls(pattern = '^agre'), 'agre.latent'))

## plot

# average trajectory
agre.average.plot <- plot.average(agre.latent, 1:4)
print(agre.average.plot)

# individual trajectories
agre.spaghetti.plot <- plot.spaghetti(agre.latent, 1:4)
print(agre.spaghetti.plot)

# combined (average + spaghetti)
agre.combined.plot <- plot.combined(agre.latent, 
                                    agre.average.data, 
                                    agre.spaghetti.data,
                                    1:4)
print(agre.combined.plot)

# clean environment
rm(list = ls(pattern = '^agre\\.|rows|^time'))

## 4 waves (T4-8)

## no change

# script
agre.nochng.script <- nochng.script('agre', 4:8)
cat(agre.nochng.script)

# fit model
agre.nochng <- sem(agre.nochng.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(agre.nochng, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## linear

# script
agre.linear.script <- linear.script('agre', 4:8)
cat(agre.linear.script)

# fit model
agre.linear <- sem(agre.linear.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(agre.linear, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## quadratic

# script
agre.quadra.script <- quadra.script('agre', 4:8)
cat(agre.quadra.script)

# fit model
agre.quadra <- sem(agre.quadra.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(agre.quadra, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## latent basis

# script
agre.latent.script <- latent.script('agre', 4:8)
cat(agre.latent.script)

# fit model
agre.latent <- sem(agre.latent.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(agre.latent, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.univariate.results(agre.nochng,
                          agre.linear,
                          agre.quadra,
                          agre.latent,
                          NA, 4:8)

## model comparisons

# compare model fits
summary(compareFit(agre.nochng, agre.linear))                 # linear fits better
summary(compareFit(agre.linear, agre.quadra))                 # quadra fits better
summary(compareFit(agre.linear, agre.latent))                 # latent fits better
summary(compareFit(agre.quadra, agre.latent, nested = FALSE)) # latent fits best

# update summary
univariate[univariate$var == 'agre', '4:8'] <- 'latent'

# clean environment
rm(list = setdiff(ls(pattern = '^agre'), 'agre.latent'))

## plot

# average trajectory
agre.average.plot <- plot.average(agre.latent, 4:8)
print(agre.average.plot)

# individual trajectories
agre.spaghetti.plot <- plot.spaghetti(agre.latent, 4:8)
print(agre.spaghetti.plot)

# combined (average + spaghetti)
agre.combined.plot <- plot.combined(agre.latent, 
                                    agre.average.data, 
                                    agre.spaghetti.data,
                                    4:8)
print(agre.combined.plot)

# clean environment
rm(list = ls(pattern = '^agre\\.|rows|^time'))

## conscientiousness ###########################################################

## 8 waves (T1-T8)

## no change

# script
cons.nochng.script <- nochng.script('cons', 1:8)
cat(cons.nochng.script)

# fit model
cons.nochng <- sem(cons.nochng.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(cons.nochng, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## linear

# script
cons.linear.script <- linear.script('cons', 1:8)
cat(cons.linear.script)

# fit model
cons.linear <- sem(cons.linear.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(cons.linear, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## quadratic

# script
cons.quadra.script <- quadra.script('cons', 1:8)
cat(cons.quadra.script)

# fit model
cons.quadra <- sem(cons.quadra.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(cons.quadra, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## latent basis

# script
cons.latent.script <- latent.script('cons', 1:8)
cat(cons.latent.script)

# fit model
cons.latent <- sem(cons.latent.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(cons.latent, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.univariate.results(cons.nochng,
                          cons.linear,
                          cons.quadra,
                          cons.latent,
                          NA, 1:8)

## model comparisons

# compare model fits
summary(compareFit(cons.nochng, cons.linear))             # linear fits better
summary(compareFit(cons.linear, cons.quadra))             # quadra fits better
summary(compareFit(cons.linear, cons.latent))             # latent fits better
summary(compareFit(cons.quadra, cons.latent, nested = F)) # quadra fits best

# update summary
univariate[univariate$var == 'cons', '1:8'] <- 'quadra'

# clean environment
rm(list = setdiff(ls(pattern = '^cons'), 'cons.quadra'))

## plot

# average trajectory
cons.average.plot <- plot.average(cons.quadra, 1:8)
print(cons.average.plot)

# if looks good, save plot
ggsave(filename = here('Output/Primary/Analysis 2/Plots/cons_average.png'), 
       plot = cons.average.plot,
       width = 6, height = 4.5, scale = 1.5)

# individual trajectories
cons.spaghetti.plot <- plot.spaghetti(cons.quadra, 1:8)
print(cons.spaghetti.plot)

# if looks good, save plot
ggsave(filename = here('Output/Primary/Analysis 2/Plots/cons_spaghetti.png'), 
       plot = cons.spaghetti.plot,
       width = 6, height = 4.5, scale = 1.5)

# combined (average + spaghetti)
cons.combined.plot <- plot.combined(cons.quadra, 
                                    cons.average.data, 
                                    cons.spaghetti.data,
                                    1:8)
print(cons.combined.plot)

# if looks good, save plot
ggsave(filename = here('Output/Primary/Analysis 2/Plots/cons_combined.png'), 
       plot = cons.combined.plot,
       width = 6, height = 4.5, scale = 1.5)

# clean environment
rm(list = ls(pattern = '^cons\\.|rows|^time'))

## 4 waves (T1-4)

## no change

# script
cons.nochng.script <- nochng.script('cons', 1:4)
cat(cons.nochng.script)

# fit model
cons.nochng <- sem(cons.nochng.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(cons.nochng, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## linear

# script
cons.linear.script <- linear.script('cons', 1:4)
cat(cons.linear.script)

# fit model
cons.linear <- sem(cons.linear.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(cons.linear, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## quadratic

# script
cons.quadra.script <- quadra.script('cons', 1:4)
cat(cons.quadra.script)

# fit model
cons.quadra <- sem(cons.quadra.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(cons.quadra, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## latent basis

# script
cons.latent.script <- latent.script('cons', 1:4)
cat(cons.latent.script)

# fit model
cons.latent <- sem(cons.latent.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(cons.latent, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.univariate.results(cons.nochng,
                          cons.linear,
                          cons.quadra,
                          cons.latent,
                          NA, 1:4)

## model comparisons

# compare model fits
summary(compareFit(cons.nochng, cons.linear))             # linear fits better
summary(compareFit(cons.linear, cons.quadra))             # quadra fits better
summary(compareFit(cons.linear, cons.latent))             # latent fits better
summary(compareFit(cons.quadra, cons.latent, nested = F)) # latent fits best

# update summary
univariate[univariate$var == 'cons', '1:4'] <- 'latent'

# clean environment
rm(list = setdiff(ls(pattern = '^cons'), 'cons.latent'))

## plot

# average trajectory
cons.average.plot <- plot.average(cons.latent, 1:4)
print(cons.average.plot)

# individual trajectories
cons.spaghetti.plot <- plot.spaghetti(cons.latent, 1:4)
print(cons.spaghetti.plot)

# combined (average + spaghetti)
cons.combined.plot <- plot.combined(cons.latent, 
                                    cons.average.data, 
                                    cons.spaghetti.data,
                                    1:4)
print(cons.combined.plot)

# clean environment
rm(list = ls(pattern = '^cons\\.|rows|^time'))

## 4 waves (T4-8)

## no change

# script
cons.nochng.script <- nochng.script('cons', 4:8)
cat(cons.nochng.script)

# fit model
cons.nochng <- sem(cons.nochng.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(cons.nochng, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## linear

# script
cons.linear.script <- linear.script('cons', 4:8)
cat(cons.linear.script)

# fit model
cons.linear <- sem(cons.linear.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(cons.linear, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## quadratic

# script
cons.quadra.script <- quadra.script('cons', 4:8)
cat(cons.quadra.script)

# fit model
cons.quadra <- sem(cons.quadra.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(cons.quadra, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## latent basis

# script
cons.latent.script <- latent.script('cons', 4:8)
cat(cons.latent.script)

# fit model
cons.latent <- sem(cons.latent.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(cons.latent, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.univariate.results(cons.nochng,
                          cons.linear,
                          cons.quadra,
                          cons.latent,
                          NA, 4:8)

## model comparisons

# compare model fits
summary(compareFit(cons.nochng, cons.linear))             # linear fits better
summary(compareFit(cons.linear, cons.quadra))             # quadra fits better
summary(compareFit(cons.linear, cons.latent))             # latent fits better
summary(compareFit(cons.quadra, cons.latent, nested = F)) # latent fits best

# update summary
univariate[univariate$var == 'cons', '4:8'] <- 'latent'

# clean environment
rm(list = setdiff(ls(pattern = '^cons'), 'cons.latent'))

## plot

# average trajectory
cons.average.plot <- plot.average(cons.latent, 4:8)
print(cons.average.plot)

# individual trajectories
cons.spaghetti.plot <- plot.spaghetti(cons.latent, 4:8)
print(cons.spaghetti.plot)

# combined (average + spaghetti)
cons.combined.plot <- plot.combined(cons.latent, 
                                    cons.average.data, 
                                    cons.spaghetti.data,
                                    4:8)
print(cons.combined.plot)

# clean environment
rm(list = ls(pattern = '^cons\\.|rows|^time'))

## neuroticism #################################################################

## 8 waves (T1-8)

## no change

# script
neur.nochng.script <- nochng.script('neur', 1:8)
cat(neur.nochng.script)

# fit model
neur.nochng <- sem(neur.nochng.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(neur.nochng, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## linear

# script
neur.linear.script <- linear.script('neur', 1:8)
cat(neur.linear.script)

# fit model
neur.linear <- sem(neur.linear.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(neur.linear, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## quadratic

# script
neur.quadra.script <- quadra.script('neur', 1:8)
cat(neur.quadra.script)

# fit model
neur.quadra <- sem(neur.quadra.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(neur.quadra, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## latent basis

# script
neur.latent.script <- latent.script('neur', 1:8)
cat(neur.latent.script)

# fit model
neur.latent <- sem(neur.latent.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(neur.latent, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.univariate.results(neur.nochng,
                          neur.linear,
                          neur.quadra,
                          neur.latent,
                          NA, 1:8)

## model comparisons

# compare model fits
summary(compareFit(neur.nochng, neur.linear))             # linear fits better
summary(compareFit(neur.linear, neur.quadra))             # quadra fits better
summary(compareFit(neur.linear, neur.latent))             # latent fits better
summary(compareFit(neur.quadra, neur.latent, nested = F)) # quadra fits best

# update summary
univariate[univariate$var == 'neur', '1:8'] <- 'quadra'

# clean environment
rm(list = setdiff(ls(pattern = '^neur'), 'neur.quadra'))

## plot

# average trajectory
neur.average.plot <- plot.average(neur.quadra, 1:8)
print(neur.average.plot)

# if looks good, save plot
ggsave(filename = here('Output/Primary/Analysis 2/Plots/neur_average.png'), 
       plot = neur.average.plot,
       width = 6, height = 4.5, scale = 1.5)

# individual trajectories
neur.spaghetti.plot <- plot.spaghetti(neur.quadra, 1:8)
print(neur.spaghetti.plot)

# if looks good, save plot
ggsave(filename = here('Output/Primary/Analysis 2/Plots/neur_spaghetti.png'), 
       plot = neur.spaghetti.plot,
       width = 6, height = 4.5, scale = 1.5)

# combined (average + spaghetti)
neur.combined.plot <- plot.combined(neur.quadra, 
                                    neur.average.data, 
                                    neur.spaghetti.data,
                                    1:8)
print(neur.combined.plot)

# if looks good, save plot
ggsave(filename = here('Output/Primary/Analysis 2/Plots/neur_combined.png'), 
       plot = neur.combined.plot,
       width = 6, height = 4.5, scale = 1.5)

# clean environment
rm(list = ls(pattern = '^neur\\.|rows|^time'))

## 4 waves (T1-4)

## no change

# script
neur.nochng.script <- nochng.script('neur', 1:4)
cat(neur.nochng.script)

# fit model
neur.nochng <- sem(neur.nochng.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(neur.nochng, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## linear

# script
neur.linear.script <- linear.script('neur', 1:4)
cat(neur.linear.script)

# fit model
neur.linear <- sem(neur.linear.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(neur.linear, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## quadratic

# script
neur.quadra.script <- quadra.script('neur', 1:4)
cat(neur.quadra.script)

# fit model
neur.quadra <- sem(neur.quadra.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(neur.quadra, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## latent basis

# script
neur.latent.script <- latent.script('neur', 1:4)
cat(neur.latent.script)

# fit model
neur.latent <- sem(neur.latent.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(neur.latent, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.univariate.results(neur.nochng,
                          neur.linear,
                          neur.quadra,
                          neur.latent,
                          NA, 1:4)

## model comparisons

# compare model fits
summary(compareFit(neur.nochng, neur.linear))             # linear fits better
summary(compareFit(neur.linear, neur.quadra))             # quadra fits better
summary(compareFit(neur.linear, neur.latent))             # latent fits better
summary(compareFit(neur.quadra, neur.latent, nested = F)) # quadra fits best

# update summary
univariate[univariate$var == 'neur', '1:4'] <- 'quadra'

# clean environment
rm(list = setdiff(ls(pattern = '^neur'), 'neur.quadra'))

## plot

# average trajectory
neur.average.plot <- plot.average(neur.quadra, 1:4)
print(neur.average.plot)

# individual trajectories
neur.spaghetti.plot <- plot.spaghetti(neur.quadra, 1:4)
print(neur.spaghetti.plot)

# combined (average + spaghetti)
neur.combined.plot <- plot.combined(neur.quadra, 
                                    neur.average.data, 
                                    neur.spaghetti.data,
                                    1:4)
print(neur.combined.plot)

# clean environment
rm(list = ls(pattern = '^neur\\.|rows|^time'))

## 4 waves (T4-8)

## no change

# script
neur.nochng.script <- nochng.script('neur', 4:8)
cat(neur.nochng.script)

# fit model
neur.nochng <- sem(neur.nochng.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(neur.nochng, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## linear

# script
neur.linear.script <- linear.script('neur', 4:8)
cat(neur.linear.script)

# fit model
neur.linear <- sem(neur.linear.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(neur.linear, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## quadratic

# script
neur.quadra.script <- quadra.script('neur', 4:8)
cat(neur.quadra.script)

# fit model
neur.quadra <- sem(neur.quadra.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(neur.quadra, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## latent basis

# script
neur.latent.script <- latent.script('neur', 4:8)
cat(neur.latent.script)

# fit model
neur.latent <- sem(neur.latent.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(neur.latent, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.univariate.results(neur.nochng,
                          neur.linear,
                          neur.quadra,
                          neur.latent,
                          NA, 4:8)

## model comparisons

# compare model fits
summary(compareFit(neur.nochng, neur.linear))                 # linear fits better
summary(compareFit(neur.linear, neur.quadra))                 # quadra fits better
summary(compareFit(neur.linear, neur.latent))                 # latent fits better
summary(compareFit(neur.quadra, neur.latent, nested = FALSE)) # latent fits best

# update summary
univariate[univariate$var == 'neur', '4:8'] <- 'latent'

# clean environment
rm(list = setdiff(ls(pattern = '^neur'), 'neur.latent'))

## plot

# average trajectory
neur.average.plot <- plot.average(neur.latent, 4:8)
print(neur.average.plot)

# individual trajectories
neur.spaghetti.plot <- plot.spaghetti(neur.latent, 4:8)
print(neur.spaghetti.plot)

# combined (average + spaghetti)
neur.combined.plot <- plot.combined(neur.latent, 
                                    neur.average.data, 
                                    neur.spaghetti.data,
                                    4:8)
print(neur.combined.plot)

# clean environment
rm(list = ls(pattern = '^neur\\.|rows|^time'))

## openness ####################################################################

## 8 waves (T1-T8)

## no change

# script
open.nochng.script <- nochng.script('open', 1:8)
cat(open.nochng.script)

# fit model
open.nochng <- sem(open.nochng.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(open.nochng, 
        fit.measures = TRUE, 
        standardized = TRUE, 
        nd = 3)

## linear

# script
open.linear.script <- linear.script('open', 1:8)
cat(open.linear.script)

# fit model
open.linear <- sem(open.linear.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(open.linear, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## quadratic

# script
open.quadra.script <- quadra.script('open', 1:8)
cat(open.quadra.script)

# fit model
open.quadra <- sem(open.quadra.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(open.quadra, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## latent basis

# script
open.latent.script <- latent.script('open', 1:8)
cat(open.latent.script)

# fit model
open.latent <- sem(open.latent.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(open.latent, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.univariate.results(open.nochng,
                          open.linear,
                          open.quadra,
                          open.latent,
                          NA, 1:8)

## model comparisons

# compare model fits
summary(compareFit(open.nochng, open.linear))                 # linear fits better
summary(compareFit(open.linear, open.quadra))                 # quadra fits better
summary(compareFit(open.linear, open.latent))                 # latent fits better
summary(compareFit(open.quadra, open.latent, nested = FALSE)) # quadra fits best

# update summary
univariate[univariate$var == 'open', '1:8'] <- 'quadra'

# clean environment
rm(list = setdiff(ls(pattern = '^open'), 'open.quadra'))

## plot

# average trajectory
open.average.plot <- plot.average(open.quadra, 1:8)
print(open.average.plot)

# if looks good, save plot
ggsave(filename = here('Output/Primary/Analysis 2/Plots/open_average.png'), 
       plot = open.average.plot,
       width = 6, height = 4.5, scale = 1.5)

# individual trajectories
open.spaghetti.plot <- plot.spaghetti(open.quadra, 1:8)
print(open.spaghetti.plot)

# if looks good, save plot
ggsave(filename = here('Output/Primary/Analysis 2/Plots/open_spaghetti.png'), 
       plot = open.spaghetti.plot,
       width = 6, height = 4.5, scale = 1.5)

# combined (average + spaghetti)
open.combined.plot <- plot.combined(open.quadra, 
                                    open.average.data, 
                                    open.spaghetti.data,
                                    1:8)
print(open.combined.plot)

# if looks good, save plot
ggsave(filename = here('Output/Primary/Analysis 2/Plots/open_combined.png'), 
       plot = open.combined.plot,
       width = 6, height = 4.5, scale = 1.5)

# clean environment
rm(list = ls(pattern = '^open\\.|rows|^time'))

## 4 waves (T1-4)

## no change

# script
open.nochng.script <- nochng.script('open', 1:4)
cat(open.nochng.script)

# fit model
open.nochng <- sem(open.nochng.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(open.nochng, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## linear

# script
open.linear.script <- linear.script('open', 1:4)
cat(open.linear.script)

# fit model
open.linear <- sem(open.linear.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(open.linear, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## quadratic

# script
open.quadra.script <- quadra.script('open', 1:4)
cat(open.quadra.script)

# fit model
open.quadra <- sem(open.quadra.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(open.quadra, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## latent basis

# script
open.latent.script <- latent.script('open', 1:4)
cat(open.latent.script)

# fit model
open.latent <- sem(open.latent.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(open.latent, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.univariate.results(open.nochng,
                          open.linear,
                          open.quadra,
                          open.latent,
                          NA, 1:4)

## model comparisons

# compare model fits
summary(compareFit(open.nochng, open.linear))                 # linear fits better
summary(compareFit(open.linear, open.quadra))                 # quadra fits better
summary(compareFit(open.linear, open.latent))                 # latent fits better
summary(compareFit(open.quadra, open.latent, nested = FALSE)) # quadra fits best

# update summary
univariate[univariate$var == 'open', '1:4'] <- 'quadra'

# clean environment
rm(list = setdiff(ls(pattern = '^open'), 'open.quadra'))

## plot

# average trajectory
open.average.plot <- plot.average(open.quadra, 1:4)
print(open.average.plot)

# individual trajectories
open.spaghetti.plot <- plot.spaghetti(open.quadra, 1:4)
print(open.spaghetti.plot)

# combined (average + spaghetti)
open.combined.plot <- plot.combined(open.quadra, 
                                    open.average.data, 
                                    open.spaghetti.data,
                                    1:4)
print(open.combined.plot)

# clean environment
rm(list = ls(pattern = '^open\\.|rows|^time'))

## 4 waves (T4-8)

## no change

# script
open.nochng.script <- nochng.script('open', 4:8)
cat(open.nochng.script)

# fit model
open.nochng <- sem(open.nochng.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(open.nochng, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## linear

# script
open.linear.script <- linear.script('open', 4:8)
cat(open.linear.script)

# fit model
open.linear <- sem(open.linear.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(open.linear, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## quadratic

# script
open.quadra.script <- quadra.script('open', 4:8)
cat(open.quadra.script)

# fit model
open.quadra <- sem(open.quadra.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(open.quadra, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## latent basis

# script
open.latent.script <- latent.script('open', 4:8)
cat(open.latent.script)

# fit model
open.latent <- sem(open.latent.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(open.latent, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.univariate.results(open.nochng,
                          open.linear,
                          open.quadra,
                          open.latent,
                          NA, 4:8)

## model comparisons

# compare model fits
summary(compareFit(open.nochng, open.linear))                 # linear fits better
summary(compareFit(open.linear, open.quadra))                 # quadra fits better
summary(compareFit(open.linear, open.latent))                 # latent fits better
summary(compareFit(open.quadra, open.latent, nested = FALSE)) # quadra fits best

# update summary
univariate[univariate$var == 'open', '4:8'] <- 'quadra'

# clean environment
rm(list = setdiff(ls(pattern = '^open'), 'open.quadra'))

## plot

# average trajectory
open.average.plot <- plot.average(open.quadra, 4:8)
print(open.average.plot)

# individual trajectories
open.spaghetti.plot <- plot.spaghetti(open.quadra, 4:8)
print(open.spaghetti.plot)

# combined (average + spaghetti)
open.combined.plot <- plot.combined(open.quadra, 
                                    open.average.data, 
                                    open.spaghetti.data,
                                    4:8)
print(open.combined.plot)

# clean environment
rm(list = ls(pattern = '^open\\.|rows|^time'))

## > average plots #############################################################

## re-create plot data

# scripts
open.quadra.script <- quadra.script('open', 1:8)
cons.quadra.script <- quadra.script('cons', 1:8)
extr.quadra.script <- quadra.script('extr', 1:8)
agre.latent.script <- latent.script('agre', 1:8)
neur.quadra.script <- quadra.script('neur', 1:8)

# fit models
open.quadra <- sem(open.quadra.script, data = data, missing = 'fiml', estimator = 'mlr')
cons.quadra <- sem(cons.quadra.script, data = data, missing = 'fiml', estimator = 'mlr')
extr.quadra <- sem(extr.quadra.script, data = data, missing = 'fiml', estimator = 'mlr')
agre.latent <- sem(agre.latent.script, data = data, missing = 'fiml', estimator = 'mlr')
neur.quadra <- sem(neur.quadra.script, data = data, missing = 'fiml', estimator = 'mlr')

# create plot data
open.average.plot <- plot.average(open.quadra, 1:8)
cons.average.plot <- plot.average(cons.quadra, 1:8)
extr.average.plot <- plot.average(extr.quadra, 1:8)
agre.average.plot <- plot.average(agre.latent, 1:8)
neur.average.plot <- plot.average(neur.quadra, 1:8)

# combine plot data (with labels)
bigfive.data <- bind_rows(open.average.data %>% mutate(trait = 'Openness to Experience'),
                          cons.average.data %>% mutate(trait = 'Conscientiousness'),
                          extr.average.data %>% mutate(trait = 'Extraversion'),
                          agre.average.data %>% mutate(trait = 'Agreeableness'),
                          neur.average.data %>% mutate(trait = 'Neuroticism'))

# re-scale for plot
bigfive.data$time.scaled <- bigfive.data$time * 4

# plot all growth curves
bigfive.plot <-
  ggplot(bigfive.data, aes(x = time.scaled,
                           y = predicted,
                           color = trait,
                           group = trait)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = 'Big Five Personality Trait Growth Trajectories',
       subtitle = 'T1-T8',
       x = 'Time (Years)',
       y = 'Level',
       color = 'Big Five Personality Trait') +
  scale_x_continuous(limits = c(0, 18),
                     breaks = seq(0, 18, 2)) +
  scale_y_continuous(limits = c(1, 4)) +
  scale_color_manual(values = c('Openness to Experience' = '#ffb900',
                                'Conscientiousness' = '#006cc4',
                                'Extraversion' = '#00ae3a',
                                'Agreeableness' = '#fc0909',
                                'Neuroticism' = '#6c1d9d')) +
  theme_classic()

# view plot
print(bigfive.plot)

# if looks good, save plot
ggsave(filename = here('Output/Primary/Analysis 2/Plots/bigfive_average.png'), 
       plot = bigfive.plot,
       width = 6.5, height = 4.5, scale = 1.5)

## > spaghetti plots ###########################################################

# create plot data
open.spaghetti.plot <- plot.spaghetti(open.quadra, 1:8)
cons.spaghetti.plot <- plot.spaghetti(cons.quadra, 1:8)
extr.spaghetti.plot <- plot.spaghetti(extr.quadra, 1:8)
agre.spaghetti.plot <- plot.spaghetti(agre.latent, 1:8)
neur.spaghetti.plot <- plot.spaghetti(neur.quadra, 1:8)

# combine plot data (with labels)
bigfive.spaghetti.data <- bind_rows(open.spaghetti.data %>% mutate(trait = 'Openness to Experience'),
                                    cons.spaghetti.data %>% mutate(trait = 'Conscientiousness'),
                                    extr.spaghetti.data %>% mutate(trait = 'Extraversion'),
                                    agre.spaghetti.data %>% mutate(trait = 'Agreeableness'),
                                    neur.spaghetti.data %>% mutate(trait = 'Neuroticism'))

# re-scale for plot
bigfive.spaghetti.data$time.scaled <- bigfive.spaghetti.data$time * 4

# plot all growth curves
bigfive.spaghetti.plot <- 
  ggplot(bigfive.spaghetti.data, aes(x = time.scaled,
                                y = predicted,
                                color = trait,
                                group = row)) +
  geom_line(size = 0.2, alpha = 0.2) +
  labs(title = 'Big Five Personality Trait Growth Trajectories',
       subtitle = 'T1-T8',
       x = 'Time (Years)',
       y = 'Level',
       color = 'Big Five Personality Trait') +
  scale_x_continuous(limits = c(0, 18),
                     breaks = seq(0, 18, 2)) +
  scale_y_continuous(limits = c(1, 4)) +
  scale_color_manual(values = c('Openness to Experience' = '#ffb900',
                                'Conscientiousness' = '#006cc4',
                                'Extraversion' = '#00ae3a',
                                'Agreeableness' = '#fc0909',
                                'Neuroticism' = '#6c1d9d')) +
  facet_wrap(~ trait, ncol = 3) +
  theme_classic() +
  guides(color = 'none')

# view plot
print(bigfive.spaghetti.plot)

# save plot
ggsave(filename = here('Output/Primary/Analysis 2/Plots/bigfive_spaghetti.png'), 
       plot = bigfive.spaghetti.plot,
       width = 10, height = 7, scale = 1.5)

## > combined plots ############################################################

# plot all growth curves
bigfive.combined.plot <- 
  ggplot() +
  geom_line(data = bigfive.spaghetti.data,
            aes(x = time.scaled, 
                y = predicted, 
                group = row,
                color = trait),
            size = 0.25, alpha = 0.3) +
  geom_line(data = bigfive.data,
            aes(x = time.scaled, 
                y = predicted), 
                color = 'black', size = 1) +
  geom_point(data = bigfive.data,
             aes(x = time.scaled, 
                 y = predicted), 
                 color = 'black', size = 2) +
  facet_wrap(~ trait, ncol = 3) +
  labs(title = 'Big Five Personality Trait Growth Trajectories',
       subtitle = 'T1–T8',
       x = 'Time (Years)',
       y = 'Level') +
  scale_x_continuous(limits = c(0, 18), breaks = seq(0, 18, 2)) +
  scale_y_continuous(limits = c(1, 4)) +
  scale_color_manual(values = c('Openness to Experience' = '#ffb900',
                                'Conscientiousness' = '#006cc4',
                                'Extraversion' = '#00ae3a',
                                'Agreeableness' = '#fc0909',
                                'Neuroticism' = '#6c1d9d')) +
  theme_classic() +
  guides(color = 'none')

# view plot
print(bigfive.combined.plot)

# save plot
ggsave(filename = here('Output/Primary/Analysis 2/Plots/bigfive_combined.png'), 
       plot = bigfive.combined.plot,
       width = 10, height = 7, scale = 1.5)

# clean environment
rm(list = ls(pattern = 'bigfive|open|cons|extr|agre|neur'))

## prosperity ##################################################################

## no change

# script
pros.nochng.script <- nochng.script('pros', 1:8)
cat(pros.nochng.script)

# fit model
pros.nochng <- sem(pros.nochng.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(pros.nochng, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## linear

# script
pros.linear.script <- linear.script('pros', 1:8)
cat(pros.linear.script)

# fit model
pros.linear <- sem(pros.linear.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(pros.linear, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## quadratic

# script
pros.quadra.script <- quadra.script('pros', 1:8)
cat(pros.quadra.script)

# fit model
pros.quadra <- sem(pros.quadra.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(pros.quadra, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## latent basis

# script
pros.latent.script <- latent.script('pros', 1:8)
cat(pros.latent.script)

# fit model
pros.latent <- sem(pros.latent.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(pros.latent, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.univariate.results(pros.nochng,
                          pros.linear,
                          pros.quadra,
                          pros.latent,
                          NA, 1:8)

## model comparisons

# compare model fits
summary(compareFit(pros.nochng, pros.linear))             # linear fits better
summary(compareFit(pros.linear, pros.quadra))             # quadra fits better
summary(compareFit(pros.linear, pros.latent))             # latent fits better
summary(compareFit(pros.quadra, pros.latent, nested = F)) # quadra fits best

# update summary
univariate[univariate$var == 'pros', '1:8'] <- 'quadra'

# clean environment
rm(list = setdiff(ls(pattern = '^pros'), 'pros.quadra'))

## plot

# average trajectory
pros.average.plot <- plot.average(pros.quadra, 1:8)
print(pros.average.plot)

# if looks good, save plot
ggsave(filename = here('Output/Primary/Analysis 2/Plots/pros_average.png'), 
       plot = pros.average.plot,
       width = 6, height = 4.5, scale = 1.5)

# individual trajectories
pros.spaghetti.plot <- plot.spaghetti(pros.quadra, 1:8)
print(pros.spaghetti.plot)

# if looks good, save plot
ggsave(filename = here('Output/Primary/Analysis 2/Plots/pros_spaghetti.png'), 
       plot = pros.spaghetti.plot,
       width = 6, height = 4.5, scale = 1.5)

# combined (average + spaghetti)
pros.combined.plot <- plot.combined(pros.quadra, 
                                    pros.average.data, 
                                    pros.spaghetti.data,
                                    1:8)
print(pros.combined.plot)

# if looks good, save plot
ggsave(filename = here('Output/Primary/Analysis 2/Plots/pros_combined.png'), 
       plot = pros.combined.plot,
       width = 6, height = 4.5, scale = 1.5)

# clean environment
rm(list = ls(pattern = '^pros\\.|rows|^time'))

## fame ########################################################################

## no change

# script
fame.nochng.script <- nochng.script('fame', 1:8)
cat(fame.nochng.script)

# fit model
fame.nochng <- sem(fame.nochng.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(fame.nochng, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## linear

# script
fame.linear.script <- linear.script('fame', 1:8)
cat(fame.linear.script)

# fit model
fame.linear <- sem(fame.linear.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(fame.linear, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## quadratic

# script
fame.quadra.script <- quadra.script('fame', 1:8)
cat(fame.quadra.script)

# fit model
fame.quadra <- sem(fame.quadra.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(fame.quadra, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## latent basis

# script
fame.latent.script <- latent.script('fame', 1:8)
cat(fame.latent.script)

# fit model
fame.latent <- sem(fame.latent.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(fame.latent, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.univariate.results(fame.nochng,
                          fame.linear,
                          fame.quadra,
                          fame.latent,
                          NA, 1:8)

## model comparisons

# compare model fits
summary(compareFit(fame.nochng, fame.linear))             # linear fits better
summary(compareFit(fame.linear, fame.quadra))             # quadra fits better
summary(compareFit(fame.linear, fame.latent))             # latent fits better
summary(compareFit(fame.quadra, fame.latent, nested = F)) # quadra fits best

# update summary
univariate[univariate$var == 'fame', '1:8'] <- 'quadra'

# clean environment
rm(list = setdiff(ls(pattern = '^fame'), 'fame.quadra'))

## plot

# average trajectory
fame.quadra.plot <- plot.average(fame.quadra, 1:8)
print(fame.quadra.plot)

# if looks good, save plot
ggsave(filename = here('Output/Primary/Analysis 2/Plots/fame_average.png'), 
       plot = fame.quadra.plot,
       width = 6, height = 4.5, scale = 1.5)

# individual trajectories
fame.spaghetti.plot <- plot.spaghetti(fame.quadra, 1:8)
print(fame.spaghetti.plot)

# if looks good, save plot
ggsave(filename = here('Output/Primary/Analysis 2/Plots/fame_spaghetti.png'), 
       plot = fame.spaghetti.plot,
       width = 6, height = 4.5, scale = 1.5)

# combined (average + spaghetti)
fame.combined.plot <- plot.combined(fame.quadra, 
                                    fame.average.data, 
                                    fame.spaghetti.data,
                                    1:8)
print(fame.combined.plot)

# if looks good, save plot
ggsave(filename = here('Output/Primary/Analysis 2/Plots/fame_combined.png'), 
       plot = fame.combined.plot,
       width = 6, height = 4.5, scale = 1.5)

# clean environment
rm(list = ls(pattern = '^fame\\.|rows|^time'))

## attractiveness ##############################################################

## no change

# script
attr.nochng.script <- nochng.script('attr', 1:8)
cat(attr.nochng.script)

# fit model
attr.nochng <- sem(attr.nochng.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(attr.nochng, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## linear

# script
attr.linear.script <- linear.script('attr', 1:8)
cat(attr.linear.script)

# fit model
attr.linear <- sem(attr.linear.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(attr.linear, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## quadratic

# script
attr.quadra.script <- quadra.script('attr', 1:8)
cat(attr.quadra.script)

# fit model
attr.quadra <- sem(attr.quadra.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(attr.quadra, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## latent basis

# script
attr.latent.script <- latent.script('attr', 1:8)
cat(attr.latent.script)

# fit model
attr.latent <- sem(attr.latent.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(attr.latent, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.univariate.results(attr.nochng,
                          attr.linear,
                          attr.quadra,
                          attr.latent,
                          NA, 1:8)

## model comparisons

# compare model fits
summary(compareFit(attr.nochng, attr.linear))             # linear fits better
summary(compareFit(attr.linear, attr.quadra))             # quadra fits better
summary(compareFit(attr.linear, attr.latent))             # latent fits best
summary(compareFit(attr.quadra, attr.latent, nested = F)) # latent fits better

# update summary
univariate[univariate$var == 'attr', '1:8'] <- 'latent'

# clean environment
rm(list = setdiff(ls(pattern = '^attr'), 'attr.latent'))

## plot

# average trajectory
attr.latent.plot <- plot.average(attr.latent, 1:8)
print(attr.latent.plot)

# if looks good, save plot
ggsave(filename = here('Output/Primary/Analysis 2/Plots/attr_average.png'), 
       plot = attr.latent.plot,
       width = 6, height = 4.5, scale = 1.5)

# individual trajectories
attr.spaghetti.plot <- plot.spaghetti(attr.latent, 1:8)
print(attr.spaghetti.plot)

# if looks good, save plot
ggsave(filename = here('Output/Primary/Analysis 2/Plots/attr_spaghetti.png'), 
       plot = attr.spaghetti.plot,
       width = 6, height = 4.5, scale = 1.5)

# combined (average + spaghetti)
attr.combined.plot <- plot.combined(attr.latent, 
                                    attr.average.data, 
                                    attr.spaghetti.data,
                                    1:8)
print(attr.combined.plot)

# if looks good, save plot
ggsave(filename = here('Output/Primary/Analysis 2/Plots/attr_combined.png'), 
       plot = attr.combined.plot,
       width = 6, height = 4.5, scale = 1.5)

# clean environment
rm(list = ls(pattern = '^attr\\.|rows|^time'))

## personal growth #############################################################

## no change

# script
pers.nochng.script <- nochng.script('pers', 1:4)
cat(pers.nochng.script)

# fit model
pers.nochng <- sem(pers.nochng.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary 
summary(pers.nochng, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## linear

# script
pers.linear.script <- linear.script('pers', 1:4)
cat(pers.linear.script)

# fit model
pers.linear <- sem(pers.linear.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(pers.linear, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## quadratic

# script
pers.quadra.script <- quadra.script('pers', 1:4)
cat(pers.quadra.script)

# fit model
pers.quadra <- sem(pers.quadra.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr') # non-positive definite

# inspect
summary(pers.quadra)
lavInspect(pers.quadra, 'cov.lv') # cov(pers.s, pers.q) is out of bounds

# rewrite script
cat(pers.quadra.script)
pers.quadra.script <- '# intercept loadings
                       pers.i =~ 1*pers.1
                       pers.i =~ 1*pers.2
                       pers.i =~ 1*pers.3
                       pers.i =~ 1*pers.4

                       # linear change loadings
                       pers.s  =~ 0*pers.1
                       pers.s  =~ 0.5*pers.2
                       pers.s  =~ 1*pers.3
                       pers.s  =~ 1.5*pers.4

                       # quadratic change loadings
                       pers.q  =~ 0*pers.1
                       pers.q  =~ 0.25*pers.2
                       pers.q  =~ 1*pers.3
                       pers.q  =~ 2.25*pers.4

                       # latent means
                       pers.i ~ 1
                       pers.s ~ 1
                       pers.q ~ 1

                       # latent variances
                       pers.i ~~ pers.i
                       pers.s ~~ pers.s
                       pers.q ~~ .001*pers.q # fixed to smallest positive number

                       # latent covariances
                       pers.i ~~ pers.s
                       pers.i ~~ .001*pers.q # fixed to smallest positive number
                       pers.s ~~ .001*pers.q # fixed to smallest positive number

                       # constrain means to 0
                       pers.1 ~ 0*1
                       pers.2 ~ 0*1
                       pers.3 ~ 0*1
                       pers.4 ~ 0*1

                       # constrain variances
                       pers.1 ~~ pers.theta*pers.1
                       pers.2 ~~ pers.theta*pers.2
                       pers.3 ~~ pers.theta*pers.3
                       pers.4 ~~ pers.theta*pers.4'

# re-fit model
pers.quadra <- sem(pers.quadra.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(pers.quadra, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## latent basis

# script
pers.latent.script <- latent.script('pers', 1:4)
cat(pers.latent.script)

# fit model
pers.latent <- sem(pers.latent.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(pers.latent, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.univariate.results(pers.nochng,
                          pers.linear,
                          pers.quadra,
                          pers.latent,
                          NA, 1:4)

## model comparisons

# compare model fits
summary(compareFit(pers.nochng, pers.linear))             # linear fits better
summary(compareFit(pers.linear, pers.quadra))             # quadra fits better
summary(compareFit(pers.linear, pers.latent))             # latent fits best
summary(compareFit(pers.quadra, pers.latent, nested = F)) # latent fits better

# update summary
univariate[univariate$var == 'pers', '1:4'] <- 'latent'

# clean environment
rm(list = setdiff(ls(pattern = '^pers'), 'pers.latent'))

## plot

# average trajectory
pers.latent.plot <- plot.average(pers.latent, 1:4)
print(pers.latent.plot)

# if looks good, save plot
ggsave(filename = here('Output/Primary/Analysis 2/Plots/pers_average.png'), 
       plot = pers.latent.plot,
       width = 6, height = 4.5, scale = 1.5)

# individual trajectories
pers.spaghetti.plot <- plot.spaghetti(pers.latent, 1:4)
print(pers.spaghetti.plot)

# if looks good, save plot
ggsave(filename = here('Output/Primary/Analysis 2/Plots/pers_spaghetti.png'), 
       plot = pers.spaghetti.plot,
       width = 6, height = 4.5, scale = 1.5)

# combined (average + spaghetti)
pers.combined.plot <- plot.combined(pers.latent, 
                                    pers.average.data, 
                                    pers.spaghetti.data,
                                    1:4)
print(pers.combined.plot)

# if looks good, save plot
ggsave(filename = here('Output/Primary/Analysis 2/Plots/pers_combined.png'), 
       plot = pers.combined.plot,
       width = 6, height = 4.5, scale = 1.5)

# clean environment
rm(list = ls(pattern = '^pers\\.|rows|^time'))

## relationship ###############################################################

## no change

# script
rela.nochng.script <- nochng.script('rela', 1:4)
cat(rela.nochng.script)

# fit model
rela.nochng <- sem(rela.nochng.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(rela.nochng, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## linear

# script
rela.linear.script <- linear.script('rela', 1:4)
cat(rela.linear.script)

# fit model
rela.linear <- sem(rela.linear.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(rela.linear, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## quadratic

# script
rela.quadra.script <- quadra.script('rela', 1:4)
cat(rela.quadra.script)

# fit model
rela.quadra <- sem(rela.quadra.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr') # negative variance

# inspect
summary(rela.quadra) # rela.q has negative variance
lavInspect(rela.quadra, 'cov.lv')

# rewrite script
cat(rela.quadra.script)
rela.quadra.script <- '# intercept loadings
                       rela.i =~ 1*rela.1
                       rela.i =~ 1*rela.2
                       rela.i =~ 1*rela.3
                       rela.i =~ 1*rela.4

                       # linear change loadings
                       rela.s  =~ 0*rela.1
                       rela.s  =~ 0.5*rela.2
                       rela.s  =~ 1*rela.3
                       rela.s  =~ 1.5*rela.4

                       # quadratic change loadings
                       rela.q  =~ 0*rela.1
                       rela.q  =~ 0.25*rela.2
                       rela.q  =~ 1*rela.3
                       rela.q  =~ 2.25*rela.4

                       # latent means
                       rela.i ~ 1
                       rela.s ~ 1
                       rela.q ~ 1

                       # latent variances
                       rela.i ~~ rela.i
                       rela.s ~~ rela.s
                       rela.q ~~ .001*rela.q # fixed to smallest positive number

                       # latent covariances
                       rela.i ~~ rela.s
                       rela.i ~~ .001*rela.q # fixed to smallest positive number
                       rela.s ~~ .001*rela.q # fixed to smallest positive number

                       # constrain means to 0
                       rela.1 ~ 0*1
                       rela.2 ~ 0*1
                       rela.3 ~ 0*1
                       rela.4 ~ 0*1

                       # constrain variances
                       rela.1 ~~ rela.theta*rela.1
                       rela.2 ~~ rela.theta*rela.2
                       rela.3 ~~ rela.theta*rela.3
                       rela.4 ~~ rela.theta*rela.4'

# re-fit model
rela.quadra <- sem(rela.quadra.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(rela.quadra, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## latent basis

# script
rela.latent.script <- latent.script('rela', 1:4)
cat(rela.latent.script)

# fit model
rela.latent <- sem(rela.latent.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr') # model estimation failed

# inspect
summary(rela.latent) # rela.l has negative variance

# rewrite script
cat(rela.latent.script)
rela.latent.script <- '# intercept loadings
                       rela.i =~ 1*rela.1
                       rela.i =~ 1*rela.2
                       rela.i =~ 1*rela.3
                       rela.i =~ 1*rela.4

                       # slope loadings
                       rela.l =~ 0*rela.1
                       rela.l =~ rela.2
                       rela.l =~ rela.3
                       rela.l =~ 1*rela.4

                       # latent means
                       rela.i ~ 1
                       rela.l ~ 1

                       # latent variances
                       rela.i ~~ rela.i
                       rela.l ~~ .001*rela.l # fixed to smallest positive number

                       # latent covariance
                       rela.i ~~ rela.l

                       # constrain means to 0
                       rela.1 ~ 0*1
                       rela.2 ~ 0*1
                       rela.3 ~ 0*1
                       rela.4 ~ 0*1

                       # constrain variances
                       rela.1 ~~ rela.theta*rela.1
                       rela.2 ~~ rela.theta*rela.2
                       rela.3 ~~ rela.theta*rela.3
                       rela.4 ~~ rela.theta*rela.4'

# re-fit model
rela.latent <- sem(rela.latent.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(rela.latent, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.univariate.results(rela.nochng,
                          rela.linear,
                          rela.quadra,
                          rela.latent,
                          NA, 1:4)

## model comparisons

# compare model fits
summary(compareFit(rela.nochng, rela.linear))             # linear fits better
summary(compareFit(rela.linear, rela.quadra))             # quadra fits better
summary(compareFit(rela.linear, rela.latent))             # latent fits better
summary(compareFit(rela.quadra, rela.latent, nested = F)) # latent fits best

# update summary
univariate[univariate$var == 'rela', '1:4'] <- 'latent'

# clean environment
rm(list = setdiff(ls(pattern = '^rela'), 'rela.latent'))

## plot

# average trajectory
rela.latent.plot <- plot.average(rela.latent, 1:4)
print(rela.latent.plot)

# if looks good, save plot
ggsave(filename = here('Output/Primary/Analysis 2/Plots/rela_average.png'), 
       plot = rela.latent.plot,
       width = 6, height = 4.5, scale = 1.5)

# individual trajectories
rela.spaghetti.plot <- plot.spaghetti(rela.latent, 1:4)
print(rela.spaghetti.plot)

# if looks good, save plot
ggsave(filename = here('Output/Primary/Analysis 2/Plots/rela_spaghetti.png'), 
       plot = rela.spaghetti.plot,
       width = 6, height = 4.5, scale = 1.5)

# combined (average + spaghetti)
rela.combined.plot <- plot.combined(rela.latent, 
                                    rela.average.data, 
                                    rela.spaghetti.data,
                                    1:4)
print(rela.combined.plot)

# if looks good, save plot
ggsave(filename = here('Output/Primary/Analysis 2/Plots/rela_combined.png'), 
       plot = rela.combined.plot,
       width = 6, height = 4.5, scale = 1.5)

# clean environment
rm(list = ls(pattern = '^rela\\.|rows|^time'))

## society #####################################################################

## no change

# script
soci.nochng.script <- nochng.script('soci', 1:4)
cat(soci.nochng.script)

# fit model
soci.nochng <- sem(soci.nochng.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(soci.nochng, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## linear

# script
soci.linear.script <- linear.script('soci', 1:4)
cat(soci.linear.script)

# fit model
soci.linear <- sem(soci.linear.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(soci.linear, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## quadratic

# script
soci.quadra.script <- quadra.script('soci', 1:4)
cat(soci.quadra.script)

# fit model
soci.quadra <- sem(soci.quadra.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(soci.quadra, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## latent basis

# script
soci.latent.script <- latent.script('soci', 1:4)
cat(soci.latent.script)

# fit model
soci.latent <- sem(soci.latent.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(soci.latent, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.univariate.results(soci.nochng,
                          soci.linear,
                          soci.quadra,
                          soci.latent,
                          NA, 1:4)

## model comparisons

# compare model fits
summary(compareFit(soci.nochng, soci.linear))             # linear fits better
summary(compareFit(soci.linear, soci.quadra))             # quadra fits better
summary(compareFit(soci.linear, soci.latent))             # latent fits better
summary(compareFit(soci.quadra, soci.latent, nested = F)) # latent

# update summary
univariate[univariate$var == 'soci', '1:4'] <- 'latent'

# clean environment
rm(list = setdiff(ls(pattern = '^soci'), 'soci.latent'))

## plot

# average trajectory
soci.latent.plot <- plot.average(soci.latent, 1:4)
print(soci.latent.plot)

# if looks good, save plot
ggsave(filename = here('Output/Primary/Analysis 2/Plots/soci_average.png'), 
       plot = soci.latent.plot,
       width = 6, height = 4.5, scale = 1.5)

# individual trajectories
soci.spaghetti.plot <- plot.spaghetti(soci.latent, 1:4)
print(soci.spaghetti.plot)

# if looks good, save plot
ggsave(filename = here('Output/Primary/Analysis 2/Plots/soci_spaghetti.png'), 
       plot = soci.spaghetti.plot,
       width = 6, height = 4.5, scale = 1.5)

# combined (average + spaghetti)
soci.combined.plot <- plot.combined(soci.latent, 
                                    soci.average.data, 
                                    soci.spaghetti.data,
                                    1:4)
print(soci.combined.plot)

# if looks good, save plot
ggsave(filename = here('Output/Primary/Analysis 2/Plots/soci_combined.png'), 
       plot = soci.combined.plot,
       width = 6, height = 4.5, scale = 1.5)

# clean environment
rm(list = ls(pattern = '^soci\\.|rows|^time'))

## physical health #############################################################

## no change

# script
phys.nochng.script <- nochng.script('phys', 1:4)
cat(phys.nochng.script)

# fit model
phys.nochng <- sem(phys.nochng.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(phys.nochng, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## linear

# script
phys.linear.script <- linear.script('phys', 1:4)
cat(phys.linear.script)

# fit model
phys.linear <- sem(phys.linear.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(phys.linear, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## quadratic

# script
phys.quadra.script <- quadra.script('phys', 1:4)
cat(phys.quadra.script)

# fit model
phys.quadra <- sem(phys.quadra.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(phys.quadra, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## latent basis

# script
phys.latent.script <- latent.script('phys', 1:4)
cat(phys.latent.script)

# fit model
phys.latent <- sem(phys.latent.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(phys.latent, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.univariate.results(phys.nochng,
                          phys.linear,
                          phys.quadra,
                          phys.latent,
                          NA, 1:4)

## model comparisons

# compare model fits
summary(compareFit(phys.nochng, phys.linear))             # linear fits better
summary(compareFit(phys.linear, phys.quadra))             # quadra fits better
summary(compareFit(phys.linear, phys.latent))             # latent fits better
summary(compareFit(phys.quadra, phys.latent, nested = F)) # quadra fits best

# update summary
univariate[univariate$var == 'phys', '1:4'] <- 'quadra'

# clean environment
rm(list = setdiff(ls(pattern = '^phys'), 'phys.quadra'))

## plot

# average trajectory
phys.quadra.plot <- plot.average(phys.quadra, 1:4)
print(phys.quadra.plot)

# if looks good, save plot
ggsave(filename = here('Output/Primary/Analysis 2/Plots/phys_average.png'), 
       plot = phys.quadra.plot,
       width = 6, height = 4.5, scale = 1.5)

# individual trajectories
phys.spaghetti.plot <- plot.spaghetti(phys.quadra, 1:4)
print(phys.spaghetti.plot)

# if looks good, save plot
ggsave(filename = here('Output/Primary/Analysis 2/Plots/phys_spaghetti.png'), 
       plot = phys.spaghetti.plot,
       width = 6, height = 4.5, scale = 1.5)

# combined (average + spaghetti)
phys.combined.plot <- plot.combined(phys.quadra, 
                                    phys.average.data, 
                                    phys.spaghetti.data,
                                    1:4)
print(phys.combined.plot)

# if looks good, save plot
ggsave(filename = here('Output/Primary/Analysis 2/Plots/phys_combined.png'), 
       plot = phys.combined.plot,
       width = 6, height = 4.5, scale = 1.5)

# clean environment
rm(list = ls(pattern = '^phys\\.|rows|^time'))
 
## hedonism ####################################################################

## no change

# script
hedo.nochng.script <- nochng.script('hedo', 1:4)
cat(hedo.nochng.script)

# fit model
hedo.nochng <- sem(hedo.nochng.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(hedo.nochng, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## linear

# script
hedo.linear.script <- linear.script('hedo', 1:4)
cat(hedo.linear.script)

# fit model
hedo.linear <- sem(hedo.linear.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(hedo.linear, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## quadratic

# script
hedo.quadra.script <- quadra.script('hedo', 1:4)
cat(hedo.quadra.script)

# fit model
hedo.quadra <- sem(hedo.quadra.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(hedo.quadra, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## latent basis

# script
hedo.latent.script <- latent.script('hedo', 1:4)
cat(hedo.latent.script)

# fit model
hedo.latent <- sem(hedo.latent.script, 
                   data = data, 
                   missing = 'fiml',
                   estimator = 'mlr')

# model summary
summary(hedo.latent, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.univariate.results(hedo.nochng,
                          hedo.linear,
                          hedo.quadra,
                          hedo.latent,
                          NA, 1:4)

## model comparisons

# compare model fits
summary(compareFit(hedo.nochng, hedo.linear))             # linear fits better
summary(compareFit(hedo.linear, hedo.quadra))             # quadra fits better
summary(compareFit(hedo.linear, hedo.latent))             # latent fits better
summary(compareFit(hedo.quadra, hedo.latent, nested = F)) # quadra fits best

# quadratic fits best for hedonism.

# update summary
univariate[univariate$var == 'hedo', '1:4'] <- 'quadra'

# clean environment
rm(list = setdiff(ls(pattern = '^hedo'), 'hedo.quadra'))

## plot

# average trajectory
hedo.quadra.plot <- plot.average(hedo.quadra, 1:4)
print(hedo.quadra.plot)

# if looks good, save plot
ggsave(filename = here('Output/Primary/Analysis 2/Plots/hedo_average.png'), 
       plot = hedo.quadra.plot,
       width = 6, height = 4.5, scale = 1.5)

# individual trajectories
hedo.spaghetti.plot <- plot.spaghetti(hedo.quadra, 1:4)
print(hedo.spaghetti.plot)

# if looks good, save plot
ggsave(filename = here('Output/Primary/Analysis 2/Plots/hedo_spaghetti.png'), 
       plot = hedo.spaghetti.plot,
       width = 6, height = 4.5, scale = 1.5)

# combined (average + spaghetti)
hedo.combined.plot <- plot.combined(hedo.quadra, 
                                    hedo.average.data, 
                                    hedo.spaghetti.data,
                                    1:4)
print(hedo.combined.plot)

# if looks good, save plot
ggsave(filename = here('Output/Primary/Analysis 2/Plots/hedo_combined.png'), 
       plot = hedo.combined.plot,
       width = 6, height = 4.5, scale = 1.5)

# clean environment
rm(list = ls(pattern = '^hedo\\.|rows|^time'))

## > average plots #############################################################

## re-create plot data

# scripts
pros.quadra.script <- quadra.script('pros', 1:8)
fame.quadra.script <- quadra.script('fame', 1:8)
attr.latent.script <- latent.script('attr', 1:8)
pers.latent.script <- latent.script('pers', 1:4)
rela.latent.script <- latent.script('rela', 1:4)
soci.latent.script <- latent.script('soci', 1:4)
phys.quadra.script <- quadra.script('phys', 1:4)
hedo.quadra.script <- quadra.script('hedo', 1:4)

# fit models
pros.quadra <- sem(pros.quadra.script, data = data, missing = 'fiml', estimator = 'mlr')
fame.quadra <- sem(fame.quadra.script, data = data, missing = 'fiml', estimator = 'mlr')
attr.latent <- sem(attr.latent.script, data = data, missing = 'fiml', estimator = 'mlr')
pers.latent <- sem(pers.latent.script, data = data, missing = 'fiml', estimator = 'mlr')
rela.latent <- sem(rela.latent.script, data = data, missing = 'fiml', estimator = 'mlr')
soci.latent <- sem(soci.latent.script, data = data, missing = 'fiml', estimator = 'mlr')
phys.quadra <- sem(phys.quadra.script, data = data, missing = 'fiml', estimator = 'mlr')
hedo.quadra <- sem(hedo.quadra.script, data = data, missing = 'fiml', estimator = 'mlr')

# create plot data
pros.average.plot <- plot.average(pros.quadra, 1:8)
fame.average.plot <- plot.average(fame.quadra, 1:8)
attr.average.plot <- plot.average(attr.latent, 1:8)
pers.average.plot <- plot.average(pers.latent, 1:4)
rela.average.plot <- plot.average(rela.latent, 1:4)
soci.average.plot <- plot.average(soci.latent, 1:4)
phys.average.plot <- plot.average(phys.quadra, 1:4)
hedo.average.plot <- plot.average(hedo.quadra, 1:4)

# combine plot data (with labels)
goals.data <- bind_rows(pros.average.data %>% mutate(goal = 'Prosperity'),
                        fame.average.data %>% mutate(goal = 'Fame'),
                        attr.average.data %>% mutate(goal = 'Attractiveness'),
                        pers.average.data %>% mutate(goal = 'Personal Growth'),
                        rela.average.data %>% mutate(goal = 'Relationships'),
                        soci.average.data %>% mutate(goal = 'Society'),
                        phys.average.data %>% mutate(goal = 'Physical Health'),
                        hedo.average.data %>% mutate(goal = 'Hedonism'))

# re-scale for plot
goals.data$time.scaled <- goals.data$time * 4

# plot all growth curves
goals.plot <-
  ggplot(goals.data, aes(x = time.scaled,
                         y = predicted,
                         color = goal,
                         group = goal)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = 'Major Life Goals Growth Trajectories',
       x = 'Time (Years)',
       y = 'Level',
       color = 'Major Life Goals') +
  scale_x_continuous(limits = c(0, 18),
                     breaks = seq(0, 18, 2)) +
  scale_y_continuous(limits = c(1, 4)) +
  scale_color_manual(values = c('Prosperity' = '#00af37',
                                'Fame' = '#74209e',
                                'Attractiveness' = '#ff6ec7',
                                'Personal Growth' = '#fcfe00',
                                'Relationships' = '#7e7e7e',
                                'Society' = '#fc7400',
                                'Physical Health' = '#006cc4',
                                'Hedonism' = '#ff0000')) +
  theme_classic()

# view plot
print(goals.plot)

# save plot
ggsave(filename = here('Output/Primary/Analysis 2/Plots/goals_average.png'), 
       plot = goals.plot,
       width = 6.5, height = 4.5, scale = 1.5)

## > spaghetti plots ###########################################################

# create plot data
pros.spaghetti.plot <- plot.spaghetti(pros.quadra, 1:8)
fame.spaghetti.plot <- plot.spaghetti(fame.quadra, 1:8)
attr.spaghetti.plot <- plot.spaghetti(attr.latent, 1:8)
pers.spaghetti.plot <- plot.spaghetti(pers.latent, 1:4)
rela.spaghetti.plot <- plot.spaghetti(rela.latent, 1:4)
soci.spaghetti.plot <- plot.spaghetti(soci.latent, 1:4)
phys.spaghetti.plot <- plot.spaghetti(phys.quadra, 1:4)
hedo.spaghetti.plot <- plot.spaghetti(hedo.quadra, 1:4)

# combine plot data (with labels)
goals.spaghetti.data <- bind_rows(pros.spaghetti.data %>% mutate(goal = 'Prosperity'),
                                  fame.spaghetti.data %>% mutate(goal = 'Fame'),
                                  attr.spaghetti.data %>% mutate(goal = 'Attractiveness'),
                                  pers.spaghetti.data %>% mutate(goal = 'Personal Growth'),
                                  rela.spaghetti.data %>% mutate(goal = 'Relationships'),
                                  soci.spaghetti.data %>% mutate(goal = 'Society'),
                                  phys.spaghetti.data %>% mutate(goal = 'Physical Health'),
                                  hedo.spaghetti.data %>% mutate(goal = 'Hedonism'))

# re-scale for plot
goals.spaghetti.data$time.scaled <- goals.spaghetti.data$time * 4

# plot all growth curves
goals.spaghetti.plot <- 
  ggplot(goals.spaghetti.data, aes(x = time.scaled,
                              y = predicted,
                              color = goal,
                              group = row)) +
  geom_line(size = 0.2, alpha = 0.2) +
  labs(title = 'Major Life Goals Growth Trajectories',
       x = 'Time (Years)',
       y = 'Level') +
  scale_x_continuous(limits = c(0, 18),
                     breaks = seq(0, 18, 2)) +
  scale_y_continuous(limits = c(1, 4)) +
  scale_color_manual(values = c('Prosperity' = '#00af37',
                                'Fame' = '#74209e',
                                'Attractiveness' = '#ff6ec7',
                                'Personal Growth' = '#fcfe00',
                                'Relationships' = '#7e7e7e',
                                'Society' = '#fc7400',
                                'Physical Health' = '#006cc4',
                                'Hedonism' = '#ff0000')) +
  facet_wrap(~ goal, ncol = 4) +
  theme_classic() +
  guides(color = 'none')

# view plot
print(goals.spaghetti.plot)

# save plot
ggsave(filename = here('Output/Primary/Analysis 2/Plots/goals_spaghetti.png'), 
       plot = goals.spaghetti.plot,
       width = 10, height = 7, scale = 1.5)

## > combined plots ############################################################

# define order for goals
goal.order <- c('Prosperity', 'Fame', 'Attractiveness',
                'Personal Growth', 'Relationships', 'Society', 
                'Physical Health', 'Hedonism')

# set factor order in both data sets
goals.spaghetti.data$goal <- factor(goals.spaghetti.data$goal, 
                                    levels = goal.order)
goals.data$goal <- factor(goals.data$goal, 
                          levels = goal.order)

# plot all growth curves
goals.combined.plot <- 
  ggplot() +
  geom_line(data = goals.spaghetti.data,
            aes(x = time.scaled, 
                y = predicted, 
                group = row,
                color = goal),
            size = 0.25, alpha = 0.3) +
  geom_line(data = goals.data,
            aes(x = time.scaled, 
                y = predicted), 
            color = 'black', size = 1) +
  geom_point(data = goals.data,
             aes(x = time.scaled, 
                 y = predicted), 
             color = 'black', size = 2) +
  facet_wrap(~ goal, ncol = 3) +
  labs(title = 'Major Life Goals Growth Trajectories',
       x = 'Time (Years)',
       y = 'Level') +
  scale_x_continuous(limits = c(0, 18), breaks = seq(0, 18, 2)) +
  scale_y_continuous(limits = c(1, 4)) +
  scale_color_manual(values = c('Prosperity' = '#00af37',
                                'Fame' = '#74209e',
                                'Attractiveness' = '#ff6ec7',
                                'Personal Growth' = '#fcfe00',
                                'Relationships' = '#7e7e7e',
                                'Society' = '#fc7400',
                                'Physical Health' = '#006cc4',
                                'Hedonism' = '#ff0000'),
                     limits = goal.order) +
  theme_classic() +
  guides(color = 'none')

# view plot
print(goals.combined.plot)

# save plot
ggsave(filename = here('Output/Primary/Analysis 2/Plots/goals_combined.png'), 
       plot = goals.combined.plot,
       width = 10, height = 7, scale = 1.5)

# clean environment
rm(list = ls(pattern = 'goals|pros|fame|attr|pers|rela|soci|phys|hedo'))

# analysis 3. co-development ###################################################

## rename libe.1 as libe
data <- data %>% 
  rename('libe' = 'libe.1')

## create composites across T3-4 for:
## security, conformism, religiosity

names(data)

# create composite
data <- data %>% 
  sjmisc::row_means(starts_with('secu'), n = 1, var = 'secu') %>% 
  sjmisc::row_means(starts_with('conf'), n = 1, var = 'conf') %>%
  sjmisc::row_means(starts_with('reli'), n = 1, var = 'reli')

# check
data %>% 
  select(starts_with('secu'), 
         starts_with('conf'), 
         starts_with('reli')) %>% 
  summary()

## extraversion ################################################################

## prosperity

## linear

# script
extr.pros.script <- bivariate.linear.script('extr', 'pros', 1:8)
cat(extr.pros.script)

# fit model
extr.pros <- sem(extr.pros.script, 
                 data = data, 
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(extr.pros, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.bivariate.linear.results(extr.pros, 1:8)

## best-fitting

# script
extr.pros.script <- bivariate.bestfit.script('extr', 'pros', 1:8)
cat(extr.pros.script)

# fit model
extr.pros <- sem(extr.pros.script, 
                 data = data, 
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(extr.pros, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.bivariate.bestfit.results(extr.pros, 1:8)

## fame

# script
extr.fame.script <- bivariate.linear.script('extr', 'fame', 1:8)
cat(extr.fame.script)

# fit model
extr.fame <- sem(extr.fame.script, 
                 data = data, 
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(extr.fame, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.bivariate.linear.results(extr.fame, 1:8)

## attractiveness

# script
extr.attr.script <- bivariate.linear.script('extr', 'attr', 1:8)
cat(extr.attr.script)

# fit model
extr.attr <- sem(extr.attr.script, 
                 data = data, 
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(extr.attr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.bivariate.linear.results(extr.attr, 1:8)

## plot

# extr.s ~~ attr.i
extr.attr.si.plot <- plot.slope.level(extr.attr,
                                      NA,
                                      1:8,
                                      'Extraversion', '.s',
                                      'Attractiveness')

# view plot
print(extr.attr.si.plot)

# if looks good, save plot
ggsave(filename = here('Output/Primary/Analysis 3/Plots/extr_attr_si.png'), 
       plot = extr.attr.si.plot,
       width = 6, height = 4.5, scale = 1.5)

## personal growth

# script
extr.pers.script <- bivariate.linear.script('extr', 'pers', 1:4)
cat(extr.pers.script)

# fit model
extr.pers <- sem(extr.pers.script, 
                 data = data, 
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(extr.pers, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.bivariate.linear.results(extr.pers, 1:4)

## relationship

# script
extr.rela.script <- bivariate.linear.script('extr', 'rela', 1:4)
cat(extr.rela.script)

# fit model
extr.rela <- sem(extr.rela.script, 
                 data = data, 
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(extr.rela, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.bivariate.linear.results(extr.rela, 1:4)

## society

# script
extr.soci.script <- bivariate.linear.script('extr', 'soci', 1:4)
cat(extr.soci.script)

# fit model
extr.soci <- sem(extr.soci.script, 
                 data = data, 
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(extr.soci, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.bivariate.linear.results(extr.soci, 1:4)

## physical health

# script
extr.phys.script <- bivariate.linear.script('extr', 'phys', 1:4)
cat(extr.phys.script)

# fit model
extr.phys <- sem(extr.phys.script, 
                 data = data, 
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(extr.phys, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.bivariate.linear.results(extr.phys, 1:4)

## hedonism

# script
extr.hedo.script <- bivariate.linear.script('extr', 'hedo', 1:4)
cat(extr.hedo.script)

# fit model
extr.hedo <- sem(extr.hedo.script, 
                 data = data, 
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(extr.hedo, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.bivariate.linear.results(extr.hedo, 1:4)

## plot

# extr.s ~~ hedo.i
extr.hedo.si.plot <- plot.slope.level(extr.hedo,
                                      NA,
                                      1:4,
                                      'Extraversion', '.s',
                                      'Hedonism')

# view plot
print(extr.hedo.si.plot)

# if looks good, save plot
ggsave(filename = here('Output/Primary/Analysis 3/Plots/extr_hedo_si.png'), 
       plot = extr.hedo.si.plot,
       width = 6, height = 4.5, scale = 1.5)

## liberal values

## extraversion T1 predicting liberal values T1

## linear

# script
extr.libe.script <- trait.predict.linear.script('extr', 'libe', 1:8)
cat(extr.libe.script)

# fit model
extr.libe <- sem(extr.libe.script,
                 data = data,
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(extr.libe, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.linear.results(extr.libe, 'trait', 1:8)

## best-fitting

# script
extr.libe.script <- trait.predict.bestfit.script('extr', 'libe', 1:8)
cat(extr.libe.script)

# fit model
extr.libe <- sem(extr.libe.script,
                 data = data,
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(extr.libe, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.bestfit.results(extr.libe, 'trait', 1:8)

## liberal values T1 predicting extraversion T1-8

## linear

# script
libe.extr.script <- goal.predict.linear.script('libe', 'extr', 1:8)
cat(libe.extr.script)

# fit model
libe.extr <- sem(libe.extr.script,
                 data = data,
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(libe.extr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.linear.results(libe.extr, 'goal', 1:8)

## best-fitting

# script
libe.extr.script <- goal.predict.bestfit.script('libe', 'extr', 1:8)
cat(libe.extr.script)

# fit model
libe.extr <- sem(libe.extr.script,
                 data = data,
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(libe.extr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.bestfit.results(libe.extr, 'goal', 1:8)

## security

## extraversion T1-4 predicting security T3/T4

## linear 

# script
extr.secu.script <- trait.predict.linear.script('extr', 'secu', 1:4)
cat(extr.secu.script)

# fit model
extr.secu <- sem(extr.secu.script,
                 data = data,
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(extr.secu, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.linear.results(extr.secu, 'trait', 1:4)

## best-fitting 

# script
extr.secu.script <- trait.predict.bestfit.script('extr', 'secu', 1:4)
cat(extr.secu.script)

# fit model
extr.secu <- sem(extr.secu.script,
                 data = data,
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(extr.secu, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.bestfit.results(extr.secu, 'trait', 1:4)

## security T3/T4 predicting extraversion T4-8

## linear

# script
secu.extr.script <- goal.predict.linear.script('secu', 'extr', 4:8)
cat(secu.extr.script)

# fit model
secu.extr <- sem(secu.extr.script,
                 data = data,
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(secu.extr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.linear.results(secu.extr, 'goal', 4:8)

## best-fitting

# script
secu.extr.script <- goal.predict.bestfit.script('secu', 'extr', 4:8)
cat(secu.extr.script)

# fit model
secu.extr <- sem(secu.extr.script,
                 data = data,
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(secu.extr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.bestfit.results(secu.extr, 'goal', 4:8)

## conformism

## extraversion T1-4 predicting conformism T3/T4

## linear

# script
extr.conf.script <- trait.predict.linear.script('extr', 'conf', 1:4)
cat(extr.conf.script)

# fit model
extr.conf <- sem(extr.conf.script,
                 data = data,
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(extr.conf, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.linear.results(extr.conf, 'trait', 1:4)

## plot

# conf ~ extr.s
extr.conf.s.plot <- plot.goal.predicted.slope(extr.conf,
                                              NA,
                                              1:4,
                                              'Conformism',
                                              'Extraversion', '.s')

# view plot
print(extr.conf.s.plot)

# if looks good, save plot
ggsave(filename = here('Output/Primary/Analysis 3/Plots/extr_conf_s.png'), 
       plot = extr.conf.s.plot,
       width = 6, height = 4.5, scale = 1.5)

## best-fitting

# script
extr.conf.script <- trait.predict.bestfit.script('extr', 'conf', 1:4)
cat(extr.conf.script)

# fit model
extr.conf <- sem(extr.conf.script,
                 data = data,
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(extr.conf, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.bestfit.results(extr.conf, 'trait', 1:4)

## conformism T3/T4 predicting extraversion T4-8

## linear

# script
conf.extr.script <- goal.predict.linear.script('conf', 'extr', 4:8)
cat(conf.extr.script)

# fit model
conf.extr <- sem(conf.extr.script,
                 data = data,
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(conf.extr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.linear.results(conf.extr, 'goal', 4:8)

## plot

# extr.s ~ conf
conf.extr.s.plot <- plot.goal.predicting.slope(conf.extr,
                                               NA,
                                               4:8,
                                               'Conformism',
                                               'Extraversion', '.s')

# view plot
print(conf.extr.s.plot)

# if looks good, save plot
ggsave(filename = here('Output/Primary/Analysis 3/Plots/conf_extr_s.png'), 
       plot = conf.extr.s.plot,
       width = 6, height = 4.5, scale = 1.5)

## best-fitting

# script
conf.extr.script <- goal.predict.bestfit.script('conf', 'extr', 4:8)
cat(conf.extr.script)

# fit model
conf.extr <- sem(conf.extr.script,
                 data = data,
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(conf.extr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.bestfit.results(conf.extr, 'goal', 4:8)

## religiosity

## extraversion T1-4 predicting religiosity T3/T4

## linear

# script
extr.reli.script <- trait.predict.linear.script('extr', 'reli', 1:4)
cat(extr.reli.script)

# fit model
extr.reli <- sem(extr.reli.script,
                 data = data,
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(extr.reli, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.linear.results(extr.reli, 'trait', 1:4)

## best-fitting

# script
extr.reli.script <- trait.predict.bestfit.script('extr', 'reli', 1:4)
cat(extr.reli.script)

# fit model
extr.reli <- sem(extr.reli.script,
                 data = data,
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(extr.reli, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.bestfit.results(extr.reli, 'trait', 1:4)

## religiosity T3/T4 predicting extraversion T4-8

## linear

# script
reli.extr.script <- goal.predict.linear.script('reli', 'extr', 4:8)
cat(reli.extr.script)

# fit model
reli.extr <- sem(reli.extr.script,
                 data = data,
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(reli.extr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.linear.results(reli.extr, 'goal', 4:8)

## plot

# extr.s ~ reli
reli.extr.s.plot <- plot.goal.predicting.slope(reli.extr,
                                               NA,
                                               4:8,
                                               'Religiosity',
                                               'Extraversion', '.s')

# view plot
print(reli.extr.s.plot)

# if looks good, save plot
ggsave(filename = here('Output/Primary/Analysis 3/Plots/reli_extr_s.png'), 
       plot = reli.extr.s.plot,
       width = 6, height = 4.5, scale = 1.5)

## best-fitting

# script
reli.extr.script <- goal.predict.bestfit.script('reli', 'extr', 4:8)
cat(reli.extr.script)

# fit model
reli.extr <- sem(reli.extr.script,
                 data = data,
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(reli.extr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.bestfit.results(reli.extr, 'goal', 4:8)

# clean environment
rm(list = ls(pattern = 'extr'))

## agreeableness ###############################################################

## prosperity

# script
agre.pros.script <- bivariate.linear.script('agre', 'pros', 1:8)
cat(agre.pros.script)

# fit model
agre.pros <- sem(agre.pros.script, 
                 data = data, 
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(agre.pros, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.bivariate.linear.results(agre.pros, 1:8)

## plot

# agre.i ~~ pros.s
agre.pros.is.plot <- plot.level.slope(agre.pros,
                                      NA,
                                      1:8,
                                      'Agreeableness',
                                      'Prosperity', '.s')

# view plot
print(agre.pros.is.plot)

# if looks good, save plot
ggsave(filename = here('Output/Primary/Analysis 3/Plots/agre_pros_is.png'), 
       plot = agre.pros.is.plot,
       width = 6, height = 4.5, scale = 1.5)

## fame

# script
agre.fame.script <- bivariate.linear.script('agre', 'fame', 1:8)
cat(agre.fame.script)

# fit model
agre.fame <- sem(agre.fame.script, 
                 data = data, 
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(agre.fame, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.bivariate.linear.results(agre.fame, 1:8)

## attractiveness

# script
agre.attr.script <- bivariate.linear.script('agre', 'attr', 1:8)
cat(agre.attr.script)

# fit model
agre.attr <- sem(agre.attr.script, 
                 data = data, 
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(agre.attr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.bivariate.linear.results(agre.attr, 1:8)

## plot

# agre.s ~~ attr.i
agre.attr.si.plot <- plot.slope.level(agre.attr,
                                      NA,
                                      1:8,
                                      'Agreeableness', '.s',
                                      'Attractiveness')

# view plot
print(agre.attr.si.plot)

# if looks good, save plot
ggsave(filename = here('Output/Primary/Analysis 3/Plots/agre_attr_si.png'), 
       plot = agre.attr.si.plot,
       width = 6, height = 4.5, scale = 1.5)

## personal growth

# script
agre.pers.script <- bivariate.linear.script('agre', 'pers', 1:4)
cat(agre.pers.script)

# fit model
agre.pers <- sem(agre.pers.script, 
                 data = data, 
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(agre.pers, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.bivariate.linear.results(agre.pers, 1:4)

## plot

# agre.i ~~ pers.s
agre.pers.is.plot <- plot.level.slope(agre.pers,
                                      NA,
                                      1:4,
                                      'Agreeableness',
                                      'Personal Growth', '.s')

# view plot
print(agre.pers.is.plot)

# if looks good, save plot
ggsave(filename = here('Output/Primary/Analysis 3/Plots/agre_pers_is.png'), 
       plot = agre.pers.is.plot,
       width = 6, height = 4.5, scale = 1.5)

# agre.s ~~ pers.i
agre.pers.si.plot <- plot.slope.level(agre.pers,
                                      NA,
                                      1:4,
                                      'Agreeableness', '.s',
                                      'Personal Growth')

# view plot
print(agre.pers.si.plot)

# if looks good, save plot
ggsave(filename = here('Output/Primary/Analysis 3/Plots/agre_pers_si.png'), 
       plot = agre.pers.si.plot,
       width = 6, height = 4.5, scale = 1.5)

## relationships

# script
agre.rela.script <- bivariate.linear.script('agre', 'rela', 1:4)
cat(agre.rela.script)

# fit model
agre.rela <- sem(agre.rela.script, 
                 data = data, 
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(agre.rela, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.bivariate.linear.results(agre.rela, 1:4)

## society

# script
agre.soci.script <- bivariate.linear.script('agre', 'soci', 1:4)
cat(agre.soci.script)

# fit model
agre.soci <- sem(agre.soci.script, 
                 data = data, 
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(agre.soci, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.bivariate.linear.results(agre.soci, 1:4)

## physical health

# script
agre.phys.script <- bivariate.linear.script('agre', 'phys', 1:4)
cat(agre.phys.script)

# fit model
agre.phys <- sem(agre.phys.script, 
                 data = data, 
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(agre.phys, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.bivariate.linear.results(agre.phys, 1:4)

## hedonism

# script
agre.hedo.script <- bivariate.linear.script('agre', 'hedo', 1:4)
cat(agre.hedo.script)

# fit model
agre.hedo <- sem(agre.hedo.script, 
                 data = data, 
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(agre.hedo, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.bivariate.linear.results(agre.hedo, 1:4)

## liberal values

## agreeableness T1 predicting liberal values T1

## linear

# script
agre.libe.script <- trait.predict.linear.script('agre', 'libe', 1:8)
cat(agre.libe.script)

# fit model
agre.libe <- sem(agre.libe.script,
                 data = data,
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(agre.libe, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.linear.results(agre.libe, 'trait', 1:8)

## best-fitting

# script
agre.libe.script <- trait.predict.bestfit.script('agre', 'libe', 1:8)
cat(agre.libe.script)

# fit model
agre.libe <- sem(agre.libe.script,
                 data = data,
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(agre.libe, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.bestfit.results(agre.libe, 'trait', 1:8)

## liberal values T1 predicting agreeableness T1-8

## linear

# script
libe.agre.script <- goal.predict.linear.script('libe', 'agre', 1:8)
cat(libe.agre.script)

# fit model
libe.agre <- sem(libe.agre.script,
                 data = data,
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(libe.agre, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.linear.results(libe.agre, 'goal', 1:8)

## plot

# agre.s ~ libe
libe.agre.s.plot <- plot.goal.predicting.slope(libe.agre,
                                               NA,
                                               1:8,
                                               'Liberal Values',
                                               'Agreeableness', '.s')

# view plot
print(libe.agre.s.plot)

# if looks good, save plot
ggsave(filename = here('Output/Primary/Analysis 3/Plots/libe_agre_s.png'), 
       plot = libe.agre.s.plot,
       width = 6, height = 4.5, scale = 1.5)

## best-fitting

# script
libe.agre.script <- goal.predict.bestfit.script('libe', 'agre', 1:8)
cat(libe.agre.script)

# fit model
libe.agre <- sem(libe.agre.script,
                 data = data,
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(libe.agre, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.bestfit.results(libe.agre, 'goal', 1:8)

## security

## agreeableness T1-4 predicting security T3/T4

## linear

# script
agre.secu.script <- trait.predict.linear.script('agre', 'secu', 1:4)
cat(agre.secu.script)

# fit model
agre.secu <- sem(agre.secu.script,
                 data = data,
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(agre.secu, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.linear.results(agre.secu, 'trait', 1:4)

## best-fitting

# script
agre.secu.script <- trait.predict.bestfit.script('agre', 'secu', 1:4)
cat(agre.secu.script)

# fit model
agre.secu <- sem(agre.secu.script,
                 data = data,
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(agre.secu, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.bestfit.results(agre.secu, 'trait', 1:4)

## security T3/T4 predicting agreeableness T4-8

## linear

# script
secu.agre.script <- goal.predict.linear.script('secu', 'agre', 4:8)
cat(secu.agre.script)

# fit model
secu.agre <- sem(secu.agre.script,
                 data = data,
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(secu.agre, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.linear.results(secu.agre, 'goal', 4:8)

## best-fitting

# script
secu.agre.script <- goal.predict.bestfit.script('secu', 'agre', 4:8)
cat(secu.agre.script)

# fit model
secu.agre <- sem(secu.agre.script,
                 data = data,
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(secu.agre, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.bestfit.results(secu.agre, 'goal', 4:8)

## conformism

## agreeableness T1-4 predicting conformism T3/T4

## linear

# script
agre.conf.script <- trait.predict.linear.script('agre', 'conf', 1:4)
cat(agre.conf.script)

# fit model
agre.conf <- sem(agre.conf.script,
                 data = data,
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(agre.conf, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.linear.results(agre.conf, 'trait', 1:4)

## best-fitting

# script
agre.conf.script <- trait.predict.bestfit.script('agre', 'conf', 1:4)
cat(agre.conf.script)

# fit model
agre.conf <- sem(agre.conf.script,
                 data = data,
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(agre.conf, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.bestfit.results(agre.conf, 'trait', 1:4)

## conformism T3/T4 predicting agreeableness T4-8

## linear

# script
conf.agre.script <- goal.predict.linear.script('conf', 'agre', 4:8)
cat(conf.agre.script)

# fit model
conf.agre <- sem(conf.agre.script,
                 data = data,
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(conf.agre, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.linear.results(conf.agre, 'goal', 4:8)

## best-fitting

# script
conf.agre.script <- goal.predict.bestfit.script('conf', 'agre', 4:8)
cat(conf.agre.script)

# fit model
conf.agre <- sem(conf.agre.script,
                 data = data,
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(conf.agre, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.bestfit.results(conf.agre, 'goal', 4:8)

## religiosity

## agreeableness T1-4 predicting religiosity T3/T4

## linear

# script
agre.reli.script <- trait.predict.linear.script('agre', 'reli', 1:4)
cat(agre.reli.script)

# fit model
agre.reli <- sem(agre.reli.script,
                 data = data,
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(agre.reli, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.linear.results(agre.reli, 'trait', 1:4)

## best-fitting

# script
agre.reli.script <- trait.predict.bestfit.script('agre', 'reli', 1:4)
cat(agre.reli.script)

# fit model
agre.reli <- sem(agre.reli.script,
                 data = data,
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(agre.reli, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.bestfit.results(agre.reli, 'trait', 1:4)

## religiosity T3/T4 predicting agreeableness T4-8

## linear

# script
reli.agre.script <- goal.predict.linear.script('reli', 'agre', 4:8)
cat(reli.agre.script)

# fit model
reli.agre <- sem(reli.agre.script,
                 data = data,
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(reli.agre, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.linear.results(reli.agre, 'goal', 4:8)

## plot

# agre.s ~ reli
reli.agre.s.plot <- plot.goal.predicting.slope(reli.agre,
                                               NA,
                                               4:8,
                                               'Religiosity',
                                               'Agreeableness', '.s')

# view plot
print(reli.agre.s.plot)

# if looks good, save plot
ggsave(filename = here('Output/Primary/Analysis 3/Plots/reli_agre_s.png'), 
       plot = reli.agre.s.plot,
       width = 6, height = 4.5, scale = 1.5)

## best-fitting

# script
reli.agre.script <- goal.predict.bestfit.script('reli', 'agre', 4:8)
cat(reli.agre.script)

# fit model
reli.agre <- sem(reli.agre.script,
                 data = data,
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(reli.agre, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.bestfit.results(reli.agre, 'goal', 4:8)

# clean environment
rm(list = ls(pattern = 'agre'))

## conscientiousness ###########################################################

## prosperity

# script
cons.pros.script <- bivariate.linear.script('cons', 'pros', 1:8)
cat(cons.pros.script)

# fit model
cons.pros <- sem(cons.pros.script, 
                 data = data, 
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(cons.pros, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.bivariate.linear.results(cons.pros, 1:8)

## plot

# cons.i ~~ pros.s
cons.pros.is.plot <- plot.level.slope(cons.pros,
                                      NA,
                                      1:8,
                                      'Conscientiousness',
                                      'Prosperity', '.s')

# view plot
print(cons.pros.is.plot)

# if looks good, save plot
ggsave(filename = here('Output/Primary/Analysis 3/Plots/cons_pros_is.png'), 
       plot = cons.pros.is.plot,
       width = 6, height = 4.5, scale = 1.5)

# cons.s ~~ pros.i
cons.pros.si.plot <- plot.slope.level(cons.pros,
                                      NA,
                                      1:8,
                                      'Conscientiousness', '.s',
                                      'Prosperity')

# view plot
print(cons.pros.si.plot)

# if looks good, save plot
ggsave(filename = here('Output/Primary/Analysis 3/Plots/cons_pros_si.png'), 
       plot = cons.pros.si.plot,
       width = 6, height = 4.5, scale = 1.5)

## fame

# script
cons.fame.script <- bivariate.linear.script('cons', 'fame', 1:8)
cat(cons.fame.script)

# fit model
cons.fame <- sem(cons.fame.script, 
                 data = data, 
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(cons.fame, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.bivariate.linear.results(cons.fame, 1:8)

## attractiveness

# script
cons.attr.script <- bivariate.linear.script('cons', 'attr', 1:8)
cat(cons.attr.script)

# fit model
cons.attr <- sem(cons.attr.script, 
                 data = data, 
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(cons.attr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.bivariate.linear.results(cons.attr, 1:8)

## plot

# cons.i ~~ attr.s
cons.attr.is.plot <- plot.level.slope(cons.attr,
                                      NA,
                                      1:8,
                                      'Conscientiousness',
                                      'Attractiveness', '.s')

# view plot
print(cons.attr.is.plot)

# if looks good, save plot
ggsave(filename = here('Output/Primary/Analysis 3/Plots/cons_attr_is.png'), 
       plot = cons.attr.is.plot,
       width = 6, height = 4.5, scale = 1.5)

## personal growth

# script
cons.pers.script <- bivariate.linear.script('cons', 'pers', 1:4)
cat(cons.pers.script)

# fit model
cons.pers <- sem(cons.pers.script, 
                 data = data, 
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(cons.pers, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.bivariate.linear.results(cons.pers, 1:4)

## relationships

# script
cons.rela.script <- bivariate.linear.script('cons', 'rela', 1:4)
cat(cons.rela.script)

# fit model
cons.rela <- sem(cons.rela.script, 
                 data = data, 
                 missing = 'fiml',
                 estimator = 'mlr') 

# model summary
summary(cons.rela, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.bivariate.linear.results(cons.rela, 1:4)

## society

# script
cons.soci.script <- bivariate.linear.script('cons', 'soci', 1:4)
cat(cons.soci.script)

# fit model
cons.soci <- sem(cons.soci.script, 
                 data = data, 
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(cons.soci, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.bivariate.linear.results(cons.soci, 1:4)

## physical health

# script
cons.phys.script <- bivariate.linear.script('cons', 'phys', 1:4)
cat(cons.phys.script)

# fit model
cons.phys <- sem(cons.phys.script, 
                 data = data, 
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(cons.phys, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.bivariate.linear.results(cons.phys, 1:4)

## plot

## hedonism

# script
cons.hedo.script <- bivariate.linear.script('cons', 'hedo', 1:4)
cat(cons.hedo.script)

# fit model
cons.hedo <- sem(cons.hedo.script, 
                 data = data, 
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(cons.hedo, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.bivariate.linear.results(cons.hedo, 1:4)

## plot

# cons.s ~~ hedo.i
cons.hedo.si.plot <- plot.slope.level(cons.hedo,
                                      NA,
                                      1:4,
                                      'Conscientiousness', '.s',
                                      'Hedonism')

# view plot
print(cons.hedo.si.plot)

# if looks good, save plot
ggsave(filename = here('Output/Primary/Analysis 3/Plots/cons_hedo_si.png'), 
       plot = cons.hedo.si.plot,
       width = 6, height = 4.5, scale = 1.5)

## liberal values

## conscientiousness T1 predicting liberal values T1

## linear

# script
cons.libe.script <- trait.predict.linear.script('cons', 'libe', 1:8)
cat(cons.libe.script)

# fit model
cons.libe <- sem(cons.libe.script,
                 data = data,
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(cons.libe, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.linear.results(cons.libe, 'trait', 1:8)

## best-fitting

# script
cons.libe.script <- trait.predict.bestfit.script('cons', 'libe', 1:8)
cat(cons.libe.script)

# fit model
cons.libe <- sem(cons.libe.script,
                 data = data,
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(cons.libe, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.bestfit.results(cons.libe, 'trait', 1:8)

## liberal values T1 predicting conscientiousness T1-8

## linear

# script
libe.cons.script <- goal.predict.linear.script('libe', 'cons', 1:8)
cat(libe.cons.script)

# fit model
libe.cons <- sem(libe.cons.script,
                 data = data,
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(libe.cons, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.linear.results(libe.cons, 'goal', 1:8)

## plot

# cons.s ~ libe
libe.cons.s.plot <- plot.goal.predicting.slope(libe.cons,
                                               NA,
                                               1:8,
                                               'Liberal Values',
                                               'Conscientiousness', '.s')

# view plot
print(libe.cons.s.plot)

# if looks good, save plot
ggsave(filename = here('Output/Primary/Analysis 3/Plots/libe_cons_s.png'), 
       plot = libe.cons.s.plot,
       width = 6, height = 4.5, scale = 1.5)

## best-fitting

# script
libe.cons.script <- goal.predict.bestfit.script('libe', 'cons', 1:8)
cat(libe.cons.script)

# fit model
libe.cons <- sem(libe.cons.script,
                 data = data,
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(libe.cons, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.bestfit.results(libe.cons, 'goal', 1:8)

## security

## conscientiousness T1-4 predicting security T3/T4

## linear

# script
cons.secu.script <- trait.predict.linear.script('cons', 'secu', 1:4)
cat(cons.secu.script)

# fit model
cons.secu <- sem(cons.secu.script,
                 data = data,
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(cons.secu, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.linear.results(cons.secu, 'trait', 1:4)

## plot

# secu ~ cons.s
cons.secu.s.plot <- plot.goal.predicted.slope(cons.secu,
                                              NA,
                                              1:4,
                                              'Security',
                                              'Conscientiousness', '.s')

# view plot
print(cons.secu.s.plot)

# if looks good, save plot
ggsave(filename = here('Output/Primary/Analysis 3/Plots/cons_secu_s.png'), 
       plot = cons.secu.s.plot,
       width = 6, height = 4.5, scale = 1.5)

## best-fitting

# script
cons.secu.script <- trait.predict.bestfit.script('cons', 'secu', 1:4)
cat(cons.secu.script)

# fit model
cons.secu <- sem(cons.secu.script,
                 data = data,
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(cons.secu, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.bestfit.results(cons.secu, 'trait', 1:4)

## security T3/T4 predicting conscientiousness T4-8

## linear

# script
secu.cons.script <- goal.predict.linear.script('secu', 'cons', 4:8)
cat(secu.cons.script)

# fit model
secu.cons <- sem(secu.cons.script,
                 data = data,
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(secu.cons, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.linear.results(secu.cons, 'goal', 4:8)

## plot

# cons.s ~ secu
secu.cons.s.plot <- plot.goal.predicting.slope(secu.cons,
                                               NA,
                                               4:8,
                                               'Security',
                                               'Conscientiousness', '.s')

# view plot
print(secu.cons.s.plot)

# if looks good, save plot
ggsave(filename = here('Output/Primary/Analysis 3/Plots/secu_cons_s.png'), 
       plot = secu.cons.s.plot,
       width = 6, height = 4.5, scale = 1.5)

## best-fitting

# script
secu.cons.script <- goal.predict.bestfit.script('secu', 'cons', 4:8)
cat(secu.cons.script)

# fit model
secu.cons <- sem(secu.cons.script,
                 data = data,
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(secu.cons, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.bestfit.results(secu.cons, 'goal', 4:8)

## conformism

## conscientiousness T1-4 predicting conformism T3/T4

## linear

# script
cons.conf.script <- trait.predict.linear.script('cons', 'conf', 1:4)
cat(cons.conf.script)

# fit model
cons.conf <- sem(cons.conf.script,
                 data = data,
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(cons.conf, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.linear.results(cons.conf, 'trait', 1:4)

## best-fitting

# script
cons.conf.script <- trait.predict.bestfit.script('cons', 'conf', 1:4)
cat(cons.conf.script)

# fit model
cons.conf <- sem(cons.conf.script,
                 data = data,
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(cons.conf, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.bestfit.results(cons.conf, 'trait', 1:4)

## conformism T3/T4 predicting conscientiousness T4-8

## linear

# script
conf.cons.script <- goal.predict.linear.script('conf', 'cons', 4:8)
cat(conf.cons.script)

# fit model
conf.cons <- sem(conf.cons.script,
                 data = data,
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(conf.cons, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.linear.results(conf.cons, 'goal', 4:8)

## best-fitting

# script
conf.cons.script <- goal.predict.bestfit.script('conf', 'cons', 4:8)
cat(conf.cons.script)

# fit model
conf.cons <- sem(conf.cons.script,
                 data = data,
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(conf.cons, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.bestfit.results(conf.cons, 'goal', 4:8)

## religiosity

## conscientiousness T1-4 predicting religiosity T3/T4

## linear

# script
cons.reli.script <- trait.predict.linear.script('cons', 'reli', 1:4)
cat(cons.reli.script)

# fit model
cons.reli <- sem(cons.reli.script,
                 data = data,
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(cons.reli, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.linear.results(cons.reli, 'trait', 1:4)

## best-fitting

# script
cons.reli.script <- trait.predict.bestfit.script('cons', 'reli', 1:4)
cat(cons.reli.script)

# fit model
cons.reli <- sem(cons.reli.script,
                 data = data,
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(cons.reli, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.bestfit.results(cons.reli, 'trait', 1:4)

## religiosity T3/T4 predicting conscientiousness T4-8

## linear

# script
reli.cons.script <- goal.predict.linear.script('reli', 'cons', 4:8)
cat(reli.cons.script)

# fit model
reli.cons <- sem(reli.cons.script,
                 data = data,
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(reli.cons, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.linear.results(reli.cons, 'goal', 4:8)

## best-fitting

# script
reli.cons.script <- goal.predict.bestfit.script('reli', 'cons', 4:8)
cat(reli.cons.script)

# fit model
reli.cons <- sem(reli.cons.script,
                 data = data,
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(reli.cons, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.bestfit.results(reli.cons, 'goal', 4:8)

# clean environment
rm(list = ls(pattern = 'cons'))

## neuroticism #################################################################

## prosperity

# script
neur.pros.script <- bivariate.linear.script('neur', 'pros', 1:8)
cat(neur.pros.script)

# fit model
neur.pros <- sem(neur.pros.script, 
                 data = data, 
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(neur.pros, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.bivariate.linear.results(neur.pros, 1:8)

## fame

# script
neur.fame.script <- bivariate.linear.script('neur', 'fame', 1:8)
cat(neur.fame.script)

# fit model
neur.fame <- sem(neur.fame.script, 
                 data = data, 
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(neur.fame, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.bivariate.linear.results(neur.fame, 1:8)

## plot

# neur.i ~~ fame.s
neur.fame.is.plot <- plot.level.slope(neur.fame,
                                      NA,
                                      1:8,
                                      'Neuroticism',
                                      'Fame', '.s')

# view plot
print(neur.fame.is.plot)

# if looks good, save plot
ggsave(filename = here('Output/Primary/Analysis 3/Plots/neur_fame_is.png'), 
       plot = neur.fame.is.plot,
       width = 6, height = 4.5, scale = 1.5)

## attractiveness

# script
neur.attr.script <- bivariate.linear.script('neur', 'attr', 1:8)
cat(neur.attr.script)

# fit model
neur.attr <- sem(neur.attr.script, 
                 data = data, 
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(neur.attr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.bivariate.linear.results(neur.attr, 1:8)

## personal growth

# script
neur.pers.script <- bivariate.linear.script('neur', 'pers', 1:4)
cat(neur.pers.script)

# fit model
neur.pers <- sem(neur.pers.script, 
                 data = data, 
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(neur.pers, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.bivariate.linear.results(neur.pers, 1:4)

## plot

# neur.i ~~ pers.s
neur.pers.is.plot <- plot.level.slope(neur.pers,
                                      NA,
                                      1:4,
                                      'Neuroticism',
                                      'Personal Growth', '.s')

# view plot
print(neur.pers.is.plot)

# if looks good, save plot
ggsave(filename = here('Output/Primary/Analysis 3/Plots/neur_pers_is.png'), 
       plot = neur.pers.is.plot,
       width = 6, height = 4.5, scale = 1.5)

## relationships

# script
neur.rela.script <- bivariate.linear.script('neur', 'rela', 1:4)
cat(neur.rela.script)

# fit model
neur.rela <- sem(neur.rela.script, 
                 data = data, 
                 missing = 'fiml',
                 estimator = 'mlr') 

# model summary
summary(neur.rela, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.bivariate.linear.results(neur.rela, 1:4)

## society

# script
neur.soci.script <- bivariate.linear.script('neur', 'soci', 1:4)
cat(neur.soci.script)

# fit model
neur.soci <- sem(neur.soci.script, 
                 data = data, 
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(neur.soci, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.bivariate.linear.results(neur.soci, 1:4)

## physical health

# script
neur.phys.script <- bivariate.linear.script('neur', 'phys', 1:4)
cat(neur.phys.script)

# fit model
neur.phys <- sem(neur.phys.script, 
                 data = data, 
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(neur.phys, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.bivariate.linear.results(neur.phys, 1:4)

## hedonism

# script
neur.hedo.script <- bivariate.linear.script('neur', 'hedo', 1:4)
cat(neur.hedo.script)

# fit model
neur.hedo <- sem(neur.hedo.script, 
                 data = data, 
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(neur.hedo, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.bivariate.linear.results(neur.hedo, 1:4)

## liberal values

## neuroticism T1 predicting liberal values T1

## linear

# script
neur.libe.script <- trait.predict.linear.script('neur', 'libe', 1:8)
cat(neur.libe.script)

# fit model
neur.libe <- sem(neur.libe.script,
                 data = data,
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(neur.libe, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.linear.results(neur.libe, 'trait', 1:8)

## best-fitting

# script
neur.libe.script <- trait.predict.bestfit.script('neur', 'libe', 1:8)
cat(neur.libe.script)

# fit model
neur.libe <- sem(neur.libe.script,
                 data = data,
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(neur.libe, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.bestfit.results(neur.libe, 'trait', 1:8)

## liberal values T1 predicting neuroticism T1-8

## linear

# script
libe.neur.script <- goal.predict.linear.script('libe', 'neur', 1:8)
cat(libe.neur.script)

# fit model
libe.neur <- sem(libe.neur.script,
                 data = data,
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(libe.neur, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.linear.results(libe.neur, 'goal', 1:8)

## best-fitting

# script
libe.neur.script <- goal.predict.bestfit.script('libe', 'neur', 1:8)
cat(libe.neur.script)

# fit model
libe.neur <- sem(libe.neur.script,
                 data = data,
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(libe.neur, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.bestfit.results(libe.neur, 'goal', 1:8)

## security

## neuroticism T1-4 predicting security T3/T4

## linear

# script
neur.secu.script <- trait.predict.linear.script('neur', 'secu', 1:4)
cat(neur.secu.script)

# fit model
neur.secu <- sem(neur.secu.script,
                 data = data,
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(neur.secu, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.linear.results(neur.secu, 'trait', 1:4)

## plot

# secu ~ neur.s
neur.secu.s.plot <- plot.goal.predicted.slope(neur.secu,
                                              NA,
                                              1:4,
                                              'Security',
                                              'Neuroticism', '.s')

# view plot
print(neur.secu.s.plot)

# if looks good, save plot
ggsave(filename = here('Output/Primary/Analysis 3/Plots/neur_secu_s.png'), 
       plot = neur.secu.s.plot,
       width = 6, height = 4.5, scale = 1.5)

## best-fitting

# script
neur.secu.script <- trait.predict.bestfit.script('neur', 'secu', 1:4)
cat(neur.secu.script)

# fit model
neur.secu <- sem(neur.secu.script,
                 data = data,
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(neur.secu, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.bestfit.results(neur.secu, 'trait', 1:4)

## security T3/T4 predicting neuroticism T4-8

## linear

# script
secu.neur.script <- goal.predict.linear.script('secu', 'neur', 4:8)
cat(secu.neur.script)

# fit model
secu.neur <- sem(secu.neur.script,
                 data = data,
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(secu.neur, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.linear.results(secu.neur, 'goal', 4:8)

## best-fitting

# script
secu.neur.script <- goal.predict.bestfit.script('secu', 'neur', 4:8)
cat(secu.neur.script)

# fit model
secu.neur <- sem(secu.neur.script,
                 data = data,
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(secu.neur, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.bestfit.results(secu.neur, 'goal', 4:8)

## conformism

## neuroticism T1-4 predicting conformism T3/T4

## linear

# script
neur.conf.script <- trait.predict.linear.script('neur', 'conf', 1:4)
cat(neur.conf.script)

# fit model
neur.conf <- sem(neur.conf.script,
                 data = data,
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(neur.conf, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.linear.results(neur.conf, 'trait', 1:4)

## plot

# conf ~ neur.s
neur.conf.s.plot <- plot.goal.predicted.slope(neur.conf,
                                              NA,
                                              1:4,
                                              'Conformism',
                                              'Neuroticism', '.s')

# view plot
print(neur.conf.s.plot)

# if looks good, save plot
ggsave(filename = here('Output/Primary/Analysis 3/Plots/neur_conf_s.png'), 
       plot = neur.conf.s.plot,
       width = 6, height = 4.5, scale = 1.5)

## best-fitting

# script
neur.conf.script <- trait.predict.bestfit.script('neur', 'conf', 1:4)
cat(neur.conf.script)

# fit model
neur.conf <- sem(neur.conf.script,
                 data = data,
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(neur.conf, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.bestfit.results(neur.conf, 'trait', 1:4)

## conformism T3/T4 predicting neuroticism T4-8

## linear

# script
conf.neur.script <- goal.predict.linear.script('conf', 'neur', 4:8)
cat(conf.neur.script)

# fit model
conf.neur <- sem(conf.neur.script,
                 data = data,
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(conf.neur, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.linear.results(conf.neur, 'goal', 4:8)

## best-fitting

# script
conf.neur.script <- goal.predict.bestfit.script('conf', 'neur', 4:8)
cat(conf.neur.script)

# fit model
conf.neur <- sem(conf.neur.script,
                 data = data,
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(conf.neur, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.bestfit.results(conf.neur, 'goal', 4:8)

## religiosity

## neuroticism T1-4 predicting religiosity T3/T4

## linear

# script
neur.reli.script <- trait.predict.linear.script('neur', 'reli', 1:4)
cat(neur.reli.script)

# fit model
neur.reli <- sem(neur.reli.script,
                 data = data,
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(neur.reli, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.linear.results(neur.reli, 'trait', 1:4)

## plot

# reli ~ neur.s
neur.reli.s.plot <- plot.goal.predicted.slope(neur.reli,
                                              NA,
                                              1:4,
                                              'Religiosity',
                                              'Neuroticism', '.s')

# view plot
print(neur.reli.s.plot)

# if looks good, save plot
ggsave(filename = here('Output/Primary/Analysis 3/Plots/neur_reli_s.png'), 
       plot = neur.reli.s.plot,
       width = 6, height = 4.5, scale = 1.5)

## best-fitting

# script
neur.reli.script <- trait.predict.bestfit.script('neur', 'reli', 1:4)
cat(neur.reli.script)

# fit model
neur.reli <- sem(neur.reli.script,
                 data = data,
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(neur.reli, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.bestfit.results(neur.reli, 'trait', 1:4)

## religiosity T3/T4 predicting neuroticism T4-8

## linear

# script
reli.neur.script <- goal.predict.linear.script('reli', 'neur', 4:8)
cat(reli.neur.script)

# fit model
reli.neur <- sem(reli.neur.script,
                 data = data,
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(reli.neur, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.linear.results(reli.neur, 'goal', 4:8)

## best-fitting

# script
reli.neur.script <- goal.predict.bestfit.script('reli', 'neur', 4:8)
cat(reli.neur.script)

# fit model
reli.neur <- sem(reli.neur.script,
                 data = data,
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(reli.neur, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.bestfit.results(reli.neur, 'goal', 4:8)

# clean environment
rm(list = ls(pattern = 'neur'))

## openness ####################################################################

## prosperity

# script
open.pros.script <- bivariate.linear.script('open', 'pros', 1:8)
cat(open.pros.script)

# fit model
open.pros <- sem(open.pros.script, 
                 data = data, 
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(open.pros, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.bivariate.linear.results(open.pros, 1:8)

## plot

# open.i ~~ pros.s
open.pros.is.plot <- plot.level.slope(open.pros,
                                      NA,
                                      1:8,
                                      'Openness',
                                      'Prosperity', '.s')

# view plot
print(open.pros.is.plot)

# if looks good, save plot
ggsave(filename = here('Output/Primary/Analysis 3/Plots/open_pros_is.png'), 
       plot = open.pros.is.plot,
       width = 6, height = 4.5, scale = 1.5)

# open.s ~~ pros.i
open.pros.si.plot <- plot.slope.level(open.pros,
                                      NA,
                                      1:8,
                                      'Openness', '.s',
                                      'Prosperity')

# view plot
print(open.pros.si.plot)

# if looks good, save plot
ggsave(filename = here('Output/Primary/Analysis 3/Plots/open_pros_si.png'), 
       plot = open.pros.si.plot,
       width = 6, height = 4.5, scale = 1.5)

## fame

# script
open.fame.script <- bivariate.linear.script('open', 'fame', 1:8)
cat(open.fame.script)

# fit model
open.fame <- sem(open.fame.script, 
                 data = data, 
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(open.fame, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.bivariate.linear.results(open.fame, 1:8)

## attractiveness

# script
open.attr.script <- bivariate.linear.script('open', 'attr', 1:8)
cat(open.attr.script)

# fit model
open.attr <- sem(open.attr.script, 
                 data = data, 
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(open.attr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.bivariate.linear.results(open.attr, 1:8)

## plot

# open.i ~~ attr.s
open.attr.is.plot <- plot.level.slope(open.attr,
                                      NA,
                                      1:8,
                                      'Openness',
                                      'Attractiveness', '.s')

# view plot
print(open.attr.is.plot)

# if looks good, save plot
ggsave(filename = here('Output/Primary/Analysis 3/Plots/open_attr_is.png'), 
       plot = open.attr.is.plot,
       width = 6, height = 4.5, scale = 1.5)

## personal growth

# script
open.pers.script <- bivariate.linear.script('open', 'pers', 1:4)
cat(open.pers.script)

# fit model
open.pers <- sem(open.pers.script, 
                 data = data, 
                 missing = 'fiml')

# model summary
summary(open.pers, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.bivariate.linear.results(open.pers, 1:4)

## plot

# open.i ~~ pers.s
open.pers.is.plot <- plot.level.slope(open.pers,
                                      NA,
                                      1:4,
                                      'Openness',
                                      'Personal Growth', '.s')

# view plot
print(open.pers.is.plot)

# if looks good, save plot
ggsave(filename = here('Output/Primary/Analysis 3/Plots/open_pers_is.png'), 
       plot = open.pers.is.plot,
       width = 6, height = 4.5, scale = 1.5)

# open.s ~~ pers.i
open.pers.si.plot <- plot.slope.level(open.pers,
                                      NA,
                                      1:4,
                                      'Openness', '.s',
                                      'Personal Growth')

# view plot
print(open.pers.si.plot)

# if looks good, save plot
ggsave(filename = here('Output/Primary/Analysis 3/Plots/open_pers_si.png'), 
       plot = open.pers.si.plot,
       width = 6, height = 4.5, scale = 1.5)

## relationships

# script
open.rela.script <- bivariate.linear.script('open', 'rela', 1:4)
cat(open.rela.script)

# fit model
open.rela <- sem(open.rela.script, 
                 data = data, 
                 missing = 'fiml',
                 estimator = 'mlr') 

# model summary
summary(open.rela, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.bivariate.linear.results(open.rela, 1:4)

## society

# script
open.soci.script <- bivariate.linear.script('open', 'soci', 1:4)
cat(open.soci.script)

# fit model
open.soci <- sem(open.soci.script, 
                 data = data, 
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(open.soci, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.bivariate.linear.results(open.soci, 1:4)

## physical health

# script
open.phys.script <- bivariate.linear.script('open', 'phys', 1:4)
cat(open.phys.script)

# fit model
open.phys <- sem(open.phys.script, 
                 data = data, 
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(open.phys, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.bivariate.linear.results(open.phys, 1:4)

## plot

# open.i ~~ phys.s
open.phys.is.plot <- plot.level.slope(open.phys,
                                      NA,
                                      1:4,
                                      'Openness',
                                      'Physical Health', '.s')

# view plot
print(open.phys.is.plot)

# if looks good, save plot
ggsave(filename = here('Output/Primary/Analysis 3/Plots/open_phys_is.png'), 
       plot = open.phys.is.plot,
       width = 6, height = 4.5, scale = 1.5)

## hedonism

# script
open.hedo.script <- bivariate.linear.script('open', 'hedo', 1:4)
cat(open.hedo.script)

# fit model
open.hedo <- sem(open.hedo.script, 
                 data = data, 
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(open.hedo, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.bivariate.linear.results(open.hedo, 1:4)

## liberal values

## openness T1 predicting liberal values T1

## linear

# script
open.libe.script <- trait.predict.linear.script('open', 'libe', 1:8)
cat(open.libe.script)

# fit model
open.libe <- sem(open.libe.script,
                 data = data,
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(open.libe, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.linear.results(open.libe, 'trait', 1:8)

## best-fitting

# script
open.libe.script <- trait.predict.bestfit.script('open', 'libe', 1:8)
cat(open.libe.script)

# fit model
open.libe <- sem(open.libe.script,
                 data = data,
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(open.libe, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.bestfit.results(open.libe, 'trait', 1:8)

## liberal values T1 predicting openness T1-8

## linear

# script
libe.open.script <- goal.predict.linear.script('libe', 'open', 1:8)
cat(libe.open.script)

# fit model
libe.open <- sem(libe.open.script,
                 data = data,
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(libe.open, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.linear.results(libe.open, 'goal', 1:8)

## best-fitting

# script
libe.open.script <- goal.predict.bestfit.script('libe', 'open', 1:8)
cat(libe.open.script)

# fit model
libe.open <- sem(libe.open.script,
                 data = data,
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(libe.open, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.bestfit.results(libe.open, 'goal', 1:8)

## security

## openness T1-4 predicting security

## linear

# script
open.secu.script <- trait.predict.linear.script('open', 'secu', 1:4)
cat(open.secu.script)

# fit model
open.secu <- sem(open.secu.script,
                 data = data,
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(open.secu, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.linear.results(open.secu, 'trait', 1:4)

## best-fitting

# script
open.secu.script <- trait.predict.bestfit.script('open', 'secu', 1:4)
cat(open.secu.script)

# fit model
open.secu <- sem(open.secu.script,
                 data = data,
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(open.secu, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.bestfit.results(open.secu, 'trait', 1:4)

## security T3/T4 predicting openness T4-8

## linear

# script
secu.open.script <- goal.predict.linear.script('secu', 'open', 4:8)
cat(secu.open.script)

# fit model
secu.open <- sem(secu.open.script,
                 data = data,
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(secu.open, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.linear.results(secu.open, 'goal', 4:8)

## best-fitting

# script
secu.open.script <- goal.predict.bestfit.script('secu', 'open', 4:8)
cat(secu.open.script)

# fit model
secu.open <- sem(secu.open.script,
                 data = data,
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(secu.open, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.bestfit.results(secu.open, 'goal', 4:8)

## conformism

## openness T1-4 predicting conformism T3/T4

## linear

# script
open.conf.script <- trait.predict.linear.script('open', 'conf', 1:4)
cat(open.conf.script)

# fit model
open.conf <- sem(open.conf.script,
                 data = data,
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(open.conf, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.linear.results(open.conf, 'trait', 1:4)

## plot

# conf ~ open.s
open.conf.s.plot <- plot.goal.predicted.slope(open.conf,
                                              NA,
                                              1:4,
                                              'Conformism',
                                              'Openness', '.s')

# view plot
print(open.conf.s.plot)

# if looks good, save plot
ggsave(filename = here('Output/Primary/Analysis 3/Plots/open_conf_s.png'), 
       plot = open.conf.s.plot,
       width = 6, height = 4.5, scale = 1.5)

## best-fitting

# script
open.conf.script <- trait.predict.bestfit.script('open', 'conf', 1:4)
cat(open.conf.script)

# fit model
open.conf <- sem(open.conf.script,
                 data = data,
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(open.conf, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.bestfit.results(open.conf, 'trait', 1:4)

## conformism T3/T4 predicting openness T4-8

## linear 

# script
conf.open.script <- goal.predict.linear.script('conf', 'open', 4:8)
cat(conf.open.script)

# fit model
conf.open <- sem(conf.open.script,
                 data = data,
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(conf.open, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.linear.results(conf.open, 'goal', 4:8)

## plot

# open.s ~ conf
conf.open.s.plot <- plot.goal.predicting.slope(conf.open,
                                               NA,
                                               4:8,
                                               'Conformism',
                                               'Openness', '.s')

# view plot
print(conf.open.s.plot)

# if looks good, save plot
ggsave(filename = here('Output/Primary/Analysis 3/Plots/conf_open_s.png'), 
       plot = conf.open.s.plot,
       width = 6, height = 4.5, scale = 1.5)

## best-fitting 

# script
conf.open.script <- goal.predict.bestfit.script('conf', 'open', 4:8)
cat(conf.open.script)

# fit model
conf.open <- sem(conf.open.script,
                 data = data,
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(conf.open, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.bestfit.results(conf.open, 'goal', 4:8)

## religiosity

## openness T1-4 predicting religiosity

## linear

# script
open.reli.script <- trait.predict.linear.script('open', 'reli', 1:4)
cat(open.reli.script)

# fit model
open.reli <- sem(open.reli.script,
                 data = data,
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(open.reli, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.linear.results(open.reli, 'trait', 1:4)

## plot

# reli ~ open.s
open.reli.s.plot <- plot.goal.predicted.slope(open.reli,
                                              NA,
                                              1:4,
                                              'Religiosity',
                                              'Openness', '.s')

# view plot
print(open.reli.s.plot)

# if looks good, save plot
ggsave(filename = here('Output/Primary/Analysis 3/Plots/open_reli_s.png'), 
       plot = open.reli.s.plot,
       width = 6, height = 4.5, scale = 1.5)

## best-fitting

# script
open.reli.script <- trait.predict.bestfit.script('open', 'reli', 1:4)
cat(open.reli.script)

# fit model
open.reli <- sem(open.reli.script,
                 data = data,
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(open.reli, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.bestfit.results(open.reli, 'trait', 1:4)

## religiosity T3/T4 predicting openness T4-8

## linear

# script
reli.open.script <- goal.predict.linear.script('reli', 'open', 4:8)
cat(reli.open.script)

# fit model
reli.open <- sem(reli.open.script,
                 data = data,
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(reli.open, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.linear.results(reli.open, 'goal', 4:8)

## plot

# open.s ~ reli
reli.open.s.plot <- plot.goal.predicting.slope(reli.open,
                                               NA,
                                               4:8,
                                               'Religiosity',
                                               'Openness', '.s')

# view plot
print(reli.open.s.plot)

# if looks good, save plot
ggsave(filename = here('Output/Primary/Analysis 3/Plots/reli_open_s.png'), 
       plot = reli.open.s.plot,
       width = 6, height = 4.5, scale = 1.5)

## best-fitting

# script
reli.open.script <- goal.predict.bestfit.script('reli', 'open', 4:8)
cat(reli.open.script)

# fit model
reli.open <- sem(reli.open.script,
                 data = data,
                 missing = 'fiml',
                 estimator = 'mlr')

# model summary
summary(reli.open, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.bestfit.results(reli.open, 'goal', 4:8)

# clean environment
rm(list = ls(pattern = 'open'))

# sensitivity analysis. gender differences (pt1. univariate) ###################

# create gender differences column
univariate$gender <- NA

# currently, male=1, female=2

table(data$gender) # 1=1254, 2=1763

# add strings labels
data$gender <- factor(data$gender,
                      levels = c(1, 2),
                      labels = c('male', 'female'))

table(data$gender) # male=1254, female=1763

# note. model summaries will print as Group 1 [female] and Group 2 [male].

## extraversion ################################################################

# script
extr.quadra.script <- quadra.script('extr', 1:8)
cat(extr.quadra.script)

## constrained model

# fit model
extr.gender.constr <- sem(extr.quadra.script, 
                          data = data, 
                          missing = 'fiml',
                          estimator = 'mlr',
                          group = 'gender',
                          group.equal = c('loadings',
                                          'means',
                                          'lv.variances',
                                          'lv.covariances'))

# model summary
summary(extr.gender.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# fit model
extr.gender.freely <- sem(extr.quadra.script, 
                          data = data, 
                          missing = 'fiml',
                          estimator = 'mlr',
                          group = 'gender',
                          group.equal = c('loadings'))

# model summary
summary(extr.gender.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.univariate.gender.results(extr.gender.constr,
                                 extr.gender.freely,
                                 'quadra',
                                 1:8)

## model comparisons

# compare model fits
summary(compareFit(extr.gender.constr, extr.gender.freely)) # freely fits better

# update summary
univariate[univariate$var == 'extr', 'gender'] <- 'freely'

## plot

# create average plots
extr.male.plot <- plot.average.gender(extr.gender.freely, 1:8, 'male')
extr.female.plot <- plot.average.gender(extr.gender.freely, 1:8, 'female')

# combined (male + female)
extr.gender.plot <- plot.combined.gender(extr.gender.freely,
                                         extr.male.average.data,
                                         extr.female.average.data,
                                         1:8)

# view plot
print(extr.gender.plot)

# if looks good, save plot
ggsave(filename = here('Output/Sensitivity/Analysis 2/Plots/extr_gender.png'), 
       plot = extr.gender.plot,
       width = 6, height = 4.5, scale = 1)

# clean environment
rm(list = ls(pattern = '^extr\\.|rows|^time'))

## agreeableness ###############################################################

# script
agre.latent.script <- latent.script('agre', 1:8)
cat(agre.latent.script)

## constrained model

# fit model
agre.gender.constr <- sem(agre.latent.script, 
                          data = data, 
                          missing = 'fiml',
                          estimator = 'mlr',
                          group = 'gender',
                          group.equal = c('loadings',
                                          'means',
                                          'lv.variances',
                                          'lv.covariances'))

# model summary
summary(agre.gender.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# fit model
agre.gender.freely <- sem(agre.latent.script, 
                          data = data, 
                          missing = 'fiml',
                          estimator = 'mlr',
                          group = 'gender',
                          group.equal = c('loadings'))

# model summary
summary(agre.gender.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.univariate.gender.results(agre.gender.constr,
                                 agre.gender.freely,
                                 'latent',
                                 1:8)

## model comparisons

# compare model fits
summary(compareFit(agre.gender.constr, agre.gender.freely)) # freely fits better

# update summary
univariate[univariate$var == 'agre', 'gender'] <- 'freely'

## plot

# create average plots
agre.male.plot <- plot.average.gender(agre.gender.freely, 1:8, 'male')
agre.female.plot <- plot.average.gender(agre.gender.freely, 1:8, 'female')

# combined (male + female)
agre.gender.plot <- plot.combined.gender(agre.gender.freely,
                                         agre.male.average.data,
                                         agre.female.average.data,
                                         1:8)

# view plot
print(agre.gender.plot)

# if looks good, save plot
ggsave(filename = here('Output/Sensitivity/Analysis 2/Plots/agre_gender.png'), 
       plot = agre.gender.plot,
       width = 6, height = 4.5, scale = 1)

# clean environment
rm(list = ls(pattern = '^agre\\.|rows|^time'))

## conscientiousness ###########################################################

# script
cons.quadra.script <- quadra.script('cons', 1:8)
cat(cons.quadra.script)

## constrained model

# fit model
cons.gender.constr <- sem(cons.quadra.script, 
                          data = data, 
                          missing = 'fiml',
                          estimator = 'mlr',
                          group = 'gender',
                          group.equal = c('loadings',
                                          'means',
                                          'lv.variances',
                                          'lv.covariances'))

# model summary
summary(cons.gender.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# fit model
cons.gender.freely <- sem(cons.quadra.script, 
                          data = data, 
                          missing = 'fiml',
                          estimator = 'mlr',
                          group = 'gender',
                          group.equal = c('loadings'))

# model summary
summary(cons.gender.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.univariate.gender.results(cons.gender.constr,
                                 cons.gender.freely,
                                 'quadra',
                                 1:8)

## model comparisons

# compare model fits
summary(compareFit(cons.gender.constr, cons.gender.freely)) # freely fits better

# update summary
univariate[univariate$var == 'cons', 'gender'] <- 'freely'

## plot

# create average plots
cons.male.plot <- plot.average.gender(cons.gender.freely, 1:8, 'male')
cons.female.plot <- plot.average.gender(cons.gender.freely, 1:8, 'female')

# combined (male + female)
cons.gender.plot <- plot.combined.gender(cons.gender.freely,
                                         cons.male.average.data,
                                         cons.female.average.data,
                                         1:8)

# view plot
print(cons.gender.plot)

# if looks good, save plot
ggsave(filename = here('Output/Sensitivity/Analysis 2/Plots/cons_gender.png'), 
       plot = cons.gender.plot,
       width = 6, height = 4.5, scale = 1)

# clean environment
rm(list = ls(pattern = '^cons\\.|rows|^time'))

## neuroticism #################################################################

# script
neur.quadra.script <- quadra.script('neur', 1:8)
cat(neur.quadra.script)

## constrained model

# fit model
neur.gender.constr <- sem(neur.quadra.script, 
                          data = data, 
                          missing = 'fiml',
                          estimator = 'mlr',
                          group = 'gender',
                          group.equal = c('loadings',
                                          'means',
                                          'lv.variances',
                                          'lv.covariances'))

# model summary
summary(neur.gender.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# fit model
neur.gender.freely <- sem(neur.quadra.script, 
                          data = data, 
                          missing = 'fiml',
                          estimator = 'mlr',
                          group = 'gender',
                          group.equal = c('loadings'))

# model summary
summary(neur.gender.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.univariate.gender.results(neur.gender.constr,
                                 neur.gender.freely,
                                 'quadra',
                                 1:8)

## model comparisons

# compare model fits
summary(compareFit(neur.gender.constr, neur.gender.freely)) # freely fits better

# update summary
univariate[univariate$var == 'neur', 'gender'] <- 'freely'

## plot

# create average plots
neur.male.plot <- plot.average.gender(neur.gender.freely, 1:8, 'male')
neur.female.plot <- plot.average.gender(neur.gender.freely, 1:8, 'female')

# combined (male + female)
neur.gender.plot <- plot.combined.gender(neur.gender.freely,
                                         neur.male.average.data,
                                         neur.female.average.data,
                                         1:8)

# view plot
print(neur.gender.plot)

# if looks good, save plot
ggsave(filename = here('Output/Sensitivity/Analysis 2/Plots/neur_gender.png'), 
       plot = neur.gender.plot,
       width = 6, height = 4.5, scale = 1)

# clean environment
rm(list = ls(pattern = '^neur\\.|rows|^time'))

## openness ####################################################################

# script
open.quadra.script <- quadra.script('open', 1:8)
cat(open.quadra.script)

## constrained model

# fit model
open.gender.constr <- sem(open.quadra.script, 
                          data = data, 
                          missing = 'fiml',
                          estimator = 'mlr',
                          group = 'gender',
                          group.equal = c('loadings',
                                          'means',
                                          'lv.variances',
                                          'lv.covariances'))

# model summary
summary(open.gender.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# fit model
open.gender.freely <- sem(open.quadra.script, 
                          data = data, 
                          missing = 'fiml',
                          estimator = 'mlr',
                          group = 'gender',
                          group.equal = c('loadings'))

# model summary
summary(open.gender.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.univariate.gender.results(open.gender.constr,
                                 open.gender.freely,
                                 'quadra',
                                 1:8)

## model comparisons

# compare model fits
summary(compareFit(open.gender.constr, open.gender.freely)) # freely fits better

# update summary
univariate[univariate$var == 'open', 'gender'] <- 'freely'

## plot

# create average plots
open.male.plot <- plot.average.gender(open.gender.freely, 1:8, 'male')
open.female.plot <- plot.average.gender(open.gender.freely, 1:8, 'female')

# combined (male + female)
open.gender.plot <- plot.combined.gender(open.gender.freely,
                                         open.male.average.data,
                                         open.female.average.data,
                                         1:8)

# view plot
print(open.gender.plot)

# if looks good, save plot
ggsave(filename = here('Output/Sensitivity/Analysis 2/Plots/open_gender.png'), 
       plot = open.gender.plot,
       width = 6, height = 4.5, scale = 1)

# clean environment
rm(list = ls(pattern = '^open\\.|rows|^time'))

## prosperity ##################################################################

# script
pros.quadra.script <- quadra.script('pros', 1:8)
cat(pros.quadra.script)

## constrained model

# fit model
pros.gender.constr <- sem(pros.quadra.script, 
                          data = data, 
                          missing = 'fiml',
                          estimator = 'mlr',
                          group = 'gender',
                          group.equal = c('loadings',
                                          'means',
                                          'lv.variances',
                                          'lv.covariances'))

# model summary
summary(pros.gender.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# fit model
pros.gender.freely <- sem(pros.quadra.script, 
                          data = data, 
                          missing = 'fiml',
                          estimator = 'mlr',
                          group = 'gender',
                          group.equal = c('loadings'))

# model summary
summary(pros.gender.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.univariate.gender.results(pros.gender.constr,
                                 pros.gender.freely,
                                 'quadra',
                                 1:8)

## model comparisons

# compare model fits
summary(compareFit(pros.gender.constr, pros.gender.freely)) # freely fits better

# update summary
univariate[univariate$var == 'pros', 'gender'] <- 'freely'

## plot

# create average plots
pros.male.plot <- plot.average.gender(pros.gender.freely, 1:8, 'male')
pros.female.plot <- plot.average.gender(pros.gender.freely, 1:8, 'female')

# combined (male + female)
pros.gender.plot <- plot.combined.gender(pros.gender.freely,
                                         pros.male.average.data,
                                         pros.female.average.data,
                                         1:8)

# view plot
print(pros.gender.plot)

# if looks good, save plot
ggsave(filename = here('Output/Sensitivity/Analysis 2/Plots/pros_gender.png'), 
       plot = pros.gender.plot,
       width = 6, height = 4.5, scale = 1)

# clean environment
rm(list = ls(pattern = '^pros\\.|rows|^time'))

## fame ########################################################################

# script
fame.quadra.script <- quadra.script('fame', 1:8)
cat(fame.quadra.script)

## constrained model

# fit model
fame.gender.constr <- sem(fame.quadra.script, 
                          data = data, 
                          missing = 'fiml',
                          estimator = 'mlr',
                          group = 'gender',
                          group.equal = c('loadings',
                                          'means',
                                          'lv.variances',
                                          'lv.covariances'))

# model summary
summary(fame.gender.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# fit model
fame.gender.freely <- sem(fame.quadra.script, 
                          data = data, 
                          missing = 'fiml',
                          estimator = 'mlr',
                          group = 'gender',
                          group.equal = c('loadings'))

# model summary
summary(fame.gender.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.univariate.gender.results(fame.gender.constr,
                                 fame.gender.freely,
                                 'quadra',
                                 1:8)

## model comparisons

# compare model fits
summary(compareFit(fame.gender.constr, fame.gender.freely)) # freely fits better

# update summary
univariate[univariate$var == 'fame', 'gender'] <- 'freely'

## plot

# create average plots
fame.male.plot <- plot.average.gender(fame.gender.freely, 1:8, 'male')
fame.female.plot <- plot.average.gender(fame.gender.freely, 1:8, 'female')

# combined (male + female)
fame.gender.plot <- plot.combined.gender(fame.gender.freely,
                                         fame.male.average.data,
                                         fame.female.average.data,
                                         1:8)

# view plot
print(fame.gender.plot)

# if looks good, save plot
ggsave(filename = here('Output/Sensitivity/Analysis 2/Plots/fame_gender.png'), 
       plot = fame.gender.plot,
       width = 6, height = 4.5, scale = 1)

# clean environment
rm(list = ls(pattern = '^fame\\.|rows|^time'))

## attractiveness ##############################################################

# script
attr.latent.script <- latent.script('attr', 1:8)
cat(attr.latent.script)

## constrained model

# fit model
attr.gender.constr <- sem(attr.latent.script, 
                          data = data, 
                          missing = 'fiml',
                          estimator = 'mlr',
                          group = 'gender',
                          group.equal = c('loadings',
                                          'means',
                                          'lv.variances',
                                          'lv.covariances'))

# model summary
summary(attr.gender.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# fit model
attr.gender.freely <- sem(attr.latent.script, 
                          data = data, 
                          missing = 'fiml',
                          estimator = 'mlr',
                          group = 'gender',
                          group.equal = c('loadings'))

# model summary
summary(attr.gender.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.univariate.gender.results(attr.gender.constr,
                                 attr.gender.freely,
                                 'latent',
                                 1:8)

## model comparisons

# compare model fits
summary(compareFit(attr.gender.constr, attr.gender.freely)) # freely fits better

# update summary
univariate[univariate$var == 'attr', 'gender'] <- 'freely'

## plot

# create average plots
attr.male.plot <- plot.average.gender(attr.gender.freely, 1:8, 'male')
attr.female.plot <- plot.average.gender(attr.gender.freely, 1:8, 'female')

# combined (male + female)
attr.gender.plot <- plot.combined.gender(attr.gender.freely,
                                         attr.male.average.data,
                                         attr.female.average.data,
                                         1:8)

# view plot
print(attr.gender.plot)

# if looks good, save plot
ggsave(filename = here('Output/Sensitivity/Analysis 2/Plots/attr_gender.png'), 
       plot = attr.gender.plot,
       width = 6, height = 4.5, scale = 1)

# clean environment
rm(list = ls(pattern = '^attr\\.|rows|^time'))

## personal growth #############################################################

# script
pers.latent.script <- latent.script('pers', 1:4)
cat(pers.latent.script)

## constrained model

# fit model
pers.gender.constr <- sem(pers.latent.script, 
                          data = data, 
                          missing = 'fiml',
                          estimator = 'mlr',
                          group = 'gender',
                          group.equal = c('loadings',
                                          'means',
                                          'lv.variances',
                                          'lv.covariances'))

# model summary
summary(pers.gender.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# fit model
pers.gender.freely <- sem(pers.latent.script, 
                          data = data, 
                          missing = 'fiml',
                          estimator = 'mlr',
                          group = 'gender',
                          group.equal = c('loadings'))

# model summary
summary(pers.gender.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.univariate.gender.results(pers.gender.constr,
                                 pers.gender.freely,
                                 'latent',
                                 1:4)

## model comparisons

# compare model fits
summary(compareFit(pers.gender.constr, pers.gender.freely)) # freely fits better

# update summary
univariate[univariate$var == 'pers', 'gender'] <- 'freely'

## plot

# create average plots
pers.male.plot <- plot.average.gender(pers.gender.freely, 1:4, 'male')
pers.female.plot <- plot.average.gender(pers.gender.freely, 1:4, 'female')

# combined (male + female)
pers.gender.plot <- plot.combined.gender(pers.gender.freely,
                                         pers.male.average.data,
                                         pers.female.average.data,
                                         1:4)

# view plot
print(pers.gender.plot)

# if looks good, save plot
ggsave(filename = here('Output/Sensitivity/Analysis 2/Plots/pers_gender.png'), 
       plot = pers.gender.plot,
       width = 6, height = 4.5, scale = 1)

# clean environment
rm(list = ls(pattern = '^pers\\.|rows|^time'))

## relationship ###############################################################

# using the script from main analysis
rela.latent.script <- '# intercept loadings
                       rela.i =~ 1*rela.1
                       rela.i =~ 1*rela.2
                       rela.i =~ 1*rela.3
                       rela.i =~ 1*rela.4

                       # slope loadings
                       rela.l =~ 0*rela.1
                       rela.l =~ rela.2
                       rela.l =~ rela.3
                       rela.l =~ 1*rela.4

                       # latent means
                       rela.i ~ 1
                       rela.l ~ 1

                       # latent variances
                       rela.i ~~ rela.i
                       rela.l ~~ .001*rela.l # fixed to smallest positive number

                       # latent covariance
                       rela.i ~~ rela.l

                       # constrain means to 0
                       rela.1 ~ 0*1
                       rela.2 ~ 0*1
                       rela.3 ~ 0*1
                       rela.4 ~ 0*1

                       # constrain variances
                       rela.1 ~~ rela.theta*rela.1
                       rela.2 ~~ rela.theta*rela.2
                       rela.3 ~~ rela.theta*rela.3
                       rela.4 ~~ rela.theta*rela.4'

## constrained model

# fit model
rela.gender.constr <- sem(rela.latent.script, 
                          data = data, 
                          missing = 'fiml',
                          estimator = 'mlr',
                          group = 'gender',
                          group.equal = c('loadings',
                                          'means',
                                          'lv.variances',
                                          'lv.covariances'))

# model summary
summary(rela.gender.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# fit model
rela.gender.freely <- sem(rela.latent.script, 
                          data = data, 
                          missing = 'fiml',
                          estimator = 'mlr',
                          group = 'gender',
                          group.equal = c('loadings')) # non-positive definite

# inspect
summary(rela.gender.freely)
lavInspect(rela.gender.freely, 'cov.lv') # cov(rela.i, rela.l) for female is out of bounds

# rewrite script
rela.latent.script <- '# intercept loadings
                       rela.i =~ 1*rela.1
                       rela.i =~ 1*rela.2
                       rela.i =~ 1*rela.3
                       rela.i =~ 1*rela.4

                       # slope loadings
                       rela.l =~ 0*rela.1
                       rela.l =~ rela.2
                       rela.l =~ rela.3
                       rela.l =~ 1*rela.4

                       # latent means
                       rela.i ~ 1
                       rela.l ~ 1

                       # latent variances
                       rela.i ~~ rela.i
                       rela.l ~~ .001*rela.l # fixed to smallest positive number

                       # latent covariance
                       rela.i ~~ .001*rela.l # fixed to smallest positive number

                       # constrain means to 0
                       rela.1 ~ 0*1
                       rela.2 ~ 0*1
                       rela.3 ~ 0*1
                       rela.4 ~ 0*1

                       # constrain variances
                       rela.1 ~~ rela.theta*rela.1
                       rela.2 ~~ rela.theta*rela.2
                       rela.3 ~~ rela.theta*rela.3
                       rela.4 ~~ rela.theta*rela.4'

## re-fit models

## constrained model

# fit model
rela.gender.constr <- sem(rela.latent.script, 
                          data = data, 
                          missing = 'fiml',
                          estimator = 'mlr',
                          group = 'gender',
                          group.equal = c('loadings',
                                          'means',
                                          'lv.variances',
                                          'lv.covariances'))

# model summary
summary(rela.gender.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# fit model
rela.gender.freely <- sem(rela.latent.script, 
                          data = data, 
                          missing = 'fiml',
                          estimator = 'mlr',
                          group = 'gender',
                          group.equal = c('loadings'))

# model summary
summary(rela.gender.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.univariate.gender.results(rela.gender.constr,
                                 rela.gender.freely,
                                 'latent',
                                 1:4)

## model comparisons

# compare model fits
summary(compareFit(rela.gender.constr, rela.gender.freely)) # freely fits better

# update summary
univariate[univariate$var == 'rela', 'gender'] <- 'freely'

## plot

# create average plots
rela.male.plot <- plot.average.gender(rela.gender.freely, 1:4, 'male')
rela.female.plot <- plot.average.gender(rela.gender.freely, 1:4, 'female')

# combined (male + female)
rela.gender.plot <- plot.combined.gender(rela.gender.freely,
                                         rela.male.average.data,
                                         rela.female.average.data,
                                         1:4)

# view plot
print(rela.gender.plot)

# if looks good, save plot
ggsave(filename = here('Output/Sensitivity/Analysis 2/Plots/rela_gender.png'), 
       plot = rela.gender.plot,
       width = 6, height = 4.5, scale = 1)

# clean environment
rm(list = ls(pattern = '^rela\\.|rows|^time'))

## society #####################################################################

# script
soci.latent.script <- latent.script('soci', 1:4)
cat(soci.latent.script)

## constrained model

# fit model
soci.gender.constr <- sem(soci.latent.script, 
                          data = data, 
                          missing = 'fiml',
                          estimator = 'mlr',
                          group = 'gender',
                          group.equal = c('loadings',
                                          'means',
                                          'lv.variances',
                                          'lv.covariances'))

# model summary
summary(soci.gender.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# fit model
soci.gender.freely <- sem(soci.latent.script, 
                          data = data, 
                          missing = 'fiml',
                          estimator = 'mlr',
                          group = 'gender',
                          group.equal = c('loadings'))

# model summary
summary(soci.gender.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.univariate.gender.results(soci.gender.constr,
                                 soci.gender.freely,
                                 'latent',
                                 1:4)

## model comparisons

# compare model fits
summary(compareFit(soci.gender.constr, soci.gender.freely)) # freely fits better

# update summary
univariate[univariate$var == 'soci', 'gender'] <- 'freely'

## plot

# create average plots
soci.male.plot <- plot.average.gender(soci.gender.freely, 1:4, 'male')
soci.female.plot <- plot.average.gender(soci.gender.freely, 1:4, 'female')

# combined (male + female)
soci.gender.plot <- plot.combined.gender(soci.gender.freely,
                                         soci.male.average.data,
                                         soci.female.average.data,
                                         1:4)

# view plot
print(soci.gender.plot)

# if looks good, save plot
ggsave(filename = here('Output/Sensitivity/Analysis 2/Plots/soci_gender.png'), 
       plot = soci.gender.plot,
       width = 6, height = 4.5, scale = 1)

# clean environment
rm(list = ls(pattern = '^soci\\.|rows|^time'))

## physical health #############################################################

# script
phys.quadra.script <- quadra.script('phys', 1:4)
cat(phys.quadra.script)

## constrained model

# fit model
phys.gender.constr <- sem(phys.quadra.script, 
                          data = data, 
                          missing = 'fiml',
                          estimator = 'mlr',
                          group = 'gender',
                          group.equal = c('loadings',
                                          'means',
                                          'lv.variances',
                                          'lv.covariances'))

# model summary
summary(phys.gender.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# fit model
phys.gender.freely <- sem(phys.quadra.script, 
                          data = data, 
                          missing = 'fiml',
                          estimator = 'mlr',
                          group = 'gender',
                          group.equal = c('loadings')) # non-positive definite

# inspect
summary(phys.gender.freely)
lavInspect(phys.gender.freely, 'cov.lv') # cov(phys.s, phys.q) for female is out of bounds

# rewrite script
cat(phys.quadra.script)
phys.quadra.script <- '# intercept loadings
                       phys.i =~ 1*phys.1
                       phys.i =~ 1*phys.2
                       phys.i =~ 1*phys.3
                       phys.i =~ 1*phys.4

                       # linear change loadings
                       phys.s  =~ 0*phys.1
                       phys.s  =~ 0.5*phys.2
                       phys.s  =~ 1*phys.3
                       phys.s  =~ 1.5*phys.4

                       # quadratic change loadings
                       phys.q  =~ 0*phys.1
                       phys.q  =~ 0.25*phys.2
                       phys.q  =~ 1*phys.3
                       phys.q  =~ 2.25*phys.4

                       # latent means
                       phys.i ~ 1
                       phys.s ~ 1
                       phys.q ~ 1

                       # latent variances
                       phys.i ~~ phys.i
                       phys.s ~~ phys.s
                       phys.q ~~ .001*phys.q # fixed to smallest positive number

                       # latent covariances
                       phys.i ~~ phys.s
                       phys.i ~~ .001*phys.q  # fixed to smallest positive number
                       phys.s ~~ .001*phys.q  # fixed to smallest positive number

                       # constrain means to 0
                       phys.1 ~ 0*1
                       phys.2 ~ 0*1
                       phys.3 ~ 0*1
                       phys.4 ~ 0*1

                       # constrain variances
                       phys.1 ~~ phys.theta*phys.1
                       phys.2 ~~ phys.theta*phys.2
                       phys.3 ~~ phys.theta*phys.3
                       phys.4 ~~ phys.theta*phys.4'

## re-fit models

## constrained model

# fit model
phys.gender.constr <- sem(phys.quadra.script, 
                          data = data, 
                          missing = 'fiml',
                          estimator = 'mlr',
                          group = 'gender',
                          group.equal = c('loadings',
                                          'means',
                                          'lv.variances',
                                          'lv.covariances'))

# model summary
summary(phys.gender.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# fit model
phys.gender.freely <- sem(phys.quadra.script, 
                          data = data, 
                          missing = 'fiml',
                          estimator = 'mlr',
                          group = 'gender',
                          group.equal = c('loadings')) # non-positive definite

# model summary
summary(phys.gender.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.univariate.gender.results(phys.gender.constr,
                                 phys.gender.freely,
                                 'quadra',
                                 1:4)

## model comparisons

# compare model fits
summary(compareFit(phys.gender.constr, phys.gender.freely)) # freely fits better

# update summary
univariate[univariate$var == 'phys', 'gender'] <- 'freely'

## plot

# create average plots
phys.male.plot <- plot.average.gender(phys.gender.freely, 1:4, 'male')
phys.female.plot <- plot.average.gender(phys.gender.freely, 1:4, 'female')

# combined (male + female)
phys.gender.plot <- plot.combined.gender(phys.gender.freely,
                                         phys.male.average.data,
                                         phys.female.average.data,
                                         1:4)

# view plot
print(phys.gender.plot)

# if looks good, save plot
ggsave(filename = here('Output/Sensitivity/Analysis 2/Plots/phys_gender.png'), 
       plot = phys.gender.plot,
       width = 6, height = 4.5, scale = 1)

# clean environment
rm(list = ls(pattern = '^phys\\.|rows|^time'))

## hedonism ####################################################################

# script
hedo.quadra.script <- quadra.script('hedo', 1:4)
cat(hedo.quadra.script)

## constrained model

# fit model
hedo.gender.constr <- sem(hedo.quadra.script, 
                          data = data, 
                          missing = 'fiml',
                          estimator = 'mlr',
                          group = 'gender',
                          group.equal = c('loadings',
                                          'means',
                                          'lv.variances',
                                          'lv.covariances'))

# model summary
summary(hedo.gender.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# fit model
hedo.gender.freely <- sem(hedo.quadra.script, 
                          data = data, 
                          missing = 'fiml',
                          estimator = 'mlr',
                          group = 'gender',
                          group.equal = c('loadings')) # non-positive definite

# inspect
summary(hedo.gender.freely)
lavInspect(hedo.gender.freely, 'cov.lv') # cov(hedo.i, hedo.q) for female is out of bounds

# rewrite script
cat(hedo.quadra.script)
hedo.quadra.script <- '# intercept loadings
                       hedo.i =~ 1*hedo.1
                       hedo.i =~ 1*hedo.2
                       hedo.i =~ 1*hedo.3
                       hedo.i =~ 1*hedo.4

                       # linear change loadings
                       hedo.s  =~ 0*hedo.1
                       hedo.s  =~ 0.5*hedo.2
                       hedo.s  =~ 1*hedo.3
                       hedo.s  =~ 1.5*hedo.4

                       # quadratic change loadings
                       hedo.q  =~ 0*hedo.1
                       hedo.q  =~ 0.25*hedo.2
                       hedo.q  =~ 1*hedo.3
                       hedo.q  =~ 2.25*hedo.4

                       # latent means
                       hedo.i ~ 1
                       hedo.s ~ 1
                       hedo.q ~ 1

                       # latent variances
                       hedo.i ~~ hedo.i
                       hedo.s ~~ hedo.s
                       hedo.q ~~ hedo.q

                       # latent covariances
                       hedo.i ~~ hedo.s
                       hedo.i ~~ .001*hedo.q # fixed to smallest positive number
                       hedo.s ~~ hedo.q

                       # constrain means to 0
                       hedo.1 ~ 0*1
                       hedo.2 ~ 0*1
                       hedo.3 ~ 0*1
                       hedo.4 ~ 0*1

                       # constrain variances
                       hedo.1 ~~ hedo.theta*hedo.1
                       hedo.2 ~~ hedo.theta*hedo.2
                       hedo.3 ~~ hedo.theta*hedo.3
                       hedo.4 ~~ hedo.theta*hedo.4'

## re-fit models

## constrained model

# fit model
hedo.gender.constr <- sem(hedo.quadra.script, 
                          data = data, 
                          missing = 'fiml',
                          estimator = 'mlr',
                          group = 'gender',
                          group.equal = c('loadings',
                                          'means',
                                          'lv.variances',
                                          'lv.covariances'))

# model summary
summary(hedo.gender.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# fit model
hedo.gender.freely <- sem(hedo.quadra.script, 
                          data = data, 
                          missing = 'fiml',
                          estimator = 'mlr',
                          group = 'gender',
                          group.equal = c('loadings'))

# model summary
summary(hedo.gender.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.univariate.gender.results(hedo.gender.constr,
                                 hedo.gender.freely,
                                 'quadra',
                                 1:4)

## model comparisons

# compare model fits
summary(compareFit(hedo.gender.constr, hedo.gender.freely)) # freely fits better

# update summary
univariate[univariate$var == 'hedo', 'gender'] <- 'freely'

## plot

# create average plots
hedo.male.plot <- plot.average.gender(hedo.gender.freely, 1:4, 'male')
hedo.female.plot <- plot.average.gender(hedo.gender.freely, 1:4, 'female')

# combined (male + female)
hedo.gender.plot <- plot.combined.gender(hedo.gender.freely,
                                         hedo.male.average.data,
                                         hedo.female.average.data,
                                         1:4)

# view plot
print(hedo.gender.plot)

# if looks good, save plot
ggsave(filename = here('Output/Sensitivity/Analysis 2/Plots/hedo_gender.png'), 
       plot = hedo.gender.plot,
       width = 6, height = 4.5, scale = 1)

# clean environment
rm(list = ls(pattern = '^hedo\\.|rows|^time'))

# sensitivity analysis. gender differences (pt2. bivariate) ####################

# initiate df for logging best-fitting bivariate models
bivariate <- data.frame(var = c('extr', 'agre', 'cons', 'neur', 'open'),
                        'pros' = NA,
                        'fame' = NA,
                        'attr' = NA,
                        'hedo' = NA,
                        'pers' = NA,
                        'phys' = NA,
                        'rela' = NA,
                        'soci' = NA,
                        'libe_predicting' = NA,
                        'secu_predicting' = NA,
                        'secu_predicted' = NA,
                        'conf_predicting' = NA,
                        'conf_predicted' = NA,
                        'reli_predicting' = NA,
                        'reli_predicted' = NA,
                         check.names = FALSE)

#> note. e.g.,
#> secu_predicting could mean secu predicting open, and
#> secu_predicted  could mean secu predicted by open.

## extraversion ################################################################

## prosperity

## constrained model

# script
extr.pros.script <- bivariate.linear.script('extr', 'pros', 1:8, FALSE)
cat(extr.pros.script)

# fit model
extr.pros.constr <- sem(extr.pros.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances'))

# model summary
summary(extr.pros.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# script
extr.pros.script <- bivariate.linear.script('extr', 'pros', 1:8, TRUE)
cat(extr.pros.script)

# fit model
extr.pros.freely <- sem(extr.pros.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances'))

# model summary
summary(extr.pros.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.bivariate.gender.results(extr.pros.constr,
                                extr.pros.freely,
                                1:8)

## model comparisons

# compare model fits
summary(compareFit(extr.pros.constr, extr.pros.freely)) # freely fits better

# update summary
bivariate[bivariate$var == 'extr', 'pros'] <- 'freely'

# # wald test for extr.i ~~ pros.i
# lavTestWald(extr.pros.freely,
#             constraints = 'female.ii - male.ii == 0') # no significant difference
# 
# ## plot
# 
# # extr.q ~~ pros.i
# extr.pros.qi.female.plot <- plot.slope.level(extr.pros.freely,
#                                              'female',
#                                              1:8,
#                                              'Extraversion', '.q',
#                                              'Prosperity')
# 
# # view plot
# print(extr.pros.qi.female.plot)
# 
# # if looks good, save plot
# ggsave(filename = here('Output/Sensitivity/Analysis 3/Plots/extr_pros_qi_female.png'), 
#        plot = extr.pros.qi.female.plot,
#        width = 6, height = 4.5, scale = 1.5)

# clean environment
rm(list = ls(pattern = '.extr'))

## fame

## constrained model

# script
extr.fame.script <- bivariate.linear.script('extr', 'fame', 1:8, FALSE)
cat(extr.fame.script)

# fit model
extr.fame.constr <- sem(extr.fame.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances'))

# model summary
summary(extr.fame.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# script
extr.fame.script <- bivariate.linear.script('extr', 'fame', 1:8, TRUE)
cat(extr.fame.script)

# fit model
extr.fame.freely <- sem(extr.fame.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances'))

# model summary
summary(extr.fame.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.bivariate.gender.results(extr.fame.constr,
                                extr.fame.freely,
                                1:8)

## model comparisons

# compare model fits
summary(compareFit(extr.fame.constr, extr.fame.freely)) # freely fits better

# update summary
bivariate[bivariate$var == 'extr', 'fame'] <- 'freely'

# # wald test for extr.i ~~ fame.i
# lavTestWald(extr.fame.freely,
#             constraints = 'female.ii - male.ii == 0') # significant difference
# 
# ## plot
# 
# ## female
# 
# # extr.q ~~ fame.i
# extr.fame.qi.female.plot <- plot.slope.level(extr.fame.freely,
#                                              'female',
#                                              1:4,
#                                              'Extraversion', '.q',
#                                              'Fame')
# 
# # view plot
# print(extr.fame.qi.female.plot)
# 
# # if looks good, save plot
# ggsave(filename = here('Output/Sensitivity/Analysis 3/Plots/extr_fame_qi_female.png'), 
#        plot = extr.fame.qi.female.plot,
#        width = 6, height = 4.5, scale = 1.5)
# 
# ## male
# 
# # extr.i ~~ fame.q
# extr.fame.iq.male.plot <- plot.level.slope(extr.fame.freely,
#                                            'male',
#                                            1:4,
#                                            'Extraversion',
#                                            'Fame', '.q')
# 
# # view plot
# print(extr.fame.iq.male.plot)
# 
# # if looks good, save plot
# ggsave(filename = here('Output/Sensitivity/Analysis 3/Plots/extr_fame_iq_male.png'), 
#        plot = extr.fame.iq.male.plot,
#        width = 6, height = 4.5, scale = 1.5)

# clean environment
rm(list = ls(pattern = '.extr'))

## attractiveness

## constrained model

# script
extr.attr.script <- bivariate.linear.script('extr', 'attr', 1:8, FALSE)
cat(extr.attr.script)

# fit model
extr.attr.constr <- sem(extr.attr.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances'))

# model summary
summary(extr.attr.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# script
extr.attr.script <- bivariate.linear.script('extr', 'attr', 1:8, TRUE)
cat(extr.attr.script)

# fit model
extr.attr.freely <- sem(extr.attr.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances'))

# model summary
summary(extr.attr.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.bivariate.gender.results(extr.attr.constr,
                                extr.attr.freely,
                                1:8)

## model comparisons

# compare model fits
summary(compareFit(extr.attr.constr, extr.attr.freely)) # freely fits better

# update summary
bivariate[bivariate$var == 'extr', 'attr'] <- 'freely'

# # wald test for extr.i ~~ attr.i
# lavTestWald(extr.attr.freely,
#             constraints = 'female.ii - male.ii == 0') # significant difference
# 
# ## plot
# 
# ## female
# 
# # extr.q ~~ attr.i
# extr.attr.qi.female.plot <- plot.slope.level(extr.attr.freely,
#                                              'female',
#                                              1:8,
#                                              'Extraversion', '.q',
#                                              'Attractiveness')
# 
# # view plot
# print(extr.attr.qi.female.plot)
# 
# # if looks good, save plot
# ggsave(filename = here('Output/Sensitivity/Analysis 3/Plots/extr_attr_qi_female.png'), 
#        plot = extr.attr.qi.female.plot,
#        width = 6, height = 4.5, scale = 1.5)
# 
# ## male
# 
# # extr.i ~~ attr.l 
# extr.attr.il.male.plot <- plot.level.slope(extr.attr.freely,
#                                            'male',
#                                            1:8,
#                                            'Extraversion',
#                                            'Attractiveness', '.l')
# 
# # view plot
# print(extr.attr.il.male.plot)
# 
# # if looks good, save plot
# ggsave(filename = here('Output/Sensitivity/Analysis 3/Plots/extr_attr_il_male.png'), 
#        plot = extr.attr.il.male.plot,
#        width = 6, height = 4.5, scale = 1.5)

# clean environment
rm(list = ls(pattern = '.extr'))

## personal growth

## constrained model

# script
extr.pers.script <- bivariate.linear.script('extr', 'pers', 1:4, FALSE)
cat(extr.pers.script)

# fit model
extr.pers.constr <- sem(extr.pers.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances'))

# model summary
summary(extr.pers.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# script
extr.pers.script <- bivariate.linear.script('extr', 'pers', 1:4, TRUE)
cat(extr.pers.script)

# fit model
extr.pers.freely <- sem(extr.pers.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances'))

# model summary
summary(extr.pers.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.bivariate.gender.results(extr.pers.constr,
                                extr.pers.freely,
                                1:4)

## model comparisons

# compare model fits
summary(compareFit(extr.pers.constr, extr.pers.freely)) # freely fits better

# update summary
bivariate[bivariate$var == 'extr', 'pers'] <- 'freely'

# # wald test for open.i ~~ pros.i
# lavTestWald(extr.pers.freely,
#             constraints = 'female.ii - male.ii == 0') # no significant difference

# clean environment
rm(list = ls(pattern = '.extr'))

## relationship

## constrained model

# script
extr.rela.script <- bivariate.linear.script('extr', 'rela', 1:4, FALSE)
cat(extr.rela.script)

# fit model
extr.rela.constr <- sem(extr.rela.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances'))

# model summary
summary(extr.rela.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# script
extr.rela.script <- bivariate.linear.script('extr', 'rela', 1:4, TRUE)
cat(extr.rela.script)

# fit model
extr.rela.freely <- sem(extr.rela.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances'))

# model summary
summary(extr.rela.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.bivariate.gender.results(extr.rela.constr,
                                extr.rela.freely,
                                1:4)

## model comparisons

# compare model fits
summary(compareFit(extr.rela.constr, extr.rela.freely)) # freely fits better

# update summary
bivariate[bivariate$var == 'extr', 'rela'] <- 'freely'

# clean environment
rm(list = ls(pattern = '.extr'))

## society

## constrained model

# script
extr.soci.script <- bivariate.linear.script('extr', 'soci', 1:4, FALSE)
cat(extr.soci.script)

# fit model
extr.soci.constr <- sem(extr.soci.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances'))

# model summary
summary(extr.soci.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# script
extr.soci.script <- bivariate.linear.script('extr', 'soci', 1:4, TRUE)
cat(extr.soci.script)

# fit model
extr.soci.freely <- sem(extr.soci.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances'))

# model summary
summary(extr.soci.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.bivariate.gender.results(extr.soci.constr,
                                extr.soci.freely,
                                1:4)

## model comparisons

# compare model fits
summary(compareFit(extr.soci.constr, extr.soci.freely)) # freely fits better

# update summary
bivariate[bivariate$var == 'extr', 'soci'] <- 'freely'

# clean environment
rm(list = ls(pattern = '.extr'))

## physical health

## constrained model

# script
extr.phys.script <- bivariate.linear.script('extr', 'phys', 1:4, FALSE)
cat(extr.phys.script)

# fit model
extr.phys.constr <- sem(extr.phys.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances'))

# model summary
summary(extr.phys.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# script
extr.phys.script <- bivariate.linear.script('extr', 'phys', 1:4, TRUE)
cat(extr.phys.script)

# fit model
extr.phys.freely <- sem(extr.phys.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances'))

# model summary
summary(extr.phys.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.bivariate.gender.results(extr.phys.constr,
                                extr.phys.freely,
                                1:4)

## model comparisons

# compare model fits
summary(compareFit(extr.phys.constr, extr.phys.freely)) # freely fits better

# update summary
bivariate[bivariate$var == 'extr', 'phys'] <- 'freely'

# # wald test for extr.i ~~ phys.i
# lavTestWald(extr.phys.freely,
#             constraints = 'female.ii - male.ii == 0') # no significant difference
# 
# ## plot
# 
# ## female
# 
# # extr.i ~~ phys.q
# extr.phys.iq.female.plot <- plot.level.slope(extr.phys.freely,
#                                              'female',
#                                              1:4,
#                                              'Extraversion',
#                                              'Physical Health', '.q')
# 
# # view plot
# print(extr.phys.iq.female.plot)
# 
# # if looks good, save plot
# ggsave(filename = here('Output/Sensitivity/Analysis 3/Plots/extr_phys_iq_female.png'), 
#        plot = extr.phys.iq.female.plot,
#        width = 6, height = 4.5, scale = 1.5)

# clean environment
rm(list = ls(pattern = '.extr'))

## hedonism

## constrained model

# script
extr.hedo.script <- bivariate.linear.script('extr', 'hedo', 1:4, FALSE)
cat(extr.hedo.script)

# fit model
extr.hedo.constr <- sem(extr.hedo.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances'))

# model summary
summary(extr.hedo.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# script
extr.hedo.script <- bivariate.linear.script('extr', 'hedo', 1:4, TRUE)
cat(extr.hedo.script)

# fit model
extr.hedo.freely <- sem(extr.hedo.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances'))

# model summary
summary(extr.hedo.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.bivariate.gender.results(extr.hedo.constr,
                                extr.hedo.freely,
                                1:4)

## model comparisons

# compare model fits
summary(compareFit(extr.hedo.constr, extr.hedo.freely)) # freely fits better

# update summary
bivariate[bivariate$var == 'extr', 'hedo'] <- 'freely'

# # wald test for extr.i ~~ hedo.i
# lavTestWald(extr.hedo.freely,
#             constraints = 'female.ii - male.ii == 0') # no significant difference
# 
# # wald test for extr.s ~~ hedo.s
# lavTestWald(extr.hedo.freely,
#             constraints = 'female.ss - male.ss == 0') # no significant difference

# clean environment
rm(list = ls(pattern = '.extr'))

## liberal values

## extraversion T1 predicting liberal values T1

## constrained model

# script
extr.libe.script <- trait.predict.linear.script('extr', 'libe', 1:8, FALSE)
cat(extr.libe.script)

# fit model
extr.libe.constr <- sem(extr.libe.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances',
                                        'regressions'))

# model summary
summary(extr.libe.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# script
extr.libe.script <- trait.predict.linear.script('extr', 'libe', 1:8, TRUE)
cat(extr.libe.script)

# fit model
extr.libe.freely <- sem(extr.libe.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances'))

# model summary
summary(extr.libe.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.gender.results(extr.libe.constr, 
                                 extr.libe.freely,
                                 'trait', 
                                 1:8)

## model comparisons

# compare model fits
summary(compareFit(extr.libe.constr, extr.libe.freely)) # freely fits better

# update summary
bivariate[bivariate$var == 'extr', 'libe_predicted'] <- 'freely'

# clean environment
rm(list = ls(pattern = '.extr'))

## liberal values T1 predicting extraversion T1-8

## constrained model

# script
libe.extr.script <- goal.predict.linear.script('libe', 'extr', 1:8, FALSE)
cat(libe.extr.script)

# fit model
libe.extr.constr <- sem(libe.extr.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances',
                                        'regressions'))

# model summary
summary(libe.extr.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# script
libe.extr.script <- goal.predict.linear.script('libe', 'extr', 1:8, TRUE)
cat(libe.extr.script)

# fit model
libe.extr.freely <- sem(libe.extr.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances'))

# model summary
summary(libe.extr.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.gender.results(libe.extr.constr, 
                                 libe.extr.freely,
                                 'goal', 
                                 1:8)

## model comparisons

# compare model fits
summary(compareFit(libe.extr.constr, libe.extr.freely)) # freely fits better

# update summary
bivariate[bivariate$var == 'extr', 'libe_predicting'] <- 'freely'

# clean environment
rm(list = ls(pattern = '.extr'))

## security

## extraversion T1-4 predicting security T3/T4

## constrained model

# script
extr.secu.script <- trait.predict.linear.script('extr', 'secu', 1:4, FALSE)
cat(extr.secu.script)

# fit model
extr.secu.constr <- sem(extr.secu.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances',
                                        'regressions'))

# model summary
summary(extr.secu.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# script
extr.secu.script <- trait.predict.linear.script('extr', 'secu', 1:4, TRUE)
cat(extr.secu.script)

# fit model
extr.secu.freely <- sem(extr.secu.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances'))

# model summary
summary(extr.secu.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.gender.results(extr.secu.constr, 
                                 extr.secu.freely,
                                 'trait', 
                                 1:4)

## model comparisons

# compare model fits
summary(compareFit(extr.secu.constr, extr.secu.freely)) # freely fits better

# update summary
bivariate[bivariate$var == 'extr', 'secu_predicted'] <- 'freely'

# clean environment
rm(list = ls(pattern = '.extr'))

## security T3/T4 predicting extraversion T4-8

## constrained model

# script
secu.extr.script <- goal.predict.linear.script('secu', 'extr', 4:8, FALSE)
cat(secu.extr.script)

# fit model
secu.extr.constr <- sem(secu.extr.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances',
                                        'regressions'))

# model summary
summary(secu.extr.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# script
secu.extr.script <- goal.predict.linear.script('secu', 'extr', 4:8, TRUE)
cat(secu.extr.script)

# fit model
secu.extr.freely <- sem(secu.extr.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances'))

# model summary
summary(secu.extr.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.gender.results(secu.extr.constr, 
                                 secu.extr.freely,
                                 'goal', 
                                 4:8)

## model comparisons

# compare model fits
summary(compareFit(secu.extr.constr, secu.extr.freely)) # freely fits better

# update summary
bivariate[bivariate$var == 'extr', 'secu_predicting'] <- 'freely'

# clean environment
rm(list = ls(pattern = '.extr'))

## conformism

## extraversion T1-4 predicting conformism T3/T4

## constrained model

# script
extr.conf.script <- trait.predict.linear.script('extr', 'conf', 1:4, FALSE)
cat(extr.conf.script)

# fit model
extr.conf.constr <- sem(extr.conf.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances',
                                        'regressions'))

# model summary
summary(extr.conf.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# script
extr.conf.script <- trait.predict.linear.script('extr', 'conf', 1:4, TRUE)
cat(extr.conf.script)

# fit model
extr.conf.freely <- sem(extr.conf.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances'))

# model summary
summary(extr.conf.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.gender.results(extr.conf.constr, 
                                 extr.conf.freely,
                                 'trait', 
                                 1:4)

## model comparisons

# compare model fits
summary(compareFit(extr.conf.constr, extr.conf.freely)) # freely fits better

# update summary
bivariate[bivariate$var == 'extr', 'conf_predicted'] <- 'freely'

# clean environment
rm(list = ls(pattern = '.extr'))

## conformism T3/T4 predicting extraversion T4-8

## constrained model

# script
conf.extr.script <- goal.predict.linear.script('conf', 'extr', 4:8, FALSE)
cat(conf.extr.script)

# fit model
conf.extr.constr <- sem(conf.extr.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances',
                                        'regressions'))

# model summary
summary(conf.extr.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# script
conf.extr.script <- goal.predict.linear.script('conf', 'extr', 4:8, TRUE)
cat(conf.extr.script)

# fit model
conf.extr.freely <- sem(conf.extr.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances'))

# model summary
summary(conf.extr.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.gender.results(conf.extr.constr, 
                                 conf.extr.freely,
                                 'goal', 
                                 4:8)

## model comparisons

# compare model fits
summary(compareFit(conf.extr.constr, conf.extr.freely)) # freely fits better

# update summary
bivariate[bivariate$var == 'extr', 'conf_predicting'] <- 'freely'

# clean environment
rm(list = ls(pattern = '.extr'))

## religiosity

## extraversion T1-4 predicting religiosity T3/T4

## constrained model

# script
extr.reli.script <- trait.predict.linear.script('extr', 'reli', 1:4, FALSE)
cat(extr.reli.script)

# fit model
extr.reli.constr <- sem(extr.reli.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances',
                                        'regressions'))

# model summary
summary(extr.reli.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# script
extr.reli.script <- trait.predict.linear.script('extr', 'reli', 1:4, TRUE)
cat(extr.reli.script)

# fit model
extr.reli.freely <- sem(extr.reli.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances'))

# model summary
summary(extr.reli.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.gender.results(extr.reli.constr, 
                                 extr.reli.freely,
                                 'trait', 
                                 1:4)

## model comparisons

# compare model fits
summary(compareFit(extr.reli.constr, extr.reli.freely)) # freely fits better

# update summary
bivariate[bivariate$var == 'extr', 'reli_predicted'] <- 'freely'

# clean environment
rm(list = ls(pattern = '.extr'))

## religiosity T3/T4 predicting extraversion T4-8

## constrained model

# script
reli.extr.script <- goal.predict.linear.script('reli', 'extr', 4:8, FALSE)
cat(reli.extr.script)

# fit model
reli.extr.constr <- sem(reli.extr.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances',
                                        'regressions'))

# model summary
summary(reli.extr.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# script
reli.extr.script <- goal.predict.linear.script('reli', 'extr', 4:8, TRUE)
cat(reli.extr.script)

# fit model
reli.extr.freely <- sem(reli.extr.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances'))

# model summary
summary(reli.extr.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.gender.results(reli.extr.constr, 
                                 reli.extr.freely,
                                 'goal', 
                                 4:8)

## model comparisons

# compare model fits
summary(compareFit(reli.extr.constr, reli.extr.freely)) # freely fits better

# update summary
bivariate[bivariate$var == 'extr', 'reli_predicting'] <- 'freely'

# clean environment
rm(list = ls(pattern = '.extr'))

## agreeableness ###############################################################

## prosperity

## constrained model

# script
agre.pros.script <- bivariate.linear.script('agre', 'pros', 1:8, FALSE)
cat(agre.pros.script)

# fit model
agre.pros.constr <- sem(agre.pros.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances'))

# model summary
summary(agre.pros.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# script
agre.pros.script <- bivariate.linear.script('agre', 'pros', 1:8, TRUE)
cat(agre.pros.script)

# fit model
agre.pros.freely <- sem(agre.pros.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances'))

# model summary
summary(agre.pros.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.bivariate.gender.results(agre.pros.constr,
                                agre.pros.freely,
                                1:8)

## model comparisons

# compare model fits
summary(compareFit(agre.pros.constr, agre.pros.freely)) # freely fits better

# update summary
bivariate[bivariate$var == 'agre', 'pros'] <- 'freely'

# # wald test for agre.i ~~ pros.i
# lavTestWald(agre.pros.freely,
#             constraints = 'female.ii - male.ii == 0') # no significant difference

# ## plot
# 
# ## female
# 
# # agre.i ~~ pros.q
# agre.pros.iq.female.plot <- plot.level.slope(agre.pros.freely,
#                                              'female',
#                                              1:8,
#                                              'Agreeableness',
#                                              'Prosperity', '.q')
# 
# # view plot
# print(agre.pros.iq.female.plot)
# 
# # if looks good, save plot
# ggsave(filename = here('Output/Sensitivity/Analysis 3/Plots/agre_pros_iq_female.png'), 
#        plot = agre.pros.iq.female.plot,
#        width = 6, height = 4.5, scale = 1.5)

# clean environment
rm(list = ls(pattern = '.agre'))

## fame

## constrained model

# script
agre.fame.script <- bivariate.linear.script('agre', 'fame', 1:8, FALSE)
cat(agre.fame.script)

# fit model
agre.fame.constr <- sem(agre.fame.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances'))

# model summary
summary(agre.fame.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# script
agre.fame.script <- bivariate.linear.script('agre', 'fame', 1:8, TRUE)
cat(agre.fame.script)

# fit model
agre.fame.freely <- sem(agre.fame.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances'))

# model summary
summary(agre.fame.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.bivariate.gender.results(agre.fame.constr,
                                agre.fame.freely,
                                1:8)

## model comparisons

# compare model fits
summary(compareFit(agre.fame.constr, agre.fame.freely)) # freely fits better

# update summary
bivariate[bivariate$var == 'agre', 'fame'] <- 'freely'

# # wald test for agre.i ~~ fame.i
# lavTestWald(agre.fame.freely,
#             constraints = 'female.ii - male.ii == 0') # no significant difference
# 
# # wald test for agre.l ~~ fame.s
# lavTestWald(agre.fame.freely,
#             constraints = 'female.ls - male.ls == 0') # no significant difference
# 
# ## plot
# 
# ## female
# 
# # agre.l ~~ fame.i
# agre.fame.li.female.plot <- plot.slope.level(agre.fame.freely,
#                                              'female',
#                                              1:8,
#                                              'Agreeableness', '.l',
#                                              'Fame')
# 
# # view plot
# print(agre.fame.li.female.plot)
# 
# # if looks good, save plot
# ggsave(filename = here('Output/Sensitivity/Analysis 3/Plots/agre_fame_li_female.png'), 
#        plot = agre.fame.li.female.plot,
#        width = 6, height = 4.5, scale = 1.5)

# clean environment
rm(list = ls(pattern = '.agre'))

## attractiveness

## constrained model

# script
agre.attr.script <- bivariate.linear.script('agre', 'attr', 1:8, FALSE)
cat(agre.attr.script)

# fit model
agre.attr.constr <- sem(agre.attr.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances'))

# model summary
summary(agre.attr.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# script
agre.attr.script <- bivariate.linear.script('agre', 'attr', 1:8, TRUE)
cat(agre.attr.script)

# fit model
agre.attr.freely <- sem(agre.attr.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances'))

# model summary
summary(agre.attr.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.bivariate.gender.results(agre.attr.constr,
                                agre.attr.freely,
                                1:8)

## model comparisons

# compare model fits
summary(compareFit(agre.attr.constr, agre.attr.freely)) # freely fits better

# update summary
bivariate[bivariate$var == 'agre', 'attr'] <- 'freely'

# ## plot
# 
# ## female
# 
# # agre.i ~~ attr.l
# agre.attr.il.female.plot <- plot.level.slope(agre.attr.freely,
#                                              'female',
#                                              1:8,
#                                              'Agreeableness',
#                                              'Attractiveness', '.l')
# 
# # view plot
# print(agre.attr.il.female.plot)
# 
# # if looks good, save plot
# ggsave(filename = here('Output/Sensitivity/Analysis 3/Plots/agre_attr_il_female.png'), 
#        plot = agre.attr.il.female.plot,
#        width = 6, height = 4.5, scale = 1.5)
# 
# # agre.l ~~ attr.i
# agre.attr.li.female.plot <- plot.slope.level(agre.attr.freely,
#                                              'female',
#                                              1:8,
#                                              'Agreeableness', '.l',
#                                              'Attractiveness')
# 
# # view plot
# print(agre.attr.li.female.plot)
# 
# # if looks good, save plot
# ggsave(filename = here('Output/Sensitivity/Analysis 3/Plots/agre_attr_li_female.png'), 
#        plot = agre.attr.li.female.plot,
#        width = 6, height = 4.5, scale = 1.5)

# clean environment
rm(list = ls(pattern = '.agre'))

## personal growth

## constrained model

# script
agre.pers.script <- bivariate.linear.script('agre', 'pers', 1:4, FALSE)
cat(agre.pers.script)

# fit model
agre.pers.constr <- sem(agre.pers.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances'))

# model summary
summary(agre.pers.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# script
agre.pers.script <- bivariate.linear.script('agre', 'pers', 1:4, TRUE)
cat(agre.pers.script)

# fit model
agre.pers.freely <- sem(agre.pers.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances'))

# model summary
summary(agre.pers.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.bivariate.gender.results(agre.pers.constr,
                                agre.pers.freely,
                                1:4)

## model comparisons

# compare model fits
summary(compareFit(agre.pers.constr, agre.pers.freely)) # freely fits better

# update summary
bivariate[bivariate$var == 'agre', 'pers'] <- 'freely'

# ## plot
# 
# ## male
# 
# # agre.i ~~ pers.l
# agre.pers.il.male.plot <- plot.level.slope(agre.pers.freely,
#                                            'male',
#                                            1:4,
#                                            'Agreeableness', 
#                                            'Personal Growth', '.l')
# 
# # view plot
# print(agre.pers.il.male.plot)
# 
# # if looks good, save plot
# ggsave(filename = here('Output/Sensitivity/Analysis 3/Plots/agre_pers_il_male.png'), 
#        plot = agre.pers.il.male.plot,
#        width = 6, height = 4.5, scale = 1.5)

# clean environment
rm(list = ls(pattern = '.agre'))

## relationship

## constrained model

# script
agre.rela.script <- bivariate.linear.script('agre', 'rela', 1:4, FALSE)
cat(agre.rela.script)

# fit model
agre.rela.constr <- sem(agre.rela.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances'))

# model summary
summary(agre.rela.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# script
agre.rela.script <- bivariate.linear.script('agre', 'rela', 1:4, TRUE)
cat(agre.rela.script)

# fit model
agre.rela.freely <- sem(agre.rela.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances'))

# model summary
summary(agre.rela.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.bivariate.gender.results(agre.rela.constr,
                                agre.rela.freely,
                                1:4)

## model comparisons

# compare model fits
summary(compareFit(agre.rela.constr, agre.rela.freely)) # freely fits better

# update summary
bivariate[bivariate$var == 'agre', 'rela'] <- 'freely'

# clean environment
rm(list = ls(pattern = '.agre'))

## society

## constrained model

# script
agre.soci.script <- bivariate.linear.script('agre', 'soci', 1:4, FALSE)
cat(agre.soci.script)

# fit model
agre.soci.constr <- sem(agre.soci.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances'))

# model summary
summary(agre.soci.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# script
agre.soci.script <- bivariate.linear.script('agre', 'soci', 1:4, TRUE)
cat(agre.soci.script)

# fit model
agre.soci.freely <- sem(agre.soci.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances'))

# model summary
summary(agre.soci.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.bivariate.gender.results(agre.soci.constr,
                                agre.soci.freely,
                                1:4)

## model comparisons

# compare model fits
summary(compareFit(agre.soci.constr, agre.soci.freely)) # freely fits better

# update summary
bivariate[bivariate$var == 'agre', 'soci'] <- 'freely'

# # wald test for agre.i ~~ soci.i
# lavTestWald(agre.soci.freely,
#             constraints = 'female.ii - male.ii == 0') # no significant difference
# 
# # wald test for agre.l ~~ soci.l
# lavTestWald(agre.soci.freely,
#             constraints = 'female.ll - male.ll == 0') # no significant difference

# clean environment
rm(list = ls(pattern = '.agre'))

## physical health

## constrained model

# script
agre.phys.script <- bivariate.linear.script('agre', 'phys', 1:4, FALSE)
cat(agre.phys.script)

# fit model
agre.phys.constr <- sem(agre.phys.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances'))

# model summary
summary(agre.phys.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# script
agre.phys.script <- bivariate.linear.script('agre', 'phys', 1:4, TRUE)
cat(agre.phys.script)

# fit model
agre.phys.freely <- sem(agre.phys.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances'))

# model summary
summary(agre.phys.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.bivariate.gender.results(agre.phys.constr,
                                agre.phys.freely,
                                1:4)

## model comparisons

# compare model fits
summary(compareFit(agre.phys.constr, agre.phys.freely)) # freely fits better

# update summary
bivariate[bivariate$var == 'agre', 'phys'] <- 'freely'

# # wald test for agre.i ~~ phys.i
# lavTestWald(agre.phys.freely,
#             constraints = 'female - male == 0') # no significant difference

# clean environment
rm(list = ls(pattern = '.agre'))

## hedonism

## constrained model

# script
agre.hedo.script <- bivariate.linear.script('agre', 'hedo', 1:4, FALSE)
cat(agre.hedo.script)

# fit model
agre.hedo.constr <- sem(agre.hedo.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances'))

# model summary
summary(agre.hedo.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# script
agre.hedo.script <- bivariate.linear.script('agre', 'hedo', 1:4, TRUE)
cat(agre.hedo.script)

# fit model
agre.hedo.freely <- sem(agre.hedo.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances'))

# model summary
summary(agre.hedo.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.bivariate.gender.results(agre.hedo.constr,
                                agre.hedo.freely,
                                1:4)

## model comparisons

# compare model fits
summary(compareFit(agre.hedo.constr, agre.hedo.freely)) # freely fits better

# update summary
bivariate[bivariate$var == 'agre', 'hedo'] <- 'freely'

# clean environment
rm(list = ls(pattern = '.agre'))

## liberal values

## agreeableness T1 predicting liberal values T1

## constrained model

# script
agre.libe.script <- trait.predict.linear.script('agre', 'libe', 1:8, FALSE)
cat(agre.libe.script)

# fit model
agre.libe.constr <- sem(agre.libe.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances',
                                        'regressions'))

# model summary
summary(agre.libe.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# script
agre.libe.script <- trait.predict.linear.script('agre', 'libe', 1:8, TRUE)
cat(agre.libe.script)

# fit model
agre.libe.freely <- sem(agre.libe.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances'))

# model summary
summary(agre.libe.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.gender.results(agre.libe.constr, 
                                 agre.libe.freely,
                                 'trait', 
                                 1:8)

## model comparisons

# compare model fits
summary(compareFit(agre.libe.constr, agre.libe.freely)) # freely fits better

# update summary
bivariate[bivariate$var == 'agre', 'libe_predicted'] <- 'freely'

# clean environment
rm(list = ls(pattern = '.agre'))

## liberal values T1 predicting agreeableness T1-8

## constrained model

# script
libe.agre.script <- goal.predict.linear.script('libe', 'agre', 1:8, FALSE)
cat(libe.agre.script)

# fit model
libe.agre.constr <- sem(libe.agre.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances',
                                        'regressions'))

# model summary
summary(libe.agre.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# script
libe.agre.script <- goal.predict.linear.script('libe', 'agre', 1:8, TRUE)
cat(libe.agre.script)

# fit model
libe.agre.freely <- sem(libe.agre.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances'))

# model summary
summary(libe.agre.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.gender.results(libe.agre.constr, 
                                 libe.agre.freely,
                                 'goal', 
                                 1:8)

## model comparisons

# compare model fits
summary(compareFit(libe.agre.constr, libe.agre.freely)) # freely fits better

# update summary
bivariate[bivariate$var == 'agre', 'libe_predicting'] <- 'freely'

# clean environment
rm(list = ls(pattern = '.agre'))

## security

## agreeableness T1-4 predicting security T3/T4

## constrained model

# script
agre.secu.script <- trait.predict.linear.script('agre', 'secu', 1:4, FALSE)
cat(agre.secu.script)

# fit model
agre.secu.constr <- sem(agre.secu.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances',
                                        'regressions'))

# model summary
summary(agre.secu.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# script
agre.secu.script <- trait.predict.linear.script('agre', 'secu', 1:4, TRUE)
cat(agre.secu.script)

# fit model
agre.secu.freely <- sem(agre.secu.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances'))

# model summary
summary(agre.secu.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.gender.results(agre.secu.constr, 
                                 agre.secu.freely,
                                 'trait', 
                                 1:4)

## model comparisons

# compare model fits
summary(compareFit(agre.secu.constr, agre.secu.freely)) # freely fits better

# update summary
bivariate[bivariate$var == 'agre', 'secu_predicted'] <- 'freely'

# clean environment
rm(list = ls(pattern = '.agre'))

## security T3/T4 predicting agreeableness T4-8

## constrained model

# script
secu.agre.script <- goal.predict.linear.script('secu', 'agre', 4:8, FALSE)
cat(secu.agre.script)

# fit model
secu.agre.constr <- sem(secu.agre.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances',
                                        'regressions'))

# model summary
summary(secu.agre.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# script
secu.agre.script <- goal.predict.linear.script('secu', 'agre', 4:8, TRUE)
cat(secu.agre.script)

# fit model
secu.agre.freely <- sem(secu.agre.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances'))

# model summary
summary(secu.agre.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.gender.results(secu.agre.constr, 
                                 secu.agre.freely,
                                 'goal', 
                                 4:8)

## model comparisons

# compare model fits
summary(compareFit(secu.agre.constr, secu.agre.freely)) # freely fits better

# update summary
bivariate[bivariate$var == 'agre', 'secu_predicting'] <- 'freely'

# clean environment
rm(list = ls(pattern = '.agre'))

## conformism

## agreeableness T1-4 predicting conformism T3/T4

## constrained model

# script
agre.conf.script <- trait.predict.linear.script('agre', 'conf', 1:4, FALSE)
cat(agre.conf.script)

# fit model
agre.conf.constr <- sem(agre.conf.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances',
                                        'regressions'))

# model summary
summary(agre.conf.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# script
agre.conf.script <- trait.predict.linear.script('agre', 'conf', 1:4, TRUE)
cat(agre.conf.script)

# fit model
agre.conf.freely <- sem(agre.conf.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances'))

# model summary
summary(agre.conf.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.gender.results(agre.conf.constr, 
                                 agre.conf.freely,
                                 'trait', 
                                 1:4)

## model comparisons

# compare model fits
summary(compareFit(agre.conf.constr, agre.conf.freely)) # freely fits better

# update summary
bivariate[bivariate$var == 'agre', 'conf_predicted'] <- 'freely'

# clean environment
rm(list = ls(pattern = '.agre'))

## conformism T3/T4 predicting agreeableness T4-8

## constrained model

# script
conf.agre.script <- goal.predict.linear.script('conf', 'agre', 4:8, FALSE)
cat(conf.agre.script)

# fit model
conf.agre.constr <- sem(conf.agre.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances',
                                        'regressions'))

# model summary
summary(conf.agre.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# script
conf.agre.script <- goal.predict.linear.script('conf', 'agre', 4:8, TRUE)
cat(conf.agre.script)

# fit model
conf.agre.freely <- sem(conf.agre.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances'))

# model summary
summary(conf.agre.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.gender.results(conf.agre.constr, 
                                 conf.agre.freely,
                                 'goal', 
                                 4:8)

## model comparisons

# compare model fits
summary(compareFit(conf.agre.constr, conf.agre.freely)) # freely fits better

# update summary
bivariate[bivariate$var == 'agre', 'conf_predicting'] <- 'freely'

# clean environment
rm(list = ls(pattern = '.agre'))

## religiosity

## agreeableness T1-4 predicting religiosity T3/T4

## constrained model

# script
agre.reli.script <- trait.predict.linear.script('agre', 'reli', 1:4, FALSE)
cat(agre.reli.script)

# fit model
agre.reli.constr <- sem(agre.reli.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances',
                                        'regressions'))

# model summary
summary(agre.reli.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# script
agre.reli.script <- trait.predict.linear.script('agre', 'reli', 1:4, TRUE)
cat(agre.reli.script)

# fit model
agre.reli.freely <- sem(agre.reli.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances'))

# model summary
summary(agre.reli.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.gender.results(agre.reli.constr, 
                                 agre.reli.freely,
                                 'trait', 
                                 1:4)

## model comparisons

# compare model fits
summary(compareFit(agre.reli.constr, agre.reli.freely)) # freely fits better

# update summary
bivariate[bivariate$var == 'agre', 'reli_predicted'] <- 'freely'

# clean environment
rm(list = ls(pattern = '.agre'))

## religiosity T3/T4 predicting agreeableness T4-8

## constrained model

# script
reli.agre.script <- goal.predict.linear.script('reli', 'agre', 4:8, FALSE)
cat(reli.agre.script)

# fit model
reli.agre.constr <- sem(reli.agre.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances',
                                        'regressions'))

# model summary
summary(reli.agre.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# script
reli.agre.script <- goal.predict.linear.script('reli', 'agre', 4:8, TRUE)
cat(reli.agre.script)

# fit model
reli.agre.freely <- sem(reli.agre.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances'))

# model summary
summary(reli.agre.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.gender.results(reli.agre.constr, 
                                 reli.agre.freely,
                                 'goal', 
                                 4:8)

## model comparisons

# compare model fits
summary(compareFit(reli.agre.constr, reli.agre.freely)) # freely fits better

# update summary
bivariate[bivariate$var == 'agre', 'reli_predicting'] <- 'freely'

# clean environment
rm(list = ls(pattern = '.agre'))

## conscientiousness ###########################################################

## prosperity

## constrained model

# script
cons.pros.script <- bivariate.linear.script('cons', 'pros', 1:8, FALSE)
cat(cons.pros.script)

# fit model
cons.pros.constr <- sem(cons.pros.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances'))

# model summary
summary(cons.pros.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# script
cons.pros.script <- bivariate.linear.script('cons', 'pros', 1:8, TRUE)
cat(cons.pros.script)

# fit model
cons.pros.freely <- sem(cons.pros.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances'))

# model summary
summary(cons.pros.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.bivariate.gender.results(cons.pros.constr,
                                cons.pros.freely,
                                1:8)

## model comparisons

# compare model fits
summary(compareFit(cons.pros.constr, cons.pros.freely)) # freely fits better

# update summary
bivariate[bivariate$var == 'cons', 'pros'] <- 'freely'

# clean environment
rm(list = ls(pattern = '.cons'))

## fame

## constrained model

# script
cons.fame.script <- bivariate.linear.script('cons', 'fame', 1:8, FALSE)
cat(cons.fame.script)

# fit model
cons.fame.constr <- sem(cons.fame.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances'))

# model summary
summary(cons.fame.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# script
cons.fame.script <- bivariate.linear.script('cons', 'fame', 1:8, TRUE)
cat(cons.fame.script)

# fit model
cons.fame.freely <- sem(cons.fame.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances'))

# model summary
summary(cons.fame.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.bivariate.gender.results(cons.fame.constr,
                                cons.fame.freely,
                                1:8)

## model comparisons

# compare model fits
summary(compareFit(cons.fame.constr, cons.fame.freely)) # freely fits better

# update summary
bivariate[bivariate$var == 'cons', 'fame'] <- 'freely'

# clean environment
rm(list = ls(pattern = '.cons'))

## attractiveness

## constrained model

# script
cons.attr.script <- bivariate.linear.script('cons', 'attr', 1:8, FALSE)
cat(cons.attr.script)

# fit model
cons.attr.constr <- sem(cons.attr.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances'))

# model summary
summary(cons.attr.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# script
cons.attr.script <- bivariate.linear.script('cons', 'attr', 1:8, TRUE)
cat(cons.attr.script)

# fit model
cons.attr.freely <- sem(cons.attr.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances'))

# model summary
summary(cons.attr.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.bivariate.gender.results(cons.attr.constr,
                                cons.attr.freely,
                                1:8)

## model comparisons

# compare model fits
summary(compareFit(cons.attr.constr, cons.attr.freely)) # constr fits better

# update summary
bivariate[bivariate$var == 'cons', 'attr'] <- 'constr'

# clean environment
rm(list = ls(pattern = '.cons'))

## personal growth

## constrained model

# script
cons.pers.script <- bivariate.linear.script('cons', 'pers', 1:4, FALSE)
cat(cons.pers.script)

# fit model
cons.pers.constr <- sem(cons.pers.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances'))

# model summary
summary(cons.pers.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# script
cons.pers.script <- bivariate.linear.script('cons', 'pers', 1:4, TRUE)
cat(cons.pers.script)

# fit model
cons.pers.freely <- sem(cons.pers.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances'))

# model summary
summary(cons.pers.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.bivariate.gender.results(cons.pers.constr,
                                cons.pers.freely,
                                1:4)

## model comparisons

# compare model fits
summary(compareFit(cons.pers.constr, cons.pers.freely)) # freely fits better

# update summary
bivariate[bivariate$var == 'cons', 'pers'] <- 'freely'

# clean environment
rm(list = ls(pattern = '.cons'))

## relationship

## constrained model

# script
cons.rela.script <- bivariate.linear.script('cons', 'rela', 1:4, FALSE)
cat(cons.rela.script)

# fit model
cons.rela.constr <- sem(cons.rela.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances'))

# model summary
summary(cons.rela.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# script
cons.rela.script <- bivariate.linear.script('cons', 'rela', 1:4, TRUE)
cat(cons.rela.script)

# fit model
cons.rela.freely <- sem(cons.rela.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances'))

# model summary
summary(cons.rela.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.bivariate.gender.results(cons.rela.constr,
                                cons.rela.freely,
                                1:4)

## model comparisons

# compare model fits
summary(compareFit(cons.rela.constr, cons.rela.freely)) # freely fits better

# update summary
bivariate[bivariate$var == 'cons', 'rela'] <- 'freely'

# clean environment
rm(list = ls(pattern = '.cons'))

## society

## constrained model

# script
cons.soci.script <- bivariate.linear.script('cons', 'soci', 1:4, FALSE)
cat(cons.soci.script)

# fit model
cons.soci.constr <- sem(cons.soci.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances'))

# model summary
summary(cons.soci.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# script
cons.soci.script <- bivariate.linear.script('cons', 'soci', 1:4, TRUE)
cat(cons.soci.script)

# fit model
cons.soci.freely <- sem(cons.soci.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances'))

# model summary
summary(cons.soci.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.bivariate.gender.results(cons.soci.constr,
                                cons.soci.freely,
                                1:4)

## model comparisons

# compare model fits
summary(compareFit(cons.soci.constr, cons.soci.freely)) # freely fits better

# update summary
bivariate[bivariate$var == 'cons', 'soci'] <- 'freely'

# clean environment
rm(list = ls(pattern = '.cons'))

## physical health

## constrained model

# script
cons.phys.script <- bivariate.linear.script('cons', 'phys', 1:4, FALSE)
cat(cons.phys.script)

# fit model
cons.phys.constr <- sem(cons.phys.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances'))

# model summary
summary(cons.phys.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# script
cons.phys.script <- bivariate.linear.script('cons', 'phys', 1:4, TRUE)
cat(cons.phys.script)

# fit model
cons.phys.freely <- sem(cons.phys.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances'))

# model summary
summary(cons.phys.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.bivariate.gender.results(cons.phys.constr,
                                cons.phys.freely,
                                1:4)

## model comparisons

# compare model fits
summary(compareFit(cons.phys.constr, cons.phys.freely)) # freely fits better

# update summary
bivariate[bivariate$var == 'cons', 'phys'] <- 'freely'

# # wald test for cons.i ~~ phys.i
# lavTestWald(cons.phys.freely,
#             constraints = 'female.ii - male.ii == 0') # no significant difference

# clean environment
rm(list = ls(pattern = '.cons'))

## hedonism

## constrained model

# script
cons.hedo.script <- bivariate.linear.script('cons', 'hedo', 1:4, FALSE)
cat(cons.hedo.script)

# fit model
cons.hedo.constr <- sem(cons.hedo.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances'))

# model summary
summary(cons.hedo.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# script
cons.hedo.script <- bivariate.linear.script('cons', 'hedo', 1:4, TRUE)
cat(cons.hedo.script)

# fit model
cons.hedo.freely <- sem(cons.hedo.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances'))

# model summary
summary(cons.hedo.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.bivariate.gender.results(cons.hedo.constr,
                                cons.hedo.freely,
                                1:4)

## model comparisons

# compare model fits
summary(compareFit(cons.hedo.constr, cons.hedo.freely)) # freely fits better

# update summary
bivariate[bivariate$var == 'cons', 'hedo'] <- 'freely'

# # wald test for cons.i ~~ hedo.i
# lavTestWald(cons.hedo.freely,
#             constraints = 'female.ii - male.ii == 0') # no significant difference
# 
# # wald test for cons.l ~~ hedo.i
# lavTestWald(cons.hedo.freely,
#             constraints = 'female.li - male.li == 0') # no significant difference

# clean environment
rm(list = ls(pattern = '.cons'))

## liberal values

## conscientiousness T1 predicting liberal values T1

## constrained model

# script
cons.libe.script <- trait.predict.linear.script('cons', 'libe', 1:8, FALSE)
cat(cons.libe.script)

# fit model
cons.libe.constr <- sem(cons.libe.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances',
                                        'regressions'))

# model summary
summary(cons.libe.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# script
cons.libe.script <- trait.predict.linear.script('cons', 'libe', 1:8, TRUE)
cat(cons.libe.script)

# fit model
cons.libe.freely <- sem(cons.libe.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances'))

# model summary
summary(cons.libe.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.gender.results(cons.libe.constr, 
                                 cons.libe.freely,
                                 'trait', 
                                 1:8)

## model comparisons

# compare model fits
summary(compareFit(cons.libe.constr, cons.libe.freely)) # freely fits better

# update summary
bivariate[bivariate$var == 'cons', 'libe_predicted'] <- 'freely'

# clean environment
rm(list = ls(pattern = '.cons'))

## liberal values predicting conscientiousness T1-8

## constrained model

# script
libe.cons.script <- goal.predict.linear.script('libe', 'cons', 1:8, FALSE)
cat(libe.cons.script)

# fit model
libe.cons.constr <- sem(libe.cons.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances',
                                        'regressions'))

# model summary
summary(libe.cons.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# script
libe.cons.script <- goal.predict.linear.script('libe', 'cons', 1:8, TRUE)
cat(libe.cons.script)

# fit model
libe.cons.freely <- sem(libe.cons.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances'))

# model summary
summary(libe.cons.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.gender.results(libe.cons.constr, 
                                 libe.cons.freely,
                                 'goal', 
                                 1:8)

## model comparisons

# compare model fits
summary(compareFit(libe.cons.constr, libe.cons.freely)) # freely fits better

# update summary
bivariate[bivariate$var == 'cons', 'libe_predicting'] <- 'freely'

# clean environment
rm(list = ls(pattern = '.cons'))

## security

## conscientiousness T1-4 predicting security

## constrained model

# script
cons.secu.script <- trait.predict.linear.script('cons', 'secu', 1:4, FALSE)
cat(cons.secu.script)

# fit model
cons.secu.constr <- sem(cons.secu.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances',
                                        'regressions'))

# model summary
summary(cons.secu.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# script
cons.secu.script <- trait.predict.linear.script('cons', 'secu', 1:4, TRUE)
cat(cons.secu.script)

# fit model
cons.secu.freely <- sem(cons.secu.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances'))

# model summary
summary(cons.secu.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.gender.results(cons.secu.constr, 
                                 cons.secu.freely,
                                 'trait', 
                                 1:4)

## model comparisons

# compare model fits
summary(compareFit(cons.secu.constr, cons.secu.freely)) # freely fits better

# update summary
bivariate[bivariate$var == 'cons', 'secu_predicted'] <- 'freely'

# clean environment
rm(list = ls(pattern = '.cons'))

## security predicting conscientiousness T4-8

## constrained model

# script
secu.cons.script <- goal.predict.linear.script('secu', 'cons', 4:8, FALSE)
cat(secu.cons.script)

# fit model
secu.cons.constr <- sem(secu.cons.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances',
                                        'regressions'))

# model summary
summary(secu.cons.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# script
secu.cons.script <- goal.predict.linear.script('secu', 'cons', 4:8, TRUE)
cat(secu.cons.script)

# fit model
secu.cons.freely <- sem(secu.cons.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances'))

# model summary
summary(secu.cons.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.gender.results(secu.cons.constr, 
                                 secu.cons.freely,
                                 'goal', 
                                 4:8)

## model comparisons

# compare model fits
summary(compareFit(secu.cons.constr, secu.cons.freely)) # freely fits better

# update summary
bivariate[bivariate$var == 'cons', 'secu_predicting'] <- 'freely'

# clean environment
rm(list = ls(pattern = '.cons'))

## conformism

## conscientiousness T1-4 predicting conformism

## constrained model

# script
cons.conf.script <- trait.predict.linear.script('cons', 'conf', 1:4, FALSE)
cat(cons.conf.script)

# fit model
cons.conf.constr <- sem(cons.conf.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances',
                                        'regressions'))

# model summary
summary(cons.conf.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# script
cons.conf.script <- trait.predict.linear.script('cons', 'conf', 1:4, TRUE)
cat(cons.conf.script)

# fit model
cons.conf.freely <- sem(cons.conf.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances'))

# model summary
summary(cons.conf.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.gender.results(cons.conf.constr, 
                                 cons.conf.freely,
                                 'trait', 
                                 1:4)

## model comparisons

# compare model fits
summary(compareFit(cons.conf.constr, cons.conf.freely)) # freely fits better

# update summary
bivariate[bivariate$var == 'cons', 'conf_predicted'] <- 'freely'

# clean environment
rm(list = ls(pattern = '.cons'))

## conformism predicting conscientiousness T4-8

## constrained model

# script
conf.cons.script <- goal.predict.linear.script('conf', 'cons', 4:8, FALSE)
cat(conf.cons.script)

# fit model
conf.cons.constr <- sem(conf.cons.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances',
                                        'regressions'))

# model summary
summary(conf.cons.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# script
conf.cons.script <- goal.predict.linear.script('conf', 'cons', 4:8, TRUE)
cat(conf.cons.script)

# fit model
conf.cons.freely <- sem(conf.cons.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances'))

# model summary
summary(conf.cons.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.gender.results(conf.cons.constr, 
                                 conf.cons.freely,
                                 'goal', 
                                 4:8)

## model comparisons

# compare model fits
summary(compareFit(conf.cons.constr, conf.cons.freely)) # freely fits better

# update summary
bivariate[bivariate$var == 'cons', 'conf_predicting'] <- 'freely'

# clean environment
rm(list = ls(pattern = '.cons'))

## religiosity

## conscientiousness T1-4 predicting religiosity

## constrained model

# script
cons.reli.script <- trait.predict.linear.script('cons', 'reli', 1:4, FALSE)
cat(cons.reli.script)

# fit model
cons.reli.constr <- sem(cons.reli.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances',
                                        'regressions'))

# model summary
summary(cons.reli.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# script
cons.reli.script <- trait.predict.linear.script('cons', 'reli', 1:4, TRUE)
cat(cons.reli.script)

# fit model
cons.reli.freely <- sem(cons.reli.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances'))

# model summary
summary(cons.reli.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.gender.results(cons.reli.constr, 
                                 cons.reli.freely,
                                 'trait', 
                                 1:4)

## model comparisons

# compare model fits
summary(compareFit(cons.reli.constr, cons.reli.freely)) # freely fits better

# update summary
bivariate[bivariate$var == 'cons', 'reli_predicted'] <- 'freely'

# clean environment
rm(list = ls(pattern = '.cons'))

## religiosity predicting conscientiousness T4-8

## constrained model

# script
reli.cons.script <- goal.predict.linear.script('reli', 'cons', 4:8, FALSE)
cat(reli.cons.script)

# fit model
reli.cons.constr <- sem(reli.cons.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances',
                                        'regressions'))

# model summary
summary(reli.cons.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# script
reli.cons.script <- goal.predict.linear.script('reli', 'cons', 4:8, TRUE)
cat(reli.cons.script)

# fit model
reli.cons.freely <- sem(reli.cons.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances'))

# model summary
summary(reli.cons.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.gender.results(reli.cons.constr, 
                                 reli.cons.freely,
                                 'goal', 
                                 4:8)

## model comparisons

# compare model fits
summary(compareFit(reli.cons.constr, reli.cons.freely)) # freely fits better

# update summary
bivariate[bivariate$var == 'cons', 'reli_predicting'] <- 'freely'

# clean environment
rm(list = ls(pattern = '.cons'))

## neuroticism #################################################################

## prosperity

## constrained model

# script
neur.pros.script <- bivariate.linear.script('neur', 'pros', 1:8, FALSE)
cat(neur.pros.script)

# fit model
neur.pros.constr <- sem(neur.pros.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances'))

# model summary
summary(neur.pros.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# script
neur.pros.script <- bivariate.linear.script('neur', 'pros', 1:8, TRUE)
cat(neur.pros.script)

# fit model
neur.pros.freely <- sem(neur.pros.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances'))

# model summary
summary(neur.pros.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.bivariate.gender.results(neur.pros.constr,
                                neur.pros.freely,
                                1:8)

## model comparisons

# compare model fits
summary(compareFit(neur.pros.constr, neur.pros.freely)) # freely fits better

# update summary
bivariate[bivariate$var == 'neur', 'pros'] <- 'freely'

# clean environment
rm(list = ls(pattern = '.neur'))

## fame

## constrained model

# script
neur.fame.script <- bivariate.linear.script('neur', 'fame', 1:8, FALSE)
cat(neur.fame.script)

# fit model
neur.fame.constr <- sem(neur.fame.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances'))

# model summary
summary(neur.fame.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# script
neur.fame.script <- bivariate.linear.script('neur', 'fame', 1:8, TRUE)
cat(neur.fame.script)

# fit model
neur.fame.freely <- sem(neur.fame.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances'))

# model summary
summary(neur.fame.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.bivariate.gender.results(neur.fame.constr,
                                neur.fame.freely,
                                1:8)

## model comparisons

# compare model fits
summary(compareFit(neur.fame.constr, neur.fame.freely)) # freely fits better

# update summary
bivariate[bivariate$var == 'neur', 'fame'] <- 'freely'

# clean environment
rm(list = ls(pattern = '.neur'))

## attractiveness

## constrained model

# script
neur.attr.script <- bivariate.linear.script('neur', 'attr', 1:8, FALSE)
cat(neur.attr.script)

# fit model
neur.attr.constr <- sem(neur.attr.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances'))

# model summary
summary(neur.attr.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# script
neur.attr.script <- bivariate.linear.script('neur', 'attr', 1:8, TRUE)
cat(neur.attr.script)

# fit model
neur.attr.freely <- sem(neur.attr.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances'))

# model summary
summary(neur.attr.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.bivariate.gender.results(neur.attr.constr,
                                neur.attr.freely,
                                1:8)

## model comparisons

# compare model fits
summary(compareFit(neur.attr.constr, neur.attr.freely)) # freely fits better

# update summary
bivariate[bivariate$var == 'neur', 'attr'] <- 'freely'

# clean environment
rm(list = ls(pattern = '.neur'))

## personal growth

## constrained model

# script
neur.pers.script <- bivariate.linear.script('neur', 'pers', 1:4, FALSE)
cat(neur.pers.script)

# fit model
neur.pers.constr <- sem(neur.pers.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances'))

# model summary
summary(neur.pers.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# script
neur.pers.script <- bivariate.linear.script('neur', 'pers', 1:4, TRUE)
cat(neur.pers.script)

# fit model
neur.pers.freely <- sem(neur.pers.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances'))

# model summary
summary(neur.pers.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.bivariate.gender.results(neur.pers.constr,
                                neur.pers.freely,
                                1:4)

## model comparisons

# compare model fits
summary(compareFit(neur.pers.constr, neur.pers.freely)) # freely fits better

# update summary
bivariate[bivariate$var == 'neur', 'pers'] <- 'freely'

# clean environment
rm(list = ls(pattern = '.neur'))

## relationship

## constrained model

# script
neur.rela.script <- bivariate.linear.script('neur', 'rela', 1:4, FALSE)
cat(neur.rela.script)

# fit model
neur.rela.constr <- sem(neur.rela.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances'))

# model summary
summary(neur.rela.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# script
neur.rela.script <- bivariate.linear.script('neur', 'rela', 1:4, TRUE)
cat(neur.rela.script)

# fit model
neur.rela.freely <- sem(neur.rela.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances'))

# model summary
summary(neur.rela.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.bivariate.gender.results(neur.rela.constr,
                                neur.rela.freely,
                                1:4)

## model comparisons

# compare model fits
summary(compareFit(neur.rela.constr, neur.rela.freely)) # freely fits better

# update summary
bivariate[bivariate$var == 'neur', 'rela'] <- 'freely'

# clean environment
rm(list = ls(pattern = '.neur'))

## society

## constrained model

# script
neur.soci.script <- bivariate.linear.script('neur', 'soci', 1:4, FALSE)
cat(neur.soci.script)

# fit model
neur.soci.constr <- sem(neur.soci.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances'))

# model summary
summary(neur.soci.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# script
neur.soci.script <- bivariate.linear.script('neur', 'soci', 1:4, TRUE)
cat(neur.soci.script)

# fit model
neur.soci.freely <- sem(neur.soci.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances'))

# model summary
summary(neur.soci.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.bivariate.gender.results(neur.soci.constr,
                                neur.soci.freely,
                                1:4)

## model comparisons

# compare model fits
summary(compareFit(neur.soci.constr, neur.soci.freely)) # freely fits better

# update summary
bivariate[bivariate$var == 'neur', 'soci'] <- 'freely'

# ## plot
# 
# ## female
# 
# # neur.i ~~ soci.l
# neur.soci.il.female.plot <- plot.level.slope(neur.soci.freely,
#                                              'female',
#                                              1:4,
#                                              'Neuroticism',
#                                              'Society', '.l')
# 
# # view plot
# print(neur.soci.il.female.plot)
# 
# # if looks good, save plot
# ggsave(filename = here('Output/Sensitivity/Analysis 3/Plots/neur_soci_il_female.png'), 
#        plot = neur.soci.il.female.plot,
#        width = 6, height = 4.5, scale = 1.5)

# clean environment
rm(list = ls(pattern = '.neur'))

## physical health

## constrained model

# script
neur.phys.script <- bivariate.linear.script('neur', 'phys', 1:4, FALSE)
cat(neur.phys.script)

# fit model
neur.phys.constr <- sem(neur.phys.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances'))

# model summary
summary(neur.phys.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# script
neur.phys.script <- bivariate.linear.script('neur', 'phys', 1:4, TRUE)
cat(neur.phys.script)

# fit model
neur.phys.freely <- sem(neur.phys.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances'))

# model summary
summary(neur.phys.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.bivariate.gender.results(neur.phys.constr,
                                neur.phys.freely,
                                1:4)

## model comparisons

# compare model fits
summary(compareFit(neur.phys.constr, neur.phys.freely)) # freely fits better

# update summary
bivariate[bivariate$var == 'neur', 'phys'] <- 'freely'

# clean environment
rm(list = ls(pattern = '.neur'))

## hedonism

## constrained model

# script
neur.hedo.script <- bivariate.linear.script('neur', 'hedo', 1:4, FALSE)
cat(neur.hedo.script)

# fit model
neur.hedo.constr <- sem(neur.hedo.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances'))

# model summary
summary(neur.hedo.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# script
neur.hedo.script <- bivariate.linear.script('neur', 'hedo', 1:4, TRUE)
cat(neur.hedo.script)

# fit model
neur.hedo.freely <- sem(neur.hedo.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances'))

# model summary
summary(neur.hedo.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.bivariate.gender.results(neur.hedo.constr,
                                neur.hedo.freely,
                                1:4)

## model comparisons

# compare model fits
summary(compareFit(neur.hedo.constr, neur.hedo.freely)) # freely fits better

# update summary
bivariate[bivariate$var == 'neur', 'hedo'] <- 'freely'

# clean environment
rm(list = ls(pattern = '.neur'))

## liberal values

## neuroticism T1 predicting liberal values T1

## constrained model

# script
neur.libe.script <- trait.predict.linear.script('neur', 'libe', 1:8, FALSE)
cat(neur.libe.script)

# fit model
neur.libe.constr <- sem(neur.libe.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances',
                                        'regressions'))

# model summary
summary(neur.libe.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# script
neur.libe.script <- trait.predict.linear.script('neur', 'libe', 1:8, TRUE)
cat(neur.libe.script)

# fit model
neur.libe.freely <- sem(neur.libe.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances'))

# model summary
summary(neur.libe.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.gender.results(neur.libe.constr, 
                                 neur.libe.freely,
                                 'trait', 
                                 1:8)

## model comparisons

# compare model fits
summary(compareFit(neur.libe.constr, neur.libe.freely)) # freely fits better

# update summary
bivariate[bivariate$var == 'neur', 'libe_predicted'] <- 'freely'

# clean environment
rm(list = ls(pattern = '.neur'))

## liberal values predicting neuroticism T1-8

## constrained model

# script
libe.neur.script <- goal.predict.linear.script('libe', 'neur', 1:8, FALSE)
cat(libe.neur.script)

# fit model
libe.neur.constr <- sem(libe.neur.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances',
                                        'regressions'))

# model summary
summary(libe.neur.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# script
libe.neur.script <- goal.predict.linear.script('libe', 'neur', 1:8, TRUE)
cat(libe.neur.script)

# fit model
libe.neur.freely <- sem(libe.neur.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances'))

# model summary
summary(libe.neur.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.gender.results(libe.neur.constr, 
                                 libe.neur.freely,
                                 'goal', 
                                 1:8)

## model comparisons

# compare model fits
summary(compareFit(libe.neur.constr, libe.neur.freely)) # freely fits better

# update summary
bivariate[bivariate$var == 'neur', 'libe_predicting'] <- 'freely'

# clean environment
rm(list = ls(pattern = '.neur'))

## security

## neuroticism T1-4 predicting security

## constrained model

# script
neur.secu.script <- trait.predict.linear.script('neur', 'secu', 1:4, FALSE)
cat(neur.secu.script)

# fit model
neur.secu.constr <- sem(neur.secu.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances',
                                        'regressions'))

# model summary
summary(neur.secu.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# script
neur.secu.script <- trait.predict.linear.script('neur', 'secu', 1:4, TRUE)
cat(neur.secu.script)

# fit model
neur.secu.freely <- sem(neur.secu.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances'))

# model summary
summary(neur.secu.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.gender.results(neur.secu.constr, 
                                 neur.secu.freely,
                                 'trait', 
                                 1:4)

## model comparisons

# compare model fits
summary(compareFit(neur.secu.constr, neur.secu.freely)) # freely fits better

# update summary
bivariate[bivariate$var == 'neur', 'secu_predicted'] <- 'freely'

# clean environment
rm(list = ls(pattern = '.neur'))

## security predicting neuroticism T4-8

## constrained model

# script
secu.neur.script <- goal.predict.linear.script('secu', 'neur', 4:8, FALSE)
cat(secu.neur.script)

# fit model
secu.neur.constr <- sem(secu.neur.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances',
                                        'regressions'))

# model summary
summary(secu.neur.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# script
secu.neur.script <- goal.predict.linear.script('secu', 'neur', 4:8, TRUE)
cat(secu.neur.script)

# fit model
secu.neur.freely <- sem(secu.neur.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances'))

# model summary
summary(secu.neur.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.gender.results(secu.neur.constr, 
                                 secu.neur.freely,
                                 'goal', 
                                 4:8)

## model comparisons

# compare model fits
summary(compareFit(secu.neur.constr, secu.neur.freely)) # freely fits better

# update summary
bivariate[bivariate$var == 'neur', 'secu_predicting'] <- 'freely'

# clean environment
rm(list = ls(pattern = '.neur'))

## conformism

## neuroticism T1-4 predicting conformism

## constrained model

# script
neur.conf.script <- trait.predict.linear.script('neur', 'conf', 1:4, FALSE)
cat(neur.conf.script)

# fit model
neur.conf.constr <- sem(neur.conf.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances',
                                        'regressions'))

# model summary
summary(neur.conf.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# script
neur.conf.script <- trait.predict.linear.script('neur', 'conf', 1:4, TRUE)
cat(neur.conf.script)

# fit model
neur.conf.freely <- sem(neur.conf.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances'))

# model summary
summary(neur.conf.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.gender.results(neur.conf.constr, 
                                 neur.conf.freely,
                                 'trait', 
                                 1:4)

## model comparisons

# compare model fits
summary(compareFit(neur.conf.constr, neur.conf.freely)) # freely fits better

# update summary
bivariate[bivariate$var == 'neur', 'conf_predicted'] <- 'freely'

# clean environment
rm(list = ls(pattern = '.neur'))

## conformism predicting neuroticism T4-8

## constrained model

# script
conf.neur.script <- goal.predict.linear.script('conf', 'neur', 4:8, FALSE)
cat(conf.neur.script)

# fit model
conf.neur.constr <- sem(conf.neur.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances',
                                        'regressions'))

# model summary
summary(conf.neur.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# script
conf.neur.script <- goal.predict.linear.script('conf', 'neur', 4:8, TRUE)
cat(conf.neur.script)

# fit model
conf.neur.freely <- sem(conf.neur.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances'))

# model summary
summary(conf.neur.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.gender.results(conf.neur.constr, 
                                 conf.neur.freely,
                                 'goal', 
                                 4:8)

## model comparisons

# compare model fits
summary(compareFit(conf.neur.constr, conf.neur.freely)) # freely fits better

# update summary
bivariate[bivariate$var == 'neur', 'conf_predicting'] <- 'freely'

# clean environment
rm(list = ls(pattern = '.neur'))

## religiosity

## neuroticism T1-4 predicting religiosity

## constrained model

# script
neur.reli.script <- trait.predict.linear.script('neur', 'reli', 1:4, FALSE)
cat(neur.reli.script)

# fit model
neur.reli.constr <- sem(neur.reli.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances',
                                        'regressions'))

# model summary
summary(neur.reli.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# script
neur.reli.script <- trait.predict.linear.script('neur', 'reli', 1:4, TRUE)
cat(neur.reli.script)

# fit model
neur.reli.freely <- sem(neur.reli.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances'))

# model summary
summary(neur.reli.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.gender.results(neur.reli.constr, 
                                 neur.reli.freely,
                                 'trait', 
                                 1:4)

## model comparisons

# compare model fits
summary(compareFit(neur.reli.constr, neur.reli.freely)) # freely fits better

# update summary
bivariate[bivariate$var == 'neur', 'reli_predicted'] <- 'freely'

# clean environment
rm(list = ls(pattern = '.neur'))

## religiosity predicting neuroticism T4-8

## constrained model

# script
reli.neur.script <- goal.predict.linear.script('reli', 'neur', 4:8, FALSE)
cat(reli.neur.script)

# fit model
reli.neur.constr <- sem(reli.neur.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances',
                                        'regressions'))

# model summary
summary(reli.neur.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# script
reli.neur.script <- goal.predict.linear.script('reli', 'neur', 4:8, TRUE)
cat(reli.neur.script)

# fit model
reli.neur.freely <- sem(reli.neur.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances'))

# model summary
summary(reli.neur.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.gender.results(reli.neur.constr, 
                                 reli.neur.freely,
                                 'goal', 
                                 4:8)

## model comparisons

# compare model fits
summary(compareFit(reli.neur.constr, reli.neur.freely)) # freely fits better

# update summary
bivariate[bivariate$var == 'neur', 'reli_predicting'] <- 'freely'

# clean environment
rm(list = ls(pattern = '.neur'))

## openness ####################################################################

## prosperity

## constrained model

# script
open.pros.script <- bivariate.linear.script('open', 'pros', 1:8, FALSE)
cat(open.pros.script)

# fit model
open.pros.constr <- sem(open.pros.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances'))

# model summary
summary(open.pros.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# script
open.pros.script <- bivariate.linear.script('open', 'pros', 1:8, TRUE)
cat(open.pros.script)

# fit model
open.pros.freely <- sem(open.pros.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances'))

# model summary
summary(open.pros.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.bivariate.gender.results(open.pros.constr,
                                open.pros.freely,
                                1:8)

## model comparisons

# compare model fits
summary(compareFit(open.pros.constr, open.pros.freely)) # freely fits better

# update summary
bivariate[bivariate$var == 'open', 'pros'] <- 'freely'

# # wald test for open.i ~~ pros.i
# lavTestWald(open.pros.freely,
#             constraints = 'female.ii - male.ii == 0') # significant difference
# 
# ## plot
# 
# ## male
# 
# # open.q ~~ pros.i
# open.pros.qi.male.plot <- plot.slope.level(open.pros.freely,
#                                            'male',
#                                            1:8,
#                                            'Openness', '.q',
#                                            'Prosperity')
# 
# # view plot
# print(open.pros.qi.male.plot)
# 
# # if looks good, save plot
# ggsave(filename = here('Output/Sensitivity/Analysis 3/Plots/open_pros_qi_male.png'), 
#        plot = open.pros.qi.male.plot,
#        width = 6, height = 4.5, scale = 1.5)

# clean environment
rm(list = ls(pattern = '.open'))

## fame

## constrained model

# script
open.fame.script <- bivariate.linear.script('open', 'fame', 1:8, FALSE)
cat(open.fame.script)

# fit model
open.fame.constr <- sem(open.fame.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances'))

# model summary
summary(open.fame.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# script
open.fame.script <- bivariate.linear.script('open', 'fame', 1:8, TRUE)
cat(open.fame.script)

# fit model
open.fame.freely <- sem(open.fame.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances'))

# model summary
summary(open.fame.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.bivariate.gender.results(open.fame.constr,
                                open.fame.freely,
                                1:8)

## model comparisons

# compare model fits
summary(compareFit(open.fame.constr, open.fame.freely)) # freely fits better

# update summary
bivariate[bivariate$var == 'open', 'fame'] <- 'freely'

# clean environment
rm(list = ls(pattern = '.open'))

## attractiveness

## constrained model

# script
open.attr.script <- bivariate.linear.script('open', 'attr', 1:8, FALSE)
cat(open.attr.script)

# fit model
open.attr.constr <- sem(open.attr.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances'))

# model summary
summary(open.attr.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# script
open.attr.script <- bivariate.linear.script('open', 'attr', 1:8, TRUE)
cat(open.attr.script)

# fit model
open.attr.freely <- sem(open.attr.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances'))

# model summary
summary(open.attr.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.bivariate.gender.results(open.attr.constr,
                                open.attr.freely,
                                1:8)

## model comparisons

# compare model fits
summary(compareFit(open.attr.constr, open.attr.freely)) # freely fits better

# update summary
bivariate[bivariate$var == 'open', 'attr'] <- 'freely'

# ## plot
# 
# ## male
# 
# # open.i ~~ attr.l
# open.attr.il.male.plot <- plot.level.slope(open.attr.freely,
#                                            'male',
#                                            1:8,
#                                            'Openness',
#                                            'Attractiveness',
#                                            '.l')
# 
# # view plot
# print(open.attr.il.male.plot)
# 
# # if looks good, save plot
# ggsave(filename = here('Output/Sensitivity/Analysis 3/Plots/open_attr_il_male.png'), 
#        plot = open.attr.il.male.plot,
#        width = 6, height = 4.5, scale = 1.5)

# clean environment
rm(list = ls(pattern = '.open'))

## personal growth

## constrained model

# script
open.pers.script <- bivariate.linear.script('open', 'pers', 1:4, FALSE)
cat(open.pers.script)

# fit model
open.pers.constr <- sem(open.pers.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances'))

# model summary
summary(open.pers.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# script
open.pers.script <- bivariate.linear.script('open', 'pers', 1:4, TRUE)
cat(open.pers.script)

# fit model
open.pers.freely <- sem(open.pers.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances'))

# model summary
summary(open.pers.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.bivariate.gender.results(open.pers.constr,
                                open.pers.freely,
                                1:4)

## model comparisons

# compare model fits
summary(compareFit(open.pers.constr, open.pers.freely)) # freely fits better

# update summary
bivariate[bivariate$var == 'open', 'pers'] <- 'freely'

# # wald test for open.i ~~ pers.i
# lavTestWald(open.pers.freely,
#             constraints = 'female.ii - male.ii == 0') # significant difference

# clean environment
rm(list = ls(pattern = '.open'))

## relationship

## constrained model

# script
open.rela.script <- bivariate.linear.script('open', 'rela', 1:4, FALSE)
cat(open.rela.script)

# fit model
open.rela.constr <- sem(open.rela.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances'))

# model summary
summary(open.rela.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# script
open.rela.script <- bivariate.linear.script('open', 'rela', 1:4, TRUE)
cat(open.rela.script)

# fit model
open.rela.freely <- sem(open.rela.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances'))

# model summary
summary(open.rela.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.bivariate.gender.results(open.rela.constr,
                                open.rela.freely,
                                1:4)

## model comparisons

# compare model fits
summary(compareFit(open.rela.constr, open.rela.freely)) # freely fits better

# update summary
bivariate[bivariate$var == 'open', 'rela'] <- 'freely'

# clean environment
rm(list = ls(pattern = '.open'))

## society

## constrained model

# script
open.soci.script <- bivariate.linear.script('open', 'soci', 1:4, FALSE)
cat(open.soci.script)

# fit model
open.soci.constr <- sem(open.soci.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances'))

# model summary
summary(open.soci.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# script
open.soci.script <- bivariate.linear.script('open', 'soci', 1:4, TRUE)
cat(open.soci.script)

# fit model
open.soci.freely <- sem(open.soci.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances'))

# model summary
summary(open.soci.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.bivariate.gender.results(open.soci.constr,
                                open.soci.freely,
                                1:4)

## model comparisons

# compare model fits
summary(compareFit(open.soci.constr, open.soci.freely)) # freely fits better

# update summary
bivariate[bivariate$var == 'open', 'soci'] <- 'freely'

# # wald test for open.i ~~ soci.i
# lavTestWald(open.soci.freely,
#             constraints = 'female.ii - male.ii == 0') # no significant difference

# clean environment
rm(list = ls(pattern = '.open'))

## physical health

## constrained model

# script
open.phys.script <- bivariate.linear.script('open', 'phys', 1:4, FALSE)
cat(open.phys.script)

# fit model
open.phys.constr <- sem(open.phys.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances'))

# model summary
summary(open.phys.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# script
open.phys.script <- bivariate.linear.script('open', 'phys', 1:4, TRUE)
cat(open.phys.script)

# fit model
open.phys.freely <- sem(open.phys.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances'))

# model summary
summary(open.phys.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.bivariate.gender.results(open.phys.constr,
                                open.phys.freely,
                                1:4)

## model comparisons

# compare model fits
summary(compareFit(open.phys.constr, open.phys.freely)) # freely fits better

# update summary
bivariate[bivariate$var == 'open', 'phys'] <- 'freely'

# # wald test for open.i ~~ phys.i
# lavTestWald(open.phys.freely,
#             constraints = 'female.ii - male.ii == 0') # no significant difference
# 
# ## plot
# 
# ## female
# 
# # open.q ~~ phys.i
# open.phys.qi.female.plot <- plot.slope.level(open.phys.freely,
#                                              'female',
#                                              1:4,
#                                              'Openness',
#                                              '.q',
#                                              'Physical Health')
# 
# # view plot
# print(open.phys.qi.female.plot)
# 
# # if looks good, save plot
# ggsave(filename = here('Output/Sensitivity/Analysis 3/Plots/open_phys_qi_female.png'), 
#        plot = open.phys.qi.female.plot,
#        width = 6, height = 4.5, scale = 1.5)

# clean environment
rm(list = ls(pattern = '.open'))

## hedonism

## constrained model

# script
open.hedo.script <- bivariate.linear.script('open', 'hedo', 1:4, FALSE)
cat(open.hedo.script)

# fit model
open.hedo.constr <- sem(open.hedo.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances'))

# model summary
summary(open.hedo.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# script
open.hedo.script <- bivariate.linear.script('open', 'hedo', 1:4, TRUE)
cat(open.hedo.script)

# fit model
open.hedo.freely <- sem(open.hedo.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances'))

# model summary
summary(open.hedo.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.bivariate.gender.results(open.hedo.constr,
                                open.hedo.freely,
                                1:4)

## model comparisons

# compare model fits
summary(compareFit(open.hedo.constr, open.hedo.freely)) # freely fits better

# update summary
bivariate[bivariate$var == 'open', 'hedo'] <- 'freely'

# # wald test open.i ~~ pros.i
# lavTestWald(open.hedo.freely,
#             constraints = 'female.ii - male.ii == 0') # no significant difference

# clean environment
rm(list = ls(pattern = '.open'))

## liberal values

## openness T1 predicting liberal values T1

## constrained model

# script
open.libe.script <- trait.predict.linear.script('open', 'libe', 1:8, FALSE)
cat(open.libe.script)

# fit model
open.libe.constr <- sem(open.libe.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances',
                                        'regressions'))

# model summary
summary(open.libe.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# script
open.libe.script <- trait.predict.linear.script('open', 'libe', 1:8, TRUE)
cat(open.libe.script)

# fit model
open.libe.freely <- sem(open.libe.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances'))

# model summary
summary(open.libe.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.gender.results(open.libe.constr, 
                                 open.libe.freely,
                                 'trait', 
                                 1:8)

## model comparisons

# compare model fits
summary(compareFit(open.libe.constr, open.libe.freely)) # freely fits better

# update summary
bivariate[bivariate$var == 'open', 'libe_predicted'] <- 'freely'

# clean environment
rm(list = ls(pattern = '.open'))

## liberal values predicting openness T1-8

## constrained model

# script
libe.open.script <- goal.predict.linear.script('libe', 'open', 1:8, FALSE)
cat(libe.open.script)

# fit model
libe.open.constr <- sem(libe.open.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances',
                                        'regressions'))

# model summary
summary(libe.open.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# script
libe.open.script <- goal.predict.linear.script('libe', 'open', 1:8, TRUE)
cat(libe.open.script)

# fit model
libe.open.freely <- sem(libe.open.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances'))

# model summary
summary(libe.open.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.gender.results(libe.open.constr, 
                                 libe.open.freely,
                                 'goal', 
                                 1:8)

## model comparisons

# compare model fits
summary(compareFit(libe.open.constr, libe.open.freely)) # freely fits better

# update summary
bivariate[bivariate$var == 'open', 'libe_predicting'] <- 'freely'

# clean environment
rm(list = ls(pattern = '.open'))

## security

## openness T1-4 predicting security

## constrained model

# script
open.secu.script <- trait.predict.linear.script('open', 'secu', 1:4, FALSE)
cat(open.secu.script)

# fit model
open.secu.constr <- sem(open.secu.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances',
                                        'regressions'))

# model summary
summary(open.secu.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# script
open.secu.script <- trait.predict.linear.script('open', 'secu', 1:4, TRUE)
cat(open.secu.script)

# fit model
open.secu.freely <- sem(open.secu.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances'))

# model summary
summary(open.secu.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.gender.results(open.secu.constr, 
                                 open.secu.freely,
                                 'trait', 
                                 1:4)

## model comparisons

# compare model fits
summary(compareFit(open.secu.constr, open.secu.freely)) # freely fits better

# update summary
bivariate[bivariate$var == 'open', 'secu_predicted'] <- 'freely'

# clean environment
rm(list = ls(pattern = '.open'))

## security T3/T4 predicting openness T4-8

## constrained model

# script
secu.open.script <- goal.predict.linear.script('secu', 'open', 4:8, FALSE)
cat(secu.open.script)

# fit model
secu.open.constr <- sem(secu.open.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances',
                                        'regressions'))

# model summary
summary(secu.open.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# script
secu.open.script <- goal.predict.linear.script('secu', 'open', 4:8, TRUE)
cat(secu.open.script)

# fit model
secu.open.freely <- sem(secu.open.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances'))

# model summary
summary(secu.open.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.gender.results(secu.open.constr, 
                                 secu.open.freely,
                                 'goal', 
                                 4:8)

## model comparisons

# compare model fits
summary(compareFit(secu.open.constr, secu.open.freely)) # freely fits better

# update summary
bivariate[bivariate$var == 'open', 'secu_predicting'] <- 'freely'

# clean environment
rm(list = ls(pattern = '.open'))

## conformism

## openness T1-4 predicting conformism

## constrained model

# script
open.conf.script <- trait.predict.linear.script('open', 'conf', 1:4, FALSE)
cat(open.conf.script)

# fit model
open.conf.constr <- sem(open.conf.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances',
                                        'regressions'))

# model summary
summary(open.conf.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# script
open.conf.script <- trait.predict.linear.script('open', 'conf', 1:4, TRUE)
cat(open.conf.script)

# fit model
open.conf.freely <- sem(open.conf.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances'))

# model summary
summary(open.conf.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.gender.results(open.conf.constr, 
                                 open.conf.freely,
                                 'trait', 
                                 1:4)

## model comparisons

# compare model fits
summary(compareFit(open.conf.constr, open.conf.freely)) # freely fits better

# update summary
bivariate[bivariate$var == 'open', 'conf_predicted'] <- 'freely'

# clean environment
rm(list = ls(pattern = '.open'))

## conformism predicting openness T4-8

## constrained model

# script
conf.open.script <- goal.predict.linear.script('conf', 'open', 4:8, FALSE)
cat(conf.open.script)

# fit model
conf.open.constr <- sem(conf.open.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances',
                                        'regressions'))

# model summary
summary(conf.open.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# script
conf.open.script <- goal.predict.linear.script('conf', 'open', 4:8, TRUE)
cat(conf.open.script)

# fit model
conf.open.freely <- sem(conf.open.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances'))

# model summary
summary(conf.open.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.gender.results(conf.open.constr, 
                                 conf.open.freely,
                                 'goal', 
                                 4:8)

## model comparisons

# compare model fits
summary(compareFit(conf.open.constr, conf.open.freely)) # freely fits better

# update summary
bivariate[bivariate$var == 'open', 'conf_predicting'] <- 'freely'

# clean environment
rm(list = ls(pattern = '.open'))

## religiosity

## openness T1-4 predicting religiosity

## constrained model

# script
open.reli.script <- trait.predict.linear.script('open', 'reli', 1:4, FALSE)
cat(open.reli.script)

# fit model
open.reli.constr <- sem(open.reli.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances',
                                        'regressions'))

# model summary
summary(open.reli.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# script
open.reli.script <- trait.predict.linear.script('open', 'reli', 1:4, TRUE)
cat(open.reli.script)

# fit model
open.reli.freely <- sem(open.reli.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances'))

# model summary
summary(open.reli.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.gender.results(open.reli.constr, 
                                 open.reli.freely,
                                 'trait', 
                                 1:4)

## model comparisons

# compare model fits
summary(compareFit(open.reli.constr, open.reli.freely)) # freely fits better

# update summary
bivariate[bivariate$var == 'open', 'reli_predicted'] <- 'freely'

# clean environment
rm(list = ls(pattern = '.open'))

## religiosity predicting openness T4-8

## constrained model

# script
reli.open.script <- goal.predict.linear.script('reli', 'open', 4:8, FALSE)
cat(reli.open.script)

# fit model
reli.open.constr <- sem(reli.open.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances',
                                        'regressions'))

# model summary
summary(reli.open.constr, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

## freely-estimated model

# script
reli.open.script <- goal.predict.linear.script('reli', 'open', 4:8, TRUE)
cat(reli.open.script)

# fit model
reli.open.freely <- sem(reli.open.script, 
                        data = data, 
                        missing = 'fiml',
                        estimator = 'mlr',
                        group = 'gender',
                        group.equal = c('loadings',
                                        'means',
                                        'lv.variances',
                                        'lv.covariances'))

# model summary
summary(reli.open.freely, 
        fit.measures = TRUE, 
        standardized = TRUE,
        nd = 3)

# export model results
export.regression.gender.results(reli.open.constr, 
                                 reli.open.freely,
                                 'goal', 
                                 4:8)

## model comparisons

# compare model fits
summary(compareFit(reli.open.constr, reli.open.freely)) # freely fits better

# update summary
bivariate[bivariate$var == 'open', 'reli_predicting'] <- 'freely'

# clean environment
rm(list = ls(pattern = '.open'))

# end of code ##################################################################