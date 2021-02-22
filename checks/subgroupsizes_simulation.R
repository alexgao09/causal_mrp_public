library(DeclareDesign)
library(tidyverse)
#library(caret)

# this script gets subgroup sizes 

pop_draw = readRDS("pop_draw.rds")

iter = 1000

full_list = c()
sa3_mc1_list = c()
sa1_re2_list = c()
sa3_mc1_re1_list = c()
sa3_mc1_re2_list = c()
sa3_mc1_re3_list = c()
sa3_mc1_re4_list = c()
sa3_mc1_re5_list = c()

# stratified cluster sampling function ----------------------------------------
stratifiedschoolsample_function = function(data, target_strata_samplesize) {
  # data is the population dataframe
  # target_strata_samplesize is a length-5 vector on number of schools to sample in each strata. From NLSM methodology pdf it should be c(28,34,32,19,27) (last column T2.3)
  strata1 = data %>% filter(school_achievement_index==1) %>% as.data.frame()
  strata2 = data %>% filter(school_achievement_index==2, minority_composition_index == 1) %>% as.data.frame()
  strata3 = data %>% filter(school_achievement_index==2, minority_composition_index == 2) %>% as.data.frame()
  strata4 = data %>% filter(school_achievement_index==3, minority_composition_index == 1) %>% as.data.frame()
  strata5 = data %>% filter(school_achievement_index==3, minority_composition_index == 2) %>% as.data.frame()
  
  # stratified sampling of schools of each strata 
  
  # simple random sample of schools with strata 1
  strata1_indices = sample(x = unique(strata1$school),
                           size = target_strata_samplesize[1],
                           replace = FALSE)
  
  strata2_indices = sample(x = unique(strata2$school),
                           size = target_strata_samplesize[2],
                           replace = FALSE)
  
  strata3_indices = sample(x = unique(strata3$school),
                           size = target_strata_samplesize[3],
                           replace = FALSE)
  
  strata4_indices = sample(x = unique(strata4$school),
                           size = target_strata_samplesize[4],
                           replace = FALSE)
  
  strata5_indices = sample(x = unique(strata5$school),
                           size = target_strata_samplesize[5],
                           replace = FALSE)
  
  # refer to page 18/191 to go from 140 schools to 65
  
  # the 140 schools sampled 
  data_140 = rbind(strata1 %>% filter(school %in% strata1_indices),
                   strata2 %>% filter(school %in% strata2_indices),
                   strata3 %>% filter(school %in% strata3_indices),
                   strata4 %>% filter(school %in% strata4_indices),
                   strata5 %>% filter(school %in% strata5_indices)) %>% as.data.frame()
  
  # the 65 schools that responded
  data_65_indices = sample(x = unique(data_140$school),
                           size = min(rpois(n=1,lambda=65), 140), # the number of schools that respond.
                           replace = FALSE)
  
  data_65 = data_140 %>% filter(school %in% data_65_indices) %>% as.data.frame()
  
  # mean student response rate is 92% within schools. See page 7/17 on main paper and 25/72 in appendix
  # data_65[sample(x = 1:nrow(data_65),
  #                size = round(nrow(data_65)*0.92),
  #                replace = FALSE),]
  
  #inv_logit_scaled(data_65$prev_gpa - mean(data_65$prev_gpa))
  
  data_65$student_sampling_prob = rbinom(length(data_65$prev_gpa),
                                         size=1,
                                         prob=brms::inv_logit_scaled(data_65$prev_gpa)) # sample students with probability according to this. Mean response rate should be around 92%
  
  print(data_65 %>% group_by(school_achievement_index) %>% summarise(sum(student_sampling_prob)/n()))
  print(sum(data_65$student_sampling_prob)/dim(data_65)[1])
  
  
  data_65[data_65$student_sampling_prob==1,]
}

# my_sampling_custom_ = declare_sampling(handler = stratifiedschoolsample_function, 
#                                        target_strata_samplesize=c(28,34,32,19,27))

pop_draw_index = data.frame(pop_draw, index = 1:dim(pop_draw)[1])
### start loop here -----

for (c in 1:iter) {
  
  set.seed(c)
  
  print(c)
  
  pop_draw_index$minority_composition_index = as.character(pop_draw_index$minority_composition_index)
  pop_draw_index$school_achievement_index = as.character(pop_draw_index$school_achievement_index)
  pop_draw_index$race_eth = as.character(pop_draw_index$race_eth)
  
  my_sampling_custom_ = declare_sampling(handler = stratifiedschoolsample_function, 
                                         target_strata_samplesize=c(28,34,32,19,27))
  
  stratified_cluster_sample_index = my_sampling_custom_(pop_draw_index)
  print(dim(stratified_cluster_sample_index))
  
  
  
  full_list = c(full_list, dim(stratified_cluster_sample_index)[1])
  sa3_mc1_list = c(sa3_mc1_list, dim(stratified_cluster_sample_index %>% filter(school_achievement_index==3,
                                                                                minority_composition_index==1))[1])
  sa1_re2_list = c(sa1_re2_list, dim(stratified_cluster_sample_index %>% filter(school_achievement_index==1,
                                                                                race_eth==2))[1])
  sa3_mc1_re1_list = c(sa3_mc1_re1_list, dim(stratified_cluster_sample_index %>% filter(school_achievement_index==3,
                                                                                        minority_composition_index==1,
                                                                                        race_eth==1))[1])
  sa3_mc1_re2_list = c(sa3_mc1_re2_list, dim(stratified_cluster_sample_index %>% filter(school_achievement_index==3,
                                                                                        minority_composition_index==1,
                                                                                        race_eth==2))[1])
  sa3_mc1_re3_list = c(sa3_mc1_re3_list, dim(stratified_cluster_sample_index %>% filter(school_achievement_index==3,
                                                                                        minority_composition_index==1,
                                                                                        race_eth==3))[1])
  sa3_mc1_re4_list = c(sa3_mc1_re4_list, dim(stratified_cluster_sample_index %>% filter(school_achievement_index==3,
                                                                                        minority_composition_index==1,
                                                                                        race_eth==4))[1])
  sa3_mc1_re5_list = c(sa3_mc1_re5_list, dim(stratified_cluster_sample_index %>% filter(school_achievement_index==3,
                                                                                        minority_composition_index==1,
                                                                                        race_eth==5))[1])
  
}





summary(full_list)
summary(sa3_mc1_list)
summary(sa1_re2_list)
summary(sa3_mc1_re1_list)
summary(sa3_mc1_re2_list)
summary(sa3_mc1_re3_list)
summary(sa3_mc1_re4_list)
summary(sa3_mc1_re5_list)

# Now get population proportions -------------------------------------------------------------------

# unique(pop_draw$minority_composition_index) = c(0,1,2). 0=both, 1=low, 2=high
pop_draw %>% summarise(n()/dim(pop_draw)[1])
pop_draw %>% filter(school_achievement_index==3, minority_composition_index==1) %>% summarise(n()/dim(pop_draw)[1])
pop_draw %>% filter(school_achievement_index==1, race_eth==2) %>% summarise(n()/dim(pop_draw)[1])
pop_draw %>% filter(school_achievement_index==3, minority_composition_index==1, race_eth==1) %>% summarise(n()/dim(pop_draw)[1])
pop_draw %>% filter(school_achievement_index==3, minority_composition_index==1, race_eth==2) %>% summarise(n()/dim(pop_draw)[1])
pop_draw %>% filter(school_achievement_index==3, minority_composition_index==1, race_eth==3) %>% summarise(n()/dim(pop_draw)[1])
pop_draw %>% filter(school_achievement_index==3, minority_composition_index==1, race_eth==4) %>% summarise(n()/dim(pop_draw)[1])
pop_draw %>% filter(school_achievement_index==3, minority_composition_index==1, race_eth==5) %>% summarise(n()/dim(pop_draw)[1])

