rm(list=ls())
gc()

N = 20 # number of simulations
ns = 1000 # number of posterior predictive draws
subsample_size = 1
cores_to_use = 20 # number of CPUs to use


# build population ---------------------------------------------------------------------

library(tidyverse)
library(DeclareDesign)
library(survey)
library(brms)
#library(dbarts)
library(parallel)

library(foreach)
library(doParallel)

# have population probabilities for each subgroup
school_achievement_levels = paste0("school",
                                   1:3) # used to be 1:4

minority_composition_levels = paste0("minority",
                                     1:2) # used to be 1:3

gender_levels = c("female", "male")

race_eth_levels = c("asian", "black", "hispanic", "white", "other")

maternal_edu_levels = c("no", "yes")


school_type_ps = expand.grid(1:length(minority_composition_levels),
                             1:length(school_achievement_levels))
colnames(school_type_ps) = c("minority_composition",
                             "school_achievement")
school_type_ps[1:2,1] = 0 # zero is combined minority composition
school_type_ps_final = school_type_ps[-1,]

pop_draw = readRDS("pop_draw.rds")
PS_mat = readRDS("PS_mat.rds")

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
  
  
  data_65$student_sampling_prob = rbinom(length(data_65$prev_gpa),
                                         size=1,
                                         prob=brms::inv_logit_scaled(data_65$prev_gpa)) # sample students with probability according to this. Mean response rate should be around 92%
  
  print(data_65 %>% group_by(school_achievement_index) %>% summarise(sum(student_sampling_prob)/n()))
  print(sum(data_65$student_sampling_prob)/dim(data_65)[1])
  data_65[data_65$student_sampling_prob==1,]
}

my_sampling_custom_ = declare_sampling(handler = stratifiedschoolsample_function, 
                                       target_strata_samplesize=c(28,34,32,19,27))

# simulation loop -------------------------------------------------------------

# lists that store models
ate_unweighted_regression_list = list()
ate_weighted_svyregression_list = list()
ate_weighted_svyregression_offset_list = list()


# lists that store ate estimates
# MRP 
ATE_ps_estimates = c()
ATE_ps_estimates_sd = c()
#median_ps_ate_estimates = c()

# svy regression with raked weights
ate_weighted_svyregression_coef_list = c() # offset
ate_weighted_svyregression_coef_sd_list = c()

# ordinary regression
ate_unweighted_svyregression_coef_list = c()
ate_unweighted_svyregression_coef_sd_list = c()


# make raking population distributions
minority_x_schoolachievement_df = pop_draw %>% group_by(school_achievement_index, minority_composition_index) %>% summarise(Freq = n()) %>% as.data.frame
minority_x_schoolachievement_df$strata_id = paste0(minority_x_schoolachievement_df$minority_composition_index, "_", minority_x_schoolachievement_df$school_achievement_index)
minority_x_schoolachievement_df = minority_x_schoolachievement_df %>% select(strata_id, Freq)
gender_df = pop_draw %>% group_by(gender) %>% summarise(Freq = n()) %>% as.data.frame
race_eth_df = pop_draw %>% group_by(race_eth) %>% summarise(Freq = n()) %>% as.data.frame

stratified_cluster_sample_first = sample_n(my_sampling_custom_(pop_draw), 500)
# fit prelim brms models
current_models_first = list()

print("Fitting prelim. brms model 1")
current_models_first[[1]] = brms::brm(formula = prev_gpa | trunc(lb=0, ub = 4.33) ~ maternal_edu + gender + (1|race_eth) + (1|school) + (1|minority_composition_index * school_achievement_index),
                                      prior = c(
                                        prior(normal(0, .25), class=b),
                                        prior(normal(2.7,.25), class=Intercept), # change sd to 2
                                        prior(normal(0, .25), class = sd),
                                        prior(normal(0, .25), class = sigma)
                                      ),
                                      data = stratified_cluster_sample_first,
                                      chains = 1,
                                      core = 1,
                                      control = list(adapt_delta = 0.99)
                                      )

print("Fitting prelim. brms model 2")
current_models_first[[2]] = brms::brm(formula = post_gpa | trunc(lb=0, ub=4.33)  ~ offset(prev_gpa) +  Z*maternal_edu + Z*gender + (Z|race_eth) + (Z|school) + (Z|minority_composition_index * school_achievement_index),
                                      prior = c(
                                        prior(normal(0, .125), class=b),
                                        prior(normal(0, .125), class=Intercept),
                                        prior(normal(0, .125), class = sd),
                                        prior(normal(0, .125), class = sigma)
                                      ),
                                      data = stratified_cluster_sample_first,
                                      chains = 1,
                                      core = 1,
                                      control = list(adapt_delta = 0.9)
)



# make cluster 
#cl = makeCluster(cores_to_use)

# number of cores to allocate for parallel computing
registerDoParallel(cores=cores_to_use)
#registerDoParallel(cl)



modelfits = foreach(r=1:N, .errorhandling = "pass") %dopar% {
  # get stratified cluster sample
  stratified_cluster_sample = my_sampling_custom_(pop_draw)
  
  if (subsample_size<1) {
    print(paste("subsample", r))
    stratified_cluster_sample = sample_n(stratified_cluster_sample, round(dim(stratified_cluster_sample)[1] * subsample_size))
  }
  print(paste("done taking sample", r))
  
  stratified_cluster_sample$strata_id = paste0(stratified_cluster_sample$minority_composition_index,
                                               "_",
                                               stratified_cluster_sample$school_achievement_index)
  
  print(paste("Stratified cluster sample size", r, ":", dim(stratified_cluster_sample)[1]))
  
  # ATE ------------------------------------------------------------------
  # stratified cluster design. initialize with equal weights for every person
  stratified_cluster_sample_svgdesign_unweighted = svydesign(id=~school,
                                                             strata=~strata_id,
                                                             data=stratified_cluster_sample)
  
  # rake
  stratified_cluster_sample_svgdesign_rake = rake(stratified_cluster_sample_svgdesign_unweighted,
                                                  list(~strata_id,
                                                       ~gender,
                                                       ~race_eth),
                                                  list(minority_x_schoolachievement_df,
                                                       gender_df,
                                                       race_eth_df))
  
  
  # get weights
  raked_weights = weights(stratified_cluster_sample_svgdesign_rake)
  
  # svy regression with raked weights
  stratified_cluster_sample_svgdesign_weighted = svydesign(id=~school,
                                                           strata=~strata_id,
                                                           data=stratified_cluster_sample,
                                                           weights=raked_weights
  )
  
  # unweighted regression, offset
  ate_unweighted_regression_temp = lm(post_gpa ~ Z +
                                        offset(prev_gpa) + as.factor(school) + as.factor(race_eth) + maternal_edu + gender,
                                      data=stratified_cluster_sample)
  
  # svy regression with raked weights, offset
  ate_weighted_svyregression_temp = svyglm(post_gpa ~ Z + offset(prev_gpa) + as.factor(race_eth) + maternal_edu + gender
                                           ,design=stratified_cluster_sample_svgdesign_weighted,
                                           rescale=TRUE)
  
  
  # CATE - SCHOOL ACHIEVEMENT 3, MIN. COMP 1 -------------------------------

  # unweighted regression, offset
  ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1 = lm(post_gpa ~ Z +
                                                                  offset(prev_gpa) + as.factor(school) + as.factor(race_eth) + maternal_edu + gender,
                                                                data = stratified_cluster_sample %>% filter(school_achievement_index==3, 
                                                                                                            minority_composition_index==1))
  
  # svy regression with raked weights, offset
  ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1 = svyglm(post_gpa ~ Z + offset(prev_gpa) + as.factor(race_eth) + maternal_edu + gender
                                                                                 ,design=stratified_cluster_sample_svgdesign_weighted,
                                                                                 rescale=TRUE,
                                                                                 subset=(school_achievement_index==3 & minority_composition_index==1))
  
  ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_noprecision = svyglm(post_gpa ~ Z + offset(prev_gpa)
                                                                                             ,design=stratified_cluster_sample_svgdesign_weighted,
                                                                                             rescale=TRUE,
                                                                                             subset=(school_achievement_index==3 & minority_composition_index==1))
  
  # CATE - SCHOOL ACHIEVEMENT LEVEL 1, AFRICAN AMERICAN -------------------
  
  ate_unweighted_regression_temp_CATE_school_achievement_1_race_eth_2 = lm(post_gpa ~ Z +
                                                                             offset(prev_gpa) + as.factor(school) + maternal_edu + gender,
                                                                           data = stratified_cluster_sample %>% filter(school_achievement_index==1,
                                                                                                                       race_eth==2))
  
  # 
  ate_weighted_svyregression_temp_school_achievement_index_1_race_eth_2 = svyglm(post_gpa ~ Z + offset(prev_gpa) + maternal_edu + gender
                                                                      ,design=stratified_cluster_sample_svgdesign_weighted,
                                                                      rescale=TRUE,
                                                                      subset=(school_achievement_index==1 & race_eth==2))
  
  ate_weighted_svyregression_temp_school_achievement_index_1_race_eth_2_noprecision = svyglm(post_gpa ~ Z + offset(prev_gpa)
                                                                                             ,design=stratified_cluster_sample_svgdesign_weighted,
                                                                                             rescale=TRUE,
                                                                                             subset=(school_achievement_index==1 & race_eth==2))
  
  
  

  # CATE - SCHOOL ACHIEVEMENT 3, MIN. COMP 1, ALL 5 RACE/ETH ---------------
  ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_templist = list()
  ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_templist = list()
  
  ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_model_templist = list()
  ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_model_templist = list()
  
  for (i in 1:length(race_eth_levels)) {
    
    # unweighted regression. offset
    ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_templist[[i]] = summary(lm(post_gpa ~ Z +
                                                                                                     offset(prev_gpa) + as.factor(school) + maternal_edu + gender,
                                                                                                   data = stratified_cluster_sample %>% filter(school_achievement_index==3,
                                                                                                                                               minority_composition_index==1,
                                                                                                                                               race_eth==i)))$coefficient
    
    # svy regression with raked weights, offset
    ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_templist[[i]] = summary(svyglm(post_gpa ~ Z + offset(prev_gpa) + maternal_edu + gender
                                                                                                         ,design=stratified_cluster_sample_svgdesign_weighted,
                                                                                                         rescale=TRUE,
                                                                                                         subset=(school_achievement_index==3 & minority_composition_index==1 & race_eth==i)))$coefficient
    
    
    ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_model_templist[[i]] = lm(post_gpa ~ Z +
                                                                                                   offset(prev_gpa) + as.factor(school) + maternal_edu + gender,
                                                                                                 data = stratified_cluster_sample %>% filter(school_achievement_index==3,
                                                                                                                                             minority_composition_index==1,
                                                                                                                                             race_eth==i))
    
    # svy regression with raked weights, offset
    ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_model_templist[[i]] = svyglm(post_gpa ~ Z + offset(prev_gpa) + maternal_edu + gender
                                                                                                       ,design=stratified_cluster_sample_svgdesign_weighted,
                                                                                                       rescale=TRUE,
                                                                                                       subset=(school_achievement_index==3 & minority_composition_index==1 & race_eth==i))
  }
  
  print(paste("done linear models", r))
  # ----------------------------------------------------
  
  

  current_models = list()
  print(paste("step 2 model", r))
  current_models[[1]] = update(current_models_first[[1]], newdata = stratified_cluster_sample)
  print(paste("Done step 2 model",
              r, 
              ". Divergent transitions:",
              sum(subset(nuts_params(current_models[[1]]), Parameter == "divergent__")$Value)
  ))
  
  
  
  #tryCatch({
  print(paste("step 3 model", r))
  current_models[[2]] = update(current_models_first[[2]], newdata = stratified_cluster_sample,
                               control = list(adapt_delta = 0.9))
  print(paste("Done step 3 model",
              r, 
              ". Divergent transitions:",
              sum(subset(nuts_params(current_models[[2]]), Parameter == "divergent__")$Value)
  ))
  
  
  print(paste("done hierarchical models", r))
  
  # x.test is to make predict function work
  stratified_cluster_sample_bart = stratified_cluster_sample[,c("minority_composition_index",
                                                                "school_achievement_index",
                                                                "school",
                                                                "gender",
                                                                "prev_gpa",
                                                                "race_eth",
                                                                "maternal_edu",
                                                                "Z")]
  stratified_cluster_sample_bart$school = factor(stratified_cluster_sample_bart$school, levels = unique(PS_mat$school))
  
  bartFit = dbarts::bart(x.train = stratified_cluster_sample_bart,
                 y.train = stratified_cluster_sample$post_gpa,
                 keeptrees=TRUE)
  
  print(paste("done BART fit", r))


  
  # In-sample MRP --------------------------------------
  
  PS_mat_insample = PS_mat[PS_mat$school %in% unique(stratified_cluster_sample$school),]
  
  PPC_step_2_samples_insample = posterior_predict(current_models[[1]],
                                                  newdata=PS_mat_insample,
                                                  summary=FALSE,
                                                  nsamples=ns) # the number of posterior predictive samples
  
  print(paste("done PPC_step_2_samples insample", r))
  
  # Use median of prev_gpa for posterior predictive distribution
  PS_mat_prev_gpa_median_insample = cbind(PS_mat_insample,
                                 prev_gpa = apply(X=PPC_step_2_samples_insample,
                                                  MARGIN=2,
                                                  FUN=mean)
  )
  
  PPC_step_3_list_insample = lapply(X = list(
    list(current_models[[2]],
         cbind(PS_mat_prev_gpa_median_insample,Z=0),
         ns),
    
    list(current_models[[2]],
         cbind(PS_mat_prev_gpa_median_insample,Z=1),
         ns)
  ),
  FUN = function(mod_data_ns_list){
    brms::posterior_predict(object = mod_data_ns_list[[1]],
                            newdata = mod_data_ns_list[[2]],
                            summary=FALSE,
                            nsamples = mod_data_ns_list[[3]],
                            allow_new_levels=TRUE,
                            sample_new_levels="gaussian")
  }
  )
  
  
  # ATE, CATE for MRP INSAMPLE  -----------------------------
  multilevel_diff_insample = sweep((PPC_step_3_list_insample[[2]] - PPC_step_3_list_insample[[1]]),
                          2,
                          PS_mat_insample$Freq,
                          "*")
  
  rm(PPC_step_3_list_insample)
  gc()
  
  ate_insample = rowSums(multilevel_diff_insample/sum(PS_mat_insample$Freq)
  )
  
  cate_school_achievement_3_min_comp_1_insample = rowSums((multilevel_diff_insample)[,which(PS_mat_insample$school_achievement_index==3 & 
                                                                                              PS_mat_insample$minority_composition_index==1)])/sum(PS_mat_insample$Freq[which(PS_mat_insample$school_achievement_index==3 & 
                                                                                                                                                                                PS_mat_insample$minority_composition_index==1)])
  
  cate_school_achievement_1_race_eth_2_insample = rowSums((multilevel_diff_insample)[,which(PS_mat_insample$school_achievement_index==1 & 
                                                                                              PS_mat_insample$race_eth==2)])/sum(PS_mat_insample$Freq[which(PS_mat_insample$school_achievement_index==1 & 
                                                                                                                                                              PS_mat_insample$race_eth==2)])
  
  cate_school_achievement_3_min_comp_1_race_eth_templist_insample = list()
  for (i in 1:length(race_eth_levels)) {
    cate_school_achievement_3_min_comp_1_race_eth_templist_insample[[i]] = rowSums((multilevel_diff_insample)[,which(PS_mat_insample$school_achievement_index==3 & 
                                                                                                                       PS_mat_insample$minority_composition_index==1 & 
                                                                                                                       PS_mat_insample$race_eth==i)])/sum(PS_mat_insample$Freq[which(PS_mat_insample$school_achievement_index==3 & 
                                                                                                                                                                                       PS_mat_insample$minority_composition_index==1 & 
                                                                                                                                                                                       PS_mat_insample$race_eth==i)])  
  }
  
  rm(multilevel_diff_insample)
  gc()
  
  print(paste("done posterior predicts for brms MRP-insample", r))
  
  
  
  PPC_step_2_samples_fullPS = posterior_predict(current_models[[1]],
                                                newdata=PS_mat,
                                                summary=FALSE,
                                                nsamples=ns,
                                                allow_new_levels=TRUE,
                                                sample_new_levels="gaussian") # the number of posterior predictive samples
  
  print(paste("done PPC_step_2_samples full PS", r))
  
  # Use median of prev_gpa for posterior predictive distribution
  PS_mat_prev_gpa_median_fullPS = cbind(PS_mat,
                                        prev_gpa = apply(X=PPC_step_2_samples_fullPS,
                                                         MARGIN=2,
                                                         FUN=mean)
  )
  
  print(paste("done PS_mat_prev_gpa_median full PS", r))
  
  rm(PPC_step_2_samples_fullPS)
  gc()
  
  
  PPC_step_3_list_fullPS = lapply(X = list(
    list(current_models[[2]],
         cbind(PS_mat_prev_gpa_median_fullPS,Z=0),
         ns),
    
    list(current_models[[2]],
         cbind(PS_mat_prev_gpa_median_fullPS,Z=1),
         ns)
  ),
  FUN = function(mod_data_ns_list){
    brms::posterior_predict(object = mod_data_ns_list[[1]],
                            newdata = mod_data_ns_list[[2]],
                            summary=FALSE,
                            nsamples = mod_data_ns_list[[3]],
                            allow_new_levels=TRUE,
                            sample_new_levels="gaussian")
  }
  )
  
  multilevel_diff_fullPS = sweep((PPC_step_3_list_fullPS[[2]] - PPC_step_3_list_fullPS[[1]]),
                                 2,
                                 PS_mat$Freq,
                                 "*")
  
  rm(PPC_step_3_list_fullPS)
  gc()
  
  ate_fullPS = rowSums(multilevel_diff_fullPS/sum(PS_mat$Freq)
  )
  
  cate_school_achievement_3_min_comp_1_fullPS = rowSums((multilevel_diff_fullPS)[,which(PS_mat$school_achievement_index==3 & 
                                                                                          PS_mat$minority_composition_index==1)])/sum(PS_mat$Freq[which(PS_mat$school_achievement_index==3 & 
                                                                                                                                                          PS_mat$minority_composition_index==1)])
  
  cate_school_achievement_1_race_eth_2_fullPS = rowSums((multilevel_diff_fullPS)[,which(PS_mat$school_achievement_index==1 & 
                                                                                          PS_mat$race_eth==2)])/sum(PS_mat$Freq[which(PS_mat$school_achievement_index==1 & 
                                                                                                                                        PS_mat$race_eth==2)])
  
  cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS = list()
  for (i in 1:length(race_eth_levels)) {
    cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS[[i]] = rowSums((multilevel_diff_fullPS)[,which(PS_mat$school_achievement_index==3 & 
                                                                                                                   PS_mat$minority_composition_index==1 & 
                                                                                                                   PS_mat$race_eth==i)])/sum(PS_mat$Freq[which(PS_mat$school_achievement_index==3 & 
                                                                                                                                                                 PS_mat$minority_composition_index==1 & 
                                                                                                                                                                 PS_mat$race_eth==i)])  
  }
  
  rm(multilevel_diff_fullPS)
  gc()
  
  print(paste("done posterior predicts for brms MRP-fullPS (imputation with Prev-GPA mean)", r))
  
  
  # ATE, CATE for MRP ----------------------------------
  
  print(paste("starting proposed MRP method", r))
  
  PPC_step_2_samples = posterior_predict(current_models[[1]],
                                         newdata=PS_mat,
                                         nsamples=ns,
                                         allow_new_levels=TRUE,
                                         sample_new_levels="gaussian",
                                         summary=FALSE
  )
  
  # Use median of prev_gpa for posterior predictive distribution
  PS_mat_prev_gpa_median = cbind(PS_mat,
                                 prev_gpa = apply(X=PPC_step_2_samples,
                                                  MARGIN=2,
                                                  FUN=mean)
  )
  
  
  PPD_postgpa_Z0_ns_newmethod_1 = matrix(0, ns, dim(PS_mat)[1])
  PPD_postgpa_Z1_ns_newmethod_1 = matrix(0, ns, dim(PS_mat)[1])
  
  post_gpa_posterior_sigma = posterior_samples(current_models[[2]], 
                                               pars = c("sigma"))
  
  
  # prevgpa_to_postgpa_func is used in the for loop below
  prevgpa_to_postgpa_func = function(mod_data_ns_list){
    truncnorm::rtruncnorm(n = dim(PS_mat)[1],
                          a=0,
                          b=4.33,
                          mean=(brms::posterior_linpred(object = mod_data_ns_list[[1]],
                                                        newdata = mod_data_ns_list[[2]],
                                                        summary=FALSE,
                                                        subset = c(mod_data_ns_list[[3]]),
                                                        allow_new_levels=TRUE,
                                                        sample_new_levels="gaussian")
                          )
                          ,
                          sd = post_gpa_posterior_sigma$sigma[mod_data_ns_list[[3]]]
    )
  }
  
  
  for (j in 1:ns) {

    
    if ((j %% 50) == 0) {
      print(paste("j r:",j, r))
    }
    
    temp_list = lapply(X = list(
      list(current_models[[2]],
           data.frame(PS_mat, 
                      prev_gpa = PPC_step_2_samples[j,],
                      Z=0),
           j
      ),
      
      list(current_models[[2]],
           data.frame(PS_mat, 
                      prev_gpa = PPC_step_2_samples[j,],
                      Z=1),
           j
      )
    ),
    

    FUN = prevgpa_to_postgpa_func
    )
    
    
    PPD_postgpa_Z0_ns_newmethod_1[j,] = temp_list[[1]]
    PPD_postgpa_Z1_ns_newmethod_1[j,] = temp_list[[2]]
    
  }
  
  # remove to save space
  rm(current_models)
  rm(PPC_step_2_samples)
  gc()
  
  print(paste(summary(rowSums(sweep(PPD_postgpa_Z1_ns_newmethod_1 - PPD_postgpa_Z0_ns_newmethod_1, 2,
                                    PS_mat$Freq,
                                    "*"))/sum(PS_mat$Freq)), r))
  
  rm(temp_list)
  gc()
  
  print(paste("Done proposed MRP PPD_postgpa_Z0_ns_newmethod_1", r))
  ###
  
  multilevel_diff = sweep(PPD_postgpa_Z1_ns_newmethod_1 - PPD_postgpa_Z0_ns_newmethod_1,
                          2,
                          PS_mat$Freq,
                          "*")
  
  rm(PPD_postgpa_Z1_ns_newmethod_1)
  rm(PPD_postgpa_Z0_ns_newmethod_1)
  gc()
  
  ate = rowSums(multilevel_diff/sum(PS_mat$Freq)
  )
  
  cate_school_achievement_3_min_comp_1 = rowSums((multilevel_diff)[,which(PS_mat$school_achievement_index==3 & PS_mat$minority_composition_index==1)])/sum(PS_mat$Freq[which(PS_mat$school_achievement_index==3 & PS_mat$minority_composition_index==1)])
  
  cate_school_achievement_1_race_eth_2 = rowSums((multilevel_diff)[,which(PS_mat$school_achievement_index==1 & PS_mat$race_eth==2)])/sum(PS_mat$Freq[which(PS_mat$school_achievement_index==1 & PS_mat$race_eth==2)])
  
  #ptm <- proc.time()
  cate_school = c()
  cate_sd_school = c()
  for (s in unique(PS_mat$school)) {
    #print(s)
    cate_school = c(cate_school, mean(rowSums((multilevel_diff)[,which(PS_mat$school == s)])/sum(PS_mat$Freq[which(PS_mat$school == s)]))
    )
    cate_sd_school = c(cate_sd_school,
                       sd(rowSums((multilevel_diff)[,which(PS_mat$school == s)])/sum(PS_mat$Freq[which(PS_mat$school == s)]))
                       )
    
  }
  
  cate_school = data.frame(school=unique(PS_mat$school), CATE=cate_school, CATE_sd=cate_sd_school)
  cate_school$in_sample = cate_school$school %in% unique(stratified_cluster_sample$school)
  #proc.time() - ptm
  
  cate_school_achievement_3_min_comp_1_race_eth_templist = list()
  for (i in 1:length(race_eth_levels)) {
    cate_school_achievement_3_min_comp_1_race_eth_templist[[i]] = rowSums((multilevel_diff)[,which(PS_mat$school_achievement_index==3 & PS_mat$minority_composition_index==1 & PS_mat$race_eth==i)])/sum(PS_mat$Freq[which(PS_mat$school_achievement_index==3 & PS_mat$minority_composition_index==1 & PS_mat$race_eth==i)])  
  }
  
  rm(multilevel_diff)
  gc()

  print(paste("done posterior predicts for brms", r))
  
  # -------------------------------------------------
  
  PS_mat_prev_gpa_median_bart_Z_1 = cbind(PS_mat_prev_gpa_median[,c("minority_composition_index",
                                                                    "school_achievement_index",
                                                                    "school",
                                                                    "gender",
                                                                    "prev_gpa",
                                                                    "race_eth",
                                                                    "maternal_edu")], Z=1)
  PS_mat_prev_gpa_median_bart_Z_1$school = factor(PS_mat_prev_gpa_median_bart_Z_1$school, levels = unique(PS_mat$school))
  
  PS_mat_prev_gpa_median_bart_Z_0 = cbind(PS_mat_prev_gpa_median[,c("minority_composition_index",
                                                                    "school_achievement_index",
                                                                    "school",
                                                                    "gender",
                                                                    "prev_gpa",
                                                                    "race_eth",
                                                                    "maternal_edu")], Z=0)
  PS_mat_prev_gpa_median_bart_Z_0$school = factor(PS_mat_prev_gpa_median_bart_Z_0$school, levels = unique(PS_mat$school))
  
  PS_mat_bartFit_predict_Z_1 = predict(bartFit,newdata = PS_mat_prev_gpa_median_bart_Z_1,
                                       type="ppd")
  
  PS_mat_bartFit_predict_Z_0 = predict(bartFit,newdata = PS_mat_prev_gpa_median_bart_Z_0,
                                       type="ppd")
  
  
  bart_diff = sweep((PS_mat_bartFit_predict_Z_1 - PS_mat_bartFit_predict_Z_0),
                    2,
                    PS_mat$Freq,
                    "*")
  
  rm(PS_mat_prev_gpa_median_bart_Z_1)
  gc()
  rm(PS_mat_prev_gpa_median_bart_Z_0)
  gc()
  rm(PS_mat_bartFit_predict_Z_1)
  gc()
  rm(PS_mat_bartFit_predict_Z_0)
  gc()

  bart_ate = rowSums(bart_diff/sum(PS_mat$Freq)
  )
  
  bart_cate_school_achievement_3_min_comp_1 = rowSums((bart_diff)[,which(PS_mat$school_achievement_index==3 & PS_mat$minority_composition_index==1)])/sum(PS_mat$Freq[which(PS_mat$school_achievement_index==3 & PS_mat$minority_composition_index==1)])
  
  bart_cate_school_achievement_1_race_eth_2 = rowSums((bart_diff)[,which(PS_mat$school_achievement_index==1 & PS_mat$race_eth==2)])/sum(PS_mat$Freq[which(PS_mat$school_achievement_index==1 & PS_mat$race_eth==2)])
  
  bart_cate_school = c()
  bart_cate_sd_school = c()
  for (s in unique(PS_mat$school)) {
    #print(s)
    
    bart_cate_school = c(bart_cate_school, mean(rowSums((bart_diff)[,which(PS_mat$school == s)])/sum(PS_mat$Freq[which(PS_mat$school == s)]))
    )
    bart_cate_sd_school = c(bart_cate_sd_school, sd(rowSums((bart_diff)[,which(PS_mat$school == s)])/sum(PS_mat$Freq[which(PS_mat$school == s)])))
  }
  
  bart_cate_school = data.frame(school=unique(PS_mat$school), CATE=bart_cate_school, CATE_sd = bart_cate_sd_school)

  bart_cate_school_achievement_3_min_comp_1_race_eth_templist = list()
  for (i in 1:length(race_eth_levels)) {
    bart_cate_school_achievement_3_min_comp_1_race_eth_templist[[i]] = rowSums((bart_diff)[,which(PS_mat$school_achievement_index==3 & PS_mat$minority_composition_index==1 & PS_mat$race_eth==i)])/sum(PS_mat$Freq[which(PS_mat$school_achievement_index==3 & PS_mat$minority_composition_index==1 & PS_mat$race_eth==i)])
  }

  print(paste("done posterior predicts for out of sample BART", r))
  
  stratified_cluster_sample_schools = unique(stratified_cluster_sample$school)
  
  insample_bart_ate = rowSums(bart_diff[,which(PS_mat$school %in% stratified_cluster_sample_schools)])/sum(PS_mat$Freq[PS_mat$school %in% stratified_cluster_sample_schools])
  
  insample_bart_cate_school_achievement_3_min_comp_1 = rowSums(bart_diff[,which(PS_mat$school %in% stratified_cluster_sample_schools &
                                                                                  PS_mat$school_achievement_index==3 & 
                                                                                  PS_mat$minority_composition_index==1)])/sum(PS_mat$Freq[which(PS_mat$school %in% stratified_cluster_sample_schools &
                                                                                                                                                  PS_mat$school_achievement_index==3 & 
                                                                                                                                                  PS_mat$minority_composition_index==1)])
  
  insample_bart_cate_school_achievement_1_race_eth_2 = rowSums(bart_diff[,which(PS_mat$school %in% stratified_cluster_sample_schools &
                                                                                  PS_mat$school_achievement_index==1 & 
                                                                                  PS_mat$race_eth==2)])/sum(PS_mat$Freq[PS_mat$school %in% stratified_cluster_sample_schools &
                                                                                                                          PS_mat$school_achievement_index==1 & 
                                                                                                                          PS_mat$race_eth==2])
  
  insample_bart_cate_school_achievement_3_min_comp_1_race_eth_templist = list()
  for (i in 1:length(race_eth_levels)) {
    insample_bart_cate_school_achievement_3_min_comp_1_race_eth_templist[[i]] = rowSums(bart_diff[,which(PS_mat$school %in% stratified_cluster_sample_schools &
                                                                                                           PS_mat$school_achievement_index==3 & 
                                                                                                           PS_mat$minority_composition_index==1 &
                                                                                                           PS_mat$race_eth==i)])/sum(PS_mat$Freq[which(PS_mat$school %in% stratified_cluster_sample_schools &
                                                                                                                                                         PS_mat$school_achievement_index==3 & 
                                                                                                                                                         PS_mat$minority_composition_index==1 &
                                                                                                                                                         PS_mat$race_eth==i)])
  }
  
  rm(bart_diff)
  gc()
  
  print(paste("done posterior predicts for in-sample BART. Starting BART using posterior dist on stratified cluster sample", r))
  # BART applied like in ACIC 2017 paper. Not prev-gpa mean imputation ---------------------------------
  stratified_cluster_sample_bart_Z_0 = stratified_cluster_sample_bart
  stratified_cluster_sample_bart_Z_1 = stratified_cluster_sample_bart
  stratified_cluster_sample_bart_Z_0$Z = 0
  stratified_cluster_sample_bart_Z_1$Z = 1

  # 
  bartFit_usingposteriordistribution = dbarts::bart(x.train = stratified_cluster_sample_bart,
                                                    y.train = stratified_cluster_sample$post_gpa,
                                                    x.test = rbind(stratified_cluster_sample_bart_Z_0, stratified_cluster_sample_bart_Z_1),
                                                    keeptrees=TRUE)
  
  bartFit_usingposteriordistribution_test_Z0 = bartFit_usingposteriordistribution$yhat.test[,1:dim(stratified_cluster_sample)[1]]
  bartFit_usingposteriordistribution_test_Z1 = bartFit_usingposteriordistribution$yhat.test[,(dim(stratified_cluster_sample)[1]+1):(2*dim(stratified_cluster_sample)[1])]
  
  
  # ate 
  bart_usingposteriordistribution_ate = rowMeans(bartFit_usingposteriordistribution_test_Z1 - bartFit_usingposteriordistribution_test_Z0)
  
  # cate for sa3 mc1
  bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1 = rowMeans((bartFit_usingposteriordistribution_test_Z1 - bartFit_usingposteriordistribution_test_Z0)[,which(stratified_cluster_sample$school_achievement_index==3 & 
                                                                                                                                                                                     stratified_cluster_sample$minority_composition_index==1)])
  # cate for sa1 re2
  bart_usingposteriordistribution_cate_school_achievement_1_race_eth_2 = rowMeans((bartFit_usingposteriordistribution_test_Z1 - bartFit_usingposteriordistribution_test_Z0)[,which(stratified_cluster_sample$school_achievement_index==1 & 
                                                                                                                                                                                     stratified_cluster_sample$race_eth==2)])
  
  bart_usingposteriordistribution_cate_school_list = list() # in-sample CATEs using BART without prev-GPA imputation
  for (s in unique(stratified_cluster_sample$school)) {
    bart_usingposteriordistribution_cate_school_list[[s]] = rowMeans((bartFit_usingposteriordistribution_test_Z1 - bartFit_usingposteriordistribution_test_Z0)[,which(stratified_cluster_sample$school==s)])
  }
  
  bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist = list()
  for (i in 1:length(race_eth_levels)) {
    bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist[[i]] = rowMeans((bartFit_usingposteriordistribution_test_Z1 - bartFit_usingposteriordistribution_test_Z0)[,which(stratified_cluster_sample$school_achievement_index==3 & 
                                                                                                                                                                                                              stratified_cluster_sample$minority_composition_index==1 &
                                                                                                                                                                                                              stratified_cluster_sample$race_eth==i
                                                                                                                                                                                                              )])
  }
  
  rm(bartFit_usingposteriordistribution_test_Z0)
  rm(bartFit_usingposteriordistribution_test_Z1)
  rm(stratified_cluster_sample_bart_Z_0)
  rm(stratified_cluster_sample_bart_Z_1)
  rm(bartFit_usingposteriordistribution)
  rm(PS_mat_prev_gpa_median)
  rm(PS_mat_prev_gpa_median_insample)
  rm(bartFit)
  gc()
  
  print(paste("done iteration", r))
  
  list(summary(ate), 
       summary(bart_ate),
       sd(ate),
       sd(bart_ate),
       summary(ate_weighted_svyregression_temp)$coefficient,
       summary(ate_unweighted_regression_temp)$coefficient,
       
       summary(cate_school_achievement_3_min_comp_1),
       summary(bart_cate_school_achievement_3_min_comp_1),
       sd(cate_school_achievement_3_min_comp_1),
       sd(bart_cate_school_achievement_3_min_comp_1),
       summary(ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1)$coefficient,
       summary(ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1)$coefficient,
       summary(ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_noprecision)$coefficient,
       
       summary(cate_school_achievement_1_race_eth_2),
       summary(bart_cate_school_achievement_1_race_eth_2),
       sd(cate_school_achievement_1_race_eth_2),
       sd(bart_cate_school_achievement_1_race_eth_2),
       summary(ate_unweighted_regression_temp_CATE_school_achievement_1_race_eth_2)$coefficient,
       summary(ate_weighted_svyregression_temp_school_achievement_index_1_race_eth_2)$coefficient,
       summary(ate_weighted_svyregression_temp_school_achievement_index_1_race_eth_2_noprecision)$coefficient,

       cate_school,
       bart_cate_school,
       
       summary(insample_bart_ate),
       sd(insample_bart_ate),
       summary(insample_bart_cate_school_achievement_3_min_comp_1),
       sd(insample_bart_cate_school_achievement_3_min_comp_1),
       summary(insample_bart_cate_school_achievement_1_race_eth_2),
       sd(insample_bart_cate_school_achievement_1_race_eth_2),
       
       lapply(X=cate_school_achievement_3_min_comp_1_race_eth_templist, FUN=summary),
       lapply(X=bart_cate_school_achievement_3_min_comp_1_race_eth_templist, FUN=summary),
       lapply(X=insample_bart_cate_school_achievement_3_min_comp_1_race_eth_templist, FUN=summary),
       lapply(X=cate_school_achievement_3_min_comp_1_race_eth_templist, FUN=sd),
       lapply(X=bart_cate_school_achievement_3_min_comp_1_race_eth_templist, FUN=sd),
       lapply(X=insample_bart_cate_school_achievement_3_min_comp_1_race_eth_templist, FUN=sd),
       ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_templist,
       ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_templist,
       
       summary(ate_insample),
       sd(ate_insample),
       summary(cate_school_achievement_3_min_comp_1_insample),
       sd(cate_school_achievement_3_min_comp_1_insample),
       summary(cate_school_achievement_1_race_eth_2_insample),
       sd(cate_school_achievement_1_race_eth_2_insample),
       lapply(X=cate_school_achievement_3_min_comp_1_race_eth_templist_insample,
              FUN=summary),
       lapply(X=cate_school_achievement_3_min_comp_1_race_eth_templist_insample,
              FUN=sd),
       
       summary(bart_usingposteriordistribution_ate),
       sd(bart_usingposteriordistribution_ate),
       summary(bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1),
       sd(bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1),
       summary(bart_usingposteriordistribution_cate_school_achievement_1_race_eth_2),
       sd(bart_usingposteriordistribution_cate_school_achievement_1_race_eth_2),
       lapply(X=bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist,
              FUN=summary),
       lapply(X=bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist,
              FUN=sd),
       bart_usingposteriordistribution_cate_school_list,
       
       c(quantile(ate, 0.025), quantile(ate, 0.975)),
       c(quantile(ate_insample, 0.025), quantile(ate_insample, 0.975)),
       c(quantile(bart_usingposteriordistribution_ate, 0.025), quantile(bart_usingposteriordistribution_ate, 0.975)),
       c(quantile(bart_ate, 0.025), quantile(bart_ate, 0.975)),
       c(quantile(insample_bart_ate, 0.025), quantile(insample_bart_ate, 0.975)),
       confint(ate_weighted_svyregression_temp)[2,],
       confint(ate_unweighted_regression_temp)[2,],
       
       c(quantile(cate_school_achievement_3_min_comp_1, 0.025), quantile(cate_school_achievement_3_min_comp_1, 0.975)),
       c(quantile(cate_school_achievement_3_min_comp_1_insample, 0.025), quantile(cate_school_achievement_3_min_comp_1_insample, 0.975)),
       c(quantile(bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1, 0.025), quantile(bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1, 0.975)),
       c(quantile(bart_cate_school_achievement_3_min_comp_1, 0.025), quantile(bart_cate_school_achievement_3_min_comp_1, 0.975)),
       c(quantile(insample_bart_cate_school_achievement_3_min_comp_1, 0.025), quantile(insample_bart_cate_school_achievement_3_min_comp_1, 0.975)),
       confint(ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1)[2,],
       confint(ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1)[2,],
       
       c(quantile(cate_school_achievement_1_race_eth_2, 0.025), quantile(cate_school_achievement_1_race_eth_2, 0.975)),
       c(quantile(cate_school_achievement_1_race_eth_2_insample, 0.025), quantile(cate_school_achievement_1_race_eth_2_insample, 0.975)),
       c(quantile(bart_usingposteriordistribution_cate_school_achievement_1_race_eth_2, 0.025), quantile(bart_usingposteriordistribution_cate_school_achievement_1_race_eth_2, 0.975)),
       c(quantile(bart_cate_school_achievement_1_race_eth_2, 0.025), quantile(bart_cate_school_achievement_1_race_eth_2, 0.975)),
       c(quantile(insample_bart_cate_school_achievement_1_race_eth_2, 0.025), quantile(insample_bart_cate_school_achievement_1_race_eth_2, 0.975)),
       confint(ate_weighted_svyregression_temp_school_achievement_index_1_race_eth_2)[2,],
       confint(ate_unweighted_regression_temp_CATE_school_achievement_1_race_eth_2)[2,],
       
       c(quantile(cate_school_achievement_3_min_comp_1_race_eth_templist[[1]], 0.025), quantile(cate_school_achievement_3_min_comp_1_race_eth_templist[[1]], 0.975)),
       c(quantile(cate_school_achievement_3_min_comp_1_race_eth_templist_insample[[1]], 0.025), quantile(cate_school_achievement_3_min_comp_1_race_eth_templist_insample[[1]], 0.975)),
       c(quantile(bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist[[1]], 0.025), quantile(bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist[[1]], 0.975)),
       c(quantile(bart_cate_school_achievement_3_min_comp_1_race_eth_templist[[1]], 0.025), quantile(bart_cate_school_achievement_3_min_comp_1_race_eth_templist[[1]], 0.975)),
       c(quantile(insample_bart_cate_school_achievement_3_min_comp_1_race_eth_templist[[1]], 0.025), quantile(insample_bart_cate_school_achievement_3_min_comp_1_race_eth_templist[[1]], 0.975)),
       confint(ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_model_templist[[1]])[2,],
       confint(ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_model_templist[[1]])[2,],
       
       c(quantile(cate_school_achievement_3_min_comp_1_race_eth_templist[[2]], 0.025), quantile(cate_school_achievement_3_min_comp_1_race_eth_templist[[2]], 0.975)),
       c(quantile(cate_school_achievement_3_min_comp_1_race_eth_templist_insample[[2]], 0.025), quantile(cate_school_achievement_3_min_comp_1_race_eth_templist_insample[[2]], 0.975)),
       c(quantile(bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist[[2]], 0.025), quantile(bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist[[2]], 0.975)),
       c(quantile(bart_cate_school_achievement_3_min_comp_1_race_eth_templist[[2]], 0.025), quantile(bart_cate_school_achievement_3_min_comp_1_race_eth_templist[[2]], 0.975)),
       c(quantile(insample_bart_cate_school_achievement_3_min_comp_1_race_eth_templist[[2]], 0.025), quantile(insample_bart_cate_school_achievement_3_min_comp_1_race_eth_templist[[2]], 0.975)),
       confint(ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_model_templist[[2]])[2,],
       confint(ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_model_templist[[2]])[2,],
       
       c(quantile(cate_school_achievement_3_min_comp_1_race_eth_templist[[3]], 0.025), quantile(cate_school_achievement_3_min_comp_1_race_eth_templist[[3]], 0.975)),
       c(quantile(cate_school_achievement_3_min_comp_1_race_eth_templist_insample[[3]], 0.025), quantile(cate_school_achievement_3_min_comp_1_race_eth_templist_insample[[3]], 0.975)),
       c(quantile(bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist[[3]], 0.025), quantile(bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist[[3]], 0.975)),
       c(quantile(bart_cate_school_achievement_3_min_comp_1_race_eth_templist[[3]], 0.025), quantile(bart_cate_school_achievement_3_min_comp_1_race_eth_templist[[3]], 0.975)),
       c(quantile(insample_bart_cate_school_achievement_3_min_comp_1_race_eth_templist[[3]], 0.025), quantile(insample_bart_cate_school_achievement_3_min_comp_1_race_eth_templist[[3]], 0.975)),
       confint(ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_model_templist[[3]])[2,],
       confint(ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_model_templist[[3]])[2,],
       
       c(quantile(cate_school_achievement_3_min_comp_1_race_eth_templist[[4]], 0.025), quantile(cate_school_achievement_3_min_comp_1_race_eth_templist[[4]], 0.975)),
       c(quantile(cate_school_achievement_3_min_comp_1_race_eth_templist_insample[[4]], 0.025), quantile(cate_school_achievement_3_min_comp_1_race_eth_templist_insample[[4]], 0.975)),
       c(quantile(bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist[[4]], 0.025), quantile(bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist[[4]], 0.975)),
       c(quantile(bart_cate_school_achievement_3_min_comp_1_race_eth_templist[[4]], 0.025), quantile(bart_cate_school_achievement_3_min_comp_1_race_eth_templist[[4]], 0.975)),
       c(quantile(insample_bart_cate_school_achievement_3_min_comp_1_race_eth_templist[[4]], 0.025), quantile(insample_bart_cate_school_achievement_3_min_comp_1_race_eth_templist[[4]], 0.975)),
       confint(ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_model_templist[[4]])[2,],
       confint(ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_model_templist[[4]])[2,],
       
       c(quantile(cate_school_achievement_3_min_comp_1_race_eth_templist[[5]], 0.025), quantile(cate_school_achievement_3_min_comp_1_race_eth_templist[[5]], 0.975)),
       c(quantile(cate_school_achievement_3_min_comp_1_race_eth_templist_insample[[5]], 0.025), quantile(cate_school_achievement_3_min_comp_1_race_eth_templist_insample[[5]], 0.975)),
       c(quantile(bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist[[5]], 0.025), quantile(bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist[[5]], 0.975)),
       c(quantile(bart_cate_school_achievement_3_min_comp_1_race_eth_templist[[5]], 0.025), quantile(bart_cate_school_achievement_3_min_comp_1_race_eth_templist[[5]], 0.975)),
       c(quantile(insample_bart_cate_school_achievement_3_min_comp_1_race_eth_templist[[5]], 0.025), quantile(insample_bart_cate_school_achievement_3_min_comp_1_race_eth_templist[[5]], 0.975)),
       confint(ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_model_templist[[5]])[2,],
       confint(ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_model_templist[[5]])[2,],
       
       summary(ate_fullPS),
       sd(ate_fullPS),
       summary(cate_school_achievement_3_min_comp_1_fullPS),
       sd(cate_school_achievement_3_min_comp_1_fullPS),
       summary(cate_school_achievement_1_race_eth_2_fullPS),
       sd(cate_school_achievement_1_race_eth_2_fullPS),
       lapply(X=cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS,
              FUN=summary),
       lapply(X=cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS,
              FUN=sd),
       
       c(quantile(cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS[[1]], 0.025), quantile(cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS[[1]], 0.975)),
       c(quantile(cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS[[2]], 0.025), quantile(cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS[[2]], 0.975)),
       c(quantile(cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS[[3]], 0.025), quantile(cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS[[3]], 0.975)),
       c(quantile(cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS[[4]], 0.025), quantile(cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS[[4]], 0.975)),
       c(quantile(cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS[[5]], 0.025), quantile(cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS[[5]], 0.975)),
       
       c(quantile(ate_fullPS, 0.025), quantile(ate_fullPS, 0.975)),
       c(quantile(cate_school_achievement_3_min_comp_1_fullPS, 0.025), quantile(cate_school_achievement_3_min_comp_1_fullPS, 0.975)),
       c(quantile(cate_school_achievement_1_race_eth_2_fullPS, 0.025), quantile(cate_school_achievement_1_race_eth_2_fullPS, 0.975))
       
       )
  
}


print("DONE PARALLEL COMPUTING")

# pop_draw is quite large so we remove it
rm(pop_draw)
gc()

rm(current_models_first)
gc()

save.image(paste0(N, "_", cores_to_use,"_",subsample_size*1000, "_v8_8.RData"))

print("DONE SAVING SIMULATIONS")

rm(list=ls())
gc()

