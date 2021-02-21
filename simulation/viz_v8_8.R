
library(ggplot2)
library(dplyr)

combined_ate_list = c()
combined_bart_ate_list = c()
combined_ate_sd_list = c()
combined_bart_ate_sd_list = c()
combined_ate_weighted_svyregression_list = c()
combined_ate_weighted_svyregression_sd_list = c()
combined_ate_unweighted_svyregression_list = c()
combined_ate_unweighted_svyregression_sd_list = c()
combined_ate_insample_list = c()
combined_ate_sd_insample_list = c()
combined_ate_fullPS_list = c()
combined_ate_fullPS_sd_list = c()

combined_cate_school_achievement_3_min_comp_1_list = c()
combined_bart_cate_school_achievement_3_min_comp_1_list = c()
combined_cate_school_achievement_3_min_comp_1_sd_list = c()
combined_bart_cate_school_achievement_3_min_comp_1_sd_list = c()
combined_ate_unweighted_regression_CATE_school_achievement_3_min_comp_1_list = c()
combined_ate_unweighted_regression_CATE_school_achievement_3_min_comp_1_sd_list = c()
combined_ate_weighted_svyregression_school_achievement_index_3_min_comp_1_list = c()
combined_ate_weighted_svyregression_school_achievement_index_3_min_comp_1_sd_list = c()
combined_ate_weighted_svyregression_school_achievement_index_3_min_comp_1_noprecision_list = c()
combined_ate_weighted_svyregression_school_achievement_index_3_min_comp_1_noprecision_sd_list = c()
combined_cate_school_achievement_3_min_comp_1_insample_list = c()
combined_cate_school_achievement_3_min_comp_1_sd_insample_list = c()
combined_cate_school_achievement_3_min_comp_1_fullPS_list = c()
combined_cate_school_achievement_3_min_comp_1_fullPS_sd_list = c()

combined_cate_school_achievement_1_race_eth_2_list = c()
combined_bart_cate_school_achievement_1_race_eth_2_list = c()
combined_cate_school_achievement_1_race_eth_2_sd_list = c()
combined_bart_cate_school_achievement_1_race_eth_2_sd_list = c()
combined_ate_unweighted_regression_CATE_school_achievement_1_race_eth_2_list = c()
combined_ate_unweighted_regression_CATE_school_achievement_1_race_eth_2_sd_list = c()
combined_ate_weighted_svyregression_school_achievement_index_1_race_eth_2_list = c()
combined_ate_weighted_svyregression_school_achievement_index_1_race_eth_2_sd_list = c()
combined_ate_weighted_svyregression_school_achievement_index_1_race_eth_2_noprecision_list = c()
combined_ate_weighted_svyregression_school_achievement_index_1_race_eth_2_noprecision_sd_list = c()
combined_cate_school_achievement_1_race_eth_2_insample_list = c()
combined_cate_school_achievement_1_race_eth_2_sd_insample_list = c()
combined_cate_school_achievement_1_race_eth_2_fullPS_list = c()
combined_cate_school_achievement_1_race_eth_2_fullPS_sd_list = c()

cate_school_list = rep(0, 11221)
bart_cate_school_list = rep(0, 11221)

cate_school_list_newmethod_unstrat = rep(0, 11221) # unstratified
cate_school_list_newmethod = rep(0, 11221) # uses the sd of the insample school CATE posterior means - stratified
cate_school_list_newmethod_tdist = rep(0, 11221) # uses the sd of the insample school CATE posterior means
cate_school_list_newmethod_CATEmeansd = rep(0, 11221) # uses the mean posterior sd within each school strata

combined_insample_bart_ate_list = c()
combined_insample_bart_ate_sd_list = c()
combined_insample_bart_cate_school_achievement_3_min_comp_1_list = c()
combined_insample_bart_cate_school_achievement_3_min_comp_1_sd_list = c()
combined_insample_bart_cate_school_achievement_1_race_eth_2_list = c()
combined_insample_bart_cate_school_achievement_1_race_eth_2_sd_list = c()

combined_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix = c()
combined_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix = c()
combined_insample_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix = c()
combined_cate_school_achievement_3_min_comp_1_race_eth_templist_insample_matrix = c()
combined_cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_matrix = c()

combined_cate_school_achievement_3_min_comp_1_race_eth_templist_sd_matrix = c()
combined_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_sd_matrix = c()
combined_insample_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_sd_matrix = c()
combined_cate_school_achievement_3_min_comp_1_race_eth_templist_sd_insample_matrix = c()
combined_cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_sd_matrix = c()

combined_ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_templist_matrix = c()
combined_ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_templist_matrix = c()

combined_ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_templist_sd_matrix = c()
combined_ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_templist_sd_matrix = c()


combined_bart_usingposteriordistribution_ate_list = c()
combined_bart_usingposteriordistribution_ate_sd_list = c()
combined_bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_list = c()
combined_bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_sd_list = c()
combined_bart_usingposteriordistribution_cate_school_achievement_1_race_eth_2_list =  c()
combined_bart_usingposteriordistribution_cate_school_achievement_1_race_eth_2_sd_list = c()
combined_bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix = c()
combined_bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist_sd_matrix = c()

combined_ate_inCI = c()
combined_ate_insample_inCI = c()
combined_bart_usingposteriordistribution_ate_inCI = c()
combined_bart_ate_inCI = c()
combined_insample_bart_ate_inCI = c()
combined_ate_weighted_svyregression_inCI = c()
combined_ate_unweighted_svyregression_inCI = c()

combined_cate_school_achievement_3_min_comp_1_inCI = c()
combined_cate_school_achievement_3_min_comp_1_insample_inCI = c()
combined_bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_inCI = c()
combined_bart_cate_school_achievement_3_min_comp_1_inCI = c()
combined_insample_bart_cate_school_achievement_3_min_comp_1_inCI = c()
combined_cate_school_achievement_3_min_comp_1_weighted_svyregression_inCI = c()
combined_cate_school_achievement_3_min_comp_1_unweighted_svyregression_inCI = c()

combined_cate_school_achievement_1_race_eth_2_inCI = c()
combined_cate_school_achievement_1_race_eth_2_insample_inCI = c()
combined_bart_usingposteriordistribution_cate_school_achievement_1_race_eth_2_inCI = c()
combined_bart_cate_school_achievement_1_race_eth_2_inCI = c()
combined_insample_bart_cate_school_achievement_1_race_eth_2_inCI = c()
combined_cate_school_achievement_1_race_eth_2_weighted_svyregression_inCI = c()
combined_cate_school_achievement_1_race_eth_2_unweighted_svyregression_inCI = c()


combined_cate_school_achievement_3_min_comp_1_race_eth_templist_1_inCI = c()
combined_cate_school_achievement_3_min_comp_1_race_eth_templist_insample_1_inCI = c()
combined_bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist_1_inCI = c()
combined_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_1_inCI = c()
combined_insample_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_1_inCI = c()
combined_ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_model_templist_1_inCI = c()
combined_ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_model_templist_1_inCI = c()

combined_cate_school_achievement_3_min_comp_1_race_eth_templist_2_inCI = c()
combined_cate_school_achievement_3_min_comp_1_race_eth_templist_insample_2_inCI = c()
combined_bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist_2_inCI = c()
combined_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_2_inCI = c()
combined_insample_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_2_inCI = c()
combined_ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_model_templist_2_inCI = c()
combined_ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_model_templist_2_inCI = c()

combined_cate_school_achievement_3_min_comp_1_race_eth_templist_3_inCI = c()
combined_cate_school_achievement_3_min_comp_1_race_eth_templist_insample_3_inCI = c()
combined_bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist_3_inCI = c()
combined_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_3_inCI = c()
combined_insample_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_3_inCI = c()
combined_ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_model_templist_3_inCI = c()
combined_ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_model_templist_3_inCI = c()

combined_cate_school_achievement_3_min_comp_1_race_eth_templist_4_inCI = c()
combined_cate_school_achievement_3_min_comp_1_race_eth_templist_insample_4_inCI = c()
combined_bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist_4_inCI = c()
combined_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_4_inCI = c()
combined_insample_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_4_inCI = c()
combined_ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_model_templist_4_inCI = c()
combined_ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_model_templist_4_inCI = c()

combined_cate_school_achievement_3_min_comp_1_race_eth_templist_5_inCI = c()
combined_cate_school_achievement_3_min_comp_1_race_eth_templist_insample_5_inCI = c()
combined_bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist_5_inCI = c()
combined_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_5_inCI = c()
combined_insample_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_5_inCI = c()
combined_ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_model_templist_5_inCI = c()
combined_ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_model_templist_5_inCI = c()

combined_cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_1_inCI = c()
combined_cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_2_inCI = c()
combined_cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_3_inCI = c()
combined_cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_4_inCI = c()
combined_cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_5_inCI = c()
combined_ate_fullPS_inCI = c()
combined_cate_school_achievement_3_min_comp_1_fullPS_inCI = c()
combined_cate_school_achievement_1_race_eth_2_fullPS_inCI = c()


is_simple_error = function(x) inherits(x, "simpleError")

#####################
filenames = c("20_20_1000_v8_8.RData")


counter = 1

for (m in filenames) {
  
  print(m)
  load(m)

  ate_list = c()
  bart_ate_list = c()
  ate_sd_list = c()
  bart_ate_sd_list = c()
  ate_weighted_svyregression_list = c()
  ate_weighted_svyregression_sd_list = c()
  ate_unweighted_svyregression_list = c()
  ate_unweighted_svyregression_sd_list = c()
  ate_insample_list = c()
  ate_insample_sd_list = c()
  
  cate_school_achievement_3_min_comp_1_list = c()
  bart_cate_school_achievement_3_min_comp_1_list = c()
  cate_school_achievement_3_min_comp_1_sd_list = c()
  bart_cate_school_achievement_3_min_comp_1_sd_list = c()
  ate_unweighted_regression_CATE_school_achievement_3_min_comp_1_list = c()
  ate_unweighted_regression_CATE_school_achievement_3_min_comp_1_sd_list = c()
  ate_weighted_svyregression_school_achievement_index_3_min_comp_1_list = c()
  ate_weighted_svyregression_school_achievement_index_3_min_comp_1_sd_list = c()
  ate_weighted_svyregression_school_achievement_index_3_min_comp_1_noprecision_list = c()
  ate_weighted_svyregression_school_achievement_index_3_min_comp_1_noprecision_sd_list = c()
  cate_school_achievement_3_min_comp_1_insample_list = c()
  cate_school_achievement_3_min_comp_1_sd_insample_list = c()
  
  cate_school_achievement_1_race_eth_2_list = c()
  bart_cate_school_achievement_1_race_eth_2_list = c()
  cate_school_achievement_1_race_eth_2_sd_list = c()
  bart_cate_school_achievement_1_race_eth_2_sd_list = c()
  ate_unweighted_regression_CATE_school_achievement_1_race_eth_2_list = c()
  ate_unweighted_regression_CATE_school_achievement_1_race_eth_2_sd_list = c()
  ate_weighted_svyregression_school_achievement_index_1_race_eth_2_list = c()
  ate_weighted_svyregression_school_achievement_index_1_race_eth_2_sd_list = c()
  ate_weighted_svyregression_school_achievement_index_1_race_eth_2_noprecision_list = c()
  ate_weighted_svyregression_school_achievement_index_1_race_eth_2_noprecision_sd_list = c()
  cate_school_achievement_1_race_eth_2_insample_list = c()
  cate_school_achievement_1_race_eth_2_sd_insample_list = c()
  

  insample_bart_ate_list = c()
  insample_bart_ate_sd_list = c()
  insample_bart_cate_school_achievement_3_min_comp_1_list = c()
  insample_bart_cate_school_achievement_3_min_comp_1_sd_list = c()
  insample_bart_cate_school_achievement_1_race_eth_2_list = c()
  insample_bart_cate_school_achievement_1_race_eth_2_sd_list = c()
  
  
  cate_school_achievement_3_min_comp_1_race_eth_templist_matrix = c()
  bart_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix = c()
  insample_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix = c()
  
  cate_school_achievement_3_min_comp_1_race_eth_templist_sd_matrix = c()
  bart_cate_school_achievement_3_min_comp_1_race_eth_templist_sd_matrix = c()
  insample_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_sd_matrix = c()
  
  ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_templist_matrix = c()
  ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_templist_matrix = c()
  
  ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_templist_sd_matrix = c()
  ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_templist_sd_matrix = c()
  
  
  bart_usingposteriordistribution_ate_list = c()
  bart_usingposteriordistribution_ate_sd_list = c()
  bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_list = c()
  bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_sd_list = c()
  bart_usingposteriordistribution_cate_school_achievement_1_race_eth_2_list =  c()
  bart_usingposteriordistribution_cate_school_achievement_1_race_eth_2_sd_list = c()
  bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix = c()
  bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist_sd_matrix = c()

  ate_inCI = c()
  ate_insample_inCI = c()
  bart_usingposteriordistribution_ate_inCI = c()
  bart_ate_inCI = c()
  insample_bart_ate_inCI = c()
  ate_weighted_svyregression_inCI = c()
  ate_unweighted_svyregression_inCI = c()
  
  cate_school_achievement_3_min_comp_1_inCI = c()
  cate_school_achievement_3_min_comp_1_insample_inCI = c()
  bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_inCI = c()
  bart_cate_school_achievement_3_min_comp_1_inCI = c()
  insample_bart_cate_school_achievement_3_min_comp_1_inCI = c()
  cate_school_achievement_3_min_comp_1_weighted_svyregression_inCI = c()
  cate_school_achievement_3_min_comp_1_unweighted_svyregression_inCI = c()
  
  cate_school_achievement_1_race_eth_2_inCI = c()
  cate_school_achievement_1_race_eth_2_insample_inCI = c()
  bart_usingposteriordistribution_cate_school_achievement_1_race_eth_2_inCI = c()
  bart_cate_school_achievement_1_race_eth_2_inCI = c()
  insample_bart_cate_school_achievement_1_race_eth_2_inCI = c()
  cate_school_achievement_1_race_eth_2_weighted_svyregression_inCI = c()
  cate_school_achievement_1_race_eth_2_unweighted_svyregression_inCI = c()
  
  
  cate_school_achievement_3_min_comp_1_race_eth_templist_1_inCI = c()
  cate_school_achievement_3_min_comp_1_race_eth_templist_insample_1_inCI = c()
  bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist_1_inCI = c()
  bart_cate_school_achievement_3_min_comp_1_race_eth_templist_1_inCI = c()
  insample_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_1_inCI = c()
  ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_model_templist_1_inCI = c()
  ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_model_templist_1_inCI = c()
  
  cate_school_achievement_3_min_comp_1_race_eth_templist_2_inCI = c()
  cate_school_achievement_3_min_comp_1_race_eth_templist_insample_2_inCI = c()
  bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist_2_inCI = c()
  bart_cate_school_achievement_3_min_comp_1_race_eth_templist_2_inCI = c()
  insample_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_2_inCI = c()
  ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_model_templist_2_inCI = c()
  ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_model_templist_2_inCI = c()
  
  cate_school_achievement_3_min_comp_1_race_eth_templist_3_inCI = c()
  cate_school_achievement_3_min_comp_1_race_eth_templist_insample_3_inCI = c()
  bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist_3_inCI = c()
  bart_cate_school_achievement_3_min_comp_1_race_eth_templist_3_inCI = c()
  insample_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_3_inCI = c()
  ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_model_templist_3_inCI = c()
  ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_model_templist_3_inCI = c()
  
  cate_school_achievement_3_min_comp_1_race_eth_templist_4_inCI = c()
  cate_school_achievement_3_min_comp_1_race_eth_templist_insample_4_inCI = c()
  bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist_4_inCI = c()
  bart_cate_school_achievement_3_min_comp_1_race_eth_templist_4_inCI = c()
  insample_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_4_inCI = c()
  ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_model_templist_4_inCI = c()
  ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_model_templist_4_inCI = c()
  
  cate_school_achievement_3_min_comp_1_race_eth_templist_5_inCI = c()
  cate_school_achievement_3_min_comp_1_race_eth_templist_insample_5_inCI = c()
  bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist_5_inCI = c()
  bart_cate_school_achievement_3_min_comp_1_race_eth_templist_5_inCI = c()
  insample_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_5_inCI = c()
  ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_model_templist_5_inCI = c()
  ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_model_templist_5_inCI = c()
  
  ate_fullPS_list = c()
  ate_fullPS_sd_list = c()
  cate_school_achievement_3_min_comp_1_fullPS_list = c()
  cate_school_achievement_3_min_comp_1_fullPS_sd_list = c()
  cate_school_achievement_1_race_eth_2_fullPS_list = c()
  cate_school_achievement_1_race_eth_2_fullPS_sd_list = c()
  cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_matrix = c()
  cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_sd_matrix = c()
  
  cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_1_inCI = c()
  cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_2_inCI = c()
  cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_3_inCI = c()
  cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_4_inCI = c()
  cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_5_inCI = c()
  
  ate_fullPS_inCI = c()
  cate_school_achievement_3_min_comp_1_fullPS_inCI = c()
  cate_school_achievement_1_race_eth_2_fullPS_inCI = c()
  
  for (r in (intersect(which(!unlist(lapply(FUN=is_simple_error, modelfits))==TRUE), 
                       which(!unlist(lapply(FUN=is.null, modelfits))))
  )
  ) {
    print(paste(r, modelfits[[r]][[1]][[3]]))
    print(paste("In sample MRP", r, modelfits[[r]][[37]][[3]]))
    
    
    ate_list = c(ate_list, modelfits[[r]][[1]][4])
    bart_ate_list = c(bart_ate_list, modelfits[[r]][[2]][4])
    ate_sd_list = c(ate_sd_list, modelfits[[r]][[3]])
    bart_ate_sd_list = c(bart_ate_sd_list, modelfits[[r]][[4]])
    ate_weighted_svyregression_list = c(ate_weighted_svyregression_list, modelfits[[r]][[5]][2,1])
    ate_weighted_svyregression_sd_list = c(ate_weighted_svyregression_sd_list, modelfits[[r]][[5]][2,2])
    ate_unweighted_svyregression_list = c(ate_unweighted_svyregression_list, modelfits[[r]][[6]][2,1])
    ate_unweighted_svyregression_sd_list = c(ate_unweighted_svyregression_sd_list, modelfits[[r]][[6]][2,2])
    ate_insample_list = c(ate_insample_list, modelfits[[r]][[37]][4])
    ate_insample_sd_list = c(ate_insample_sd_list, modelfits[[r]][[38]])
    
    cate_school_achievement_3_min_comp_1_list = c(cate_school_achievement_3_min_comp_1_list, modelfits[[r]][[7]][4])
    bart_cate_school_achievement_3_min_comp_1_list = c(bart_cate_school_achievement_3_min_comp_1_list, modelfits[[r]][[8]][4])
    cate_school_achievement_3_min_comp_1_sd_list = c(cate_school_achievement_3_min_comp_1_sd_list, modelfits[[r]][[9]])
    bart_cate_school_achievement_3_min_comp_1_sd_list = c(bart_cate_school_achievement_3_min_comp_1_sd_list, modelfits[[r]][[10]])
    ate_unweighted_regression_CATE_school_achievement_3_min_comp_1_list = c(ate_unweighted_regression_CATE_school_achievement_3_min_comp_1_list, modelfits[[r]][[11]][2,1])
    ate_unweighted_regression_CATE_school_achievement_3_min_comp_1_sd_list = c(ate_unweighted_regression_CATE_school_achievement_3_min_comp_1_sd_list, modelfits[[r]][[11]][2,2])
    ate_weighted_svyregression_school_achievement_index_3_min_comp_1_list = c(ate_weighted_svyregression_school_achievement_index_3_min_comp_1_list, modelfits[[r]][[12]][2,1])
    ate_weighted_svyregression_school_achievement_index_3_min_comp_1_sd_list = c(ate_weighted_svyregression_school_achievement_index_3_min_comp_1_sd_list, modelfits[[r]][[12]][2,2])
    ate_weighted_svyregression_school_achievement_index_3_min_comp_1_noprecision_list = c(ate_weighted_svyregression_school_achievement_index_3_min_comp_1_noprecision_list, modelfits[[r]][[13]][2,1])
    ate_weighted_svyregression_school_achievement_index_3_min_comp_1_noprecision_sd_list = c(ate_weighted_svyregression_school_achievement_index_3_min_comp_1_noprecision_sd_list, modelfits[[r]][[13]][2,2])
    cate_school_achievement_3_min_comp_1_insample_list = c(cate_school_achievement_3_min_comp_1_insample_list, modelfits[[r]][[39]][4])
    cate_school_achievement_3_min_comp_1_sd_insample_list = c(cate_school_achievement_3_min_comp_1_sd_insample_list, modelfits[[r]][[40]])
    
    
    cate_school_achievement_1_race_eth_2_list = c(cate_school_achievement_1_race_eth_2_list, modelfits[[r]][[14]][4])
    bart_cate_school_achievement_1_race_eth_2_list = c(bart_cate_school_achievement_1_race_eth_2_list, modelfits[[r]][[15]][4])
    cate_school_achievement_1_race_eth_2_sd_list = c(cate_school_achievement_1_race_eth_2_sd_list, modelfits[[r]][[16]])
    bart_cate_school_achievement_1_race_eth_2_sd_list = c(bart_cate_school_achievement_1_race_eth_2_sd_list, modelfits[[r]][[17]])
    ate_unweighted_regression_CATE_school_achievement_1_race_eth_2_list = c(ate_unweighted_regression_CATE_school_achievement_1_race_eth_2_list, modelfits[[r]][[18]][2,1])
    ate_unweighted_regression_CATE_school_achievement_1_race_eth_2_sd_list = c(ate_unweighted_regression_CATE_school_achievement_1_race_eth_2_sd_list, modelfits[[r]][[18]][2,2])
    ate_weighted_svyregression_school_achievement_index_1_race_eth_2_list = c(ate_weighted_svyregression_school_achievement_index_1_race_eth_2_list, modelfits[[r]][[19]][2,1])
    ate_weighted_svyregression_school_achievement_index_1_race_eth_2_sd_list = c(ate_weighted_svyregression_school_achievement_index_1_race_eth_2_sd_list, modelfits[[r]][[19]][2,2])
    ate_weighted_svyregression_school_achievement_index_1_race_eth_2_noprecision_list = c(ate_weighted_svyregression_school_achievement_index_1_race_eth_2_noprecision_list, modelfits[[r]][[20]][2,1])
    ate_weighted_svyregression_school_achievement_index_1_race_eth_2_noprecision_sd_list = c(ate_weighted_svyregression_school_achievement_index_1_race_eth_2_noprecision_sd_list, modelfits[[r]][[20]][2,2])
    cate_school_achievement_1_race_eth_2_insample_list = c(cate_school_achievement_1_race_eth_2_insample_list, modelfits[[r]][[41]][4])
    cate_school_achievement_1_race_eth_2_sd_insample_list = c(cate_school_achievement_1_race_eth_2_sd_insample_list, modelfits[[r]][[42]])
    
    cate_school_list = cate_school_list + modelfits[[r]][[21]]$CATE
    #cate_school_list = cate_school_list + modelfits[[r]][[21]]
    bart_cate_school_list = bart_cate_school_list + modelfits[[r]][[22]]$CATE
    
    #modelfits[[r]][[21]]$CATE_school_newmethod_stratified = rep(0, dim(modelfits[[r]][[21]])[1])
    
    CATE_school_truth_joined_new_strata = right_join(unique(PS_mat[,c("school", "minority_composition_index", "school_achievement_index")]), 
                                                     modelfits[[r]][[21]],
                                                     by="school")
    CATE_school_truth_joined_new_strata$cate_school_newmethod_stratified = rep(0, dim(modelfits[[r]][[21]])[1])
    
    
    
    CATE_school_truth_joined_new_strata_summary = CATE_school_truth_joined_new_strata %>% 
      filter(in_sample==TRUE) %>%
      group_by(minority_composition_index, school_achievement_index) %>% 
      summarise(school_strata_mean = mean(CATE),
                school_strata_sd = sd(CATE),
                t_distribution_sd = sd(CATE)*sqrt(1 + 1/n()), # the sd function function uses n-1 as denom
                mean_of_posterior_sd = mean(CATE_sd),
                n=n())
    
    for (j in 1:dim(CATE_school_truth_joined_new_strata)[1]) {
      if (CATE_school_truth_joined_new_strata$in_sample[j]==FALSE) { # no in sample school

        CATE_school_truth_joined_new_strata$cate_school_newmethod[j] = mean(CATE_school_truth_joined_new_strata$CATE) + rt(n=1,
                                                                                                                           df=dim(CATE_school_truth_joined_new_strata %>% filter(in_sample==TRUE))[1]-1) * (sd(CATE_school_truth_joined_new_strata$CATE)*sqrt(1+1/dim(CATE_school_truth_joined_new_strata %>% filter(in_sample==TRUE))[1]))
        # +
          #0.1 # 0.1 is sd(colMeans(sweep(multilevel_diff, 2, PS_mat$Freq, "/")))
        
        CATE_school_truth_joined_new_strata$cate_school_newmethod_stratified[j] = rnorm(n=1, 
                                                                                        mean = (CATE_school_truth_joined_new_strata_summary %>% 
                                                                                                  filter(minority_composition_index==CATE_school_truth_joined_new_strata$minority_composition_index[j],
                                                                                                         school_achievement_index==CATE_school_truth_joined_new_strata$school_achievement_index[j]))$school_strata_mean,
                                                                                        sd = sqrt( (sd((CATE_school_truth_joined_new_strata %>% filter(in_sample==TRUE))$CATE))^2 + (
                                                                                          (CATE_school_truth_joined_new_strata_summary %>% 
                                                                                             filter(minority_composition_index==CATE_school_truth_joined_new_strata$minority_composition_index[j],
                                                                                                    school_achievement_index==CATE_school_truth_joined_new_strata$school_achievement_index[j]))$school_strata_sd
                                                                                        )^2 )
        )
        
        CATE_school_truth_joined_new_strata$cate_school_newmethod_stratified_tdist[j] = (CATE_school_truth_joined_new_strata_summary %>% 
                                                                                           filter(minority_composition_index==CATE_school_truth_joined_new_strata$minority_composition_index[j],
                                                                                                  school_achievement_index==CATE_school_truth_joined_new_strata$school_achievement_index[j]))$school_strata_mean + rt(n=1, 
                                                                                                                                                                                                                      df = (CATE_school_truth_joined_new_strata_summary %>% 
                                                                                                                                                                                                                              filter(minority_composition_index==CATE_school_truth_joined_new_strata$minority_composition_index[j],
                                                                                                                                                                                                                                     school_achievement_index==CATE_school_truth_joined_new_strata$school_achievement_index[j]))$n - 1
                                                                                                  ) * (CATE_school_truth_joined_new_strata_summary %>% 
                                                                                                         filter(minority_composition_index==CATE_school_truth_joined_new_strata$minority_composition_index[j],
                                                                                                                school_achievement_index==CATE_school_truth_joined_new_strata$school_achievement_index[j]))$t_distribution_sd
        
        CATE_school_truth_joined_new_strata$cate_school_newmethod_stratified_meanposteriorsd[j] = rnorm(n=1, 
                                                                                                        mean = (CATE_school_truth_joined_new_strata_summary %>% 
                                                                                                                  filter(minority_composition_index==CATE_school_truth_joined_new_strata$minority_composition_index[j],
                                                                                                                         school_achievement_index==CATE_school_truth_joined_new_strata$school_achievement_index[j]))$school_strata_mean,
                                                                                                        sd = (CATE_school_truth_joined_new_strata_summary %>% 
                                                                                                                filter(minority_composition_index==CATE_school_truth_joined_new_strata$minority_composition_index[j],
                                                                                                                       school_achievement_index==CATE_school_truth_joined_new_strata$school_achievement_index[j]))$mean_of_posterior_sd
        )
        
      } else { # in sample
        CATE_school_truth_joined_new_strata$cate_school_newmethod[j] = modelfits[[r]][[21]]$CATE[j]
        CATE_school_truth_joined_new_strata$cate_school_newmethod_stratified[j] = modelfits[[r]][[21]]$CATE[j]
        CATE_school_truth_joined_new_strata$cate_school_newmethod_stratified_tdist[j] = modelfits[[r]][[21]]$CATE[j]
        CATE_school_truth_joined_new_strata$cate_school_newmethod_stratified_meanposteriorsd[j] = modelfits[[r]][[21]]$CATE[j]
      }
      
    }
    
    cate_school_list_newmethod_unstrat = cate_school_list_newmethod_unstrat + CATE_school_truth_joined_new_strata$cate_school_newmethod
    cate_school_list_newmethod = cate_school_list_newmethod + CATE_school_truth_joined_new_strata$cate_school_newmethod_stratified
    cate_school_list_newmethod_tdist = cate_school_list_newmethod_tdist + CATE_school_truth_joined_new_strata$cate_school_newmethod_stratified_tdist
    cate_school_list_newmethod_CATEmeansd = cate_school_list_newmethod_CATEmeansd + CATE_school_truth_joined_new_strata$cate_school_newmethod_stratified_meanposteriorsd
    
    insample_bart_ate_list = c(insample_bart_ate_list, modelfits[[r]][[23]][4])
    insample_bart_ate_sd_list = c(insample_bart_ate_sd_list, modelfits[[r]][[24]])
    insample_bart_cate_school_achievement_3_min_comp_1_list = c(insample_bart_cate_school_achievement_3_min_comp_1_list, modelfits[[r]][[25]][4])
    insample_bart_cate_school_achievement_3_min_comp_1_sd_list = c(insample_bart_cate_school_achievement_3_min_comp_1_sd_list, modelfits[[r]][[26]])
    insample_bart_cate_school_achievement_1_race_eth_2_list = c(insample_bart_cate_school_achievement_1_race_eth_2_list, modelfits[[r]][[27]][4])
    insample_bart_cate_school_achievement_1_race_eth_2_sd_list = c(insample_bart_cate_school_achievement_1_race_eth_2_sd_list, modelfits[[r]][[28]])
    
    cate_school_achievement_3_min_comp_1_race_eth_templist_matrix = cbind(cate_school_achievement_3_min_comp_1_race_eth_templist_matrix, 
                                                                          unlist(lapply(X=modelfits[[r]][[29]],FUN="[[",4))
                                                                          )
    bart_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix = cbind(bart_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix, 
                                                                               unlist(lapply(X=modelfits[[r]][[30]],FUN="[[",4))
                                                                               )
    insample_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix = cbind(insample_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix, 
                                                                                        unlist(lapply(X=modelfits[[r]][[31]],FUN="[[",4))
                                                                                        )
    
    cate_school_achievement_3_min_comp_1_race_eth_templist_sd_matrix = cbind(cate_school_achievement_3_min_comp_1_race_eth_templist_sd_matrix, 
                                                                             unlist(modelfits[[r]][[32]])
                                                                             )
    bart_cate_school_achievement_3_min_comp_1_race_eth_templist_sd_matrix = cbind(bart_cate_school_achievement_3_min_comp_1_race_eth_templist_sd_matrix,
                                                                                  unlist(modelfits[[r]][[33]])
                                                                                  )
    insample_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_sd_matrix = cbind(insample_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_sd_matrix, 
                                                                                           unlist(modelfits[[r]][[34]])         
                                                                                           )
    
    ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_templist_matrix = cbind(ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_templist_matrix,
                                                                                                unlist(lapply(X=modelfits[[r]][[35]], FUN="[", 2, 1))
                                                                                                )
    ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_templist_matrix = cbind(ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_templist_matrix,
                                                                                                  unlist(lapply(X=modelfits[[r]][[36]], FUN="[", 2, 1))
                                                                                                  )
    
    ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_templist_sd_matrix = cbind(ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_templist_sd_matrix,
                                                                                                   unlist(lapply(X=modelfits[[r]][[35]], FUN="[", 2, 2))
                                                                                                   )
    ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_templist_sd_matrix = cbind(ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_templist_sd_matrix,
                                                                                                     unlist(lapply(X=modelfits[[r]][[36]], FUN="[", 2, 2))
                                                                                                     )
    
    combined_cate_school_achievement_3_min_comp_1_race_eth_templist_insample_matrix = cbind(combined_cate_school_achievement_3_min_comp_1_race_eth_templist_insample_matrix, 
                                                                                            unlist(lapply(X=modelfits[[r]][[43]],FUN="[[",4))
                                                                                            )
    combined_cate_school_achievement_3_min_comp_1_race_eth_templist_sd_insample_matrix = cbind(combined_cate_school_achievement_3_min_comp_1_race_eth_templist_sd_insample_matrix,
                                                                                               unlist(modelfits[[r]][[44]]))
    
    
    bart_usingposteriordistribution_ate_list = c(bart_usingposteriordistribution_ate_list, modelfits[[r]][[45]][4])
    bart_usingposteriordistribution_ate_sd_list = c(bart_usingposteriordistribution_ate_sd_list, modelfits[[r]][[46]])
    bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_list = c(bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_list, modelfits[[r]][[47]][4])
    bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_sd_list = c(bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_sd_list, modelfits[[r]][[48]])
    bart_usingposteriordistribution_cate_school_achievement_1_race_eth_2_list =  c(bart_usingposteriordistribution_cate_school_achievement_1_race_eth_2_list, modelfits[[r]][[49]][4])
    bart_usingposteriordistribution_cate_school_achievement_1_race_eth_2_sd_list = c(bart_usingposteriordistribution_cate_school_achievement_1_race_eth_2_sd_list, modelfits[[r]][[50]])
    bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix = cbind(bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix,
                                                                                                          unlist(lapply(X=modelfits[[r]][[51]],FUN="[[",4))
                                                                                                          )
    bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist_sd_matrix = cbind(bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist_sd_matrix,
                                                                                                             unlist(modelfits[[r]][[52]])
                                                                                                             )
    

    
    ate_inCI = c(ate_inCI, 0.126 >= modelfits[[r]][[54]][1] & 0.126 <= modelfits[[r]][[54]][2] )
    ate_insample_inCI = c(ate_insample_inCI, 0.126 >= modelfits[[r]][[55]][1] & 0.126 <= modelfits[[r]][[55]][2] )
    bart_usingposteriordistribution_ate_inCI = c(bart_usingposteriordistribution_ate_inCI, 0.126 >= modelfits[[r]][[56]][1] & 0.126 <= modelfits[[r]][[56]][2] )
    bart_ate_inCI = c(bart_ate_inCI, 0.126 >= modelfits[[r]][[57]][1] & 0.126 <= modelfits[[r]][[57]][2] )
    insample_bart_ate_inCI = c(insample_bart_ate_inCI, 0.126 >= modelfits[[r]][[58]][1] & 0.126 <= modelfits[[r]][[58]][2] )
    ate_weighted_svyregression_inCI = c(ate_weighted_svyregression_inCI, 0.126 >= modelfits[[r]][[59]][1] & 0.126 <= modelfits[[r]][[59]][2] )
    ate_unweighted_svyregression_inCI = c(ate_unweighted_svyregression_inCI, 0.126 >= modelfits[[r]][[60]][1] & 0.126 <= modelfits[[r]][[60]][2] )
    
    cate_school_achievement_3_min_comp_1_inCI = c(cate_school_achievement_3_min_comp_1_inCI, .065 >= modelfits[[r]][[61]][1] & .065 <= modelfits[[r]][[61]][2])
    cate_school_achievement_3_min_comp_1_insample_inCI = c(cate_school_achievement_3_min_comp_1_insample_inCI, .065 >= modelfits[[r]][[62]][1] & .065 <= modelfits[[r]][[62]][2])
    bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_inCI = c(bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_inCI, .065 >= modelfits[[r]][[63]][1] & .065 <= modelfits[[r]][[63]][2])
    bart_cate_school_achievement_3_min_comp_1_inCI = c(bart_cate_school_achievement_3_min_comp_1_inCI, .065 >= modelfits[[r]][[64]][1] & .065 <= modelfits[[r]][[64]][2])
    insample_bart_cate_school_achievement_3_min_comp_1_inCI = c(insample_bart_cate_school_achievement_3_min_comp_1_inCI, .065 >= modelfits[[r]][[65]][1] & .065 <= modelfits[[r]][[65]][2])
    cate_school_achievement_3_min_comp_1_weighted_svyregression_inCI = c(cate_school_achievement_3_min_comp_1_weighted_svyregression_inCI, .065 >= modelfits[[r]][[66]][1] & .065 <= modelfits[[r]][[66]][2])
    cate_school_achievement_3_min_comp_1_unweighted_svyregression_inCI = c(cate_school_achievement_3_min_comp_1_unweighted_svyregression_inCI, .065 >= modelfits[[r]][[67]][1] & .065 <= modelfits[[r]][[67]][2])
    
    cate_school_achievement_1_race_eth_2_inCI = c(cate_school_achievement_1_race_eth_2_inCI, .195 >= modelfits[[r]][[68]][1] & .195 <= modelfits[[r]][[68]][2])
    cate_school_achievement_1_race_eth_2_insample_inCI = c(cate_school_achievement_1_race_eth_2_insample_inCI, .195 >= modelfits[[r]][[69]][1] & .195 <= modelfits[[r]][[69]][2])
    bart_usingposteriordistribution_cate_school_achievement_1_race_eth_2_inCI = c(bart_usingposteriordistribution_cate_school_achievement_1_race_eth_2_inCI, .195 >= modelfits[[r]][[70]][1] & .195 <= modelfits[[r]][[70]][2])
    bart_cate_school_achievement_1_race_eth_2_inCI = c(bart_cate_school_achievement_1_race_eth_2_inCI, .195 >= modelfits[[r]][[71]][1] & .195 <= modelfits[[r]][[71]][2] )
    insample_bart_cate_school_achievement_1_race_eth_2_inCI = c(insample_bart_cate_school_achievement_1_race_eth_2_inCI, .195 >= modelfits[[r]][[72]][1] & .195 <= modelfits[[r]][[72]][2])
    cate_school_achievement_1_race_eth_2_weighted_svyregression_inCI = c(cate_school_achievement_1_race_eth_2_weighted_svyregression_inCI, .195 >= modelfits[[r]][[73]][1] & .195 <= modelfits[[r]][[73]][2])
    cate_school_achievement_1_race_eth_2_unweighted_svyregression_inCI = c(cate_school_achievement_1_race_eth_2_unweighted_svyregression_inCI, .195 >= modelfits[[r]][[74]][1] & .195 <= modelfits[[r]][[74]][2])
    
    cate_school_achievement_3_min_comp_1_race_eth_templist_1_inCI = c(cate_school_achievement_3_min_comp_1_race_eth_templist_1_inCI, 0.024 >= modelfits[[r]][[75]][1] & 0.024 <= modelfits[[r]][[75]][2] )
    cate_school_achievement_3_min_comp_1_race_eth_templist_insample_1_inCI = c(cate_school_achievement_3_min_comp_1_race_eth_templist_insample_1_inCI, 0.024 >= modelfits[[r]][[76]][1] & 0.024 <= modelfits[[r]][[76]][2] )
    bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist_1_inCI = c(bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist_1_inCI, 0.024 >= modelfits[[r]][[77]][1] & 0.024 <= modelfits[[r]][[77]][2] )
    bart_cate_school_achievement_3_min_comp_1_race_eth_templist_1_inCI = c(bart_cate_school_achievement_3_min_comp_1_race_eth_templist_1_inCI, 0.024 >= modelfits[[r]][[78]][1] & 0.024 <= modelfits[[r]][[78]][2] )
    insample_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_1_inCI = c(insample_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_1_inCI, 0.024 >= modelfits[[r]][[79]][1] & 0.024 <= modelfits[[r]][[79]][2] )
    ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_model_templist_1_inCI = c(ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_model_templist_1_inCI, 0.024 >= modelfits[[r]][[80]][1] & 0.024 <= modelfits[[r]][[80]][2] )
    ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_model_templist_1_inCI = c(ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_model_templist_1_inCI, 0.024 >= modelfits[[r]][[81]][1] & 0.024 <= modelfits[[r]][[81]][2] )
    
    cate_school_achievement_3_min_comp_1_race_eth_templist_2_inCI = c(cate_school_achievement_3_min_comp_1_race_eth_templist_2_inCI, 0.081 >= modelfits[[r]][[82]][1] & 0.081 <= modelfits[[r]][[82]][2] )
    cate_school_achievement_3_min_comp_1_race_eth_templist_insample_2_inCI = c(cate_school_achievement_3_min_comp_1_race_eth_templist_insample_2_inCI, 0.081 >= modelfits[[r]][[83]][1] & 0.081 <= modelfits[[r]][[83]][2] )
    bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist_2_inCI = c(bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist_2_inCI, 0.081 >= modelfits[[r]][[84]][1] & 0.081 <= modelfits[[r]][[84]][2] )
    bart_cate_school_achievement_3_min_comp_1_race_eth_templist_2_inCI = c(bart_cate_school_achievement_3_min_comp_1_race_eth_templist_2_inCI, 0.081 >= modelfits[[r]][[85]][1] & 0.081 <= modelfits[[r]][[85]][2] )
    insample_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_2_inCI = c(insample_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_2_inCI, 0.081 >= modelfits[[r]][[86]][1] & 0.081 <= modelfits[[r]][[86]][2] )
    ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_model_templist_2_inCI = c(ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_model_templist_2_inCI, 0.081 >= modelfits[[r]][[87]][1] & 0.081 <= modelfits[[r]][[87]][2] )
    ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_model_templist_2_inCI = c(ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_model_templist_2_inCI, 0.081 >= modelfits[[r]][[88]][1] & 0.081 <= modelfits[[r]][[88]][2] )
    
    cate_school_achievement_3_min_comp_1_race_eth_templist_3_inCI = c(cate_school_achievement_3_min_comp_1_race_eth_templist_3_inCI, 0.06 >= modelfits[[r]][[89]][1] & 0.06 <= modelfits[[r]][[89]][2] )
    cate_school_achievement_3_min_comp_1_race_eth_templist_insample_3_inCI = c(cate_school_achievement_3_min_comp_1_race_eth_templist_insample_3_inCI, 0.06 >= modelfits[[r]][[90]][1] & 0.06 <= modelfits[[r]][[90]][2] )
    bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist_3_inCI = c(bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist_3_inCI, 0.06 >= modelfits[[r]][[91]][1] & 0.06 <= modelfits[[r]][[91]][2] )
    bart_cate_school_achievement_3_min_comp_1_race_eth_templist_3_inCI = c(bart_cate_school_achievement_3_min_comp_1_race_eth_templist_3_inCI, 0.06 >= modelfits[[r]][[92]][1] & 0.06 <= modelfits[[r]][[92]][2] )
    insample_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_3_inCI = c(insample_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_3_inCI, 0.06 >= modelfits[[r]][[93]][1] & 0.06 <= modelfits[[r]][[93]][2] )
    ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_model_templist_3_inCI = c(ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_model_templist_3_inCI, 0.06 >= modelfits[[r]][[94]][1] & 0.06 <= modelfits[[r]][[94]][2] )
    ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_model_templist_3_inCI = c(ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_model_templist_3_inCI, 0.06 >= modelfits[[r]][[95]][1] & 0.06 <= modelfits[[r]][[95]][2] )
    
    cate_school_achievement_3_min_comp_1_race_eth_templist_4_inCI = c(cate_school_achievement_3_min_comp_1_race_eth_templist_4_inCI, 0.081 >= modelfits[[r]][[96]][1] & 0.081 <= modelfits[[r]][[96]][2] )
    cate_school_achievement_3_min_comp_1_race_eth_templist_insample_4_inCI = c(cate_school_achievement_3_min_comp_1_race_eth_templist_insample_4_inCI, 0.081 >= modelfits[[r]][[97]][1] & 0.081 <= modelfits[[r]][[97]][2] )
    bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist_4_inCI = c(bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist_4_inCI, 0.081 >= modelfits[[r]][[98]][1] & 0.081 <= modelfits[[r]][[98]][2] )
    bart_cate_school_achievement_3_min_comp_1_race_eth_templist_4_inCI = c(bart_cate_school_achievement_3_min_comp_1_race_eth_templist_4_inCI, 0.081 >= modelfits[[r]][[99]][1] & 0.081 <= modelfits[[r]][[99]][2] )
    insample_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_4_inCI = c(insample_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_4_inCI, 0.081 >= modelfits[[r]][[100]][1] & 0.081 <= modelfits[[r]][[100]][2] )
    ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_model_templist_4_inCI = c(ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_model_templist_4_inCI, 0.081 >= modelfits[[r]][[101]][1] & 0.081 <= modelfits[[r]][[101]][2] )
    ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_model_templist_4_inCI = c(ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_model_templist_4_inCI, 0.081 >= modelfits[[r]][[102]][1] & 0.081 <= modelfits[[r]][[102]][2] )
    
    cate_school_achievement_3_min_comp_1_race_eth_templist_5_inCI = c(cate_school_achievement_3_min_comp_1_race_eth_templist_5_inCI, 0.024 >= modelfits[[r]][[103]][1] & 0.024 <= modelfits[[r]][[103]][2] )
    cate_school_achievement_3_min_comp_1_race_eth_templist_insample_5_inCI = c(cate_school_achievement_3_min_comp_1_race_eth_templist_insample_5_inCI, 0.024 >= modelfits[[r]][[104]][1] & 0.024 <= modelfits[[r]][[104]][2] )
    bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist_5_inCI = c(bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist_5_inCI, 0.024 >= modelfits[[r]][[105]][1] & 0.024 <= modelfits[[r]][[105]][2] )
    bart_cate_school_achievement_3_min_comp_1_race_eth_templist_5_inCI = c(bart_cate_school_achievement_3_min_comp_1_race_eth_templist_5_inCI, 0.024 >= modelfits[[r]][[106]][1] & 0.024 <= modelfits[[r]][[106]][2] )
    insample_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_5_inCI = c(insample_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_5_inCI, 0.024 >= modelfits[[r]][[107]][1] & 0.024 <= modelfits[[r]][[107]][2] )
    ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_model_templist_5_inCI = c(ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_model_templist_5_inCI, 0.024 >= modelfits[[r]][[108]][1] & 0.024 <= modelfits[[r]][[108]][2] )
    ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_model_templist_5_inCI = c(ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_model_templist_5_inCI, 0.024 >= modelfits[[r]][[109]][1] & 0.024 <= modelfits[[r]][[109]][2] )
    
    ate_fullPS_list = c(ate_fullPS_list, modelfits[[r]][[110]][4])
    ate_fullPS_sd_list = c(ate_fullPS_sd_list, modelfits[[r]][[111]])
    cate_school_achievement_3_min_comp_1_fullPS_list = c(cate_school_achievement_3_min_comp_1_fullPS_list, modelfits[[r]][[112]][4])
    cate_school_achievement_3_min_comp_1_fullPS_sd_list = c(cate_school_achievement_3_min_comp_1_fullPS_sd_list, modelfits[[r]][[113]])
    cate_school_achievement_1_race_eth_2_fullPS_list = c(cate_school_achievement_1_race_eth_2_fullPS_list, modelfits[[r]][[114]][4])
    cate_school_achievement_1_race_eth_2_fullPS_sd_list = c(cate_school_achievement_1_race_eth_2_fullPS_sd_list, modelfits[[r]][[115]])
    cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_matrix = cbind(cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_matrix, unlist(lapply(X=modelfits[[r]][[116]],FUN="[[",4)) )
    cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_sd_matrix = cbind(cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_sd_matrix, unlist(modelfits[[r]][[117]]) )
    
    cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_1_inCI = c(cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_1_inCI, 0.024 >= modelfits[[r]][[118]][1] & 0.024 <= modelfits[[r]][[118]][2] )
    cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_2_inCI = c(cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_2_inCI, 0.081 >= modelfits[[r]][[119]][1] & 0.081 <= modelfits[[r]][[119]][2] )
    cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_3_inCI = c(cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_3_inCI, 0.06 >= modelfits[[r]][[120]][1] & 0.06 <= modelfits[[r]][[120]][2] )
    cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_4_inCI = c(cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_4_inCI, 0.081 >= modelfits[[r]][[121]][1] & 0.081 <= modelfits[[r]][[121]][2] )
    cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_5_inCI = c(cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_5_inCI, 0.024 >= modelfits[[r]][[122]][1] & 0.024 <= modelfits[[r]][[122]][2] )
    
    ate_fullPS_inCI = c(ate_fullPS_inCI, 0.126 >= modelfits[[r]][[123]][1] & 0.126 <= modelfits[[r]][[123]][2] )
    cate_school_achievement_3_min_comp_1_fullPS_inCI = c(cate_school_achievement_3_min_comp_1_fullPS_inCI, 0.065 >= modelfits[[r]][[124]][1] & 0.065 <= modelfits[[r]][[124]][2] )
    cate_school_achievement_1_race_eth_2_fullPS_inCI = c(cate_school_achievement_1_race_eth_2_fullPS_inCI,  0.195 >= modelfits[[r]][[125]][1] & 0.195 <= modelfits[[r]][[125]][2] )
    
    counter = counter + 1
  }
  
  # fill up list
  combined_ate_list = c(combined_ate_list, ate_list)
  combined_bart_ate_list = c(combined_bart_ate_list, bart_ate_list)
  combined_ate_sd_list = c(combined_ate_sd_list, ate_sd_list)
  combined_bart_ate_sd_list = c(combined_bart_ate_sd_list, bart_ate_sd_list)
  combined_ate_weighted_svyregression_list = c(combined_ate_weighted_svyregression_list, ate_weighted_svyregression_list)
  combined_ate_weighted_svyregression_sd_list = c(combined_ate_weighted_svyregression_sd_list, ate_weighted_svyregression_sd_list)
  combined_ate_unweighted_svyregression_list = c(combined_ate_unweighted_svyregression_list, ate_unweighted_svyregression_list)
  combined_ate_unweighted_svyregression_sd_list = c(combined_ate_unweighted_svyregression_sd_list, ate_unweighted_svyregression_sd_list)
  combined_ate_insample_list = c(combined_ate_insample_list, ate_insample_list)
  combined_ate_sd_insample_list = c(combined_ate_sd_insample_list, ate_insample_sd_list)
  
  combined_cate_school_achievement_3_min_comp_1_list = c(combined_cate_school_achievement_3_min_comp_1_list, cate_school_achievement_3_min_comp_1_list)
  combined_bart_cate_school_achievement_3_min_comp_1_list = c(combined_bart_cate_school_achievement_3_min_comp_1_list, bart_cate_school_achievement_3_min_comp_1_list)
  combined_cate_school_achievement_3_min_comp_1_sd_list = c(combined_cate_school_achievement_3_min_comp_1_sd_list, cate_school_achievement_3_min_comp_1_sd_list)
  combined_bart_cate_school_achievement_3_min_comp_1_sd_list = c(combined_bart_cate_school_achievement_3_min_comp_1_sd_list, bart_cate_school_achievement_3_min_comp_1_sd_list)
  combined_ate_unweighted_regression_CATE_school_achievement_3_min_comp_1_list = c(combined_ate_unweighted_regression_CATE_school_achievement_3_min_comp_1_list, ate_unweighted_regression_CATE_school_achievement_3_min_comp_1_list)
  combined_ate_unweighted_regression_CATE_school_achievement_3_min_comp_1_sd_list = c(combined_ate_unweighted_regression_CATE_school_achievement_3_min_comp_1_sd_list, ate_unweighted_regression_CATE_school_achievement_3_min_comp_1_sd_list)
  combined_ate_weighted_svyregression_school_achievement_index_3_min_comp_1_list = c(combined_ate_weighted_svyregression_school_achievement_index_3_min_comp_1_list, ate_weighted_svyregression_school_achievement_index_3_min_comp_1_list)
  combined_ate_weighted_svyregression_school_achievement_index_3_min_comp_1_sd_list = c(combined_ate_weighted_svyregression_school_achievement_index_3_min_comp_1_sd_list, ate_weighted_svyregression_school_achievement_index_3_min_comp_1_sd_list)
  combined_ate_weighted_svyregression_school_achievement_index_3_min_comp_1_noprecision_list = c(combined_ate_weighted_svyregression_school_achievement_index_3_min_comp_1_noprecision_list, ate_weighted_svyregression_school_achievement_index_3_min_comp_1_noprecision_list)
  combined_ate_weighted_svyregression_school_achievement_index_3_min_comp_1_noprecision_sd_list = c(combined_ate_weighted_svyregression_school_achievement_index_3_min_comp_1_noprecision_sd_list, ate_weighted_svyregression_school_achievement_index_3_min_comp_1_noprecision_sd_list)
  combined_cate_school_achievement_3_min_comp_1_insample_list = c(combined_cate_school_achievement_3_min_comp_1_insample_list, cate_school_achievement_3_min_comp_1_insample_list)
  combined_cate_school_achievement_3_min_comp_1_sd_insample_list = c(combined_cate_school_achievement_3_min_comp_1_sd_insample_list, cate_school_achievement_3_min_comp_1_sd_insample_list)
  
  combined_cate_school_achievement_1_race_eth_2_list = c(combined_cate_school_achievement_1_race_eth_2_list, cate_school_achievement_1_race_eth_2_list)
  combined_bart_cate_school_achievement_1_race_eth_2_list = c(combined_bart_cate_school_achievement_1_race_eth_2_list, bart_cate_school_achievement_1_race_eth_2_list)
  combined_cate_school_achievement_1_race_eth_2_sd_list = c(combined_cate_school_achievement_1_race_eth_2_sd_list, cate_school_achievement_1_race_eth_2_sd_list)
  combined_bart_cate_school_achievement_1_race_eth_2_sd_list = c(combined_bart_cate_school_achievement_1_race_eth_2_sd_list, bart_cate_school_achievement_1_race_eth_2_sd_list)
  combined_ate_unweighted_regression_CATE_school_achievement_1_race_eth_2_list = c(combined_ate_unweighted_regression_CATE_school_achievement_1_race_eth_2_list, ate_unweighted_regression_CATE_school_achievement_1_race_eth_2_list)
  combined_ate_unweighted_regression_CATE_school_achievement_1_race_eth_2_sd_list = c(combined_ate_unweighted_regression_CATE_school_achievement_1_race_eth_2_sd_list, ate_unweighted_regression_CATE_school_achievement_1_race_eth_2_sd_list)
  combined_ate_weighted_svyregression_school_achievement_index_1_race_eth_2_list = c(combined_ate_weighted_svyregression_school_achievement_index_1_race_eth_2_list, ate_weighted_svyregression_school_achievement_index_1_race_eth_2_list)
  combined_ate_weighted_svyregression_school_achievement_index_1_race_eth_2_sd_list = c(combined_ate_weighted_svyregression_school_achievement_index_1_race_eth_2_sd_list, ate_weighted_svyregression_school_achievement_index_1_race_eth_2_sd_list)
  combined_ate_weighted_svyregression_school_achievement_index_1_race_eth_2_noprecision_list = c(combined_ate_weighted_svyregression_school_achievement_index_1_race_eth_2_noprecision_list, ate_weighted_svyregression_school_achievement_index_1_race_eth_2_noprecision_list)
  combined_ate_weighted_svyregression_school_achievement_index_1_race_eth_2_noprecision_sd_list = c(combined_ate_weighted_svyregression_school_achievement_index_1_race_eth_2_noprecision_sd_list, ate_weighted_svyregression_school_achievement_index_1_race_eth_2_noprecision_sd_list)
  combined_cate_school_achievement_1_race_eth_2_insample_list = c(combined_cate_school_achievement_1_race_eth_2_insample_list, cate_school_achievement_1_race_eth_2_insample_list)
  combined_cate_school_achievement_1_race_eth_2_sd_insample_list = c(combined_cate_school_achievement_1_race_eth_2_sd_insample_list, cate_school_achievement_1_race_eth_2_sd_insample_list)
  
  
  combined_insample_bart_ate_list = c(combined_insample_bart_ate_list, insample_bart_ate_list)
  combined_insample_bart_ate_sd_list = c(combined_insample_bart_ate_sd_list, insample_bart_ate_sd_list)
  combined_insample_bart_cate_school_achievement_3_min_comp_1_list = c(combined_insample_bart_cate_school_achievement_3_min_comp_1_list, insample_bart_cate_school_achievement_3_min_comp_1_list)
  combined_insample_bart_cate_school_achievement_3_min_comp_1_sd_list = c(combined_insample_bart_cate_school_achievement_3_min_comp_1_sd_list, insample_bart_cate_school_achievement_3_min_comp_1_sd_list)
  combined_insample_bart_cate_school_achievement_1_race_eth_2_list = c(combined_insample_bart_cate_school_achievement_1_race_eth_2_list, insample_bart_cate_school_achievement_1_race_eth_2_list)
  combined_insample_bart_cate_school_achievement_1_race_eth_2_sd_list = c(combined_insample_bart_cate_school_achievement_1_race_eth_2_sd_list, insample_bart_cate_school_achievement_1_race_eth_2_sd_list)
  
  
  combined_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix = cbind(combined_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix, cate_school_achievement_3_min_comp_1_race_eth_templist_matrix)
  combined_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix = cbind(combined_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix, bart_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix)
  combined_insample_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix = cbind(combined_insample_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix, insample_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix)
  
  combined_cate_school_achievement_3_min_comp_1_race_eth_templist_sd_matrix = cbind(combined_cate_school_achievement_3_min_comp_1_race_eth_templist_sd_matrix, cate_school_achievement_3_min_comp_1_race_eth_templist_sd_matrix)
  combined_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_sd_matrix = cbind(combined_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_sd_matrix, bart_cate_school_achievement_3_min_comp_1_race_eth_templist_sd_matrix)
  combined_insample_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_sd_matrix = cbind(combined_insample_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_sd_matrix, insample_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_sd_matrix)
  
  combined_ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_templist_matrix = cbind(combined_ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_templist_matrix, ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_templist_matrix)
  combined_ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_templist_matrix = cbind(combined_ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_templist_matrix, ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_templist_matrix)
  
  combined_ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_templist_sd_matrix = cbind(combined_ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_templist_sd_matrix, ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_templist_sd_matrix)
  combined_ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_templist_sd_matrix = cbind(combined_ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_templist_sd_matrix, ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_templist_sd_matrix)
  
  
  combined_bart_usingposteriordistribution_ate_list = c(combined_bart_usingposteriordistribution_ate_list, bart_usingposteriordistribution_ate_list)
  combined_bart_usingposteriordistribution_ate_sd_list = c(combined_bart_usingposteriordistribution_ate_sd_list, bart_usingposteriordistribution_ate_sd_list)
  combined_bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_list = c(combined_bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_list, bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_list)
  combined_bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_sd_list = c(combined_bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_sd_list, bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_sd_list)
  combined_bart_usingposteriordistribution_cate_school_achievement_1_race_eth_2_list =  c(combined_bart_usingposteriordistribution_cate_school_achievement_1_race_eth_2_list, bart_usingposteriordistribution_cate_school_achievement_1_race_eth_2_list)
  combined_bart_usingposteriordistribution_cate_school_achievement_1_race_eth_2_sd_list = c(combined_bart_usingposteriordistribution_cate_school_achievement_1_race_eth_2_sd_list, bart_usingposteriordistribution_cate_school_achievement_1_race_eth_2_sd_list)
  combined_bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix = cbind(combined_bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix, bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix)
  combined_bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist_sd_matrix = cbind(combined_bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist_sd_matrix, bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist_sd_matrix)
  
  combined_ate_inCI = c(combined_ate_inCI, ate_inCI)
  combined_ate_insample_inCI = c(combined_ate_insample_inCI, ate_insample_inCI)
  combined_bart_usingposteriordistribution_ate_inCI = c(combined_bart_usingposteriordistribution_ate_inCI, bart_usingposteriordistribution_ate_inCI)
  combined_bart_ate_inCI = c(combined_bart_ate_inCI, bart_ate_inCI)
  combined_insample_bart_ate_inCI = c(combined_insample_bart_ate_inCI, insample_bart_ate_inCI)
  combined_ate_weighted_svyregression_inCI = c(combined_ate_weighted_svyregression_inCI, ate_weighted_svyregression_inCI)
  combined_ate_unweighted_svyregression_inCI = c(combined_ate_unweighted_svyregression_inCI, ate_unweighted_svyregression_inCI)
  
  combined_cate_school_achievement_3_min_comp_1_inCI = c(combined_cate_school_achievement_3_min_comp_1_inCI, cate_school_achievement_3_min_comp_1_inCI)
  combined_cate_school_achievement_3_min_comp_1_insample_inCI = c(combined_cate_school_achievement_3_min_comp_1_insample_inCI, cate_school_achievement_3_min_comp_1_insample_inCI)
  combined_bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_inCI = c(combined_bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_inCI, bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_inCI)
  combined_bart_cate_school_achievement_3_min_comp_1_inCI = c(combined_bart_cate_school_achievement_3_min_comp_1_inCI, bart_cate_school_achievement_3_min_comp_1_inCI)
  combined_insample_bart_cate_school_achievement_3_min_comp_1_inCI = c(combined_insample_bart_cate_school_achievement_3_min_comp_1_inCI, insample_bart_cate_school_achievement_3_min_comp_1_inCI)
  combined_cate_school_achievement_3_min_comp_1_weighted_svyregression_inCI = c(combined_cate_school_achievement_3_min_comp_1_weighted_svyregression_inCI, cate_school_achievement_3_min_comp_1_weighted_svyregression_inCI)
  combined_cate_school_achievement_3_min_comp_1_unweighted_svyregression_inCI = c(combined_cate_school_achievement_3_min_comp_1_unweighted_svyregression_inCI, cate_school_achievement_3_min_comp_1_unweighted_svyregression_inCI)
  
  combined_cate_school_achievement_1_race_eth_2_inCI = c(combined_cate_school_achievement_1_race_eth_2_inCI, cate_school_achievement_1_race_eth_2_inCI)
  combined_cate_school_achievement_1_race_eth_2_insample_inCI = c(combined_cate_school_achievement_1_race_eth_2_insample_inCI, cate_school_achievement_1_race_eth_2_insample_inCI)
  combined_bart_usingposteriordistribution_cate_school_achievement_1_race_eth_2_inCI = c(combined_bart_usingposteriordistribution_cate_school_achievement_1_race_eth_2_inCI, bart_usingposteriordistribution_cate_school_achievement_1_race_eth_2_inCI)
  combined_bart_cate_school_achievement_1_race_eth_2_inCI = c(combined_bart_cate_school_achievement_1_race_eth_2_inCI, bart_cate_school_achievement_1_race_eth_2_inCI)
  combined_insample_bart_cate_school_achievement_1_race_eth_2_inCI = c(combined_insample_bart_cate_school_achievement_1_race_eth_2_inCI, insample_bart_cate_school_achievement_1_race_eth_2_inCI)
  combined_cate_school_achievement_1_race_eth_2_weighted_svyregression_inCI = c(combined_cate_school_achievement_1_race_eth_2_weighted_svyregression_inCI, cate_school_achievement_1_race_eth_2_weighted_svyregression_inCI)
  combined_cate_school_achievement_1_race_eth_2_unweighted_svyregression_inCI = c(combined_cate_school_achievement_1_race_eth_2_unweighted_svyregression_inCI, cate_school_achievement_1_race_eth_2_unweighted_svyregression_inCI)
  
  
  combined_cate_school_achievement_3_min_comp_1_race_eth_templist_1_inCI = c(combined_cate_school_achievement_3_min_comp_1_race_eth_templist_1_inCI, cate_school_achievement_3_min_comp_1_race_eth_templist_1_inCI)
  combined_cate_school_achievement_3_min_comp_1_race_eth_templist_insample_1_inCI = c(combined_cate_school_achievement_3_min_comp_1_race_eth_templist_insample_1_inCI, cate_school_achievement_3_min_comp_1_race_eth_templist_insample_1_inCI)
  combined_bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist_1_inCI = c(combined_bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist_1_inCI, bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist_1_inCI)
  combined_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_1_inCI = c(combined_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_1_inCI, bart_cate_school_achievement_3_min_comp_1_race_eth_templist_1_inCI)
  combined_insample_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_1_inCI = c(combined_insample_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_1_inCI, insample_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_1_inCI)
  combined_ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_model_templist_1_inCI = c(combined_ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_model_templist_1_inCI, ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_model_templist_1_inCI)
  combined_ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_model_templist_1_inCI = c(combined_ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_model_templist_1_inCI, ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_model_templist_1_inCI)
  
  combined_cate_school_achievement_3_min_comp_1_race_eth_templist_2_inCI = c(combined_cate_school_achievement_3_min_comp_1_race_eth_templist_2_inCI, cate_school_achievement_3_min_comp_1_race_eth_templist_2_inCI)
  combined_cate_school_achievement_3_min_comp_1_race_eth_templist_insample_2_inCI = c(combined_cate_school_achievement_3_min_comp_1_race_eth_templist_insample_2_inCI, cate_school_achievement_3_min_comp_1_race_eth_templist_insample_2_inCI)
  combined_bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist_2_inCI = c(combined_bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist_2_inCI, bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist_2_inCI)
  combined_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_2_inCI = c(combined_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_2_inCI, bart_cate_school_achievement_3_min_comp_1_race_eth_templist_2_inCI)
  combined_insample_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_2_inCI = c(combined_insample_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_2_inCI, insample_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_2_inCI)
  combined_ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_model_templist_2_inCI = c(combined_ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_model_templist_2_inCI, ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_model_templist_2_inCI)
  combined_ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_model_templist_2_inCI = c(combined_ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_model_templist_2_inCI, ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_model_templist_2_inCI)
  
  combined_cate_school_achievement_3_min_comp_1_race_eth_templist_3_inCI = c(combined_cate_school_achievement_3_min_comp_1_race_eth_templist_3_inCI, cate_school_achievement_3_min_comp_1_race_eth_templist_3_inCI)
  combined_cate_school_achievement_3_min_comp_1_race_eth_templist_insample_3_inCI = c(combined_cate_school_achievement_3_min_comp_1_race_eth_templist_insample_3_inCI, cate_school_achievement_3_min_comp_1_race_eth_templist_insample_3_inCI)
  combined_bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist_3_inCI = c(combined_bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist_3_inCI, bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist_3_inCI)
  combined_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_3_inCI = c(combined_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_3_inCI, bart_cate_school_achievement_3_min_comp_1_race_eth_templist_3_inCI)
  combined_insample_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_3_inCI = c(combined_insample_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_3_inCI, insample_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_3_inCI)
  combined_ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_model_templist_3_inCI = c(combined_ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_model_templist_3_inCI, ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_model_templist_3_inCI)
  combined_ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_model_templist_3_inCI = c(combined_ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_model_templist_3_inCI, ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_model_templist_3_inCI)
  
  combined_cate_school_achievement_3_min_comp_1_race_eth_templist_4_inCI = c(combined_cate_school_achievement_3_min_comp_1_race_eth_templist_4_inCI, cate_school_achievement_3_min_comp_1_race_eth_templist_4_inCI)
  combined_cate_school_achievement_3_min_comp_1_race_eth_templist_insample_4_inCI = c(combined_cate_school_achievement_3_min_comp_1_race_eth_templist_insample_4_inCI, cate_school_achievement_3_min_comp_1_race_eth_templist_insample_4_inCI)
  combined_bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist_4_inCI = c(combined_bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist_4_inCI, bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist_4_inCI)
  combined_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_4_inCI = c(combined_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_4_inCI, bart_cate_school_achievement_3_min_comp_1_race_eth_templist_4_inCI)
  combined_insample_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_4_inCI = c(combined_insample_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_4_inCI, insample_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_4_inCI)
  combined_ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_model_templist_4_inCI = c(combined_ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_model_templist_4_inCI, ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_model_templist_4_inCI)
  combined_ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_model_templist_4_inCI = c(combined_ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_model_templist_4_inCI, ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_model_templist_4_inCI)
  
  combined_cate_school_achievement_3_min_comp_1_race_eth_templist_5_inCI = c(combined_cate_school_achievement_3_min_comp_1_race_eth_templist_5_inCI, cate_school_achievement_3_min_comp_1_race_eth_templist_5_inCI)
  combined_cate_school_achievement_3_min_comp_1_race_eth_templist_insample_5_inCI = c(combined_cate_school_achievement_3_min_comp_1_race_eth_templist_insample_5_inCI, cate_school_achievement_3_min_comp_1_race_eth_templist_insample_5_inCI)
  combined_bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist_5_inCI = c(combined_bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist_5_inCI, bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist_5_inCI)
  combined_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_5_inCI = c(combined_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_5_inCI, bart_cate_school_achievement_3_min_comp_1_race_eth_templist_5_inCI)
  combined_insample_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_5_inCI = c(combined_insample_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_5_inCI, insample_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_5_inCI)
  combined_ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_model_templist_5_inCI = c(combined_ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_model_templist_5_inCI, ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_model_templist_5_inCI)
  combined_ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_model_templist_5_inCI = c(combined_ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_model_templist_5_inCI, ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_model_templist_5_inCI)
  
  combined_ate_fullPS_list = c(combined_ate_fullPS_list, ate_fullPS_list)
  combined_ate_fullPS_sd_list = c(combined_ate_fullPS_sd_list, ate_fullPS_sd_list)
  combined_cate_school_achievement_3_min_comp_1_fullPS_list = c(combined_cate_school_achievement_3_min_comp_1_fullPS_list, cate_school_achievement_3_min_comp_1_fullPS_list)
  combined_cate_school_achievement_3_min_comp_1_fullPS_sd_list = c(combined_cate_school_achievement_3_min_comp_1_fullPS_sd_list, cate_school_achievement_3_min_comp_1_fullPS_sd_list)
  combined_cate_school_achievement_1_race_eth_2_fullPS_list = c(combined_cate_school_achievement_1_race_eth_2_fullPS_list, cate_school_achievement_1_race_eth_2_fullPS_list)
  combined_cate_school_achievement_1_race_eth_2_fullPS_sd_list = c(combined_cate_school_achievement_1_race_eth_2_fullPS_sd_list, cate_school_achievement_1_race_eth_2_fullPS_sd_list)
  combined_cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_matrix = cbind(combined_cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_matrix, cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_matrix)
  combined_cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_sd_matrix = cbind(combined_cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_sd_matrix, cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_sd_matrix)
  
  combined_cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_1_inCI = c(combined_cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_1_inCI, cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_1_inCI)
  combined_cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_2_inCI = c(combined_cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_2_inCI, cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_2_inCI)
  combined_cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_3_inCI = c(combined_cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_3_inCI, cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_3_inCI)
  combined_cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_4_inCI = c(combined_cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_4_inCI, cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_4_inCI)
  combined_cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_5_inCI = c(combined_cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_5_inCI, cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_5_inCI)
  
  combined_ate_fullPS_inCI = c(combined_ate_fullPS_inCI, ate_fullPS_inCI)
  combined_cate_school_achievement_3_min_comp_1_fullPS_inCI = c(combined_cate_school_achievement_3_min_comp_1_fullPS_inCI, cate_school_achievement_3_min_comp_1_fullPS_inCI)
  combined_cate_school_achievement_1_race_eth_2_fullPS_inCI = c(combined_cate_school_achievement_1_race_eth_2_fullPS_inCI, cate_school_achievement_1_race_eth_2_fullPS_inCI)
  
}


coverage_df = data.frame(model = c("MRP-MI", "BART", "BARP", "SVY", "OLS", "MRP"),
                         
                         ate_coverage = c(
                           mean(combined_ate_inCI),
                           mean(combined_bart_usingposteriordistribution_ate_inCI),
                           mean(combined_bart_ate_inCI),
                           mean(combined_ate_weighted_svyregression_inCI),
                           mean(combined_ate_unweighted_svyregression_inCI),
                           mean(combined_ate_fullPS_inCI)
                         ),
                         
                         cate_school_achievement_3_min_comp_1_coverage = c(
                           mean(combined_cate_school_achievement_3_min_comp_1_inCI),
                           mean(combined_bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_inCI),
                           mean(combined_bart_cate_school_achievement_3_min_comp_1_inCI),
                           mean(combined_cate_school_achievement_3_min_comp_1_weighted_svyregression_inCI),
                           mean(combined_cate_school_achievement_3_min_comp_1_unweighted_svyregression_inCI),
                           mean(combined_cate_school_achievement_3_min_comp_1_fullPS_inCI)
                         ),
                         
                         cate_school_achievement_1_race_eth_2_coverage = c(
                           mean(combined_cate_school_achievement_1_race_eth_2_inCI),
                           mean(combined_bart_usingposteriordistribution_cate_school_achievement_1_race_eth_2_inCI),
                           mean(combined_bart_cate_school_achievement_1_race_eth_2_inCI),
                           mean(combined_cate_school_achievement_1_race_eth_2_weighted_svyregression_inCI),
                           mean(combined_cate_school_achievement_1_race_eth_2_unweighted_svyregression_inCI),
                           mean(combined_cate_school_achievement_1_race_eth_2_fullPS_inCI)
                         )
                         
)

xtable::xtable(coverage_df)

coverage_df_cates = data.frame(model = c("MRP-MI", "BART", "BARP", "SVY", "OLS", "MRP"),
                               
                               cate_school_achievement_3_min_comp_1_race_eth_1_coverage = c(
                                 mean(combined_cate_school_achievement_3_min_comp_1_race_eth_templist_1_inCI),
                                 mean(combined_bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist_1_inCI),
                                 mean(combined_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_1_inCI),
                                 mean(combined_ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_model_templist_1_inCI),
                                 mean(combined_ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_model_templist_1_inCI),
                                 mean(combined_cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_1_inCI)
                               ),
                               
                               cate_school_achievement_3_min_comp_1_race_eth_2_coverage = c(
                                 mean(combined_cate_school_achievement_3_min_comp_1_race_eth_templist_2_inCI),
                                 mean(combined_bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist_2_inCI),
                                 mean(combined_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_2_inCI),
                                 mean(combined_ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_model_templist_2_inCI),
                                 mean(combined_ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_model_templist_2_inCI),
                                 mean(combined_cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_2_inCI)
                               ),
                               
                               cate_school_achievement_3_min_comp_1_race_eth_3_coverage = c(
                                 mean(combined_cate_school_achievement_3_min_comp_1_race_eth_templist_3_inCI),
                                 mean(combined_bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist_3_inCI),
                                 mean(combined_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_3_inCI),
                                 mean(combined_ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_model_templist_3_inCI),
                                 mean(combined_ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_model_templist_3_inCI),
                                 mean(combined_cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_3_inCI)
                               ),
                               
                               cate_school_achievement_3_min_comp_1_race_eth_4_coverage = c(
                                 mean(combined_cate_school_achievement_3_min_comp_1_race_eth_templist_4_inCI),
                                 mean(combined_bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist_4_inCI),
                                 mean(combined_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_4_inCI),
                                 mean(combined_ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_model_templist_4_inCI),
                                 mean(combined_ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_model_templist_4_inCI),
                                 mean(combined_cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_4_inCI)
                               ),
                               
                               cate_school_achievement_3_min_comp_1_race_eth_5_coverage = c(
                                 mean(combined_cate_school_achievement_3_min_comp_1_race_eth_templist_5_inCI),
                                 mean(combined_bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist_5_inCI),
                                 mean(combined_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_5_inCI),
                                 mean(combined_ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_model_templist_5_inCI),
                                 mean(combined_ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_model_templist_5_inCI),
                                 mean(combined_cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_5_inCI)
                               )
)

xtable::xtable(coverage_df_cates)


# # visualization of SA3 x MC1 x race/eth -----------------------
SA3_MC1_race_eth_cate_truth = c(0.024, 0.081, 0.06, 0.081, 0.024)

# 
cate_sa3_mc1_race_eth_df = data.frame(model = c("MRP-MI", "BARP", "BART", "OLS", "SVY", "MRP"),
                                      re1_l2 = c(
                                        (mean(combined_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix[1,]) - SA3_MC1_race_eth_cate_truth[1])^2 + mean((combined_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix[1,] - mean(combined_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix[1,]))^2),
                                        (mean(combined_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix[1,]) - SA3_MC1_race_eth_cate_truth[1])^2 + mean((combined_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix[1,] - mean(combined_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix[1,]))^2),
                                        (mean(combined_bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix[1,]) - SA3_MC1_race_eth_cate_truth[1])^2 + mean((combined_insample_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix[1,] - mean(combined_insample_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix[1,]))^2),
                                        (mean(combined_ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_templist_matrix[1,]) - SA3_MC1_race_eth_cate_truth[1])^2 + mean((combined_ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_templist_matrix[1,] - mean(combined_ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_templist_matrix[1,]))^2),
                                        (mean(combined_ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_templist_matrix[1,]) - SA3_MC1_race_eth_cate_truth[1])^2 + mean((combined_ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_templist_matrix[1,] - mean(combined_ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_templist_matrix[1,]))^2),
                                        (mean(combined_cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_matrix[1,]) - SA3_MC1_race_eth_cate_truth[1])^2 + mean((combined_cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_matrix[1,] - mean(combined_cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_matrix[1,]))^2)
                                      ),
                                      re1_psr = c(
                                        mean(-((combined_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix[1,] - SA3_MC1_race_eth_cate_truth[1])/(combined_cate_school_achievement_3_min_comp_1_race_eth_templist_sd_matrix[1,]))^2 - log(combined_cate_school_achievement_3_min_comp_1_race_eth_templist_sd_matrix[1,]^2)),
                                        mean(-((combined_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix[1,] - SA3_MC1_race_eth_cate_truth[1])/(combined_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_sd_matrix[1,]))^2 - log(combined_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_sd_matrix[1,]^2)),
                                        mean(-((combined_bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix[1,] - SA3_MC1_race_eth_cate_truth[1])/(combined_bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist_sd_matrix[1,]))^2 - log(combined_bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist_sd_matrix[1,]^2)),
                                        mean(-((combined_ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_templist_matrix[1,] - SA3_MC1_race_eth_cate_truth[1])/(combined_ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_templist_sd_matrix[1,]))^2 - log(combined_ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_templist_sd_matrix[1,]^2)),
                                        mean(replace(-((combined_ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_templist_matrix[1,] - SA3_MC1_race_eth_cate_truth[1])/(combined_ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_templist_sd_matrix[1,]))^2 - log(combined_ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_templist_sd_matrix[1,]^2),
                                                is.infinite(-((combined_ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_templist_matrix[1,] - SA3_MC1_race_eth_cate_truth[1])/(combined_ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_templist_sd_matrix[1,]))^2 - log(combined_ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_templist_sd_matrix[1,]^2)),
                                                NA), na.rm=TRUE),
                                        mean(-((combined_cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_matrix[1,] - SA3_MC1_race_eth_cate_truth[1])/(combined_cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_sd_matrix[1,]))^2 - log(combined_cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_sd_matrix[1,]^2))
                                      ),

                                      re2_l2 = c(
                                        (mean(combined_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix[2,]) - SA3_MC1_race_eth_cate_truth[2])^2 + mean((combined_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix[2,] - mean(combined_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix[2,]))^2),
                                        (mean(combined_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix[2,]) - SA3_MC1_race_eth_cate_truth[2])^2 + mean((combined_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix[2,] - mean(combined_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix[2,]))^2),
                                        (mean(combined_bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix[2,]) - SA3_MC1_race_eth_cate_truth[2])^2 + mean((combined_insample_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix[2,] - mean(combined_insample_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix[2,]))^2),
                                        (mean(combined_ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_templist_matrix[2,]) - SA3_MC1_race_eth_cate_truth[2])^2 + mean((combined_ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_templist_matrix[2,] - mean(combined_ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_templist_matrix[2,]))^2),
                                        (mean(combined_ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_templist_matrix[2,]) - SA3_MC1_race_eth_cate_truth[2])^2 + mean((combined_ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_templist_matrix[2,] - mean(combined_ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_templist_matrix[2,]))^2),
                                        (mean(combined_cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_matrix[2,]) - SA3_MC1_race_eth_cate_truth[2])^2 + mean((combined_cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_matrix[2,] - mean(combined_cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_matrix[2,]))^2)
                                      ),
                                      re2_psr = c(
                                        mean(-((combined_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix[2,] - SA3_MC1_race_eth_cate_truth[2])/(combined_cate_school_achievement_3_min_comp_1_race_eth_templist_sd_matrix[2,]))^2 - log(combined_cate_school_achievement_3_min_comp_1_race_eth_templist_sd_matrix[2,]^2)),
                                        mean(-((combined_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix[2,] - SA3_MC1_race_eth_cate_truth[2])/(combined_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_sd_matrix[2,]))^2 - log(combined_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_sd_matrix[2,]^2)),
                                        mean(-((combined_bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix[2,] - SA3_MC1_race_eth_cate_truth[2])/(combined_bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist_sd_matrix[2,]))^2 - log(combined_bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist_sd_matrix[2,]^2)),
                                        mean(-((combined_ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_templist_matrix[2,] - SA3_MC1_race_eth_cate_truth[2])/(combined_ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_templist_sd_matrix[2,]))^2 - log(combined_ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_templist_sd_matrix[2,]^2)),
                                        mean(replace(-((combined_ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_templist_matrix[2,] - SA3_MC1_race_eth_cate_truth[2])/(combined_ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_templist_sd_matrix[2,]))^2 - log(combined_ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_templist_sd_matrix[2,]^2),
                                                     is.infinite(-((combined_ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_templist_matrix[2,] - SA3_MC1_race_eth_cate_truth[2])/(combined_ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_templist_sd_matrix[2,]))^2 - log(combined_ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_templist_sd_matrix[2,]^2)),
                                                     NA), na.rm=TRUE),
                                        mean(-((combined_cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_matrix[2,] - SA3_MC1_race_eth_cate_truth[2])/(combined_cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_sd_matrix[2,]))^2 - log(combined_cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_sd_matrix[2,]^2))
                                      ),

                                      re3_l2 = c(
                                        (mean(combined_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix[3,]) - SA3_MC1_race_eth_cate_truth[3])^2 + mean((combined_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix[3,] - mean(combined_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix[3,]))^2),
                                        (mean(combined_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix[3,]) - SA3_MC1_race_eth_cate_truth[3])^2 + mean((combined_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix[3,] - mean(combined_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix[3,]))^2),
                                        (mean(combined_bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix[3,]) - SA3_MC1_race_eth_cate_truth[3])^2 + mean((combined_insample_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix[3,] - mean(combined_insample_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix[3,]))^2),
                                        (mean(combined_ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_templist_matrix[3,]) - SA3_MC1_race_eth_cate_truth[3])^2 + mean((combined_ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_templist_matrix[3,] - mean(combined_ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_templist_matrix[3,]))^2),
                                        (mean(combined_ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_templist_matrix[3,]) - SA3_MC1_race_eth_cate_truth[3])^2 + mean((combined_ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_templist_matrix[3,] - mean(combined_ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_templist_matrix[3,]))^2),
                                        (mean(combined_cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_matrix[3,]) - SA3_MC1_race_eth_cate_truth[3])^2 + mean((combined_cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_matrix[3,] - mean(combined_cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_matrix[3,]))^2)
                                      ),
                                      re3_psr = c(
                                        mean(-((combined_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix[3,] - SA3_MC1_race_eth_cate_truth[3])/(combined_cate_school_achievement_3_min_comp_1_race_eth_templist_sd_matrix[3,]))^2 - log(combined_cate_school_achievement_3_min_comp_1_race_eth_templist_sd_matrix[3,]^2)),
                                        mean(-((combined_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix[3,] - SA3_MC1_race_eth_cate_truth[3])/(combined_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_sd_matrix[3,]))^2 - log(combined_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_sd_matrix[3,]^2)),
                                        mean(-((combined_bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix[3,] - SA3_MC1_race_eth_cate_truth[3])/(combined_bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist_sd_matrix[3,]))^2 - log(combined_bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist_sd_matrix[3,]^2)),
                                        mean(-((combined_ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_templist_matrix[3,] - SA3_MC1_race_eth_cate_truth[3])/(combined_ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_templist_sd_matrix[3,]))^2 - log(combined_ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_templist_sd_matrix[3,]^2)),
                                        mean(replace(-((combined_ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_templist_matrix[3,] - SA3_MC1_race_eth_cate_truth[3])/(combined_ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_templist_sd_matrix[3,]))^2 - log(combined_ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_templist_sd_matrix[3,]^2),
                                                     is.infinite(-((combined_ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_templist_matrix[3,] - SA3_MC1_race_eth_cate_truth[3])/(combined_ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_templist_sd_matrix[3,]))^2 - log(combined_ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_templist_sd_matrix[3,]^2)),
                                                     NA), na.rm=TRUE),
                                        mean(-((combined_cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_matrix[3,] - SA3_MC1_race_eth_cate_truth[3])/(combined_cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_sd_matrix[3,]))^2 - log(combined_cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_sd_matrix[3,]^2))
                                      ),

                                      re4_l2 = c(
                                        (mean(combined_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix[4,]) - SA3_MC1_race_eth_cate_truth[4])^2 + mean((combined_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix[4,] - mean(combined_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix[4,]))^2),
                                        (mean(combined_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix[4,]) - SA3_MC1_race_eth_cate_truth[4])^2 + mean((combined_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix[4,] - mean(combined_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix[4,]))^2),
                                        (mean(combined_bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix[4,]) - SA3_MC1_race_eth_cate_truth[4])^2 + mean((combined_insample_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix[4,] - mean(combined_insample_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix[4,]))^2),
                                        (mean(combined_ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_templist_matrix[4,]) - SA3_MC1_race_eth_cate_truth[4])^2 + mean((combined_ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_templist_matrix[4,] - mean(combined_ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_templist_matrix[4,]))^2),
                                        (mean(combined_ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_templist_matrix[4,]) - SA3_MC1_race_eth_cate_truth[4])^2 + mean((combined_ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_templist_matrix[4,] - mean(combined_ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_templist_matrix[4,]))^2),
                                        (mean(combined_cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_matrix[4,]) - SA3_MC1_race_eth_cate_truth[4])^2 + mean((combined_cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_matrix[4,] - mean(combined_cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_matrix[4,]))^2)
                                      ),
                                      re4_psr = c(
                                        mean(-((combined_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix[4,] - SA3_MC1_race_eth_cate_truth[4])/(combined_cate_school_achievement_3_min_comp_1_race_eth_templist_sd_matrix[4,]))^2 - log(combined_cate_school_achievement_3_min_comp_1_race_eth_templist_sd_matrix[4,]^2)),
                                        mean(-((combined_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix[4,] - SA3_MC1_race_eth_cate_truth[4])/(combined_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_sd_matrix[4,]))^2 - log(combined_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_sd_matrix[4,]^2)),
                                        mean(-((combined_bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix[4,] - SA3_MC1_race_eth_cate_truth[4])/(combined_bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist_sd_matrix[4,]))^2 - log(combined_bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist_sd_matrix[4,]^2)),
                                        mean(-((combined_ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_templist_matrix[4,] - SA3_MC1_race_eth_cate_truth[4])/(combined_ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_templist_sd_matrix[4,]))^2 - log(combined_ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_templist_sd_matrix[4,]^2)),
                                        mean(replace(-((combined_ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_templist_matrix[4,] - SA3_MC1_race_eth_cate_truth[4])/(combined_ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_templist_sd_matrix[4,]))^2 - log(combined_ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_templist_sd_matrix[4,]^2),
                                                     is.infinite(-((combined_ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_templist_matrix[4,] - SA3_MC1_race_eth_cate_truth[4])/(combined_ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_templist_sd_matrix[4,]))^2 - log(combined_ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_templist_sd_matrix[4,]^2)),
                                                     NA), na.rm=TRUE),
                                        mean(-((combined_cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_matrix[4,] - SA3_MC1_race_eth_cate_truth[4])/(combined_cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_sd_matrix[4,]))^2 - log(combined_cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_sd_matrix[4,]^2))
                                      ),

                                      re5_l2 = c(
                                        (mean(combined_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix[5,]) - SA3_MC1_race_eth_cate_truth[5])^2 + mean((combined_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix[5,] - mean(combined_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix[5,]))^2),
                                        (mean(combined_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix[5,]) - SA3_MC1_race_eth_cate_truth[5])^2 + mean((combined_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix[5,] - mean(combined_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix[5,]))^2),
                                        (mean(combined_bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix[5,]) - SA3_MC1_race_eth_cate_truth[5])^2 + mean((combined_insample_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix[5,] - mean(combined_insample_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix[5,]))^2),
                                        (mean(combined_ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_templist_matrix[5,]) - SA3_MC1_race_eth_cate_truth[5])^2 + mean((combined_ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_templist_matrix[5,] - mean(combined_ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_templist_matrix[5,]))^2),
                                        (mean(combined_ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_templist_matrix[5,]) - SA3_MC1_race_eth_cate_truth[5])^2 + mean((combined_ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_templist_matrix[5,] - mean(combined_ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_templist_matrix[5,]))^2),
                                        (mean(combined_cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_matrix[5,]) - SA3_MC1_race_eth_cate_truth[5])^2 + mean((combined_cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_matrix[5,] - mean(combined_cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_matrix[5,]))^2)
                                      ),
                                      re5_psr = c(
                                        mean(-((combined_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix[5,] - SA3_MC1_race_eth_cate_truth[5])/(combined_cate_school_achievement_3_min_comp_1_race_eth_templist_sd_matrix[5,]))^2 - log(combined_cate_school_achievement_3_min_comp_1_race_eth_templist_sd_matrix[5,]^2)),
                                        mean(-((combined_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix[5,] - SA3_MC1_race_eth_cate_truth[5])/(combined_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_sd_matrix[5,]))^2 - log(combined_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_sd_matrix[5,]^2)),
                                        mean(-((combined_bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix[5,] - SA3_MC1_race_eth_cate_truth[5])/(combined_bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist_sd_matrix[5,]))^2 - log(combined_bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist_sd_matrix[5,]^2)),
                                        mean(-((combined_ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_templist_matrix[5,] - SA3_MC1_race_eth_cate_truth[5])/(combined_ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_templist_sd_matrix[5,]))^2 - log(combined_ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_templist_sd_matrix[5,]^2)),
                                        mean(replace(-((combined_ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_templist_matrix[5,] - SA3_MC1_race_eth_cate_truth[5])/(combined_ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_templist_sd_matrix[5,]))^2 - log(combined_ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_templist_sd_matrix[5,]^2),
                                                     is.infinite(-((combined_ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_templist_matrix[5,] - SA3_MC1_race_eth_cate_truth[5])/(combined_ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_templist_sd_matrix[5,]))^2 - log(combined_ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_templist_sd_matrix[5,]^2)),
                                                     NA), na.rm=TRUE),
                                        mean(-((combined_cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_matrix[5,] - SA3_MC1_race_eth_cate_truth[5])/(combined_cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_sd_matrix[5,]))^2 - log(combined_cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_sd_matrix[5,]^2))
                                      )

                                      )

xtable::xtable(cate_sa3_mc1_race_eth_df,digits=4)
xtable::xtable(format(cate_sa3_mc1_race_eth_df, scientific=TRUE))
# cbind(cate_sa3_mc1_race_eth_df[,1],cate_sa3_mc1_race_eth_df[,-1]*10000)

# visualization of schools -------------------------------------
CATE_school = readRDS("CATE_school_v4.rds")
PS_mat=readRDS("PS_mat.rds")
colnames(CATE_school)[2:1002] = ""
CATE_school_truth = data.frame(school = CATE_school[,1],truth=rowMeans(CATE_school[,-1]))
CATE_school_truth_joined = left_join(data.frame(school=unique(PS_mat$school)), CATE_school_truth)

CATE_school_truth_joined_strata = left_join(CATE_school_truth_joined,
                                            unique(PS_mat[,c("minority_composition_index", "school_achievement_index", "school")]), 
                                            by = c("school"))
CATE_school_truth_joined_strata$strata = paste0(CATE_school_truth_joined_strata$minority_composition_index,
                                                "_",
                                                CATE_school_truth_joined_strata$school_achievement_index)



# latex table for MSE and proper scoring rule
df_output = data.frame(
  model = c("MRP-MI", "BARP", "BART", "OLS", "SVY", "MRP"),
  
  ate_l2_risk=c((mean(combined_ate_list) - .126)^2 + mean((combined_ate_list - mean(combined_ate_list))^2),
                (mean(combined_bart_ate_list) - .126)^2 + mean((combined_bart_ate_list - mean(combined_bart_ate_list))^2),
                (mean(combined_bart_usingposteriordistribution_ate_list) - .126)^2 + mean((combined_bart_usingposteriordistribution_ate_list - mean(combined_bart_usingposteriordistribution_ate_list))^2),
                (mean(combined_ate_unweighted_svyregression_list) - .126)^2 +mean((combined_ate_unweighted_svyregression_list - mean(combined_ate_unweighted_svyregression_list))^2),
                (mean(combined_ate_weighted_svyregression_list) - .126)^2 + mean((combined_ate_weighted_svyregression_list - mean(combined_ate_weighted_svyregression_list))^2),
                (mean(combined_ate_fullPS_list) - .126)^2 + mean((combined_ate_fullPS_list - mean(combined_ate_fullPS_list))^2)
                ),
  
  ate_scoring_rule = c(mean(-((combined_ate_list - .126)/(combined_ate_sd_list))^2 - log(combined_ate_sd_list^2)),
                       mean(-((combined_bart_ate_list - .126)/(combined_bart_ate_sd_list))^2 - log(combined_bart_ate_sd_list^2)),
                       mean(-((combined_bart_usingposteriordistribution_ate_list - .126)/(combined_bart_usingposteriordistribution_ate_sd_list))^2 - log(combined_bart_usingposteriordistribution_ate_sd_list^2)),
                       mean(-((combined_ate_unweighted_svyregression_list - .126)/(combined_ate_unweighted_svyregression_sd_list))^2 - log(combined_ate_weighted_svyregression_sd_list^2)),
                       mean(-((combined_ate_weighted_svyregression_list - .126)/(combined_ate_weighted_svyregression_sd_list))^2 - log(combined_ate_weighted_svyregression_sd_list^2)),
                       mean(-((combined_ate_fullPS_list - .126)/(combined_ate_fullPS_sd_list))^2 - log(combined_ate_fullPS_sd_list^2))
  ),
  
  
  cate_sa3_mc1_l2_risk = c(
    (mean(combined_cate_school_achievement_3_min_comp_1_list) - .065)^2 + mean((combined_cate_school_achievement_3_min_comp_1_list - mean(combined_cate_school_achievement_3_min_comp_1_list))^2),
    (mean(combined_bart_cate_school_achievement_3_min_comp_1_list) - .065)^2 + mean((combined_bart_cate_school_achievement_3_min_comp_1_list - mean(combined_bart_cate_school_achievement_3_min_comp_1_list))^2),
    (mean(combined_bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_list) - .065)^2 + mean((combined_bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_list - mean(combined_bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_list))^2),
    (mean(combined_ate_unweighted_regression_CATE_school_achievement_3_min_comp_1_list) - .065)^2 + mean((combined_ate_unweighted_regression_CATE_school_achievement_3_min_comp_1_list - mean(combined_ate_unweighted_regression_CATE_school_achievement_3_min_comp_1_list))^2),
    (mean(combined_ate_weighted_svyregression_school_achievement_index_3_min_comp_1_list) - .065)^2 + mean((combined_ate_weighted_svyregression_school_achievement_index_3_min_comp_1_list - mean(combined_ate_weighted_svyregression_school_achievement_index_3_min_comp_1_list))^2),
    (mean(combined_cate_school_achievement_3_min_comp_1_fullPS_list) - .065)^2 + mean((combined_cate_school_achievement_3_min_comp_1_fullPS_list - mean(combined_cate_school_achievement_3_min_comp_1_fullPS_list))^2)
  ),
  
  cate_sa3_mc1_scoring_rule = c(
    mean(-((combined_cate_school_achievement_3_min_comp_1_list - .065)/(combined_cate_school_achievement_3_min_comp_1_sd_list))^2 - log(combined_cate_school_achievement_3_min_comp_1_sd_list^2)),
    mean(-((combined_bart_cate_school_achievement_3_min_comp_1_list - .065)/(combined_bart_cate_school_achievement_3_min_comp_1_sd_list))^2 - log(combined_bart_cate_school_achievement_3_min_comp_1_sd_list^2)),
    mean(-((combined_bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_list - .065)/(combined_bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_sd_list))^2 - log(combined_bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_sd_list^2)),
    mean(-((combined_ate_unweighted_regression_CATE_school_achievement_3_min_comp_1_list[which(combined_ate_unweighted_regression_CATE_school_achievement_3_min_comp_1_sd_list<Inf)] - .065)/(combined_ate_unweighted_regression_CATE_school_achievement_3_min_comp_1_sd_list[which(combined_ate_unweighted_regression_CATE_school_achievement_3_min_comp_1_sd_list<Inf)]))^2 - 
           log(combined_ate_unweighted_regression_CATE_school_achievement_3_min_comp_1_sd_list[which(combined_ate_unweighted_regression_CATE_school_achievement_3_min_comp_1_sd_list<Inf)]^2)),
    mean(-((combined_ate_weighted_svyregression_school_achievement_index_3_min_comp_1_list[which(combined_ate_weighted_svyregression_school_achievement_index_3_min_comp_1_sd_list<Inf)] - .065)/(combined_ate_weighted_svyregression_school_achievement_index_3_min_comp_1_sd_list[which(combined_ate_weighted_svyregression_school_achievement_index_3_min_comp_1_sd_list<Inf)]))^2 - 
           log(combined_ate_weighted_svyregression_school_achievement_index_3_min_comp_1_sd_list[which(combined_ate_weighted_svyregression_school_achievement_index_3_min_comp_1_sd_list<Inf)]^2)),
    mean(-((combined_cate_school_achievement_3_min_comp_1_fullPS_list - .065)/(combined_cate_school_achievement_3_min_comp_1_fullPS_sd_list))^2 - log(combined_cate_school_achievement_3_min_comp_1_fullPS_sd_list^2))
    
  ),
  
  
  cate_sa1_re2_l2_risk = c(
    (mean(combined_cate_school_achievement_1_race_eth_2_list) - .195)^2 + mean((combined_cate_school_achievement_1_race_eth_2_list - mean(combined_cate_school_achievement_1_race_eth_2_list))^2),
    (mean(combined_bart_cate_school_achievement_1_race_eth_2_list) - .195)^2 + mean((combined_bart_cate_school_achievement_1_race_eth_2_list - mean(combined_bart_cate_school_achievement_1_race_eth_2_list))^2),
    (mean(combined_bart_usingposteriordistribution_cate_school_achievement_1_race_eth_2_list) - .195)^2 + mean((combined_bart_usingposteriordistribution_cate_school_achievement_1_race_eth_2_list - mean(combined_bart_usingposteriordistribution_cate_school_achievement_1_race_eth_2_list))^2),
    (mean(combined_ate_unweighted_regression_CATE_school_achievement_1_race_eth_2_list) - .195)^2 + mean((combined_ate_unweighted_regression_CATE_school_achievement_1_race_eth_2_list - mean(combined_ate_unweighted_regression_CATE_school_achievement_1_race_eth_2_list))^2),
    (mean(combined_ate_weighted_svyregression_school_achievement_index_1_race_eth_2_list) - .195)^2 + mean((combined_ate_weighted_svyregression_school_achievement_index_1_race_eth_2_list - mean(combined_ate_weighted_svyregression_school_achievement_index_1_race_eth_2_list))^2),
    (mean(combined_cate_school_achievement_1_race_eth_2_fullPS_list) - .195)^2 + mean((combined_cate_school_achievement_1_race_eth_2_fullPS_list - mean(combined_cate_school_achievement_1_race_eth_2_fullPS_list))^2)
    
  ),
  
  cate_sa1_re2_scoring_rule = c(
    mean(-((combined_cate_school_achievement_1_race_eth_2_list - .195)/(combined_cate_school_achievement_1_race_eth_2_sd_list))^2 - log(combined_cate_school_achievement_1_race_eth_2_sd_list^2)),
    mean(-((combined_bart_cate_school_achievement_1_race_eth_2_list - .195)/(combined_bart_cate_school_achievement_1_race_eth_2_sd_list))^2 - log(combined_bart_cate_school_achievement_1_race_eth_2_sd_list^2)),
    mean(-((combined_bart_usingposteriordistribution_cate_school_achievement_1_race_eth_2_list - .195)/(combined_bart_usingposteriordistribution_cate_school_achievement_1_race_eth_2_sd_list))^2 - log(combined_bart_usingposteriordistribution_cate_school_achievement_1_race_eth_2_sd_list^2)),
    mean(-((combined_ate_unweighted_regression_CATE_school_achievement_1_race_eth_2_list[which(combined_ate_unweighted_regression_CATE_school_achievement_1_race_eth_2_sd_list<Inf)] - .195)/(combined_ate_unweighted_regression_CATE_school_achievement_1_race_eth_2_sd_list[which(combined_ate_unweighted_regression_CATE_school_achievement_1_race_eth_2_sd_list<Inf)]))^2 - 
           log(combined_ate_unweighted_regression_CATE_school_achievement_1_race_eth_2_sd_list[which(combined_ate_unweighted_regression_CATE_school_achievement_1_race_eth_2_sd_list<Inf)]^2)),
    mean(-((combined_ate_weighted_svyregression_school_achievement_index_1_race_eth_2_list[which(combined_ate_weighted_svyregression_school_achievement_index_1_race_eth_2_sd_list<Inf)] - .195)/(combined_ate_weighted_svyregression_school_achievement_index_1_race_eth_2_sd_list[which(combined_ate_weighted_svyregression_school_achievement_index_1_race_eth_2_sd_list<Inf)]))^2 - 
           log(combined_ate_weighted_svyregression_school_achievement_index_1_race_eth_2_sd_list[which(combined_ate_weighted_svyregression_school_achievement_index_1_race_eth_2_sd_list<Inf)]^2)),
    mean(-((combined_cate_school_achievement_1_race_eth_2_fullPS_list - .195)/(combined_cate_school_achievement_1_race_eth_2_fullPS_sd_list))^2 - log(combined_cate_school_achievement_1_race_eth_2_fullPS_sd_list^2))
  )
  
)


xtable::xtable(df_output,digits=6)
xtable::xtable(format(df_output,scientific=TRUE),digits=6)


# FOR THE OVERLEAF PAPER ----------------------------------------

PS_mat = readRDS("PS_mat.rds")

ate_df_paper = rbind(data.frame(estimate=combined_ate_unweighted_svyregression_list, method="OLS"),
                     data.frame(estimate=combined_ate_weighted_svyregression_list, method="SVY"),
                     data.frame(estimate=combined_bart_usingposteriordistribution_ate_list, method="BART"),
                     data.frame(estimate=combined_bart_ate_list, method="BARP-I"),
                     data.frame(estimate=combined_ate_fullPS_list, method="MRP-I"),
                     data.frame(estimate=combined_ate_list, method="MRP-MI")
)

cate_sa3_mc1_df_paper = rbind(data.frame(estimate=combined_ate_unweighted_regression_CATE_school_achievement_3_min_comp_1_list, method="OLS"),
                              data.frame(estimate=combined_ate_weighted_svyregression_school_achievement_index_3_min_comp_1_list, method="SVY"),
                              data.frame(estimate=combined_bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_list, method="BART"),
                              data.frame(estimate=combined_bart_cate_school_achievement_3_min_comp_1_list, method="BARP-I"),
                              data.frame(estimate=combined_cate_school_achievement_3_min_comp_1_fullPS_list, method="MRP-I"),
                              data.frame(estimate=combined_cate_school_achievement_3_min_comp_1_list, method="MRP-MI")
)

cate_sa1_re2_df_paper = rbind(data.frame(estimate=combined_ate_unweighted_regression_CATE_school_achievement_1_race_eth_2_list, method="OLS"),
                              data.frame(estimate=combined_ate_weighted_svyregression_school_achievement_index_1_race_eth_2_list, method="SVY"),
                              data.frame(estimate=combined_bart_usingposteriordistribution_cate_school_achievement_1_race_eth_2_list, method="BART"),
                              data.frame(estimate=combined_bart_cate_school_achievement_1_race_eth_2_list, method="BARP-I"),
                              data.frame(estimate=combined_cate_school_achievement_1_race_eth_2_fullPS_list, method="MRP-I"),
                              data.frame(estimate=combined_cate_school_achievement_1_race_eth_2_list, method="MRP-MI")
)


ate_plot_paper = ggplot(ate_df_paper, aes(x=method, y=estimate)) + 
  geom_boxplot() +
  geom_jitter(width = 0.1)+ geom_hline(yintercept=0.126,col="red") + ylim(min(rbind(ate_df_paper, cate_sa3_mc1_df_paper, cate_sa1_re2_df_paper)$estimate)-.01,
                                                                          max(rbind(ate_df_paper, cate_sa3_mc1_df_paper, cate_sa1_re2_df_paper)$estimate)+.01) +
  xlab("Method") + ylab("Treatment effect point estimate") +
  theme_bw() +
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.text=element_text(size=13*1.6),
        axis.title=element_text(size=20*1.6, face="bold",margin=200),
        legend.text = element_text(size=20*1.6),
        legend.position  = "bottom",
        legend.key.size = unit(13*1.6,"line"),
        legend.key.height = unit(3*1.6,"line"),
        legend.key = element_rect(fill = "transparent",color = "transparent"),
        legend.title = element_text(size=20*1.6, face="bold"),
        strip.text.x = element_text(size=20*1.6, face="bold", margin = margin(t=25,b=25) ),
        strip.background = element_rect(fill="transparent",color="transparent"),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
  )


cate_sa3_mc1_plot_paper = ggplot(cate_sa3_mc1_df_paper, aes(x=method, y=estimate)) + 
  geom_boxplot() +
  geom_jitter(width = 0.1)+ geom_hline(yintercept=0.065,col="red") + ylim(min(rbind(ate_df_paper, cate_sa3_mc1_df_paper, cate_sa1_re2_df_paper)$estimate)-.01,
                                                                            max(rbind(ate_df_paper, cate_sa3_mc1_df_paper, cate_sa1_re2_df_paper)$estimate)+.01) +
  xlab("Method") + ylab("") +
  theme_bw() +
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.text=element_text(size=13*1.6),
        axis.title=element_text(size=20*1.6, face="bold",margin=200),
        legend.text = element_text(size=20*1.6),
        legend.position  = "bottom",
        legend.key.size = unit(13*1.6,"line"),
        legend.key.height = unit(3*1.6,"line"),
        legend.key = element_rect(fill = "transparent",color = "transparent"),
        legend.title = element_text(size=20*1.6, face="bold"),
        strip.text.x = element_text(size=20*1.6, face="bold", margin = margin(t=25,b=25) ),
        strip.background = element_rect(fill="transparent",color="transparent"),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
  )


cate_sa1_re2_plot_paper = ggplot(cate_sa1_re2_df_paper, aes(x=method, y=estimate)) + 
  geom_boxplot() +
  geom_jitter(width = 0.1)+ geom_hline(yintercept=0.195,col="red") +  ylim(min(rbind(ate_df_paper, cate_sa3_mc1_df_paper, cate_sa1_re2_df_paper)$estimate)-.01,
                                                                           max(rbind(ate_df_paper, cate_sa3_mc1_df_paper, cate_sa1_re2_df_paper)$estimate)+.01) +
  xlab("Method") + ylab("") +
  theme_bw() +
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.text=element_text(size=13*1.6),
        axis.title=element_text(size=20*1.6, face="bold",margin=200),
        legend.text = element_text(size=20*1.6),
        legend.position  = "bottom",
        legend.key.size = unit(13*1.6,"line"),
        legend.key.height = unit(3*1.6,"line"),
        legend.key = element_rect(fill = "transparent",color = "transparent"),
        legend.title = element_text(size=20*1.6, face="bold"),
        strip.text.x = element_text(size=20*1.6, face="bold", margin = margin(t=25,b=25) ),
        strip.background = element_rect(fill="transparent",color="transparent"),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
  )


png(filename = "plot_with_ate_100runs.png",
    width = 1800, height = 600)

cowplot::plot_grid(ate_plot_paper,
                   cate_sa3_mc1_plot_paper,
                   cate_sa1_re2_plot_paper,
                   ncol=3,
                   labels=c("ATE (100%; 11950)", "CATE: SA3, MC1 (19.9%; 1730)", "CATE: SA1, RE2 (3.2%; 330)"),
                   label_size=26)

dev.off()







cate_sa3_mc1_race_eth_plots_paper = list()
for (i in 1:length(race_eth_levels)) {
  print(i)
  
  if (i == 1) {
    y_lab_name = "Treatment effect point esimate"
  } else{ y_lab_name = ""}
  
  cate_sa3_mc1_race_eth_current_df_paper = rbind(data.frame(estimate = combined_ate_weighted_svyregression_temp_school_achievement_index_3_min_comp_1_templist_matrix[i,], method = "OLS"),
                                                 data.frame(estimate = combined_ate_unweighted_regression_temp_CATE_school_achievement_3_min_comp_1_templist_matrix[i,], method = "SVY"),
                                                 data.frame(estimate = combined_bart_usingposteriordistribution_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix[i,], method="BART"),
                                                 data.frame(estimate = combined_bart_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix[i,], method = "BARP-I"),
                                                 data.frame(estimate = combined_cate_school_achievement_3_min_comp_1_race_eth_templist_fullPS_matrix[i,], method = "MRP-I"),
                                                 data.frame(estimate = combined_cate_school_achievement_3_min_comp_1_race_eth_templist_matrix[i,], method = "MRP-MI")
                                                 )
  
  cate_sa3_mc1_race_eth_current_df_paper$estimate_minus_truth = cate_sa3_mc1_race_eth_current_df_paper$estimate - SA3_MC1_race_eth_cate_truth[i]
  
  cate_sa3_mc1_race_eth_plots_paper[[i]] = ggplot(cate_sa3_mc1_race_eth_current_df_paper, aes(x=method,
                                                                                              y=estimate)) + 
    geom_boxplot() +
    geom_jitter(width = 0.1) + geom_hline(yintercept=SA3_MC1_race_eth_cate_truth[i], # SA3_MC1_race_eth_cate_truth[i]
                                          col="red") + ylim(-.25,.4) + 
    xlab("Method") + ylab(y_lab_name) +
    theme_bw() +
    theme(plot.title = element_text(size = 20, face = "bold"),
          axis.text=element_text(size=13*1.6),
          axis.title=element_text(size=20*1.6, face="bold",margin=200),
          legend.text = element_text(size=20*1.6),
          legend.position  = "bottom",
          legend.key.size = unit(13*1.6,"line"),
          legend.key.height = unit(3*1.6,"line"),
          legend.key = element_rect(fill = "transparent",color = "transparent"),
          legend.title = element_text(size=20*1.6, face="bold"),
          strip.text.x = element_text(size=20*1.6, face="bold", margin = margin(t=25,b=25) ),
          strip.background = element_rect(fill="transparent",color="transparent"),
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
    )
  
  
}


# get sizes in-sample groups in the manuscript. Treatment effect estimates are calculated for them
pop_draw %>% filter(minority_composition_index==1, school_achievement_index==3) %>% 
  summarise(n()/dim(pop_draw)[1])

pop_draw %>% filter(school_achievement_index==1, race_eth==2) %>% 
  summarise(n()/dim(pop_draw)[1])

pop_draw %>% filter(school_achievement_index==3, minority_composition_index==1, race_eth==1) %>% 
  summarise(n()/dim(pop_draw)[1])

pop_draw %>% filter(school_achievement_index==3, minority_composition_index==1, race_eth==2) %>% 
  summarise(n()/dim(pop_draw)[1])

pop_draw %>% filter(school_achievement_index==3, minority_composition_index==1, race_eth==3) %>% 
  summarise(n()/dim(pop_draw)[1])

pop_draw %>% filter(school_achievement_index==3, minority_composition_index==1, race_eth==4) %>% 
  summarise(n()/dim(pop_draw)[1])

pop_draw %>% filter(school_achievement_index==3, minority_composition_index==1, race_eth==5) %>% 
  summarise(n()/dim(pop_draw)[1])



png(filename = "plot_with_fivecates_100runs.png",
    width = 2500, height = 840)


cowplot::plot_grid(cate_sa3_mc1_race_eth_plots_paper[[1]],
                   cate_sa3_mc1_race_eth_plots_paper[[2]],
                   cate_sa3_mc1_race_eth_plots_paper[[3]],
                   cate_sa3_mc1_race_eth_plots_paper[[4]],
                   cate_sa3_mc1_race_eth_plots_paper[[5]],
                   ncol=5,
                   labels=c("Asian (1.3%; 110)", "Black (1.7%; 150)", "Hispanic (3.9%; 340)", "White (10.3%; 890)", "Other (2.8%; 240)"),
                   label_size = 26)

dev.off()




# Plots of school CATEs --------------------------------------------------


aa = inner_join(CATE_school_truth_joined[,c(1,2)], modelfits[[1]][[21]])

aa_hist_dat = data.frame(g = c(rep("estimate", dim(aa)[1]), rep("truth", dim(aa)[1]) ),
                         v = c(aa$CATE, aa$truth))

aa_hist_dat_insample = data.frame(g = c(rep("estimate", dim(aa %>% filter(in_sample==TRUE))[1]), rep("truth", dim(aa %>% filter(in_sample==TRUE))[1]) ),
                                  v = c(aa %>% filter(in_sample==TRUE) %>%  select(CATE) %>% unlist,
                                        aa %>% filter(in_sample==TRUE) %>% select(truth) %>% unlist))

A = ggplot(data = CATE_school_truth_joined_strata, aes(x=cate_school_list/(counter - 1), y = truth, colour = factor(strata))) +
  geom_point(alpha=0.75) +
  xlab("MRP-MI CATE estimates for all schools") + ylab("True CATEs for all schools") + 
  scale_colour_manual(name = "",
                        values=c("darkgreen","purple","brown","blue","red"), c("Low School Ach.", "Low x Medium", "Low x High", "High x Medium", "High x High")) + 
  geom_abline(intercept = 0, slope = 1) +
  xlim(.05, .2) + ylim(-.05, .3) + 
  theme_bw() +
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.text=element_text(size=20*1.6),
        axis.title=element_text(size=20*1.6, face="bold",margin=200),
        legend.text = element_text(size=20*1.6),
        legend.position  = "bottom",
        legend.key.size = unit(1,"line"),
        legend.key.height = unit(3*1.6,"line"),
        legend.key = element_rect(fill = "transparent",color = "transparent"),
        legend.title = element_text(size=20*1.6, face="bold"),
        strip.text.x = element_text(size=20*1.6, face="bold", margin = margin(t=25,b=25) ),
        strip.background = element_rect(fill="transparent",color="transparent")
  ) + guides(colour = guide_legend(override.aes = list(size=5)))


B = ggplot(aa_hist_dat, aes(x = v, group = g)) + 
  geom_histogram(data=subset(aa_hist_dat,g=="truth"),aes(fill=g),alpha=0.75) + 
  geom_histogram(data=subset(aa_hist_dat,g=="estimate"),aes(fill=g),alpha=0.75) + 
  xlab("MRP-MI CATE estimates for all schools") + ylab("Count") + xlim(c(-0.05,0.3)) +
  scale_fill_manual(name="", values=c("red","darkgray"), c("MRP-MI","Truth")) +
  theme_bw() +
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.text=element_text(size=20*1.6),
        axis.title=element_text(size=20*1.6, face="bold",margin=200),
        legend.text = element_text(size=20*1.6),
        legend.position  = "bottom",
        legend.key.size = unit(2.5,"line"),
        legend.key.height = unit(1,"line"),
        legend.key = element_rect(fill = "transparent",color = "transparent"),
        legend.title = element_text(size=20*1.6, face="bold"),
        strip.text.x = element_text(size=5, face="bold", margin = margin(t=25,b=25) ),
        strip.background = element_rect(fill="transparent",color="transparent")
  )


C = ggplot(aa_hist_dat_insample, aes(x = v, group = g)) + 
  geom_histogram(data=subset(aa_hist_dat_insample,g=="truth"),aes(fill=g),alpha=0.75) + 
  geom_histogram(data=subset(aa_hist_dat_insample,g=="estimate"),aes(fill=g),alpha=0.75) + 
  xlab("MRP-MI CATE estimates for in-sample schools") + ylab("Count") +  xlim(c(-0.05,0.3)) +
  scale_fill_manual(name="", values=c("red","darkgray"), c("MRP-MI","Truth")) +
  theme_bw() +
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.text=element_text(size=20*1.6),
        axis.title=element_text(size=20*1.6, face="bold",margin=200),
        legend.text = element_text(size=20*1.6),
        legend.position  = "bottom",
        legend.key.size = unit(2.5,"line"),
        legend.key.height = unit(1,"line"),
        legend.key = element_rect(fill = "transparent",color = "transparent"),
        legend.title = element_text(size=20*1.6, face="bold"),
        strip.text.x = element_text(size=5, face="bold", margin = margin(t=25,b=25) ),
        strip.background = element_rect(fill="transparent",color="transparent")
  )

  

png(filename = "plot_outofsampleschoolCATEs_100runs.png",
    width = 2000, height = 2400)

  gridExtra::grid.arrange(A,B,C,ncol=1)

dev.off()

# PS mat xtable ------------------------------------------------
xtable::xtable(rbind(head(PS_mat[,1:7]), tail(PS_mat[,1:7])))


