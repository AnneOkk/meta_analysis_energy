barebones_main <- function(data) {
  ma_r <- ma_r(ma_method = "ic",
       rxyi = Rxy,
       sample_id = `APA citation`,
       n = `N_Sample Size`,
       rxx = Rxx_Predictor,
       ryy = Ryy_Criterion,
       construct_x = "Predictor_overall",
       construct_y = "NAME_Criterion y _Category1",
       collapse_method = "average",
       #intercor = intercor_cons,
       wt_type = 'sample_size',
       correct_rxx = TRUE,
       correct_ryy = TRUE,
       impute_artifacts = TRUE,
       data = data)

  out_ic <- summary(ma_r)
  get_escalc(ma_r)$`analysis_id: 1`$individual_correction$true_score
  CESE <- get_metatab(out_ic)$individual_correction$true_score %>%
    select(k, N, mean_r, sd_r, mean_rho, sd_rho,
           CI_LL_95, CI_UL_95, CR_LL_80, CR_UL_80)
  ma_het_CESE <- heterogeneity(ma_obj = ma_r)
  var_arti <- (heterogeneity(ma_het_CESE)$heterogeneity)$`analysis id: 1`$individual_correction$true_score$cor
  cbind(CESE, var_arti)
}

cat_mods <- function(data, moderator, moderators_name, cat_mod = T){
  ma_res_mod <- ma_r(ma_method = "ic",
                      rxyi = Rxy,
                      sample_id = `APA citation`,
                      n = `N_Sample Size`,
                      rxx = Rxx_Predictor,
                      ryy = Ryy_Criterion,
                      construct_x = "Predictor_overall",
                      construct_y = "NAME_Criterion y _Category1",
                      collapse_method = "average",
                      #intercor = intercor_cons,
                      wt_type = 'sample_size',
                      correct_rxx = TRUE,
                      correct_ryy = TRUE,
                      impute_artifacts = TRUE,
                      moderators = moderators_name,
                      cat_moderators = cat_mod,
                      data = data)

  #CRSE_anova_pub_stat <- anova(ma_res_mod, ma_method = "ic")
  get_metatab(ma_res_mod)$individual_correction$true_score %>%
    select(construct_x, construct_y, moderator, k, N, mean_r, sd_r, mean_rho, sd_rho,
           CI_LL_95, CI_UL_95, CR_LL_80, CR_UL_80)
}

cat_mods_sub <- function(data, moderator, moderators_name, cat_mod = T){
  ma_res_mod <- ma_r(ma_method = "ic",
                     rxyi = Rxy,
                     sample_id = `APA citation`,
                     n = `N_Sample Size`,
                     rxx = Rxx_Predictor,
                     ryy = Ryy_Criterion,
                     construct_x = "Predictor_overall",
                     construct_y = "NAME_Criterion y_specific concept category2",
                     collapse_method = "average",
                     #intercor = intercor_cons,
                     wt_type = 'sample_size',
                     correct_rxx = TRUE,
                     correct_ryy = TRUE,
                     impute_artifacts = TRUE,
                     moderators = moderators_name,
                     cat_moderators = cat_mod,
                     data = data)
  anova_results <- anova(ma_res_mod,  ma_method = "ic")
  #CRSE_anova_pub_stat <- anova(ma_res_mod, ma_method = "ic")
  metatab <- get_metatab(ma_res_mod)$individual_correction$true_score %>%
    select(construct_x, construct_y, moderator, k, N, mean_r, sd_r, mean_rho, sd_rho,
           CI_LL_95, CI_UL_95, CR_LL_80, CR_UL_80)
  return(list(anova_results, metatab))
}

cat_mods_overall <- function(data, moderator, moderators_name, cat_mod = T){
  ma_res_mod <- ma_r(ma_method = "ic",
                     rxyi = Rxy,
                     sample_id = `APA citation`,
                     n = `N_Sample Size`,
                     rxx = Rxx_Predictor,
                     ryy = Ryy_Criterion,
                     construct_x = "Predictor_overall",
                     construct_y = "NAME_Criterion y _Category1",
                     collapse_method = "average",
                     #intercor = intercor_cons,
                     wt_type = 'sample_size',
                     correct_rxx = TRUE,
                     correct_ryy = TRUE,
                     impute_artifacts = TRUE,
                     moderators = moderators_name,
                     cat_moderators = cat_mod,
                     data = data)
  anova_results <- anova(ma_res_mod,  ma_method = "ic")
  #CRSE_anova_pub_stat <- anova(ma_res_mod, ma_method = "ic")
  metatab <- get_metatab(ma_res_mod)$individual_correction$true_score %>%
    select(construct_x, construct_y, moderator, k, N, mean_r, sd_r, mean_rho, sd_rho,
           CI_LL_95, CI_UL_95, CR_LL_80, CR_UL_80)
  return(list(anova_results, metatab))
}


format_table <- function(data){
  data <- data %>% mutate(mean_r = format(round(mean_r, digits = 2), nsmall = 2)) %>%
    mutate(mean_r = sub("0/.", ".", mean_r)) %>%
    mutate(sd_r = format(round(sd_r, digits = 2), nsmall = 2)) %>%
    mutate(sd_r = sub("0/.", ".", sd_r)) %>%
    mutate(mean_rho = format(round(mean_rho, digits = 2), nsmall = 2)) %>%
    mutate(mean_rho = sub("0/.", ".", mean_rho)) %>%
    mutate(sd_rho = format(round(sd_rho, digits = 2), nsmall = 2)) %>%
    mutate(sd_rho = sub("0/.", ".", sd_rho)) %>%
    mutate(CI_LL_95 = format(round(CI_LL_95, digits = 2), nsmall = 2)) %>%
    mutate(CI_LL_95 = sub("0/.", ".", CI_LL_95)) %>%
    mutate(CI_UL_95 = format(round(CI_UL_95, digits = 2), nsmall = 2)) %>%
    mutate(CI_UL_95 = sub("0/.", ".", CI_UL_95)) %>%
    mutate(CR_LL_80 = format(round(CR_LL_80, digits = 2), nsmall = 2)) %>%
    mutate(CR_LL_80 = sub("0/.", ".", CR_LL_80)) %>%
    mutate(CR_UL_80 = format(round(CR_UL_80, digits = 2), nsmall = 2)) %>%
    mutate(CR_UL_80 = sub("0/.", ".", CR_UL_80)) %>%
    mutate(CI_LL_95 = paste0("[", CI_LL_95, ", ", CI_UL_95, "]")) %>%
    mutate(CR_LL_80 = paste0("[", CR_LL_80, ", ", CR_UL_80, "]")) %>%
    select(-CI_UL_95, -CR_UL_80) %>%
    mutate(N = round(N, 0))
}

format_table_anova <- function(data){
  data <- data %>% select(level_1, level_2, mean_1, mean_2, diff, CI_LL_95, CI_UL_95) %>%
    mutate(mean_1 = format(round(mean_1, digits = 2), nsmall = 2)) %>%
    mutate(mean_1 = sub("0/.", ".", mean_1)) %>%
    mutate(mean_2 = format(round(mean_2, digits = 2), nsmall = 2)) %>%
    mutate(mean_2 = sub("0/.", ".", mean_2)) %>%
    mutate(diff = format(round(diff, digits = 2), nsmall = 2)) %>%
    mutate(diff = sub("0/.", ".", diff)) %>%
    mutate(CI_LL_95 = format(round(CI_LL_95, digits = 2), nsmall = 2)) %>%
    mutate(CI_LL_95 = sub("0/.", ".", CI_LL_95)) %>%
    mutate(CI_UL_95 = format(round(CI_UL_95, digits = 2), nsmall = 2)) %>%
    mutate(CI_UL_95 = sub("0/.", ".", CI_UL_95)) %>%
    mutate(CI_LL_95 = paste0("[", CI_LL_95, ", ", CI_UL_95, "]")) %>%
    select(-CI_UL_95)
}



cum_plots <- function(data){
ma_res_CESE <- ma_r(ma_method = "ic",
                    rxyi = Rxy,
                    sample_id = Number,
                    n = `N_Sample Size`,
                    rxx = Rxx_Predictor,
                    ryy = Ryy_Criterion,
                    construct_x = "Predictor_overall",
                    construct_y = "NAME_Criterion y_specific concept category2",
                    collapse_method = "average",
                    #intercor = intercor_cons,
                    wt_type = 'sample_size',
                    correct_rxx = TRUE,
                    correct_ryy = TRUE,
                    impute_artifacts = TRUE,
                    #moderators = moderators_name,
                    #cat_moderators = cat_mod,
                    data = data)
ma_obj <- psychmeta::sensitivity(ma_res_CESE,
                      leave1out = TRUE,
                      bootstrap = TRUE,
                      cumulative = TRUE,
                      sort_method = "weight",
                      boot_iter = 1000,
                      boot_conf_level = 0.95,
                      boot_ci_type = "norm")

ma_obj$cumulative[[1]]$individual_correction$true_score$plots[1]

out_cumulative <- get_cumulative(ma_obj)
out_cumulative[[1]]$individual_correction$true_score
out_plots <- get_plots(ma_obj, plot_types = c("leave1out", "cumulative"))
cum_CESE <- out_plots$cumulative$`analysis id: 1`$individual_correction$true_score$plots[1]
cum_CESE}

cum_plots_overall <- function(data){
  ma_res_CESE <- ma_r(ma_method = "ic",
                      rxyi = Rxy,
                      sample_id = Number,
                      n = `N_Sample Size`,
                      rxx = Rxx_Predictor,
                      ryy = Ryy_Criterion,
                      construct_x = "Predictor_overall",
                      construct_y = "NAME_Criterion y _Category1",
                      collapse_method = "average",
                      #intercor = intercor_cons,
                      wt_type = 'sample_size',
                      correct_rxx = TRUE,
                      correct_ryy = TRUE,
                      impute_artifacts = TRUE,
                      #moderators = moderators_name,
                      #cat_moderators = cat_mod,
                      data = data)
  ma_obj <- psychmeta::sensitivity(ma_res_CESE,
                                   leave1out = TRUE,
                                   bootstrap = TRUE,
                                   cumulative = TRUE,
                                   sort_method = "weight",
                                   boot_iter = 1000,
                                   boot_conf_level = 0.95,
                                   boot_ci_type = "norm")

  ma_obj$cumulative[[1]]$individual_correction$true_score$plots[1]

  out_cumulative <- get_cumulative(ma_obj)
  out_cumulative[[1]]$individual_correction$true_score
  out_plots <- get_plots(ma_obj, plot_types = c("leave1out", "cumulative"))
  cum_CESE <- out_plots$cumulative$`analysis id: 1`$individual_correction$true_score$plots[1]
  cum_CESE}


cleantext <- function(x){
  
  sapply(1:length(x),function(y){
    bad = hunspell(x[y])[[1]]
    good = unlist(lapply(hunspell_suggest(bad),`[[`,1))
    
    if (length(bad)){
      for (i in 1:length(bad)){
        x[y] <<- gsub(bad[i],good[i],x[y])
      }}})
  x
}


