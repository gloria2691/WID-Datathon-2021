# Title     : Women in data Science Datathon 2021
# Objective : Classification Problem, predict diabetes yes/no
# Team      : <team_name>
# Related rscripts: data_processing.R, target_prediction.R, functions.R
# Created on: 2/18/2021
###----------------------------------------

f_cols_by_cat <- function(codebook=""){
  if(codebook==""){
    codebook <- fread(file.path(data_dir, "DataDictionaryWiDS2021.csv"))
    colnames(codebook) <- gsub(" ", "_", tolower(colnames(codebook)))
  }
  ### Group variables by category
  #table(codebook$category)
  category_labels = gsub(" ", "", unique(tolower(codebook$category)))
  cols_cat = list()
  for (i in c(1:length(category_labels))) {
    category <- unique(codebook$category)[i]
    category_label = category_labels[i]
    cols_cat[[category_label]] <- codebook$variable_name[codebook$category == category]
  }
  return(cols_cat)
}


f_get_cols_strint <- function (dat){
  ### Group variables by type
cols_binary=c()
cols_numeric=c()
cols_character=c()
cols_intstr = list()
for (col in colnames(dat)) {
  if(is.numeric(dat[[col]]) & length(unique(dat[[col]])) ==2 )  cols_binary <- c(cols_binary, col)
  if(is.numeric(dat[[col]])) cols_numeric <- c(cols_numeric, col)
  if(is.character(dat[[col]])) cols_character <- c(cols_character, col)

}
cols_intstr[['binary']] <- cols_binary
cols_intstr[['numeric']] <- cols_numeric
cols_intstr[['character']] <- cols_character
return(cols_intstr)
}

p_hist_by_target <- function(dat=train_df,
                             selected_cols=cols_intstr$numeric[1:10],
                             target_var = 'diabetes_mellitus',
                             plotname="phist",
                             SAVE=FALSE){
 # title = names(cols)[i]
    phist <- dat %>%
      select_at(c(selected_cols, target_var)) %>%
      pivot_longer(cols = -target_var) %>%
      rename('target_var' = target_var) %>%
      ggplot(aes(x = value, fill = as.factor(target_var), group=target_var)) +
      geom_histogram() +
      facet_wrap(~name, scales = "free")+
      labs(x="Value", y="Count", fill=target_var) +
      theme(legend.position = "top")+
      scale_fill_manual(values=c("deepskyblue3","orange"))
    if(SAVE){
      if(!dir.exists(file.path("fig")))dir.create(file.path("fig"))
      ggsave(paste0(plotname,'.png'),phist, path=file.path("fig"), device = "png")
    }
return(phist)
}

p_bar_by_target <- function(dat=train_df,
                             selected_cols=cols_intstr$character,
                             target_var = 'diabetes_mellitus',
                             plotname="pbar",
                             SAVE=FALSE){
    l = length(selected_cols)
    selected_cols <- train_df  %>% select_at(selected_cols) %>% select_if(is.character) %>% colnames()
    print(paste0("Removed ",l - length(selected_cols)," non character variables"))
    pbar <- train_df %>%
      select_at(c(selected_cols, target_var)) %>%
      pivot_longer(cols = -target_var) %>%
      rename('target_var' = target_var) %>%
      ggplot() +
      geom_bar(aes(x = value, group = target_var, fill = as.factor(target_var))) +
      facet_wrap(~name, scales = "free") +
      labs(x="Value", y="Count", fill=target_var) +
      coord_flip() +
      theme(legend.position = "top")+
      scale_fill_manual(values=c("deepskyblue3","orange"))
    if(SAVE){
      if(!dir.exists(file.path("fig")))dir.create(file.path("fig"))
      ggsave(paste0(plotname,'.png'),pbar, path=file.path("fig"), device = "png")
    }
return(pbar)
}


f_variable_cleanup <- function(dat){
  dat$comorbidity_score = dat[,'aids'] * 23 + dat[,'cirrhosis'] * 4  + dat[,'hepatic_failure'] * 16 + dat[,'immunosuppression'] * 10 + dat[,'leukemia'] * 10 + dat[,'lymphoma'] * 13 + dat[,'solid_tumor_with_metastasis'] * 11
  dat$comorbidity_score[is.na(dat$comorbidity_score )] = 0
  dat$gcs_sum = dat[,'gcs_eyes_apache']+dat[,'gcs_motor_apache']+dat[,'gcs_verbal_apache']
  dat$gcs_sum[is.na(dat$gcs_sum )] = 0
  dat$apache_2_diagnosis_type = round(dat$apache_2_diagnosis,1)
  dat$apache_2_diagnosis_type[is.na(dat$apache_2_diagnosis_type )] = -100
  dat$apache_3j_diagnosis_type = round(dat$apache_3j_diagnosis,1)
  dat$apache_3j_diagnosis_type[is.na(dat$apache_3j_diagnosis_type )] = -100


  dat$abmi = dat[,'age']/dat[,'bmi']
  dat$agi = dat[,'weight']/dat[,'age']


  drop_cols = c('abmi', 'age_type', 'aids', 'albumin_apache', 'albumin_h1_value_range', 'albumin_h1_zero_range','albumin_tot_change_value_range_normed', 'apache_3j_diagnosis-cat_age', 'apache_post_operative','apache_post_operative_std_d1_temp_max', 'arf_apache_std_d1_hemaglobin_max', 'arterial_pco2_d1_h1_max_eq','arterial_pco2_d1_h1_min_eq', 'arterial_pco2_d1_zero_range', 'arterial_pco2_h1_zero_range','arterial_ph_apache', 'arterial_ph_d1_h1_max_eq', 'arterial_ph_d1_value_range', 'arterial_ph_d1_zero_range','arterial_ph_h1_zero_range', 'arterial_po2_d1_h1_max_eq', 'arterial_po2_d1_h1_min_eq', 'arterial_po2_d1_value_range', 'bilirubin_h1_value_range', 'bilirubin_h1_zero_range','bilirubin_tot_change_value_range_normed', 'bmi_type', 'bun_d1_h1_max_eq', 'bun_d1_zero_range','bun_h1_value_range', 'bun_h1_zero_range', 'calcium_d1_zero_range', 'calcium_h1_value_range','calcium_h1_zero_range', 'creatinine_h1_zero_range', 'd1_albumin_min', 'd1_arterial_pco2_min','d1_arterial_ph_max', 'd1_arterial_ph_min', 'd1_calcium_max', 'd1_diasbp_max', 'd1_diasbp_min','d1_hematocrit_min', 'd1_inr_max', 'd1_inr_min', 'd1_mbp_invasive_max', 'd1_mbp_invasive_min', 'd1_mbp_max','d1_mbp_min', 'd1_mbp_noninvasive_max', 'd1_mbp_noninvasive_min', 'd1_pao2fio2ratio_max', 'd1_pao2fio2ratio_min', 'd1_platelets_max', 'd1_resprate_max', 'd1_sysbp_invasive_min', 'd1_temp_min','d1_wbc_min', 'diasbp_d1_h1_max_eq', 'diasbp_d1_zero_range', 'diasbp_invasive_d1_h1_max_eq','diasbp_invasive_d1_value_range', 'diasbp_invasive_d1_zero_range', 'diasbp_invasive_h1_value_range','diasbp_invasive_h1_zero_range', 'diasbp_noninvasive_d1_h1_max_eq', 'diasbp_noninvasive_d1_zero_range','diasbp_noninvasive_h1_zero_range', 'diff_bmi', 'elective_surgery_mean_d1_sysbp_min', 'gcs_unable_apache','h1_albumin_max', 'h1_albumin_min', 'h1_arterial_pco2_max', 'h1_arterial_pco2_min', 'h1_arterial_ph_min','h1_arterial_po2_max', 'h1_bilirubin_max', 'h1_bun_max', 'h1_creatinine_min', 'h1_diasbp_noninvasive_max','h1_heartrate_max', 'h1_heartrate_min', 'h1_hemaglobin_min', 'h1_hematocrit_max', 'h1_hematocrit_min','h1_lactate_max', 'h1_lactate_min', 'h1_mbp_invasive_max', 'h1_mbp_invasive_min', 'h1_mbp_max', 'h1_mbp_min','h1_mbp_noninvasive_max', 'h1_mbp_noninvasive_min', 'h1_pao2fio2ratio_max', 'h1_pao2fio2ratio_min','h1_platelets_max', 'h1_platelets_min', 'h1_resprate_max', 'h1_resprate_min', 'h1_sodium_max','h1_spo2_max', 'h1_spo2_min', 'h1_sysbp_max', 'h1_sysbp_min', 'h1_sysbp_noninvasive_max','h1_sysbp_noninvasive_min', 'h1_temp_max', 'h1_temp_min', 'h1_wbc_max', 'h1_wbc_min', 'hco3_d1_h1_max_eq','hco3_d1_h1_min_eq', 'hco3_h1_value_range', 'hco3_h1_zero_range', 'heartrate_d1_zero_range','heartrate_h1_zero_range', 'height', 'hemaglobin_d1_value_range', 'hemaglobin_d1_zero_range','hematocrit_apache', 'hematocrit_d1_h1_min_eq', 'hematocrit_d1_value_range', 'hematocrit_d1_zero_range','inr_d1_h1_max_eq', 'inr_d1_h1_min_eq', 'inr_d1_value_range', 'inr_d1_zero_range', 'inr_day_more_extreme','inr_h1_value_range', 'inr_h1_zero_range', 'inr_started_after_firstHour', 'intubated_apache_mean_d1_spo2_max','lactate_h1_value_range', 'lactate_h1_zero_range', 'lymphoma', 'map_apache', 'mbp_d1_zero_range','mbp_h1_zero_range', 'mbp_invasive_d1_h1_min_eq', 'mbp_invasive_d1_value_range', 'mbp_invasive_d1_zero_range','mbp_invasive_h1_zero_range', 'mbp_noninvasive_d1_h1_max_eq', 'mbp_noninvasive_d1_h1_min_eq','mbp_noninvasive_d1_zero_range', 'mbp_noninvasive_h1_zero_range', 'mean_diff_d1_inr_min','mean_diff_h1_bilirubin_min', 'mean_diff_h1_inr_max', 'paco2_apache', 'paco2_for_ph_apache','pao2fio2ratio_apache', 'pao2fio2ratio_h1_value_range', 'pao2fio2ratio_h1_zero_range','rank_frqenc_leukemia', 'wbc_h1_value_range','platelets_d1_value_range', 'platelets_h1_zero_range', 'potassium_d1_h1_max_eq','potassium_h1_value_range','potassium_h1_zero_range', 'rank_frqenc_apache_2_diagnosis', 'resprate_apache', 'resprate_d1_h1_min_eq','resprate_d1_zero_range', 'sodium_d1_h1_min_eq', 'sodium_d1_zero_range','spo2_d1_h1_max_eq','sysbp_d1_zero_range', 'sysbp_h1_zero_range', 'sysbp_invasive_d1_h1_min_eq','sysbp_invasive_d1_zero_range','sysbp_noninvasive_d1_h1_min_eq', 'sysbp_noninvasive_d1_zero_range','sysbp_noninvasive_h1_zero_range','temp_d1_zero_range', 'ventilated_apache_std_d1_glucose_min', 'wbc_apache', 'wbc_d1_h1_min_eq','wbc_d1_value_range', 'wbc_d1_zero_range', 'wbc_h1_zero_range', 'gcs_eyes_apache_mean_d1_bun_min','rank_frqenc_aids')
  drop_cols <- colnames(dat)[ colnames(dat) %in% drop_cols ]
  dat = dat %>% dplyr::select(-c(drop_cols))

return (dat)
}

f_predict_and_save_submission_csv <- function(test_dat, final_model, fname="", SAVE_DIR=""){

  submit_df <- fread(file.path(data_dir, "SolutionTemplateWiDS2021.csv"))
  submit_df$diabetes_mellitus = predict(final_model, test_dat,type = "prob")$ranger$diabetes

  #table(submit_df$diabetes_mellitus)
  if(SAVE_DIR==""){
    SAVE_DIR <- file.path(getwd(),"submit_csv")
  }

  if(!dir.exists(SAVE_DIR))dir.create(SAVE_DIR)
  if(fname=="")fname=paste0(gsub("-","",Sys.Date()),"_mr_SubmissionWiDS2021.csv")
  fwrite(submit_df,file.path(SAVE_DIR,fname))
  print(paste0("Submission csv saved under ", file.path(SAVE_DIR,fname)))
  return(submit_df)
}
