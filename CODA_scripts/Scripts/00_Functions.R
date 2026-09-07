pacman::p_load(tidyverse,jtools,ggrepel,rempsyc)
options(scipen = 999) # digits = 3

reg_sim = function(outcome, pred, data) {
  # Concatenate the predictor and covariates into a single formula string
  formula_string = paste(outcome, '~', pred)
  model <- lm(formula = as.formula(formula_string), data = data)
  model_sum = summ(model, scale = F, model.info = T, confint = T,
                   model.fit = T, digits = 3,transform.response = F)
  model_sum_std = summ(model, scale = T, model.info = T, confint = T,
                       model.fit = T, digits = 3,transform.response = T)
  model_r2 = summary(model)$adj.r.squared
  model_ests = model_sum$coeftable %>% as.data.frame() %>%  
    rownames_to_column(var = 'predictor') %>% filter(predictor != '(Intercept)')
  model_ests_std = model_sum_std$coeftable %>% as.data.frame() %>%  
    rownames_to_column(var = 'predictor') %>% filter(predictor != '(Intercept)') %>%
    dplyr::select(1:4)
  colnames(model_ests) = c('predictor',"B","low_ci","high_ci", "t_val", "p_val")
  colnames(model_ests_std) = c('predictor',"B_std","low_ci_std","high_ci_std")
  model_ests_all = model_ests %>% left_join(model_ests_std,by = 'predictor') %>% 
    mutate(r2_model = model_r2, outcome = outcome,model = pred)
  model_ests_all = model_ests_all %>% dplyr::select(c(12,1:2,7,3,8,4,9,5:6,10,11))
  model_ests_all = model_ests_all %>% mutate_if(is.numeric, round,digits = 3)
  model_emm = as.data.frame(emmeans(model, pred)) %>% 
    pivot_wider(names_from = 1,values_from = 2:ncol(.))
  model_emm =model_emm %>% mutate_if(is.numeric, round,digits = 3)
  results = list(model,model_ests_all,model_emm)
  names(results) = c("model","model_ests_all",'model_emm')
  return(results)
  return(results)
  
}

reg_mul = function(outcome, pred, covariates, data) {
  # Concatenate the predictor and covariates into a single formula string
  formula_string = paste(outcome, '~', pred, '+', paste(covariates, collapse = ' + '))
  model <- lm(formula = as.formula(formula_string), data = data)
  model_sum = summ(model, scale =F, model.info = T, confint = T,
                   model.fit = T, digits = 3,transform.response = F)
  model_sum_std = summ(model, scale =T, model.info = T, confint = T,
                   model.fit = T, digits = 3,transform.response = T)
  model_r2 = summary(model)$adj.r.squared
  model_ests = model_sum$coeftable %>% as.data.frame() %>%  
    rownames_to_column(var = 'predictor') %>% filter(predictor != '(Intercept)')
  model_ests_std = model_sum_std$coeftable %>% as.data.frame() %>%  
    rownames_to_column(var = 'predictor') %>% filter(predictor != '(Intercept)') %>%
    dplyr::select(1:4)
  colnames(model_ests) = c('predictor',"B","low_ci","high_ci", "t_val", "p_val")
  colnames(model_ests_std) = c('predictor',"B_std","low_ci_std","high_ci_std")
  model_ests_all = model_ests %>% left_join(model_ests_std,by = 'predictor') %>% 
    mutate(r2_model = model_r2, outcome = outcome,model = pred)
  model_ests_all = model_ests_all %>% dplyr::select(c(12,1:2,7,3,8,4,9,5:6,10,11))
  model_ests_all = model_ests_all %>% mutate_if(is.numeric, round,digits = 3)
  model_emm = as.data.frame(emmeans(model, pred)) %>% 
    pivot_wider(names_from = 1,values_from = 2:ncol(.))
  model_emm =model_emm %>% mutate_if(is.numeric, round,digits = 3)
  results = list(model,model_ests_all,model_emm)
  names(results) = c("model","model_ests_all",'model_emm')
  return(results)
}

reg_int = function(outcome, pred, int, covariates, data) {
  # Concatenate the predictor and covariates into a single formula string
  formula_string = paste(outcome, '~', pred,'*', int, '+', paste(covariates, collapse = ' + '))
  model <- lm(formula = as.formula(formula_string), data = data)
  model_std <- lm(formula = as.formula(formula_string), data = jtools::standardize(data))
  model_sum = summ(model, scale = F, model.info = T, confint = T,
                   model.fit = T, digits = 3,transform.response = F)
  model_sum_std = summ(model, scale =T, model.info = T, confint = T,
                       model.fit = T, digits = 3,transform.response = T)
  model_r2 = summary(model)$adj.r.squared
  model_ests = model_sum$coeftable %>% as.data.frame() %>%  
    rownames_to_column(var = 'predictor') %>% filter(predictor != '(Intercept)')
  model_ests_std = model_sum_std$coeftable %>% as.data.frame() %>%  
    rownames_to_column(var = 'predictor') %>% filter(predictor != '(Intercept)') %>%
    dplyr::select(1:4)
  colnames(model_ests) = c('predictor',"B","low_ci","high_ci", "t_val", "p_val")
  colnames(model_ests_std) = c('predictor',"B_std","low_ci_std","high_ci_std")
  model_ests_all = model_ests %>% left_join(model_ests_std,by = 'predictor') %>% 
    mutate(r2_model = model_r2, outcome = outcome,model = pred)
  model_ests_all = model_ests_all %>% dplyr::select(c(12,1:2,7,3,8,4,9,5:6,10,11))
  #formula_string2 = paste(outcome, '~', pred, '+', paste(covariates, collapse = ' + '))
  #list_models <- data %>% dplyr::filter(is.na(!!sym(int)) == FALSE) %>% group_split(!!sym(int),.keep = T) %>% 
  #  map(~lm(formula = as.formula(formula_string2),data=.x)) 
  list_models <- data %>% dplyr::filter(is.na(!!sym(int)) == FALSE) %>% group_split(!!sym(int),.keep = T) %>% 
      map(~reg_mul(outcome, pred, covariates, .x))
  names(list_models) = data %>% dplyr::select(int) %>% pull %>% levels
  model_ests_all = model_ests_all %>% mutate_if(is.numeric, round,digits = 3)
  results = list(model,model_std,model_ests_all,list_models)
  names(results) = c("model","model_std", "model_ests_all","list_models")
  return(results)
}


pacman::p_load(tidyverse,jtools,ggrepel)
options(scipen = 999) # digits = 3

reg_sim = function(outcome, pred, data) {
  # Concatenate the predictor and covariates into a single formula string
  formula_string = paste(outcome, '~', pred)
  model <- lm(formula = as.formula(formula_string), data = data)
  model_sum = summ(model, scale = F, model.info = T, confint = T,
                   model.fit = T, digits = 3,transform.response = F)
  model_sum_std = summ(model, scale = T, model.info = T, confint = T,
                       model.fit = T, digits = 3,transform.response = T)
  model_r2 = summary(model)$adj.r.squared
  model_ests = model_sum$coeftable %>% as.data.frame() %>%  
    rownames_to_column(var = 'predictor') %>% filter(predictor != '(Intercept)')
  model_ests_std = model_sum_std$coeftable %>% as.data.frame() %>%  
    rownames_to_column(var = 'predictor') %>% filter(predictor != '(Intercept)') %>%
    dplyr::select(1:4)
  colnames(model_ests) = c('predictor',"B","low_ci","high_ci", "t_val", "p_val")
  colnames(model_ests_std) = c('predictor',"B_std","low_ci_std","high_ci_std")
  model_ests_all = model_ests %>% left_join(model_ests_std,by = 'predictor') %>% 
    mutate(r2_model = model_r2, outcome = outcome,model = pred)
  model_ests_all = model_ests_all %>% dplyr::select(c(12,1:2,7,3,8,4,9,5:6,10,11))
  model_ests_all = model_ests_all %>% mutate_if(is.numeric, round,digits = 3)
  results = list(model,model_ests_all)
  names(results) = c("model","model_ests_all")
  return(results)
  
}

reg_mul = function(outcome, pred, covariates, data) {
  # Concatenate the predictor and covariates into a single formula string
  formula_string = paste(outcome, '~', pred, '+', paste(covariates, collapse = ' + '))
  model <- lm(formula = as.formula(formula_string), data = data)
  model_sum = summ(model, scale =F, model.info = T, confint = T,
                   model.fit = T, digits = 3,transform.response = F)
  model_sum_std = summ(model, scale =T, model.info = T, confint = T,robust = FALSE,
                       model.fit = T, digits = 3,transform.response = T)
  model_r2 = summary(model)$adj.r.squared
  model_ests = model_sum$coeftable %>% as.data.frame() %>%  
    rownames_to_column(var = 'predictor') %>% filter(predictor != '(Intercept)')
  model_ests_std = model_sum_std$coeftable %>% as.data.frame() %>%  
    rownames_to_column(var = 'predictor') %>% filter(predictor != '(Intercept)') %>%
    dplyr::select(1:4)
  colnames(model_ests) = c('predictor',"B","low_ci","high_ci", "t_val", "p_val")
  colnames(model_ests_std) = c('predictor',"B_std","low_ci_std","high_ci_std")
  model_ests_all = model_ests %>% left_join(model_ests_std,by = 'predictor') %>% 
    mutate(r2_model = model_r2, outcome = outcome,model = pred)
  model_ests_all = model_ests_all %>% dplyr::select(c(12,1:2,7,3,8,4,9,5:6,10,11))
  model_ests_all = model_ests_all %>% mutate_if(is.numeric, round,digits = 3)
  #model_emm = as.data.frame(emmeans(model, pred)) %>% 
  #  pivot_wider(names_from = 1,values_from = 2:ncol(.))
  #model_emm =model_emm %>% mutate_if(is.numeric, round,digits = 3)
  results = list(model,model_ests_all#,model_emm
  )
  names(results) = c("model","model_ests_all"#,'model_emm'
  )
  return(results)
}



mod_med_func = function(outcome, pred, med, covariates, data) {
  pro_med = bruceR::PROCESS(data, y=outcome, x=pred, 
                            meds = med, covs = covariates, digits = 3, std = TRUE,
                            ci="boot", nsim=bootstraping, seed=123)
}


reg_int_c = function(outcome, pred, int, covariates, data) {
  # Concatenate the predictor and covariates into a single formula string
  formula_string = paste(outcome, '~', pred,'*', int, '+', paste(covariates, collapse = ' + '))
  model <- lm(formula = as.formula(formula_string), data = data)
  model_sum = summ(model, scale = F, model.info = T, confint = T,
                   model.fit = T, digits = 3,transform.response = F)
  model_sum_std = summ(model, scale =T, model.info = T, confint = T,
                       model.fit = T, digits = 3,transform.response = T)
  model_r2 = summary(model)$adj.r.squared
  model_ests = model_sum$coeftable %>% as.data.frame() %>%  
    rownames_to_column(var = 'predictor') %>% filter(predictor != '(Intercept)')
  model_ests_std = model_sum_std$coeftable %>% as.data.frame() %>%  
    rownames_to_column(var = 'predictor') %>% filter(predictor != '(Intercept)') %>%
    dplyr::select(1:4)
  colnames(model_ests) = c('predictor',"B","low_ci","high_ci", "t_val", "p_val")
  colnames(model_ests_std) = c('predictor',"B_std","low_ci_std","high_ci_std")
  model_ests_all = model_ests %>% left_join(model_ests_std,by = 'predictor') %>% 
    mutate(r2_model = model_r2, outcome = outcome,model = pred)
  model_ests_all = model_ests_all %>% dplyr::select(c(12,1:2,7,3,8,4,9,5:6,10,11))
  #list_models <- data %>% dplyr::filter(is.na(!!sym(int)) == FALSE) %>% group_split(!!sym(int),.keep = T) %>% 
  #  map(~reg_mul(outcome, pred, covariates, .x))
  #names(list_models) = data %>% dplyr::select(int) %>% pull %>% levels
  model_ests_all = model_ests_all %>% mutate_if(is.numeric, round,digits = 3)
  results = list(model,model_ests_all#,list_models
  )
  names(results) = c("model","model_ests_all"#,"list_models"
  )
  return(results)
}



med_func = function(outcome, pred, med, covariates,bootstraping, data) {
  mediation_model = bruceR::PROCESS(data, y=outcome, x=pred, #center = TRUE,
                  meds = med, covs = covariates, digits = 3, std = TRUE,
                  ci="boot", nsim=bootstraping, seed=123)
  mediation_results_raw = mediation_model$results[[1]]$mediation %>% rownames_to_column() %>% mutate(across(where(is.numeric), round, 3)) %>% 
     janitor::clean_names() %>% rename(estimate = "effect", effect = "rowname")
  mediation_results_raw$predictor = pred
  mediation_results_raw$med = med
  mediation_results_raw$outcome = outcome
  mediation_results = mediation_model$results[[1]]$mediation %>% rownames_to_column() %>% .[c(1,2,9,6)] %>% 
    add_row(rowname = 'prop_med',Effect = .$Effect[1]/.$Effect[3] * 100,`[Boot 95% CI]` = NA) %>% 
    mutate(Effect = round(Effect,3),p = sprintf('%.2f', p)) %>%  unite('effect', 2:3,sep = ' ',na.rm = T) %>% 
    pivot_wider(names_from = rowname,values_from = c(2,3)) %>% janitor::clean_names() %>% 
    mutate(model = 'All', effect_prop_med = round(as.numeric(effect_prop_med),1)) %>% dplyr::select(model,1:7) #%>% janitor::clean_names()
  mediation_results$predictor = pred
  mediation_results$med = med
  mediation_results$outcome = outcome
  #smat = extract_mediation_summary(mediate_results) 
  results = list(mediation_model,mediation_results,mediation_results_raw)
  names(results) = c("mediation_model","mediation_results","mediation_results_raw")
  return(results)
} 


reg_int = function(outcome, pred, int, covariates, data) {
  # Construir fórmula con interacción
  formula_string = paste(outcome, '~', pred, '*', int, '+', paste(covariates, collapse = ' + '))
  model <- lm(formula = as.formula(formula_string), data = data)
  model_std <- lm(formula = as.formula(formula_string), data = jtools::standardize(data))
  
  # Resúmenes
  model_sum = summ(model, scale = FALSE, model.info = TRUE, confint = TRUE,
                   model.fit = TRUE, digits = 3, transform.response = FALSE)
  model_sum_std = summ(model, scale = TRUE, model.info = TRUE, confint = TRUE,
                       model.fit = TRUE, digits = 3, transform.response = TRUE)
  
  model_r2 = summary(model)$adj.r.squared
  
  # Estimaciones
  model_ests = model_sum$coeftable %>%
    as.data.frame() %>%
    rownames_to_column(var = 'predictor') %>%
    filter(predictor != '(Intercept)')
  
  model_ests_std = model_sum_std$coeftable %>%
    as.data.frame() %>%
    rownames_to_column(var = 'predictor') %>%
    filter(predictor != '(Intercept)') %>%
    dplyr::select(1:4)
  
  colnames(model_ests) = c('predictor',"B","low_ci","high_ci", "t_val", "p_val")
  colnames(model_ests_std) = c('predictor',"B_std","low_ci_std","high_ci_std")
  
  model_ests_all = model_ests %>%
    left_join(model_ests_std, by = 'predictor') %>%
    mutate(r2_model = model_r2, outcome = outcome, model = pred) %>%
    dplyr::select(c(12,1:2,7,3,8,4,9,5:6,10,11)) %>%
    mutate_if(is.numeric, round, digits = 3)
  
  # Agrupar por variable de interacción y ajustar modelos por grupo
  data_no_na <- data %>% dplyr::filter(!is.na(.data[[int]]))
  split_data <- data_no_na %>% group_split(.data[[int]], .keep = TRUE)
  group_labels <- data_no_na %>% group_keys(.data[[int]]) %>% pull(1)
  
  list_models <- map(split_data, ~reg_mul(outcome, pred, covariates, .x))
  names(list_models) <- as.character(group_labels)
  
  # Output
  results = list(model, model_std, model_ests_all, list_models)
  names(results) = c("model", "model_std", "model_ests_all", "list_models")
  return(results)
}

# 1 - instala de nuevo el paquete myfunctions:
#remotes::install_github("jhmigueles/myfunctions")

# 2 - lanza esta función (la verás en tu environment):
values4table_logReg = function(models) {
  
  # get values
  n = length(effects(models[[1]]))
  coefs = list()
  for (i in 1:length(models)) coefs[[i]] = summary(models[[i]])
  CIs = list()
  for (i in 1:length(models)) CIs[[i]] = exp(confint(models[[i]]))
  
  # get behavior names
  dominantBeh = c()
  for (i in 1:length(coefs)) {
    tmp = rownames(coefs[[i]]$coefficients)[2]
    dominantBeh[i] = paste0(unlist(strsplit(tmp, "min"))[1], "min")
  }
  
  # get betas
  betas = c()
  for (i in 1:length(coefs)) betas[i] = round(exp(coefs[[i]]$coefficients[2,1]), 3)
  
  # get p values
  pvals = c()
  for (i in 1:length(coefs)) pvals[i] = round(coefs[[i]]$coefficients[2,4], 3)
  
  # get p values
  CIlow = CIup = c()
  for (i in 1:length(CIs)) {
    CIlow[i] = round(CIs[[i]][2,1], 3)
    CIup[i] = round(CIs[[i]][2,2], 3)
  }
  
  # build table
  table = data.frame(dominantBeh = dominantBeh,
                     N = n,
                     OR = betas,
                     CIlow = CIlow,
                     CIup = CIup,
                     P = pvals)
  
  # show in console
  cat("\n")
  cat(paste0(rep("_", getOption("width")), collapse = ""))
  cat("\nModel coefficients:\n")
  print(table)
}

interaction <- function(data, compo, outcome, covariates, moderator) {
  
  # Correr modelos
  fits <- myfunctions::lm_coda(
    data = data,
    compo = compo,
    outcome = outcome,
    covariates = covariates,
    moderator = moderator
  )
  
  # Nombre de los comportamientos
  behaviors <- c("Moderate-to-Vigorous Physical Activity", 
                 "Light Physical Activity", 
                 "Sedentary Behavior", 
                 "Sleep")
  
  # Extraer resultados
  results <- lapply(seq_along(fits), function(i) {
    model <- fits[[i]]
    sm <- summary(model)
    ci <- confint(model)
    
    # Buscar fila con interacción (ej. ilr1*moderator)
    row_idx <- grep(paste0("ilr.*", moderator), rownames(sm$coefficients))
    
    # Selección robusta del índice para el CI
    if (length(row_idx) == 1) {
      est <- sm$coefficients[row_idx, "Estimate"]
      pval <- sm$coefficients[row_idx, "Pr(>|t|)"]
      conf.low <- ci[row_idx, 1]
      conf.high <- ci[row_idx, 2]
    } else {
      est <- NA
      pval <- NA
      conf.low <- NA
      conf.high <- NA
    }
    
    n <- length(effects(model))
    
    data.frame(
      Behavior = behaviors[i],
      n = n,
      beta_CI = if (!is.na(est)) {
        paste0(round(est, 2), " (", round(conf.low, 2), " to ", round(conf.high, 2), ")")
      } else {
        NA
      },
      `p_value` = if (!is.na(pval)) round(pval, 3) else NA,
      stringsAsFactors = FALSE
    )
  })
  
  # Combinar resultados
  do.call(rbind, results)
}

interaction_plot <- function(data, compo, outcome, covariates, moderator) {
  
  # Correr modelos
  fits <- myfunctions::lm_coda(
    data = data,
    compo = compo,
    outcome = outcome,
    covariates = covariates,
    moderator = moderator
  )
  
  # Nombre de los comportamientos
  behaviors <- c("Moderate-to-Vigorous Physical Activity", 
                 "Light Physical Activity", 
                 "Sedentary Behavior", 
                 "Sleep")
  
  # Extraer resultados
  results <- lapply(seq_along(fits), function(i) {
    model <- fits[[i]]
    sm <- summary(model)
    ci <- confint(model)
    
    # Buscar fila con interacción (ej. ilr1*moderator)
    row_idx <- grep(paste0("ilr.*", moderator), rownames(sm$coefficients))
    
    # Selección robusta del índice para el CI
    if (length(row_idx) == 1) {
      est <- sm$coefficients[row_idx, "Estimate"]
      pval <- sm$coefficients[row_idx, "Pr(>|t|)"]
      conf.low <- ci[row_idx, 1]
      conf.high <- ci[row_idx, 2]
    } else {
      est <- NA
      pval <- NA
      conf.low <- NA
      conf.high <- NA
    }
    
    n <- length(effects(model))
    
    # Construir columna b_ci
    est_round <- round(est, 2)
    conf.low_round <- round(conf.low, 2)
    conf.high_round <- round(conf.high, 2)
    b_ci <- if (!is.na(est)) {
      paste0(est_round, " (", conf.low_round, " to ", conf.high_round, ")")
    } else {
      NA
    }
    
    data.frame(
      Behavior = behaviors[i],
      n = n,
      beta = est_round,
      lowerCI = conf.low_round,
      upperCI = conf.high_round,
      CI = if (!is.na(conf.low)) paste0(conf.low_round, " to ", conf.high_round) else NA,
      p_value_interaction = round(pval, 3),
      outcome = outcome,
      moderator = moderator,
      
      b_ci = b_ci,
      stringsAsFactors = FALSE
    )
  })
  
  # Combinar resultados
  do.call(rbind, results)
}


interaction_subgroups_plot <- function(data, compo, outcome, covariates) {
  dataset_name <- deparse(substitute(data))
  # Correr modelos sin moderador
  fits <- myfunctions::lm_coda(
    data = data,
    compo = compo,
    outcome = outcome,
    covariates = covariates
  )
  
  # Nombre de los comportamientos
  behaviors <- c("Moderate-to-Vigorous Physical Activity", 
                 "Light Physical Activity", 
                 "Sedentary Behavior", 
                 "Sleep")
  
  # Extraer resultados
  results <- lapply(seq_along(fits), function(i) {
    model <- fits[[i]]
    sm <- summary(model)
    ci <- confint(model)
    
    # Selección del coeficiente principal (usaremos el primero de ilr)
    row_idx <- 2
    
    if (length(row_idx) == 1) {
      est <- sm$coefficients[row_idx, "Estimate"]
      pval <- sm$coefficients[row_idx, "Pr(>|t|)"]
      conf.low <- ci[row_idx, 1]
      conf.high <- ci[row_idx, 2]
    } else {
      est <- NA
      pval <- NA
      conf.low <- NA
      conf.high <- NA
    }
    
    n <- length(effects(model))
    
    est_round <- round(est, 2)
    conf.low_round <- round(conf.low, 2)
    conf.high_round <- round(conf.high, 2)
    b_ci <- if (!is.na(est)) {
      paste0(est_round, " (", conf.low_round, " to ", conf.high_round, ")")
    } else {
      NA
    }
    
    data.frame(
      Behavior = behaviors[i],
      n = n,
      beta = est_round,
      lowerCI = conf.low_round,
      upperCI = conf.high_round,
      CI = if (!is.na(conf.low)) paste0(conf.low_round, " to ", conf.high_round) else NA,
      p_value = round(pval, 2),
      outcome = outcome,
      dataset_name = dataset_name,
      b_ci = b_ci,
      stringsAsFactors = FALSE
    )
  })
  
  # Combinar resultados
  do.call(rbind, results)
}


fmt <- function(x) {
  ifelse(
    is.na(x),
    NA,
    ifelse(
      abs(x) < 0.01 & x != 0,
      sub("^(-?)0", "\\1", sprintf("%.3f", x)),
      sprintf("%.2f", x)
    )
  )
}

format_f <- function(x) {
  ifelse(
    is.na(x),
    NA,
    sprintf("%.2f", x)
  )
}

format_p <- function(x) {
  ifelse(
    is.na(x),
    NA,
    ifelse(
      x < 0.001,
      "<.001",
      ifelse(
        x < 0.01,
        sub("^0", "", sprintf("%.3f", x)),  # 0.008 -> .008
        sprintf("%.2f", x)                  # 0.034 -> 0.03
      )
    )
  )
}

get_30min_change_LPA_from_SB <- function(data_group, outcome_var, compo1, covs2, colors, color_id = 1) {
  
  # 1. Modelo one-to-one: +LPA -SB
  x_obj <- myfunctions::reallocationPlot(
    data = data_group,
    comps = compo1,
    comps.names = c("MVPA", "LPA", "SB", "Sleep"),
    outcome = outcome_var,
    covs = covs2,
    comparisons = "one-v-one",
    increase = "LPA",
    decrease = "SB",
    xlim = c(-5, 30),
    ylim = c(-0.5, 0.5),
    ylab = "Change in z-score",
    xlab = "↓ SB → ↑ LPA",
    main = "Reallocation plot increasing LPA and decreasing SB",
    col = colors[color_id]
  )
  
  # 2. Modelo con LPA como comportamiento aumentado
  mod <- x_obj$LPA
  
  # 3. Composición media en minutos
  mean_comp <- colMeans(data_group[, compo1], na.rm = TRUE)
  names(mean_comp) <- c("MVPA", "LPA", "SB", "Sleep")
  
  # 4. Composición reallocated: +30 min LPA, -30 min SB
  new_comp <- mean_comp
  new_comp["LPA"] <- new_comp["LPA"] + 30
  new_comp["SB"]  <- new_comp["SB"] - 30
  
  if (new_comp["SB"] <= 0) {
    stop("SB queda negativo o cero tras restar 30 min. Usa menos minutos.")
  }
  
  # 5. Convertir a proporciones
  mean_prop <- mean_comp / sum(mean_comp)
  new_prop  <- new_comp / sum(new_comp)
  
  # 6. ILRs con LPA como primer pivot
  make_lpa_ilr <- function(comp) {
    
    MVPA  <- comp["MVPA"]
    LPA   <- comp["LPA"]
    SB    <- comp["SB"]
    Sleep <- comp["Sleep"]
    
    data.frame(
      LPA_MVPA.SB.Sleep = sqrt(3/4) * log(LPA / ((MVPA * SB * Sleep)^(1/3))),
      MVPA_SB.Sleep     = sqrt(2/3) * log(MVPA / ((SB * Sleep)^(1/2))),
      SB_Sleep          = sqrt(1/2) * log(SB / Sleep)
    )
  }
  
  baseline_df <- make_lpa_ilr(mean_prop)
  new_df      <- make_lpa_ilr(new_prop)
  
  # 7. Añadir covariates medias
  for (cv in covs2) {
    baseline_df[[cv]] <- mean(data_group[[cv]], na.rm = TRUE)
    new_df[[cv]]      <- mean(data_group[[cv]], na.rm = TRUE)
  }
  
  # 8. Predicciones
  pred_baseline <- predict(mod, newdata = baseline_df)
  pred_new      <- predict(mod, newdata = new_df)
  
  change_30min <- as.numeric(pred_new - pred_baseline)
  
  # 9. Beta, CI y p del ILR_LPA proporcional
  beta_table <- summary(mod)$coefficients
  ci_table   <- confint(mod)
  
  beta <- beta_table["LPA_MVPA.SB.Sleep", "Estimate"]
  pval <- beta_table["LPA_MVPA.SB.Sleep", "Pr(>|t|)"]
  lci  <- ci_table["LPA_MVPA.SB.Sleep", 1]
  uci  <- ci_table["LPA_MVPA.SB.Sleep", 2]
  
  output <- data.frame(
    outcome = outcome_var,
    beta_ILR_LPA = beta,
    lowerCI = lci,
    upperCI = uci,
    p_value = pval,
    change_30min_SB_to_LPA = change_30min,
    baseline_pred = as.numeric(pred_baseline),
    new_pred = as.numeric(pred_new)
  )
  
  return(output)
}
