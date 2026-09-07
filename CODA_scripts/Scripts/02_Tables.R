# ----  Run analyses and Prepare Tables ----

# 0. Install and load library -----------------------------------------------------------------
remotes::install_github("jhmigueles/deltacomp", force = TRUE)
remotes::install_github("jhmigueles/myfunctions", force = TRUE)
install.packages("cowplot")
install.packages("ggplot2")
install.packages("ggpubr")
install.packages("rlang")
install.packages("grid")

pacman::p_load(tidyverse, compositions, writexl,myfunctions, deltacomp, ggplot2, ggtern, dplyr, compositions, psych, corrplot, ppcor, ggforce, gridExtra, cowplot, ggpubr, rlang, sjPlot, tidyverse, openxlsx, readr, 
               zCompositions, broom, QuantPsyc, metR, grid,writexl,multilevelmediation, mudplyr, rlang, afex, readr, readxl, tidyverse, hrbrthemes, openxlsx, ggsci, ggpmisc, data.table, zoo, 
               gtools, pipeR, car, languageR, tableone, sjPlot, sjmisc, sjlabelled, ggeffects, survival, RNOmni, reshape2, showtext, ppcor, 
               Hmisc, corrplot, broom, ggplot2, facetscales, ggrepel, mice, ggrain, lavaan, mediation, raincloudplots, psych, mediation, 
               mvtnorm, berryFunctions, sandwich, ggrain, MetBrewer, gridExtra, LMMstar, sjtabledf, mlbench, magrittr, ggpubr, openxlsx, 
               patchwork, cowplot, effects, sjPlot, DescTools, nlme, rms, simstudy, emmeans, ggrepel, lme4, lmerTest, plotmodels, grid, 
               forestploter, forestplot, checkmate, mediation,dplyr,compositions)

# 1. Read and join files -----------------------------------------------------------------
data = read.csv("raw_data/clean_data_v20_bts1min.csv")

## 1.1 Functions -----------------------------------------------------------------
source("scripts/01.1_AD_median_categorization.R")
source("scripts/00_Functions.R")


## 1.2 Creation databases -----------------------------------------------------------------
high_ct <- data %>% filter(ADsig_Williams2021_thick_vol_score_cat == "1") 
low_ct <- data %>% filter(ADsig_Williams2021_thick_vol_score_cat == "0")
high_md <- data %>% filter(ADsig_Williams2021_gmmd_score_cat == "1") 
low_md <- data %>% filter(ADsig_Williams2021_gmmd_score_cat == "0")

# 2. Setting things of data.frame -----------------------------------------------------------------
outcomes = c("attentional_control_mean_z","episodic_mem_mean_z", "processing_speed_mean_z",
             "visuospatial_mean_z","working_mem_mean_z" ,
             "ADsig_Williams2021_gmmd_score_z", "ADsig_Williams2021_thick_vol_score_z") #AD signatures
outnames = c("Attentional/inhibitory control","Episodic memory", "Processing speed","Visuospatial processing",
             "Working memory","Gray matter mean diffusivity signature",
             "Thickness/volume signature")

compo1 = c("dur_day_mvpa_bts_1_min_wei", "dur_day_total_lig_mvpa1_min_wei",
           "dur_day_total_in_min_wei", "dur_spt_min_wei")

# 3.Tables  -----------------------------------------------------------------
##Table 1. Descriptive ----
outcomes_table1 <- c(
  "screen_gender","screen_age","screen_years_edu","edu_cat", "dxa_wt_ave", "dxa_ht_ave", "dxa_bmi", "pet_amyloid_status_mni","gen_apoee4_carrier",
  "screen_tics_score","moca_total_score","mmse_total_score","attentional_control_mean_z","episodic_mem_mean_z", "processing_speed_mean_z",
  "visuospatial_mean_z","working_mem_mean_z",
  "ADsig_Williams2021_gmmd_score_z","ADsig_Williams2021_thick_vol_score_z",
  "dur_day_mvpa_bts_1_min_wei", "dur_day_total_lig_mvpa1_min_wei",
  "dur_day_total_in_min_wei", "dur_spt_min_wei"
)

# Categorical variables
cat_vars <- c("screen_gender","edu_cat", "pet_amyloid_status_mni", "gen_apoee4_carrier")

# Creation of table
Overall <- print(
  tableone::CreateTableOne(
    vars = outcomes_table1,
    factorVars = cat_vars,  
    addOverall = F,
    data = data
  ),
  showAllLevels = F
)

# table por GMMD
By_gmmd <- print(
  tableone::CreateTableOne(
    vars = outcomes_table1,
    factorVars = cat_vars,   
    strata = "ADsig_Williams2021_gmmd_score_cat",
    data = data
  ),
  showAllLevels = FALSE
)

# Table por CT
By_ct <- print(
  tableone::CreateTableOne(
    vars = outcomes_table1,
    factorVars = cat_vars,   
    strata = "ADsig_Williams2021_thick_vol_score_cat",
    data = data
  ),
  showAllLevels = FALSE
)
# Combine tables and format
table1 <- as.data.frame(cbind(Overall,By_gmmd, By_ct))

table1 <- table1[, -c(4,5,8,9 )]

# Convert to data.table and add row names
setDT(table1, keep.rownames = TRUE)

# Insert empty rows for readability
rows_to_insert <- c(2,12, 16,22,25)
table1 <- insertRows(table1, rows_to_insert, new = NA)

# Define outcomes for clarity
outcomes_labels <- c("n", "Physical characteristics", "Female (n, %)", "Age (y)", "Education length (y)", "≥ 12 years education (n, %)",
                     "Weight (kg)", "Height (cm)", "Body mass index (kg/m2)",
                     "≥12 Pathological amyloid load (n, %)",
                     "Apolipoprotein E4 carrier (n, %)","General cognition", "Spanish version of the modified Telephone Interview of Cognitive Status (score)",
                     "Montreal Cognitive Assessment (Raw score)", "Mini‑Mental State Examination (Raw score)",
                      "Cognitive domains", "Attentional/inhibitory control (z-score)","Episodic memory (z-score)", "Processing speed (z-score)", 
                     "Visuospatial processing (z-score)", "Working memory (z-score)",
                     "AD brain signatures", "Thickness/volume signature (z-score)", "Gray matter mean diffusivity signature (z-score)",
                     "Movement behaviors","Moderate-to-vigorous physical activity (min/day)", "Light physical activity (min/day)", "Sedentary behavior (min/day)", "Sleep time (min/day)"
                      
)
outcomes_labels <- data.frame(rn = outcomes_labels)

# Combine outcomes with table data
table1 <- cbind(outcomes_labels, table1)

# Remove unnecessary columns
table1 <- subset(table1, select = -2)

# Save the table to an Excel file
write.xlsx(table1, "processed_data/table1.xlsx", rownames = FALSE)

rm(outcomes_labels, Overall, table1)

## Table for Figure 2.Linear regression models (MOVEMENT BEHAVIORS VS COGNITION)  ----

#1. Episodic memory
table_episodic_mem_mean_z_cross <- interaction_subgroups_plot(
  data = data,
  compo = compo1,
  outcome = "episodic_mem_mean_z",
  covariates = c("screen_age","screen_gender", "screen_years_edu"))

  # 2. Processing speed
table_processing_speed_mean_z_cross <- interaction_subgroups_plot(
  data = data,
  compo = compo1,
  outcome = "processing_speed_mean_z",
  covariates = c("screen_age", "screen_gender","screen_years_edu")
)

# 3. Working memory
table_working_mem_mean_z_cross <- interaction_subgroups_plot(
  data = data,
  compo = compo1,
  outcome = "working_mem_mean_z",
  covariates = c("screen_age","screen_gender", "screen_years_edu")
)

# 4. Attentional control
table_attentional_control_mean_z_cross <- interaction_subgroups_plot(
  data = data,
  compo = compo1,
  outcome = "attentional_control_mean_z",
  covariates = c("screen_age", "screen_gender","screen_years_edu")
)

# 5. Visuospatial
table_visuospatial_mean_z_cross <- interaction_subgroups_plot(
  data = data,
  compo = compo1,
  outcome = "visuospatial_mean_z",
  covariates = c("screen_age", "screen_gender","screen_years_edu")
)

# Tittles
episodic_header <- data.frame(Behavior = "Episodic memory",   n = NA,beta =NA, lowerCI=NA, upperCI=NA, CI = NA, p_value = NA,outcome=NA, dataset_name =NA, b_ci=NA)
processing_header <- data.frame(Behavior = "Processing speed",  n = NA,beta =NA, lowerCI=NA, upperCI=NA, CI = NA, p_value = NA,outcome=NA, dataset_name =NA, b_ci=NA)
working_header <- data.frame(Behavior = "Working memory",  n = NA,beta =NA, lowerCI=NA, upperCI=NA, CI = NA, p_value = NA,outcome=NA, dataset_name =NA, b_ci=NA)
attentional_header <- data.frame(Behavior = "Attentional/inhibitory control",  n = NA,beta =NA, lowerCI=NA, upperCI=NA, CI = NA, p_value = NA, outcome=NA,dataset_name =NA, b_ci=NA)
visuo_header <- data.frame(Behavior = "Visuospatial processing", n = NA,beta =NA, lowerCI=NA, upperCI=NA, CI = NA, p_value = NA,outcome=NA, dataset_name =NA, b_ci=NA)


table_main <- rbind(
  episodic_header,
  table_episodic_mem_mean_z_cross,
  processing_header,
  table_processing_speed_mean_z_cross,
  working_header,
  table_working_mem_mean_z_cross,
  attentional_header,
  table_attentional_control_mean_z_cross,
  visuo_header,
  table_visuospatial_mean_z_cross
)

table_figure2 <- rbind(
  table_episodic_mem_mean_z_cross,
  table_processing_speed_mean_z_cross,
  table_working_mem_mean_z_cross,
  table_attentional_control_mean_z_cross,
  table_visuospatial_mean_z_cross
  
)

write_xlsx(table_figure2, "processed_data/table_figure2.xlsx")

rm(episodic_header,
   table_episodic_mem_mean_z_cross,processing_header,
   table_processing_speed_mean_z_cross,
   working_header,table_working_mem_mean_z_cross,
   attentional_header,table_attentional_control_mean_z_cross,
   visuo_header,table_visuospatial_mean_z_cross,
   thick_header,table_thick,
   gmd_header,table_gmmd
)

##Table for Figure 3-Interactions GMMD------------------------------------------------

#1. Episodic memory
table_episodic_mem_mean_z_cross <- interaction_plot(
  data = data,
  compo = compo1,
  outcome = "episodic_mem_mean_z",
  covariates = c( "screen_gender", "screen_years_edu"),
  moderator = "ADsig_Williams2021_gmmd_score_z"
)

# 2. Processing speed
table_processing_speed_mean_z_cross <- interaction_plot(
  data = data,
  compo = compo1,
  outcome = "processing_speed_mean_z",
  covariates = c( "screen_gender", "screen_years_edu"),
  moderator = "ADsig_Williams2021_gmmd_score_z"
)

# 3. Working memory
table_working_mem_mean_z_cross <- interaction_plot(
  data = data,
  compo = compo1,
  outcome = "working_mem_mean_z",
  covariates = c( "screen_gender", "screen_years_edu"),
  moderator = "ADsig_Williams2021_gmmd_score_z"
)

# 4. Attentional control
table_attentional_control_mean_z_cross <- interaction_plot(
  data = data,
  compo = compo1,
  outcome = "attentional_control_mean_z",
  covariates = c( "screen_gender", "screen_years_edu"),
  moderator = "ADsig_Williams2021_gmmd_score_z"
)

# 5. Visuospatial
table_visuospatial_mean_z_cross <- interaction_plot(
  data = data,
  compo = compo1,
  outcome = "visuospatial_mean_z",
  covariates = c("screen_gender","screen_years_edu"),
  moderator = "ADsig_Williams2021_gmmd_score_z"
)


table_interactions_plot_gmd <- rbind( 
  
  table_episodic_mem_mean_z_cross,
  
  table_processing_speed_mean_z_cross,
  
  table_working_mem_mean_z_cross,
  
  table_attentional_control_mean_z_cross,
  
  table_visuospatial_mean_z_cross
)

write_xlsx(table_interactions_plot_gmd, "processed_data/table_interaction_plot_gmd.xlsx")

##Table for Figure 3-Subgroups GMMD  ------------------------------------------------
#1. Episodic memory 
table_episodic_mem_mean_z_cross_high <- interaction_subgroups_plot(
  data = high_md,
  compo = compo1,
  outcome = "episodic_mem_mean_z",
  covariates = c( "screen_gender", "screen_years_edu")
)

table_episodic_mem_mean_z_cross_low <- interaction_subgroups_plot(
  data = low_md,
  compo = compo1,
  outcome = "episodic_mem_mean_z",
  covariates = c( "screen_gender", "screen_years_edu")
)

# 2. Processing speed
table_processing_speed_mean_z_cross_high <- interaction_subgroups_plot(
  data = high_md,
  compo = compo1,
  outcome = "processing_speed_mean_z",
  covariates = c( "screen_gender", "screen_years_edu")
)

table_processing_speed_mean_z_cross_low <- interaction_subgroups_plot(
  data = low_md,
  compo = compo1,
  outcome = "processing_speed_mean_z",
  covariates = c( "screen_gender", "screen_years_edu")
)

# 3. Working memory
table_working_mem_mean_z_cross_high <- interaction_subgroups_plot(
  data = high_md,
  compo = compo1,
  outcome = "working_mem_mean_z",
  covariates = c( "screen_gender", "screen_years_edu")
)
table_working_mem_mean_z_cross_low <- interaction_subgroups_plot(
  data = low_md,
  compo = compo1,
  outcome = "working_mem_mean_z",
  covariates = c( "screen_gender", "screen_years_edu")
)

# 4. Attentional control
table_attentional_control_mean_z_cross_high <- interaction_subgroups_plot(
  data = high_md,
  compo = compo1,
  outcome = "attentional_control_mean_z",
  covariates = c( "screen_gender", "screen_years_edu")
)
table_attentional_control_mean_z_cross_low <- interaction_subgroups_plot(
  data = low_md,
  compo = compo1,
  outcome = "attentional_control_mean_z",
  covariates = c( "screen_gender", "screen_years_edu")
)


# 5. Visuospatial
table_visuospatial_mean_z_cross_high <- interaction_subgroups_plot(
  data = high_md,
  compo = compo1,
  outcome = "visuospatial_mean_z",
  covariates = c( "screen_gender","screen_years_edu")
)

table_visuospatial_mean_z_cross_low <- interaction_subgroups_plot(
  data = low_md,
  compo = compo1,
  outcome = "visuospatial_mean_z",
  covariates = c( "screen_gender","screen_years_edu")
)


table_subgroup_plot_gmd <- rbind( 
  table_episodic_mem_mean_z_cross_high,
  table_episodic_mem_mean_z_cross_low,
  table_processing_speed_mean_z_cross_high,
  table_processing_speed_mean_z_cross_low,
  table_working_mem_mean_z_cross_high,
  table_working_mem_mean_z_cross_low,
  table_attentional_control_mean_z_cross_high,
  table_attentional_control_mean_z_cross_low,
  table_visuospatial_mean_z_cross_high,
  table_visuospatial_mean_z_cross_low
)

table_subgroup_plot_gmd <- table_subgroup_plot_gmd %>%
  separate(dataset_name, into = c("subgroup", "moderator"), sep = "_", remove = FALSE)

write_xlsx(table_subgroup_plot_gmd, "processed_data/table_subgroups_plot_gmd.xlsx")


## Table S1. F-tests for moderation by AD brain signatures ------------------------------------------------------
compo <- compo1

outcomes <- c(
  "attentional_control_mean_z",
  "episodic_mem_mean_z",
  "processing_speed_mean_z",
  "visuospatial_mean_z",
  "working_mem_mean_z"
)

outcome_labels <- c(
  attentional_control_mean_z = "Attentional/inhibitory control",
  episodic_mem_mean_z = "Episodic memory",
  processing_speed_mean_z = "Processing speed",
  visuospatial_mean_z = "Visuospatial processing",
  working_mem_mean_z = "Working memory"
)

covariates_cog <- c("screen_gender", "screen_years_edu")

moderator_gmmd <- "ADsig_Williams2021_gmmd_score_z"
moderator_thick <- "ADsig_Williams2021_thick_vol_score_z"

# 2. Create ILR coordinates

create_ilr_data <- function(data, compo) {
  
  compo_data <- data %>%
    dplyr::select(all_of(compo))
  
  ilr_values <- compositions::ilr(compositions::acomp(compo_data))
  ilr_values <- as.data.frame(ilr_values)
  names(ilr_values) <- paste0("ilr", seq_len(ncol(ilr_values)))
  
  data_ilr <- bind_cols(data, ilr_values)
  
  return(data_ilr)
}

# 3. Function to extract global F-test for time-use composition

get_composition_test <- function(data, compo, outcome, covariates) {
  
  data_ilr <- create_ilr_data(data, compo)
  ilrs <- paste0("ilr", seq_len(length(compo) - 1))
  
  vars_needed <- c(outcome, covariates, ilrs)
  
  d <- data_ilr %>%
    dplyr::select(all_of(vars_needed)) %>%
    tidyr::drop_na()
  
  cov_string <- paste(covariates, collapse = " + ")
  ilr_string <- paste(ilrs, collapse = " + ")
  
  model_cov <- lm(
    as.formula(paste(outcome, "~", cov_string)),
    data = d
  )
  
  model_comp <- lm(
    as.formula(paste(outcome, "~", cov_string, "+", ilr_string)),
    data = d
  )
  
  test <- anova(model_cov, model_comp)
  
  tibble(
    variable = "Time-use composition",
    outcome = outcome,
    F = test$F[2],
    P = test$`Pr(>F)`[2],
    df1 = test$Df[2],
    df2 = test$Res.Df[2],
    n = nobs(model_comp)
  )
}

# 4. Function to extract global F-test for interaction


get_interaction_test <- function(data, compo, outcome, covariates, moderator, label) {
  
  data_ilr <- create_ilr_data(data, compo)
  ilrs <- paste0("ilr", seq_len(length(compo) - 1))
  
  vars_needed <- c(outcome, covariates, moderator, ilrs)
  
  d <- data_ilr %>%
    dplyr::select(all_of(vars_needed)) %>%
    tidyr::drop_na()
  
  cov_string <- paste(covariates, collapse = " + ")
  ilr_string <- paste(ilrs, collapse = " + ")
  
  model_no_int <- lm(
    as.formula(
      paste(outcome, "~", cov_string, "+", moderator, "+", ilr_string)
    ),
    data = d
  )
  
  model_with_int <- lm(
    as.formula(
      paste(outcome, "~", cov_string, "+", moderator, "* (", ilr_string, ")")
    ),
    data = d
  )
  
  test <- anova(model_no_int, model_with_int)
  
  tibble(
    variable = label,
    outcome = outcome,
    F = test$F[2],
    P = test$`Pr(>F)`[2],
    df1 = test$Df[2],
    df2 = test$Res.Df[2],
    n = nobs(model_with_int)
  )
}

# 5. Run all tests


table_composition <- map_dfr(
  outcomes,
  ~ get_composition_test(
    data = data,
    compo = compo,
    outcome = .x,
    covariates = covariates_cog
  )
)

table_gmmd_interaction <- map_dfr(
  outcomes,
  ~ get_interaction_test(
    data = data,
    compo = compo,
    outcome = .x,
    covariates = covariates_cog,
    moderator = moderator_gmmd,
    label = "Time-use composition × GMMD signature"
  )
)

table_thick_interaction <- map_dfr(
  outcomes,
  ~ get_interaction_test(
    data = data,
    compo = compo,
    outcome = .x,
    covariates = covariates_cog,
    moderator = moderator_thick,
    label = "Time-use composition × thickness/volume signature"
  )
)

table_anova <- bind_rows(
  table_composition,
  table_gmmd_interaction,
  table_thick_interaction
)


# 7. Reshape table
table_anova <- table_anova %>%
  mutate(
    outcome_label = outcome_labels[outcome],
    F = format_f(F),
    P = format_p(P),
    variable = factor(
      variable,
      levels = c(
        "Time-use composition",
        "Time-use composition × GMMD signature",
        "Time-use composition × thickness/volume signature"
      )
    )
  ) %>%
  dplyr::select(variable, outcome_label, F, P) %>%
  pivot_wider(
    names_from = outcome_label,
    values_from = c(F, P),
    names_glue = "{outcome_label}_{.value}"
  ) %>%
  arrange(variable) %>%
  dplyr::select(
    variable,
    `Attentional/inhibitory control_F`,
    `Attentional/inhibitory control_P`,
    `Episodic memory_F`,
    `Episodic memory_P`,
    `Processing speed_F`,
    `Processing speed_P`,
    `Visuospatial processing_F`,
    `Visuospatial processing_P`,
    `Working memory_F`,
    `Working memory_P`
  )


# Rename columns to simple names first
colnames(table_anova) <- c(
  "Variable",
  "Attentional/inhibitory control_F",
  "Attentional/inhibitory control_P",
  "Episodic memory_F",
  "Episodic memory_P",
  "Processing speed_F",
  "Processing speed_P",
  "Visuospatial processing_F",
  "Visuospatial processing_P",
  "Working memory_F",
  "Working memory_P"
)

# First header row: outcome names
header_row_1 <- data.frame(
  Variable = "",
  `Attentional/inhibitory control_F` = "Attentional/inhibitory control",
  `Attentional/inhibitory control_P` = "",
  `Episodic memory_F` = "Episodic memory",
  `Episodic memory_P` = "",
  `Processing speed_F` = "Processing speed",
  `Processing speed_P` = "",
  `Visuospatial processing_F` = "Visuospatial processing",
  `Visuospatial processing_P` = "",
  `Working memory_F` = "Working memory",
  `Working memory_P` = "",
  check.names = FALSE
)

# Second header row: F and P
header_row_2 <- data.frame(
  Variable = "",
  `Attentional/inhibitory control_F` = "F",
  `Attentional/inhibitory control_P` = "P",
  `Episodic memory_F` = "F",
  `Episodic memory_P` = "P",
  `Processing speed_F` = "F",
  `Processing speed_P` = "P",
  `Visuospatial processing_F` = "F",
  `Visuospatial processing_P` = "P",
  `Working memory_F` = "F",
  `Working memory_P` = "P",
  check.names = FALSE
)

# Combine header rows + results
table_anova <- bind_rows(
  header_row_1,
  header_row_2,
  table_anova
)

# Save as Excel
write_xlsx(
  table_anova,
  "processed_data/table_anova.xlsx"
)


## Table S2. Interactions CT and GMMD ------------------------------------------------------

### CT ------------------------------------------------------
#1. Episodic memory
table_episodic_mem_mean_z_cross <- interaction(
  data = data,
  compo = compo1,
  outcome = "episodic_mem_mean_z",
  covariates = c( "screen_gender", "screen_years_edu"),
  moderator = "ADsig_Williams2021_thick_vol_score_z"
)

# 2. Processing speed
table_processing_speed_mean_z_cross <- interaction(
  data = data,
  compo = compo1,
  outcome = "processing_speed_mean_z",
  covariates = c( "screen_gender", "screen_years_edu"),
  moderator = "ADsig_Williams2021_thick_vol_score_z"
)

# 3. Working memory
table_working_mem_mean_z_cross <- interaction(
  data = data,
  compo = compo1,
  outcome = "working_mem_mean_z",
  covariates = c( "screen_gender", "screen_years_edu"),
  moderator = "ADsig_Williams2021_thick_vol_score_z"
)

# 4. Attentional control
table_attentional_control_mean_z_cross <- interaction(
  data = data,
  compo = compo1,
  outcome = "attentional_control_mean_z",
  covariates = c( "screen_gender", "screen_years_edu"),
  moderator = "ADsig_Williams2021_thick_vol_score_z"
)

# 5. Visuospatial
table_visuospatial_mean_z_cross <- interaction(
  data = data,
  compo = compo1,
  outcome = "visuospatial_mean_z",
  covariates = c( "screen_gender", "screen_years_edu"),
  moderator = "ADsig_Williams2021_thick_vol_score_z"
)


# Tittles
# Tittles
moderator_header<-data.frame(Behavior = NA,   n = "AD thickness/volume signature ", beta_CI = NA, p_value = NA)
episodic_header <- data.frame(Behavior = "Episodic memory",    n = NA, beta_CI = NA, p_value = NA)
processing_header <- data.frame(Behavior = "Processing speed",  n = NA, beta_CI = NA, p_value = NA)
working_header <- data.frame(Behavior = "Working memory", n = NA, beta_CI = NA, p_value = NA)
attentional_header <- data.frame(Behavior = "Attentional/inhibitory control",   n = NA, beta_CI = NA, p_value = NA)
visuo_header <- data.frame(Behavior = "Visuospatial processing",   n = NA, beta_CI = NA, p_value = NA)


table_ct <- rbind(
    moderator_header,
    attentional_header,
    table_attentional_control_mean_z_cross,
    episodic_header,
    table_episodic_mem_mean_z_cross,
    processing_header,
    table_processing_speed_mean_z_cross,
    visuo_header,
    table_visuospatial_mean_z_cross,
    working_header,
    table_working_mem_mean_z_cross
)
library(writexl)
write_xlsx(table_ct, "processed_data/table_interaction_ct.xlsx")

### GMMD ------------------------------------------------------
#1. Episodic memory
table_episodic_mem_mean_z_cross <- interaction(
  data = data,
  compo = compo1,
  outcome = "episodic_mem_mean_z",
  covariates = c( "screen_gender", "screen_years_edu"),
  moderator = "ADsig_Williams2021_gmmd_score_z"
)

# 2. Processing speed
table_processing_speed_mean_z_cross <- interaction(
  data = data,
  compo = compo1,
  outcome = "processing_speed_mean_z",
  covariates = c( "screen_gender", "screen_years_edu"),
  moderator = "ADsig_Williams2021_gmmd_score_z"
)

# 3. Working memory
table_working_mem_mean_z_cross <- interaction(
  data = data,
  compo = compo1,
  outcome = "working_mem_mean_z",
  covariates = c( "screen_gender", "screen_years_edu"),
  moderator = "ADsig_Williams2021_gmmd_score_z"
)

# 4. Attentional control
table_attentional_control_mean_z_cross <- interaction(
  data = data,
  compo = compo1,
  outcome = "attentional_control_mean_z",
  covariates = c( "screen_gender", "screen_years_edu"),
  moderator = "ADsig_Williams2021_gmmd_score_z"
)

# 5. Visuospatial
table_visuospatial_mean_z_cross <- interaction(
  data = data,
  compo = compo1,
  outcome = "visuospatial_mean_z",
  covariates = c( "screen_gender", "screen_years_edu"),
  moderator = "ADsig_Williams2021_gmmd_score_z"
)



# Tittles
moderator_header<-data.frame(Behavior = NA,   n = "AD gray matter mean diffusivity signature ", beta_CI = NA, p_value = NA)
episodic_header <- data.frame(Behavior = "Episodic memory",    n = NA, beta_CI = NA, p_value = NA)
processing_header <- data.frame(Behavior = "Processing speed",  n = NA, beta_CI = NA, p_value = NA)
working_header <- data.frame(Behavior = "Working memory", n = NA, beta_CI = NA, p_value = NA)
attentional_header <- data.frame(Behavior = "Attentional/inhibitory control",   n = NA, beta_CI = NA, p_value = NA)
visuo_header <- data.frame(Behavior = "Visuospatial processing",   n = NA, beta_CI = NA, p_value = NA)

table_gmmd <- rbind(
  moderator_header,
  attentional_header,
  table_attentional_control_mean_z_cross,
  episodic_header,
  table_episodic_mem_mean_z_cross,
  processing_header,
  table_processing_speed_mean_z_cross,
  visuo_header,
  table_visuospatial_mean_z_cross,
  working_header,
  table_working_mem_mean_z_cross

)

write_xlsx(table_gmmd, "processed_data/table_interaction_gmd.xlsx")

##Merge both data frames
table_interaction_brain <- cbind(
  table_ct,
  table_gmmd[ , -which(names(table_gmmd) == "Behavior")])

table_interaction_brain <- table_interaction_brain %>%
  setNames(c("Behavior",
             "n_ct", "beta_ct", "pval_ct",
             "n_gmd", "beta_gmd", "pval_gmd"))

table_interaction_brain <- table_interaction_brain %>%
  mutate(
    pval_ct = fmt(pval_ct),
    pval_gmd = fmt(pval_gmd)
  )

write_xlsx(table_interaction_brain, "processed_data/table_interaction_brain.xlsx")

#Additional data extraction------------------------------------------------------


wm_high_30 <- get_30min_change_LPA_from_SB(
  data_group = high_md,
  outcome_var = "working_mem_mean_z",
  compo1 = compo1,
  covs2 = covs2,
  colors = colors,
  color_id = 1
)

wm_high_30

ac_low_30 <- get_30min_change_LPA_from_SB(
  data_group = low_md,
  outcome_var = "attentional_control_mean_z",
  compo1 = compo1,
  covs2 = covs2,
  colors = colors,
  color_id = 2
)

ac_low_30

