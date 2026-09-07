# Database cleaning#
rm(list = ls())

# ---- 1. Load libraries----
library(tidyverse)
library(openxlsx)
library(readr)
library(compositions)
library(zCompositions)

# ---- 2. Load & Prepare Data ----
data_acc <- read.xlsx("raw_data/data_acc.xlsx")
data_cog <- read.xlsx("raw_data/data_cognitive_processed.xlsx")
data_cog = data_cog[which(data_cog$redcap_event_name == "pre_intervention_arm_1"), ]
data_dem = read.xlsx("raw_data/AGUEDA-Demo_DATA_2025-06-09_1921.xlsx")
data_dxa <- read_csv("raw_data/cognitive_variables.csv")
data_apoe = read.csv("raw_data/apoe_data.csv")
data_memory = read.csv("raw_data/memory_data.csv")
data_hippovol = read.xlsx("raw_data/hippo_hippoAB_clipped_marked2.xlsx")
data_brainage = read.csv("raw_data/agueda_brainage_20250512.csv")
data_brainage = data_brainage[which(data_brainage$redcap_event_name == "pre_intervention_arm_1"), ]
data_AD = read.csv("raw_data/agueda_ADsignatures_20251014.csv")
data_AD = data_AD[which(data_AD$redcap_event_name == "Pre"), ]

# merge data to create one database
data_analisis0 <- merge(data_acc, data_dem) #[, c("record_id","pet_amyloid_status_mni","disease_basic_comor","disease_advance_comor", "screen_gender", "screen_age",  "screen_years_edu","psqi_comp1","psqi_sleep_quality","dxa_bmi","screen_tics_score")], by = "record_id")

data_analisis1 <- merge(data_analisis0, data_cog[, c("record_id", "comp_ex_func_mean_z","episodic_mem_mean_z", "processing_speed_mean_z",
                                                     "working_mem_mean_z" ,"attentional_control_mean_z", "visuospatial_mean_z",
                                                     "mmse_total_score", "moca_total_score")], by = "record_id", all = TRUE)

data_analisis2 <- merge(data_analisis1, data_dxa[, c("record_id")], by = "record_id", all = TRUE)
data_analisis3 <-  merge(data_analisis2, data_memory[, c("record_id", "cog1_rof_raw_copy", "cog1_rof_raw_mem", "cog1_ravlt_learning_correct", "cog1_ravlt_imme_correct", "cog1_ravlt_del_correct")], by = "record_id", all = TRUE)

data_analisis4 <- merge(data_analisis3, data_apoe[, c("record_id", "gen_apoee4_carrier")], by = "record_id", all = TRUE)

data_analisis5 <- merge(data_analisis4, data_hippovol, by = "record_id", all = TRUE)

data_analisis6 <- merge(data_analisis5, data_brainage, by = "record_id", all = TRUE)

data_analisis <- merge(data_analisis6, data_AD, by = "record_id", all = TRUE)

# remove no necessary database 
rm(data_pet,data_dem,data_dxa, data_analisis0, data_analisis1, data_cog,data_analisis2, data_analisis3, data_AD, data_brainage,data_analisis4,data_analisis5,data_analisis6, data_hippovol, data_acc,data_mmse_gds_moca, data_apoe, data_memory)

# ---- 3. Clean and Transform Data ----
##Bouts aggregation-------------------------------------------------------

# bouts 1 min or more
data_analisis$dur_day_mvpa_bts_1_min_wei = rowSums(data_analisis[, c("dur_day_mvpa_bts_1_2_min_wei", "dur_day_mvpa_bts_2_5_min_wei", 
                                                                     "dur_day_mvpa_bts_5_10_min_wei", "dur_day_mvpa_bts_10_min_wei")])
##New Light PA (adding unbouted mvpa) --------------------------------------
#1 (sum moderate PA + vigorous PA to calculate MVPA total)
data_analisis$dur_day_total_mvpa_min_wei = (data_analisis$dur_day_total_mod_min_wei_t450 + data_analisis$dur_day_total_vig_min_wei_t450)
#2 (remove variable of MVPA above 1 minute from total MVPA to create MVPA unbouted)
data_analisis$dur_day_mvpa1_unb_min_wei = data_analisis$dur_day_total_mvpa_min_wei - data_analisis$dur_day_mvpa_bts_1_min_wei
#3 (sum the MVPA unbouted and light PA to create a new variable of LPA) 
data_analisis$dur_day_total_lig_mvpa1_min_wei = rowSums(data_analisis[, c("dur_day_mvpa1_unb_min_wei",
                                                                          "dur_day_total_lig_min_wei")])

#4. Definition of variables of interest ---------------------------------------------------

compo1 = c("dur_day_mvpa_bts_1_min_wei", "dur_day_total_lig_mvpa1_min_wei",
           "dur_day_total_in_min_wei", "dur_spt_min_wei")

covs = c("screen_age", "screen_gender", "screen_years_edu")
covs2 = c("screen_gender", "screen_years_edu")

outcomes = c("episodic_mem_mean_z", "processing_speed_mean_z",
             "working_mem_mean_z" ,"attentional_control_mean_z", "visuospatial_mean_z",
             "ADsig_Williams2021_thick_vol_score_z", "ADsig_Williams2021_gmmd_score_z") #AD signatures
outnames = c("Episodic memory", "Processing speed", "Working memory",
                     "Attentional control", "Visuospatial processing", 
             "AD thickness/volume signature", "AD gray matter mean diffusivity signature ")

## Standardize compositions to 24 hours ------------------------------------
data_analisis[, c("dur_day_mvpa_bts_1_min_wei",
                  "dur_day_total_lig_mvpa1_min_wei",
                  "dur_day_total_in_min_wei",
                  "dur_spt_min_wei")] = clo(data_analisis[, compo1], total = 1440)

# Redefine compositions
compo1 = c("dur_day_mvpa_bts_1_min_wei", "dur_day_total_lig_mvpa1_min_wei",
           "dur_day_total_in_min_wei", "dur_spt_min_wei")

#5. Final data base creation----------------------------------------------------------

# creation of categorized variables 
data_analisis$cat_MVPA_bts1 <- cut(data_analisis$dur_day_mvpa_bts_1_min_wei, breaks = c(0, 150, 300, Inf)/7,
                                   labels = c("inactive", "active", "very active"), right = FALSE)
mvpa_cat = c("cat_MVPA_bts1")

data_analisis$cat2_MVPA_bts1 <- cut(data_analisis$dur_day_mvpa_bts_1_min_wei,
                                   breaks = c(0, 150, Inf)/7,  # Convertido a minutos por día
                                   labels = c("<150min", "≥150min"), #"<150min", "≥150min"
                                   right = FALSE)

data_analisis$edu_cat <- as.factor(ifelse(data_analisis$screen_years_edu <= 12, 0, 1))
data_analisis$screen_age_cat <- as.factor(ifelse(data_analisis$screen_age >= 72, 0, 1)) #Redefined the variable 0(Olders) and 1=1(Youngers)


# delete participants with missing data for ACC data
data_analisis <- data_analisis[complete.cases(data_analisis$filename), ] 

#creation database to work on for statistical analisis
write.csv(data_analisis, "raw_data/clean_data_v20_bts1min.csv", row.names = FALSE)
