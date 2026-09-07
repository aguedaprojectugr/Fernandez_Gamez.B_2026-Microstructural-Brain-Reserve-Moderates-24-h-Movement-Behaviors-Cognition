### Categorization of High/Low AD thickness/volume signature
library(dplyr)
# Calcular mediana PRE
median_thick_vol <- median(
  data$ADsig_Williams2021_thick_vol_score_z[data$redcap_event_name == "Pre"],
  na.rm = TRUE
)

# Crear tabla SOLO con PRE válidos
thick_cat <- data %>%
  filter(
    redcap_event_name == "Pre",
    !is.na(ADsig_Williams2021_thick_vol_score_z)
  ) %>%
  mutate(
    ADsig_Williams2021_thick_vol_score_cat = case_when(
      ADsig_Williams2021_thick_vol_score_z <= median_thick_vol ~ "0",
      ADsig_Williams2021_thick_vol_score_z > median_thick_vol ~ "1"
    )
  ) %>%
  dplyr::select(record_id, ADsig_Williams2021_thick_vol_score_cat)

# Hacer join seguro
data <- data %>%
  dplyr::select(-contains("ADsig_Williams2021_thick_vol_score_cat")) %>%
  left_join(thick_cat, by = "record_id") %>%
  mutate(
    ADsig_Williams2021_thick_vol_score_cat =
      factor(ADsig_Williams2021_thick_vol_score_cat,
             levels = c("0", "1"))
  )

################## Categorization of High/Low AD MD signature



# Calcular mediana PRE
median_gmmd <- median(
  data$ADsig_Williams2021_gmmd_score_z[data$redcap_event_name == "Pre"],
  na.rm = TRUE
)

# Crear tabla SOLO con PRE válidos
gmmd_cat <- data %>%
  filter(
    redcap_event_name == "Pre",
    !is.na(ADsig_Williams2021_gmmd_score_z)
  ) %>%
  mutate(
    ADsig_Williams2021_gmmd_score_cat = case_when(
      ADsig_Williams2021_gmmd_score_z <= median_gmmd ~ "0",
      ADsig_Williams2021_gmmd_score_z > median_gmmd ~ "1"
    )
  ) %>%
  dplyr::select(record_id, ADsig_Williams2021_gmmd_score_cat)

# Join seguro
data <- data %>%
  dplyr::select(-contains("ADsig_Williams2021_gmmd_score_cat")) %>%
  left_join(gmmd_cat, by = "record_id") %>%
  mutate(
    ADsig_Williams2021_gmmd_score_cat =
      factor(ADsig_Williams2021_gmmd_score_cat,
             levels = c("0", "1"))
  )


median_thick_vol <- median(data$ADsig_Williams2021_thick_vol_score_z[data$redcap_event_name == "Pre"], na.rm = TRUE)
median_thick_vol

# Mediana de AD MD score
median_gmmd <- median(data$ADsig_Williams2021_gmmd_score_z[data$redcap_event_name == "Pre"], na.rm = TRUE)
median_gmmd
