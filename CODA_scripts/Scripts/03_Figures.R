# 0. Install and load library -----------------------------------------------------------------
pacman::p_load(myfunctions, deltacomp, ggplot2, ggtern, dplyr, compositions, psych, corrplot, ppcor, ggforce, gridExtra, cowplot, ggpubr, rlang, sjPlot, tidyverse, openxlsx, readr, 
               zCompositions, broom, QuantPsyc, metR, grid,writexl,multilevelmediation, mudplyr, rlang, afex, readr, readxl, tidyverse, hrbrthemes, openxlsx, ggsci, ggpmisc, data.table, zoo, 
               gtools, pipeR, car, languageR, tableone, sjPlot, sjmisc, sjlabelled, ggeffects, survival, RNOmni, reshape2, showtext, ppcor, 
               Hmisc, corrplot, broom, ggplot2, facetscales, ggrepel, mice, ggrain, lavaan, mediation, raincloudplots, psych, mediation, 
               mvtnorm, berryFunctions, sandwich, ggrain, MetBrewer, gridExtra, LMMstar, sjtabledf, mlbench, magrittr, ggpubr, openxlsx, 
               patchwork, cowplot, effects, sjPlot, DescTools, nlme, rms, simstudy, emmeans, ggrepel, lme4, lmerTest, plotmodels, grid, 
               forestploter, forestplot, checkmate, mediation,dplyr,magick, ggtext)

# 1. Figures -----------------------------------------------------------------
## Fig 1.A - Ternary plot GMMD ------------------------------------------------
colors_groups <- c("Higher GMMD" = "#6A3D9A",
                   "Lower GMMD"  = "#009E73")
# colors to use 
colors <- c(
  "#6A3D9A",  # morado fuerte
  "#009E73",
  "#D55E00",
  "#0072B2"
)

# theme of the plot
ternary_theme = ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white"),
                               panel.background = ggplot2::element_rect(fill = "grey75"),
                               text = ggplot2::element_text(size = 15, face = 2,
                                                            colour = "black"),
                               axis.text = ggplot2::element_text(size = 15,
                                                                 face = 2, colour = "black"),
                               line = ggplot2::element_line(linewidth = 1, colour = "black"))

# --- Preparación datos A ---
ternary_data_a_high <- as.data.frame(compositions::clo(high_md[, compo1], total = 1440) / 1440)
colnames(ternary_data_a_high) <- c("MVPA","LPA","SB","Sleep")
ternary_data_a_high$Group <- "Higher GMMD"
ternary_data_a_low <- as.data.frame(compositions::clo(low_md[, compo1], total = 1440) / 1440)
colnames(ternary_data_a_low) <- c("MVPA","LPA","SB","Sleep")
ternary_data_a_low$Group <- "Lower GMMD"

# Unir ambos
ternary_data_a <- rbind(ternary_data_a_high, ternary_data_a_low)

# Calcular centroides
gm <- function(x) exp(mean(log(x), na.rm = TRUE))

mark_points_a_high <- compositions::clo(apply(high_md[, compo1], 2, gm), total = 1440) / 1440
mark_points_a_low  <- compositions::clo(apply(low_md[, compo1], 2, gm), total = 1440) / 1440

mark_points_a <- rbind(
  data.frame(t(mark_points_a_high), Group = "Higher GMMD"),
  data.frame(t(mark_points_a_low),  Group = "Lower GMMD")
)
colnames(mark_points_a)[1:4] <- c("MVPA","LPA","SB","Sleep")

# --- Plot A ---
colors_groups <- c("Higher GMMD" = "#6A3D9A", "Lower GMMD" = "#009E73")

f2a <- ggtern(data = ternary_data_a,
              aes(x = MVPA, y = LPA, z = SB, color = Group, fill = Group)) +
  stat_ellipse(level = 0.95, alpha = 0.2) +
  stat_density_tern(geom = "polygon", bins = 4, alpha = 0.4, bdl = 0.010) +
  geom_point(data = mark_points_a,
             aes(x = MVPA, y = LPA, z = SB, color = Group),
             linewidth = 1.2) +
  scale_color_manual(
    values = colors_groups,
    labels = c(
      "<b>Higher GMMD</b><br><b>MVPA:</b> 2.72% (39.25 min)<br><b>LPA:</b> 17.69% (254.76 min)<br><b>SB:</b> 48.55% (699.18 min)",
      
      "<b>Lower GMMD</b><br><b>MVPA:</b> 1.96% (28.32 min)<br><b>LPA:</b> 16.96% (244.79 min)<br><b>SB:</b> 49.05% (706.44 min)"
    )
  ) +
  
  scale_fill_manual(
    values = colors_groups,
    labels = c(
      "<b>Higher GMMD</b><br><b>MVPA:</b> 2.72% (39.25 min)<br><b>LPA:</b> 17.69% (254.76 min)<br><b>SB:</b> 48.55% (699.18 min)",
      
      "<b>Lower GMMD</b><br><b>MVPA:</b> 1.96% (28.32 min)<br><b>LPA:</b> 16.96% (244.79 min)<br><b>SB:</b> 49.05% (706.44 min)"
    )
  ) +
  theme(
    legend.position = c(0.95, 0.95),   # posición dentro del plot (x, y) de 0 a 1
    legend.justification = c("right", "top"), # qué esquina de la leyenda se usa como referencia
    legend.text.align = 0,             # alineación del texto a la izquierda
    legend.key.height = unit(2, "lines"),
    legend.text = ggtext::element_markdown(
      face = "plain") # altura de cada clave para los textos largos
  ) +
  ternary_theme +
  theme_showarrows() +
  labs(x = "", xarrow = "MVPA (%)",
       y = "", yarrow = "LPA (%)",
       z = "", zarrow = "SB (%)") +
  ggtitle("A.Ternary plots for gray matter mean diffusivity signature") +
theme(plot.title = element_text(face = "bold", size = 16,
                                margin = margin(t = 20, r = 0, b = 10, l = 0)))
# --- Preparación datos B ---
# --- Preparación datos B (GMMD) con el método normalizado ---
high_md$totalPA <- rowSums(high_md[, c("dur_day_mvpa_bts_1_min_wei", "dur_day_total_lig_mvpa1_min_wei")])
low_md$totalPA  <- rowSums(low_md[,  c("dur_day_mvpa_bts_1_min_wei", "dur_day_total_lig_mvpa1_min_wei")])

# 1. Crear dataframe inicial de 3 partes
ternary_data_b_high <- as.data.frame(high_md[, c("totalPA","dur_day_total_in_min_wei","dur_spt_min_wei")])
colnames(ternary_data_b_high) <- c("PA","SB","Sleep")

ternary_data_b_low <- as.data.frame(low_md[, c("totalPA","dur_day_total_in_min_wei","dur_spt_min_wei")])
colnames(ternary_data_b_low) <- c("PA","SB","Sleep")

# 2. Re-cerrar las composiciones a 1440 / 1 (igual que tu compañero)
ternary_data_b_high <- as.data.frame(compositions::clo(ternary_data_b_high[, c("PA","SB","Sleep")], total = 1440) / 1440)
ternary_data_b_high$Group <- "Higher GMMD"

ternary_data_b_low  <- as.data.frame(compositions::clo(ternary_data_b_low[, c("PA","SB","Sleep")], total = 1440) / 1440)
ternary_data_b_low$Group <- "Lower GMMD"

ternary_data_b <- rbind(ternary_data_b_high, ternary_data_b_low)

# Centroides
mark_points_b_high <- compositions::clo(apply(ternary_data_b_high[,1:3], 2, gm), total = 1440) / 1440
mark_points_b_low  <- compositions::clo(apply(ternary_data_b_low[,1:3], 2, gm), total = 1440) / 1440

mark_points_b <- rbind(
  data.frame(t(mark_points_b_high), Group = "Higher GMMD"),
  data.frame(t(mark_points_b_low),  Group = "Lower GMMD")
)

# --- Plot B ---
f2b <- ggtern(data = ternary_data_b,
              aes(x = PA, y = Sleep, z = SB, color = Group, fill = Group)) +
  stat_ellipse(level = 0.95, alpha = 0.2) +
  stat_density_tern(geom = "polygon", bins = 4, alpha = 0.4, bdl = 0.010) +
  geom_point(data = mark_points_b,
             aes(x = PA, y = Sleep, z = SB, color = Group),
             linewidth = 1.2) +
  scale_color_manual(
    values = colors_groups,
    labels = c(
      "<b>Higher GMMD</b><br><b>PA:</b> 20.42% (294.01 min)<br><b>Sleep:</b> 31.03% (446.81 min)<br><b>SB:</b> 48.55% (699.18 min)",
      "<b>Lower GMMD</b><br><b>PA:</b> 18.97% (273.11 min)<br><b>Sleep:</b> 31.98% (460.45 min)<br><b>SB:</b> 49.05% (706.44 min)"
    )
  ) +
  
  scale_fill_manual(
    values = colors_groups,
    labels = c(
      "<b>Higher GMMD</b><br><b>PA:</b> 20.42% (294.01 min)<br><b>Sleep:</b> 31.03% (446.81 min)<br><b>SB:</b> 48.55% (699.18 min)",
      "<b>Lower GMMD</b><br><b>PA:</b> 18.97% (273.11 min)<br><b>Sleep:</b> 31.98% (460.45 min)<br><b>SB:</b> 49.05% (706.44 min)"
    )
  ) +
  theme(
    legend.position = c(0.95, 0.95),   # posición dentro del plot (x, y) de 0 a 1
    legend.justification = c("right", "top"), # qué esquina de la leyenda se usa como referencia
    legend.text.align = 0,             # alineación del texto a la izquierda
    legend.key.height = unit(2, "lines"),
    legend.text = ggtext::element_markdown(
      face = "plain")# altura de cada clave para los textos largos
  ) +
  ternary_theme +
  theme_showarrows() +
  labs(x = "", xarrow = "PA (%)",
       y = "", yarrow = "Sleep (%)",
       z = "", zarrow = "SB (%)") +
  ggtitle(" ") +
  theme(plot.title = element_text(
    face = "bold",
    size = 16,
    margin = margin(t = 20, b = 10)
  ))

# Exportar cada plot primero
ggsave("processed_data/f2a.png", f2a, width = 10, height = 8, dpi = 600)
ggsave("processed_data/f2b.png", f2b, width = 10, height = 8, dpi = 600)

# Leerlos con magick
img_a <- image_read("processed_data/f2a.png")
img_b <- image_read("processed_data/f2b.png")

fig1.a_ter <- image_append(c(img_a,img_b))
image_write(fig1.a_ter, "processed_data/fig1.a_ter.png")

#How to obtain the data for the table

mark_points_a_out <- mark_points_a %>%
  mutate(across(c(MVPA, LPA, SB, Sleep), ~ .x * 1440, .names = "{col}_min"),
         across(c(MVPA, LPA, SB, Sleep), ~ .x * 100, .names = "{col}_pct"))
mark_points_a_out


mark_points_b_out <- mark_points_b %>%
  mutate(across(c(PA, Sleep, SB), ~ .x * 1440, .names = "{col}_min"),
         across(c(PA, Sleep, SB), ~ .x * 100, .names = "{col}_pct"))
mark_points_b_out

## Fig 1.B - Ternary plot CT ------------------------------------------------

colors_groups <- c("Higher thickness/volume" = "#6A3D9A",
                   "Lower thickness/volume"  = "#009E73")
# colors to use 
colors <- c(
  "#6A3D9A",  # morado fuerte
  "#009E73",
  "#D55E00",
  "#0072B2"
)

# theme of the plot
ternary_theme = ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white"),
                               panel.background = ggplot2::element_rect(fill = "grey75"),
                               text = ggplot2::element_text(size = 15, face = 2,
                                                            colour = "black"),
                               axis.text = ggplot2::element_text(size = 15,
                                                                 face = 2, colour = "black"),
                               line = ggplot2::element_line(linewidth = 1, colour = "black"))

# --- Preparación datos A ---
ternary_data_a_high <- as.data.frame(compositions::clo(high_ct[, compo1], total = 1440) / 1440)
colnames(ternary_data_a_high) <- c("MVPA","LPA","SB","Sleep")
ternary_data_a_high$Group <- "Higher thickness/volume"
ternary_data_a_low <- as.data.frame(compositions::clo(low_ct[, compo1], total = 1440) / 1440)
colnames(ternary_data_a_low) <- c("MVPA","LPA","SB","Sleep")
ternary_data_a_low$Group <- "Lower thickness/volume"

# Unir ambos
ternary_data_a <- rbind(ternary_data_a_high, ternary_data_a_low)

# Calcular centroides
gm <- function(x) exp(mean(log(x), na.rm = TRUE))

mark_points_a_high <- compositions::clo(apply(high_ct[, compo1], 2, gm), total = 1440) / 1440
mark_points_a_low  <- compositions::clo(apply(low_ct[, compo1], 2, gm), total = 1440) / 1440

mark_points_a <- rbind(
  data.frame(t(mark_points_a_high), Group = "Higher thickness/volume"),
  data.frame(t(mark_points_a_low),  Group = "Lower thickness/volume")
)
colnames(mark_points_a)[1:4] <- c("MVPA","LPA","SB","Sleep")

# --- Plot A ---
colors_groups <- c("Higher thickness/volume" = "#6A3D9A", "Lower thickness/volume" = "#009E73")

f2a <- ggtern(data = ternary_data_a,
              aes(x = MVPA, y = LPA, z = SB, color = Group, fill = Group)) +
  stat_ellipse(level = 0.95, alpha = 0.2) +
  stat_density_tern(geom = "polygon", bins = 4, alpha = 0.4, bdl = 0.010) +
  geom_point(data = mark_points_a,
             aes(x = MVPA, y = LPA, z = SB, color = Group),
             linewidth = 1.2) +
  scale_color_manual(
    values = colors_groups,
    labels = c(
      "<b>Higher thickness/volume</b><br><b>MVPA:</b> 2.10% (30.27 min)<br><b>LPA:</b> 17.33% (249.56 min)<br><b>SB:</b> 48.69% (701.16 min)",
      
      "<b>Lower thickness/volume</b><br><b>MVPA:</b> 2.51% (36.26 min)<br><b>LPA:</b> 17.30% (249.16 min)<br><b>SB:</b> 48.96% (705.04 min)"
    )
  ) +
  
  scale_fill_manual(
    values = colors_groups,
    labels = c(
      "<b>Higher thickness/volume</b><br><b>MVPA:</b> 2.10% (30.27 min)<br><b>LPA:</b> 17.33% (249.56 min)<br><b>SB:</b> 48.69% (701.16 min)",
      
      "<b>Lower thickness/volume</b><br><b>MVPA:</b> 2.51% (36.26 min)<br><b>LPA:</b> 17.30% (249.16 min)<br><b>SB:</b> 48.96% (705.04 min)"
    )
  ) +
  theme(
    legend.position = c(0.95, 0.95),   # posición dentro del plot (x, y) de 0 a 1
    legend.justification = c("right", "top"), # qué esquina de la leyenda se usa como referencia
    legend.text.align = 0,             # alineación del texto a la izquierda
    legend.key.height = unit(2, "lines"),
    legend.text = ggtext::element_markdown(
      face = "plain")# altura de cada clave para los textos largos
  ) +
  ternary_theme +
  theme_showarrows() +
  labs(x = "", xarrow = "MVPA (%)",
       y = "", yarrow = "LPA (%)",
       z = "", zarrow = "SB (%)") +
  ggtitle("B.Ternary plots for thickness/volume signature") + 
  theme(plot.title = element_text(face = "bold", size = 16,
                                  margin = margin(t = 20, r = 0, b = 10, l = 0)))
# --- Preparación datos B ---
high_ct$totalPA <- rowSums(high_ct[, c("dur_day_mvpa_bts_1_min_wei", "dur_day_total_lig_mvpa1_min_wei")])
low_ct$totalPA  <- rowSums(low_ct[,  c("dur_day_mvpa_bts_1_min_wei", "dur_day_total_lig_mvpa1_min_wei")])

# 1. Crear dataframe inicial de 3 partes
ternary_data_b_high <- as.data.frame(high_ct[, c("totalPA","dur_day_total_in_min_wei","dur_spt_min_wei")])
colnames(ternary_data_b_high) <- c("PA","SB","Sleep")

ternary_data_b_low <- as.data.frame(low_ct[, c("totalPA","dur_day_total_in_min_wei","dur_spt_min_wei")])
colnames(ternary_data_b_low) <- c("PA","SB","Sleep")

# 2. Re-cerrar las composiciones a 1440 / 1 (igual que tu compañero)
ternary_data_b_high <- as.data.frame(compositions::clo(ternary_data_b_high[, c("PA","SB","Sleep")], total = 1440) / 1440)
ternary_data_b_high$Group <- "Higher thickness/volume"

ternary_data_b_low  <- as.data.frame(compositions::clo(ternary_data_b_low[, c("PA","SB","Sleep")], total = 1440) / 1440)
ternary_data_b_low$Group <- "Lower thickness/volume"

ternary_data_b <- rbind(ternary_data_b_high, ternary_data_b_low)

# Centroides
mark_points_b_high <- compositions::clo(apply(ternary_data_b_high[,1:3], 2, gm), total = 1440) / 1440
mark_points_b_low  <- compositions::clo(apply(ternary_data_b_low[,1:3], 2, gm), total = 1440) / 1440

mark_points_b <- rbind(
  data.frame(t(mark_points_b_high), Group = "Higher thickness/volume"),
  data.frame(t(mark_points_b_low),  Group = "Lower thickness/volume")
)

# --- Plot B ---
f2b <- ggtern(data = ternary_data_b,
              aes(x = PA, y = Sleep, z = SB, color = Group, fill = Group)) +
  stat_ellipse(level = 0.95, alpha = 0.2) +
  stat_density_tern(geom = "polygon", bins = 4, alpha = 0.4, bdl = 0.010) +
  geom_point(data = mark_points_b,
             aes(x = PA, y = Sleep, z = SB, color = Group),
             linewidth = 1.2) +
  scale_color_manual(
    values = colors_groups,
    labels = c(
      "<b>Higher thickness/volume</b><br><b>PA:</b> 19.43% (279.83 min)<br><b>Sleep:</b> 31.88% (459.01 min)<br><b>SB:</b> 48.69% (701.16 min)",
      "<b>Lower thickness/volume</b><br><b>PA:</b> 19.82% (285.42 min)<br><b>Sleep:</b> 31.22% (449.54 min)<br><b>SB:</b> 48.96% (705.04 min)"
    )
  ) +
  
  scale_fill_manual(
    values = colors_groups,
    labels = c(
      "<b>Higher thickness/volume</b><br><b>PA:</b> 19.43% (279.83 min)<br><b>Sleep:</b> 31.88% (459.01 min)<br><b>SB:</b> 48.69% (701.16 min)",
      "<b>Lower thickness/volume</b><br><b>PA:</b> 19.82% (285.42 min)<br><b>Sleep:</b> 31.22% (449.54 min)<br><b>SB:</b> 48.96% (705.04 min)"
    )
  ) +
  theme(
    legend.position = c(0.95, 0.95),   # posición dentro del plot (x, y) de 0 a 1
    legend.justification = c("right", "top"), # qué esquina de la leyenda se usa como referencia
    legend.text.align = 0,             # alineación del texto a la izquierda
    legend.key.height = unit(2, "lines"),
    legend.text = ggtext::element_markdown(
      face = "plain")# altura de cada clave para los textos largos
  ) +
  ternary_theme +
  theme_showarrows() +
  labs(x = "", xarrow = "PA (%)",
       y = "", yarrow = "Sleep (%)",
       z = "", zarrow = "SB (%)") +
  ggtitle(" ") +
  theme(plot.title = element_text(
    face = "bold",
    size = 16,
    margin = margin(t = 20, b = 10)
  ))


# Exportar cada plot primero
ggsave("processed_data/f2a.png", f2a, width = 10, height = 8, dpi = 600)
ggsave("processed_data/f2b.png", f2b, width = 10, height = 8, dpi = 600)

# Leerlos con magick
img_a <- image_read("processed_data/f2a.png")
img_b <- image_read("processed_data/f2b.png")

fig1.b_ter <- image_append(c(img_a, img_b))
image_write(fig1.b_ter, "processed_data/fig1.b_ter.png")


#How to obtain the data for the table
mark_points_a_out <- mark_points_a %>%
  mutate(across(c(MVPA, LPA, SB, Sleep), ~ .x * 1440, .names = "{col}_min"),
         across(c(MVPA, LPA, SB, Sleep), ~ .x * 100, .names = "{col}_pct"))
mark_points_a_out


mark_points_b_out <- mark_points_b %>%
  mutate(across(c(PA, Sleep, SB), ~ .x * 1440, .names = "{col}_min"),
         across(c(PA, Sleep, SB), ~ .x * 100, .names = "{col}_pct"))
mark_points_b_out


library(magick)

# Leer imágenes exportadas
img_a <- image_read("processed_data/fig1.a_ter.png")
img_b <- image_read("processed_data/fig1.b_ter.png")


# Unir horizontalmente
fig1_combined <- image_append(c(img_a, img_b), stack = TRUE)

# Guardar el resultado
image_write(fig1_combined, "processed_data/fig1_combined.png")


## Fig 2. 24-h behaviour + Cognition  --------------------------------------------------------------
# Paleta base de tus otras figuras
base_colors <- c("#6A3D9A", "#009E73", "#D55E00", "#0072B2")

# Generar 7 colores únicos a partir de la paleta base
extended_colors <- colorRampPalette(base_colors)(7)

# Asignar un color distinto a cada outcome
colors <- c(
  "Working memory" = extended_colors[3],
  "Visuospatial processing" = extended_colors[4],
  "Processing speed" = extended_colors[5],
  "Episodic memory" = extended_colors[6],
  "Attentional/inhibitory control" = extended_colors[7]
)


# Combine multiple dataa frames into a single dataa frame
lista_dataframes <- list(table_figure2)
table_plot_main <- bind_rows(lista_dataframes)
# Filter for specific outcomes
table_plot_main <- table_plot_main %>%
  mutate(
    outcome = case_when(
      outcome == "attentional_control_mean_z" ~ "Attentional/inhibitory control",
      outcome == "episodic_mem_mean_z" ~ "Episodic memory",
      outcome == "processing_speed_mean_z" ~ "Processing speed",
      outcome == "visuospatial_mean_z" ~ "Visuospatial processing",
      outcome == "working_mem_mean_z" ~ "Working memory",
      TRUE ~ outcome
    ),
    Behavior = case_when(
      Behavior == "Moderate-to-Vigorous Physical Activity" ~ "MVPA (ilr)",
      Behavior == "Light Physical Activity" ~ "LPA (ilr)",
      Behavior == "Sedentary Behavior" ~ "Sedentary behavior (ilr)",
      Behavior == "Sleep" ~ "Sleep (ilr)",
      TRUE ~ Behavior
    )
  )


# Set factor levels for outcome, time, and type
table_plot_main$outcome <- factor(table_plot_main$outcome, levels = c(
  "Working memory","Visuospatial processing","Processing speed",
  "Episodic memory","Attentional/inhibitory control"))
table_plot_main$Behavior <- factor(table_plot_main$Behavior, levels =c("MVPA (ilr)","LPA (ilr)","Sedentary behavior (ilr)","Sleep (ilr)"))



#table_plot_main$type_high_low <- factor(table_plot_main$type_high_low, levels = c("Lower","Higher"))
#table_plot_main$type2 <- factor
# Define colors for plot

p1 <- ggplot(table_plot_main, aes(x = beta, y = outcome)) +
  geom_point(aes(color = outcome),
             position = position_dodge(width = 0.4), size = 1) +# Add points to the plot with colors based on 'type2', adjusting position to avoid overlap
  geom_errorbar(aes(color = outcome, x = beta,  
                    xmin = lowerCI, xmax = upperCI), width = 0, position = position_dodge(width = 0.8)) +# Add error bars to the points, with positions adjusted similarly to the points
  facet_wrap(~Behavior, ncol = 10, scales = "free_x") + # Create separate panels for each moderator, with free x-axis scales
  scale_y_discrete(expand = expansion(mult = c(0, 0))) + 
  theme_bw() +# Use a white background theme
  theme(
    panel.grid = element_blank(), # Customize theme elements
    text = element_text(family = "Arial", color = "black"),  # Set text properties
    legend.position = "none",  # Position legend at the bottom
    legend.title = element_blank(),  # Remove legend title
    legend.direction = "horizontal",       # ← AQUÍ CAMBIADO
    legend.box = "horizontal",           
    #legend.background = element_rect(fill='transparent'),  # Commented out: Transparent legend background
    #legend.box.background = element_rect(fill='transparent'),  # Commented out: Transparent legend box background
    strip.text = element_text(size = 8, face = "bold"),  # Set facet strip text properties
    strip.text.x = element_text(size = 8, face = "bold"),  # Set x facet strip text properties
    strip.text.y = element_text(size = 0, face = "bold"),  # Set y facet strip text properties
    legend.text = element_text(size = 8),  # Set legend text size
    axis.text.x = element_text(size = 8),  # Set x-axis text size
    axis.title.x = element_text(size = 8),  # Set x-axis title size
    axis.text.y = element_text(size = 8, margin = margin(t = 0, b = 0)),
    plot.caption = element_text(size = 8),  # Set plot caption text size
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0),  # Set legend margin
    legend.spacing.y = unit(-0.2, "cm"),  # Set y-axis legend spacing
    plot.margin = unit(c(0.8, 0.8, 0.8, 0.8), "mm"),
    panel.spacing.y = unit(0, "lines")) + # Set plot margins
  labs(x = "Effect size (Beta)", y = NULL) +  # Set axis labels
  geom_text(aes(label = b_ci,
                color = outcome),, hjust = 0.4, vjust = -0.9, size = 2.5, 
            position = position_dodge2(width = 0.8, preserve = "single"), show.legend = FALSE) + # Add text labels showing the 'change' values
  geom_text(aes(label = paste0("p = ", format(round(p_value, digits = 2))), color = outcome),
            hjust = 0.4, vjust = 1.6, size = 2,
            position = position_dodge2(width = 0.8, preserve = "single"), show.legend = FALSE)  + # Add text labels showing the p-values for 24 weeks
  scale_color_manual( values = colors) + # Manually set colors and labels for the legend
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", alpha = 0.5) +  # Add a dashed vertical line at x = 0
  guides(colour = guide_legend(reverse = TRUE)) # Customize the legend guide

dev.off()  # Turn off the current graphical device
print(p1)  

png("processed_data/fig2_all.png", units = "in", width = 12, height = 5, res = 300)
grid::grid.draw(cbind(ggplotGrob(p1),size = "last"))
dev.off()                   


## Fig 3. Subgroups --------------------------------------------------------------

# Combine multiple dataa frames into a single data frame
table_figure3 <- table_subgroup_plot_gmd %>%
  right_join(
    dplyr::select(as.data.frame(table_interactions_plot_gmd), outcome, Behavior, p_value_interaction),
    by = c("outcome", "Behavior")
  )

data_long <- bind_rows(table_figure3)

# Filter for specific outcomes
data_long <- data_long %>%
  mutate(
    outcome = case_when(
      outcome == "attentional_control_mean_z" ~ "Attentional/inhibitory control",
      outcome == "episodic_mem_mean_z" ~ "Episodic memory",
      outcome == "processing_speed_mean_z" ~ "Processing speed",
      outcome == "visuospatial_mean_z" ~ "Visuospatial processing",
      outcome == "working_mem_mean_z" ~ "Working memory",
      TRUE ~ outcome
    ),
    moderator = case_when(
      moderator == "md" ~ "Grey matter mean diffusivity signature",
      TRUE ~ moderator
    ),
    Behavior = case_when(
      Behavior == "Moderate-to-Vigorous Physical Activity" ~ "MVPA (ilr)",
      Behavior == "Light Physical Activity" ~ "LPA (ilr)",
      Behavior == "Sedentary Behavior" ~ "Sedentary behavior (ilr)",
      Behavior == "Sleep" ~ "Sleep (ilr)",
      TRUE ~ Behavior
    ),
    subgroup = case_when(
      subgroup  == "high" ~ "Higher GMMD",
      subgroup  == "low" ~ "Lower GMMD",
      TRUE ~ subgroup
    ))

data_long <- data_long %>%
  mutate(
    p_value_interaction = fmt(p_value_interaction)
  )

# Set factor levels for outcome, time, and type

data_long$outcome <- factor(data_long$outcome, levels = c("Working memory","Visuospatial processing", "Processing speed","Episodic memory","Attentional/inhibitory control"))
data_long$Behavior <- factor(data_long$Behavior, levels =c("MVPA (ilr)","LPA (ilr)","Sedentary behavior (ilr)","Sleep (ilr)"))
data_long$moderator <- factor(data_long$moderator, levels = c("Grey matter mean diffusivity signature"))


#data_long$type_high_low <- factor(data_long$type_high_low, levels = c("Lower","Higher"))
#data_long$type2 <- factor
data_long$group <- base::interaction(data_long$subgroup, data_long$moderator)

# Combine multiple dataa frames into a single dataa frame
data_long <- data_long %>%
  mutate(
    Behavior_mod = paste(Behavior, "\n(", moderator, ")", sep = "")
  )
Behavior_mod <- c("Sleep","Sedentary behavior","LPA","MVPA")
mod_levels <- c("AD grey matter mean diffusivity signature")
beh_levels <- c("Sleep","Sedentary behavior","LPA","MVPA")


rects <- data.frame(
  ymin = c(0.5, 2.5, 4.5),  # Sombreado del fondo
  ymax = c(1.5, 3.5,5.5),  
  xmin = -Inf,
  xmax = Inf
)
# Create the first plot for the 'Sex' moderator
p1 <- ggplot(data_long, aes(x = beta, y = outcome)) +
  geom_rect(data = rects,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            inherit.aes = FALSE,
            fill = "grey90", alpha = 0.5) + 
  geom_point(aes( color = subgroup),
             position = position_dodge(width = 0.8), size = 1) +# Add points to the plot with colors based on 'type2', adjusting position to avoid overlap
  geom_errorbar(aes(color = subgroup, x = beta,  
                    xmin = lowerCI, xmax = upperCI), width = 0, position = position_dodge(width = 0.8)) +# Add error bars to the points, with positions adjusted similarly to the points
  facet_grid(~Behavior, scales = "free_x" ) + # Create separate panels for each moderator, with free x-axis scales
  theme_bw() +
  theme(
    panel.grid = element_blank()) +   # elimina todas las cuadrículas +# Use a white background theme
  theme( # Customize theme elements
    text = element_text(family = "Arial", color = "black"),  # Set text properties
    legend.position = "bottom",  # Position legend at the bottom
    legend.title = element_blank(),  # Remove legend title
    legend.direction = "horizontal",  # Make legend vertical
    #legend.background = element_rect(fill='transparent'),  # Commented out: Transparent legend background
    #legend.box.background = element_rect(fill='transparent'),  # Commented out: Transparent legend box background
    strip.text = element_text(size = 8, face = "bold"),  # Set facet strip text properties
    strip.text.x = element_text(size = 8, face = "bold"),  # Set x facet strip text properties
    strip.text.y = element_text(size = 8, face = "bold"),  # Set y facet strip text properties
    legend.text = element_text(size = 8),  # Set legend text size
    axis.text.x = element_text(size = 8),  # Set x-axis text size
    axis.title.x = element_text(size = 8),  # Set x-axis title size
    axis.text.y = element_text(size = 7),  # Set y-axis text size
    plot.caption = element_text(size = 7),  # Set plot caption text size
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0),  # Set legend margin
    legend.spacing.y = unit(-0.2, "cm"),  # Set y-axis legend spacing
    plot.margin = unit(c(0.8, 0.8, 0.8, 0.8), "mm") ) + # Set plot margins
  labs(x = "Effect size (Beta)", y = NULL) +  # Set axis labels
  geom_text(aes(label = b_ci,
                color = subgroup), hjust = 0.4, vjust = -0.9, size = 2.5, 
            position = position_dodge(width = 0.8), show.legend = FALSE) + # Add text labels showing the 'change' values
  geom_text(aes(label = paste0("p = ", format(round(p_value, digits = 2))), color = subgroup),
            hjust = -0.1, vjust = 1.2, size = 2,
            position = position_dodge(width = 0.8), show.legend = FALSE) +
  geom_text(aes(
    x = mean(beta, na.rm = TRUE),
    y = outcome,
    label = ifelse(
      p_value_interaction < 0.05,
      paste0("p for int = ", p_value_interaction, " **"),
      paste0("p for int = ", p_value_interaction)
    )
  ),
  inherit.aes = FALSE,
  size = 2.5,
  vjust = 0.5,
  color = "grey40") +
  #scale_color_manual(labels = c("MVPA","LPA","Sedentary behavior", "Sleep"), values = colors) + # Manually set colors and labels for the legend
  scale_color_manual(
    values = c(
      "Higher GMMD" = "#6A3D9A",   # naranja
      "Lower GMMD"  = "#009E73"    
    ),
    labels = c("Higher GMMD", "Lower GMMD")) + # Manually set colors and labels for the legend
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", alpha = 0.5) +  # Add a dashed vertical line at x = 0
  guides(colour = guide_legend(reverse = TRUE)) # Customize the legend guide

dev.off()  # Turn off the current graphical device
print(p1)  

png("processed_data/fig3_moderators.png", units = "in", width = 12, height = 7, res = 300)
p1
dev.off()    


#2. Supplemtary material ----
## Fig S1. Correlation plot --------------------------------------------------------------

data_corr = data %>% dplyr:: select(c(attentional_control_mean_z,episodic_mem_mean_z, processing_speed_mean_z,
                                      visuospatial_mean_z, working_mem_mean_z ,ADsig_Williams2021_gmmd_score_z,
                                      ADsig_Williams2021_thick_vol_score_z ))



colnames(data_corr)=c("Attentional/inhibitory control","Episodic memory", "Processing speed","Visuospatial processing",
                      "Working memory",  "Gray matter mean diffusivity signature",
                      "Thickness/volume signature"  )

cormat_data_all <- round(cor(data_corr),2)
correlacion = rcorr(as.matrix(data_corr), type = "pearson")
R.correlacion = correlacion$r
p.correlacion = correlacion$P
tiff("processed_data/fig_correlation.tiff", units="in", width=35, height=30, res=250)
corrplot(R.correlacion,tl.col= "black", tl.srt=60,number.cex = 3,
         type= "lower", method="color", tl.cex = 3, addCoef.col = "black",
         cl.cex = 3) 
dev.off()
correlacion$P

##Fig S2. Reallocation plots ----
# Plot the beta coefficient for significant association, the plot will be divided into 4 parts in which each part has a reallocation plot with males and females trend line  
# the reallocation plots show the reallocation time in one behaviors with the others proportionally (1 vs others, example: increase MVPA and decrease proportionally the others) 
# and per pair (1 vs 1, example: increase MVPA and decrease LPA)

covs2 = c("screen_gender", "screen_years_edu")
compo1 = c("dur_day_mvpa_bts_1_min_wei", "dur_day_total_lig_mvpa1_min_wei",
           "dur_day_total_in_min_wei", "dur_spt_min_wei")

outcomes = c("episodic_mem_mean_z", "processing_speed_mean_z",
             "working_mem_mean_z" ,"attentional_control_mean_z", "visuospatial_mean_z")

outnames = c( "Episodic memory", "Processing speed", "Working memory",
              "Attentional control", "Visuospatial processing")

#Fig- Attentional_control  y LPA (propotional reallocations) ----
dev.new()

#save image
png("processed_data/figAC.png",height = 2.6, width = 10,
    res = 600, units = "in")

layout(matrix(1:4, nrow = 1, ncol = 4, byrow = TRUE))
par(oma = c(0.5, 0.5, 0.5, 0.5))# Márgenes exteriores para que haya espacio arriba
par(mar = c(4, 4, 2, 1))  # Márgenes internos entre gráficos
par(cex.axis = 0.8, cex.lab = 1,cex.main = 1)  # Tamaño de texto

outcomes = c("attentional_control_mean_z")
behavior_increase = "LPA"



for (i in 1:length(outcomes)) {
  
  x= myfunctions::reallocationPlot(data = high_md,      
                                   comps = compo1,
                                   comps.names = c("MVPA", "LPA", "SB", "Sleep"),
                                   outcome = outcomes[i],
                                   covs = covs2,
                                   comparisons = "prop-realloc",
                                   increase = behavior_increase[i],
                                   xlim = c(-5,30),
                                   ylim = c(-0.5, 0.5),
                                   ylab = "Change in z-score",
                                   xlab = "Min/day proportionally reallocated of LPA",
                                   main = "Proportional reallocation plot of LPA \n with other behaviours",
                                   col = colors[1])
  bY = summary(x$LPA)$coefficients["LPA_MVPA.SB.Sleep", "Estimate"]
  pY = summary(x$LPA)$coefficients["LPA_MVPA.SB.Sleep", "Pr(>|t|)"]
  
  par(new = TRUE)
  
  x= myfunctions::reallocationPlot(data = low_md,
                                   comps = compo1,
                                   comps.names = c("MVPA", "LPA", "SB", "Sleep"),
                                   outcome = outcomes[i],
                                   covs = covs2,
                                   comparisons = "prop-realloc",
                                   increase = behavior_increase[i],
                                   xlim = c(-5,30),
                                   ylim = c(-0.5, 0.5),
                                   ylab = "",
                                   xlab = "",
                                   col = colors[2])
  bO = summary(x$LPA)$coefficients["LPA_MVPA.SB.Sleep", "Estimate"]
  pO = summary(x$LPA)$coefficients["LPA_MVPA.SB.Sleep", "Pr(>|t|)"]
  
  # text p values
  legend(
    "topleft",
    legend = c(
      paste0("β = ", round(bY, 2), ", p = ", format(round(pY, 2), digits = 2)),
      paste0("β = ", round(bO, 2), ", p = ", format(round(pO, 2), digits = 2))
    ),
    text.col = colors[1:2],
    bty = "n"
  )
}

##Increase LPA, decrease MVPA (1-to-1)----
for (i in 1:length(outcomes)) {
  
  x= myfunctions::reallocationPlot(data = high_md,      
                                   comps = compo1,
                                   comps.names = c("MVPA", "LPA", "SB", "Sleep"),
                                   outcome = outcomes[i],
                                   covs = covs2,
                                   comparisons = c("one-v-one"),
                                   increase = behavior_increase[i],
                                   decrease = "MVPA",
                                   xlim = c(-5,25),
                                   ylim = c(-0.5, 0.5),
                                   ylab = "Change in z-score",
                                   xlab = "\u2193 MVPA \u2192 \u2191 LPA",
                                   main = "Reallocation plot increasing \n LPA and decreasing MVPA",
                                   col = colors[1])
  summary(x)
  #pY = summary(x$LPA)$coefficients["LPA_MVPA", "Pr(>|t|)"]
  par(new = TRUE)
  
  x= myfunctions::reallocationPlot(data = low_md,
                                   comps = compo1,
                                   comps.names = c("MVPA", "LPA", "SB", "Sleep"),
                                   outcome = outcomes[i],
                                   covs = covs2,
                                   comparisons = c("one-v-one"),
                                   increase = behavior_increase[i],
                                   decrease = "MVPA",
                                   xlim = c(-5,25),
                                   ylim = c(-0.5, 0.5),
                                   ylab = "",
                                   xlab = "\u2193 MVPA \u2192 \u2191 LPA",
                                   col = colors[2])
  coef_table <- summary(x$LPA)$coefficients
  
  #pY <- coef_table[grep("LPA_MVPA", rownames(coef_table)), "Pr(>|t|)"]
  
  #pY = summary(x$LPA)$coefficients["LPA_MVPA", "Pr(>|t|)"]

}

rownames(summary(x$MVPA)$coefficients)
summary(x$MVPA)$coefficients

##Increase LPA, decrease SB (1-to-1)----

for (i in 1:length(outcomes)) {
  
  x= myfunctions::reallocationPlot(data = high_md,      
                                   comps = compo1,
                                   comps.names = c("MVPA", "LPA", "SB", "Sleep"),
                                   outcome = outcomes[i], covs = covs2,
                                   comparisons = c("one-v-one"),
                                   increase = behavior_increase[i],
                                   decrease = "SB", 
                                   xlim = c(-5,30),
                                   ylim = c(-0.5, 0.5),
                                   ylab = "Change in zScore",
                                   xlab = "\u2193 SB \u2192 \u2191 LPA",
                                   main = "Reallocation plot increasing \n LPA and decreasing SB",
                                   col = colors[1])
  #pY = summary(x$SB)$coefficients[1, "Pr(>|t|)"]
  par(new = TRUE)
  
  x= myfunctions::reallocationPlot(data = low_md,
                                   comps = compo1,
                                   comps.names = c("MVPA", "LPA", "SB", "Sleep"),
                                   outcome = outcomes[i],
                                   covs = covs2,
                                   comparisons = c("one-v-one"),
                                   increase = behavior_increase[i],
                                   decrease = "SB",
                                   xlim = c(-5,30),
                                   ylim = c(-0.5, 0.5),
                                   ylab = "",
                                   xlab = "\u2193 SB \u2192 \u2191 LPA",
                                   col = colors[2])
  
  
  
}
##Increase LPA, decrease SLEEP (1-to-1)----
for (i in 1:length(outcomes)) {
  
  x= myfunctions::reallocationPlot(data = high_md,      
                                   comps = compo1,
                                   comps.names = c("MVPA", "LPA", "SB", "Sleep"),
                                   outcome = outcomes[i], 
                                   covs = covs2,
                                   comparisons = c("one-v-one"),
                                   increase = behavior_increase[i],
                                   decrease = "Sleep",
                                   xlim = c(-5,30),
                                   ylim = c(-0.5, 0.5),
                                   ylab = "Change in zScore",
                                   xlab = "\u2193 Sleep \u2192 \u2191 LPA",
                                   main = "Reallocation plot increasing \nLPA and decreasing sleep",
                                   col = colors[1])
  
  par(new = TRUE)
  
  x= myfunctions::reallocationPlot(data = low_md,
                                   comps = compo1,
                                   comps.names = c("MVPA", "LPA", "SB", "Sleep"),
                                   outcome = outcomes[i],
                                   covs = covs2,
                                   comparisons = c("one-v-one"),
                                   increase = behavior_increase[i],
                                   decrease = "Sleep",
                                   xlim = c(-5,30),
                                   ylim = c(-0.5, 0.5),
                                   ylab = "",
                                   xlab = "\u2193 Sleep \u2192 \u2191 LPA",
                                   col = colors[2])
  

}


dev.off()

#Fig- Processing speed  y LPA (propotional reallocations) ----
dev.new()

png("processed_data/figPS.png", height = 2.6, width = 10,
    res = 600, units = "in")

layout(matrix(1:4, nrow = 1, ncol = 4, byrow = TRUE))
par(oma = c(0.5, 0.5, 0.5, 0.5))# Márgenes exteriores para que haya espacio arriba
par(mar = c(4, 4, 2, 1))  # Márgenes internos entre gráficos
par(cex.axis = 0.8, cex.lab = 1,cex.main = 1)  # Tamaño de texto

outcomes = c("processing_speed_mean_z")
behavior_increase = "LPA"


for (i in 1:length(outcomes)) {
  
  x= myfunctions::reallocationPlot(data = high_md,      
                                   comps = compo1,
                                   comps.names = c("MVPA", "LPA", "SB", "Sleep"),
                                   outcome = outcomes[i],
                                   covs = covs2,
                                   comparisons = "prop-realloc",
                                   increase = behavior_increase[i],
                                   xlim = c(-5,30),
                                   ylim = c(-0.5, 0.5),
                                   ylab = "Change in z-score",
                                   xlab = "Min/day proportionally reallocated of LPA",
                                   main = "Proportional reallocation plot of LPA \n with other behaviours",
                                   col = colors[1])
  bY = summary(x$LPA)$coefficients["LPA_MVPA.SB.Sleep", "Estimate"]
  pY = summary(x$LPA)$coefficients["LPA_MVPA.SB.Sleep", "Pr(>|t|)"]
  
  par(new = TRUE)
  
  x= myfunctions::reallocationPlot(data = low_md,
                                   comps = compo1,
                                   comps.names = c("MVPA", "LPA", "SB", "Sleep"),
                                   outcome = outcomes[i],
                                   covs = covs2,
                                   comparisons = "prop-realloc",
                                   increase = behavior_increase[i],
                                   xlim = c(-5,30),
                                   ylim = c(-0.5, 0.5),
                                   ylab = "",
                                   xlab = "",
                                   col = colors[2])
  bO = summary(x$LPA)$coefficients["LPA_MVPA.SB.Sleep", "Estimate"]
  pO = summary(x$LPA)$coefficients["LPA_MVPA.SB.Sleep", "Pr(>|t|)"]
  
  # text p values
  legend(
    "topleft",
    legend = c(
      paste0("β = ", round(bY, 2), ", p = ", format(round(pY, 2), digits = 2)),
      paste0("β = ", round(bO, 2), ", p = ", format(round(pO, 2), digits = 2))
    ),
    text.col = colors[1:2],
    bty = "n"
  )
}


##Increase LPA, decrease MVPA (1-to-1)----
for (i in 1:length(outcomes)) {
  
  x= myfunctions::reallocationPlot(data = high_md,      
                                   comps = compo1,
                                   comps.names = c("MVPA", "LPA", "SB", "Sleep"),
                                   outcome = outcomes[i],
                                   covs = covs2,
                                   comparisons = c("one-v-one"),
                                   increase = behavior_increase[i],
                                   decrease = "MVPA",# EN ESTE CASO HA SALIDO SIGNIFICATIVO EL SUENO ENTONCES LO PONEMOS COMO INCREASE
                                   xlim = c(-5,25),
                                   ylim = c(-0.5, 0.5),
                                   ylab = "Change in z-score",
                                   xlab = "\u2193 MVPA \u2192 \u2191 LPA",
                                   main = "Reallocation plot increasing \n LPA and decreasing MVPA",
                                   col = colors[1])
  
  par(new = TRUE)
  
  x= myfunctions::reallocationPlot(data = low_md,
                                   comps = compo1,
                                   comps.names = c("MVPA", "LPA", "SB", "Sleep"),
                                   outcome = outcomes[i],
                                   covs = covs2,
                                   comparisons = c("one-v-one"),
                                   increase = behavior_increase[i],
                                   decrease = "MVPA",
                                   xlim = c(-5,25),
                                   ylim = c(-0.5, 0.5),
                                   ylab = "",
                                   xlab = "\u2193 MVPA \u2192 \u2191 LPA",
                                   col = colors[2])
  
  
}

##Increase LPA, decrease SB (1-to-1)----

for (i in 1:length(outcomes)) {
  
  x= myfunctions::reallocationPlot(data = high_md,      
                                   comps = compo1,
                                   comps.names = c("MVPA", "LPA", "SB", "Sleep"),
                                   outcome = outcomes[i], covs = covs2,
                                   comparisons = c("one-v-one"),
                                   increase = behavior_increase[i],
                                   decrease = "SB", 
                                   xlim = c(-5,30),
                                   ylim = c(-0.5, 0.5),
                                   ylab = "Change in zScore",
                                   xlab = "\u2193 SB \u2192 \u2191 LPA",
                                   main = "Reallocation plot increasing \n LPA and decreasing SB",
                                   col = colors[1])
  #pY = summary(x$SB)$coefficients[1, "Pr(>|t|)"]
  par(new = TRUE)
  
  x= myfunctions::reallocationPlot(data = low_md,
                                   comps = compo1,
                                   comps.names = c("MVPA", "LPA", "SB", "Sleep"),
                                   outcome = outcomes[i],
                                   covs = covs2,
                                   comparisons = c("one-v-one"),
                                   increase = behavior_increase[i],
                                   decrease = "SB",
                                   xlim = c(-5,30),
                                   ylim = c(-0.5, 0.5),
                                   ylab = "",
                                   xlab = "\u2193 SB \u2192 \u2191 LPA",
                                   col = colors[2])
  

}

##Increase LPA, decrease SLEEP (1-to-1)----
for (i in 1:length(outcomes)) {
  
  x= myfunctions::reallocationPlot(data = high_md,      
                                   comps = compo1,
                                   comps.names = c("MVPA", "LPA", "SB", "Sleep"),
                                   outcome = outcomes[i], 
                                   covs = covs2,
                                   comparisons = c("one-v-one"),
                                   increase = behavior_increase[i],
                                   decrease = "Sleep",
                                   xlim = c(-5,30),
                                   ylim = c(-0.5, 0.5),
                                   ylab = "Change in zScore",
                                   xlab = "\u2193 Sleep \u2192 \u2191 LPA",
                                   main = "Reallocation plot increasing \nLPA and decreasing sleep",
                                   col = colors[1])
  
  par(new = TRUE)
  
  x= myfunctions::reallocationPlot(data = low_md,
                                   comps = compo1,
                                   comps.names = c("MVPA", "LPA", "SB", "Sleep"),
                                   outcome = outcomes[i],
                                   covs = covs2,
                                   comparisons = c("one-v-one"),
                                   increase = behavior_increase[i],
                                   decrease = "Sleep",
                                   xlim = c(-5,30),
                                   ylim = c(-0.5, 0.5),
                                   ylab = "",
                                   xlab = "\u2193 Sleep \u2192 \u2191 LPA",
                                   col = colors[2])
  
}

dev.off()


#Fig- Working memory LPA (propotional reallocations) ----
dev.new()

#save image
png("processed_data/figWM.png", height = 2.6, width = 10,
    res = 600, units = "in")

layout(matrix(1:4, nrow = 1, ncol = 4, byrow = TRUE))
par(oma = c(0.5, 0.5, 0.5, 0.5))# Márgenes exteriores para que haya espacio arriba
par(mar = c(4, 4, 2, 1))  # Márgenes internos entre gráficos
par(cex.axis = 0.8, cex.lab = 1,cex.main = 1)  # Tamaño de texto

outcomes = c("working_mem_mean_z")
behavior_increase = "LPA"



for (i in 1:length(outcomes)) {
  
  x= myfunctions::reallocationPlot(data = high_md,      
                                   comps = compo1,
                                   comps.names = c("MVPA", "LPA", "SB", "Sleep"),
                                   outcome = outcomes[i],
                                   covs = covs2,
                                   comparisons = "prop-realloc",
                                   increase = behavior_increase[i],
                                   xlim = c(-5,30),
                                   ylim = c(-0.5, 0.5),
                                   ylab = "Change in z-score",
                                   xlab = "Min/day proportionally reallocated of LPA",
                                   main = "Proportional reallocation plot of LPA \n with other behaviours",
                                   col = colors[1])
  bY = summary(x$LPA)$coefficients["LPA_MVPA.SB.Sleep", "Estimate"]
  pY = summary(x$LPA)$coefficients["LPA_MVPA.SB.Sleep", "Pr(>|t|)"]
  
  par(new = TRUE)
  
  x= myfunctions::reallocationPlot(data = low_md,
                                   comps = compo1,
                                   comps.names = c("MVPA", "LPA", "SB", "Sleep"),
                                   outcome = outcomes[i],
                                   covs = covs2,
                                   comparisons = "prop-realloc",
                                   increase = behavior_increase[i],
                                   xlim = c(-5,30),
                                   ylim = c(-0.5, 0.5),
                                   ylab = "",
                                   xlab = "",
                                   col = colors[2])
  bO = summary(x$LPA)$coefficients["LPA_MVPA.SB.Sleep", "Estimate"]
  pO = summary(x$LPA)$coefficients["LPA_MVPA.SB.Sleep", "Pr(>|t|)"]
  
  # text p values
  legend(
    "topleft",
    legend = c(
      paste0("β = ", round(bY, 2), ", p = ", format(round(pY, 2), digits = 2)),
      paste0("β = ", round(bO, 2), ", p = ", format(round(pO, 2), digits = 2))
    ),
    text.col = colors[1:2],
    bty = "n"
  )
  
}


##Increase LPA, decrease MVPA (1-to-1)----
for (i in 1:length(outcomes)) {
  
  x= myfunctions::reallocationPlot(data = high_md,      
                                   comps = compo1,
                                   comps.names = c("MVPA", "LPA", "SB", "Sleep"),
                                   outcome = outcomes[i],
                                   covs = covs2,
                                   comparisons = c("one-v-one"),
                                   increase = behavior_increase[i],
                                   decrease = "MVPA",# EN ESTE CASO HA SALIDO SIGNIFICATIVO EL SUENO ENTONCES LO PONEMOS COMO INCREASE
                                   xlim = c(-5,25),
                                   ylim = c(-0.5, 0.5),
                                   ylab = "Change in z-score",
                                   xlab = "\u2193 MVPA \u2192 \u2191 LPA",
                                   main = "Reallocation plot increasing \n LPA and decreasing MVPA",
                                   col = colors[1])
  
  
  par(new = TRUE)
  
  x= myfunctions::reallocationPlot(data = low_md,
                                   comps = compo1,
                                   comps.names = c("MVPA", "LPA", "SB", "Sleep"),
                                   outcome = outcomes[i],
                                   covs = covs2,
                                   comparisons = c("one-v-one"),
                                   increase = behavior_increase[i],
                                   decrease = "MVPA",
                                   xlim = c(-5,25),
                                   ylim = c(-0.5, 0.5),
                                   ylab = "",
                                   xlab = "\u2193 MVPA \u2192 \u2191 LPA",
                                   col = colors[2])
  

}

##Increase LPA, decrease SB (1-to-1)----

for (i in 1:length(outcomes)) {
  
  x= myfunctions::reallocationPlot(data = high_md,      
                                   comps = compo1,
                                   comps.names = c("MVPA", "LPA", "SB", "Sleep"),
                                   outcome = outcomes[i], covs = covs2,
                                   comparisons = c("one-v-one"),
                                   increase = behavior_increase[i],
                                   decrease = "SB", 
                                   xlim = c(-5,30),
                                   ylim = c(-0.5, 0.5),
                                   ylab = "Change in zScore",
                                   xlab = "\u2193 SB \u2192 \u2191 LPA",
                                   main = "Reallocation plot increasing \n LPA and decreasing SB",
                                   col = colors[1])
  #pY = summary(x$SB)$coefficients[1, "Pr(>|t|)"]
  par(new = TRUE)
  
  x= myfunctions::reallocationPlot(data = low_md,
                                   comps = compo1,
                                   comps.names = c("MVPA", "LPA", "SB", "Sleep"),
                                   outcome = outcomes[i],
                                   covs = covs2,
                                   comparisons = c("one-v-one"),
                                   increase = behavior_increase[i],
                                   decrease = "SB",
                                   xlim = c(-5,30),
                                   ylim = c(-0.5, 0.5),
                                   ylab = "",
                                   xlab = "\u2193 SB \u2192 \u2191 LPA",
                                   col = colors[2])
  
}

##Increase LPA, decrease SLEEP (1-to-1)----
for (i in 1:length(outcomes)) {
  
  x= myfunctions::reallocationPlot(data = high_md,      
                                   comps = compo1,
                                   comps.names = c("MVPA", "LPA", "SB", "Sleep"),
                                   outcome = outcomes[i], 
                                   covs = covs2,
                                   comparisons = c("one-v-one"),
                                   increase = behavior_increase[i],
                                   decrease = "Sleep",
                                   xlim = c(-5,30),
                                   ylim = c(-0.5, 0.5),
                                   ylab = "Change in zScore",
                                   xlab = "\u2193 Sleep \u2192 \u2191 LPA",
                                   main = "Reallocation plot increasing \nLPA and decreasing sleep",
                                   col = colors[1])
  
  par(new = TRUE)
  
  x= myfunctions::reallocationPlot(data = low_md,
                                   comps = compo1,
                                   comps.names = c("MVPA", "LPA", "SB", "Sleep"),
                                   outcome = outcomes[i],
                                   covs = covs2,
                                   comparisons = c("one-v-one"),
                                   increase = behavior_increase[i],
                                   decrease = "Sleep",
                                   xlim = c(-5,30),
                                   ylim = c(-0.5, 0.5),
                                   ylab = "",
                                   xlab = "\u2193 Sleep \u2192 \u2191 LPA",
                                   col = colors[2])
  
  
}

dev.off()

#Fig- Episodic memory MVPA (propotional reallocations) ----
dev.new()

#save image
png("processed_data/figEM_MVPA.png", height = 2.6, width = 10,
    res = 600, units = "in")

layout(matrix(1:4, nrow = 1, ncol = 4, byrow = TRUE))
par(oma = c(0.5, 0.5, 0.5, 0.5))# Márgenes exteriores para que haya espacio arriba
par(mar = c(4, 4, 2, 1))  # Márgenes internos entre gráficos
par(cex.axis = 0.8, cex.lab = 1,cex.main = 1)  # Tamaño de texto

outcomes = c("episodic_mem_mean_z")
behavior_increase = "MVPA"



for (i in 1:length(outcomes)) {
  
  x= myfunctions::reallocationPlot(data = high_md,      
                                   comps = compo1,
                                   comps.names = c("MVPA", "LPA", "SB", "Sleep"),
                                   outcome = outcomes[i],
                                   covs = covs2,
                                   comparisons = "prop-realloc",
                                   increase = behavior_increase[i],
                                   xlim = c(-5,30),
                                   ylim = c(-0.5, 0.5),
                                   ylab = "Change in z-score",
                                   xlab = "Min/day proportionally reallocated of MVPA",
                                   main = "Proportional reallocation plot of MVPA \n with other behaviours",
                                   col = colors[1])
  bY = summary(x$MVPA)$coefficients["MVPA_LPA.SB.Sleep", "Estimate"]
  pY = summary(x$MVPA)$coefficients["MVPA_LPA.SB.Sleep", "Pr(>|t|)"]
  
  par(new = TRUE)
  
  x= myfunctions::reallocationPlot(data = low_md,
                                   comps = compo1,
                                   comps.names = c("MVPA", "LPA", "SB", "Sleep"),
                                   outcome = outcomes[i],
                                   covs = covs2,
                                   comparisons = "prop-realloc",
                                   increase = behavior_increase[i],
                                   xlim = c(-5,30),
                                   ylim = c(-0.5, 0.5),
                                   ylab = "",
                                   xlab = "",
                                   col = colors[2])
  bO = summary(x$MVPA)$coefficients["MVPA_LPA.SB.Sleep", "Estimate"]
  pO = summary(x$MVPA)$coefficients["MVPA_LPA.SB.Sleep", "Pr(>|t|)"]
  
  # text p values
  legend(
    "topleft",
    legend = c(
      paste0("β = ", round(bY, 2), ", p = ", format(round(pY, 2), digits = 2)),
      paste0("β = ", round(bO, 2), ", p = ", format(round(pO, 2), digits = 2))
    ),
    text.col = colors[1:2],
    bty = "n"
  )
}


##Increase MVPA, decrease LPA (1-to-1)----
for (i in 1:length(outcomes)) {
  
  x= myfunctions::reallocationPlot(data = high_md,      
                                   comps = compo1,
                                   comps.names = c("MVPA", "LPA", "SB", "Sleep"),
                                   outcome = outcomes[i], covs = covs2,
                                   comparisons = c("one-v-one"),
                                   increase = behavior_increase[i],
                                   decrease = "LPA",# EN ESTE CASO HA SALIDO SIGNIFICATIVO EL SUENO ENTONCES LO PONEMOS COMO INCREASE
                                   xlim = c(-5,30),
                                   ylim = c(-0.5, 0.5),
                                   ylab = "Change in z-score",
                                   xlab = "\u2193 LPA \u2192 \u2191 MVPA",
                                   main = "Reallocation plot increasing \nMVPA and decreasing LPA",
                                   col = colors[1])
  
  
  par(new = TRUE)
  
  x= myfunctions::reallocationPlot(data = low_md,
                                   comps = compo1,
                                   comps.names = c("MVPA", "LPA", "SB", "Sleep"),
                                   outcome = outcomes[i],
                                   covs = covs2,
                                   comparisons = c("one-v-one"),
                                   increase = behavior_increase[i],
                                   decrease = "LPA",
                                   xlim = c(-5,30),
                                   ylim = c(-0.5, 0.5),
                                   ylab = "",
                                   xlab = "\u2193 LPA \u2192 \u2191 MVPA",
                                   col = colors[2])
  
  
}

##Increase MVPA, decrease SB (1-to-1)----

for (i in 1:length(outcomes)) {
  
  x= myfunctions::reallocationPlot(data = high_md,      
                                   comps = compo1,
                                   comps.names = c("MVPA", "LPA", "SB", "Sleep"),
                                   outcome = outcomes[i], covs = covs2,
                                   comparisons = c("one-v-one"),
                                   increase = behavior_increase[i],
                                   decrease = "SB", 
                                   xlim = c(-5,30),
                                   ylim = c(-0.5, 0.5),
                                   ylab = "Change in zScore",
                                   xlab = "\u2193 SB \u2192 \u2191 MVPA",
                                   main = "Reallocation plot increasing \nMVPA and decreasing SB",
                                   col = colors[1])
  #pY = summary(x$SB)$coefficients[1, "Pr(>|t|)"]
  
  par(new = TRUE)
  
  x= myfunctions::reallocationPlot(data = low_md,
                                   comps = compo1,
                                   comps.names = c("MVPA", "LPA", "SB", "Sleep"),
                                   outcome = outcomes[i],
                                   covs = covs2,
                                   comparisons = c("one-v-one"),
                                   increase = behavior_increase[i],
                                   decrease = "SB",
                                   xlim = c(-5,30),
                                   ylim = c(-0.5, 0.5),
                                   ylab = "",
                                   xlab = "\u2193 SB \u2192 \u2191 MVPA",
                                   col = colors[2])
  
  # text p values
}

##Increase MVPA, decrease SLEEP (1-to-1)----
for (i in 1:length(outcomes)) {
  
  x= myfunctions::reallocationPlot(data = high_md,      
                                   comps = compo1,
                                   comps.names = c("MVPA", "LPA", "SB", "Sleep"),
                                   outcome = outcomes[i], 
                                   covs = covs2,
                                   comparisons = c("one-v-one"),
                                   increase = behavior_increase[i],
                                   decrease = "Sleep",
                                   xlim = c(-5,30),
                                   ylim = c(-0.5, 0.5),
                                   ylab = "Change in zScore",
                                   xlab = "\u2193 Sleep \u2192 \u2191 MVPA",
                                   main = "Reallocation plot increasing \nMVPA and decreasing sleep",
                                   col = colors[1])
  
  
  par(new = TRUE)
  
  x= myfunctions::reallocationPlot(data = low_md,
                                   comps = compo1,
                                   comps.names = c("MVPA", "LPA", "SB", "Sleep"),
                                   outcome = outcomes[i],
                                   covs = covs2,
                                   comparisons = c("one-v-one"),
                                   increase = behavior_increase[i],
                                   decrease = "Sleep",
                                   xlim = c(-5,30),
                                   ylim = c(-0.5, 0.5),
                                   ylab = "",
                                   xlab = "\u2193 Sleep \u2192 \u2191 MVPA",
                                   col = colors[2])
  
}

dev.off()

#Fig- Episodic memory LPA (propotional reallocations) ----

dev.new()

#save image
png("processed_data/figEM_LPA.png", height = 2.6, width = 10,
    res = 600, units = "in")

layout(matrix(1:4, nrow = 1, ncol = 4, byrow = TRUE))
par(oma = c(0.5, 0.5, 0.5, 0.5))# Márgenes exteriores para que haya espacio arriba
par(mar = c(4, 4, 2, 1))  # Márgenes internos entre gráficos
par(cex.axis = 0.8, cex.lab = 1,cex.main = 1)  # Tamaño de texto

outcomes = c("episodic_mem_mean_z")
behavior_increase = "LPA"

for (i in 1:length(outcomes)) {
  
  x= myfunctions::reallocationPlot(data = high_md,      
                                   comps = compo1,
                                   comps.names = c("MVPA", "LPA", "SB", "Sleep"),
                                   outcome = outcomes[i],
                                   covs = covs2,
                                   comparisons = "prop-realloc",
                                   increase = behavior_increase[i],
                                   xlim = c(-5,30),
                                   ylim = c(-0.5, 0.5),
                                   ylab = "Change in z-score",
                                   xlab = "Min/day proportionally reallocated of LPA",
                                   main = "Proportional reallocation plot of LPA \n with other behaviours",
                                   col = colors[1])
  bY = summary(x$LPA)$coefficients["LPA_MVPA.SB.Sleep", "Estimate"]
  pY = summary(x$LPA)$coefficients["LPA_MVPA.SB.Sleep", "Pr(>|t|)"]
  
  par(new = TRUE)
  
  x= myfunctions::reallocationPlot(data = low_md,
                                   comps = compo1,
                                   comps.names = c("MVPA", "LPA", "SB", "Sleep"),
                                   outcome = outcomes[i],
                                   covs = covs2,
                                   comparisons = "prop-realloc",
                                   increase = behavior_increase[i],
                                   xlim = c(-5,30),
                                   ylim = c(-0.5, 0.5),
                                   ylab = "",
                                   xlab = "",
                                   col = colors[2])
  bO = summary(x$LPA)$coefficients["LPA_MVPA.SB.Sleep", "Estimate"]
  pO = summary(x$LPA)$coefficients["LPA_MVPA.SB.Sleep", "Pr(>|t|)"]
  
  # text p values
  legend(
    "topleft",
    legend = c(
      paste0("β = ", round(bY, 2), ", p = ", format(round(pY, 2), digits = 2)),
      paste0("β = ", round(bO, 2), ", p = ", format(round(pO, 2), digits = 2))
    ),
    text.col = colors[1:2],
    bty = "n"
  )
  
}



##Increase LPA, decrease MVPA (1-to-1)----
for (i in 1:length(outcomes)) {
  
  x= myfunctions::reallocationPlot(data = high_md,      
                                   comps = compo1,
                                   comps.names = c("MVPA", "LPA", "SB", "Sleep"),
                                   outcome = outcomes[i],
                                   covs = covs2,
                                   comparisons = c("one-v-one"),
                                   increase = behavior_increase[i],
                                   decrease = "MVPA",# EN ESTE CASO HA SALIDO SIGNIFICATIVO EL SUENO ENTONCES LO PONEMOS COMO INCREASE
                                   xlim = c(-5,25),
                                   ylim = c(-0.5, 0.5),
                                   ylab = "Change in z-score",
                                   xlab = "\u2193 MVPA \u2192 \u2191 LPA",
                                   main = "Reallocation plot increasing \n LPA and decreasing MVPA",
                                   col = colors[1])
  
  par(new = TRUE)
  
  x= myfunctions::reallocationPlot(data = low_md,
                                   comps = compo1,
                                   comps.names = c("MVPA", "LPA", "SB", "Sleep"),
                                   outcome = outcomes[i],
                                   covs = covs2,
                                   comparisons = c("one-v-one"),
                                   increase = behavior_increase[i],
                                   decrease = "MVPA",
                                   xlim = c(-5,25),
                                   ylim = c(-0.5, 0.5),
                                   ylab = "",
                                   xlab = "\u2193 MVPA \u2192 \u2191 LPA",
                                   col = colors[2])
  
}

##Increase LPA, decrease SB (1-to-1)----

for (i in 1:length(outcomes)) {
  
  x= myfunctions::reallocationPlot(data = high_md,      
                                   comps = compo1,
                                   comps.names = c("MVPA", "LPA", "SB", "Sleep"),
                                   outcome = outcomes[i], covs = covs2,
                                   comparisons = c("one-v-one"),
                                   increase = behavior_increase[i],
                                   decrease = "SB", 
                                   xlim = c(-5,30),
                                   ylim = c(-0.5, 0.5),
                                   ylab = "Change in zScore",
                                   xlab = "\u2193 SB \u2192 \u2191 LPA",
                                   main = "Reallocation plot increasing \n LPA and decreasing SB",
                                   col = colors[1])
  #pY = summary(x$SB)$coefficients[1, "Pr(>|t|)"]
  par(new = TRUE)
  
  x= myfunctions::reallocationPlot(data = low_md,
                                   comps = compo1,
                                   comps.names = c("MVPA", "LPA", "SB", "Sleep"),
                                   outcome = outcomes[i],
                                   covs = covs2,
                                   comparisons = c("one-v-one"),
                                   increase = behavior_increase[i],
                                   decrease = "SB",
                                   xlim = c(-5,30),
                                   ylim = c(-0.5, 0.5),
                                   ylab = "",
                                   xlab = "\u2193 SB \u2192 \u2191 LPA",
                                   col = colors[2])
  
}

##Increase LPA, decrease SLEEP (1-to-1)----
for (i in 1:length(outcomes)) {
  
  x= myfunctions::reallocationPlot(data = high_md,      
                                   comps = compo1,
                                   comps.names = c("MVPA", "LPA", "SB", "Sleep"),
                                   outcome = outcomes[i], 
                                   covs = covs2,
                                   comparisons = c("one-v-one"),
                                   increase = behavior_increase[i],
                                   decrease = "Sleep",
                                   xlim = c(-5,30),
                                   ylim = c(-0.5, 0.5),
                                   ylab = "Change in zScore",
                                   xlab = "\u2193 Sleep \u2192 \u2191 LPA",
                                   main = "Reallocation plot increasing \nLPA and decreasing sleep",
                                   col = colors[1])
  
  par(new = TRUE)
  
  x= myfunctions::reallocationPlot(data = low_md,
                                   comps = compo1,
                                   comps.names = c("MVPA", "LPA", "SB", "Sleep"),
                                   outcome = outcomes[i],
                                   covs = covs2,
                                   comparisons = c("one-v-one"),
                                   increase = behavior_increase[i],
                                   decrease = "Sleep",
                                   xlim = c(-5,30),
                                   ylim = c(-0.5, 0.5),
                                   ylab = "",
                                   xlab = "\u2193 Sleep \u2192 \u2191 LPA",
                                   col = colors[2])
  
}

dev.off()



#Fig- Episodic memory sleep (propotional reallocations) ----
dev.new()

png("processed_data/figEM_SLEEP.png",  height = 2.6, width = 10,
    res = 600, units = "in")

layout(matrix(1:4, nrow = 1, ncol = 4, byrow = TRUE))
par(oma = c(0.5, 0.5, 0.5, 0.5))# Márgenes exteriores para que haya espacio arriba
par(mar = c(4, 4, 2, 1))  # Márgenes internos entre gráficos
par(cex.axis = 0.8, cex.lab = 1,cex.main = 1)  # Tamaño de texto

outcomes = c("episodic_mem_mean_z")
behavior_increase = "Sleep"


behavior_increase = "Sleep"

for (i in 1:length(outcomes)) {
  
  x= myfunctions::reallocationPlot(data = high_md,      
                                   comps = compo1,
                                   comps.names = c("MVPA", "LPA", "SB", "Sleep"),
                                   outcome = outcomes[i],
                                   covs = covs2,
                                   comparisons = "prop-realloc",
                                   increase = behavior_increase[i],
                                   xlim = c(-5,30),
                                   ylim = c(-0.5, 0.5),
                                   ylab = "Change in z-score",
                                   xlab = "Min/day proportionally reallocated of sleep",
                                   main = "Proportional reallocation plot of sleep \n with other behaviours",
                                   col = colors[1])
  bY = summary(x$Sleep)$coefficients["Sleep_MVPA.LPA.SB", "Estimate"]
  pY = summary(x$Sleep)$coefficients["Sleep_MVPA.LPA.SB", "Pr(>|t|)"]
  
  par(new = TRUE)
  
  x= myfunctions::reallocationPlot(data = low_md,
                                   comps = compo1,
                                   comps.names = c("MVPA", "LPA", "SB", "Sleep"),
                                   outcome = outcomes[i],
                                   covs = covs2,
                                   comparisons = "prop-realloc",
                                   increase = behavior_increase[i],
                                   xlim = c(-5,30),
                                   ylim = c(-0.5, 0.5),
                                   ylab = "",
                                   xlab = "",
                                   col = colors[2])
  bO = summary(x$Sleep)$coefficients["Sleep_MVPA.LPA.SB", "Estimate"]
  pO = summary(x$Sleep)$coefficients["Sleep_MVPA.LPA.SB", "Pr(>|t|)"]
  
  # text p values
  legend(
    "topleft",
    legend = c(
      paste0("β = ", round(bY, 2), ", p = ", format(round(pY, 2), digits = 2)),
      paste0("β = ", round(bO, 2), ", p = ", format(round(pO, 2), digits = 2))
    ),
    text.col = colors[1:2],
    bty = "n"
  )
  
}


##Increase Sleep, decrease MVPA (1-to-1)----
for (i in 1:length(outcomes)) {
  
  x= myfunctions::reallocationPlot(data = high_md,      
                                   comps = compo1,
                                   comps.names = c("MVPA", "LPA", "SB", "Sleep"),
                                   outcome = outcomes[i],
                                   covs = covs2,
                                   comparisons = c("one-v-one"),
                                   increase = behavior_increase[i],
                                   decrease = "MVPA",# EN ESTE CASO HA SALIDO SIGNIFICATIVO EL SUENO ENTONCES LO PONEMOS COMO INCREASE
                                   xlim = c(-5,25),
                                   ylim = c(-0.5, 0.5),
                                   ylab = "Change in z-score",
                                   xlab = "\u2193 MVPA \u2192 \u2191 sleep",
                                   main = "Reallocation plot increasing \n sleep and decreasing MVPA",
                                   col = colors[1])
  
  par(new = TRUE)
  
  x= myfunctions::reallocationPlot(data = low_md,
                                   comps = compo1,
                                   comps.names = c("MVPA", "LPA", "SB", "Sleep"),
                                   outcome = outcomes[i],
                                   covs = covs2,
                                   comparisons = c("one-v-one"),
                                   increase = behavior_increase[i],
                                   decrease = "MVPA",
                                   xlim = c(-5,25),
                                   ylim = c(-0.5, 0.5),
                                   ylab = "",
                                   xlab = "\u2193 MVPA \u2192 \u2191 sleep",
                                   col = colors[2])
  

}

##Increase Sleep, decrease LPA (1-to-1)----

for (i in 1:length(outcomes)) {
  
  x= myfunctions::reallocationPlot(data = high_md,      
                                   comps = compo1,
                                   comps.names = c("MVPA", "LPA", "SB", "Sleep"),
                                   outcome = outcomes[i], covs = covs2,
                                   comparisons = c("one-v-one"),
                                   increase = behavior_increase[i],
                                   decrease = "LPA", 
                                   xlim = c(-5,30),
                                   ylim = c(-0.5, 0.5),
                                   ylab = "Change in zScore",
                                   xlab = "\u2193 LPA \u2192 \u2191 sleep",
                                   main = "Reallocation plot increasing \n sleep and decreasing LPA",
                                   col = colors[1])
  
  par(new = TRUE)
  
  x= myfunctions::reallocationPlot(data = low_md,
                                   comps = compo1,
                                   comps.names = c("MVPA", "LPA", "SB", "Sleep"),
                                   outcome = outcomes[i],
                                   covs = covs2,
                                   comparisons = c("one-v-one"),
                                   increase = behavior_increase[i],
                                   decrease = "LPA",
                                   xlim = c(-5,30),
                                   ylim = c(-0.5, 0.5),
                                   ylab = "",
                                   xlab = "\u2193 LPA \u2192 \u2191 sleep",
                                   col = colors[2])
  
}

##Increase sleep, decrease SB (1-to-1)----
for (i in 1:length(outcomes)) {
  
  x= myfunctions::reallocationPlot(data = high_md,      
                                   comps = compo1,
                                   comps.names = c("MVPA", "LPA", "SB", "Sleep"),
                                   outcome = outcomes[i], 
                                   covs = covs2,
                                   comparisons = c("one-v-one"),
                                   increase = behavior_increase[i],
                                   decrease = "SB",
                                   xlim = c(-5,30),
                                   ylim = c(-0.5, 0.5),
                                   ylab = "Change in zScore",
                                   xlab = "\u2193 SB \u2192 \u2191 sleep",
                                   main = "Reallocation plot increasing \n sleep and decreasing SB",
                                   col = colors[1])
  par(new = TRUE)
  
  x= myfunctions::reallocationPlot(data = low_md,
                                   comps = compo1,
                                   comps.names = c("MVPA", "LPA", "SB", "Sleep"),
                                   outcome = outcomes[i],
                                   covs = covs2,
                                   comparisons = c("one-v-one"),
                                   increase = behavior_increase[i],
                                   decrease = "SB",
                                   xlim = c(-5,30),
                                   ylim = c(-0.5, 0.5),
                                   ylab = "",
                                   xlab = "\u2193 SB \u2192 \u2191 sleep",
                                   col = colors[2])
  
}

dev.off()



## Settle up S2.Figure ------------------------------------------------------------
dev.new()
png(filename = "processed_data/fig_legend.png", width = 15, height = 2.6, res = 600, units = "in")

par(mar = c(0, 2, 0, 2)) 

plot.new() 
legend("center", legend = c("Higher GMMD", "Lower GMMD"), 
                  lwd = 8, col = colors[1:2], cex = 4, bty = "n", horiz = TRUE)
dev.off()

figAC  <- ggdraw() + draw_image("processed_data/figAC.png")
figWM  <- ggdraw() + draw_image("processed_data/figWM.png")
figPS  <- ggdraw() + draw_image("processed_data/figPS.png")
figEM_MVPA  <- ggdraw() + draw_image("processed_data/figEM_MVPA.png")
figEM_LPA   <- ggdraw() + draw_image("processed_data/figEM_LPA.png")
figEM_SLEEP <- ggdraw() + draw_image("processed_data/figEM_SLEEP.png")
fig_legend <- ggdraw() + draw_image("processed_data/fig_legend.png")


#Title
title_top <- ggdraw() + 
  draw_label(
    "",
    fontface = "bold", size = 4, x = 0, hjust = 0
  )

tight <- function(p) p + theme(plot.margin = margin(0, 0, 0, 0))

figAC  <- tight(figAC)
figWM  <- tight(figWM)
figPS  <- tight(figPS)
figEM_MVPA  <- tight(figEM_MVPA)
figEM_LPA   <- tight(figEM_LPA)
figEM_SLEEP <- tight(figEM_SLEEP)

##Block attention
title_AT <- ggdraw() +
  draw_label(
    "Attentional/inhibitory control — increasing LPA",
    fontface = "bold",
    size = 4,
    x = 0.02,
    hjust = 0
  )

AT_panel <- cowplot::plot_grid(
  title_AT,
  figAC,
  ncol = 1,
  rel_heights = c(0.13, 1)
)

#Block working memory

title_WM <- ggdraw() +
  draw_label(
    "Working memory — increasing LPA",
    fontface = "bold",
    size = 4,
    x = 0.02,
    hjust = 0
  )

WM_panel <- cowplot::plot_grid(
  title_WM,
  figWM,
  ncol = 1,
  rel_heights = c(0.13, 1)
)
#Block Procesig speed

title_PE <- ggdraw() +
  draw_label(
    "Processing speed — increasing LPA",
    fontface = "bold",
    size = 4,
    x = 0.02,
    hjust = 0
  )

PE_panel <- cowplot::plot_grid(
  title_PE,
  figPS,
  ncol = 1,
  rel_heights = c(0.13, 1)
)

#Left panel
left_panel <- cowplot::plot_grid(
  AT_panel,
  PE_panel,
  WM_panel,
  ncol = 1,
  align = "v"
)

#Block Episodic memory ---

title_EM_MVPA <- ggdraw() +
  draw_label(
    "Episodic memory — increasing MVPA",
    fontface = "bold",
    size = 4,
    x = 0.02,
    hjust = 0
  )

title_EM_LPA <- ggdraw() +
  draw_label(
    "Episodic memory — increasing LPA",
    fontface = "bold",
    size = 4,
    x = 0.02,
    hjust = 0
  )

title_EM_SLEEP <- ggdraw() +
  draw_label(
    "Episodic memory — increasing sleep",
    fontface = "bold",
    size = 4,
    x = 0.02,
    hjust = 0
  )

panel_EM_MVPA <- cowplot::plot_grid(
  title_EM_MVPA,
  figEM_MVPA,
  ncol = 1,
  rel_heights = c(0.13, 1)
)

panel_EM_LPA <- cowplot::plot_grid(
  title_EM_LPA,
  figEM_LPA,
  ncol = 1,
  rel_heights = c(0.13, 1)
)

panel_EM_SLEEP <- cowplot::plot_grid(
  title_EM_SLEEP,
  figEM_SLEEP,
  ncol = 1,
  rel_heights = c(0.13, 1)
)

em_panel <- cowplot::plot_grid(
  panel_EM_MVPA,
  panel_EM_LPA,
  panel_EM_SLEEP,
  ncol = 1,
  align = "v"
)

all_blocks <- cowplot::plot_grid(
  left_panel,
  em_panel,
  ncol = 2,
  align = "v"
)

# Final plot
full_panel <- cowplot::plot_grid(
  title_top,
  all_blocks,
  fig_legend,
  ncol = 1,
  rel_heights = c(0.05, 1, 0.10),
  align = "v",
  axis = "tb"
)

# Line between panels 
full_panel_with_vline <- ggdraw() +
  draw_plot(full_panel) + 
  draw_line(
    x = c(0.50, 0.50),   # centrado exacto
    y = c(0.09, 0.92),
    colour = "grey40",
    size = 0.25,
    linetype = "dashed"
  )

# Save it
ggsave(
  "processed_data/fig4_relocations_P.png",
  full_panel_with_vline,
  width = 6,
  height = 3,   # altura proporcional a 3 filas
  dpi = 600
)




