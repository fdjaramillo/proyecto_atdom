# ============================================================
# 03_02 Cluster_figures.R
# ============================================================

source(here("scripts", "00_0 setup.R"))

# HCPC DENDROGRAM
# ============================================================

res_hcpc<-readRDS(here("data","processed","res_hcpc.rds"))


# 4-group hierarchical partition

tree_groups <- cutree(
  res_hcpc$call$t$tree,
  k = 4
)

# Obtain final HCPC clusters

hcpc_groups <- res_hcpc$data.clust$clust

names(hcpc_groups) <- rownames(
  res_hcpc$data.clust
)

# Align final HCPC clusters with dendrogram observations

hcpc_groups_aligned <- hcpc_groups[
  names(tree_groups)]

# Check alignment
stopifnot(
  identical(
    names(tree_groups),
    names(hcpc_groups_aligned)
  )
)

tree_cols <- c(
  "#009E73",
  "#0072B2",
  "#E69F00",
  "#CC79A7"
)

#Plot

png(
  filename = here(
    "Output",
    "Cluster",
    "4_cluster_dendrogram.png"
  ),
  width = 2400,
  height = 1800,
  res = 300
)

# More space at bottom for phenotype labels
par(
  mar = c(3, 4, 4, 2),
  xpd = NA
)

plot(
  res_hcpc$call$t$tree,
  labels = FALSE,
  hang = -1,
  main = "Hierarchical clustering dendrogram",
  sub = "",
  xlab = "",
  ylab = "Dissimilarity",
  cex.main = 1.2,
  cex.lab = 1,
  cex.axis = 0.9
)

# 4. Four-group hierarchical partition

rect.hclust(
  res_hcpc$call$t$tree,
  k = 4,
  border = tree_cols
)

dev.off()


# INDIVIDUALS IN FACTORIAL SPACE BY CLUSTER



# Cluster projection
# ============================================================

res_famd<-readRDS(here("data","processed","res_famd.rds"))


plot_ind <- as.data.frame(
  res_famd$ind$coord[, 1:2]
) %>%
  mutate(
    cluster = factor(res_hcpc$data.clust$clust)
  )

names(plot_ind)[1:2] <- c("Dim1", "Dim2")

p_cluster <- fviz_cluster(
  res_hcpc,
  axes = c(1, 2),
  geom = "point",
  pointsize = 2,
  alpha = 0.7,
  ellipse = T,
  palette =  c(
    "#0072B2",  # Lower complexity - blue
    "#009E73",  # Social vulnerability - green
    "#E69F00",  # High multimorbidity - orange
    "#CC79A7"),   # Neurocognitive-functional dependency - purple,
  ggtheme = theme_classic(base_size = 14),
  main = "FAMD projection of baseline\n phenotypes in home-based care"
) +
  labs(
    x = "Dimension 1",
    y = "Dimension 2"
  ) +
  
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.title = element_text(
      face = "bold",
      hjust = 0.5
    ),
    axis.text = element_text(
      color = "black"
    )
  )

p_cluster

ggsave(
  filename = here("output", "Cluster", "4_cluster_projection.png"),
  plot = p_cluster,
  width = 10,
  height = 8,
  units = "in",
  dpi = 300
)

#Heat map
# ============================================================

df_cluster<-readRDS(here("data","cluster","df_Cluster"))

# Variables for prevalence heatmap

heatmap_vars <- c(
  # Functional
  "severe_total_dependency",
  "double_incontinence",
  
  # Social
  "TIRS_cat",
  "living_alone",
  "MACA",
  
  # Neurocognitive
  "dementia",
  "parkinson",
  "stroke",
  
  # Mental health
  "anxiety_depression",
  
  # Cardiometabolic / renal
  "heart_failure",
  "ischemic_heart",
  "diabetes",
  "ckd",
  
  # Respiratory / musculoskeletal / cancer
  "copd",
  "Arthritis_osteoarthritis",
  "Cancer"
)

# 2. Publication labels

heatmap_labels <- c(
  severe_total_dependency = "Severe or total functional dependency",
  double_incontinence     = "Fecal and urinary incontinence",
  TIRS_cat                = "Social risk",
  MACA                    = "End of life registry",
  living_alone            = "Living alone",
  dementia                = "Dementia",
  parkinson               = "Parkinson disease",
  stroke                  = "Stroke",
  anxiety_depression      = "Anxiety or depression",
  heart_failure           = "Heart failure",
  ischemic_heart          = "Ischemic heart disease",
  diabetes                = "Diabetes",
  ckd                     = "Chronic kidney disease",
  copd                    = "Chronic obstructive pulmonary disease",
  Arthritis_osteoarthritis= "Arthritis or osteoarthritis",
  Cancer                  = "Cancer"
)

df_cluster_heatmap <- df_cluster%>%
  
  mutate(
    # Barthel: severe or total dependency
    severe_total_dependency = case_when(
      barthel_cat %in% c("Total (<20)", "Severe (20-35)") ~ "si",
      !is.na(barthel_cat) ~ "no",
      TRUE ~ NA_character_
    )%>%
      as.factor(),
    
    # Double incontinence
    double_incontinence = case_when(
      incontinence_cat == "Fecal and urinary" ~ "si",
      !is.na(incontinence_cat) ~ "no",
      TRUE ~ NA_character_
    )%>%
      as.factor(),
    
    # living_alone
    living_alone = case_when(
      living_alone == "Yes" ~ "si",
      living_alone == "No" ~ "no",
      TRUE ~ NA_character_
    )%>%
      as.factor()
  )

# Calculate prevalence within each phenotype

heatmap_data <- df_cluster_heatmap %>%
  mutate(
    cluster = factor(
      cluster,
      levels = c(
        "1",
        "2",
        "3",
        "4"
      ),
      labels = c(
        "Lower complexity",
        "Social vulnerability",
        "High multimorbidity",
        "Neurocognitive-functional dependency"
      )
    )
  ) %>%
  group_by(cluster) %>%
  summarise(
    across(
      all_of(heatmap_vars),
      ~ mean(.x == "si", na.rm = TRUE) * 100
    ),
    .groups = "drop"
  )

# ------------------------------------------------------------
# 4. Convert to matrix
#    Rows = characteristics
#    Columns = phenotypes
# ------------------------------------------------------------

mat <- heatmap_data %>%
  select(cluster, all_of(heatmap_vars)) %>%
  column_to_rownames("cluster") %>%
  as.matrix() %>%
  t()
  

# 5. Apply publication labels

rownames(mat) <- heatmap_labels[rownames(mat)]

# 6. Labels displayed inside cells

labels_mat <- matrix(
  paste0(sprintf("%.1f", mat), "%"),
  nrow = nrow(mat),
  ncol = ncol(mat),
  dimnames = dimnames(mat)
)

# 5. Guardar heatmap a 300 dpi

png(
  filename = here(
    "Output",
    "Cluster",
    "cluster_heatmap.png"
  ),
  units = "cm",
  width = 29.5,
  height = 19,
  res = 300
)

pheatmap(
  mat,

  color = colorRampPalette(
    c(
      "#fff5f0",
      "#fc9272",
      "#de2d26",
      "#a50f15"
    )
  )(100),

  breaks = seq(0, 100, length.out = 101),

  scale = "none",
  border_color = NA,
  legend = F,

  cluster_rows = FALSE,
  cluster_cols = FALSE,

  labels_row = rownames(mat),   # variable names
  labels_col = colnames(mat),   # phenotype names

  fontsize = 10,
  fontsize_row = 11,
  fontsize_col = 10,

  angle_col = 0,

  display_numbers = labels_mat,
  number_color = "black",

  main = "Clinical and social characteristics of home-based primary care phenotypes"
)

dev.off()
