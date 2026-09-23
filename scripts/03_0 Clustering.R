# ============================================================
# 03_0 Cluster case_mix selection analysis.R
# ============================================================
library(here)
source(here("scripts", "00_0 setup.R"))

# LOAD AND PREPARE DATA

df<-readRDS(here("data","processed","DF_pacientes_inicial.RDS"))%>%
      mutate(ID=as.character(ID))

metadata_dict_inicial <- read_csv2(here("data", "metadata_dict_inicial.csv"))

# 2. CREATE DATASET FOR CLUSTER ANALYSIS

TB_FAMD <- apply_all_transformations_inicial(df, metadata_dict_inicial)%>%
  select(c(1,2,3,4,5,8,12,16,19,20,21,23:47))%>%
  left_join(df[,c(1,5,9,11,22,24,29)],
            by=c("ID"))%>%
  mutate(SEXE=as.factor(SEXE),
         
         # Mental-health domain: combine anxiety and depression to avoid giving
         # excessive weight to closely related diagnoses
         
         anxiety_depression = case_when(
           depression == "si" | anxiety == "si" ~ "si",
           depression == "no" & anxiety == "no" ~ "no",
           TRUE ~ NA_character_
         ),
         anxiety_depression = factor(anxiety_depression),
         
         # GMA level.NOT an active clustering variable.
         # Retained for external characterization.
         
         GMA_PNIV = factor(GMA_PNIV, ordered = TRUE),
         
         # Correct anomalous TIRS value
         
         TIRS_num=ifelse(TIRS_num==0.64,0,TIRS_num),
         
         # Cancer: combine cancer and metastatic cancer.
         
         cancer = case_when(
           Cancer == "si" | Metastasic_cancer == "si" ~ "si",
           Cancer == "no" & Metastasic_cancer == "no" ~ "no",
           TRUE ~ NA_character_
         ),
         cancer = factor(cancer)
         )%>%
  rename(BRADEN=BRADEN.x)%>%
  select(-sex_female,-PHC_name,-depression,-anxiety,-Metastasic_cancer,-Cancer)

# DEFINE ACTIVE AND EXTERNAL VARIABLES

# Active variables:
# These variables DEFINE the patient profiles.
           
vars_famd_core <- c(
  "Edat",
  "SEXE",
  "BARTHEL",
  
  "TIRS_num",
  "living_alone",
  
  "heart_failure",
  "ischemic_heart",
  
  "dementia",
  "parkinson",
  "stroke",
  "anxiety_depression",
  
  "diabetes",
  "ckd",
  
  "copd",

  "cancer")

# External / supplementary variables:
#
# These variables DO NOT define the clusters.
# They will subsequently be used to characterize and compare
# the resulting patient profiles.

vars_sup <- c(
  "MACA",
  "GMA_N_CRONIQUES",
  "GMA_PNIV",
  "Walking_distance",
  "years_home_based",
  "Home_based_PHC_org",
  "respiratory_failure")


# 5. CREATE ANALYTIC DATASET

TB_FAMD_analysis <- TB_FAMD %>%
  select(
    ID,
    all_of(vars_famd_core),
    all_of(vars_sup)
  )

# 6. MISSING-DATA DESCRIPTION

missing_data <- TB_FAMD_analysis %>%
  summarise(
    across(
      -ID,
      ~ mean(is.na(.)) * 100
    )
  ) %>%
  tidyr::pivot_longer(
    everything(),
    names_to = "Variable",
    values_to = "Missing_pct"
  ) %>%
  arrange(desc(Missing_pct))

missing_data

# 7. PREPARE ACTIVE DATA FOR FAMD
# ID and external variables are excluded here.
# Only active variables participate in construction
# of the factorial space.

famd_active <- TB_FAMD_analysis %>%
  select(all_of(vars_famd_core))

# 8. ESTIMATE NUMBER OF DIMENSIONS FOR IMPUTATION
# estim_ncpFAMD() selects the number of latent dimensions used specifically for reconstruction of missing values.
# This is NOT the number of dimensions subsequently retained for HCPC.

set.seed(1234)

nb <- estim_ncpFAMD(
  famd_active,
  ncp.max = 10
)

saveRDS(nb,here("data","processed","MissingFAMD.RDS"))

# 9. IMPUTE MISSING VALUES

nb<-readRDS(here("data","processed","MissingFAMD.RDS"))

set.seed(1234)

imp_famd <- imputeFAMD(
  famd_active,
  ncp = nb$ncp
)

famd_imputed <- imp_famd$completeObs

# Check that imputation retained all individuals

dim(famd_imputed)

sum(is.na(famd_imputed))

# 10. EXPLORATORY FAMD

# First retain 10 dimensions to inspect the factorial
# structure before deciding how many dimensions to use
# in HCPC.

res_famd_full <- FAMD(
  famd_imputed,
  ncp = 10,
  graph = FALSE
)

# 11. FAMD EIGENVALUES
# ============================================================

famd_eigenvalues <- res_famd_full$eig %>%
  
  as.data.frame() %>%
  
  rownames_to_column(
    "Dimension"
  ) %>%
  
  transmute(
    Dimension,
    Eigenvalue = eigenvalue,
    `Variance (%)` =
      `percentage of variance`,
    `Cumulative variance (%)` =
      `cumulative percentage of variance`
  ) %>%
  
  mutate(
    across(
      where(is.numeric),
      ~ round(.x, 2)
    )
  )

famd_eigenvalues

write.xlsx(famd_eigenvalues,here("Output","Cluster","explained_var.RDS"))

# First 6 dimensions used in the main analysis

famd_eigenvalues_6 <- famd_eigenvalues %>%
  slice_head(
    n = 6
  )

famd_eigenvalues_6


# 12. SCREE PLOT

p_scree <- fviz_screeplot(
  res_famd_full,
  addlabels = TRUE
)

ggsave(
  filename = here("Output","Cluster", "FAMD_screeplot.png"),
  plot = p_scree,
  width = 7,
  height = 5,
  units = "in",
  dpi = 300
)

# 13. VARIABLE CONTRIBUTIONS TO FAMD DIMENSIONS
# ============================================================

# Numerical table

famd_contributions <- res_famd_full$var$contrib[, 1:6] %>%
  as.data.frame() %>%
  rownames_to_column(
    "Variable"
  ) %>%
  mutate(
    across(
      where(is.numeric),
      ~ round(.x, 2)
    )
  )

famd_contributions

# Contribution plots

fviz_contrib(
  res_famd_full,
  choice = "var",
  axes = 1,
  top = 15
)

factoextra::fviz_contrib(
  res_famd_full,
  choice = "var",
  axes = 2,
  top = 15
)

factoextra::fviz_contrib(
  res_famd_full,
  choice = "var",
  axes = 3,
  top = 15
)

factoextra::fviz_contrib(
  res_famd_full,
  choice = "var",
  axes = 4,
  top = 15
)

factoextra::fviz_contrib(
  res_famd_full,
  choice = "var",
  axes = 5,
  top = 15
)

factoextra::fviz_contrib(
  res_famd_full,
  choice = "var",
  axes = 6,
  top = 15
)

# ============================================================
# 14. FAMD VARIABLE MAPS
# ============================================================

# Dimensions 1–2
factoextra::fviz_famd_var(
  res_famd_full,
  axes = c(1, 2),
  repel = TRUE
)

# Dimensions 3–4
factoextra::fviz_famd_var(
  res_famd_full,
  axes = c(3, 4),
  repel = TRUE
)

# Dimensions 5–6
factoextra::fviz_famd_var(
  res_famd_full,
  axes = c(5, 6),
  repel = TRUE
)

# 15. FINAL FAMD FOR HCPC
# ============================================================

# Six dimensions were retained because:
#
# - they explain approximately 53% of total FAMD variance;
# - Dimension 6 has eigenvalue approximately 1;
# - Dimensions 1–6 remain clinically interpretable;
# - later dimensions add progressively less information.
#
# IMPORTANT:
# We therefore explicitly reconstruct the FAMD using six
# retained components for HCPC.

res_famd <- FAMD(
  famd_imputed,
  ncp = 6,
  graph = FALSE
)

# ============================================================
# 16. HCPC
# ============================================================

# Automatic selection of number of clusters.

set.seed(1234)

res_hcpc <- FactoMineR::HCPC(
  res_famd,
  nb.clust = -1,
  graph = FALSE
)

# Number and size of clusters

cluster_sizes <- res_hcpc$data.clust %>%
  count(
    clust,
    name = "n"
  ) %>%
  mutate(
    percent = round(
      n / sum(n) * 100,
      1
    )
  )

cluster_sizes


# ============================================================
# 17. HCPC DENDROGRAM
# ============================================================

plot(
  res_hcpc,
  choice = "tree"
)

png(
  filename = here("output", "cluster", "cluster_dendrogram.png"),
  width = 2400,
  height = 1800,
  res = 300
)

par(mar = c(3.5, 2, 4, 2))

plot(
  res_hcpc$call$t$tree,
  labels = FALSE,
  hang = -1,
  main = "Hierarchical clustering dendrogram",
  sub = "",
  xlab = ""
)

rect.hclust(
  res_hcpc$call$t$tree,
  k = 3,
  border = c("#e9c46a", "#e68a2e", "#e45760")
)

legend(
  "bottomleft",
  inset = c(0.02, -0.08),
  legend = c(
    "1",
    "2", 
    "3"
  ),
  col = c("#e9c46a", "#e68a2e", "#e45760"),
  lwd = 3,
  horiz = TRUE,
  bty = "n",
  cex = 0.5,
  xpd = TRUE,
  x.intersp = 0.9,
  y.intersp = 1.0
)

dev.off()


# ============================================================
# 18. INDIVIDUALS IN FACTORIAL SPACE BY CLUSTER
# ============================================================


library(dplyr)
library(tibble)
library(ggplot2)
library(ggrepel)
library(grid)

# ============================================================
# 1. Cluster projection
# ============================================================

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
  palette = c("#e9c46a", "#e68a2e", "#e45760"),
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
  filename = here("output", "Cluster", "cluster_plot.png"),
  plot = p_cluster,
  width = 10,
  height = 8,
  units = "in",
  dpi = 300
)

# ============================================================
# 19. ADD CLUSTER ASSIGNMENT
# ============================================================

# Generate an ID-cluster key.
#
# This is safer than assuming that TB_FAMD_analysis and df
# always have exactly the same row order.

cluster_key <- TB_FAMD_analysis %>%
  
  select(
    ID
  ) %>%
  
  mutate(
    cluster = factor(
      res_hcpc$data.clust$clust
    )
  )


# Add clusters to original patient dataset

df_cluster <- df %>%
  left_join(
    cluster_key,
    by = "ID"
  )


# Verify

table(
  df_cluster$cluster,
  useNA = "ifany"
)


# 20. GLOBAL HCPC VARIABLE DESCRIPTION

desc_var <- res_hcpc$desc.var


# 21. GLOBAL QUANTITATIVE DESCRIPTORS

desc_quanti <- res_hcpc$desc.var$quanti.var %>%
  
  as.data.frame() %>%
  
  rownames_to_column(
    "Variable"
  ) %>%
  
  mutate(
    
    Eta2 = round(
      Eta2,
      3
    ),
    
    `P-value` = format.pval(
      `P-value`,
      digits = 3,
      eps = 0.001
    )
  )

desc_quanti

# 22. HCPC CATEGORICAL DESCRIPTORS BY CLUSTER
# ============================================================

hcpc_desc_category <- imap_dfr(
  res_hcpc$desc.var$category,
  function(x, cluster_id) {
    
    as.data.frame(x) %>%
      
      rownames_to_column(
        "category"
      ) %>%
      
      # Keep the presence ("si") of each disease/condition.
      # This avoids reporting both "no" and "si".
      filter(
        str_detect(
          category,
          "_si$"
        )
      ) %>%
      
      select(
        category,
        `Cla/Mod`,
        `Mod/Cla`,
        Global,
        v.test,
        p.value
      ) %>%
      
      mutate(
        
        # Example:
        # dementia=dementia_si  --> dementia
        
        category = str_replace(
          category,
          "=.*_si$",
          ""
        ),
        
        `Cla/Mod` = round(
          `Cla/Mod`,
          1
        ),
        
        `Mod/Cla` = round(
          `Mod/Cla`,
          1
        ),
        
        Global = round(
          Global,
          1
        ),
        
        v.test = round(
          v.test,
          2
        ),
        
        p.value = format.pval(
          p.value,
          digits = 3,
          eps = 0.001
        ),
        
        Cluster = factor(
          cluster_id
        )
      ) %>%
      
      relocate(
        Cluster
      )
  }
)

hcpc_desc_category

# 23. POSITIVE CATEGORICAL DESCRIPTORS
# ============================================================

hcpc_desc_category_positive <- hcpc_desc_category %>%
  filter(
    v.test > 0
  ) %>%
  arrange(
    Cluster,
    desc(v.test)
  )

hcpc_desc_category_positive

# 24. HCPC QUANTITATIVE DESCRIPTORS BY CLUSTER
# ============================================================

hcpc_desc_quanti <- purrr::imap_dfr(
  res_hcpc$desc.var$quanti,
  function(x, cluster_id) {
    
    if (is.null(x)) {
      return(NULL)
    }
    
    as.data.frame(x) %>%
      
      rownames_to_column(
        "Variable"
      ) %>%
      
      mutate(
        
        across(
          c(
            v.test,
            `Mean in category`,
            `Overall mean`,
            `sd in category`,
            `Overall sd`
          ),
          ~ round(.x, 2)
        ),
        
        p.value = format.pval(
          p.value,
          digits = 3,
          eps = 0.001
        ),
        
        Cluster = factor(
          cluster_id
        )
      ) %>%
      
      relocate(
        Cluster
      )
  }
)

hcpc_desc_quanti

# 25. ADD EXTERNAL VARIABLES TO CLUSTER DATASET
# ============================================================

cluster_external <- TB_FAMD_analysis %>%
  
  select(
    ID,
    all_of(vars_sup)
  ) %>%
  
  left_join(
    cluster_key,
    by = "ID"
  )

# 26. HOME-BASED PHC ORGANIZATION BY CLUSTER
# ============================================================

tab_org <- table(
  cluster_external$cluster,
  cluster_external$Home_based_PHC_org
)

tab_org


# Percentage distribution of clusters WITHIN each
# organizational model.
#
# This is the main table for examining differential case-mix.

tab_org_col_pct <- round(
  prop.table(
    tab_org,
    margin = 2
  ) * 100,
  1
)

tab_org_col_pct

# 27. ASSOCIATION BETWEEN CLUSTER AND ORGANIZATIONAL MODEL
# ============================================================

chi_org <- chisq.test(
  tab_org
)

chi_org


# Expected frequencies

chi_org$expected


# Effect size

cramer_org <- DescTools::CramerV(
  tab_org
)

cramer_org

# 28. EXTERNAL CHARACTERIZATION OF CLUSTERS
# ============================================================

cluster_external_summary <- cluster_external %>%
  
  group_by(
    cluster
  ) %>%
  
  summarise(
    
    # Advanced complexity
    MACA_pct = mean(
      MACA == "si",
      na.rm = TRUE
    ) * 100,
    
    # Number of chronic diseases
    GMA_N_CRONIQUES_mean = mean(
      GMA_N_CRONIQUES,
      na.rm = TRUE
    ),
    
    GMA_N_CRONIQUES_sd = sd(
      GMA_N_CRONIQUES,
      na.rm = TRUE
    ),
    
    # Distance
    Walking_distance_mean = mean(
      Walking_distance,
      na.rm = TRUE
    ),
    
    Walking_distance_sd = sd(
      Walking_distance,
      na.rm = TRUE
    ),
    
    # Time enrolled in ATDOM
    years_home_based_mean = mean(
      years_home_based,
      na.rm = TRUE
    ),
    
    years_home_based_sd = sd(
      years_home_based,
      na.rm = TRUE
    ),
    
# Severe respiratory disease
    respiratory_failure_pct = mean(
      respiratory_failure == "si",
      na.rm = TRUE
    ) * 100,
    
    n = n(),
    
    .groups = "drop"
  ) %>%
  
  mutate(
    across(
      where(is.numeric),
      ~ round(.x, 2)
    )
  )

cluster_external_summary

#Table

metadata_dict_inicial <- read_csv2(here("data", "metadata_dict_inicial.csv"))

df_cluster_desc <- apply_all_transformations_inicial(
  df_cluster,
  metadata_dict_inicial
) %>%
  
  left_join(
    df_cluster %>% select(ID, cluster),
    by = "ID"
  ) %>%
  
  mutate(
    
    # Ansiedad o depresión
    anxiety_depression = case_when(
      depression == "si" | anxiety == "si" ~ "si",
      depression == "no" & anxiety == "no" ~ "no",
      TRUE ~ NA_character_
    ),
    
    anxiety_depression = factor(
      anxiety_depression,
      levels = c("no", "si")
    ),
    
    # Cualquier cáncer
    cancer = case_when(
      Cancer == "si" | Metastasic_cancer == "si" ~ "si",
      Cancer == "no" & Metastasic_cancer == "no" ~ "no",
      TRUE ~ NA_character_
    ),
    
    cancer = factor(
      cancer,
      levels = c("no", "si")
    )
  )

desc_cluster <- descrTable(
  cluster ~ .,
  data = df_cluster_desc,
  byrow = F,
  show.all = F,
  chisq.test.perm = TRUE,
  method = 2,
  hide = "no",
  include.miss = TRUE,
  extra.labels = c("", "", "", "")
)

desc_cluster

export2md(
  desc_cluster,
  format = "html"
)


saveRDS(df_cluster_desc,here("data","processed","Cluster_id"))

#Heat map
summary(df_cluster_desc)

domain_vars <- c(
  "TIRS_cat",
  "living_alone",
  "MACA",
  "heart_failure",
  "ischemic_heart",
  "dementia",
  "parkinson",
  "stroke",
  "anxiety_depression",
  "diabetes",
  "ckd",
  "copd",
  "cancer"
)

# 2. Calcular prevalencia de cada condición por cluster
# ------------------------------------------------------------

perfil <- df_cluster_desc %>%
  group_by(cluster) %>%
  summarise(
    across(
      all_of(domain_vars),
      ~ mean(.x == "si", na.rm = TRUE)
    ),
    .groups = "drop"
  )

perfil

# ------------------------------------------------------------
# 3. Convertir a matriz para pheatmap
# ------------------------------------------------------------

mat <- as.matrix(
  perfil[, -1]
)

rownames(mat) <- c(
  "Lower-burden,\nfunctionally preserved",
  "Neurocognitive–\nhigh dependency",
  "Cardiometabolic–respiratory\nmultimorbidity"
)

colnames(mat) <- c(
  "Heart failure",
  "Ischemic heart disease",
  "Dementia",
  "Parkinson disease",
  "Stroke",
  "Anxiety/depression",
  "Diabetes",
  "Chronic kidney disease",
  "COPD",
  "Cancer"
)

# ------------------------------------------------------------
# 4. Crear etiquetas con porcentajes
# ------------------------------------------------------------

labels_mat <- matrix(
  paste0(
    round(mat * 100, 1),
    "%"
  ),
  nrow = nrow(mat),
  ncol = ncol(mat)
)

# ------------------------------------------------------------
# 5. Guardar heatmap a 300 dpi
# ------------------------------------------------------------

png(
  filename = here(
    "Output",
    "Cluster",
    "cluster_heatmap.png"
  ),
  units = "cm",
  width = 29.5,
  height = 16,
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
  
  breaks = seq(
    0,
    1,
    length.out = 101
  ),
  
  scale = "none",
  
  border_color = NA,
  
  legend = FALSE,
  
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  
  fontsize = 10,
  fontsize_row = 11,
  fontsize_col = 10,
  
  angle_col = 0,
  display_numbers = labels_mat,
  
  number_color = "black",
  
  main =
    "Clinical phenotypes among patients receiving home-based primary care"
)

dev.off()
