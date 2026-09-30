# ============================================================
# 03_0 Cluster case_mix selection analysis.R
# ============================================================
library(here)
source(here("scripts", "00_0 setup.R"))

# LOAD AND PREPARE DATA

DF_Inicial <- readRDS(here("data","processed","DF_pacientes_inicial.RDS"))%>%
  mutate(ID=as.character(ID))

df<-readRDS(here("data","processed","DF_pacientes_inicial_transformed.RDS"))%>%
      mutate(ID=as.character(ID))

# 2. CREATE DATASET FOR CLUSTER ANALYSIS

TB_FAMD <- df%>%
  select(c(1:5,8,12,13,16,17,19,20,21,23:45))%>%
  left_join(DF_Inicial[,c(1,5,8,9,11,22,24,29)],
            by=c("ID"))%>%
  mutate(SEXE=as.factor(SEXE),
         # GMA level.NOT an active clustering variable.
         # Retained for external characterization.
         
         GMA_PNIV = factor(GMA_PNIV, ordered = TRUE),
         
         # Correct anomalous TIRS value
         
         TIRS_num=ifelse(TIRS_num==0.64,0,TIRS_num)
         )%>%
  rename(BRADEN=BRADEN.x)%>%
  select(-sex_female,-PHC_name)

# DEFINE ACTIVE AND EXTERNAL VARIABLES

# Active variables:
# These variables DEFINE the patient profiles.

vars_famd_core <- c(
  "BARTHEL",
  "incontinence_cat",
  
  "TIRS_num",
  "living_alone",
  
  "heart_failure",
  "ischemic_heart",

  "dementia",
  "parkinson",
  "stroke",
  "anxiety_depression",
  
  "Arthritis_osteoarthritis",

  "diabetes",
  "ckd",
  
  "copd",

  "Cancer")

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
  "SEXE",
  "Edat")


# 5. CREATE ANALYTIC DATASET

TB_FAMD_analysis <- TB_FAMD %>%
  select(
    ID,
    all_of(vars_famd_core),
    all_of(vars_sup)
  )

saveRDS(TB_FAMD_analysis, here("data", "processed", "cluster_data.rds"))

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

# FAMD EIGENVALUES
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

write.xlsx(famd_contributions,here("Output","Cluster", "famd_contributions.xlsx"))

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
  graph = F
)

saveRDS(res_famd,here("data","processed","res_famd.rds"))

# 16. HCPC
# Automatic selection of number of clusters.

set.seed(1234)

res_hcpc <- HCPC(
  res_famd,
  nb.clust = 4,
  graph = FALSE
)

saveRDS(res_hcpc,here("data","processed","res_hcpc.rds"))

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

# ADD CLUSTER ASSIGNMENT
# ============================================================

# Generate an ID-cluster key.
#
# This is safer than assuming that TB_FAMD_analysis and df
# always have exactly the same row order.

TB_FAMD_analysis<-readRDS(here("data", "processed", "cluster_data.rds"))

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

saveRDS(df_cluster,here("data","cluster","df_Cluster"))

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

# Characterization of the 4 phenotypes

metadata_dict_inicial <- read_csv2(
  here("data", "metadata_dict_inicial.csv")
)

# Apply transformations FIRST

df_cluster_desc <-df_cluster%>% #apply_all_transformations_inicial(
  #df_cluster,
  #metadata_dict_inicial
#)%>%
  #left_join(df_cluster[,c(1,46)],
  #          by="ID")%>%
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
    ))%>%
  select(-GMA_groups)
    
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

export2md(
  desc_cluster,
  format = "html"
)

export2html(
  desc_cluster,
  format = "html",
  "desc_cluster.html"
)

export2xls(
  desc_cluster,
  file = here("Output", "Tables", "desc_cluster.xlsx")
)

saveRDS(df_cluster_desc,here("data","processed","df_cluster_desc.rds"))
