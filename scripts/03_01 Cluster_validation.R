# ============================================================
# 10_cluster_validation.R
# Fowlkes-Mallows validation for FAMD-HCPC clustering
# ============================================================

source(here("scripts", "00_0 setup.R"))

res_famd_original <-readRDS(here("data","processed","res_famd.rds"))
res_hcpc_original <-readRDS(here("data","processed","res_hcpc.rds"))


# ============================================================
# 1. Load clustering dataset
# ============================================================

cluster_data<-readRDS(here("data", "processed", "cluster_data.rds"))%>%
  mutate(living_alone = case_when(
      living_alone == "Yes" ~ "si",
      living_alone == "No" ~ "no",
      TRUE ~ NA_character_)%>%
      as.factor()
  )%>%
  select(ID,BARTHEL,incontinence_cat,TIRS_num,living_alone,
         heart_failure,ischemic_heart,dementia,parkinson,
         stroke,anxiety_depression,Arthritis_osteoarthritis,
         diabetes,ckd,copd,Cancer)

# Prepare clustering dataset
id_var <- "ID"

famd_vars  <- setdiff(
  names(cluster_data),
  "ID"
)
# 3. Dataset for stability analysis
# ============================================================

df_famd <- cluster_data %>%
  select(
    all_of(id_var),
    all_of(famd_vars)
  )

nb <- readRDS(
  here("data", "processed", "MissingFAMD.RDS")
)

# ============================================================
# 6. Test one 80% subsample
# ============================================================

# Guardar la pertenencia original junto al ID:

original_clusters <- tibble(
  ID = df_famd[[id_var]],
  cluster_original = factor(
    res_hcpc_original$data.clust$clust
  )
)

# 4. Repeated subsampling and reclustering
# ============================================================

set.seed(123)

n_iter <- 100
subsample_prop <- 0.80

fm_results <- map_dfr(1:n_iter, function(i) {
  
  message("Iteration ", i, " of ", n_iter)

# Random 80% subsample
    
  sampled_ids <- sample(
    df_famd[[id_var]],
    size = floor(nrow(df_famd) * subsample_prop),
    replace = FALSE
  )
  
  df_sub <- df_famd %>%
    filter(.data[[id_var]] %in% sampled_ids)

# Re-estimate FAMD

  # Dataset containing missing values
  famd_sub <- df_sub %>%
    select(all_of(famd_vars))
  
  # Imputation within the subsample
  imp_sub <- imputeFAMD(
    famd_sub,
    ncp = nb$ncp
  )
  
famd_sub_imputed <- imp_sub$completeObs
  
  # FAMD
  res_famd_sub <- FAMD(
    famd_sub_imputed,
    ncp = 5,
    graph = FALSE
  )
  
  # HCPC
  res_hcpc_sub <- HCPC(
    res_famd_sub,
    nb.clust = 4,
    graph = FALSE
  )
# Cluster assignments in subsample
    
  sub_clusters <- tibble(
    ID = df_sub[[id_var]],
    cluster_resampled = factor(
      res_hcpc_sub$data.clust$clust
    )
  )

# Compare with original classification
  
  comparison <- original_clusters %>%
    inner_join(
      sub_clusters,
      by = "ID"
    )

# Fowlkes-Mallows index
  
  fm_value <- fowlkes_mallows(
    comparison$cluster_original,
    comparison$cluster_resampled
  )
  
  tibble(
    iteration = i,
    n_patients = nrow(comparison),
    fowlkes_mallows = fm_value,
    comparison = list(comparison)
  )
})

# ============================================================
# 5. Summary of cluster stability
# ============================================================


fm_summary <- fm_results %>%
  summarise(
    iterations = n(),
    mean_FM = mean(fowlkes_mallows, na.rm = TRUE),
    sd_FM = sd(fowlkes_mallows, na.rm = TRUE),
    median_FM = median(fowlkes_mallows, na.rm = TRUE),
    p25_FM = quantile(fowlkes_mallows, 0.25, na.rm = TRUE),
    p75_FM = quantile(fowlkes_mallows, 0.75, na.rm = TRUE),
    min_FM = min(fowlkes_mallows, na.rm = TRUE),
    max_FM = max(fowlkes_mallows, na.rm = TRUE)
  )

fm_summary

# ============================================================
# 6. Plot distribution of Fowlkes-Mallows index
# ============================================================

p_fm <- ggplot(
  fm_results,
  aes(x = fowlkes_mallows)
) +
  geom_histogram(
    bins = 20,
    color = "black",
    fill = "grey80"
  ) +
  geom_vline(
    xintercept = median(fm_results$fowlkes_mallows, na.rm = TRUE),
    linetype = "dashed",
    linewidth = 1
  ) +
  labs(
    title = "Stability of the 4-cluster solution",
    x = "Fowlkes-Mallows index",
    y = "Number of resampling iterations"
  ) +
  theme_classic(base_size = 12)


# ============================================================
# 7. Save outputs
# ============================================================

saveRDS(
  fm_summary,
  here("Output","Cluster", "fm_summary.rds")
)

write_xlsx(
  fm_summary,
  here("Output","Cluster", "fm_summary..xlsx")
)

ggsave(
  filename = here("Output","Cluster", "fowlkes_mallows_stability.png"),
  plot = p_fm,
  width = 8,
  height = 5,
  dpi = 300
)
