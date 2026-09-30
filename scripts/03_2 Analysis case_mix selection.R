# ============================================================
# 03_2 Analysis case_mix selection.R
# ============================================================

source(here("scripts", "00_0 setup.R"))

df<-readRDS(here("data","cluster","df_cluster_desc"))

UC<-readRDS(here("data","SF","BCN_adreces_users_SF_included_Data_table.rds"))%>%
  mutate(ID=as.character(ID))

SES<-readRDS(here("data","processed","Data_SES_UC.rds"))

df<-df%>%
  left_join(UC[,c(2,29)],
            by="ID")%>%
  left_join(SES,
            by="Seccio_Censal")


#Descriptius raw i proporcio

case_prop <- df %>%
  count(Home_based_PHC_org, cluster) %>%
  group_by(Home_based_PHC_org) %>%
  mutate(
    total = sum(n),
    pct = 100 * n / total
  ) %>%
  ungroup() %>%
  mutate(
    value = sprintf("%d (%.1f%%)", n, pct)
  ) %>%
  select(Home_based_PHC_org, cluster, value) %>%
  pivot_wider(
    names_from = cluster,
    values_from = value,
    names_prefix = "Cluster_"
  )

write.xlsx(case_prop,here("Output","Tables","Case_mix_org.xlsx"))

df <- df %>%
  mutate(
    Home_based_PHC_org = relevel(Home_based_PHC_org, ref="UAB_consulta"),
    sex_female = case_when(sex_female=="Yes"~1,
                           sex_female=="No"~0)
  )

df %>%
  summarise(
    N = n(),
    age_mean = mean(Edat),
    age_sd = sd(Edat),
    age_median = median(Edat),
    age_q1 = quantile(Edat, 0.25),
    age_q3 = quantile(Edat, 0.75),
    female = mean(sex_female) * 100
  )

# ============================================================
# MULTILEVEL CASE-MIX ANALYSIS
# Step 1. Check hierarchical data structure
# ============================================================
# Patients are nested within census units (UC).
#
# Patient level:
#   - phenotype (cluster)
#   - age
#   - sex
#
# Census-unit level:
#   - area-level SES (Index_SES)
#   - random intercept for census unit
#
# Organisational model is determined at a higher territorial
# level and is included as the main fixed exposure.


# Number of patients and census units


df %>%
  summarise(
    n_patients = n(),
    n_census_units = n_distinct(Seccio_Censal)
  )

# ============================================================
# Distribution of patients across census units
# ============================================================

df %>%
  count(
    Seccio_Censal,
    name = "n_patients"
  )%>%
  summarise(
    n_UC = n(),
    min = min(n_patients),
    Q1 = quantile(n_patients, 0.25),
    median = median(n_patients),
    mean = mean(n_patients),
    Q3 = quantile(n_patients, 0.75),
    max = max(n_patients)
  )

#cuántas UC tienen muy pocos pacientes:

df %>%
  count(
    Seccio_Censal,
    name = "n_patients"
  )%>%
  summarise(
    n_1 = sum(n_patients == 1),
    n_2_4 = sum(n_patients >= 2 & n_patients <= 4),
    n_5_9 = sum(n_patients >= 5 & n_patients <= 9),
    n_10_plus = sum(n_patients >= 10)
  )

# Check number of census units in the case-mix dataset
# ============================================================

n_distinct(df$Seccio_Censal)

# Census units by organisational model

df %>%
  count(
    Home_based_PHC_org,
    Seccio_Censal,
    name = "n_patients"
  )%>%
  summarise(
    n_UC = n(),
    Total_patiens = sum(n_patients),
    min = min(n_patients),
    Q1 = quantile(n_patients, 0.25),
    median = median(n_patients),
    mean = mean(n_patients),
    Q3 = quantile(n_patients, 0.75),
    max = max(n_patients),
    .by = Home_based_PHC_org
  )

# ============================================================
# Step 4. Mean-center age
# ============================================================
# Centering age improves interpretation of model intercepts.
#
# Edat_c = 0 corresponds to a patient of average age.
#
# The age coefficient still represents the change associated
# with a 1-year increase in age.

mean_age <- mean(
  df$Edat,
  na.rm = TRUE
)

df <- df %>%
  mutate(
    Edat_c = Edat - mean_age
  )

mean(df$Edat_c)

# Standardize area-level SES at census-unit level
# ============================================================
# First obtain one SES observation per census unit.
#
# This prevents census units with more home-care patients
# from receiving greater weight when calculating the mean
# and standard deviation of the contextual SES index.

SES_UC <- df %>%
  distinct(
    Seccio_Censal,
    Index_SES
  )

SES_mean <- mean(
  SES_UC$Index_SES,
  na.rm = TRUE
)

SES_sd <- sd(
  SES_UC$Index_SES,
  na.rm = TRUE
)

SES_mean
SES_sd

df<- df %>%
  mutate(
    Index_SES_z =
      (Index_SES - SES_mean) / SES_sd
  )

summary(df$Index_SES_z)
#
# ============================================================
# MM0 - Empty multilevel multinomial model
# ============================================================
# Outcome:
#   4-category phenotype
#
# Random effect:
#   census-unit-specific intercept
#
# This model estimates between-census-unit heterogeneity
# in phenotype membership before introducing covariates.


MM0 <- mblogit(
  formula =
    cluster ~ 1,
  
  random = ~ 1 | Seccio_Censal,
  
  data = df
  )

summary(MM0)
logLik(MM0)

# ============================================================
# MM0 - Store census-unit random-effect variances
# ============================================================
# The diagonal elements of the random-effect covariance matrix
# represent between-census-unit variance for each multinomial
# logit relative to the reference phenotype (Lower complexity).

MM0_random_variance <- tibble(
  comparison = c(
    "Social vulnerability vs Lower complexity",
    "High multimorbidity vs Lower complexity",
    "Neurocognitive-functional dependency vs Lower complexity"
  ),
  variance_UC = c(
    0.0549182,
    0.0475436,
    0.0491648
  )
)

MM0_random_variance

# ============================================================
# MM1 - Age- and sex-adjusted multilevel multinomial model
# ============================================================
# Estimate differences in phenotype membership across
# organisational models after adjustment for age and sex.
#
# A census-unit random intercept accounts for residual
# correlation among patients sharing the same residential
# context.
#
# Reference outcome:
#   Lower complexity
#
# Reference organisational model:
#   UAB_consulta (Traditional)

MM1 <- mblogit(
  formula =
    cluster ~
    Home_based_PHC_org +
    Edat_c +
    sex_female,
  
  random = ~ 1 | Seccio_Censal,
  
  data = df
)

summary(MM1)
mclogit:::getSummary.mblogit(MM1, alpha = 0.05)

# ============================================================
# MM0 vs MM1 - Proportional change in random-effect variance
# ============================================================

MM1_random_variance <- tibble(
  comparison = c(
    "Social vulnerability vs Lower complexity",
    "High multimorbidity vs Lower complexity",
    "Neurocognitive-functional dependency vs Lower complexity"
  ),
  
  # Replace with the diagonal variance estimates from summary(MM1)
  variance_UC_MM1 = c(
    NA,
    NA,
    NA
  )
)

variance_comparison <- MM0_random_variance %>%
  left_join(
    MM1_random_variance,
    by = "comparison"
  ) %>%
  mutate(
    PCV_percent =
      100 * (variance_UC - variance_UC_MM1) /
      variance_UC
  )

variance_comparison

# Mean and SD of Index_SES across census units

SES_mean <- mean(
  df$Index_SES,
  na.rm = TRUE
)

SES_sd <- sd(
  df$Index_SES,
  na.rm = TRUE
)

df <- df %>%
  mutate(
    Index_SES_z =
      (Index_SES - SES_mean) / SES_sd
  )

df %>%
  distinct(
    Seccio_Censal,
    Index_SES_z
  ) %>%
  summarise(
    mean_SES_z = mean(Index_SES_z),
    SD_SES_z = sd(Index_SES_z),
    min_SES_z = min(Index_SES_z),
    max_SES_z = max(Index_SES_z)
  )

# ============================================================
# MM2 - Fully adjusted multilevel multinomial model
# ============================================================
# Estimate differences in phenotype membership across
# organisational models after adjustment for:
#
#   Individual level:
#     - age
#     - sex
#
#   Contextual level:
#     - census-unit socioeconomic status
#
# A census-unit random intercept accounts for residual
# correlation among patients sharing the same residential
# context.
#
# Index_SES_z is standardized across census units, so its
# coefficient represents a 1-SD difference in area-level SES.

MM2 <- mblogit(
  formula =
    cluster ~
    Home_based_PHC_org +
    Edat_c +
    sex_female +
    Index_SES_z,
  
  random = ~ 1 | Seccio_Censal,
  
  data = df
)

summary(MM2)
mclogit:::getSummary.mblogit(MM2, alpha = 0.05)


# ============================================================
# Compare census-unit random-effect variance: MM1 vs MM2
# ============================================================
# This descriptive comparison examines how the estimated
# residual between-census-unit heterogeneity changes after
# adding area-level SES to the age- and sex-adjusted model.
#
# Because this is a nonlinear multinomial model, these changes
# should not be interpreted as a formal proportion of variance
# "explained" by SES.

variance_comparison <- tibble(
  phenotype_contrast = c(
    "Social vulnerability vs Lower complexity",
    "High multimorbidity vs Lower complexity",
    "Neurocognitive-functional dependency vs Lower complexity"
  ),
  
  variance_MM1 = c(
    0.050705,
    0.072908,
    0.075346
  ),
  
  variance_MM2 = c(
    0.0234470,
    0.0242211,
    0.0236447
  )
) %>%
  mutate(
    absolute_change = variance_MM2 - variance_MM1,
    relative_change_pct =
      100 * (variance_MM2 - variance_MM1) / variance_MM1
  )

variance_comparison

methods(class = class(MM2)[1])

methods("predict")

# ============================================================
# MM2 - Check prediction using data
# ============================================================
# Create a copy of the analytical dataset and assign all
# patients to the Traditional organisational model.
#
# Age, sex, SES, and census unit remain unchanged.
#
# This is the first step toward obtaining standardized
# (marginal) phenotype probabilities by organisational model.

df_test <- df

df_test$Home_based_PHC_org <- factor(
  "UAB_consulta",
  levels = levels(df$Home_based_PHC_org)
)

# Predict the probability of each phenotype under the
# hypothetical scenario in which all patients are assigned
# to the Traditional organisational model.

pred_test <- predict(
  MM2,
  newdata = df_test,
  type = "response"
)


# Check dimensions

dim(pred_test)


# Check first predicted probabilities

head(pred_test)


# Probabilities should sum to 1 for every patient

range(rowSums(pred_test))

# ============================================================
# MM2 - Standardized phenotype probabilities
# ============================================================
# Estimate standardized probabilities using predictive margins.
#
# For each organisational model:
#   1. retain the observed age, sex, SES, and census unit
#      distribution of the analytical sample;
#   2. assign all patients to the same organisational model;
#   3. predict phenotype probabilities;
#   4. average individual predicted probabilities.
#
# This standardizes comparisons across organisational models
# to the same covariate distribution.


# Store organisational model levels

org_levels <- levels(df$Home_based_PHC_org)

org_levels

# Function to obtain standardized probabilities for one
# organisational model

get_standardized_probs <- function(org) {
  
  # Copy observed analytical population
  newdata <- df
  
  # Assign the same organisational model to all patients
  newdata$Home_based_PHC_org <- factor(
    org,
    levels = org_levels
  )
  
  # Obtain individual predicted phenotype probabilities
  pred <- predict(
    MM2,
    newdata = newdata,
    type = "response"
  )
  
  # Average predicted probabilities across all patients
  tibble(
    Home_based_PHC_org = org,
    cluster = colnames(pred),
    probability = colMeans(pred)
  )
}

# ============================================================
# Obtain standardized probabilities for all organisations
# ============================================================

prob_MM2 <- purrr::map_dfr(
  org_levels,
  get_standardized_probs
)

prob_MM2

prob_MM2 <- prob_MM2 %>%
  mutate(
    probability_pct = probability * 100
  )

prob_MM2

# ============================================================
# MM2 - Cluster bootstrap for standardized probabilities
# ============================================================
# Bootstrap census units rather than individual patients to
# preserve the within-census-unit correlation structure.
#
# Census units are sampled with replacement. When the same
# census unit is selected more than once, each sampled copy
# receives a new identifier so that duplicated clusters are
# treated as independent bootstrap clusters.
#
# For each bootstrap sample:
#   1. resample census units;
#   2. refit the fully adjusted multilevel multinomial model;
#   3. estimate standardized phenotype probabilities for each
#      organisational model;
#   4. store the estimates for subsequent inference.
# ============================================================


# ------------------------------------------------------------
# 1. Function to resample census units
# ------------------------------------------------------------

sample_UC_bootstrap <- function(data) {
  
  # Identify census units in the analytical sample
  UC_ids <- unique(data$Seccio_Censal)
  
  # Sample the same number of census units with replacement
  sampled_UC <- sample(
    UC_ids,
    size = length(UC_ids),
    replace = TRUE
  )
  
  # Reconstruct the bootstrap dataset.
  # Each sampled copy receives a new census-unit identifier.
  purrr::map2_dfr(
    sampled_UC,
    seq_along(sampled_UC),
    function(uc, boot_id) {
      
      data %>%
        filter(Seccio_Censal == uc) %>%
        mutate(
          Seccio_Censal_boot = factor(boot_id)
        )
    }
  )
}


# ------------------------------------------------------------
# 2. Function to refit MM2 in each bootstrap sample
# ------------------------------------------------------------

fit_boot_MM2 <- function(boot_data) {
  
  mclogit::mblogit(
    cluster ~
      Home_based_PHC_org +
      Edat_c +
      sex_female +
      Index_SES_z,
    
    random = ~ 1 | Seccio_Censal_boot,
    
    data = boot_data
  )
}


# ------------------------------------------------------------
# 3. Function to obtain standardized probabilities
# ------------------------------------------------------------
# For each organisational model, all patients in the bootstrap
# sample are hypothetically assigned to that model while their
# observed age, sex, SES, and census-unit characteristics are
# retained.
#
# Individual predicted probabilities are then averaged to
# obtain standardized phenotype probabilities.

get_boot_probs <- function(model, boot_data) {
  
  org_levels <- levels(df$Home_based_PHC_org)
  
  purrr::map_dfr(
    org_levels,
    function(org) {
      
      # Preserve the covariate distribution of the
      # bootstrap sample
      newdata <- boot_data
      
      # Assign all patients to the same organisational model
      newdata$Home_based_PHC_org <- factor(
        org,
        levels = org_levels
      )
      
      # Obtain individual predicted probabilities
      pred <- predict(
        model,
        newdata = newdata,
        type = "response"
      )
      
      # Average predicted probabilities across patients
      tibble(
        Home_based_PHC_org = org,
        cluster = colnames(pred),
        probability = colMeans(pred)
      )
    }
  )
}


# ------------------------------------------------------------
# 4. Run final cluster bootstrap
# ------------------------------------------------------------

set.seed(1234)

B <- 1000

boot_results <- purrr::map_dfr(
  seq_len(B),
  function(b) {
    
    # Display progress every 25 replications
    if (b %% 25 == 0) {
      message(
        "Bootstrap replication ",
        b,
        " / ",
        B
      )
    }
    
    tryCatch({
      
      # Resample census units
      boot_data <- sample_UC_bootstrap(df)
      
      # Refit fully adjusted multilevel multinomial model
      boot_model <- fit_boot_MM2(
        boot_data
      )
      
      # Obtain standardized probabilities
      get_boot_probs(
        boot_model,
        boot_data
      ) %>%
        mutate(
          bootstrap = b
        )
      
    },
    
    # Prevent an occasional failed model from stopping
    # the complete bootstrap procedure
    error = function(e) {
      
      message(
        "Replication ",
        b,
        " failed: ",
        conditionMessage(e)
      )
      
      return(NULL)
    })
  }
)

saveRDS(boot_results,here("data","processed","boot_results.rds"))

# ------------------------------------------------------------
# 5. Check successful bootstrap replications
# ------------------------------------------------------------

successful_boot <- n_distinct(
  boot_results$bootstrap
)

failed_boot <- 1000 - successful_boot

successful_boot
failed_boot


# Each successful replication should contain:
# 4 organisational models × 4 phenotypes = 16 estimates

table(
  table(boot_results$bootstrap)
)

# ============================================================
# MM2 - Bootstrap 95% CIs for standardized probabilities
# ============================================================
# Percentile bootstrap confidence intervals are obtained from
# the empirical distribution of bootstrap estimates.
#
# Point estimates are retained from the original MM2 model;
# bootstrap estimates are used only to quantify uncertainty.
# ============================================================

prob_MM2_CI <- boot_results %>%
  group_by(
    Home_based_PHC_org,
    cluster
  ) %>%
  summarise(
    CI_low = quantile(
      probability,
      probs = 0.025,
      na.rm = TRUE
    ),
    
    CI_high = quantile(
      probability,
      probs = 0.975,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  )

# Combine original standardized probabilities with
# bootstrap confidence intervals

prob_MM2_final <- prob_MM2 %>%
  left_join(
    prob_MM2_CI,
    by = c(
      "Home_based_PHC_org",
      "cluster"
    )
  ) %>%
  mutate(
    probability_pct = probability * 100,
    CI_low_pct = CI_low * 100,
    CI_high_pct = CI_high * 100,
    
    result = sprintf(
      "%.1f (%.1f-%.1f)",
      probability_pct,
      CI_low_pct,
      CI_high_pct
    )
  )


# Display publication-ready estimates

prob_MM2_final %>%
  select(
    cluster,
    Home_based_PHC_org,
    probability_pct,
    CI_low_pct,
    CI_high_pct,
    result
  )

# ============================================================
# Publication-ready table
# Standardized probability, % (95% bootstrap CI)
# ============================================================

Table_MM2 <- prob_MM2_final %>%
  select(
    Home_based_PHC_org,
    cluster,
    result
  ) %>%
  tidyr::pivot_wider(
    names_from = cluster,
    values_from = result
  )

Table_MM2

write.xlsx(Table_MM2,here("Output","Tables","Table_cluster_probability.xlsx"))
# ============================================================
# MM2 - Pairwise differences in standardized probabilities
# ============================================================
# Aim:
#   Compare standardized probabilities of phenotype membership
#   between the 4 home-based PHC organisational models.
#
# Inference:
#   - Point estimates: original fully adjusted MM2 model
#   - 95% CI: percentile census-unit cluster bootstrap
#   - SE: SD of the bootstrap contrast distribution
#   - P values: two-sided tests using bootstrap SEs
#   - Multiplicity: Holm adjustment across the 6 pairwise
#     comparisons within each phenotype
#
# Differences are expressed as percentage points (pp).
# ============================================================


# ------------------------------------------------------------
# Reshape bootstrap standardized probabilities
# ------------------------------------------------------------

boot_wide <- boot_results %>%
  select(
    bootstrap,
    cluster,
    Home_based_PHC_org,
    probability
  ) %>%
  pivot_wider(
    names_from = Home_based_PHC_org,
    values_from = probability
  )


# ------------------------------------------------------------
# Calculate all 6 pairwise contrasts
# within each bootstrap replication
# ------------------------------------------------------------

boot_contrasts <- boot_wide %>%
  transmute(
    bootstrap,
    cluster,
    
    `UAB_consulta - Equip_Atdom` =
      UAB_consulta - Equip_Atdom,
    
    `UAB_consulta - Equip_Inf` =
      UAB_consulta - Equip_Inf,
    
    `UAB_consulta - UAB_consulta_reforc` =
      UAB_consulta - UAB_consulta_reforc,
    
    `Equip_Atdom - Equip_Inf` =
      Equip_Atdom - Equip_Inf,
    
    `Equip_Atdom - UAB_consulta_reforc` =
      Equip_Atdom - UAB_consulta_reforc,
    
    `Equip_Inf - UAB_consulta_reforc` =
      Equip_Inf - UAB_consulta_reforc
  ) %>%
  pivot_longer(
    cols = -c(bootstrap, cluster),
    names_to = "contrast",
    values_to = "difference"
  )


# ------------------------------------------------------------
# Obtain point estimates from the original MM2 model
# ------------------------------------------------------------
# Bootstrap replicates are used to estimate uncertainty.
# Point estimates remain those from the original fitted model.

original_wide <- prob_MM2 %>%
  select(
    cluster,
    Home_based_PHC_org,
    probability
  ) %>%
  pivot_wider(
    names_from = Home_based_PHC_org,
    values_from = probability
  )


contrasts_original <- original_wide %>%
  transmute(
    cluster,
    
    `UAB_consulta - Equip_Atdom` =
      UAB_consulta - Equip_Atdom,
    
    `UAB_consulta - Equip_Inf` =
      UAB_consulta - Equip_Inf,
    
    `UAB_consulta - UAB_consulta_reforc` =
      UAB_consulta - UAB_consulta_reforc,
    
    `Equip_Atdom - Equip_Inf` =
      Equip_Atdom - Equip_Inf,
    
    `Equip_Atdom - UAB_consulta_reforc` =
      Equip_Atdom - UAB_consulta_reforc,
    
    `Equip_Inf - UAB_consulta_reforc` =
      Equip_Inf - UAB_consulta_reforc
  ) %>%
  pivot_longer(
    cols = -cluster,
    names_to = "contrast",
    values_to = "estimate"
  )


# ------------------------------------------------------------
# Percentile cluster-bootstrap 95% CIs
# ------------------------------------------------------------

contrast_CI <- boot_contrasts %>%
  group_by(
    cluster,
    contrast
  ) %>%
  summarise(
    CI_low = quantile(
      difference,
      probs = 0.025,
      na.rm = TRUE
    ),
    
    CI_high = quantile(
      difference,
      probs = 0.975,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  )


# ------------------------------------------------------------
# Bootstrap standard errors
# ------------------------------------------------------------
# The SD of the bootstrap distribution provides the bootstrap
# SE for each pairwise difference.

boot_SE <- boot_contrasts %>%
  group_by(
    cluster,
    contrast
  ) %>%
  summarise(
    boot_SE = sd(
      difference,
      na.rm = TRUE
    ),
    
    n_boot = sum(
      !is.na(difference)
    ),
    
    .groups = "drop"
  )


# Check number of successful bootstrap estimates

boot_SE %>%
  arrange(n_boot)


# ------------------------------------------------------------
# Two-sided P values using bootstrap SEs
# ------------------------------------------------------------

contrast_tests <- contrasts_original %>%
  left_join(
    boot_SE,
    by = c(
      "cluster",
      "contrast"
    )
  ) %>%
  mutate(
    z_boot = estimate / boot_SE,
    
    p_value = 2 * pnorm(
      -abs(z_boot)
    )
  )


# ------------------------------------------------------------
# Holm multiplicity adjustment
# ------------------------------------------------------------
# Holm adjustment is applied separately within each phenotype,
# corresponding to the 6 pairwise comparisons among the
# 4 organisational models.

contrast_tests <- contrast_tests %>%
  group_by(cluster) %>%
  mutate(
    p_holm = p.adjust(
      p_value,
      method = "holm"
    )
  ) %>%
  ungroup()


# ------------------------------------------------------------
# Combine estimates, bootstrap CIs, and adjusted P values
# ------------------------------------------------------------

contr_MM2_final <- contrasts_original %>%
  left_join(
    contrast_CI,
    by = c(
      "cluster",
      "contrast"
    )
  ) %>%
  left_join(
    contrast_tests %>%
      select(
        cluster,
        contrast,
        boot_SE,
        n_boot,
        z_boot,
        p_value,
        p_holm
      ),
    by = c(
      "cluster",
      "contrast"
    )
  ) %>%
  mutate(
    
    # Convert probabilities to percentage points
    difference_pp = estimate * 100,
    CI_low_pp = CI_low * 100,
    CI_high_pp = CI_high * 100,
    boot_SE_pp = boot_SE * 100,
    
    # Publication-ready estimate and CI
    result = sprintf(
      "%.1f (%.1f to %.1f)",
      difference_pp,
      CI_low_pp,
      CI_high_pp
    ),
    
    # Publication-ready unadjusted P value
    p_value_display = case_when(
      p_value < 0.001 ~ "<.001",
      TRUE ~ sprintf("%.3f", p_value)
    ),
    
    # Publication-ready Holm-adjusted P value
    p_holm_display = case_when(
      p_holm < 0.001 ~ "<.001",
      TRUE ~ sprintf("%.3f", p_holm)
    )
  )


# ------------------------------------------------------------
# Inspect complete results
# ------------------------------------------------------------

print(
  contr_MM2_final %>%
    select(
      cluster,
      contrast,
      difference_pp,
      CI_low_pp,
      CI_high_pp,
      boot_SE_pp,
      n_boot,
      p_value,
      p_holm
    ),
  n = 24
)


# ------------------------------------------------------------
# Publication-ready eTable
# ------------------------------------------------------------

contr_MM2_table <- contr_MM2_final %>%
  select(
    cluster,
    contrast,
    result,
    p_holm_display
  ) %>%
  rename(
    Phenotype = cluster,
    Comparison = contrast,
    `Difference, percentage points (95% bootstrap CI)` = result,
    `Holm-adjusted P value` = p_holm_display
  )%>%
  mutate(
    Comparison = recode(Comparison,
      "UAB_consulta - Equip_Atdom" =
        "Traditional team-based - Multidisciplinary home care unit",
      
      "UAB_consulta - Equip_Inf" =
        "Traditional team-based - Nurse-led home care unit",
      
      "UAB_consulta - UAB_consulta_reforc" =
        "Traditional team-based - Reinforced team-based",
      
      "Equip_Atdom - Equip_Inf" =
        "Multidisciplinary home care unit - Nurse-led home care unit",
      
      "Equip_Atdom - UAB_consulta_reforc" =
        "Multidisciplinary home care unit - Reinforced team-based",
      
      "Equip_Inf - UAB_consulta_reforc" =
        "Nurse-led home care unit - Reinforced team-based"
    )
  )

print(
  contr_MM2_table,
  n = 24
)


# ------------------------------------------------------------
# Export complete analytical results
# ------------------------------------------------------------

write.xlsx(
  contr_MM2_table,
  here(
    "Output",
    "Tables",
    "Contrasts_prob_cluster.xlsx"
  ),
  overwrite = TRUE
)



# ============================================================
# Figure X - Standardized phenotype probabilities
# ============================================================

prob_MM2_plot <- prob_MM2_final %>%
  mutate(
    
    # Publication-ready labels
    Home_based_PHC_org = recode(
      Home_based_PHC_org,
      "UAB_consulta" = "Traditional",
      "Equip_Atdom" = "Dedicated\nhome-care team",
      "Equip_Inf" = "Nurse-led",
      "UAB_consulta_reforc" = "Reinforced\ntraditional"
    ),
    
    # Order organisational models
    Home_based_PHC_org = factor(
      Home_based_PHC_org,
      levels = c(
        "Traditional",
        "Dedicated\nhome-care team",
        "Nurse-led",
        "Reinforced\ntraditional"
        
      )
    ),
    
    # Numeric positions allow categories to be placed closer together
    x_pos = as.numeric(Home_based_PHC_org),
    
    # Order phenotype panels
    cluster = factor(
      cluster,
      levels = c(
        "Lower complexity",
        "Social vulnerability",
        "High multimorbidity",
        "Neurocognitive-functional dependency"
      )
    )
  )



Figure_case_mix <- ggplot(
  prob_MM2_plot,
  aes(
    x = x_pos,
    y = probability_pct
  )
) +
  
  # Point estimates
  geom_point(
    size = 3
  ) +
  
  # 95% cluster-bootstrap CIs
  geom_errorbar(
    aes(
      ymin = CI_low_pct,
      ymax = CI_high_pct
    ),
    width = 0.08,
    linewidth = 0.8
  ) +
  
  # Phenotype panels
  facet_wrap(
    ~ cluster,
    ncol = 2
  ) +
  
  # ----------------------------------------------------------
# Compact x-axis
# ----------------------------------------------------------

scale_x_continuous(
  breaks = 1:4,
  labels = c(
    "Traditional",
    "Dedicated\nhome-care team",
    "Nurse-led",
    "Reinforced\ntraditional"
  ),
  
  # Increasing these limits creates margins at both sides,
  # bringing the four categories visually closer together
  limits = c(0.4, 4.6),
  
  expand = c(0, 0)
) +
  
  # Common y-axis
  scale_y_continuous(
    limits = c(0, 50),
    breaks = seq(0, 50, 10),
    expand = c(0, 0)
  ) +
  
  labs(
    x = NULL,
    y = "Standardized probability, %",
    title = "Standardized Probabilities of Patient Phenotype Membership by Home-Based Primary Care Organizational Model"
  ) +
  
  theme_minimal(
    base_size = 11
  ) +
  
  theme(
    
    # All text black
    text = element_text(
      colour = "black",
      size = 11
    ),
    
    axis.text = element_text(
      colour = "black",
      size = 11
    ),
    
    axis.text.x = element_text(
      colour = "black",
      size = 11,
      hjust = 0.5
    ),
    
    axis.title.y = element_text(
      colour = "black",
      size = 11
    ),
    
    strip.text = element_text(
      colour = "black",
      size = 11
    ),
    
    plot.title = element_text(
      colour = "black",
      size = 13,
      hjust = 0
    ),
    
    # --------------------------------------------------------
    # Black frame around each phenotype panel
    # --------------------------------------------------------
    
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      linewidth = 0.6
    ),
    
    # Reduce space between the 4 panels
    panel.spacing = unit(
      0.6,
      "lines"
    ),
    
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    
    theme(
      
      text = element_text(
        colour = "black",
        size = 11
      ),
      
      axis.text = element_text(
        colour = "black",
        size = 11
      ),
      
      axis.text.x = element_text(
        colour = "black",
        size = 11,
        hjust = 0.5
      ),
      
      axis.title.y = element_text(
        colour = "black",
        size = 11
      ),
      
      strip.text = element_text(
        colour = "black",
        size = 11
      ),
      
      plot.title = element_text(
        colour = "black",
        size = 11,
        hjust = 0
      ),
      
      # Remove vertical grid lines
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      
      # Keep horizontal grid lines
      panel.grid.major.y = element_line(
        linewidth = 0.3
      ),
      
      panel.grid.minor.y = element_blank(),
      
      # Frame each phenotype panel
      panel.border = element_rect(
        colour = "black",
        fill = NA,
        linewidth = 0.6
      ),
      
      panel.spacing = unit(
        0.6,
        "lines"
      ),
      
      legend.position = "none"
    )
  )

Figure_case_mix

ggsave(
  filename = here("Output","Figures", "Figure_case_mix.png"),
  plot = Figure_case_mix,
  width = 297,
  height = 210,
  units = "mm",
  dpi = 300,
  bg = "white"
)

