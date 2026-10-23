# ====
# 03_0v2 Cluster case_mix selection analysis v2.R
# ====

source(here("scripts", "00_0 setup.R"))

# LOAD AND PREPARE DATA

DF_Inicial <- readRDS(here("data","processed","DF_pacientes_inicial.RDS"))%>%
  mutate(ID=as.character(ID))

df<-readRDS(here("data","processed","DF_pacientes_inicial_transformed.RDS"))%>%
  mutate(ID=as.character(ID))

UC<-readRDS(here("data","SF","BCN_adreces_users_SF_included_Data_table.rds"))%>%
  mutate(ID=as.character(ID))

SES<-readRDS(here("data","processed","Data_SES_UC.rds"))


df<-df%>%
  left_join(UC[,c(2,29)],
            by="ID")%>%
  left_join(SES,
            by="Seccio_Censal")


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


df<- df %>%
  mutate(
    Index_SES_z =
      (Index_SES - SES_mean) / SES_sd
  )



# 2. CREATE DATASET FOR CASE-MIX ANALYSIS
names(df)
TB_case_mix <- df %>%
  
  # Add variables from the original clinical database
  left_join(
    DF_Inicial %>%
      dplyr::select(
        ID,
        UP_ABS,
        GMA_PNIV,
        VISITES_ANY,
        BARTHEL,
        PFEIFER,
        BRADEN_raw = `BRADEN.x`
      ),
    by = "ID",
    relationship = "many-to-one"
  ) %>%
  
  mutate(
    
    # Morbidity category
    GMA_PNIV = as.factor(GMA_PNIV),
    
    # Correct previously identified anomalous value
    TIRS_num = if_else(
      !is.na(TIRS_num) & TIRS_num == 0.64,
      0,
      TIRS_num
    )
    
  )

# 4. EXPLORE CASE-MIX VARIABLES

variables_case_mix <- c(
  "TIRS_num",
  "living_alone",
  "BARTHEL",
  "dementia",
  "parkinson",
  "GMA_PNIV"
)

# 5. MISSING DATA BY PRIMARY CARE CENTER

TB_case_mix %>%
  group_by(
    UP_ABS,
    Home_based_PHC_org
  ) %>%
  summarise(
    n = n(),
    
    across(
      all_of(variables_case_mix),
      ~ round(mean(is.na(.x)) * 100, 1),
      .names = "missing_{.col}"
    ),
    
    .groups = "drop"
  )%>%
  print(x, width = Inf)

# ====
# STEP 4. DEFINE CASE-MIX INDICATORS
# ====

TB_case_mix <- TB_case_mix %>%
  mutate(
    
    # SOCIAL NEEDS
    # Positive TIRS score
    Social_positive = TIRS_num > 0 | living_alone=="Yes" ,
    
    # FUNCTIONAL NEEDS
    # Severe or total dependency (Barthel <40)
    Barthel_severe = BARTHEL < 40,
    
    # NEUROCOGNITIVE NEEDS
    # Registered dementia diagnosis
    Dementia_parkinson = dementia == "si" | parkinson == "si"
    
  )

# ====
# STEP 7. CASE-MIX COMPARISON BY ORGANIZATIONAL MODEL
# ====

TB_case_mix <- TB_case_mix %>%
  mutate(
    
    # ORGANIZATIONAL MODEL
    Home_based_PHC_org = factor(
      case_match(
        as.character(Home_based_PHC_org),
        "UAB_consulta" ~ "Traditional team-based",
        "UAB_consulta_reforc" ~ "Reinforced team-based",
        "Equip_Inf" ~ "Nurse-led unit",
        "Equip_Atdom" ~ "Multidisciplinary unit",
        .default = as.character(Home_based_PHC_org)
      ),
      levels = c(
        "Traditional team-based",
        "Reinforced team-based",
        "Nurse-led unit",
        "Multidisciplinary unit"
      )
    ),
    
    # SOCIAL NEEDS
    # Positive TIRS score OR living alone
    Social_positive = factor(
      case_when(
        TIRS_num > 0 | living_alone == "Yes" ~ "Yes",
        TIRS_num == 0 & living_alone == "No" ~ "No",
        TRUE ~ NA_character_
      ),
      levels = c("No", "Yes")
    ),
    
    # FUNCTIONAL NEEDS
    # Severe/total dependency: Barthel < 40
    Barthel_severe = factor(
      case_when(
        is.na(BARTHEL) ~ NA_character_,
        BARTHEL < 40 ~ "Yes",
        TRUE ~ "No"
      ),
      levels = c("No", "Yes")
    ),
    
    # NEURODEGENERATIVE CONDITIONS
    # Dementia OR Parkinson disease
    Dementia_parkinson = factor(
      case_when(
        dementia == "si" | parkinson == "si" ~ "Yes",
        dementia == "no" & parkinson == "no" ~ "No"),
      levels = c("No", "Yes")
    )
  )

variables_case_mix_final <- c(
  "Social_positive",
  "Barthel_severe",
  "Dementia_parkinson",
  "GMA_PNIV"
)

# ====
# DESCRIPTIVE TABLE BY ORGANIZATIONAL MODEL
# ====


df_cluster_desc <-TB_case_mix

case_mix_table <- descrTable(
  Home_based_PHC_org ~
    Social_positive +
    Barthel_severe +
    Dementia_parkinson +
    GMA_PNIV,
  
  data = TB_case_mix,
  
  include.miss = TRUE,
  show.all = FALSE,
  chisq.test.perm = TRUE,

  show.n = TRUE,
  show.p.overall = TRUE,
  show.p.mul = FALSE,
  digits = 2,
  digits.p = 3
)

export2md(case_mix_table,format = "html")

export2xls(
  case_mix_table,
  file = here("Output", "Tables", "case_mix_table2.xlsx")
)


TB_case_mix <- TB_case_mix %>%
  mutate(
    Edat_10 = (Edat - 85) / 10
  )


### Modeling case-mix. ### 

#Social

MM_social <- glmer(
  Social_positive ~
    Home_based_PHC_org +
    Edat_10 +
    sex_female +
    Index_SES_z +
  (1 | Seccio_Censal),
  
  data = TB_case_mix,
  family = binomial(link = "logit"),
  
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(MM_social)
VarCorr(MM_social)
isSingular(MM_social)

# Marginally standardized probabilities
emm_social <- emmeans(
  MM_social,
  ~ Home_based_PHC_org,
  counterfactuals = "Home_based_PHC_org"
)

# Estimated probabilities and 95% CI
summary(
  emm_social,
  infer = c(TRUE, TRUE)
)

# Comparisons against traditional team-based model
contr_social <- contrast(
  emm_social,
  method = "trt.vs.ctrl",
  ref = 1,
  adjust = "holm"
)

summary(
  contr_social,
  infer = c(TRUE, TRUE)
)


##BARTHEL
MM_functional <- glmer(
  Barthel_severe ~
    Home_based_PHC_org +
    Edat_10 +
    sex_female +
    Index_SES_z +
    (1 | Seccio_Censal),
  
  data = TB_case_mix,
  family = binomial(link = "logit"),
  
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(MM_functional)
isSingular(MM_functional)

# Marginally standardized probabilities
emm_functional <- emmeans(
  MM_functional,
  ~ Home_based_PHC_org,
  counterfactuals = "Home_based_PHC_org"
)

# Estimated probabilities and 95% CI
summary(
  emm_functional,
  infer = c(TRUE, TRUE)
)

# Comparisons against traditional team-based model
contr_functional <- contrast(
  emm_functional,
  method = "trt.vs.ctrl",
  ref = 1,
  adjust = "holm"
)

summary(
  contr_functional,
  infer = c(TRUE, TRUE)
)

#Neurodegenerative

MM_neuro <- glmer(
  Dementia_parkinson ~
    Home_based_PHC_org +
    Edat_10 +
    sex_female +
    Index_SES_z +
    (1 | Seccio_Censal),
  
  data = TB_case_mix,
  family = binomial(link = "logit"),
  
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

# Model diagnostics
summary(MM_neuro)
VarCorr(MM_neuro)
isSingular(MM_neuro)


emm_neuro <- emmeans(
  MM_neuro,
  ~ Home_based_PHC_org,
  counterfactuals = "Home_based_PHC_org"
)

# Adjusted prevalence and 95% CI
summary(
  emm_neuro,
  infer = c(TRUE, FALSE)
)

# Comparisons against traditional model
contr_neuro <- contrast(
  emm_neuro,
  method = "trt.vs.ctrl",
  ref = 1,
  adjust = "holm"
)

summary(
  contr_neuro,
  infer = c(TRUE, TRUE)
)

#complexity


TB_case_mix <- TB_case_mix %>%
  mutate(
    GMA_PNIV = factor(
      as.character(GMA_PNIV),
      levels = c("1", "2", "3", "4"),
      ordered = TRUE
    )
  )

MM_complexity <- clmm(
  GMA_PNIV ~
    Home_based_PHC_org +
    Edat_10 +
    sex_female +
    Index_SES_z +
    (1 | Seccio_Censal),
  
  data = TB_case_mix,
  link = "logit",
  Hess = TRUE,
  nAGQ = 1
)

# Model results
summary(MM_complexity)

# Optimization convergence
MM_complexity$optRes$convergence

# Estimated random-effect variance
VarCorr(MM_complexity)

# Number of observations
nobs(MM_complexity)



# ====
# CLINICAL COMPLEXITY (GMA_PNIV)
# Marginal standardization and adjusted comparisons
# ====

# ------------------------------------------------------------
# 1. Analytical sample
# ------------------------------------------------------------

vars_model <- c(
  "GMA_PNIV",
  "Home_based_PHC_org",
  "Edat_10",
  "sex_female",
  "Index_SES_z",
  "Seccio_Censal"
)

datos_GMA <- TB_case_mix[
  complete.cases(TB_case_mix[, vars_model]),
  ,
  drop = FALSE
]

# Organizational model levels
org_levels <- levels(
  datos_GMA$Home_based_PHC_org
)

# Verify reference category
stopifnot(
  org_levels[1] == "Traditional team-based"
)


# ------------------------------------------------------------
# 2. Extract model parameters
# ------------------------------------------------------------

# Regression coefficients
beta <- MM_complexity$beta

# Ordinal thresholds
alpha <- MM_complexity$alpha

# Parameter vector
theta <- c(alpha, beta)

# Variance-covariance matrix
V <- as.matrix(vcov(MM_complexity))

V <- V[
  names(theta),
  names(theta),
  drop = FALSE
]

# Extract standard errors
SE <- sqrt(diag(vcov(MM_complexity))[names(alpha)])

# Calculate z statistics and P values
z <- alpha / SE
p <- 2 * pnorm(abs(z), lower.tail = FALSE)

# Results
data.frame(
  Threshold = names(alpha),
  Estimate = as.numeric(alpha),
  Std_Error = as.numeric(SE),
  z_value = as.numeric(z),
  p_value = round(as.numeric(p),6)
)

# ------------------------------------------------------------
# 3. Design matrices for marginal standardization
# ------------------------------------------------------------

X_list <- setNames(
  lapply(org_levels, function(org) {
    
    nd <- datos_GMA
    
    # Assign all patients to the same organizational model
    nd$Home_based_PHC_org <- factor(
      rep(org, nrow(nd)),
      levels = org_levels
    )
    
    X <- model.matrix(
      ~ Home_based_PHC_org +
        Edat_10 +
        sex_female +
        Index_SES_z,
      data = nd
    )
    
    X[, names(beta), drop = FALSE]
    
  }),
  org_levels
)


# ------------------------------------------------------------
# 4. Standardized probabilities for all GMA categories
# ------------------------------------------------------------

std_GMA <- function(X) {
  
  # Linear predictor
  eta <- drop(X %*% beta)
  
  # Cumulative probabilities
  p_le1 <- plogis(alpha["1|2"] - eta)
  p_le2 <- plogis(alpha["2|3"] - eta)
  p_le3 <- plogis(alpha["3|4"] - eta)
  
  # Category-specific marginal probabilities
  c(
    GMA_1 = mean(p_le1),
    GMA_2 = mean(p_le2 - p_le1),
    GMA_3 = mean(p_le3 - p_le2),
    GMA_4 = mean(1 - p_le3)
  )
}

# Calculate standardized probabilities
prob_GMA_raw <- t(
  vapply(
    X_list,
    std_GMA,
    numeric(4)
  )
)

# Convert to percentages for presentation
prob_GMA <- data.frame(
  Home_based_PHC_org = rownames(prob_GMA_raw),
  round(100 * prob_GMA_raw, 1),
  row.names = NULL,
  check.names = FALSE
)

print(prob_GMA)

# Verify probabilities sum to 1 before rounding
rowSums(prob_GMA_raw)


# ====
# UNCERTAINTY ESTIMATION
# ====

# ------------------------------------------------------------
# 5. Parametric simulation
# ------------------------------------------------------------

set.seed(2026)

B <- 2000

theta_sim <- MASS::mvrnorm(
  n = B,
  mu = theta,
  Sigma = V
)

colnames(theta_sim) <- names(theta)


# ------------------------------------------------------------
# 6. Point estimates: P(GMA = 4)
# ------------------------------------------------------------

p4_est <- sapply(X_list, function(X) {
  
  eta <- drop(X %*% beta)
  
  mean(
    plogis(eta - alpha["3|4"])
  )
})


# ------------------------------------------------------------
# 7. Simulated standardized probabilities
# ------------------------------------------------------------

p4_sim <- sapply(X_list, function(X) {
  
  # Predicted linear predictors for all simulations
  eta <- X %*% t(
    theta_sim[, names(beta), drop = FALSE]
  )
  
  # Apply simulated threshold
  eta <- sweep(
    eta,
    2,
    theta_sim[, "3|4"],
    "-"
  )
  
  # Mean predicted probability in each simulation
  colMeans(plogis(eta))
})


# ------------------------------------------------------------
# 8. Confidence intervals for GMA 4
# ------------------------------------------------------------

ci_p4 <- apply(
  p4_sim,
  2,
  quantile,
  probs = c(0.025, 0.975)
)

GMA4_results <- data.frame(
  Model = org_levels,
  Probability = round(100 * unname(p4_est), 1),
  CI_low = round(100 * unname(ci_p4[1, ]), 1),
  CI_high = round(100 * unname(ci_p4[2, ]), 1),
  row.names = NULL
)

print(GMA4_results)


# ====
# CONTRASTS AGAINST TRADITIONAL TEAM-BASED
# ====

# ------------------------------------------------------------
# 9. Absolute prevalence differences
# ------------------------------------------------------------

# Point estimates
diff_est <- p4_est[-1] - p4_est[1]

# Paired differences across simulations
diff_sim <- sweep(
  p4_sim[, -1, drop = FALSE],
  1,
  p4_sim[, 1],
  "-"
)

# Nominal 95% confidence intervals
ci_diff <- apply(
  diff_sim,
  2,
  quantile,
  probs = c(0.025, 0.975)
)


# ------------------------------------------------------------
# 10. P values from ordinal model coefficients
# ------------------------------------------------------------

# Organizational coefficients vs traditional model
coef_names <- paste0(
  "Home_based_PHC_org",
  org_levels[-1]
)

# Coefficient estimates
b_org <- beta[coef_names]

# Standard errors
se_org <- sqrt(
  diag(
    V[coef_names, coef_names, drop = FALSE]
  )
)

# Wald statistics
z_org <- unname(b_org / se_org)

# Unadjusted P values
p_raw <- 2 * pnorm(
  abs(z_org),
  lower.tail = FALSE
)


# ------------------------------------------------------------
# 11. Holm correction (3 contrasts within GMA)
# ------------------------------------------------------------

p_Holm <- p.adjust(
  p_raw,
  method = "holm"
)


# ------------------------------------------------------------
# 12. Final adjusted contrast table
# ------------------------------------------------------------

GMA4_contrasts <- data.frame(
  
  Comparison = org_levels[-1],
  
  Difference_pp = round(
    100 * unname(diff_est), 1
  ),
  
  CI_low_pp = round(
    100 * unname(ci_diff[1, ]), 1
  ),
  
  CI_high_pp = round(
    100 * unname(ci_diff[2, ]), 1
  ),
  
  p_raw = signif(p_raw, 3),
  
  p_Holm = signif(p_Holm, 3),
  
  row.names = NULL
)

print(GMA4_contrasts)
print(GMA4_results)

# Differences from traditional model
diff_est <- p4_est[-1] - p4_est[1]

diff_sim <- sweep(
  p4_sim[, -1, drop = FALSE],
  1,
  p4_sim[, 1],
  "-"
)

ci_diff <- apply(
  diff_sim,
  2,
  quantile,
  probs = c(0.025, 0.975)
)

GMA4_contrasts <- data.frame(
  Comparison = org_levels[-1],
  Difference_pp = round(100 * diff_est, 1),
  CI_low_pp = round(100 * ci_diff[1, ], 1),
  CI_high_pp = round(100 * ci_diff[2, ], 1)
)


# Extract standardized probabilities from emmeans
extract_probs <- function(emm, dimension) {
  
  as.data.frame(
    summary(emm, infer = c(TRUE, FALSE))
  ) %>%
    transmute(
      Dimension = dimension,
      Model = as.character(Home_based_PHC_org),
      Probability = 100 * prob,
      CI_low = 100 * asymp.LCL,
      CI_high = 100 * asymp.UCL
    )
}

# Combine four case-mix dimensions
case_mix_final <- bind_rows(
  
  extract_probs(
    emm_social,
    "Social needs"
  ),
  
  extract_probs(
    emm_functional,
    "Functional dependency"
  ),
  
  extract_probs(
    emm_neuro,
    "Neurodegenerative conditions"
  ),
  
  GMA4_results %>%
    transmute(
      Dimension = "Clinical complexity (GMA 4)",
      Model = as.character(Model),
      Probability = Probability,
      CI_low = CI_low,
      CI_high = CI_high
    )
  
) %>%
  mutate(
    Result = sprintf(
      "%.1f (%.1f-%.1f)",
      Probability,
      CI_low,
      CI_high
    )
  )

# Final table
case_mix_table_final <- case_mix_final %>%
  dplyr::select(Dimension, Model, Result) %>%
  tidyr::pivot_wider(
    names_from = Model,
    values_from = Result
  )

print(case_mix_table_final, width = Inf)


# Extract comparisons with nominal 95% CI
extract_contrasts <- function(emm, dimension) {
  
  contr <- contrast(
    emm,
    method = "trt.vs.ctrl",
    ref = 1,
    adjust = "none"
  )
  
  as.data.frame(
    confint(contr, adjust = "none")
  ) %>%
    dplyr::transmute(
      Dimension = dimension,
      Comparison = as.character(contrast),
      Difference_pp = 100 * estimate,
      CI_low_pp = 100 * asymp.LCL,
      CI_high_pp = 100 * asymp.UCL
    )
}

# Combine comparisons
case_mix_contrasts <- bind_rows(
  
  extract_contrasts(
    emm_social,
    "Social needs"
  ),
  
  extract_contrasts(
    emm_functional,
    "Functional dependency"
  ),
  
  extract_contrasts(
    emm_neuro,
    "Neurodegenerative conditions"
  ),
  
  GMA4_contrasts %>%
    dplyr::mutate(
      Dimension = "Clinical complexity (GMA 4)",
      Comparison = paste0(
        Comparison,
        " - Traditional team-based"
      )
    ) %>%
    dplyr::select(
      Dimension,
      Comparison,
      Difference_pp,
      CI_low_pp,
      CI_high_pp
    )
  
) %>%
  dplyr::mutate(
    Result = sprintf(
      "%.1f (%.1f to %.1f)",
      Difference_pp,
      CI_low_pp,
      CI_high_pp
    )
  )

print(
  case_mix_contrasts
)


# Prepare figure data
fig_case_mix <- case_mix_contrasts %>%
  dplyr::mutate(
    
    # Organizational model labels
    Model = dplyr::case_when(
      grepl("Reinforced", Comparison) ~
        "Reinforced Traditional",
      
      grepl("Nurse-led", Comparison) ~
        "Nurse-led",
      
      grepl("Multidisciplinary", Comparison) ~
        "Multidisciplinary Team",
      
      TRUE ~ NA_character_
    ),
    
    # Order organizational models
    Model = factor(
      Model,
      levels = c(
        "Multidisciplinary Team",
        "Nurse-led",
        "Reinforced Traditional"
      )
    ),
    
    # Rename clinical dimension
    Dimension = dplyr::case_when(
      Dimension == "Clinical complexity (GMA 4)" ~
        "High comorbidity and complexity",
      TRUE ~ as.character(Dimension)
    ),
    
    # Order clinical dimensions
    Dimension = factor(
      Dimension,
      levels = c(
        "Social needs",
        "Functional dependency",
        "Neurodegenerative conditions",
        "High comorbidity and complexity"
      )
    )
  )

# Forest plot

Figure_2 <- ggplot(
  fig_case_mix,
  aes(
    x = Difference_pp,
    y = Model
  )
) +
  

# Reference line

geom_vline(
    xintercept = 0,
    linetype = "dashed",
    linewidth = 0.4,
    colour = "grey50"
  ) +
  

# Confidence intervals

  # Confidence intervals
  geom_segment(
    aes(
      x = CI_low_pp,
      xend = CI_high_pp,
      yend = Model
    ),
    linewidth = 0.65,
    colour = "black"
  ) +
  

# Point estimates

  # Point estimates
geom_point(
    size = 2.8,
    colour = "black"
  ) +
  

# Four panels

  # Four panels
  facet_wrap(
    ~ Dimension,
    ncol = 2
  ) +
  

# Common scale

  # Common scale
  scale_x_continuous(
    limits = c(-15, 20),
    breaks = seq(-15, 20, 5)
  ) +
  
  labs(
    x = "Adjusted prevalence difference (percentage points)",
    y = NULL
  ) +
  
  theme_bw(base_size = 11) +
  
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(
      colour = "black",
      linewidth = 0.4
    ),
    strip.background = element_blank(),
    strip.text = element_text(
      face = "bold",
      size = 11,
      colour = "black"
    ),
    axis.text = element_text(
      colour = "black",
      size = 11
    ),
    axis.title = element_text(
      colour = "black",
      size = 11
    ),
    panel.spacing = grid::unit(1, "lines")
  )

Figure_2


ggsave(
  filename = here("Output", "Figures", "Standarized_probs.png"),
  plot = Figure_2,
  width = 10,
  height = 8,
  units = "in",
  dpi = 300
)


# ====
# FINAL CASE-MIX CONTRASTS TABLE
# Holm correction within each clinical dimension
# ====

# Organizational models


modelos <- c(
  "Reinforced team-based",
  "Nurse-led unit",
  "Multidisciplinary unit"
)


# ------------------------------------------------------------
# 1. Extract binary model contrasts
# ------------------------------------------------------------

extract_contrasts <- function(emm, dimension) {
  

# Unadjusted comparisons against traditional

  contr <- emmeans::contrast(
    emm,
    method = "trt.vs.ctrl",
    ref = 1,
    adjust = "none"
  )
  
  res <- as.data.frame(
    summary(
      contr,
      infer = c(TRUE, TRUE),
      adjust = "none"
    )
  )
  

# Verify comparison order

stopifnot(

    nrow(res) == length(modelos),
    all(vapply(
      seq_along(modelos),
      function(i) grepl(
        modelos[i],
        as.character(res$contrast[i]),
        fixed = TRUE
      ),
      logical(1)
    ))
  )
  
  tibble(
    Dimension = dimension,
    Model = modelos,
    Difference_pp = res$estimate * 100,
    CI_low_pp = res$asymp.LCL * 100,
    CI_high_pp = res$asymp.UCL * 100,
    p_raw = res$p.value
  )
}


# ------------------------------------------------------------
# 2. Binary outcomes
# ------------------------------------------------------------

social_results <- extract_contrasts(
  emm_social,
  "Social needs"
)

functional_results <- extract_contrasts(
  emm_functional,
  "Functional dependency"
)

neuro_results <- extract_contrasts(
  emm_neuro,
  "Neurodegenerative conditions"
)

# ------------------------------------------------------------
# 3. Clinical complexity: GMA level 4
# ------------------------------------------------------------

# Extract organizational coefficients

coef_names <- paste0(
  "Home_based_PHC_org",
  modelos
)

beta_GMA <- MM_complexity$beta[coef_names]

# Variance-covariance matrix

V_GMA <- as.matrix(vcov(MM_complexity))

SE_GMA <- sqrt(
  diag(
    V_GMA[coef_names, coef_names, drop = FALSE]
  )
)

# Unadjusted Wald P values (full precision)
p_GMA <- 2 * pnorm(
  abs(beta_GMA / SE_GMA),
  lower.tail = FALSE
)

# Match the previously estimated probability contrasts
idx <- match(
  modelos,
  as.character(GMA4_contrasts$Comparison)
)

stopifnot(!anyNA(idx))

GMA_results <- tibble(
  Dimension = "Clinical complexity (GMA 4)",
  Model = modelos,
  Difference_pp = GMA4_contrasts$Difference_pp[idx],
  CI_low_pp = GMA4_contrasts$CI_low_pp[idx],
  CI_high_pp = GMA4_contrasts$CI_high_pp[idx],
  p_raw = as.numeric(p_GMA)
)


# ------------------------------------------------------------
# 4. Combine all 12 contrasts
# ------------------------------------------------------------

case_mix_contrasts_final <- bind_rows(
  social_results,
  functional_results,
  neuro_results,
  GMA_results
) %>%
  

# Holm correction within each dimension
  group_by(Dimension) %>%
  mutate(
    p_Holm = p.adjust(
      p_raw,
      method = "holm"
    )
  ) %>%
  ungroup()


# ------------------------------------------------------------
# 5. Format final table for manuscript
# ------------------------------------------------------------

case_mix_table_final <- case_mix_contrasts_final %>%
  mutate(
    

# Format absolute differences and 95% CI
    Adjusted_difference = sprintf(
      "%.1f (%.1f to %.1f)",
      Difference_pp,
      CI_low_pp,
      CI_high_pp
    ),
    

# P values
    P_nominal = ifelse(
      p_raw < 0.001,
      "<.001",
      sprintf("%.3f", p_raw)
    ),
    
    P_Holm = ifelse(
      p_Holm < 0.001,
      "<.001",
      sprintf("%.3f", p_Holm)
    )
  ) %>%
  dplyr::select(
    Dimension,
    Model,
    Adjusted_difference,
    P_nominal,
    P_Holm
  )

# Display all results
print(
  case_mix_table_final,
  n = Inf,
  width = Inf
)


write_xlsx(
  case_mix_table_final,
  here("Output", "Tables", "case_mix_table_probs.xlsx")
)

case_mix_contrasts_final %>%
  group_by(Dimension) %>%
  summarise(
    n_comparisons = n(),
    n_significant_nominal = sum(p_raw < 0.05),
    n_significant_Holm = sum(p_Holm < 0.05),
    .groups = "drop"
  )
