# ============================================================
# 03_2 Analysis case_mix selection.R
# ============================================================
library(here)
source(here("scripts", "00_0 setup.R"))

df<-readRDS(here("data","processed","Cluster_id"))

case_mix_raw <- df %>%
  count(Home_based_PHC_org, cluster) %>%
  group_by(Home_based_PHC_org) %>%
  mutate(
    total = sum(n),
    pct = 100 * n / total
  ) %>%
  ungroup()

case_mix_raw

case_mix_table <- case_mix_raw %>%
  mutate(
    value = sprintf("%d (%.1f%%)", n, pct)
  ) %>%
  select(Home_based_PHC_org, cluster, value) %>%
  pivot_wider(
    names_from = cluster,
    values_from = value,
    names_prefix = "Cluster_"
  )

case_mix_table


#2. Contraste global: ¿el case-mix difiere según organización?

tab_cluster_org <- table(
    df$Home_based_PHC_org,
    df$cluster
  )

tab_cluster_org

chi_case_mix <- chisq.test(tab_cluster_org)

chi_case_mix

library(effectsize)

cramers_v(tab_cluster_org)

##Epidemiology
table(df$cluster)

table(df$Home_based_PHC_org)

table(df$cluster, df$Home_based_PHC_org)

df_case_mix <- df %>%
  mutate(
    cluster = factor(cluster),
    Home_based_PHC_org = factor(Home_based_PHC_org),
    sex_female = factor(sex_female)
  )

df_case_mix <- df_case_mix %>%
  mutate(
    cluster = relevel(cluster, ref = "1")
  )

df_case_mix <- df_case_mix %>%
  mutate(
    Home_based_PHC_org =
      relevel(Home_based_PHC_org, ref = "UAB_consulta")
  )

library(nnet)

mod_case_mix_1 <- multinom(
  cluster ~ Home_based_PHC_org +
    Edat +
    sex_female,
  data = df_case_mix,
  na.action = na.omit,
  trace = FALSE
)

summary(mod_case_mix_1)

mod_case_mix_2 <- multinom(
  cluster ~ Home_based_PHC_org +
    Edat +
    sex_female +
    Walking_distance,
  data = df_case_mix,
  na.action = na.omit,
  trace = FALSE
)

summary(mod_case_mix_2)

library("marginaleffects")

prob_case_mix <- avg_predictions(
  mod_case_mix_2,
  newdata = datagrid(
    Home_based_PHC_org = unique
  ),
  by = c("Home_based_PHC_org", "group"),
  type = "probs"
)

prob_case_mix

comparisons_case_mix <- avg_comparisons(
  mod_case_mix_2,
  variables = "Home_based_PHC_org",
  by = "group",
  type = "probs"
)

comparisons_case_mix

mod_case_mix_null <- multinom(
  cluster ~
    Edat +
    sex_female +
    Walking_distance,
  data = df_case_mix,
  trace = FALSE
)

anova(
  mod_case_mix_null,
  mod_case_mix_2,
  test = "Chisq"
)

comparisons_case_mix_all <- avg_comparisons(
  mod_case_mix_2,
  variables = list(
    Home_based_PHC_org = "pairwise"
  ),
  by = "group",
  type = "probs"
)

comparisons_case_mix_all

comparisons_case_mix_all <- comparisons_case_mix_all %>%
  group_by(group) %>%
  mutate(
    p_holm = p.adjust(p.value, method = "holm")
  ) %>%
  ungroup()

comparisons_case_mix_all <- comparisons_case_mix_all %>%
  mutate(
    estimate_pp = estimate * 100,
    conf.low_pp = conf.low * 100,
    conf.high_pp = conf.high * 100
  )

table(df_case_mix$cluster)
table(df_case_mix$Home_based_PHC_org)

library(dplyr)
library(ggplot2)
library(forcats)

# 1. Prepare data


# 1. Prepare adjusted probabilities

case_mix_matrix <- prob_case_mix %>%
  mutate(
    phenotype = factor(
      group,
      levels = c("1", "2", "3"),
      labels = c(
        "Lower-complexity",
        "Neurocognitive-functional dependency",
        "Cardiometabolic-renal multimorbidity"
      )
    ),
    
    organisation = factor(
      Home_based_PHC_org,
      levels = c(
        "UAB_consulta",
        "UAB_consulta_reforc",
        "Equip_Atdom",
        "Equip_Inf"
      ),
      labels = c(
        "Consultation-based",
        "Reinforced consultation-based",
        "Dedicated home-care team",
        "Nurse-led"
      )
    ),
    
    estimate_pct = estimate * 100,
    conf.low_pct = conf.low * 100,
    conf.high_pct = conf.high * 100,
    
    value = sprintf(
      "%.1f (%.1f–%.1f)",
      estimate_pct,
      conf.low_pct,
      conf.high_pct
    )
  )

case_mix_matrix_final <- case_mix_matrix %>%
  select(
    organisation,
    phenotype,
    value
  ) %>%
  pivot_wider(
    names_from = phenotype,
    values_from = value
  ) %>%
  arrange(
    factor(
      organisation,
      levels = c(
        "Consultation-based",
        "Reinforced consultation-based",
        "Dedicated home-care team",
        "Nurse-led"
      )
    )
  )

case_mix_matrix_final

#ᵃ Dedicated home-care team vs reinforced consultation-based: +7.1 percentage points (95% CI, 2.8 to 11.5); Holm-adjusted P=.008.
#ᵇ Dedicated home-care team vs nurse-led: +8.9 percentage points (95% CI, 3.3 to 14.6); Holm-adjusted P=.009.
