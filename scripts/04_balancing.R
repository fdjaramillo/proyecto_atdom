# ============================================================
# 02_Balancing groups.R
# ============================================================

source(here("scripts", "00_setup.R"))

###Balancing

df <- readRDS("data/processed/df_cleaned.rds")

df<-df%>%
  mutate(Age_quantile=factor(ntile(df$Edat,10)),
         GMA_CODE_quantile=factor(ntile(df$GMA_CODE,10)))

###Check balance
bal.tab(organit_atdom_2~ GMA_CODE_quantile+Age_quantile+sex_female+barthel_cat+TIRS_num+incontinence_cat+MACA+PCC, 
        data = df,
        thresholds = c(m = .05))

W.out <- weightit(organit_atdom_2~ GMA_CODE_quantile+Age_quantile+sex_female+barthel_cat+TIRS_num+incontinence_cat+MACA+PCC,
                  data = df,
                  estimand = "ATO",
                  method = "glm")

bal.tab(W.out, 
        stats = "mean.diffs",
        thresholds = c(m = .05))

summary(W.out) #print the output

### Love plot
png(
  filename = here("Output", "Figures", "Balance_plot.png"),
  width = 2400,
  height = 1800,
  res = 300
)


love.plot(
  W.out,
  abs = TRUE,
  threshold = 0.05,
  position = "bottom"
)

dev.off()

df_balanced <- df %>%
  mutate(w_ato = W.out$weights)

continous_outcomes <- c("INGRES_num", "SEM_num", "emergency_visits")

cat_outcomes <- c(
  "emergency_visits_cat2",
  "SEM_num_cat2",
  "INGRES_num_cat2",
  "Exitus"
)


#### Crudo
results_all <- run_models_automatic(
  data = df_balanced,
  continuous_outcomes = continous_outcomes,
  categorical_outcomes = cat_outcomes,
  exposure = "organit_atdom_2",
  weights_var = "w_ato"
)

print(n = 21,results_all)

#### Adjusted
results_adjusted <- run_models_automatic(
  data = df_balanced,
  continuous_outcomes = continous_outcomes,
  categorical_outcomes = cat_outcomes,
  exposure = "organit_atdom_2",
  weights_var = "w_ato",
  adjust_vars = c("Edat", "sex_female","GMA_CODE","TIRS_num","living_alone","barthel_cat","PCC","MACA")
)

print(n = 21,results_adjusted)
