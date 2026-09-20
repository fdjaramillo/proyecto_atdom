# Funciones de Modelado Estadístico

run_models_automatic <- function(data, continuous_outcomes, categorical_outcomes,
                                 exposure, weights_var, adjust_vars = NULL) {
  
  rhs <- if (is.null(adjust_vars) || length(adjust_vars) == 0) {
    exposure
  } else {
    paste(c(exposure, adjust_vars), collapse = " + ")
  }
  
  keep_exposure_terms <- function(df) {
    df %>%
      filter(term == exposure | startsWith(term, paste0(exposure)))
  }
  
  results_continuous <- map_dfr(continuous_outcomes, function(outcome) {
    
    formula_model <- as.formula(paste(outcome, "~", rhs))
    
    model <- glm(
      formula_model,
      data = data,
      weights = data[[weights_var]],
      family = gaussian()
    )
    
    tidy(model, conf.int = TRUE) %>%
      keep_exposure_terms() %>%
      mutate(
        term = gsub(paste0("^", exposure), "", term),
        outcome = outcome,
        model_type = "Gaussian",
        measure = "Beta",
        n = stats::nobs(model),
        estimate_final = estimate,
        conf.low_final = conf.low,
        conf.high_final = conf.high
      )
  })

  results_categorical <- map_dfr(categorical_outcomes, function(outcome) {
    
    formula_model <- as.formula(paste(outcome, "~", rhs))
    
    model <- glm(
      formula_model,
      data = data,
      weights = data[[weights_var]],
      family = quasibinomial()
    )
    
    tidy(model, conf.int = TRUE, exponentiate = TRUE) %>%
      keep_exposure_terms() %>%
      mutate(
        term = gsub(paste0("^", exposure), "", term),
        outcome = outcome,
        model_type = "Logistic",
        measure = "OR",
        n = stats::nobs(model),
        estimate_final = estimate,
        conf.low_final = conf.low,
        conf.high_final = conf.high
      )
  })
  
  bind_rows(results_continuous, results_categorical) %>%
    mutate(
      model_formula = paste0(outcome, " ~ ", rhs),
      result = paste0(
        round(estimate_final, 2),
        " (",
        round(conf.low_final, 2),
        "; ",
        round(conf.high_final, 2),
        ")"
      ),
      p.value = signif(p.value, 3)
    ) %>%
    select(
      outcome,
      model_type,
      measure,
      term,
      n,
      result,
      p.value,
      model_formula
    )
}

#Robust standard errors.

get_robust_results <- function(model, data){
  
  V <- vcovCL(
    model,
    cluster = data$Seccio_Censal,
    type = "HC1"
  )
  
  se <- sqrt(diag(V))
  beta <- coef(model)
  z <- beta / se
  
  tibble(
    term = names(beta),
    beta = round(beta,3),
    std.error = round(se,3),
    IRR = round(exp(beta),2),
    conf.low = round(exp(beta - 1.96 * se),2),
    conf.high = round(exp(beta + 1.96 * se),2),
    statistic = round(z,3),
    p.value = round(2 * pnorm(abs(z), lower.tail = FALSE),4)
  )
}

# Función para preparar solo los efectos de organización
prep_model <- function(res, model_name) {
  
  res %>%
    filter(grepl("^Home_based_PHC_org", term)) %>%
    mutate(
      Home_based_PHC_org = case_when(
        grepl("Equip_Atdom", term) ~ "Equip ATDOM",
        grepl("Equip_Inf", term) ~ "Equip Inf",
        grepl("UAB_consulta_reforc", term) ~ "UAB consulta reforç"
      ),
      estimate = sprintf(
        "%.2f (%.2f–%.2f)",
        IRR, conf.low, conf.high
      ),
      Model = model_name
    ) %>%
    select(Home_based_PHC_org, Model, estimate)
}
