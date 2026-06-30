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
