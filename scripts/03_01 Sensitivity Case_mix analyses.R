# ============================================================================
# ATDOM | Leave-One-Centre-Out (LOCO)
# Acceso: Poisson M2. Case-mix: 3 GLMM binarios + 1 GLMM ordinal.
# ============================================================================

ref_org <- "Traditional team-based"
modelos_alt <- c("Multidisciplinary unit", "Nurse-led unit", "Reinforced team-based")
centros <- c("00460", "00462", "00474", "00475", "00477", "00478", "01004")
exclusiones <- c("Ninguno", centros)
dimensiones <- c("Social_positive", "Barthel_severe", "Dementia_parkinson", "GMA_PNIV")
out_dir <- "ATDOM_LOCO_resultados"
dir.create(out_dir, showWarnings = FALSE)

min_ok <- function(x) if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)
max_ok <- function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)

# Conversión explícita de variables binarias, sin reinterpretar valores dudosos.
a_01 <- function(x, nombre) {
  if (is.logical(x)) return(as.integer(x))
  z <- tolower(trimws(as.character(x)))
  z <- chartr("áéíóúü", "aeiouu", z)
  si <- c("1", "si", "yes", "true", "positivo", "positive", "mujer", "female")
  no <- c("0", "no", "false", "negativo", "negative", "hombre", "male")
  incorrectos <- unique(z[!is.na(z) & nzchar(z) & !z %in% c(si, no)])
  if (length(incorrectos)) stop(nombre, ": códigos no reconocidos: ", paste(incorrectos, collapse = ", "))
  out <- rep(NA_integer_, length(z))
  out[z %in% si] <- 1L
  out[z %in% no] <- 0L
  out
}

# Datos: relación sección-centro SIN multiplicar por el número de pacientes

UC <- readRDS(here::here("data", "processed", "tasas_cobertura_atdom_long.rds"))
SES <- readRDS(here::here("data", "processed", "Data_SES_UC.rds"))
TB_case_mix <- readRDS(here::here("data", "processed", "TB_case_mix"))

UC <- UC %>% mutate(Seccio_Censal = as.character(Seccio_Censal))

ses_uc <- SES %>%
  transmute(Seccio_Censal = as.character(Seccio_Censal),
            Index_SES = as.numeric(as.character(Index_SES))) %>%
  distinct()

asignaciones <- TB_case_mix %>%
  transmute(Seccio_Censal = as.character(Seccio_Censal),
            centro = as.character(UP_ABS),
            Home_based_PHC_org = as.character(Home_based_PHC_org)) %>%
  filter(!is.na(centro), centro != "", !is.na(Home_based_PHC_org)) %>%
  distinct()

# Se excluyen las 42 secciones sin centro; se conservan secciones compartidas.

access <- UC %>%
  left_join(ses_uc, by = "Seccio_Censal", relationship = "many-to-one") %>%
  inner_join(asignaciones, by = "Seccio_Censal", relationship = "many-to-many")

# Estratos de población cero: no pueden entrar en un offset log(poblacion_total).
access <- filter(access, poblacion_total > 0)

# Media y DE calculadas UNA vez, ponderando cada sección una sola vez.
ses_referencia <- access %>% distinct(Seccio_Censal, Index_SES)
media_ses <- mean(ses_referencia$Index_SES)
de_ses <- sd(ses_referencia$Index_SES)

access <- access %>%
  mutate(Index_SES_z = (Index_SES - media_ses) / de_ses,
         Home_based_PHC_org = relevel(factor(Home_based_PHC_org), ref = ref_org),
         edat_cat = factor(edat_cat, levels = c("15_64", "65_74", "75_84", "85_plus")),
         sexo = factor(sexo))

# Mantener únicamente secciones atribuibles a un solo centro
access <- access %>%
  group_by(Seccio_Censal) %>%
  filter(n_distinct(centro) == 1L) %>%
  ungroup() %>%
  droplevels()

# AVISO DE DISEÑO: los totales de UC pueden estar replicados en centros compartidos.
# El LOCO de acceso será exploratorio hasta verificar atribución centro-específica.

n_compartidas <- access %>% distinct(Seccio_Censal, centro) %>%
  count(Seccio_Censal) %>% filter(n > 1L) %>% nrow()
if (n_compartidas > 0) warning(n_compartidas, " secciones aparecen en varios centros: comprobar denominadores/numeradores.")

# --- 2. Poisson M2: modelo completo y siete exclusiones ----------------------
fit_access <- function(excluido) {
  d <- if (excluido == "Ninguno") access else filter(access, centro != excluido)
  d <- droplevels(d)
  mod <- glm(n_atdom ~ Home_based_PHC_org + edat_cat + sexo + Index_SES_z +
               offset(log(poblacion_total)),
             family = poisson(link = "log"), data = d, na.action = na.fail)
  if (!mod$converged || anyNA(coef(mod))) stop("M2 inválido tras excluir ", excluido)
  V <- sandwich::vcovCL(mod, cluster = d$Seccio_Censal, type = "HC1")
  
  bind_rows(lapply(modelos_alt, function(org) {
    if (!org %in% levels(d$Home_based_PHC_org)) {
      return(data.frame(excluido = excluido, modelo = org,
                        IRR = NA_real_, IC95_inf = NA_real_, IC95_sup = NA_real_))
    }
    termino <- paste0("Home_based_PHC_org", org)
    if (!termino %in% names(coef(mod))) stop("Falta coeficiente: ", termino)
    b <- unname(coef(mod)[termino]); se <- sqrt(V[termino, termino])
    if (!is.finite(b) || !is.finite(se)) stop("IRR o error estándar no finito: ", excluido)
    data.frame(excluido = excluido, modelo = org, IRR = exp(b),
               IC95_inf = exp(b - 1.96 * se), IC95_sup = exp(b + 1.96 * se))
  }))
}

estim_access <- bind_rows(lapply(exclusiones, fit_access))
full_access <- estim_access %>% filter(excluido == "Ninguno") %>%
  transmute(modelo, IRR_full = IRR, IC_inf_full = IC95_inf, IC_sup_full = IC95_sup)

eTable_1 <- estim_access %>% filter(excluido != "Ninguno") %>%
  left_join(full_access, by = "modelo") %>% group_by(modelo) %>%
  summarise(n_estimables = sum(!is.na(IRR)),
            IRR_min = min_ok(IRR), IRR_max = max_ok(IRR),
            cambio_max_pct = max_ok(abs(100 * (IRR / IRR_full - 1))),
            inversiones = sum((IRR - 1) * (IRR_full - 1) < 0, na.rm = TRUE),
            .groups = "drop") %>%
  left_join(full_access, by = "modelo") %>%
  mutate(`Full-sample IRR (95% CI)` = sprintf("%.2f (%.2f-%.2f)", IRR_full, IC_inf_full, IC_sup_full),
         `LOCO IRR range` = sprintf("%.2f-%.2f", IRR_min, IRR_max),
         cambio_max_pct = round(cambio_max_pct, 1)) %>%
  select(`Organizational model` = modelo, `Full-sample IRR (95% CI)`,
         `LOCO IRR range`, `Maximum relative change (%)` = cambio_max_pct,
         `Estimable exclusions` = n_estimables, `Direction reversals` = inversiones)

# --- 3. Datos individuales y cuatro dimensiones -----------------------------
cm <- TB_case_mix %>%
  mutate(centro = as.character(UP_ABS),
         Seccio_Censal = as.factor(Seccio_Censal),
         Home_based_PHC_org = relevel(factor(Home_based_PHC_org), ref = ref_org),
         Edat_10 = (as.numeric(as.character(Edat)) - 85) / 10,
         sex_female = a_01(sex_female, "sex_female"),
         Social_positive = a_01(Social_positive, "Social_positive"),
         Barthel_severe = a_01(Barthel_severe, "Barthel_severe"),
         Dementia_parkinson = a_01(Dementia_parkinson, "Dementia_parkinson"),
         GMA_PNIV = ordered(as.character(GMA_PNIV), levels = as.character(1:4))) %>%
  filter(!is.na(centro), !is.na(Seccio_Censal), !is.na(Edat_10),
         !is.na(sex_female), !is.na(Index_SES_z), !is.na(Home_based_PHC_org))


# Predicciones binarias a partir de la parte fija (intercepto aleatorio = 0).
prob_binaria <- function(fit, referencia, org, niveles) {
  nd <- referencia
  nd$Home_based_PHC_org <- factor(org, levels = niveles)
  mean(predict(fit, newdata = nd, type = "response", re.form = NA))
}

# Modelo ordinal: P(GMA = 4) = 1 - logistic(alpha_3 - X beta), b_aleatorio = 0.
prob_gma4 <- function(fit, referencia, org, niveles) {
  nd <- referencia
  nd$Home_based_PHC_org <- factor(org, levels = niveles)
  X <- model.matrix(~ Home_based_PHC_org + Edat_10 + sex_female + Index_SES_z, nd)
  if (length(fit$alpha) != 3L || any(!is.finite(fit$alpha)) ||
      !length(fit$beta) || any(!is.finite(fit$beta)) ||
      !all(names(fit$beta) %in% colnames(X))) stop("Parámetros ordinales inválidos.")
  eta <- as.numeric(X[, names(fit$beta), drop = FALSE] %*% fit$beta)
  mean(1 - plogis(unname(fit$alpha[3]) - eta))
}

# Ajustar una dimensión usando siempre la misma población de referencia.
fit_dimension <- function(dim, excluido, referencia) {
  d <- if (excluido == "Ninguno") referencia else filter(referencia, centro != excluido)
  d <- droplevels(d)
  niveles <- levels(d$Home_based_PHC_org)
  if (!ref_org %in% niveles) stop("Falta el modelo tradicional en ", excluido)
  
  if (dim != "GMA_PNIV") {
    if (length(unique(d[[dim]])) != 2L) stop("Variable sin variación: ", dim)
    form <- as.formula(paste(dim,
                             "~ Home_based_PHC_org + Edat_10 + sex_female + Index_SES_z + (1 | Seccio_Censal)"))
    fit <- lme4::glmer(form, data = d, family = binomial(link = "logit"),
                       control = lme4::glmerControl(optimizer = "bobyqa",
                                                    optCtrl = list(maxfun = 200000)))
    if (anyNA(lme4::fixef(fit))) stop("Modelo binario con coeficientes no estimables: ", dim)
    if (length(fit@optinfo$conv$lme4$messages)) {
      stop("Revisar convergencia del modelo binario: ", dim, ", excluido ", excluido)
    }
    predecir <- function(org) prob_binaria(fit, referencia, org, niveles)
  } else {
    if (!identical(levels(droplevels(d$GMA_PNIV)), as.character(1:4))) {
      stop("Faltan niveles GMA en el ajuste de ", excluido)
    }
    fit <- ordinal::clmm(GMA_PNIV ~ Home_based_PHC_org + Edat_10 + sex_female +
                           Index_SES_z + (1 | Seccio_Censal),
                         data = d, link = "logit", Hess = TRUE, nAGQ = 1L)
    if (!is.null(fit$optRes$convergence) && fit$optRes$convergence != 0) {
      stop("Modelo ordinal sin convergencia: ", excluido)
    }
    predecir <- function(org) prob_gma4(fit, referencia, org, niveles)
  }
  
  p_trad <- predecir(ref_org)
  bind_rows(lapply(modelos_alt, function(org) {
    p_alt <- if (org %in% niveles) predecir(org) else NA_real_
    data.frame(dimension = dim, modelo = org, excluido = excluido,
               diferencia_pp = 100 * (p_trad - p_alt))
  }))
}

estim_case <- bind_rows(lapply(dimensiones, function(dim) {
  # Casos completos específicos de la dimensión; NO cambian con la exclusión.
  referencia <- cm[!is.na(cm[[dim]]), , drop = FALSE]
  bind_rows(lapply(exclusiones, function(exc) fit_dimension(dim, exc, referencia)))
}))

full_case <- estim_case %>% filter(excluido == "Ninguno") %>%
  select(dimension, modelo, diferencia_full_pp = diferencia_pp)

eTable_2 <- estim_case %>% filter(excluido != "Ninguno") %>%
  left_join(full_case, by = c("dimension", "modelo")) %>%
  group_by(dimension, modelo) %>%
  summarise(n_estimables = sum(!is.na(diferencia_pp)),
            diferencia_min = min_ok(diferencia_pp),
            diferencia_max = max_ok(diferencia_pp),
            cambio_max_pp = max_ok(abs(diferencia_pp - diferencia_full_pp)),
            inversiones = sum(diferencia_pp * diferencia_full_pp < 0, na.rm = TRUE),
            .groups = "drop") %>%
  left_join(full_case, by = c("dimension", "modelo")) %>%
  mutate(dimension = if_else(dimension == "GMA_PNIV", "GMA_PNIV (level 4)", dimension),
         `Full-sample difference (pp)` = round(diferencia_full_pp, 2),
         `LOCO difference range (pp)` = sprintf("%.2f to %.2f", diferencia_min, diferencia_max),
         cambio_max_pp = round(cambio_max_pp, 2)) %>%
  select(Dimension = dimension, `Organizational model` = modelo,
         `Full-sample difference (pp)`, `LOCO difference range (pp)`,
         `Maximum absolute change (pp)` = cambio_max_pp,
         `Estimable exclusions` = n_estimables, `Direction reversals` = inversiones)

# Comprobaciones finales: solo la exclusión de 00462 carece de nurse-led.
if (nrow(eTable_1) != 3L || nrow(eTable_2) != 12L ||
    any(!eTable_1$`Estimable exclusions` %in% c(6L, 7L)) ||
    any(!eTable_2$`Estimable exclusions` %in% c(6L, 7L))) {
  stop("Las tablas no tienen el número esperado de comparaciones estimables.")
}

write.xlsx(eTable_1, here("Output","ATDOM_LOCO_results", "eTable_1_LOCO_access.xlsx"))
write.xlsx(eTable_2, here("Output","ATDOM_LOCO_results", "eTable_2_LOCO_case_mix.xlsx"))

print(eTable_1)
print(eTable_2, n = Inf)

