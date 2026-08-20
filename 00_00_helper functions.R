require('tidyverse')

## 00_00_00 Data import ========================================================
multi_read_sas <- function(prefix, years, suffix) {
  purrr::map(years, ~haven::read_sas(paste0(prefix, .x, suffix))) %>%
    set_names(paste0(years))
}

## 00_00_01 Regression helpers =================================================
scac_ckd_core_analysis <- function(cohort, outcome, model, exposure, lag) {
  formula <- as.formula(paste0('surv_', outcome, '$', cohort, 
                               ' ~ ',
                               exposure, lag, ' + ', 
                               models[model]))
  fit <- coxph(formula,
               data = get(paste0('data_6_', outcome))[[cohort]])
  return(
    data.frame(
      cohort = cohort,
      outcome = outcome,
      exposure = exposure,
      lag = lag,
      model = model,
      beta = coef(fit)[paste0(exposure, lag)],
      se = summary(fit)$coefficients[paste0(exposure, lag), 'se(coef)'],
      py = fit$n,
      events = fit$nevent
    )
  )
}

scac_ckd_er_analysis <- function(cohort, outcome, model, exposure, lag) {
  formula <- as.formula(paste0('surv_', outcome, '$', cohort,
                                   ' ~ ',
                                   exposure, lag, ' + ',
                                   models[model]))
  data_cat <- get(paste0('data_6_', outcome))[[cohort]] %>%
    mutate(across(
      all_of(paste0(exposure, lag)),
      ~ ntile(.x, 4) %>%
        factor() %>%
        recode_factor(ref = '1')
    ))
  data_cont <- get(paste0('data_6_', outcome))[[cohort]] %>%
    mutate(across(all_of(paste0(exposure, lag)),
                  ~ ntile(.x, 4)))
  fit_cat <- coxph(formula,
                   data = data_cat)
  fit_cont <- coxph(formula,
                    data = data_cont)
  return(
    data.frame(
      cohort = cohort,
      outcome = outcome,
      exposure = exposure,
      lag = lag,
      model = model,
      beta2 = coef(fit_cat)[paste0(exposure, lag, '2')],
      se2 = summary(fit_cat)$coefficients[paste0(exposure, lag, '2'), 'se(coef)'],
      beta3 = coef(fit_cat)[paste0(exposure, lag, '3')],
      se3 = summary(fit_cat)$coefficients[paste0(exposure, lag, '3'), 'se(coef)'],
      beta4 = coef(fit_cat)[paste0(exposure, lag, '4')],
      se4 = summary(fit_cat)$coefficients[paste0(exposure, lag, '4'), 'se(coef)'],
      p.trend = summary(fit_cont)$coefficients[paste0(exposure, lag), 'Pr(>|z|)'],
      py = fit$n,
      events = fit$nevent
    )
  )
}

