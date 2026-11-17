library('tidyverse')
library('survival')
library('splines')

source(here::here('Code and stats/00_00_helper functions.R'))

data_7_CKD <- readRDS(here::here('Code and stats', 'data_7_CKD_2026-10-08.rds'))
data_7_RRT <- readRDS(here::here('Code and stats', 'data_7_RRT_2026-10-08.rds'))

## 01_02_00 Set up surv objects ================================================
surv_CKD <- map(data_7_CKD,
                ~ Surv(time = .x$date_in %>% as.double,
                       time2 = .x$date_out %>% as.double,
                       event = .x$date_CKD_p == 1,
                       type = 'counting'))

surv_RRT <- map(data_7_RRT,
                ~ Surv(time = .x$date_in %>% as.double,
                       time2 = .x$date_out %>% as.double,
                       event = .x$date_RRT_p == 1,
                       type = 'counting'))

## 01_02_01 Define covariate models ============================================
models <- c(
  m0 = 'ns(age_in, 4) + sex',
  m1 = 'ns(age_in, 4) + sex + ses + edu + civ + smo + phy + ns(bmi, 4)',
  m2 = 'ns(age_in, 4) + sex + ses + edu + civ + smo + phy + ns(bmi, 4) + (date_DM_p == 2) + (date_HPT_p == 2)')

## 01_02_02 Regression analyses ================================================
res_er <- crossing(
  cohort = c('pps', 'got'),
  outcome = c('CKD', 'RRT'),
  model = c('m0', 'm1', 'm2'),
  exposure = c('pm25', 'pm10', 'nox', 'bc'),
  lag = paste0('_lag', c('1', '2_5', '6_10'))
) %>% 
  purrr::pmap(\(cohort, outcome, model, exposure, lag) 
              scac_ckd_er_analysis(cohort, outcome, model, exposure, lag)) %>% 
  bind_rows()

## 01_02_03 Cleanup, plot and save =============================================
res_2 <- res_er %>%
  tibble::remove_rownames() %>% 
  pivot_longer(cols = c(contains('beta'), contains('se'))) %>% 
  mutate(quartile = str_sub(name, start = -1, end = -1),
         name = str_sub(name, start = 0, end = -2)) %>% 
  pivot_wider(id_cols = c('cohort', 
                          'outcome', 
                          'exposure', 
                          'lag', 
                          'model',
                          'py', 
                          'events',
                          'quartile', 
                          'p.trend'),
              names_from = 'name', 
              values_from = 'value') %>% 
  mutate(exposure = paste0(exposure, lag),
         HR = paste0(sprintf(round(exp(beta), 2), 
                             fmt = '%0.2f'), ' (',
                     sprintf(round(exp(beta-1.96*se), 2), 
                             fmt = '%0.2f'), '-',
                     sprintf(round(exp(beta+1.96*se), 2), 
                             fmt = '%0.2f'), ')')) %>% 
  select(-lag)

writexl::write_xlsx(res_2, 
                    here::here('Outputs', 
                               paste0('res_er_', Sys.Date(), '.xlsx')))
