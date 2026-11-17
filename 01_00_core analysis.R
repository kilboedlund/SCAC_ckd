library('tidyverse')
library('survival')
library('splines')
source(here::here('Code and stats/00_00_helper functions.R'))

data_7_CKD <- readRDS(here::here('Code and stats', 'data_7_CKD_2026-10-08.rds'))
data_7_RRT <- readRDS(here::here('Code and stats', 'data_7_RRT_2026-10-08.rds'))

## 01_00_00 Set up surv objects ================================================
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

## 01_00_01 Define covariate models ============================================
models <- c(
  m0 = 'ns(age_in, 4) + sex',
  m1 = 'ns(age_in, 4) + sex + ses + edu + civ + smo + phy + ns(bmi, 4)',
  m2 = 'ns(age_in, 4) + sex + ses + edu + civ + smo + phy + ns(bmi, 4) + (date_DM_p == 2) + (date_HPT_p == 2)')

## 01_00_02 Regression analyses ================================================
res <- crossing(
  cohort = c('pps', 'got'),
  outcome = c('CKD', 'RRT'),
  model = c('m0', 'm1', 'm2'),
  exposure = c('pm25', 'pm10', 'nox', 'bc'),
  lag = paste0('_lag', c('1', '2_5', '6_10'))
) %>% 
  purrr::pmap(\(cohort, outcome, model, exposure, lag) 
              scac_ckd_core_analysis(cohort, outcome, model, exposure, lag)) %>% 
  bind_rows()

## 01_00_03 Cleanup, plot and save =============================================
res_2 <- res %>%
  tibble::remove_rownames() %>% 
  #present nox and bc in 10 and 0.1 ug/m3 increments
  mutate(inc = case_when(exposure == 'nox' ~ 10,
                         exposure == 'bc' ~ .1,
                         T ~ 1),
         exposure = paste0(exposure, lag),
         HR = paste0(sprintf(round(exp(beta*inc), 2), 
                             fmt = '%0.2f'), ' (',
                     sprintf(round(exp((beta-1.96*se)*inc), 2), 
                             fmt = '%0.2f'), '-',
                     sprintf(round(exp((beta+1.96*se)*inc), 2), 
                             fmt = '%0.2f'), ')')) %>% 
  select(-lag)

writexl::write_xlsx(res_2, 
                    here::here('Outputs', 
                               paste0('res_main_', Sys.Date(), '.xlsx')))

res_2 %>%
  filter(model == 'm1') %>% 
  mutate(expo_f = str_sub(exposure, 1, 4) %>% 
           str_remove('l') %>%
           str_remove('_') %>% 
           str_remove(' ') %>% 
           str_to_upper() %>% 
           str_replace_all('X', 'x'),
         lag_f = case_when(str_detect(exposure, 'lag1') ~ '1 y',
                           str_detect(exposure, 'lag2') ~ '2-5 y',
                           str_detect(exposure, 'lag6') ~ '6-10 y'),
         exposure_f = paste0(expo_f, '\n', lag_f)) %>% 
  ggplot(aes(x = exposure_f, colour = cohort)) +
  geom_hline(aes(yintercept = 1), linetype = 'dashed') +
  geom_point(aes(y = exp(beta)^inc),
             position = position_dodge(width = .5)) +
  geom_errorbar(aes(ymin = exp((beta-1.96*se)*inc),
                    ymax = exp((beta+1.96*se)*inc)),
                width = .5,
                position = position_dodge(width = .5)) +
  scale_y_continuous(transform = 'log', 
                     breaks = c(.5, 0.67, 1, 1.5, 2)) +
  coord_cartesian(ylim = c(.5, 2.2)) +
  labs(x = NULL, y = 'HR (95% CI)', colour = 'Cohort') +
  facet_wrap(vars(outcome), ncol = 1) +
  theme_linedraw() +
  theme(panel.grid.minor = element_blank())

ggsave(here::here('Outputs', paste0('res_main_', Sys.Date(), '.png')), 
                  width = 5, height = 6)
           