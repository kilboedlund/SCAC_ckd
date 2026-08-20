library('tidyverse')
library('survival')
library('splines')

source(here::here('Code and stats/00_00_helper functions.R'))

data_5 <- readRDS(here::here('Code and stats/data_5_2025-12-01.rds'))

## 01_01_00 Prepare analysis datasets ==========================================
data_6_cGN <- data_5 %>%
  map(
    ~ .x %>%
      mutate(
        smo = as.factor(smo) %>% relevel(ref = '1'),
        date_in = as.Date.character(paste0(year, '-01-01'),
                                    format = '%Y-%m-%d'),
        date_out = as.Date.character(paste0(year, '-12-31'),
                                     format = '%Y-%m-%d'),
        age_in = year - as.double(date_birth)
      ) %>%
      filter((date_cGN_p != 2) &
               (date_in >= date_fup) &
               (date_in <= date_death | is.na(date_death)) &
               (age_in >= 30) &
               (age_in < 90) &
               ((date_in >= date_immig) | is.na(date_immig)) &
               abroad3y == 0
      ) %>%
      mutate(
        date_out = case_when(
          date_cGN_p == 1 ~ date_cGN,
          date_death < date_out ~ date_death,
          T ~ date_out
        ),
        date_out = if_else(month(date_out) == 1 &
                             day(date_out) == 1,
                           date_out + 1,
                           date_out)
      )
  )

data_6_DN <- data_5 %>%
  map(
    ~ .x %>%
      mutate(
        smo = as.factor(smo) %>% relevel(ref = '1'),
        date_in = as.Date.character(paste0(year, '-01-01'),
                                    format = '%Y-%m-%d'),
        date_out = as.Date.character(paste0(year, '-12-31'),
                                     format = '%Y-%m-%d'),
        age_in = year - as.double(date_birth)
      ) %>%
      filter((date_DN_p != 2) &
               (date_in >= date_fup) &
               (date_in <= date_death | is.na(date_death)) &
               (age_in >= 30) &
               (age_in < 90) &
               ((date_in >= date_immig) | is.na(date_immig)) &
               abroad3y == 0
      ) %>%
      mutate(
        date_out = case_when(
          date_DN_p == 1 ~ date_DN,
          date_death < date_out ~ date_death,
          T ~ date_out
        ),
        date_out = if_else(month(date_out) == 1 &
                             day(date_out) == 1,
                           date_out + 1,
                           date_out)
      )
  )

data_6_HN <- data_5 %>%
  map(
    ~ .x %>%
      mutate(
        smo = as.factor(smo) %>% relevel(ref = '1'),
        date_in = as.Date.character(paste0(year, '-01-01'),
                                    format = '%Y-%m-%d'),
        date_out = as.Date.character(paste0(year, '-12-31'),
                                     format = '%Y-%m-%d'),
        age_in = year - as.double(date_birth)
      ) %>%
      filter((date_HN_p != 2) &
               (date_in >= date_fup) &
               (date_in <= date_death | is.na(date_death)) &
               (age_in >= 30) &
               (age_in < 90) &
               ((date_in >= date_immig) | is.na(date_immig)) &
               abroad3y == 0
      ) %>%
      mutate(
        date_out = case_when(
          date_HN_p == 1 ~ date_HN,
          date_death < date_out ~ date_death,
          T ~ date_out
        ),
        date_out = if_else(month(date_out) == 1 &
                             day(date_out) == 1,
                           date_out + 1,
                           date_out)
      )
  )
## 01_01_01 Set up surv objects ================================================
surv_cGN <- map(data_6_cGN,
                ~ Surv(time = .x$date_in %>% as.double,
                       time2 = .x$date_out %>% as.double,
                       event = .x$date_cGN_p == 1,
                       type = 'counting'))

surv_DN <- map(data_6_DN,
                ~ Surv(time = .x$date_in %>% as.double,
                       time2 = .x$date_out %>% as.double,
                       event = .x$date_DN_p == 1,
                       type = 'counting'))

surv_HN <- map(data_6_HN,
                ~ Surv(time = .x$date_in %>% as.double,
                       time2 = .x$date_out %>% as.double,
                       event = .x$date_HN_p == 1,
                       type = 'counting'))

## 01_00_02 Define covariate models ============================================
models <- c(
  m0 = 'ns(age_in, 4) + sex',
  m1 = 'ns(age_in, 4) + sex + ses',
  m2 = 'ns(age_in, 4) + sex + ses + edu + smo + phy',
  m3 = 'ns(age_in, 4) + sex + ses + edu + smo + phy + ns(bmi, 4) + (date_DM_p > 0) + (date_HPT_p > 0)')

## 01_00_03 Regression analyses ================================================
res <- crossing(
  cohort = c('pps', 'got'),
  outcome = c('cGN', 'DN', 'HN'),
  model = c('m0', 'm1', 'm2'),
  exposure = c('pm25', 'pm10', 'nox'),
  lag = paste0('_lag', c('1', '2_5', '6_10'))
) %>% 
  purrr::pmap(\(cohort, outcome, model, exposure, lag) 
              scac_ckd_core_analysis(cohort, outcome, model, exposure, lag)) %>% 
  bind_rows()

## 01_00_04 Cleanup, plot and save =============================================
res_2 <- res %>%
  tibble::remove_rownames() %>% 
  #correction factor to present bc and nox in .1 and 10ug/m3 increments
  mutate(corrf = case_when(exposure == 'nox' ~ 10,
                           exposure == 'bc' ~ .1,
                           T ~ 1),
         exposure = paste0(exposure, lag),
         HR = paste0(sprintf(round(exp(beta)^corrf, 2), 
                             fmt = '%0.2f'), ' (',
                     sprintf(round(exp(beta-1.96*se)^corrf, 2), 
                             fmt = '%0.2f'), '-',
                     sprintf(round(exp(beta+1.96*se)^corrf, 2), 
                             fmt = '%0.2f'), ')')) %>% 
  select(-lag)

writexl::write_xlsx(res_2, 
                    here::here('Outputs', 
                               paste0('res_aetiological_', Sys.Date(), '.xlsx')))

res_2 %>%
  filter(model == 'm2') %>% 
  mutate(expo_f = str_sub(exposure, 1, 4) %>% 
           str_remove('_') %>% 
           str_to_upper() %>% 
           str_replace_all('X', 'x'),
         lag_f = case_when(str_detect(exposure, 'lag1') ~ '1 y',
                           str_detect(exposure, 'lag2') ~ '2-5 y',
                           str_detect(exposure, 'lag6') ~ '6-10 y'),
         exposure_f = paste0(expo_f, '\n', lag_f)) %>% 
  ggplot(aes(x = exposure_f, colour = cohort)) +
  geom_hline(aes(yintercept = 1), linetype = 'dashed') +
  geom_point(aes(y = exp(beta)),
             position = position_dodge(width = .5)) +
  geom_errorbar(aes(ymin = exp(beta-1.96*se),
                    ymax = exp(beta+1.96*se)),
                width = .5,
                position = position_dodge(width = .5)) +
  scale_y_continuous(transform = 'log', 
                     breaks = c(.5, 0.67, 1, 1.5, 2)) +
  coord_cartesian(ylim = c(.5, 2.2)) +
  labs(x = NULL, y = 'HR (95% CI)', colour = 'Cohort') +
  facet_wrap(vars(outcome), ncol = 1) +
  theme_linedraw() +
  theme(panel.grid.minor = element_blank())

ggsave(here::here('Outputs', paste0('res_aetiological_', Sys.Date(), '.png')), 
       width = 5, height = 6)