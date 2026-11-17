library('tidyverse')
setwd("/safe/data/Research projects/SCAC-CKD")

#data_5 <- readRDS(here::here('Code and stats', 'data_5_2026-10-08.rds'))
#data_7_CKD <- readRDS(here::here('Code and stats', 'data_7_CKD_2026-10-08.rds'))

df_table1 <- data_5 %>%
  map(
    ~ .x %>%
      mutate(
        smo = as.factor(smo) %>% relevel(ref = '1'),
        phy = as.factor(phy) %>% relevel(ref = '1'),
        date_in = as.Date.character(paste0(year, '-01-01'),
                                    format = '%Y-%m-%d'),
        date_out = as.Date.character(paste0(year, '-12-31'),
                                     format = '%Y-%m-%d'),
        age_in = year - as.double(date_birth)
      ) %>% 
      ungroup
  )

table1 <- map(df_table1, 
              \(x) purrr::reduce(
                list(
                  data.frame(var_code = c('n', 'age', 'women', 'ses', 'edu', 'smo', 'civ', 'phy', 'bmi'),
                             var_name = c('N',
                                          paste0('Age in 1990, mean (SD)'), 
                                          'Women, %',
                                          'Area-level inc. 1994 (kSEK), mean (SD)',
                                          'Low education, %',
                                          'Never-smokers, %',
                                          'Married or cohabiting, %',
                                          'Sedentary lifestyle, %',
                                          'BMI, mean (SD)')),
                  x %>% 
                    filter(year(as.Date(date_in)) == 1990,
                           !is.na(age_in),
                           !is.na(sex),
                           !is.na(ses),
                           !is.na(edu),
                           !is.na(civ),
                           !is.na(smo),
                           !is.na(phy),
                           !is.na(bmi),
                           !is.na(pm25)) %>% 
                    summarise(
                      n = sprintf('%i', n()),
                      age = paste0(sprintf('%.1f', mean(age_in)), ' (', sprintf('%.1f', sd(age_in)), ')'),
                      women = sprintf('%.0f%%', 100 * mean(sex == 0)),
                      ses = paste0(sprintf('%.1f', mean(ses / 1000, na.rm = T)), ' (', sprintf('%.1f', sd(ses / 1000, na.rm = T)), ')'),
                      edu = paste0(sprintf('%.1f', 100 * mean(edu == '1', na.rm = T)), '%'),
                      smo = paste0(sprintf('%.1f', 100 * mean(smo == '3', na.rm = T)), '%'),
                      civ = paste0(sprintf('%.1f', 100 * mean(civ == '1', na.rm = T)), '%'),
                      phy = paste0(sprintf('%.1f', 100 * mean(phy == '1', na.rm = T)), '%'),
                      bmi = paste0(sprintf('%.1f', mean(bmi, na.rm = T)), ' (', sprintf('%.1f', sd(bmi, na.rm = T)), ')')
                    ) %>% 
                    pivot_longer(cols = everything(), names_to = 'var_code', values_to = 'all'),
                  x %>% 
                    filter(year(as.Date(date_in)) == 1990,
                           !is.na(age_in),
                           !is.na(sex),
                           !is.na(ses),
                           !is.na(edu),
                           !is.na(civ),
                           !is.na(smo),
                           !is.na(phy),
                           !is.na(bmi),
                           !is.na(pm25)) %>% 
                    mutate(pm25_q = ntile(pm25, 4)) %>% 
                    filter(pm25_q == 1) %>% 
                    summarise(
                      n = sprintf('%i', n()),
                      age = paste0(sprintf('%.1f', mean(age_in)), ' (', sprintf('%.1f', sd(age_in)), ')'),
                      women = sprintf('%.0f%%', 100 * mean(sex == 0)),
                      ses = paste0(sprintf('%.1f', mean(ses / 1000, na.rm = T)), ' (', sprintf('%.1f', sd(ses / 1000, na.rm = T)), ')'),
                      edu = paste0(sprintf('%.1f', 100 * mean(edu == '1', na.rm = T)), '%'),
                      smo = paste0(sprintf('%.1f', 100 * mean(smo == '3', na.rm = T)), '%'),
                      civ = paste0(sprintf('%.1f', 100 * mean(civ == '1', na.rm = T)), '%'),
                      phy = paste0(sprintf('%.1f', 100 * mean(phy == '1', na.rm = T)), '%'),
                      bmi = paste0(sprintf('%.1f', mean(bmi, na.rm = T)), ' (', sprintf('%.1f', sd(bmi, na.rm = T)), ')')
                    ) %>% pivot_longer(cols = everything(), names_to = 'var_code', values_to = 'pm25_q1'),
                  x %>% 
                    filter(year(as.Date(date_in)) == 1990,
                           !is.na(age_in),
                           !is.na(sex),
                           !is.na(ses),
                           !is.na(edu),
                           !is.na(civ),
                           !is.na(smo),
                           !is.na(phy),
                           !is.na(bmi),
                           !is.na(pm25)) %>%
                    mutate(pm25_q = ntile(pm25, 4)) %>% 
                    filter(pm25_q == 2) %>% 
                    summarise(
                      n = sprintf('%i', n()),
                      age = paste0(sprintf('%.1f', mean(age_in)), ' (', sprintf('%.1f', sd(age_in)), ')'),
                      women = sprintf('%.0f%%', 100 * mean(sex == 0)),
                      ses = paste0(sprintf('%.1f', mean(ses / 1000, na.rm = T)), ' (', sprintf('%.1f', sd(ses / 1000, na.rm = T)), ')'),
                      edu = paste0(sprintf('%.1f', 100 * mean(edu == '1', na.rm = T)), '%'),
                      smo = paste0(sprintf('%.1f', 100 * mean(smo == '3', na.rm = T)), '%'),
                      civ = paste0(sprintf('%.1f', 100 * mean(civ == '1', na.rm = T)), '%'),
                      phy = paste0(sprintf('%.1f', 100 * mean(phy == '1', na.rm = T)), '%'),
                      bmi = paste0(sprintf('%.1f', mean(bmi, na.rm = T)), ' (', sprintf('%.1f', sd(bmi, na.rm = T)), ')')
                    ) %>% 
                    pivot_longer(cols = everything(), names_to = 'var_code', values_to = 'pm25_q2'),
                  x %>% 
                    filter(year(as.Date(date_in)) == 1990,
                           !is.na(age_in),
                           !is.na(sex),
                           !is.na(ses),
                           !is.na(edu),
                           !is.na(civ),
                           !is.na(smo),
                           !is.na(phy),
                           !is.na(bmi),
                           !is.na(pm25)) %>%
                    mutate(pm25_q = ntile(pm25, 4)) %>% 
                    filter(pm25_q == 3) %>% 
                    summarise(
                      n = sprintf('%i', n()),
                      age = paste0(sprintf('%.1f', mean(age_in)), ' (', sprintf('%.1f', sd(age_in)), ')'),
                      women = sprintf('%.0f%%', 100 * mean(sex == 0)),
                      ses = paste0(sprintf('%.1f', mean(ses / 1000, na.rm = T)), ' (', sprintf('%.1f', sd(ses / 1000, na.rm = T)), ')'),
                      edu = paste0(sprintf('%.1f', 100 * mean(edu == '1', na.rm = T)), '%'),
                      smo = paste0(sprintf('%.1f', 100 * mean(smo == '3', na.rm = T)), '%'),
                      civ = paste0(sprintf('%.1f', 100 * mean(civ == '1', na.rm = T)), '%'),
                      phy = paste0(sprintf('%.1f', 100 * mean(phy == '1', na.rm = T)), '%'),
                      bmi = paste0(sprintf('%.1f', mean(bmi, na.rm = T)), ' (', sprintf('%.1f', sd(bmi, na.rm = T)), ')')
                    ) %>% pivot_longer(cols = everything(), names_to = 'var_code', values_to = 'pm25_q3'),
                  x %>% 
                    filter(year(as.Date(date_in)) == 1990,
                           !is.na(age_in),
                           !is.na(sex),
                           !is.na(ses),
                           !is.na(edu),
                           !is.na(civ),
                           !is.na(smo),
                           !is.na(phy),
                           !is.na(bmi),
                           !is.na(pm25)) %>%
                    mutate(pm25_q = ntile(pm25, 4)) %>% 
                    filter(pm25_q == 4) %>% 
                    summarise(
                      n = sprintf('%i', n()),
                      age = paste0(sprintf('%.1f', mean(age_in)), ' (', sprintf('%.1f', sd(age_in)), ')'),
                      women = sprintf('%.0f%%', 100 * mean(sex == 0)),
                      ses = paste0(sprintf('%.1f', mean(ses / 1000, na.rm = T)), ' (', sprintf('%.1f', sd(ses / 1000, na.rm = T)), ')'),
                      edu = paste0(sprintf('%.1f', 100 * mean(edu == '1', na.rm = T)), '%'),
                      smo = paste0(sprintf('%.1f', 100 * mean(smo == '3', na.rm = T)), '%'),
                      civ = paste0(sprintf('%.1f', 100 * mean(civ == '1', na.rm = T)), '%'),
                      phy = paste0(sprintf('%.1f', 100 * mean(phy == '1', na.rm = T)), '%'),
                      bmi = paste0(sprintf('%.1f', mean(bmi, na.rm = T)), ' (', sprintf('%.1f', sd(bmi, na.rm = T)), ')')
                    ) %>% 
                    pivot_longer(cols = everything(), names_to = 'var_code', values_to = 'pm25_q4'),
                  x %>% 
                    filter(year(as.Date(date_in)) == 1990,
                           is.na(age_in) |
                             is.na(sex) |
                             is.na(ses) |
                             is.na(edu) |
                             is.na(civ) |
                             is.na(smo) |
                             is.na(phy) |
                             is.na(bmi) |
                             is.na(pm25)) %>%
                    summarise(
                      n = sprintf('%i', n()),
                      age = paste0(sprintf('%.1f', mean(age_in)), ' (', sprintf('%.1f', sd(age_in)), ')'),
                      women = sprintf('%.0f%%', 100 * mean(sex == 0, na.rm = T)),
                      ses = paste0(sprintf('%.1f', mean(ses / 1000, na.rm = T)), ' (', sprintf('%.1f', sd(ses / 1000, na.rm = T)), ')'),
                      edu = paste0(sprintf('%.1f', 100 * mean(edu == '1', na.rm = T)), '%'),
                      smo = paste0(sprintf('%.1f', 100 * mean(smo == '3', na.rm = T)), '%'),
                      civ = paste0(sprintf('%.1f', 100 * mean(civ == '1', na.rm = T)), '%'),
                      phy = paste0(sprintf('%.1f', 100 * mean(phy == '1', na.rm = T)), '%'),
                      bmi = paste0(sprintf('%.1f', mean(bmi, na.rm = T)), ' (', sprintf('%.1f', sd(bmi, na.rm = T)), ')')
                    ) %>% pivot_longer(cols = everything(), names_to = 'var_code', values_to = 'excl')
                ),
                left_join)
)

writexl::write_xlsx(table1, here::here('Outputs', paste0('table1_', Sys.Date(), '.xlsx')))

table2 <- map(
  data_7_CKD,
  \(x) left_join(
    filter(x, year(date_in) == 2000) %>% 
      select(contains('lag')) %>% 
      summarise(across(everything(), 
                       \(x) sprintf('%.1f (%.1f)', median(x, na.rm = T), IQR(x, na.rm = T)))) %>% 
      pivot_longer(cols = everything(), names_to = 'exposure', values_to = 'median_iqr_2000') %>% 
      mutate(pollutant = str_sub(exposure, 1, 4)) %>% 
      arrange(desc(pollutant), exposure),
    select(x, contains('lag')) %>% 
      cor(use = 'pairwise') %>% 
      as.data.frame() %>% 
      mutate(across(everything(), \(y) sprintf('%.2f', y))) %>% 
      rownames_to_column('exposure') %>% 
      arrange(exposure) %>% 
      select(exposure, contains('pm25_lag')),
    by = 'exposure') %>% 
    select(-c('pollutant'))
) 
writexl::write_xlsx(table2, here::here('Outputs', paste0('table2_', Sys.Date(), '.xlsx')))

## 00_03_01 Visualizations of py and cases over time ===========================
cowplot::plot_grid(
  data_6_CKD %>%
    map2(
      c('got', 'pps'),
      ~ .x %>%
        mutate(
          cohort = .y,
          has_expo =
            !is.na(pm25_lag1) |
            !is.na(pm25_lag2_5) |
            !is.na(pm25_lag6_10),
          has_covariates = 
            !is.na(ses) & 
            #!is.na(edu) & 
            !is.na(smo) & 
            !is.na(phy) & 
            !is.na(bmi)
        )
    ) %>%
    bind_rows %>%
    ggplot(aes(x = year, fill = has_expo, alpha = has_covariates)) +
    geom_histogram(binwidth = 1) +
    scale_x_continuous(limits = c(1989, 2024)) +
    scale_alpha_manual(values = c(.5, 1)) +
    facet_wrap(vars(cohort), nrow = 1) +
    ggtitle('Person-years (for CKD) over calendar years'),
  data_6_CKD %>%
    map2(
      c('got', 'pps'),
      ~ .x %>%
        mutate(
          cohort = .y,
          has_expo =
            !is.na(pm25_lag1) |
            !is.na(pm25_lag2_5) |
            !is.na(pm25_lag6_10),
          has_covariates = 
            !is.na(ses) & 
            #!is.na(edu) & 
            !is.na(smo) & 
            !is.na(phy) & 
            !is.na(bmi)
        )
    ) %>%
    bind_rows %>%
    filter(date_CKD_p == 1) %>%
    ggplot(aes(x = year, fill = has_expo, alpha = has_covariates)) +
    geom_histogram(binwidth = 1) +
    scale_x_continuous(limits = c(1989, 2024)) +
    scale_alpha_manual(values = c(.5, 1)) +
    facet_wrap(vars(cohort), nrow = 1) +
    ggtitle('CKD cases over calendar years'),
  ncol = 1,
  align = 'v'
)
ggsave(here::here('Outputs', paste0('Person-years and CKD cases_', 
                                    Sys.Date(),
                                    '.svg')))

cowplot::plot_grid(
  data_6_CKD %>% 
    map2(c('got', 'pps'), 
         ~.x %>% 
           mutate(cohort = .y,  
                  has_expo =  !is.na(pm25_lag1) | 
                    !is.na(pm25_lag2_5) | 
                    !is.na(pm25_lag6_10))) %>% 
    bind_rows %>% 
    filter(date_CKD_p == 1) %>% 
    ggplot(aes(x = age_in, fill = has_expo)) + 
    geom_histogram(binwidth = 1) + 
    facet_wrap(vars(cohort), nrow = 1) +
    ggtitle('CKD cases over age years'),
  data_6_RRT %>% 
    map2(c('got', 'pps'), 
         ~.x %>% 
           mutate(cohort = .y,  
                  has_expo =  !is.na(pm25_lag1) | 
                    !is.na(pm25_lag2_5) | 
                    !is.na(pm25_lag6_10))) %>% 
    bind_rows %>% 
    filter(date_RRT_p == 1) %>% 
    ggplot(aes(x = age_in, fill = has_expo)) + 
    geom_histogram(binwidth = 1) + 
    facet_wrap(vars(cohort), nrow = 1) +
    ggtitle('RRT cases over age years'),
  ncol = 1,
  align = 'v'
)
ggsave(here::here('Outputs', paste0('CKD and RRT cases over age_', 
                                    Sys.Date(),
                                    '.svg')))


cowplot::plot_grid(
  data_6_CKD %>% 
    map2(c('got', 'pps'), 
         ~.x %>% 
           mutate(cohort = .y,  
                  has_expo =  !is.na(pm25_lag1) | 
                    !is.na(pm25_lag2_5) | 
                    !is.na(pm25_lag6_10))) %>% 
    bind_rows %>% 
    filter(date_CKD_p == 1) %>% 
    ggplot(aes(x = year, fill = has_expo)) + 
    geom_histogram(binwidth = 1) + 
    facet_wrap(vars(cohort), nrow = 1) +
    ggtitle('CKD cases over calendar years'),
  data_6_RRT %>% 
    map2(c('got', 'pps'), 
         ~.x %>% 
           mutate(cohort = .y,  
                  has_expo =  !is.na(pm25_lag1) | 
                    !is.na(pm25_lag2_5) | 
                    !is.na(pm25_lag6_10))) %>% 
    bind_rows %>% 
    filter(date_RRT_p == 1) %>% 
    ggplot(aes(x = year, fill = has_expo)) + 
    geom_histogram(binwidth = 1) + 
    facet_wrap(vars(cohort), nrow = 1) +
    ggtitle('RRT cases over calendar years'),
  ncol = 1,
  align = 'v'
)
ggsave(here::here('Outputs', paste0('CKD and RRT cases_', 
                                    Sys.Date(),
                                    '.svg')))


par_1 %>%
  pivot_longer(cols = -id,
               names_to = 'diagnosis',
               names_prefix = 'date_',
               values_to = 'year_diag') %>% 
  mutate(year_diag = year(year_diag) %>% as.integer()) %>% 
  ggplot(aes(x = year_diag, fill = diagnosis)) +
  geom_bar() +
  ggtitle('Diagnoses in the In- and Outpatient Register')
ggsave(here::here('Outputs', paste0('Diagnoses per year (PAR)_', 
                                    Sys.Date(),
                                    '.svg')))

dors_1 %>%
  pivot_longer(cols = -c(id, reg, date_death),
               names_to = 'diagnosis',
               names_prefix = 'date_',
               values_to = 'year_diag') %>% 
  mutate(year_diag = year(year_diag) %>% as.integer()) %>% 
  ggplot(aes(x = year_diag, fill = diagnosis)) +
  geom_bar() +
  ggtitle('Diagnoses in the Cause-of-Death Register')
ggsave(here::here('Outputs', paste0('Diagnoses per year (DORS)_', 
                                    Sys.Date(),
                                    '.svg')))

