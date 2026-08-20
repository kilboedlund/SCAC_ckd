library('tidyverse')

setwd("/safe/data/Research projects/SCAC-CKD")

# 00_03_00 Overview map ========================================================
coords_sf <- coords %>% 
  filter(!is.na(Xkoord), !is.na(Ykoord)) %>% 
  sf::st_as_sf(coords = c('Ykoord', 'Xkoord'), crs = 3006) %>% 
  sf::st_transform(4326)

bbox <- sf::st_bbox(
  c(xmin = 11,
    ymin = 55,
    xmax = 24,
    ymax = 69),
  crs = 4326)

ggplot() +
  geom_sf(data = rnaturalearth::ne_countries(country = c('norway', 
                                                         'sweden', 
                                                         'finland',
                                                         'denmark',
                                                         'estonia',
                                                         'latvia',
                                                         'lithuania',
                                                         'russia',
                                                         'poland',
                                                         'germany'), 
                                             scale = 'medium'),
          fill = 'white',
          colour = 'black',
          linewidth = 1) +
  geom_sf(data = scac_sf,
          aes(colour = cohort),
          size = 1,
          shape = 20,
          alpha = 0.25) +
  coord_sf(xlim = sf::st_bbox(bbox)[c('xmin', 'xmax')],
           ylim = sf::st_bbox(bbox)[c('ymin', 'ymax')]) +
  scale_colour_manual(values = c('darkorange', 'darkorchid')) +
  theme_bw() +
  theme(legend.position = 'none',
        panel.background = element_rect(fill = 'deepskyblue4'))

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

