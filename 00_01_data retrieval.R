library('tidyverse')

setwd("/safe/data/Research projects/SCAC-CKD")

source(here::here('Code and stats/00_00_helper functions.R'))

scb_path <- 'Data from SCB/Leverans_250903/'
sos_path <- 'Data from SoS/116891_2023_Leo Stockfelt_K1_Best_Lev1/'
exp_path <- 'Data exposure/'

## 00_00_00 Indata files, i.e. the data we submitted to SCB ====================
indata_got <- haven::read_sas(
  here::here(scb_path, 'got_indata_lev.sas7bdat'))

indata_pps <- haven::read_sas(
  here::here(scb_path, 'pps_indata_lev.sas7bdat'))

## 00_00_01 Demographic data files from SCB registers ==========================
ke_lev_demografi <- haven::read_sas(
  here::here(scb_path, 'ke_lev_demografi.sas7bdat')
)
# this file includes data on sex, and date and region of birth, for all 
# cohorts

#ke_lev_fob <- multi_read_sas(paste0(scb_path, 'ke_lev_fob_'),
#                                    c(1970, 1975, 1980, 1985, 1990),
#                                    '.sas7bdat') %>% 
#  map(~filter(.x, GOT_MONICA == '1' | PPS == '1') %>% 
#        select(-c(DIET, SNACK)))
# these files include data on occupation in 1970, for all cohorts

ke_lev_koord <- multi_read_sas(paste0(scb_path, 'ke_lev_koord_'), 
                               2012:2023,
                               '.sas7bdat') %>% 
  map(~filter(.x, Got_monica == 1) %>% 
        select(-Got_monica) %>% 
        pivot_longer(cols = -lopnr, names_to = 'dir', values_to = 'coords')) %>% 
  map2(2012:2023, ~ .x %>% mutate(year = .y)) %>% 
  bind_rows() %>% 
  rename('id' = 'lopnr')
# this file includes data on X and Y coordinates, for GOT-MONICA

ke_lev_migrationer <- haven::read_sas(
  here::here(scb_path, 'ke_lev_migrationer.sas7bdat')
)

ke_lev_lisa <- multi_read_sas(paste0(scb_path, 'ke_lev_lisa_'),
                             c(1990, 2001:2023),
                             '.sas7bdat') %>% 
  map2(c(1990, 2001:2023),
       ~ .x %>% 
         filter(GOT_MONICA == '1' | PPS == '1') %>% 
         select(-c(DIET, SNACK), id = LopNr) %>% 
         mutate(year = .y)) %>% 
  bind_rows

ke_lev_lisa <- list(got = ke_lev_lisa %>% 
                      filter(GOT_MONICA == 1) %>% 
                      select(-GOT_MONICA, PPS) %>% 
                      mutate(cohort = 'got'),
                    pps = ke_lev_lisa %>% 
                      filter(PPS == 1) %>% 
                      select(-GOT_MONICA, PPS) %>% 
                      mutate(cohort = 'pps'))

## 00_00_02 Hospital and Cause-of-Death data from SoS ==========================
dors <- haven::read_sas(
  here::here(sos_path, 'ut_r_dors_116891_2023.sas7bdat')
)
# this file contains dors data for PPS and GOT-MONICA
dors %>% 
  mutate(date = as.Date.character(DODSDAT, format = '%Y%m%d')) %>% 
  select(LopNr, date) %>% 
  left_join(ke_lev_demografi %>% 
              select(LopNr, GOT_MONICA, PPS)) %>% 
  pivot_longer(cols = c(GOT_MONICA, PPS), names_to = 'cohort') %>% 
  filter(value == '1') %>% select(-value) %>% 
  ggplot(aes(x = year(date), fill = cohort)) + 
  geom_histogram(binwidth = 1) +
  labs(x = 'Year', y = 'Count') +
  facet_wrap(vars(cohort), ncol = 1) +
  theme(legend.position = 'none') +
  ggtitle('Cause-of-Death Register')

par_ov <- haven::read_sas(
  here::here(sos_path, 'ut_r_par_ov_116891_2023.sas7bdat')
)

# this file contains outpatient visit data for PPS and GOT-MONICA
par_ov %>% 
  mutate(date = as.Date.character(INDATUMA, format = '%Y%m%d')) %>% 
  select(LopNr, date) %>% 
  left_join(ke_lev_demografi %>% 
              select(LopNr, GOT_MONICA, PPS) %>% distinct) %>% 
  pivot_longer(cols = c(GOT_MONICA, PPS), names_to = 'cohort') %>% 
  filter(value == '1') %>% select(-value) %>% 
  ggplot(aes(x = year(date), fill = cohort)) + 
  geom_histogram(binwidth = 1) +
  labs(x = 'Year', y = 'Count') +
  facet_wrap(vars(cohort), ncol = 1) +
  theme(legend.position = 'none') +
  ggtitle('Outpatient Register')

par_sv <- haven::read_sas(
  here::here(sos_path, 'ut_r_par_sv_116891_2023.sas7bdat')
)

# this file contains inpatient data for PPS and GOT-MONICA
par_sv %>% 
  mutate(date = as.Date.character(INDATUMA, format = '%Y%m%d')) %>% 
  select(LopNr, date) %>% 
  left_join(ke_lev_demografi %>% 
              select(LopNr, GOT_MONICA, PPS) %>% distinct) %>% 
  pivot_longer(cols = c(GOT_MONICA, PPS), names_to = 'cohort') %>%
  filter(value == '1') %>% select(-value) %>% 
  ggplot(aes(x = year(date), fill = cohort)) + 
  geom_histogram(binwidth = 1) +
  labs(x = 'Year', y = 'Count') +
  facet_wrap(vars(cohort), ncol = 1) +
  theme(legend.position = 'none') +
  ggtitle('Inpatient Register')

## 00_00_03 Exposure data ======================================================
expo_data <- list(
  pm25_1990 = stars::read_stars(here::here(exp_path, 'xPM25_1990_total.tif')),
  pm25_2000 = stars::read_stars(here::here(exp_path, 'xPM25_2000_total.tif')),
  pm25_2011 = stars::read_stars(here::here(exp_path, 'xPM25_2011_total.tif')),
  pm10_1990 = stars::read_stars(here::here(exp_path, 'xPM10_1990_total.tif')),
  pm10_2000 = stars::read_stars(here::here(exp_path, 'xPM10_2000_total.tif')),
  pm10_2011 = stars::read_stars(here::here(exp_path, 'xPM10_2011_total.tif')),
  nox_1990 = stars::read_stars(here::here(exp_path, 'NOx_1990_total.tif')),
  nox_2000 = stars::read_stars(here::here(exp_path, 'NOx_2000_total.tif')),
  nox_2011 = stars::read_stars(here::here(exp_path, 'NOx_2011_total.tif')),
  bc_1990 = stars::read_stars(here::here(exp_path, 'xBC_1990_total.tif')),
  bc_2000 = stars::read_stars(here::here(exp_path, 'xBC_2000_total.tif')),
  bc_2011 = stars::read_stars(here::here(exp_path, 'xBC_2011_total.tif'))
) %>% 
  map(\(x) sf::st_set_crs(x, 3006))

