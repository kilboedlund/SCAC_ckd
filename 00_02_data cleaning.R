library('tidyverse')

setwd("/safe/data/Research projects/SCAC-CKD")

## 00_02_00 Create two base data sets ==========================================
# These are lists of separate cohort datasets
data_1 <- list(
  got = ke_lev_demografi %>%
    filter(SenPNr == 1) %>% 
    select(
      id = LopNr,
      date_birth = FodAr,
      reg_birth = VarldsdelNamn,
      GOT_MONICA
    ) %>%
    filter(GOT_MONICA == '1') %>%
    select(-c(GOT_MONICA)) %>%
    left_join(
      indata_got %>%
        mutate(year = as.Date.character(paste0(
          as.character(year + 1), '0101'
        ),
        format = '%Y%m%d')) %>%
        select(
          id = LopNr,
          date_fup = year,
          date_death_indata = d_death,
          sex,
          ses = area_income94,
          civ = mar_stat,
          edu = edulev,
          smo = smoking,
          smo_numcigs = numcigs,
          phy = physact,
          bmi
        ),
      by = 'id'
    ),
  
  pps = ke_lev_demografi %>%
    filter(SenPNr == 1) %>% 
    select(
      id = LopNr,
      date_birth = FodAr,
      reg_birth = VarldsdelNamn,
      PPS
    ) %>%
    filter(PPS == '1') %>%
    select(-c(PPS)) %>%
    left_join(
      indata_pps %>%
        select(
          id = LopNr,
          date_death_indata = d_death,
          sex,
          ses = area_income94,
          civ = mar_stat,
          edu = edulev,
          smo = smoking,
          phy = physact,
          bmi
        ) %>% 
        mutate(date_fup = as.Date.numeric(1826)),
      by = 'id'
    )
)
# 21 individuals are in both GOT_MONICA and PPS

## 00_02_01 Create a clean coordinate data set =================================
# This section extracts coordinates and assigns exposure for each of the cohorts 
# listed
coords_1 <- list(got = indata_got,
                 pps = indata_pps) %>%
  purrr::map(
    ~ .x %>%
      select(contains('koord'), id = LopNr) %>%
      pivot_longer(
        cols = contains('koord'),
        names_to = 'dir',
        values_to = 'coords'
      ) %>%
      mutate(
        year = str_sub(dir, -4, -1) %>% as.integer,
        dir = str_sub(dir, 1, 6) %>% str_to_lower
      ) %>%
      pivot_wider(
        id_cols = 'id',
        names_from = c('dir', 'year'),
        names_sep = '_',
        values_from = 'coords'
      )
  ) %>%
  purrr::map2(
    c(2012, 2013),
    ~ .x %>%
      left_join(
        ke_lev_koord %>%
          filter(year > .y) %>%
          pivot_wider(
            id_cols = 'id',
            names_from = c('dir', 'year'),
            names_sep = '_',
            values_from = 'coords'
          ),
        by = 'id'
      ) %>%
      pivot_longer(
        cols = -'id',
        names_to = 'dir',
        values_to = 'coords'
      ) %>%
      mutate(
        year = str_sub(dir, -4, -1) %>% as.integer,
        dir = str_sub(dir, 1, 6) %>% str_to_lower
      ) %>%
      pivot_wider(
        id_cols = c('id', 'year'),
        names_from = 'dir',
        values_from = 'coords'
      ) %>%
      filter(!is.na(xkoord) & !is.na(ykoord))
  )

coords_2 <-
  coords_1 %>%
  map(~ .x %>%
        select(xkoord, ykoord) %>%
        distinct() %>%
        sf::st_as_sf(coords = c('ykoord', 'xkoord')))

## 00_02_02 Connect the coordinate data set with air pollution data ============
# meteorological yearly correction coefficients for 1991-1999 and 2001-2010
corr_coef <- c(1.067, 1.065, 1.040, 1.027, 1.135, 1.174, 1.200, 0.995, 1.086,
               1.122, 1.032, 1.124, 1.033, 1.020, 1.016, 1.014, 0.994, 1.070,
               1.213)

# extract exposure values for all ever-inhabited coordinates for 1990, 2000, and
# 2011, and then impute intermediate years linearly using correction coef above
coords_3 <- map(
  coords_2,
  ~ purrr::reduce(
    list(
      stars::st_extract(expo_data$pm25_1990,
                        at = .x),
      stars::st_extract(expo_data$pm25_2000,
                        at = .x),
      stars::st_extract(expo_data$pm25_2011,
                        at = .x),
      stars::st_extract(expo_data$pm10_1990,
                        at = .x),
      stars::st_extract(expo_data$pm10_2000,
                        at = .x),
      stars::st_extract(expo_data$pm10_2011,
                        at = .x),
      stars::st_extract(expo_data$nox_1990,
                        at = .x),
      stars::st_extract(expo_data$nox_2000,
                        at = .x),
      stars::st_extract(expo_data$nox_2011,
                        at = .x),
      stars::st_extract(expo_data$bc_1990,
                        at = .x),
      stars::st_extract(expo_data$bc_2000,
                        at = .x),
      stars::st_extract(expo_data$bc_2011,
                        at = .x)
    ),
    sf::st_join
  ) %>%
    rename(
      'pm25_1990' = 'xPM25_1990_total.tif',
      'pm25_2000' = 'xPM25_2000_total.tif',
      'pm25_2011' = 'xPM25_2011_total.tif',
      'pm10_1990' = 'xPM10_1990_total.tif',
      'pm10_2000' = 'xPM10_2000_total.tif',
      'pm10_2011' = 'xPM10_2011_total.tif',
      'nox_1990' = 'NOx_1990_total.tif',
      'nox_2000' = 'NOx_2000_total.tif',
      'nox_2011' = 'NOx_2011_total.tif',
      'bc_1990' = 'xBC_1990_total.tif',
      'bc_2000' = 'xBC_2000_total.tif',
      'bc_2011' = 'xBC_2011_total.tif'
    ) %>%
    mutate(
      pm25_1991 = ((9 / 10) * pm25_1990 + (1 / 10) * pm25_2000) * corr_coef[1],
      pm25_1992 = ((8 / 10) * pm25_1990 + (2 / 10) * pm25_2000) * corr_coef[2],
      pm25_1993 = ((7 / 10) * pm25_1990 + (3 / 10) * pm25_2000) * corr_coef[3],
      pm25_1994 = ((6 / 10) * pm25_1990 + (4 / 10) * pm25_2000) * corr_coef[4],
      pm25_1995 = ((5 / 10) * pm25_1990 + (5 / 10) * pm25_2000) * corr_coef[5],
      pm25_1996 = ((4 / 10) * pm25_1990 + (6 / 10) * pm25_2000) * corr_coef[6],
      pm25_1997 = ((3 / 10) * pm25_1990 + (7 / 10) * pm25_2000) * corr_coef[7],
      pm25_1998 = ((2 / 10) * pm25_1990 + (8 / 10) * pm25_2000) * corr_coef[8],
      pm25_1999 = ((1 / 10) * pm25_1990 + (9 / 10) * pm25_2000) * corr_coef[9],
      pm25_2001 = ((10 / 11) * pm25_2000 + (1 / 11) * pm25_2011) *
        corr_coef[10],
      pm25_2002 = ((9 / 11) * pm25_2000 + (2 / 11) * pm25_2011) * corr_coef[11],
      pm25_2003 = ((8 / 11) * pm25_2000 + (3 / 11) * pm25_2011) * corr_coef[12],
      pm25_2004 = ((7 / 11) * pm25_2000 + (4 / 11) * pm25_2011) * corr_coef[13],
      pm25_2005 = ((6 / 11) * pm25_2000 + (5 / 11) * pm25_2011) * corr_coef[14],
      pm25_2006 = ((5 / 11) * pm25_2000 + (6 / 11) * pm25_2011) * corr_coef[15],
      pm25_2007 = ((4 / 11) * pm25_2000 + (7 / 11) * pm25_2011) * corr_coef[16],
      pm25_2008 = ((3 / 11) * pm25_2000 + (8 / 11) * pm25_2011) * corr_coef[17],
      pm25_2009 = ((2 / 11) * pm25_2000 + (9 / 11) * pm25_2011) * corr_coef[18],
      pm25_2010 = ((1 / 11) * pm25_2000 + (10 / 11) * pm25_2011) *
        corr_coef[19],
      
      pm10_1991 = ((9 / 10) * pm10_1990 + (1 / 10) * pm10_2000) * corr_coef[1],
      pm10_1992 = ((8 / 10) * pm10_1990 + (2 / 10) * pm10_2000) * corr_coef[2],
      pm10_1993 = ((7 / 10) * pm10_1990 + (3 / 10) * pm10_2000) * corr_coef[3],
      pm10_1994 = ((6 / 10) * pm10_1990 + (4 / 10) * pm10_2000) * corr_coef[4],
      pm10_1995 = ((5 / 10) * pm10_1990 + (5 / 10) * pm10_2000) * corr_coef[5],
      pm10_1996 = ((4 / 10) * pm10_1990 + (6 / 10) * pm10_2000) * corr_coef[6],
      pm10_1997 = ((3 / 10) * pm10_1990 + (7 / 10) * pm10_2000) * corr_coef[7],
      pm10_1998 = ((2 / 10) * pm10_1990 + (8 / 10) * pm10_2000) * corr_coef[8],
      pm10_1999 = ((1 / 10) * pm10_1990 + (9 / 10) * pm10_2000) * corr_coef[9],
      pm10_2001 = ((10 / 11) * pm10_2000 + (1 / 11) * pm10_2011) *
        corr_coef[10],
      pm10_2002 = ((9 / 11) * pm10_2000 + (2 / 11) * pm10_2011) * corr_coef[11],
      pm10_2003 = ((8 / 11) * pm10_2000 + (3 / 11) * pm10_2011) * corr_coef[12],
      pm10_2004 = ((7 / 11) * pm10_2000 + (4 / 11) * pm10_2011) * corr_coef[13],
      pm10_2005 = ((6 / 11) * pm10_2000 + (5 / 11) * pm10_2011) * corr_coef[14],
      pm10_2006 = ((5 / 11) * pm10_2000 + (6 / 11) * pm10_2011) * corr_coef[15],
      pm10_2007 = ((4 / 11) * pm10_2000 + (7 / 11) * pm10_2011) * corr_coef[16],
      pm10_2008 = ((3 / 11) * pm10_2000 + (8 / 11) * pm10_2011) * corr_coef[17],
      pm10_2009 = ((2 / 11) * pm10_2000 + (9 / 11) * pm10_2011) * corr_coef[18],
      pm10_2010 = ((1 / 11) * pm10_2000 + (10 / 11) * pm10_2011) *
        corr_coef[19],
      
      nox_1991 = ((9 / 10) * nox_1990 + (1 / 10) * nox_2000) * corr_coef[1],
      nox_1992 = ((8 / 10) * nox_1990 + (2 / 10) * nox_2000) * corr_coef[2],
      nox_1993 = ((7 / 10) * nox_1990 + (3 / 10) * nox_2000) * corr_coef[3],
      nox_1994 = ((6 / 10) * nox_1990 + (4 / 10) * nox_2000) * corr_coef[4],
      nox_1995 = ((5 / 10) * nox_1990 + (5 / 10) * nox_2000) * corr_coef[5],
      nox_1996 = ((4 / 10) * nox_1990 + (6 / 10) * nox_2000) * corr_coef[6],
      nox_1997 = ((3 / 10) * nox_1990 + (7 / 10) * nox_2000) * corr_coef[7],
      nox_1998 = ((2 / 10) * nox_1990 + (8 / 10) * nox_2000) * corr_coef[8],
      nox_1999 = ((1 / 10) * nox_1990 + (9 / 10) * nox_2000) * corr_coef[9],
      nox_2001 = ((10 / 11) * nox_2000 + (1 / 11) * nox_2011) *
        corr_coef[10],
      nox_2002 = ((9 / 11) * nox_2000 + (2 / 11) * nox_2011) * corr_coef[11],
      nox_2003 = ((8 / 11) * nox_2000 + (3 / 11) * nox_2011) * corr_coef[12],
      nox_2004 = ((7 / 11) * nox_2000 + (4 / 11) * nox_2011) * corr_coef[13],
      nox_2005 = ((6 / 11) * nox_2000 + (5 / 11) * nox_2011) * corr_coef[14],
      nox_2006 = ((5 / 11) * nox_2000 + (6 / 11) * nox_2011) * corr_coef[15],
      nox_2007 = ((4 / 11) * nox_2000 + (7 / 11) * nox_2011) * corr_coef[16],
      nox_2008 = ((3 / 11) * nox_2000 + (8 / 11) * nox_2011) * corr_coef[17],
      nox_2009 = ((2 / 11) * nox_2000 + (9 / 11) * nox_2011) * corr_coef[18],
      nox_2010 = ((1 / 11) * nox_2000 + (10 / 11) * nox_2011) *
        corr_coef[19],
      
      bc_1991 = ((9 / 10) * bc_1990 + (1 / 10) * bc_2000) * corr_coef[1],
      bc_1992 = ((8 / 10) * bc_1990 + (2 / 10) * bc_2000) * corr_coef[2],
      bc_1993 = ((7 / 10) * bc_1990 + (3 / 10) * bc_2000) * corr_coef[3],
      bc_1994 = ((6 / 10) * bc_1990 + (4 / 10) * bc_2000) * corr_coef[4],
      bc_1995 = ((5 / 10) * bc_1990 + (5 / 10) * bc_2000) * corr_coef[5],
      bc_1996 = ((4 / 10) * bc_1990 + (6 / 10) * bc_2000) * corr_coef[6],
      bc_1997 = ((3 / 10) * bc_1990 + (7 / 10) * bc_2000) * corr_coef[7],
      bc_1998 = ((2 / 10) * bc_1990 + (8 / 10) * bc_2000) * corr_coef[8],
      bc_1999 = ((1 / 10) * bc_1990 + (9 / 10) * bc_2000) * corr_coef[9],
      bc_2001 = ((10 / 11) * bc_2000 + (1 / 11) * bc_2011) *
        corr_coef[10],
      bc_2002 = ((9 / 11) * bc_2000 + (2 / 11) * bc_2011) * corr_coef[11],
      bc_2003 = ((8 / 11) * bc_2000 + (3 / 11) * bc_2011) * corr_coef[12],
      bc_2004 = ((7 / 11) * bc_2000 + (4 / 11) * bc_2011) * corr_coef[13],
      bc_2005 = ((6 / 11) * bc_2000 + (5 / 11) * bc_2011) * corr_coef[14],
      bc_2006 = ((5 / 11) * bc_2000 + (6 / 11) * bc_2011) * corr_coef[15],
      bc_2007 = ((4 / 11) * bc_2000 + (7 / 11) * bc_2011) * corr_coef[16],
      bc_2008 = ((3 / 11) * bc_2000 + (8 / 11) * bc_2011) * corr_coef[17],
      bc_2009 = ((2 / 11) * bc_2000 + (9 / 11) * bc_2011) * corr_coef[18],
      bc_2010 = ((1 / 11) * bc_2000 + (10 / 11) * bc_2011) *
        corr_coef[19]
    )
)

# back-join the extracted pollutant concentrations to the actual coordinates 
coords_4 <- purrr::map2(coords_1 %>%
                          map( ~ .x %>% 
                                 filter(!is.na(ykoord) &
                                          !is.na(xkoord)) %>%
                                 sf::st_as_sf(coords = c('ykoord', 'xkoord'))),
                        coords_3,
                        
                        ~ sf::st_join(.x, .y, by = 'geometry') %>%
                          pivot_longer(cols = -c('id', 'year', 'geometry'),
                                       names_to = c('expo', 'year_expo'),
                                       names_sep = '_',
                                       values_to = 'value'
                          ) %>% 
                          filter(year == year_expo) %>% 
                          pivot_wider(id_cols = c('id', 'year', 'geometry'),
                                      names_from = 'expo',
                                      values_from = 'value')
)

# Here I plot PM25 for sanity check
for (e in c('pm25', 'pm10', 'nox', 'bc')) {
  map2(coords_4,
       names(coords_4),
       ~ {
         ggplot() +
           geom_sf(data = rnaturalearth::ne_countries(
             country = 'sweden',
             type = 'countries',
             scale = 'medium'
           )) +
           geom_sf(data = .x %>%
                     sf::st_set_crs(3006) %>%
                     filter(!is.na(get(e)), year %in% c(1990, 1995, 2000, 2010)),
                   aes(colour = bc)) +
           stat_sf_coordinates() +
           scale_colour_viridis_c(option = 'turbo') +
           coord_sf(xlim = c(11, 13), ylim = c(57.2, 58.6)) +
           facet_wrap(vars(year)) +
           labs(x = NULL, y = NULL) +
           theme_bw() +
           theme(axis.text = element_blank(),
                 axis.ticks = element_blank())
         ggsave(
           filename = here::here('Outputs', 'Maps', 
                                 paste0(.y, '_map_', e , '.svg')),
           device = 'svg',
           height = 8,
           width = 5
         )
       })
}

# Merge together the coordinate/exposure data with the cohort data
data_2 <-
  map2(data_1,
       coords_4,
       ~ expand_grid(id = unique(.x %>% pull(id)), 
                     year = 1990:2023) %>% 
         left_join(.x, 
                   by = 'id') %>% 
         left_join(.y,
                   by = c('id', 'year'))
  )

## 00_02_03 Connect indata data set with migration data ========================
# First I clean the migration data
mig_1 <- ke_lev_migrationer %>%
  rename('id' = 'LopNr',
         'date_mig' = 'Datum',
         'dir_mig' = 'Posttyp') %>%
  mutate(date_mig = as.Date.character(date_mig %>% as.character, format = '%Y%m%d'))

# Then I find individuals who are >3 years abroad
mig_2 <- mig_1 %>% 
  arrange(id, date_mig) %>% 
  group_by(id) %>% 
  mutate(
    next_date = lead(date_mig),
    next_dir = lead(dir_mig)) %>% 
  filter(dir_mig == 'Utv') %>% 
  transmute(id,
            start = date_mig,
            end = case_when(
              next_dir == 'Inv' ~ next_date,
              T ~ as.Date.character('2023-12-31', format = '%Y-%m-%d')
            )
  ) %>% 
  ungroup() %>% 
  mutate(
    dur_years = as.numeric(end-start) / 365.25,
    long_abroad = dur_years > 3 ) %>% 
  filter(long_abroad)

# Then I transform to one-year-per-row and mask the individuals who are >3 years 
# abroad
mig_3 <- mig_2 %>% 
  mutate(year_start = year(start),
         year_end = year(end-days(1))) %>% 
  rowwise() %>% 
  mutate(year = list(seq(year_start, year_end))) %>% 
  unnest(year) %>% 
  ungroup() %>% 
  distinct(id, year) %>% 
  mutate(abroad3y = 1L)

# Merge together the migration data with the cohort data
data_3 <- data_2 %>%
  map( ~ left_join(.x,
                   mig_3,
                   by = c('id', 'year')) %>%
         replace_na(list('abroad3y' = 0)))

## 00_02_04 Connect indata and covariate data set with diagnosis data ==========
dors_1 <- dors %>%
  unite('diag',
        c(ULORSAK, contains('MORSAK')),
        sep = ' ',
        remove = T) %>%
  mutate(
    DODSDAT =
      case_when(
        str_sub(DODSDAT, -4, -3) == "00" ~
          str_sub(DODSDAT, 1, 4) %>% paste0('0601'),
        str_sub(DODSDAT, -2) == "00" ~
          str_sub(DODSDAT, 1, 6) %>% paste0('15'),
        TRUE ~ DODSDAT
      ) %>%
      as.Date.character(format = '%Y%m%d'),
    date_CKD = if_else((ICD == 9 & str_detect(diag, '\\b585')) |
                         (ICD == 10 & str_detect(diag, '\\bN18')),
                       true = DODSDAT,
                       false = NA_Date_),
    date_DM  = if_else((ICD == 9 & str_detect(diag, '\\b250')) |
                         (ICD == 10 & str_detect(diag, '\\bE1*')),
                       true = DODSDAT, 
                       false = NA_Date_),
    date_HPT = if_else((ICD == 9 & str_detect(diag, '\\b401')) |
                         (ICD == 10 & str_detect(diag, '\\bI1*')),
                       true = DODSDAT, 
                       false = NA_Date_),
    date_cGN = if_else((ICD == 9 & str_detect(diag, '\\b582')) |
                         (ICD == 10 & str_detect(diag, '\\bN03')),
                       true = DODSDAT, 
                       false = NA_Date_),
    date_DN  = if_else((ICD == 9 & str_detect(diag, '\\b250D')) |
                         (ICD == 10 & str_detect(diag, '\\bE1[0-4].2')),
                       true = DODSDAT,
                       false = NA_Date_),
    date_HN  = if_else((ICD == 9 & str_detect(diag, '\\b40[34]')) |
                         (ICD == 10 & str_detect(diag, '\\bI1[23]')),
                       true = DODSDAT,
                       false = NA_Date_)) %>%
  mutate(reg = 'dors') %>%
  select(id = LopNr, date_death = DODSDAT, date_CKD, date_DM, date_HPT, 
         date_cGN, date_DN, date_HN, reg)

par_1 <- bind_rows(par_sv %>% mutate(reg = 'sv'),
                   par_ov %>% mutate(reg = 'ov')) %>%
  unite('diag', c(contains('DIA')), sep = ' ', remove = T) %>%
  mutate(
    INDATUMA =
      case_when(
        str_sub(INDATUMA, -4, -3) == "00" ~
          str_sub(INDATUMA, 1, 4) %>% paste0('0601'),
        str_sub(INDATUMA, -2) == "00" ~
          str_sub(INDATUMA, 1, 6) %>% paste0('15'),
        TRUE ~ INDATUMA
      ) %>%
      as.Date.character(format = '%Y%m%d'),
    date_CKD = if_else((str_detect(diag, '\\b585')) |
                         (str_detect(diag, '\\bN18')),
                       true = INDATUMA,
                       false = NA_Date_),
    date_RRT = if_else((str_detect(diag, '\\bV(56|45B|42A)')) |
                         (str_detect(diag, '\\bZ(49|99\\.2|94\\.0)')),
                       true = INDATUMA,
                       false = NA_Date_),
    date_DM  = if_else((str_detect(diag, '\\b250')) |
                         (str_detect(diag, '\\bE1*')),
                       true = INDATUMA, 
                       false = NA_Date_),
    date_HPT = if_else((str_detect(diag, '\\b401')) |
                         (str_detect(diag, '\\bI1*')),
                       true = INDATUMA, 
                       false = NA_Date_),
    date_cGN = if_else((str_detect(diag, '\\b582')) |
                         (str_detect(diag, '\\bN03')),
                       true = INDATUMA, 
                       false = NA_Date_),
    date_DN  = if_else((str_detect(diag, '\\b250D')) |
                         (str_detect(diag, '\\bE1[0-4].2')),
                       true = INDATUMA,
                       false = NA_Date_),
    date_HN  = if_else((str_detect(diag, '\\b40[34]')) |
                         (str_detect(diag, '\\bI1[23]')),
                       true = INDATUMA,
                       false = NA_Date_)) %>%
  select(id = LopNr, contains('date_'))

data_4 <- data_3 %>%
  map( ~ left_join(
    .x,
    bind_rows(dors_1, par_1) %>%
      group_by(id) %>%
      summarise(across(
        contains('date_'),
        ~ if_else(any(!is.na(.x)),
                  true = min(.x, na.rm = T),
                  false = NA_Date_)
      )),
    by = 'id'
  ) %>% 
# ensure that date of death occurring before CoD register retrieval are carried 
# over from the indata
    mutate(date_death = case_when(
      !is.na(date_death) ~ date_death,
      is.na(date_death) &
        !is.na(date_death_indata) ~ date_death_indata,
      T ~ NA_Date_
    )))

## 00_02_05 Construct exposure lags ============================================
data_5 <- data_4 %>%
  map(
    ~ .x %>%
      select(-geometry) %>%
      group_by(id) %>%
      arrange(year) %>%
      mutate(across(c('pm25', 'pm10', 'nox', 'bc'),
                    ~ slider::slide_dbl(.x,
                                        mean,
                                        .before = 1,
                                        .complete = T,
                                        .after = -1),
                    .names = '{col}_lag1'),
             across(c('pm25', 'pm10', 'nox', 'bc'),
                    ~ slider::slide_dbl(.x,
                                        mean,
                                        .before = 5,
                                        .complete = T,
                                        .after = -2),
                    .names = '{col}_lag2_5'),
             across(c('pm25', 'pm10', 'nox', 'bc'),
                    ~ slider::slide_dbl(.x,
                                        mean,
                                        .before = 10,
                                        .complete = T,
                                        .after = -6),      
                    .names = '{col}_lag6_10')) %>%
      ungroup() %>% 
      mutate(across(paste('date',
                          c('CKD', 'RRT', 'DM', 'HPT', 'cGN', 'DN', 'HN'),
                          sep = '_'),
                    ~ case_when(is.na(.x) | (year(.x) > year) ~ 0,
                                year(.x) == year ~ 1,
                                year(.x) < year ~ 2),
                    .names = '{col}_p')))

## 00_02_06 Right and left censor ==============================================
#data_5 <- readRDS(here::here('Code and stats/data_5_2025-12-01.rds'))

data_6_CKD <- data_5 %>%
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
      filter((date_CKD_p != 2) &
               (date_in >= date_fup) &
               (date_in <= date_death | is.na(date_death)) &
               (age_in >= 30) &
               (age_in < 90) &
               ((date_in >= date_immig) | is.na(date_immig)) &
               abroad3y == 0
      ) %>%
      mutate(
        date_out = case_when(
          date_CKD_p == 1 ~ date_CKD,
          date_death < date_out ~ date_death,
          T ~ date_out
        ),
        date_out = if_else(month(date_out) == 1 &
                             day(date_out) == 1,
                           date_out + 1,
                           date_out)
      )
  )
      
data_6_RRT <- data_5 %>%
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
      filter((date_RRT_p != 2) &
               (date_out >= date_fup) &
               (date_death >= date_in | is.na(date_death)) &
               (age_in >= 30) &
               (age_in < 90) &
               ((date_in >= date_immig) | is.na(date_immig)) &
               abroad3y == 0
      ) %>%
      mutate(
        date_out = case_when(
          date_RRT_p == 1 ~ date_RRT,
          date_death < date_out ~ date_death,
          T ~ date_out
        ),
        date_out = if_else(month(date_out) == 1 &
                             day(date_out) == 1,
                           date_out + 1,
                           date_out)
      )
  )

## 00_02_07 Save complete clean basis data sets to file ========================
saveRDS(data_4, file = here::here('Code and stats', 
                                  paste0('data_4_', Sys.Date(), '.rds')))

saveRDS(data_5, file = here::here('Code and stats', 
                                  paste0('data_5_', Sys.Date(), '.rds')))

saveRDS(data_6_CKD, file = here::here('Code and stats',
                                      paste0('data_6_CKD_',
                                             Sys.Date(), 
                                             '.rds')))

saveRDS(data_6_RRT, file = here::here('Code and stats',
                                      paste0('data_6_RRT_',
                                             Sys.Date(), 
                                             '.rds')))

