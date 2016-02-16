# Directories
root <- getwd() # should be the tb_on_the_rise directory
setwd('data')
data_dir <- getwd()
setwd(root)
setwd('code')
code_dir <- getwd()

# Libraries
library(foreign)
library(dplyr)
library(ggplot2)
library(readr)

# Read in data
setwd(data_dir)
read_data <- function(x){
  suppressMessages(suppressWarnings(read.spss(x, to.data.frame = TRUE)))
}
tb <- read_data('RETUBE_NOTTOUCH_FINAL.sav')

# Define simple functions for cleaning up dates
clean_date <- function(x, type = 'day'){
  temp <- suppressWarnings(as.numeric(as.character(x)))
  if(type == 'day'){
    result <- ifelse(is.na(temp) | temp > 31 | temp < 1, NA, temp)
  }
  if(type == 'month'){
    result <- ifelse(is.na(temp) | temp > 12 | temp < 1, NA, temp)
  }
  if(type == 'year'){
    result <- ifelse(is.na(temp) | temp > 2016 | temp < 1970, NA, temp)
  }
  return(result)
}
make_date <- function(year, month, day){
  if(any(is.na(year), is.na(month), is.na(day))){
    result <- NA
  } else {
    result <- as.Date(paste0(year, '-', month, '-', day))
  }
  return(result)
}

# Clarify / recode according to Salome's explanations
tb <- 
  tb %>%
  rename(study_code = n_retube, # study code
         year = year_book, # year of registration at the book of the TB program
         registration_number = nit) %>% # tuberculosis registration number
  # Dates = ttm, tarv, ttm_end, bk
  mutate(ttm_year = clean_date(ttm_year, 'year'),
         ttm_month = clean_date(ttm_month, 'month'),
         ttm_day = clean_date(ttm_day, 'day'),
         ttm_end_year = clean_date(ttm_end_year, 'year'),
         ttm_end_month = clean_date(ttm_end_month, 'month'),
         ttm_end_day = clean_date(ttm_end_day, 'day'),
         tarv_year = clean_date(tarv_year, 'year'),
         tarv_month = clean_date(tarv_month, 'month'),
         tarv_day = clean_date(tarv_day, 'day'),
         bk_year = clean_date(bk_year, 'year'),
         bk_month = clean_date(bk_month, 'month'),
         bk_day = clean_date(bk_day, 'day')) %>%
  mutate(ttm_date = make_date(ttm_year, ttm_month, ttm_day),
         ttm_end_date = make_date(ttm_end_year, ttm_end_month, ttm_end_day),
         tarv_date = make_date(tarv_year, tarv_month, tarv_day),
         bk_date = make_date(bk_year, bk_month, bk_day)) %>%
  mutate(gender = ifelse(gender == 1, 'male',
                         ifelse(gender == 2, 'female',
                                NA)),
         tb_type = ifelse(tb_type == 1, 'pulmonary',
                          ifelse(tb_type == 2, 'extrapulmonary',
                                 NA)),
         case_type = ifelse(case_type == 1, 'new case',
                            ifelse(case_type == 2, 'relapse',
                                   ifelse(case_type == 3, 'treatment after lost to follow up',
                                          ifelse(case_type == 4, 'treatment failure',
                                                 ifelse(case_type == 5, 'transferred',
                                                        ifelse(case_type == 6, 'recurrent',
                                                               ifelse(case_type == 7, 'chronic',
                                                                      ifelse(case_type == 8, 'MDR',
                                                                             ifelse(case_type == 0, 'other',
                                                                                    NA))))))))),
         hiv_status = ifelse(hiv_status == 1, 'positive',
                             ifelse(hiv_status == 2, 'negative',
                                    ifelse(hiv_status == 9, 'unknown', 
                                           NA))),
         tpc = ifelse(tpc == 0, 'no', # preventive therarpy with clotrimoxazol (prevention of candidiasis)
                      ifelse(tpc == 1, 'yes',
                             NA)),
         children_u5 = children_u5, # not sure what this means yet
         ttm_result = ifelse(ttm_result == 1, 'cured',
                             ifelse(ttm_result == 2, 'treatment completed',
                                    ifelse(ttm_result == 3, 'treatment failed',
                                           ifelse(ttm_result == 4, 'lost to follow up',
                                                  ifelse(ttm_result == 5, 'death',
                                                         ifelse(ttm_result == 6, 'not evaluated: transferred out',
                                                                NA)))))),
         health_unit_register = ifelse(health_unit_register == 1, 'Manhiça',
                                       ifelse(health_unit_register == 2, 'Xinavane',
                                              ifelse(health_unit_register == 3, 'Palmeira',
                                                     NA))),
         bk_result = ifelse(bk_result == 0, 'negative',
                            ifelse(bk_result == 1, '1',
                                   ifelse(bk_result == 2, '2',
                                          ifelse(bk_result == 3, '3',
                                                 ifelse(bk_result == 4, 'not done',
                                                        ifelse(bk_result == 5, 'scanti',
                                                               NA))))))) %>%
  mutate(
    # New categories proposed by Salome
    smear_result = ifelse(bk_result == 'negative', 'smear negative',
                          ifelse(bk_result %in% c('1', '2', '3', '5'), 'smear positive',
                                 ifelse(bk_result == 'not done', 'not done',
                                        NA))),
    new_case = ifelse(case_type == 'new case', 'new case', 
                      ifelse(case_type == 'transferred', 'not known',
                             ifelse(case_type %in% c('other', 
                                                     'relapse', 
                                                     'treatment after lost to follow up', 
                                                     'treatment failure', 
                                                     'recurrent', 
                                                     'chronic', 
                                                     'MDR'),
                                    'previously treated cases',
                                    NA))),
    incident_case = ifelse(case_type %in% c('new case', 
                                            'relapse', 
                                            'recurrent'), 
                           'incident case',
                           ifelse(case_type %in% c('other', 
                                                   'treatment after lost to follow up', 
                                                   'treatment failure', 
                                                   'transferred', 
                                                   'chronic', 
                                                   'MDR'),
                                  'non incident case',
                                  NA))) %>%
  # Convert all ages to years
  mutate(age = gsub(' ', '', age)) %>%
  mutate(years_old = 
           suppressWarnings(
           ifelse(grepl('D', age),
                  as.numeric(gsub('D', '', as.character(age))) / 365.25,
                  ifelse(grepl('M', age),
                         as.numeric(gsub('M', '', as.character(age))) / 12,
                         ifelse(age == '999', NA,
                                as.numeric(as.character(age))))))) %>%
  # round years
  mutate(years_old_round = 
           round(years_old))

# Write csv of cleaned data
# setwd(data_dir)
# # write_csv(tb, 'cleaned_data.csv')
# setwd(root)
