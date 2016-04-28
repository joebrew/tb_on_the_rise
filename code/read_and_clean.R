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

##### GET POPULATION BY YEAR
# Manually code based on the two html files herein:
# - Pop_por_distrito_Map_province_2007.htm
# - Pop_por_distrito_Map_province_1997.htm
population <- 
  data.frame(age_group = rep(c('00',
                               '01-04',
                               '05-09',
                               '10-14',
                               '15-19',
                               '20-24',
                               '25-29',
                               '30-34',
                               '35-39',
                               '40-44',
                               '45-49',
                               '50-54',
                               '55-59',
                               '60-64',
                               '65-69',
                               '70-74',
                               '75-79',
                               '80+'), 
                             2))
# Add the two years
population$year <-
  rep(c(1997, 2007), each = nrow(population) / 2)
# Add the two genders
males <- population; males$sex <- 'male'
females <- population; females$sex <- 'female'
# Overwite population with our genders
population <- rbind(males, females)
rm(males, females)
# Populate with the data from the htmls
population$n <- NA
population$n[population$year == 2007 & 
               population$sex == 'male'] <-
  c(18294,	
    71362,	
    81491,	
    72298,	
    61643,	
    56441,	
    51213,	
    38603,	
    29392,	
    24123,	
    21259,	
    15034,	
    10760,	
    7713,
    6588,	
    4566,	
    3012,	
    2220)
population$n[population$year == 2007 & 
               population$sex == 'female'] <- 
  c(18320, 
    71862, 
    83061, 
    73945, 
    63691, 
    67678, 
    58576, 
    43714, 
    33632, 
    26201, 
    21532, 
    17468, 
    13666, 
    10311, 
    8959, 
    6886, 
    4988, 
    5207)
population$n[population$year == 1997 & 
               population$sex == 'male'] <-
  c(11324, 
    47372, 
    54564, 
    55013, 
    47587, 
    31514, 
    24292, 
    21659, 
    21395, 
    15487, 
    12243, 
    9277, 
    8920, 
    7048, 
    5492, 
    2644, 
    2314, 
    1644)

population$n[population$year == 1997 & 
               population$sex == 'female'] <-
  c(11636, 
    48501, 
    55412, 
    55293, 
    50270, 
    41213, 
    30931, 
    25934, 
    22479, 
    17106, 
    15577, 
    12679, 
    11050, 
    8971, 
    7741, 
    4260, 
    4044, 
    3293)

##### INTERPOLATE POPULATION FOR YEARS WE DON'T HAVE
# (AND EXTRAPOLATE FOR YEARS BEFORE FIRST CENSUS)
denom <- 
  expand.grid(year = 1995:2007,
              sex = c('male', 'female'),
              age_group = unique(sort(population$age_group)))
denom$n <- NA
years <- sort(unique(denom$year))
n_years <- length(years)

# Fill up those that are already known
for (i in 1:nrow(denom)){
  this_row <- denom[i,]
  n <- 
    population$n[population$year == this_row$year &
                   population$sex == this_row$sex &
                   population$age_group == this_row$age_group]
  if(length(n) == 1){
    denom$n[i] <- n
  }
}

# Interpolate / Extrapolate the missings
for (sex in c('male', 'female')){
  for (age_group in unique(sort(population$age_group))){
    # Calculate slope
    sub_data <- population[population$sex == sex & 
                             population$age_group == age_group,]
    growth <- sub_data$n[sub_data$year == 2007] - 
      sub_data$n[sub_data$year == 1997]
    # Divde growth by number of years (year-specific group growth)
    by_year <- growth / 10
    # Add the results into denom
    for (year in unique(denom$year[denom$sex == sex &
                                   denom$age_group == age_group])){
      # See how many years since 1997
      years_since_1997 <- year - 1997
      # Get the 1997 val
      val_1997 <- sub_data$n[sub_data$year == 1997]
      # Get the value for that year
      val <- val_1997 + (years_since_1997 * by_year)
      # Stick that value into results
      denom$n[denom$sex == sex &
                denom$age_group == age_group &
                denom$year == year] <- val
    }
  }
}

# Specify whether the data is from census or estimate
denom <- 
  denom %>%
  left_join(population %>%
              mutate(data_source = 'census'),
            by = c("year", "sex", "age_group", "n")) %>%
  mutate(data_source = ifelse(is.na(data_source), 'estimate', data_source))

# Overwrite population, and remove garbage
population <- denom
rm(i, age_group, sex, denom, sub_data, this_row)

# NOW USE DSS's 2008 to 2014 GROWTH RATE TO
# EXTRAPOLATE FORWARD
denom <- 
  expand.grid(year = 2008:2014,
              sex = c('male', 'female'),
              age_group = sort(unique(population$age_group)),
              n = NA,
              data_source = 'estimate')

# Define a dss growth rate
# based on Populacao_1997 a 2014.xls
# sent by ariel to Alberto and Joe
dss_growth_rate <- 
  c(50819,
    52130,
    53961,
    55155,
    55810,
    56964,
    57992) / 
  50819
df <- data.frame(num = 1:length(dss_growth_rate),
                 val = dss_growth_rate)
fit <- lm(val ~ num, data = df)
dss_growth_rate <- as.numeric(coef(fit)['num'])
rm(df, fit)

# Apply the growth rate to all of denom (forward-looking extrapolations)
population_2007 <- population[population$year == 2007,]
for (i in 1:nrow(denom)){
  # subset data
  sub_data <- denom[i,]
  # Get the 2007 value
  value_2007 <- 
    population_2007$n[population_2007$sex == sub_data$sex &
                        population_2007$age_group == sub_data$age_group]
  # Get difference in years
  year_difference <- sub_data$year - 2007
  # Multiply year difference by growth rate to get additions
  new_val <- ((year_difference * dss_growth_rate) + 1) * 
    value_2007
  # Inject into dataframe
  denom$n[i] <- new_val
}

# Join denom to population
population <- rbind(population, denom)

# Arrange
population <- 
  population %>%
  arrange(year, sex, age_group)

# Make a temporary data frame and plot
temp <- population

temp$Group <- paste0(temp$sex, ' age ', temp$age_group)

ggplot(data = temp,
       aes(x = year, y = n, group = Group, color = Group)) +
  geom_line()

# Remove junk
rm(denom, population_2007, sub_data, temp,
   by_year, dss_growth_rate, growth, i,
   n, n_years, new_val, val, val_1997,
   year, year_difference, years, years_since_1997)

##### BACK TO TB DATA
# Get age groups compatible with our population data
age_group_df <- 
  data.frame(years_old_round = 0:99)
age_group_df$age_group <- 
  c('00',
    rep('01-04', 4),
    rep(as.character(sort(unique(population$age_group)))[3:17], each = 5),
    rep('80+', 20))
tb <- 
  tb %>%
  left_join(age_group_df,
            by = 'years_old_round')
rm(age_group_df)

# Write csvs of cleaned data
setwd(data_dir)
write_csv(tb, 'cleaned_data.csv')
write_csv(population, 'population.csv')
setwd(root)
