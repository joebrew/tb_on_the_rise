# Directories
root <- getwd() # should be the tb_on_the_rise directory
setwd('data')
data_dir <- getwd()
setwd(root)
setwd('code')
code_dir <- getwd()
setwd(root)

# Libraries
library(foreign)
library(dplyr)
library(ggplot2)
library(readr)
library(ggthemes)

# Read and clean
source(paste0(code_dir, '/read_and_clean.R'))

# Theme for plotting
source(paste0(code_dir, '/theme.R'))

##### RESULTS SECTION

# Overall and demographic characteristics

# Number of overall cases
length(which(tb$incident_case))

# Distribution by geography
table(tb$health_unit_register[tb$incident_case])
prop.table(table(tb$health_unit_register[tb$incident_case])) * 100

# Mean age of registered tb patients
summary(tb$years_old[tb$incident_case], na.rm = TRUE)
sd(tb$years_old[tb$incident_case], na.rm = TRUE)

# Mode
tb %>%
  filter(incident_case) %>%
  group_by(years_old_round) %>%
  tally() %>%
  arrange(desc(n))

# Sex
table(tb$gender[tb$incident_case])
prop.table(table(tb$gender[tb$incident_case]))

# Overall incidence by age group
temp <- 
  tb %>%
  filter(incident_case) %>%
  group_by(age_group) %>%
  summarise(cases = n()) %>%
  # join to population (annual AVERAGE denominator)
  left_join(population %>%
              group_by(age_group) %>%
              summarise(n = sum(n, na.rm = TRUE))) %>%
  ungroup %>%
  # get percentage
  mutate(p = cases / n * 100,
         incidence = cases / n * 100000) %>%
  filter(!is.na(age_group)) %>%
  arrange(age_group) 

# Greatest NUMBER of cases
temp %>% arrange(desc(cases)) %>% mutate(p_of_all = cases / sum(cases) * 100)

# Greatest INCIDENCE rate of cases
temp %>% arrange(desc(incidence))

# Figure of overall incidence by age group
ggplot(data = temp,
       aes(x = age_group,
           y = incidence)) +
  geom_bar(stat = 'identity',
           fill = 'darkorange',
           alpha = 0.6) +
  theme_tb() +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1)) +
  xlab('Age group') +
  ylab('Incidence (per 100,000)') +
  ggtitle('Average TB incidence by age group')

# Population of district over all years
temp <- population %>%
  filter(year >= 1997,
         year <= 2012) %>%
  group_by(year) %>%
  summarise(n = sum(n))
temp

# Growth over those years
(temp$n[length(temp$n)] / temp$n[1] * 100) - 100

# Incident rate per 100,000
incidence <- 
  tb %>%
  filter(incident_case) %>%
  filter(year >= 1997,
         year <= 2012) %>%
  group_by(year, age_group, sex) %>%
  summarise(cases = n()) %>%
  # join to population for period
  left_join(population %>%
              filter(year >= 1997,
                     year <= 2012) %>%
              group_by(age_group, year, sex) %>%
              summarise(n = sum(n, na.rm = TRUE))) %>%
              ungroup %>%
  # calculate incidence per 100000
  mutate(incidence = cases / n * 100000)

# Absolute numbers from 2008-2012 versus 1997-2001
temp <- 
  incidence %>%
  group_by(time_period = ifelse(year >= 2008 & year <= 2012,
                                '2008-2012',
                                ifelse(year >= 1997 & year <= 2001,
                                       '1997-2001',
                                       NA))) %>%
  summarise(cases = sum(cases)) %>%
  filter(!is.na(temp$time_period))
# Growth in cases from 2008-2012 compared to 1997-2001
(temp$cases[temp$time_period == '2008-2012'] / 
  temp$cases[temp$time_period == '1997-2001'] * 100) -100

# Get incidence per 100000

# total number of incident cases during study period
nrow(tb[tb$incident_case,])
table(tb$incident_case)
prop.table(table(tb$incident_case))

# Get incidence not broken down by age group or sex
ts <- incidence %>%
  group_by(year) %>%
  summarise(cases = sum(cases, na.rm = TRUE),
            n = sum(n, na.rm = TRUE)) %>%
  mutate(incidence = cases / n * 100000)

ggplot(data = ts,
       aes(x = year, y = incidence)) +
  geom_area(fill = 'darkorange',
            alpha = 0.6) +
  xlab('Year') +
  ylab('Incidence (per 100,000)') +
  theme_tb() +
  ggtitle('TB incidence over time')

# Overall incidence by age group
temp <- 
  tb %>%
  filter(incident_case) %>%
  group_by(age_group) %>%
  summarise(cases = n()) %>%
  # join to population (annual AVERAGE denominator)
  left_join(population %>%
              group_by(age_group) %>%
              summarise(n = sum(n, na.rm = TRUE))) %>%
  ungroup %>%
  # get percentage
  mutate(p = cases / n * 100,
         incidence = cases / n * 100000) %>%
  filter(!is.na(age_group)) %>%
  arrange(age_group) 

# Figure of overall incidence by age group
ggplot(data = temp,
       aes(x = age_group,
           y = incidence)) +
  geom_bar(stat = 'identity',
           fill = 'darkorange',
           alpha = 0.6) +
  theme_tb() +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1)) +
  xlab('Age group') +
  ylab('Incidence (per 100,000)') +
  ggtitle('Average TB incidence by age group')

# Plot incidence by sex
temp <- 
  incidence %>%
  group_by(year, sex = Hmisc::capitalize(sex)) %>%
  summarise(cases = sum(cases, na.rm = TRUE),
            n = sum(n, na.rm = TRUE)) %>%
  mutate(incidence = cases / n * 100000)

# cols <- colorRampPalette(brewer.pal(9, 'Spectral'))(length(unique(temp$sex)))
cols <- c('darkgreen', 'darkorange')
ggplot(data = temp,
       aes(x = year,
           y = incidence)) +
  geom_line(aes(color = sex, group = sex),
            alpha = 0.6, size = 2) +
  scale_color_manual(name = 'Sex',
                     values = cols) +
  guides(col = guide_legend(reverse = TRUE)) +
  xlab('Year') +
  ylab('Incidence (per 100,000)') +
  ggtitle('Incidence over time by sex') +
  theme_tb()

# Plot incidence by age group AND year
temp <- 
  incidence %>%
  group_by(year, age_group) %>%
  summarise(cases = sum(cases, na.rm = TRUE),
            n = sum(n, na.rm = TRUE)) %>%
  mutate(incidence = cases / n * 100000)

cols <- colorRampPalette(brewer.pal(9, 'Spectral'))(length(unique(temp$age_group)))
ggplot(data = temp,
       aes(x = year,
           y = incidence)) +
  geom_line(aes(color = age_group, group = age_group)) +
  scale_color_manual(name = 'Age group',
                     values = cols) +
  xlab('Year') +
  ylab('Incidence (per 100,000)') +
  ggtitle('Incidence over time by age group') +
  theme_tb()

# Which group over all time?
temp <- 
  incidence %>%
  group_by(age_group, sex) %>%
  summarise(cases = sum(cases),
            n = sum(n)) %>%
  mutate(incidence = cases / n * 100000) %>%
  ungroup %>%
  arrange(desc(incidence)) %>%
  filter(!is.na(age_group))

# Compared to women?
temp$incidence[temp$age_group == '60-64' &
                 temp$sex == 'male']
temp$incidence[temp$age_group == '60-64' &
                 temp$sex == 'female']

# Chart men vs. women full period (by age group)
cols <- adjustcolor(c('darkgreen', 'darkorange'), alpha.f = 0.6)
temp$sex <- Hmisc::capitalize(temp$sex)
ggplot(data = temp,
       aes(x = age_group, y = incidence, group = sex, fill = sex)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_tb() +
  theme(axis.text.x = element_text(angle = 45)) +
  xlab('Age group') +
  ylab('Incidence (per 100,000)') +
  ggtitle('Incidence by sex and age group') +
  scale_fill_manual(name = 'Sex',
                    values = cols) +
  guides(fill = guide_legend(reverse = TRUE))

#### HIV STATUS AND TB

# how many hivs before 2006
table(tb$hiv_status[tb$year < 2006])

# What percentage unknown since 2008
table(tb$hiv_status[tb$year >= 2008])
prop.table(table(tb$hiv_status[tb$year >= 2008], useNA = 'ifany'))

# Total number known
table(tb$hiv_status %in% c('negative', 'positive'))
prop.table(table(tb$hiv_status %in% c('negative', 'positive')))
table(tb$hiv_status[tb$hiv_status != 'unknown'])
prop.table(table(tb$hiv_status[tb$hiv_status != 'unknown']))
table(tb$sex[tb$hiv_status == 'positive'])
prop.table(table(tb$sex[tb$hiv_status == 'positive']))
temp <-
  tb %>%
  filter(incident_case) %>%
  group_by(year, hiv_status) %>%
  tally %>%
  filter(!is.na(year),
         !is.na(hiv_status))

# Make sure to have all years
left <- 
  expand.grid(year = unique(sort(temp$year)),
              hiv_status = sort(unique(temp$hiv_status)))
temp <- left_join(left, temp)
temp$n[is.na(temp$n)] <- 0
# Reorder factor levels for plotting
temp$hiv_status <- Hmisc::capitalize(temp$hiv_status)
temp$hiv_status <- factor(temp$hiv_status,
                          levels = c('Unknown',
                                     'Negative',
                                     'Positive'))

# What percentage of all coinefections were among patients 
# aes 15 to 49
tb %>%
  filter(!is.na(years_old_round)) %>%
  filter(hiv_status == 'positive') %>%
  group_by(fi = years_old_round >= 18 & years_old_round <= 49) %>%
  tally %>%
  mutate(p = n / sum(n) * 100)


cols <- c('darkgrey', 'darkgreen', 'darkorange')

ggplot(data = temp,
       aes(x = year, 
           y = n)) +
  geom_area(aes(fill = hiv_status),
            # color = 'black',
            position = 'stack', alpha = 0.7) +
  scale_fill_manual(name = 'HIV status',
                    values = cols) +
  xlab('Year') +
  ylab('Incident TB cases') +
  theme_tb() +
  ggtitle('HIV status among incident TB cases by year')

#### AGE GROUP AMONG TB HIV CASES
temp <- tb %>%
  group_by(age_group) %>%
  filter(hiv_status != 'unkown') %>%
  summarise(hiv = length(which(hiv_status == 'positive')),
            tb = n()) %>%
  filter(!is.na(age_group))
# Gather
temp <-
  temp %>%
  tidyr::gather(key, value, hiv:tb)
temp$key <-
  ifelse(temp$key == 'hiv', 'TB & HIV',
         ifelse(temp$key == 'tb', 'TB',
                NA))

cols <- adjustcolor(c('darkgreen', 'darkorange'), alpha.f = 0.6)
ggplot(data = temp,
       aes(x = age_group, y = value, group = key, fill = key)) +
  geom_bar(stat = 'identity', position = 'stack') +
  theme_tb() +
  theme(axis.text.x = element_text(angle = 45)) +
  xlab('Age group') +
  ylab('Cases)') +
  ggtitle('Total TB and TB-HIV co-infections') +
  scale_fill_manual(name = 'Status',
                    values = cols) 
  # guides(fill = guide_legend(reverse = TRUE))

