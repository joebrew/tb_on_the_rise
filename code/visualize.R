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
source(paste0(code_dir, 'theme.R'))

# Filter out periods outside of study period
tb <- tb %>%
  filter(year >= 1997, 
         year <= 2012)

popuation <-
  population %>%
  filter(year >= 1997, 
         year <= 2012)


# Rename gender sex in tb
tb$sex <- tb$gender
##### RESULTS SECTION

# Overall and demographic characteristics

# Number of overall cases
nrow(tb)


# Number of overall cases
length(which(tb$incident_case == 'incident case'))

# Distribution by geography
table(tb$health_unit_register[tb$incident_case == 'incident case'])
prop.table(table(tb$health_unit_register[tb$incident_case == 'incident case']))

# Mean age of registered tb patients
summary(tb$years_old[tb$incident_case == 'incident case'], na.rm = TRUE)
sd(tb$years_old[tb$incident_case == 'incident case'], na.rm = TRUE)
tb %>%
  filter(incident_case == 'incident case') %>%
  group_by(years_old_round) %>%
  tally() %>%
  arrange(desc(n))

# Sex
table(tb$gender[tb$incident_case == 'incident case'])
prop.table(table(tb$gender[tb$incident_case == 'incident case']))

# Proportion of cases by age group
temp <- 
  tb %>%
  filter(incident_case == 'incident case') %>%
  group_by(age_group) %>%
  summarise(cases = n()) %>%
  # join to population AVERAGE for period
  left_join(population %>%
              filter(year >= 1997,
                     year <= 2012) %>%
              group_by(age_group, year) %>%
              summarise(n = sum(n, na.rm = TRUE)) %>%
              ungroup %>%
              group_by(age_group) %>%
              summarise(n = mean(n))) %>%
  # get percentage
  mutate(p = cases / n * 100,
         incidence = cases / n * 100000) %>%
  filter(!is.na(age_group)) %>%
  arrange(age_group)
temp %>%
  arrange(desc(p)) 

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
population %>%
  filter(year >= 1997,
         year <= 2012) %>%
  group_by(year) %>%
  summarise(n = sum(n))

# Incident rate per 100,000
incidence <- 
  tb %>%
  filter(incident_case == 'incident case') %>%
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
incidence %>%
  group_by(time_period = ifelse(year >= 2008 & year <= 2012,
                                '2008-2012',
                                ifelse(year >= 1997 & year <= 2001,
                                       '1997-2001',
                                       NA))) %>%
  summarise(cases = sum(cases))

# Get incidence per 100000

# total number of incident cases during study period
nrow(tb[tb$incident_case == 'incident case',])
table(tb$incident_case == 'incident case')
prop.table(table(tb$incident_case == 'incident case'))

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

# Plot incidence by age group
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


# Plot incidence by sex
temp <- 
  incidence %>%
  group_by(year, sex = Hmisc::capitalize(sex)) %>%
  summarise(cases = sum(cases, na.rm = TRUE),
            n = sum(n, na.rm = TRUE)) %>%
  mutate(incidence = cases / n * 100000)

cols <- colorRampPalette(brewer.pal(9, 'Spectral'))(length(unique(temp$sex)))
ggplot(data = temp,
       aes(x = year,
           y = incidence)) +
  geom_line(aes(color = sex, group = sex)) +
  scale_color_manual(name = 'Sex',
                     values = cols) +
  xlab('Year') +
  ylab('Incidence (per 100,000)') +
  ggtitle('Incidence over time by sex') +
  theme_tb()

# Which group over all time?
temp <- 
  incidence %>%
  group_by(age_group, sex) %>%
  summarise(cases = sum(cases),
            n = sum(n)) %>%
  mutate(incidence = cases / n * 100000) %>%
  ungroup %>%
  arrange(desc(incidence))

# Compared to women?
temp$incidence[temp$age_group == '60-64' &
                 temp$sex == 'female']

# Chart men vs. women full period (by age group)
cols <- adjustcolor(c('darkorange', 'darkgreen'), alpha.f = 0.6)
ggplot(data = temp,
       aes(x = age_group, y = incidence, group = sex, fill = sex)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_tb() +
  theme(axis.text.x = element_text(angle = 45)) +
  xlab('Age group') +
  ylab('Incidence (per 100,000)') +
  ggtitle('Incidence by sex and age group') +
  scale_fill_manual(name = 'Sex',
                    values = cols)